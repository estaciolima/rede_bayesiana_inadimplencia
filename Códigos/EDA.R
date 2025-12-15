install.packages("data.table") # recomendado para leitura de arquivos maiores 
install.packages("discretization")
install.packages("ranger")
install.packages("purrr")
install.packages("rlang")
install.packages("bnlearn")
install.packages("pROC")
install.packages("caret")

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Rgraphviz")

library(data.table)
library(dplyr)
library(tibble)
library(stringr)
library(discretization)
library(ranger)
library(purrr)
library(rlang)
library(bnlearn)
library(Rgraphviz)
library(pROC)
library(caret)

# carregar funções auxiliares
source("Códigos/funcoes.R")

set.seed(123)

# carregar dados
df <- fread("Dados/dataset.csv")

# filtrar base por um calendário específico
df <- df %>% filter(str_detect(issue_d, "2014"))
df <- pre_processar(df)
df <- df %>% select(-issue_d)

# observar histograma de todas as variáveis
plot_histograma(df)

# Algumas variáveis possuem poucos valores distintos, observar:
unique_counts <- sapply(df %>% select(where(is.numeric)), function(x) length(unique(x)))
unique_counts

# 1) Tamanho total
n <- nrow(df)

# 2) Embaralhar índices de 1 a n
idx_all <- sample.int(n)

# 3) Definir proporções
prop_train <- 0.6
prop_val   <- 0.2
prop_test  <- 0.2  # o resto

n_train <- floor(prop_train * n)
n_val   <- floor(prop_val   * n)
# o restante vai para teste
n_test  <- n - n_train - n_val

# 4) Separar índices (todos disjuntos)
train_idx <- idx_all[1:n_train]

val_idx   <- idx_all[(n_train + 1):(n_train + n_val)]

test_idx  <- idx_all[(n_train + n_val + 1):(n_train + n_val + n_test)]

# 5) Usando slice() depois
df_train <- df %>% slice(train_idx)
df_val   <- df %>% slice(val_idx)
df_test  <- df %>% slice(test_idx)


# 6) Verificar distribuição de default
df_train %>% count(default) %>% mutate(prop = n/sum(n))
df_val %>% count(default) %>% mutate(prop = n/sum(n))
df_test %>% count(default) %>% mutate(prop = n/sum(n))

# usar um método de seleção de variáveis para reduzir a dimensionalidade
# escolheu-se observar as feature importances obtidas através de uma random forest
rf <- ranger(default ~ .,
             data = df_train,
             num.trees = 100,
             importance = 'impurity',
             probability = FALSE,
             respect.unordered.factors = "order")

features_importance <- as.data.frame(rf$variable.importance) %>%
  rownames_to_column(var = "feature") %>% 
  rename(importance = "rf$variable.importance") %>% 
  arrange(desc(importance))

# verificar features_importance
dev.off() # resetar parâmetros dos gráficos
barplot(features_importance$importance[1:40],
        names.arg = features_importance$feature[1:40],
        las = 2, cex.names = 0.7)

# top k features
# k foi definido a partir da observação do barplot
k <- 5
top_k <- features_importance$feature[1:k] 
df_reduced <- df %>% select(all_of(c(top_k, "default")))
plot_histograma(df_reduced)

# aplicar no meu banco
res <- quantize_with_zero_bin(df_reduced, train_idx, k = 3)
df_reduced_cat <- res$df

df_factor_only <- df_reduced_cat %>% 
  select(where(~ !is.numeric(.x)))

df_factor_only <- as.data.frame(df_factor_only)

df_train2 <- df_factor_only %>% slice(train_idx)
df_val2 <- df_factor_only %>% slice(val_idx)
df_test2 <- df_factor_only %>% slice(test_idx)

# temos k + 1 variáveis: top k preditoras + default
# vamos fazer a DAG agora
# Método por score:
vars_sem_default <- setdiff(colnames(df_train2), "default")

blacklist_default_parent <- data.frame(
  from = "default",
  to   = vars_sem_default,
  stringsAsFactors = FALSE
)

dev.off()
bn.bds <- hc(df_train2, score = "bds", blacklist = blacklist_default_parent)
graphviz.plot(bn.bds)
bn.bds2 <- hc(df_train2, score = "bds", iss = 10) # não sei o que é esse iss
graphviz.plot(bn.bds2)

bn.bdeu <- hc(df_train2, score = "bde", iss = 10)
graphviz.plot(bn.bdeu)

bn.restart = hc(df_train2, score = "bde", iss = 1, restart = 10, perturb = 5)
graphviz.plot(bn.restart)

# bnlearn implements several constraint-based algorithms, each with its own 
#function: pc.stable, gs(), iamb(), mmpc(), si.hiton.pc(), etc.
cpdag = si.hiton.pc(df_train2, undirected = FALSE)
graphviz.plot(cpdag)
cpdag2 = pc.stable(df_train2, undirected = FALSE)
graphviz.plot(cpdag2)
cpdag3 = gs(df_train2, undirected = FALSE)
graphviz.plot(cpdag3)
cpdag4 = mmpc(df_train2, undirected = FALSE)
graphviz.plot(cpdag4)

# lista de candidatos de estrutura – só usando o TREINO
candidatos_dag <- list(
  hc_bde_iss1  = hc(df_train2, score = "bde", iss = 1, blacklist = blacklist_default_parent),
  hc_bde_iss10 = hc(df_train2, score = "bde", iss = 10, blacklist = blacklist_default_parent),
  hc_bds       = hc(df_train2, score = "bds", blacklist = blacklist_default_parent)
)

# Se quiser incluir métodos constraint-based, converta CPDAG -> DAG
cpdag_pc  <- pc.stable(df_train2, undirected = FALSE, blacklist = blacklist_default_parent)
dag_pc    <- cextend(cpdag_pc)

cpdag_hit <- si.hiton.pc(df_train2, undirected = FALSE, blacklist = blacklist_default_parent)
dag_hit   <- cextend(cpdag_hit)

candidatos_dag$pc_stable  <- dag_pc
candidatos_dag$si_hiton   <- dag_hit

resultados <- purrr::imap(candidatos_dag, ~{
  cat("Avaliando:", .y, "\n")
  avaliar_bn(dag      = .x,
             df_train = df_train2,
             df_val   = df_val2,
             target   = "default",
             positive = "1")
})


resumo <- purrr::imap_dfr(resultados, ~{
  tibble::tibble(
    modelo         = .y,
    auc            = .x$metrics$auc,
    F1_default     = .x$metrics$F1_default,
    F1_best        = .x$metrics$F1_best,
    best_threshold_F1 = .x$metrics$best_threshold_F1,
    prec_best = .x$metrics$prec_best,
    best_threshold_prec = .x$metrics$best_threshold_prec,
    recall_best = .x$metrics$recall_best,
    best_threshold_rec = .x$metrics$best_threshold_rec
  )
})

resumo

# testar seleção de variáveis com markov blanket
res <- quantize_with_zero_bin(df, train_idx, k = 3)
df_reduced_cat <- res$df

df_factor_only <- df_reduced_cat %>% 
  select(where(~ !is.numeric(.x)))

df_factor_only <- as.data.frame(df_factor_only)

df_train_cat <- df_factor_only %>% slice(train_idx)

# seleção por markov blanket
mb_vars <- learn.mb(df_train_cat,
                    node   = "default",
                    method = "iamb")


df_reduced_mb <- df_reduced_cat %>% select(all_of(c(mb_vars, "default")))
df_reduced_mb <- as.data.frame(df_reduced_mb) # conversão necessária para trabalhar corretamente com bnlearn
df_train_mb <- df_reduced_mb %>% slice(train_idx)
df_val_mb <- df_reduced_mb %>% slice(val_idx)

dev.off()

# lista de candidatos de estrutura – só usando o TREINO
candidatos_dag <- list(
  hc_bde_iss1  = hc(df_train_mb, score = "bde", iss = 1),
  hc_bde_iss10 = hc(df_train_mb, score = "bde", iss = 10),
  hc_bds       = hc(df_train_mb, score = "bds")
)

# Se quiser incluir métodos constraint-based, converta CPDAG -> DAG
cpdag_pc  <- pc.stable(df_train_mb, undirected = FALSE)
graphviz.plot(cpdag_pc)
dag_pc    <- cextend(cpdag_pc)

cpdag_hit <- si.hiton.pc(df_train_mb, undirected = FALSE)
dag_hit   <- cextend(cpdag_hit)

candidatos_dag$pc_stable  <- dag_pc
candidatos_dag$si_hiton   <- dag_hit

resultados <- purrr::imap(candidatos_dag, ~{
  cat("Avaliando:", .y, "\n")
  avaliar_bn(dag      = .x,
             df_train = df_train_mb,
             df_val   = df_val_mb,
             target   = "default",
             positive = "1")
})


resumo <- purrr::imap_dfr(resultados, ~{
  tibble::tibble(
    modelo         = .y,
    auc            = .x$metrics$auc,
    F1_default     = .x$metrics$F1_default,
    F1_best        = .x$metrics$F1_best,
    best_threshold_F1 = .x$metrics$best_threshold_F1,
    prec_best = .x$metrics$prec_best,
    best_threshold_prec = .x$metrics$best_threshold_prec,
    recall_best = .x$metrics$recall_best,
    best_threshold_rec = .x$metrics$best_threshold_rec
  )
})

resumo
