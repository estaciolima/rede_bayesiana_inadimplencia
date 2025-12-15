###############################################
# 0) PACOTES
###############################################

packages <- c(
  "data.table","dplyr","stringr","discretization",
  "ranger","purrr","bnlearn","pROC","caret","tibble"
)

for(p in packages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Rgraphviz")
library(Rgraphviz)

###############################################
# 1) FUNÇÕES AUXILIARES (SAFE)
###############################################

prepare_for_bn <- function(df) {
  as.data.frame(df)
}

make_blacklist_default <- function(df, target="default"){
  data.frame(
    from = target,
    to   = setdiff(colnames(df), target),
    stringsAsFactors = FALSE
  )
}

make_blacklist_default_root <- function(df, target = "default") {
  data.frame(
    from = setdiff(colnames(df), target),
    to   = target,
    stringsAsFactors = FALSE
  )
}

###############################################
# 2) FUNÇÃO avaliar_bn (INALTERADA)
###############################################

avaliar_bn <- function(dag, df_train, df_val,
                       target="default", positive="1"){
  
  bn_model <- bn.fit(dag, data=df_train)
  
  pred_probs <- predict(bn_model, node=target, data=df_val, prob=TRUE)
  probs_matrix <- attr(pred_probs, "prob")
  
  y_true <- df_val[[target]]
  levs <- levels(y_true)
  negative <- setdiff(levs, positive)[1]
  
  if(is.null(rownames(probs_matrix)))
    rownames(probs_matrix) <- levs
  
  prob_pos <- as.numeric(probs_matrix[positive, ])
  
  y_pred_default <- factor(ifelse(prob_pos >= 0.5, positive, negative),
                           levels=levs)
  
  acc_default <- mean(y_pred_default == y_true)
  F1_default  <- caret::F_meas(y_pred_default, y_true, relevant=positive)
  
  roc_obj <- pROC::roc(y_true, prob_pos)
  auc_val <- as.numeric(pROC::auc(roc_obj))
  
  ths <- seq(0,1,by=0.01)
  
  resultados <- sapply(ths, function(t){
    y_pred <- factor(ifelse(prob_pos >= t, positive, negative), levels=levs)
    conf <- table(Pred=y_pred, Real=y_true)
    
    TP <- conf[positive, positive]
    FP <- sum(conf[positive,]) - TP
    FN <- sum(conf[,positive]) - TP
    
    if((TP+FP)==0 || (TP+FN)==0)
      return(c(precision=0, recall=0, F1=0))
    
    prec <- TP/(TP+FP)
    rec  <- TP/(TP+FN)
    
    if((prec+rec)==0)
      return(c(precision=0, recall=0, F1=0))
    
    c(precision=prec, recall=rec, F1=2*prec*rec/(prec+rec))
  })
  
  list(
    metrics = list(
      auc = auc_val,
      acc_default = acc_default,
      F1_default = F1_default,
      F1_best = max(resultados["F1",]),
      best_threshold_F1 = ths[which.max(resultados["F1",])]
    )
  )
}

###############################################
# 3) DADOS E SPLIT
###############################################

source("Códigos/funcoes.R")

set.seed(123)

df <- fread("Dados/dataset.csv") %>%
  filter(str_detect(issue_d,"2014")) %>%
  pre_processar() %>%
  select(-issue_d)

n <- nrow(df)
idx_all <- sample(n)

train_idx <- idx_all[1:floor(0.6*n)]
val_idx   <- idx_all[(floor(0.6*n)+1):floor(0.8*n)]

###############################################
# 4) EXPERIMENTO A — RF → BN (LOW MEMORY)
###############################################

cat("=== EXPERIMENTO A: RF → BN ===\n")

rf <- ranger(default~., data=df[train_idx,],
             num.trees=100,
             importance="impurity",
             respect.unordered.factors="order")

top_k <- names(sort(rf$variable.importance, decreasing=TRUE))[1:5]
df_rf <- df %>% select(all_of(c(top_k,"default")))

rm(rf); gc()

res <- quantize_with_zero_bin(df_rf, train_idx, k=3)
df_cat <- res$df %>% select(where(~!is.numeric(.))) %>% as.data.frame()
rm(res); gc()

df_train_rf <- prepare_for_bn(df_cat[train_idx,])
df_val_rf   <- prepare_for_bn(df_cat[val_idx,])

blacklist_rf <- make_blacklist_default(df_train_rf)

resultados_rf <- list()

# 1) Naive Bayes
cat(">> Naive Bayes\n")
dag <- naive.bayes(df_train_rf, training="default")
resultados_rf$naive_bayes <- avaliar_bn(dag, df_train_rf, df_val_rf)
rm(dag); gc()

# 2) TAN
cat(">> Tree Bayes (TAN) causa problema de memória\n")
dag <- tree.bayes(df_train_rf, training="default")
resultados_rf$tree_bayes <- avaliar_bn(dag, df_train_rf, df_val_rf)
rm(dag); gc()

# 3) HC BDe ISS=10
cat(">> HC (BDe, ISS=10)\n")
dag <- hc(df_train_rf, score="bde", iss=10, blacklist=blacklist_rf)
resultados_rf$hc_bde <- avaliar_bn(dag, df_train_rf, df_val_rf)
rm(dag); gc()

# 4) Constraint-based (PC-stable APENAS)
cat(">> PC-stable\n")
cpdag <- pc.stable(df_train_rf, undirected=FALSE, blacklist=blacklist_rf)
dag <- cextend(cpdag)
resultados_rf$pc_stable <- avaliar_bn(dag, df_train_rf, df_val_rf)
rm(cpdag, dag); gc()

print(
  purrr::imap_dfr(resultados_rf, ~
                    tibble(modelo=.y,
                           auc=.x$metrics$auc,
                           F1=.x$metrics$F1_best))
)

###############################################
# 5) EXPERIMENTO B — BN COMPLETA → MB → RETREINO
###############################################

cat("=== EXPERIMENTO B: BN completa → MB ===\n")

res <- quantize_with_zero_bin(df, train_idx, k=3)
df_cat_all <- res$df %>% select(where(~!is.numeric(.))) %>% as.data.frame()
rm(res); gc()

df_train_all <- prepare_for_bn(df_cat_all[train_idx,])
df_val_all   <- prepare_for_bn(df_cat_all[val_idx,])

blacklist_all <- make_blacklist_default(df_train_all)

cat(">> HC completo (BDe, ISS=10)\n")
#dag_full <- hc(df_train_all, score="bde", iss=10, blacklist=blacklist_all)
dag_full <- readRDS("dag_full.rds")
graphviz.plot(dag_full)
res_full <- avaliar_bn(dag_full, df_train_all, df_val_all)

cat("AUC:", res_full$metrics$auc,
    "F1:", res_full$metrics$F1_best, "\n")

# Markov Blanket
mb_vars <- mb(dag_full, "default")
rm(dag_full); gc()

cat(">> Re-treino com Markov Blanket\n")
df_mb <- df_cat_all %>% select(all_of(c(mb_vars,"default")))

df_train_mb <- prepare_for_bn(df_mb[train_idx,])
df_val_mb   <- prepare_for_bn(df_mb[val_idx,])

blacklist_mb <- make_blacklist_default(df_train_mb)

dag_mb <- hc(df_train_mb, score="bde", iss=10, blacklist=blacklist_mb)
res_mb <- avaliar_bn(dag_mb, df_train_mb, df_val_mb)

print(res_mb$metrics)

graphviz.plot(dag_mb)

rm(dag_mb); gc()

###############################################
# FIM
###############################################
