###############################################
# 0) CARREGAR PACOTES
###############################################

packages <- c("dplyr","bnlearn","pROC","caret","tibble", "data.table")
for(p in packages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

###############################################
# 2) CARREGAR SUA FUNÇÃO DE DISCRETIZAÇÃO
###############################################

source("Códigos/funcoes.R")   # certifique-se que quantize_with_zero_bin() está aqui


###############################################
# 3) CARREGAR BASE (df)
###############################################

df <- fread("Dados/dataset.csv")

# filtrar base por um calendário específico
df <- df %>% filter(str_detect(issue_d, "2014"))
df <- pre_processar(df)
df <- df %>% select(-issue_d)

###############################################
# 4) SPLIT train / val
###############################################

set.seed(123)
n <- nrow(df)
idx_all <- sample(n)

prop_train <- 0.6
prop_val   <- 0.2

n_train <- floor(prop_train * n)
n_val   <- floor(prop_val   * n)

train_idx <- idx_all[1:n_train]
val_idx   <- idx_all[(n_train+1):(n_train+n_val)]

df_train <- df[train_idx, ]
df_val   <- df[val_idx,   ]

###############################################
# 5) LOOP SOBRE VÁRIAS DISCRETIZAÇÕES
###############################################

ks <- c(3,4,5)

resultados_finais <- list()

for(k in ks){
  cat("\n===========================\n")
  cat("Testando discretização k =", k, "\n")
  
  # aplicar discretização usando apenas o treino como referência
  res <- quantize_with_zero_bin(df, train_idx, k = k)
  df_cat <- res$df
  
  df_train_cat <- df_cat[train_idx, ]
  df_val_cat   <- df_cat[val_idx,   ]
  
  ###############################################
  # 6) SELEÇÃO DE VARIÁVEIS VIA MARKOV BLANKET
  ###############################################
  
  mb_vars <- mmpc(df_train_cat, target = "default")
  cat("Markov Blanket:", paste(mb_vars, collapse=", "), "\n")
  
  df_mb_train <- df_train_cat[, c(mb_vars, "default")]
  df_mb_val   <- df_val_cat[,   c(mb_vars, "default")]
  
  ###############################################
  # 7) APLICAR VÁRIOS ISS NA APRENDIZAGEM DE DAG
  ###############################################
  
  iss_values <- c(1,5,10,20)
  
  dags <- lapply(iss_values, function(iss){
    hc(df_mb_train, score="bde", iss=iss)
  })
  names(dags) <- paste0("iss_", iss_values)
  
  ###############################################
  # 8) AVALIAR CADA DAG E GUARDAR RESULTADOS
  ###############################################
  
  resultados_k <- lapply(dags, function(dag){
    avaliar_bn(dag, df_mb_train, df_mb_val)
  })
  
  resultados_finais[[paste0("k_",k)]] <- resultados_k
}


###############################################
# 9) CRIAR TABELA RESUMO
###############################################

library(tibble)

linha_resumo <- function(nome_k, lista_res){
  do.call(rbind, lapply(names(lista_res), function(nm){
    metr <- lista_res[[nm]]$metrics
    tibble(
      discret_k = nome_k,
      modelo = nm,
      auc = metr$auc,
      F1_best = metr$F1_best,
      prec_best = metr$prec_best,
      recall_best = metr$recall_best,
      best_threshold_F1 = metr$best_threshold_F1
    )
  }))
}

tabela <- do.call(rbind, lapply(names(resultados_finais), function(nm){
  linha_resumo(nm, resultados_finais[[nm]])
}))

print(tabela)

###############################################
# 10) SALVAR RESULTADOS
###############################################

write.csv(tabela, "resultados_bn_otimizados.csv", row.names=FALSE)

cat("\n\n=== Pipeline finalizado! ===\n")
