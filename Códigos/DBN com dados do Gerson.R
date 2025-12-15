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


df <- as.data.frame(fread("Dados/df_final_para_modelagem.csv", header = TRUE)) # sempre importante colocar em data.frame para lidar com o bnlearn
df <- df %>% select(-V1)
df <- df %>%
  mutate(across(where(is.character), as.factor))
str(df)

set.seed(123)
n <- nrow(df)
idx_all <- sample(n)

prop_train <- 0.6
prop_val   <- 0.2
prop_test  <- 0.2  # o resto

n_train <- floor(prop_train * n)
n_val   <- floor(prop_val   * n)
n_test  <- n - n_train - n_val

train_idx <- idx_all[1:n_train]
val_idx   <- idx_all[(n_train+1):(n_train+n_val)]
test_idx  <- idx_all[(n_train + n_val + 1):(n_train + n_val + n_test)]

df_train <- df[train_idx, ]
df_val   <- df[val_idx,   ]

make_blacklist_default <- function(df, target = "default") {
  data.frame(
    from = setdiff(colnames(df), target),
    to   = target,
    stringsAsFactors = FALSE
  )
}

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

blacklist <- make_blacklist_default(df_train)
resultados <- list()

# 1) Naive Bayes
cat(">> Naive Bayes\n")
dag <- naive.bayes(df_train, training="default")
graphviz.plot(dag)
resultados$naive_bayes <- avaliar_bn(dag, df_train, df_val, positive="yes")

# 2) TAN
cat(">> Tree Bayes (TAN) causa problema de memória\n")
dag <- tree.bayes(df_train, training="default")
graphviz.plot(dag)
resultados$tree_bayes <- avaliar_bn(dag, df_train, df_val, positive="yes")

# 3) HC BDe ISS=10
cat(">> HC (BDe, ISS=10)\n")
dag <- hc(df_train, score="bde", iss=10)
graphviz.plot(dag)
resultados$hc_bde <- avaliar_bn(dag, df_train, df_val, positive="yes")

# 4) Constraint-based (PC-stable APENAS)
cat(">> PC-stable\n")
cpdag <- pc.stable(df_train, undirected=FALSE)
dag <- cextend(cpdag)
graphviz.plot(cpdag)
resultados$pc_stable <- avaliar_bn(dag, df_train, df_val, positive="yes")

# 5)
cat(">> HC (BDs)\n")
dag <- hc(df_train, score = "bds")
graphviz.plot(dag)
resultados$hc_bds <- avaliar_bn(dag, df_train, df_val, positive="yes")

hc_bde_iss1  <- hc(df_train, score = "bde", iss = 1)
resultados$hc_bde_iss1 <- avaliar_bn(hc_bde_iss1, df_train, df_val, positive="yes")

hc_bde_iss10 <- hc(df_train, score = "bde", iss = 10)
resultados$hc_bde_iss10 <- avaliar_bn(hc_bde_iss10, df_train, df_val, positive="yes")

cpdag_hit <- si.hiton.pc(df_train, undirected = FALSE)
dag_hit   <- cextend(cpdag_hit)
resultados$dag_hit <- avaliar_bn(dag_hit, df_train, df_val, positive="yes")

print(
  purrr::imap_dfr(resultados, ~
                    tibble(modelo=.y,
                           auc=.x$metrics$auc,
                           F1=.x$metrics$F1_best))
)
