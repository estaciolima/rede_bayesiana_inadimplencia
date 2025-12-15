library(dplyr)
library(tidymodels)
library(bnlearn)
library(pROC)
# BiocManager::install("Rgraphviz")
library(Rgraphviz)

set.seed(123)

# Carregar e preparar os dados
df <- read.csv("data/df_final_para_modelagem.csv")

df <- dplyr::select(df, -X)

df <- df %>%
  dplyr::mutate(across(.cols = where(is.character), .fns = as.factor))

#
# Modelo Logístico
#
split <- initial_split(df, prop = 0.8, strata = default)
train <- training(split)
test  <- testing(split)

recipe_logit <- recipe(default ~ ., data = train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

logit_model <- logistic_reg() %>%
  set_engine("glm")

wf_logit <- workflow() %>%
  add_recipe(recipe_logit) %>%
  add_model(logit_model)

fit_logit <- fit(wf_logit, train)

pred_logit <- predict(fit_logit, test, type = "prob") %>%
  bind_cols(test)

auc_logit <- roc_auc(pred_logit, truth = default, .pred_yes)

#
# Modelo Baysiano de Redes 
#
dag <- hc(train)
# dag <- tabu(train)

blacklist <- data.frame(
  from = "default",
  to   = setdiff(names(train), "default")
)

dag <- hc(train, blacklist = blacklist)

# -------------
subgraphs <- list(
  Perfil = c(
    "risco_zip_code_bin",
    "home_is_rent",
    "mo_sin_old_rev_tl_op_disc"
  ),
  
  Financeiro = c(
    "annual_inc_disc",
    "dti_disc"
  ),
  
  Score = c(
    "fico_range_low_disc",
    "last_fico_range_high_disc"
  ),
  
  Contrato = c(
    "grade",
    "loan_amnt_disc",
    "installment_disc"
  ),
  
  Default = "default"
)

graphviz.plot(
  dag,
  layout = "dot",
  groups = subgraphs,
  highlight = list(nodes = "default",col = "tomato", fill = "orange"),
  main = "DAG - Risco de Crédito"
)


# ------------
parents(dag, "default")
#O evento de default é diretamente influenciado pela qualidade do crédito (grade),
#o valor do emprestimo (loan_amnt_disc) e score de crédito (FICO), 
#mesmo após controlar pelas demais variáveis.

bn_fit <- bn.fit(dag, data = train)

predict_bn_prob <- function(bn_fit, newdata, target = "default") {
    sapply(seq_len(nrow(newdata)), function(i) {
    cpquery(
      bn_fit,
      event = (default == "yes"),
      evidence = as.list(newdata[i, setdiff(names(newdata), target)]),
      method = "lw"
    )
  })
}

prob_bn <- predict_bn_prob(bn_fit, test)

pred_bn <- test %>%
  dplyr::mutate(.pred_yes = prob_bn)

auc_bn <- roc_auc(pred_bn, truth = default, .pred_yes)
auc_bn

# Naive Bayes (opcional)
dag_naive <- naive.bayes(train, training = "default")
naive_fit <- bn.fit(dag, data = train, method = "bayes")

predict_naiveb_prob <- function(naive_fit, newdata, target = "default") {
  sapply(seq_len(nrow(newdata)), function(i) {
    cpquery(
      naive_fit,
      event = (default == "yes"),
      evidence = as.list(newdata[i, setdiff(names(newdata), target)]),
      method = "lw"
    )
  })
}

pred_naive <- predict_naiveb_prob(naive_fit, test)

pred_naive <- test %>%
  dplyr::mutate(.pred_yes = pred_naive)

auc_naive <- roc_auc(pred_naive, truth = default, .pred_yes)
auc_naive

## 
## Tree-Augmented Naive Bayes (TAN)

dag_tan <- tree.bayes(train, training = "default")

fit_tan <- bn.fit(dag_tan, data = train, method = "bayes")

pred_tan <- tibble::tibble(
  .pred_yes = sapply(1:nrow(test), function(i) {
    cpquery(
      fit_tan,
      event = (default == "yes"),
      evidence = as.list(test[i, setdiff(names(test), "default")]),
      method = "lw"
    )
  })
) %>%
  bind_cols(test)

auc_tan <- yardstick::roc_auc(
  pred_tan,
  truth = default,
  .pred_yes
)$.estimate




# Grafico curva ROC
library(pROC)
library(ggplot2)

roc_logit <- roc(
  response = pred_logit$default,
  predictor = pred_logit$.pred_yes,
  levels = c("no", "yes"),
  direction = "<"
)

roc_bn <- roc(
  response = pred_bn$default,
  predictor = pred_bn$.pred_yes,
  levels = c("no", "yes"),
  direction = "<"
)

roc_naive <- roc(
  response = pred_naive$default,
  predictor = pred_naive$.pred_yes,
  levels = c("no", "yes"),
  direction = "<"
)

roc_tan <- roc(
  response = pred_tan$default,
  predictor = pred_tan$.pred_yes,
  levels = c("no", "yes"),
  direction = "<"
)


df_roc_logit <- data.frame(
  fpr = 1 - roc_logit$specificities,
  tpr = roc_logit$sensitivities,
  model = "Regressão Logística"
)

df_roc_bn <- data.frame(
  fpr = 1 - roc_bn$specificities,
  tpr = roc_bn$sensitivities,
  model = "Rede Bayesiana (DAG)"
)

df_roc_naive <- data.frame(
  fpr = 1 - roc_naive$specificities,
  tpr = roc_naive$sensitivities,
  model = "Naive Bayes"
)

df_roc_tan <- data.frame(
  fpr = 1 - roc_tan$specificities,
  tpr = roc_tan$sensitivities,
  model = "Tree-Augmented Naive Bayes (TAN)"
)

df_roc <- rbind(df_roc_logit, df_roc_bn, df_roc_naive,df_roc_tan)
auc_logit <- auc(roc_logit)
auc_bn    <- auc(roc_bn)
auc_naive    <- auc(roc_naive)
auc_tan <- auc(roc_tan)


ggplot(df_roc, aes(x = fpr, y = tpr, color = model)) +
  geom_line(size = .7) +
  geom_abline(
    intercept = 0, slope = 1,
    linetype = "dashed", color = "gray50"
  ) +
  labs(
    title = "Curva ROC — Comparação de Modelos",
    caption =  paste0(
      "AUC Logística = ", round(auc_logit, 3),
      " | AUC Rede Bayesiana = ", round(auc_bn, 3),
      " | AUC Naive Bayes = ", round(auc_naive, 3),
      " | AUC TAN = ", round(auc_tan, 3)
    ),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Modelo"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

ggsave("img/roc_comparacao_modelos.png", width = 8, height = 6)

## matrix de confussão 
threshold <- 0.5

pred_logit <- pred_logit %>%
  dplyr::mutate(
    pred_class = ifelse(.pred_yes >= threshold, "yes", "no"),
    pred_class = factor(pred_class, levels = c("no", "yes"))
  )

pred_bn <- pred_bn %>%
  dplyr::mutate(
    pred_class = ifelse(.pred_yes >= threshold, "yes", "no"),
    pred_class = factor(pred_class, levels = c("no", "yes"))
  )

pred_naive <- pred_naive %>%
  dplyr::mutate(
    pred_class = ifelse(.pred_yes >= threshold, "yes", "no"),
    pred_class = factor(pred_class, levels = c("no", "yes"))
  )

pred_tan <- pred_tan %>%
  mutate(
    pred_class = ifelse(.pred_yes >= threshold, "yes", "no"),
    pred_class = factor(pred_class, levels = c("no", "yes"))
  )


library(yardstick)
conf_mat_logit <- conf_mat(
  pred_logit,
  truth = default,
  estimate = pred_class
)

conf_mat_bn <- conf_mat(
  pred_bn,
  truth = default,
  estimate = pred_class
)

conf_mat_naive <- conf_mat(
  pred_naive,
  truth = default,
  estimate = pred_class
)

conf_mat_tan <- conf_mat(
  pred_tan,
  truth = default,
  estimate = pred_class
)

acc_logit <- yardstick::accuracy(pred_logit, truth = default, estimate = pred_class)
acc_bn    <- yardstick::accuracy(pred_bn, truth = default, estimate = pred_class)
acc_naive <- yardstick::accuracy(pred_naive, truth = default, estimate = pred_class)
acc_tan    <- yardstick::accuracy(pred_tan, truth = default, estimate = pred_class)

f1_logit <- f_meas(
  pred_logit,
  truth = default,
  estimate = pred_class,
  event_level = "second"
)

f1_bn <- f_meas(
  pred_bn,
  truth = default,
  estimate = pred_class,
  event_level = "second"
)

f1_naive <- f_meas(
  pred_naive,
  truth = default,
  estimate = pred_class,
  event_level = "second"
)

f1_tan <- f_meas(
  pred_tan,
  truth = default,
  estimate = pred_class,
  event_level = "second"
)

results <- tibble::tibble(
  Modelo = c("Regressão Logística", "Rede Bayesiana (DAG)", "Naive Bayes", "Tree-Augmented Naive Bayes"),
  Acuracia = c(acc_logit$.estimate, acc_bn$.estimate, acc_naive$.estimate, acc_tan$.estimate),
  F1_Score = c(f1_logit$.estimate, f1_bn$.estimate, f1_naive$.estimate, f1_tan$.estimate),
  AUC = c(auc_logit, auc_bn, auc_naive, auc_tan)
)

results

# 
# Curva KS
plot_ks <- function(
    data,
    truth,
    prob_col = ".pred_yes",
    positive = "yes",
    titulo = "Curva KS"
) {
  
  library(dplyr)
  library(ggplot2)
  
  truth <- rlang::ensym(truth)
  
  df_ks <- data %>%
    dplyr::mutate(
      prob = .data[[prob_col]],
      evento = ifelse(!!truth == positive, 1, 0)
    ) %>%
    dplyr::arrange(prob) %>%
    dplyr::mutate(
      cum_evento = cumsum(evento) / sum(evento),
      cum_nao_evento = cumsum(1 - evento) / sum(1 - evento)
    )
  
  ks_value <- max(abs(df_ks$cum_evento - df_ks$cum_nao_evento), na.rm = TRUE)
  
  p <- ggplot(df_ks) +
    geom_line(aes(x = prob, y = cum_evento, color = "no")) +
    geom_line(aes(x = prob, y = cum_nao_evento, color = "yes")) +
    geom_vline(
      xintercept = df_ks$prob[which.max(abs(df_ks$cum_evento - df_ks$cum_nao_evento))],
      linetype = "dashed"
    ) +
    labs(
      x = "Probabilidade predita",
      y = "Distribuição acumulada",
      color = "",
      title = titulo,
      subtitle = paste0("KS = ", round(ks_value, 3))
    ) 
  
  list(
    ks = ks_value,
    plot = p,
    data = df_ks
  )
}

ks_logit <- plot_ks(
  data   = pred_logit,
  truth  = default,
  titulo = "Regressão Logística"
)

ks_bn <- plot_ks(
  data   = pred_bn,
  truth  = default,
  titulo = "Rede Bayesiana"
)

ks_naive <- plot_ks(
  data   = pred_naive,
  truth  = default,
  titulo = "Naive Bayes"
)

ks_tan <- plot_ks(
  data   = pred_tan,
  truth  = default,
  titulo = "Tree-Augmented Naive Bayes"
)

gridExtra::grid.arrange(
  ks_logit$plot,
  ks_bn$plot,
  ks_naive$plot,
  ks_tan$plot,
  ncol = 2
)
  