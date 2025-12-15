library(dplyr)
library(tidymodels)
library(bnlearn)
library(pROC)

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
auc_logit

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

library(bnlearn)
# BiocManager::install("Rgraphviz")
library(Rgraphviz)
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

df_roc <- rbind(df_roc_logit, df_roc_bn)

auc_logit <- auc(roc_logit)
auc_bn    <- auc(roc_bn)

ggplot(df_roc, aes(x = fpr, y = tpr, color = model)) +
  geom_line(size = 1) +
  geom_abline(
    intercept = 0, slope = 1,
    linetype = "dashed", color = "gray50"
  ) +
  labs(
    title = "Curva ROC — Comparação de Modelos",
    caption =  paste0(
      "AUC Logística = ", round(auc_logit, 3),
      " | AUC Rede Bayesiana = ", round(auc_bn, 3)
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
  mutate(
    pred_class = ifelse(.pred_yes >= threshold, "yes", "no"),
    pred_class = factor(pred_class, levels = c("no", "yes"))
  )

pred_bn <- pred_bn %>%
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

acc_logit <- accuracy(pred_logit, truth = default, estimate = pred_class)
acc_bn    <- accuracy(pred_bn, truth = default, estimate = pred_class)

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

results <- tibble::tibble(
  Modelo = c("Regressão Logística", "Rede Bayesiana (DAG)"),
  Acuracia = c(acc_logit$.estimate, acc_bn$.estimate),
  F1_Score = c(f1_logit$.estimate, f1_bn$.estimate),
  AUC = c(auc_logit, auc_bn)
)

results

