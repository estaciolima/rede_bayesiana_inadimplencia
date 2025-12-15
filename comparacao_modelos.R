# 1. Carregar Pacotes
library(dplyr)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(ranger)
library(discrim)
library(klaR)

# 2. Ler e Preparar os Dados
set.seed(42) # Para reprodutibilidade

df_data <- read.csv("data/df_final_para_modelagem.csv") %>%
  dplyr::select(-X) # Remove a coluna de índice
names(df_data)
data_split <- initial_split(df_data, prop = 0.80, strata = default)
df_train <- training(data_split)
df_test <- testing(data_split)

# 3. Definir o Recipe (Pré-processamento)
credit_recipe <- recipe(default ~ ., data = df_train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>% 
  step_mutate(across(.cols = where(is.character), .fns = as.factor)) 

# 4. Definir os Modelos
# A) Regressão Logística
lr_model <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# B) Random Forest
rf_model <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("classification")

# C) Naive Bayes
#nb_model <- naive_Bayes() %>%
#  set_engine("klaR") %>%
#  set_mode("classification")

# 5. Criar os Workflows (Pipeline)
lr_workflow <- workflow() %>%
  add_recipe(credit_recipe) %>%
  add_model(lr_model)

rf_workflow <- workflow() %>%
  add_recipe(credit_recipe) %>%
  add_model(rf_model)

# nb_workflow <- workflow() %>%
#  add_recipe(credit_recipe) %>%
#  add_model(nb_model)


# 6. Ajustar (Treinar) os Modelos
print("Treinando Regressão Logística...")
lr_fit <- lr_workflow %>% fit(data = df_train)

print("Treinando Random Forest...")
rf_fit <- rf_workflow %>% fit(data = df_train)

#print("Treinando Naive Bayes...")
#nb_fit <- nb_workflow %>% fit(data = df_train)

# 7. Coletar Previsões de Probabilidade 
make_predictions <- function(model_fit, model_name, test_data = df_test) {
  predict(model_fit, new_data = df_test, type = "prob") %>%
    dplyr::rename(.pred_yes = .pred_yes) %>% 
    dplyr::bind_cols(df_test["default"]) %>% 
    dplyr::mutate(model = model_name)
}

# Previsões para todos os modelos
lr_prob <- make_predictions(lr_fit, "Regressão Logística", test_data = df_test)
rf_prob <- make_predictions(rf_fit, "Random Forest", test_data = df_test)
#nb_prob <- make_predictions(nb_fit, "Naive Bayes", test_data = df_test)

all_predictions <- bind_rows(lr_prob, rf_prob) # , nb_prob)

# 8. Comparação e Avaliação dos Modelos
metrics_comparison <- all_predictions %>%
  group_by(model) %>%
  roc_auc(truth = default, .pred_yes)

metrics_comparison

roc_data <- all_predictions %>%
  mutate(default = factor(default, levels = c("yes", "no"))) %>%
  dplyr::group_by(model) %>%
  roc_curve(truth = default, .pred_yes) %>%
  ungroup()


ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line(linewidth = 1, show.legend = TRUE) +
  geom_abline(lty = 2, color = "gray50") + # Linha de 45 graus (modelo aleatório)
  labs(
    title = "Curva ROC - Comparação de Modelos",
    color = "Modelo",
    x = "Taxa de Falsos Positivos (1 - Especificidade)",
    y = "Taxa de Verdadeiros Positivos (Sensibilidade)"
  ) 

# Salve o gráfico (opcional)
ggsave("img/curva_roc_comparacao.png", width = 8, height = 6)

# 9. Gerar Matrizes de Confusão

get_confusion_matrix_caret <- function(model_fit, model_name, test_data) {
  
  class_predictions <- predict(model_fit, new_data = test_data, type = "class")
  
  # CORREÇÃO CRÍTICA: Garantir que a variável alvo seja um fator
  truth <- factor(test_data$default, levels = c("yes", "no")) 
  
  prediction <- class_predictions$.pred_class
  
  conf_matrix <- caret::confusionMatrix(
    data = prediction, 
    reference = truth, 
    positive = "yes"
  )
  
  print(paste("--- Matriz de Confusão para:", model_name, "---"))
  print(conf_matrix)
}

# Aplicar a função para cada modelo:
get_confusion_matrix_caret(lr_fit, "Regressão Logística", df_test)
get_confusion_matrix_caret(rf_fit, "Random Forest", df_test)
get_confusion_matrix_caret(nb_fit, "Naive Bayes", df_test)

