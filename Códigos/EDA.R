# recomendado para leitura de arquivos maiores
install.packages("data.table")
install.packages("skimr")
install.packages("discretization")

library(data.table)
library(dplyr)
library(stringr)
library(skimr)
library(discretization)

pre_processar <- function(df) {
  # realizar preprocessamento no dataframe
  df_size <- dim(df)[1]
  
  # remover variáveis desnecessárias ou duplicadas
  df <- df %>% select(-url, 
                      -funded_amnt_inv, # duplicada
                      -out_prncp_inv, # duplicada
                      -total_pymnt_inv, # duplicada
                      -title,
                      -id,
                      -policy_code,
                      -earliest_cr_line,
                      -last_pymnt_d,
                      -last_credit_pull_d) 
  
  # verifica falores faltantes ou vazios
  res <- rbindlist(
    lapply(names(df), function(col) {
      x <- df[[col]]
      # cálculo vetorizado
      complete_rate <- mean(!is.na(x))
      # só checar empty strings em character (evita overhead desnecessário)
      character.empty <- if (is.character(x)) sum(x == "" & !is.na(x)) else 0L
      var_type <- if(is.character(x)) "character" else "numeric"
      list(variable = col, var_type = var_type, complete_rate = complete_rate, character.empty = character.empty)
    })
  )
  
  # Remover variáveis com muitos valores faltantes.
  # No caso de variáveis character, verifica-se se existem strings vazias.
  character_vars <- res %>% 
    filter(var_type == "character") %>% 
    mutate(empty_rate_char = character.empty/df_size) %>% 
    filter(empty_rate_char < 0.1) %>%
    select(variable)
  
  numeric_vars <- res %>% 
    filter(var_type == "numeric") %>% 
    filter(complete_rate > 0.9) %>% 
    select(variable)
  
  # seleciona-se as variáveis segundo esses critérios
  selecionar_variaveis <- c(character_vars$variable, numeric_vars$variable)
  df <- df %>% select(all_of(selecionar_variaveis))
  
  # substitui strings vazias com NA
  df <- df %>% mutate(across(where(is.character), ~ na_if(., "")))
  
  # Por fim, remove-se todas as linhas com algum valor faltante
  df <- df[complete.cases(df),]
  
  # criar variável default
  df <- df %>% mutate(default = if_else(loan_status %in% c("Default", "Charged Off", "Default Does not meet the credit policy. Status:Charged Off") ,
                                        1,
                                        0)) %>% 
    mutate(default = as.factor(default))
  
  df <- df %>% select(-loan_status)
  
  # converter colunas com data em colunas com ano do tipo numérico
  # df$earliest_cr_line <- as.numeric(str_extract(df$earliest_cr_line, "\\d{4}"))
  # df$last_pymnt_d <- as.numeric(str_extract(df$last_pymnt_d, "\\d{4}"))
  # df$last_credit_pull_d<- as.numeric(str_extract(df$last_credit_pull_d, "\\d{4}"))
  
  # coverte tudo que for character para factor
  df <- df %>% mutate(across(where(is.character), as.factor))
  
  # todas essas variáveis são numéricas, mas apresentam poucos valores únicos,
  # logo faz sentido convertê-las diretamente para fatores
  vars_numeric_para_factor <- c("inq_last_6mths",
                                "acc_now_delinq",
                                "chargeoff_within_12_mths",
                                "num_tl_120dpd_2m",
                                "num_tl_30dpd",
                                "pub_rec_bankruptcies")
  
  df <- df %>% mutate(across(all_of(vars_numeric_para_factor), as.factor))
  
  return(df)
}

# carregar dados
df <- fread("Dados/dataset.csv")

# Abordagem (2)
# Na verdade, faz sentido filtrar a base original pelo ano de 2014.
# E então fazer as análises, já que pode haver mudanças ao longo dos anos.
# A primeira abordagem faz mais sentido caso o interesse seja considerar
# todos os anos. Inclusive, pode ser uma proposta para um projeto com uma DBN.
# Carreguei novamente o df para conter a base completa.
df_2014 <- df %>% filter(str_detect(issue_d, "2014"))

df_2014 <- pre_processar(df_2014) # Resultou em uma variável a mais
df_2014 <- df_2014 %>% select(-issue_d)

paste("Proporção de exemplos de inadimplência na amostra:", mean(df_2014$default))

# TODO: Converter variáveis numéricas em categóricas
numeric_vars <- df_2014 %>% 
  select(where(is.numeric)) %>% 
  colnames()

# observar histograma de todas as variáveis
par(mfrow=c(3,3))

for (v in numeric_vars) {
  hist(df_2014[[v]], main = v, breaks = 30, xlab = "")
}

# Algumas variáveis possuem poucos valores distintos, observar:
unique_counts <- sapply(df_2014 %>% select(where(is.numeric)), function(x) length(unique(x)))
unique_counts

# Função segura para criar cuts com quantis reais
safe_cut <- function(x, k = 5, sample_x = NULL) {
  # x: vetor numérico
  # k: número desejado de bins
  # sample_x: vetor usado para calcular quantis (do treino)
  if (all(is.na(x))) return(list(is_factor = TRUE, factor_result = NA, breaks = NULL))
  uniq <- unique(na.omit(x))
  n_uniq <- length(uniq)
  
  # se poucos valores distintos -> tratar como factor
  if (n_uniq <= 1) {
    # tudo igual -> retorna factor de 1 nível
    return(list(is_factor = TRUE, factor_result = factor(x), breaks = NULL))
  } else if (n_uniq <= k) {
    # poucos níveis: manter como fator com níveis ordenados por valor
    return(list(is_factor = TRUE, factor_result = factor(x, levels = sort(uniq)), breaks = NULL))
  }
  
  # calcula cortes em sample_x (ou x se sample_x NULL)
  base_vec <- if (!is.null(sample_x)) sample_x else x
  probs <- seq(0, 1, length.out = k + 1)
  cuts <- unique(quantile(base_vec, probs = probs, na.rm = TRUE, type = 7))
  
  # se cuts gerou menos intervals (ties), reduzir k e recalcular até ter >=2 bins
  while (length(cuts) <= 2 && length(probs) > 2) {
    # reduzir k
    k <- max(2, floor((length(cuts)-1) * 0.8))  # tentativa conservadora
    probs <- seq(0, 1, length.out = k + 1)
    cuts <- unique(quantile(base_vec, probs = probs, na.rm = TRUE, type = 7))
  }
  # se ainda problema (extremo), fallback: equal width via range
  if (length(cuts) <= 2) {
    rng <- range(base_vec, na.rm = TRUE)
    cuts <- seq(rng[1], rng[2], length.out = 3) # 2 bins
  }
  
  # cria factor usando cut (aplicável a todo x)
  fac <- cut(x, breaks = cuts, include.lowest = TRUE, right = FALSE,
             labels = paste0("B", seq_len(length(cuts)-1)))
  return(list(is_factor = FALSE, factor_result = fac, breaks = cuts))
}

safe_cut(df_2014$percent_bc_gt_75)


