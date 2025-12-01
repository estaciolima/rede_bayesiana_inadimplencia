# recomendado para leitura de arquivos maiores
install.packages("data.table")
install.packages("skimr")

library(data.table)
library(dplyr)
library(stringr)
library(skimr)

pre_processar <- function(df) {
  # realizar preprocessamento no dataframe
  df_size <- dim(df)[1]
  
  # remover variáveis
  # url: inútil
  # as demais são duplicadas de outras
  df <- df %>% select(-url, 
                      -funded_amnt_inv,
                      -out_prncp_inv,
                      -total_pymnt_inv,
                      -title) 
  
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
  
  selecionar_variaveis <- c(character_vars$variable, numeric_vars$variable)
  df <- df %>% select(all_of(selecionar_variaveis))
  
  # substitui strings vazias com NA
  df <- df %>% mutate(across(where(is.character), ~ na_if(., "")))
  
  # Por fim, remove-se todas as linhas com algum valor faltante
  df <- df[complete.cases(df),]
  
  # criar variável default
  df <- df %>% mutate(default = if_else(loan_status == "Default",
                                        1,
                                        0)
                      )
  
  df <- df %>% select(-loan_status)
  
  return(df)
}

# carregar dados
df <- fread("Dados/dataset.csv")

# Abordagem (1)
df <- pre_processar(df)

# Após essa filtragem, a base ainda possui mais de 1.8 Mi de linhas, mais do que suficiente
# Souadda et. al (2025) restringiu sua análise ao ano de 2014. Pode ser interessante
# fazer isso para uma comparação.
df_2014 <- df %>% filter(str_detect(issue_d, "2014"))

# Abordagem (2)
# Na verdade, faz sentido filtrar a base original pelo ano de 2014.
# E então fazer as análises, já que pode haver mudanças ao longo dos anos.
# A primeira abordagem faz mais sentido caso o interesse seja considerar
# todos os anos. Inclusive, pode ser uma proposta para um projeto com uma DBN.
# Carreguei novamente o df para conter a base completa.
df_2014 <- df %>% filter(str_detect(issue_d, "2014"))

skim(df_2014)

df_2014 <- pre_processar(df_2014) # Resultou em uma variável a mais
df_2014 <- df_2014 %>% select(-issue_d)

# TODO: converter variávies com datas para numéricas apenas com anos
# TODO: Converter variáveis numéricas em categóricas


