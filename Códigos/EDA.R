# recomendado para leitura de arquivos maiores
install.packages("data.table")
install.packages("skimr")

library(data.table)
library(dplyr)

df <- fread("Dados/dataset.csv")

df_size <- dim(df)[1]

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

# Filtrar dataset por essas variáveis apenas.
# Manter com o mesmo nome em razão do tamanho do dataset para otimizar a memória que não é abundante no meu PC
df <- df %>% select(all_of(selecionar_variaveis))

# Por fim, remove-se todas as linhas com algum valor faltante
df <- df[complete.cases(df),]
