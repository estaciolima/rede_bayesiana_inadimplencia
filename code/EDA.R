library(tidyverse) 
library(rlang)

#
# carregar dados
#
df <- arrow::read_parquet("data/amostra250k.parquet")

# tudo que for vazio transforma NA
df <- df %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))

df %>% glimpse()

#
### Definir a target
#
df$purpose %>% janitor::tabyl() %>% arrange(desc(n))
df$loan_status %>% janitor::tabyl() %>% arrange(desc(n))

df %>% 
  select(purpose, loan_status)  %>%
  janitor::tabyl(purpose, loan_status) 

df <- df %>% 
  mutate(default = if_else(
    loan_status %in% c("Charged Off", "Default", "Late (31-120 days)", 
                       "Does not meet the credit policy. Status:Charged Off"), 1, 0)
  ) %>%
  select(-loan_status)

df$default %>% janitor::tabyl() %>% arrange(desc(n)) 

tab = df %>%
  group_by(default) %>%
  summarise(n = n()) %>%
  mutate(percentual = n / sum(n) * 100)

tab$default <- if_else(tab$default == 1, 'Yes', 'No')

# gerar grafico DEFAULT com o percentual e com um apecto de artico 
# inserir legenda dos valores
# remover o eixo y  
ggplot(tab, aes(x = factor(default), y = percentual, fill = factor(default))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentual, 0), "%")), vjust = -0.1) +
  labs(title = "",
       subtitle = "",
       x = "Inadimplência",
       y = "(%)") +
  scale_fill_manual(values = c("No" = "steelblue", "Yes" = "tomato")) +
  theme_minimal() +
  theme(legend.position = "none")

#
# Remover variáveis irrelevantes
#
rm_col <- read.csv('data/rm_vars_leakage.csv') 
rm_col <- rm_col$vars_leakage
df <- df %>% select(-all_of(rm_col))

# Variaveis com mais de 60% de dados faltantes 
DataExplorer::plot_missing(df)

info_df <- skimr::skim(df) 
col_na50 <- info_df %>% 
  filter(complete_rate < 0.6) %>% 
  pull(skim_variable)

df <- df %>% select(-all_of(col_na50))

DataExplorer::plot_missing(df)

### ETL 
source('code/funcoes.R')
#
# Tratar variaveis com cardialidade alta 
#
rm_var_nivel_unico <- function(data) {

  colunas_para_manter <- data |>
    purrr::map_lgl(~ length(unique(.x)) > 1)
  
  data[, ..colunas_para_manter]
}

df <- rm_var_nivel_unico(df)

# Avaliar cardianilidade das variáveis categóricas
cat_vars <- df %>% select(where(is.character)) %>% colnames()
card_cat_vars <- sapply(df %>% select(all_of(cat_vars), -id), function(x) length(unique(x)))
card_cat_vars <- sort(card_cat_vars)
barplot(
  card_cat_vars, 
  las=2,
  cex.names=0.7,
  horiz = TRUE,
  main = 'Cardinalidade das variáveis categóricas'
  )

var_card_bins <- card_cat_vars[card_cat_vars > 5]
var_card_bins %>% names()

df %>% select(all_of(names(var_card_bins)) , default) %>% view()

col_select <- c('zip_code', 'home_ownership','emp_title', 'grade', 'emp_length') 

df$grade %>% janitor::tabyl() 
df$home_ownership %>% janitor::tabyl() 
df$emp_length %>% janitor::tabyl() 

df <- df %>% 
  mutate(
    grade = factor(grade, levels = c('A','B','C','D','E','F','G'), ordered = TRUE),
    home_is_rent = if_else(home_ownership == 'RENT', 'yes', 'no'),
    # Detect o numero do string e se for menos que 4, 0 a 4 anos, 4 a 9 anos, mais que 10 anos
    emp_length = case_when(
      emp_length %in% c('< 1 year', '1 year', '2 years', '3 years') ~ '0-4 years',
      emp_length %in% c('4 years', '5 years', '6 years', '7 years', '8 years', '9 years') ~ '4-9 years',
      emp_length %in% c('10+ years') ~ '10+ years',
      TRUE ~ 'Others'
    ),
    emp_title = if_else(is.na(emp_title) | emp_title == "", "Others", emp_title) %>% 
      str_trim() %>% str_to_lower()
    ) %>% select(-home_ownership)

# df$emp_title <-janitor::make_clean_names(df$emp_title)

cat_vars_to_bin <- c('zip_code', 'emp_title') 
VAR_TARGET <- "default" 
N_BINS <- 3

dfT <- cat_vars_to_bin %>%
  purrr::reduce(
    .x = ., # A lista de var_cat
    .f = function(data, var_cat_atual) {
      
      message(paste("Processando Target Encoding para:", var_cat_atual))
      
      df_resultante <- create_risk_bins(
        df = data,
        var_cat = var_cat_atual,
        var_target = VAR_TARGET,
        n_bins = N_BINS
      )
    
      return(df_resultante)
      
    },
    .init = df 
  )

DataExplorer::plot_bar(dfT %>% select(Risco_zip_code_BIN,Risco_emp_title_BIN, grade,emp_length, default), by = "default")

dfT <- dfT %>% select(-Risco_emp_title_BIN)

#
# Avaliacao
#
#DataExplorer::plot_bar(dfT %>% select(where(is.character)))
#DataExplorer::plot_histogram(dfT %>% select(where(is.numeric)))

#DataExplorer::plot_bar(dfT %>% select(where(is.character),VAR_TARGET), by = VAR_TARGET)
#DataExplorer::plot_boxplot(dfT %>% select(where(is.numeric),VAR_TARGET), by = VAR_TARGET)

# 
# Alta colinearidade
#
DataExplorer::plot_correlation(
  dfT %>% select(where(is.numeric)), 
  cor_args = list(use = "pairwise.complete.obs")
  )

# ver grupo com alta colineliaridade
variaveis_para_remover <- rm_var_alta_corr(
  df = dfT,
  target_var = "default",
  limite_cor_feature = 0.6
)

print(variaveis_para_remover)

variaveis_para_remover <- setdiff(variaveis_para_remover, c("loan_amnt", "installment"))

if (length(variaveis_para_remover) > 0) {
  dfT <- dfT %>% 
    select(-all_of(variaveis_para_remover))
  
  cat("\nColunas no DataFrame Final (sem redundância):", names(dfT), "\n")
}

DataExplorer::plot_correlation(dfT %>% select(where(is.numeric)), cor_args = list(use = "pairwise.complete.obs"))

# Selecao prelimiar de variaveis 

col_select <- c(
  'dti','installment','home_is_rent','grade',
  'last_fico_range_high', 'pct_tl_nvr_dlq', 'loan_amnt',
  'Risco_zip_code_BIN', 'mo_sin_old_rev_tl_op', 'annual_inc', 
  'inq_last_6mths', 'fico_range_low',
  'default'
)

dfT <- dfT %>% select(all_of(col_select))

#DataExplorer::plot_bar(dfT %>% select(where(is.character),VAR_TARGET), by = VAR_TARGET)
#DataExplorer::plot_boxplot(dfT %>% select(where(is.numeric),VAR_TARGET), by = VAR_TARGET)

#
# Selecao parametrica 
# 
write_csv(dfT, 'data/df_pre_processado.csv')

# df <- dfT %>% filter(str_detect(issue_d, "2014"))
df <- read_csv("data/df_pre_processado.csv") %>% janitor::clean_names()

skimr::skim(df)
DataExplorer::plot_missing(df)

#df <- df %>% sample_n(100000)

# NA -> Mediana 
df <- df |>
  mutate(across(.cols = where(is.numeric),.fns = ~ replace_na(.x, median(.x, na.rm = TRUE)) ))

df$default <- if_else(df$default == 1, 'yes', 'no') %>% as.factor()

df <- df |>
  mutate(across(.cols = where(is.numeric),.fns = ~ replace_na(.x, median(.x, na.rm = TRUE)) ))

df <- df %>%
  mutate(across(.cols = where(is.character), .fns = as.factor))

variaveis_numericas <- names(df) %>%
  setdiff(c("default", "home_is_rent", "grade", "risco_zip_code_bin", "pct_tl_nvr_dlq", "inq_last_6mths", "term")) 

df <- df %>% select(-c("pct_tl_nvr_dlq", "inq_last_6mths"))

df <- df %>%
  mutate(
    across(
      .cols = all_of(variaveis_numericas),
      # Chamada da função: x = valor da coluna atual; y = valor do target (df$default)
      .fns = ~ discretizar_var_num(x = ., y = df$default, n_min = 2, n_max = 4),
      .names = "{.col}_disc"
    )
  )


df <- df %>%
  select(-all_of(variaveis_numericas))

df %>% glimpse()

write.csv(df, 'data/df_final_para_modelagem.csv')

DataExplorer::plot_bar(df, by = "default")

# Parei aqui 
