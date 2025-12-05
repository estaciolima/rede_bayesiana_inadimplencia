install.packages("data.table") # recomendado para leitura de arquivos maiores 
install.packages("skimr")
install.packages("discretization")
install.packages("ranger")
install.packages("purr")
install.packages("rlang")
install.packages("bnlearn")

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Rgraphviz")

library(data.table)
library(dplyr)
library(tibble)
library(stringr)
library(skimr)
library(discretization)
library(ranger)
library(purrr)
library(rlang)
library(bnlearn)
library(Rgraphviz)

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
  
  # TODO: vou remover todas as variáveis que são zero inflated.
  # Depois eu trato elas.
  df <- df %>% select(-pub_rec, -out_prncp, -recoveries)
  
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

# TODO: Converter variáveis numéricas em categóricas
numeric_vars <- df_2014 %>% 
  select(where(is.numeric)) %>% 
  colnames()

# observar histograma de todas as variáveis
plot_histograma <- function(df) {
  numeric_vars <- df %>% 
    select(where(is.numeric)) %>% 
    colnames()
  
  par(mfrow=c(3,3))
  
  for (v in numeric_vars) {
    hist(df[[v]], main = v, breaks = 30, xlab = "")
  }
}

plot_histograma(df)

# Algumas variáveis possuem poucos valores distintos, observar:
unique_counts <- sapply(df_2014 %>% select(where(is.numeric)), function(x) length(unique(x)))
unique_counts

################################################
# Verificar importânicia das variáveis antes de aplicar discretização porque
# a discretização tá sendo meio problemática, preciso agilizar as coisas.
set.seed(123)

# random forest
rf <- ranger(default ~ .,
             data = df_2014,
             num.trees = 300,
             importance = 'impurity',
             probability = FALSE,
             respect.unordered.factors = "order")

features_importance <- as.data.frame(rf$variable.importance) %>%
  rownames_to_column(var = "feature") %>% 
  rename(importance = "rf$variable.importance") %>% 
  arrange(desc(importance))

# verificar features_importance
dev.off() # resetar parâmetros dos gráficos
barplot(features_importance$importance[1:40], names.arg = 1:40)

# top k features
k <- 11

top_k <- features_importance$feature[1:k] 

df_reduced <- df_2014 %>% select(all_of(c(top_k, "default")))

plot_histograma(df_reduced)

# Quantização de variáveis
# Função principal: quantização com tratamento de zero-inflation
quantize_with_zero_bin <- function(df,
                                   vars = NULL,            # vetor de nomes; NULL -> todas numéricas
                                   k = 5,                  # número desejado de bins (além do bin "Zero")
                                   train_sample_size = 200000, # amostra para calcular quantis
                                   zero_label = "Zero",    # rótulo do bin para zeros
                                   min_unique_to_bin = 5   # se n_unique <= isto, converter em factor
) {
  df <- as_tibble(df)
  # escolher variáveis: todas numéricas por padrão
  if (is.null(vars)) {
    vars <- df %>% select(where(is.numeric)) %>% names()
  } else {
    # checar existência
    missing_vars <- setdiff(vars, names(df))
    if (length(missing_vars) > 0) stop("Variáveis não encontradas: ", paste(missing_vars, collapse = ", "))
  }
  
  n <- nrow(df)
  sample_size <- min(train_sample_size, n)
  set.seed(123)
  train_idx <- sample.int(n, sample_size)
  train_sample <- df %>% slice(train_idx)
  
  cuts_list <- list()
  df_out <- df
  
  for (col in vars) {
    x_full <- df[[col]]
    x_sample <- train_sample[[col]]
    
    # número de valores distintos (não NA) no conjunto inteiro e no subgrupo não-zero
    uniq_all <- unique(na.omit(x_full))
    n_unique_all <- length(uniq_all)
    
    # proporção de zeros
    prop_zero <- mean(x_full == 0, na.rm = TRUE)
    
    # se poucos níveis distintos -> tratar como factor (não criar bins)
    if (n_unique_all <= min_unique_to_bin) {
      message(glue::glue("[{col}] poucos níveis ({n_unique_all}) → convertendo para factor"))
      df_out <- df_out %>% mutate( !!sym(col) := as.factor(.data[[col]]) )
      cuts_list[[col]] <- NULL
      next
    }
    
    # Se existem zeros > 0, vamos criar um bin 'Zero' e calcular cortes só nos não-zero
    has_zero <- prop_zero > 0
    
    # vetor base para quantis: valores não-zero da amostra (se existirem)
    if (has_zero) {
      sample_nonzero <- x_sample[!is.na(x_sample) & x_sample != 0]
      full_nonzero <- x_full[!is.na(x_full) & x_full != 0]
    } else {
      sample_nonzero <- x_sample[!is.na(x_sample)]
      full_nonzero <- x_full[!is.na(x_full)]
    }
    
    # se não há dados não-zero suficientes, tratar como factor
    if (length(unique(sample_nonzero)) <= 1 || length(sample_nonzero) < 10) {
      # fallback: se quase tudo zero ou muito pouco variabilidade -> factor
      message(glue::glue("[{col}] quase sem valores não-zero ou pouca variabilidade -> convertendo para factor"))
      df_out <- df_out %>% mutate( !!sym(col) := as.factor(.data[[col]]) )
      cuts_list[[col]] <- NULL
      next
    }
    
    # calcular cortes por quantis (em sample_nonzero)
    probs <- seq(0, 1, length.out = k + 1)
    cuts <- unique(quantile(sample_nonzero, probs = probs, na.rm = TRUE, type = 7))
    
    # se os cortes colapsarem (muitos ties), reduzir k até conseguir pelo menos 2 intervals
    k_cur <- k
    while (length(cuts) <= 2 && k_cur > 2) {
      k_cur <- k_cur - 1
      probs <- seq(0, 1, length.out = k_cur + 1)
      cuts <- unique(quantile(sample_nonzero, probs = probs, na.rm = TRUE, type = 7))
    }
    # fallback se ainda falhar: criar 2 bins via range
    if (length(cuts) <= 2) {
      rng <- range(sample_nonzero, na.rm = TRUE)
      cuts <- seq(rng[1], rng[2], length.out = 3)
    }
    
    # garantir que cortes sejam aplicáveis ao full_nonzero (expandir limites se necessário)
    # acrescentar -Inf e +Inf para garantir cobertura e evitar NA por valores fora do range
    # mas preferimos usar include.lowest = TRUE e right = FALSE
    # montar labels
    n_bins_nonzero <- length(cuts) - 1
    labels_nonzero <- paste0("B", seq_len(n_bins_nonzero))
    
    # aplicar: criar coluna nova col_q (factor)
    new_col <- paste0(col, "_q")
    df_out <- df_out %>%
      mutate( !!sym(new_col) := case_when(
        is.na(.data[[col]]) ~ NA_character_,
        (.data[[col]] == 0 & has_zero) ~ zero_label,
        TRUE ~ as.character(cut(.data[[col]],
                                breaks = cuts,
                                include.lowest = TRUE,
                                right = FALSE,
                                labels = labels_nonzero))
      ))
    
    # transformar em factor com níveis ordenados: Zero (se existir) + B1..Bn
    if (has_zero) {
      levels_order <- c(zero_label, labels_nonzero)
    } else {
      levels_order <- labels_nonzero
    }
    df_out <- df_out %>% mutate( !!sym(new_col) := factor(.data[[new_col]], levels = levels_order, ordered = TRUE) )
    
    # salvar cuts (incluindo flag has_zero e labels) para re-aplicação
    cuts_list[[col]] <- list(cuts = cuts, has_zero = has_zero, labels = levels_order)
  } # fim loop cols
  
  return(list(df = df_out, cuts = cuts_list))
}

# Função auxiliar para reaplicar cuts salvos em um novo dataset (ex.: teste/prod)
apply_saved_cuts <- function(df_new, cuts_list, zero_label = "Zero") {
  df_new <- as_tibble(df_new)
  for (col in names(cuts_list)) {
    info <- cuts_list[[col]]
    if (is.null(info)) next
    cuts <- info$cuts
    has_zero <- info$has_zero
    labels <- info$labels
    new_col <- paste0(col, "_q")
    
    df_new <- df_new %>%
      mutate( !!sym(new_col) := case_when(
        is.na(.data[[col]]) ~ NA_character_,
        (.data[[col]] == 0 & has_zero) ~ zero_label,
        TRUE ~ as.character(cut(.data[[col]],
                                breaks = cuts,
                                include.lowest = TRUE,
                                right = FALSE,
                                labels = labels[labels != zero_label]))
      )) %>%
      mutate( !!sym(new_col) := factor(.data[[new_col]], levels = labels, ordered = TRUE))
  }
  return(df_new)
}


# aplicar no meu banco
res <- quantize_with_zero_bin(df_reduced, k = 3, train_sample_size=10000000)
df_reduced_cat <- res$df

df_factor_only <- df_reduced_cat %>% 
  select(where(~ !is.numeric(.x)))

df_factor_only <- as.data.frame(df_factor_only)

# temos k + 1 variáveis: top k preditoras + default
# vamos fazer a DAG agora
# Método por score:
dev.off()
bn.bds <- hc(df_factor_only, score = "bds")
graphviz.plot(bn.bds)
bn.bds2 <- hc(df_factor_only, score = "bds", iss = 10) # não sei o que é esse iss
graphviz.plot(bn.bds2)

bn.bdeu <- hc(df_factor_only, score = "bde", iss = 10)
graphviz.plot(bn.bdeu)

bn.restart = hc(df_factor_only, score = "bde", iss = 1, restart = 10, perturb = 5)
graphviz.plot(bn.restart)

# bnlearn implements several constraint-based algorithms, each with its own 
#function: pc.stable, gs(), iamb(), mmpc(), si.hiton.pc(), etc.
cpdag = si.hiton.pc(df_factor_only, undirected = FALSE)
graphviz.plot(cpdag)
cpdag2 = pc.stable(df_factor_only, undirected = FALSE)
graphviz.plot(cpdag2)
cpdag3 = gs(df_factor_only, undirected = FALSE)
graphviz.plot(cpdag3)
cpdag4 = mmpc(df_factor_only, undirected = FALSE)
graphviz.plot(cpdag4)

