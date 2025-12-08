#### Funções auxiliares ####
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

# plotar histograma de variáveis numéricas
plot_histograma <- function(df) {
  numeric_vars <- df %>% 
    select(where(is.numeric)) %>% 
    colnames()
  
  par(mfrow=c(3,3))
  
  for (v in numeric_vars) {
    hist(df[[v]], main = v, breaks = 30, xlab = "")
  }
}

# Quantização de variáveis
# Função principal: quantização com tratamento de zero-inflation
quantize_with_zero_bin <- function(df,
                                   train_idx,              # índices da amostra de treino
                                   vars = NULL,            # vetor de nomes; NULL -> todas numéricas
                                   k = 5,                  # número desejado de bins (além do bin "Zero")
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
    if (length(missing_vars) > 0)
      stop("Variáveis não encontradas: ", paste(missing_vars, collapse = ", "))
  }
  
  n <- nrow(df)
  train_df <- df %>% slice(train_idx)   # apenas treino
  
  cuts_list <- list()
  df_out <- df
  
  for (col in vars) {
    x_train <- train_df[[col]]   # só treino
    x_full  <- df[[col]]         # base completa (usada só na aplicação dos cortes)
    
    # número de valores distintos (não NA) no TREINO
    uniq_train <- unique(na.omit(x_train))
    n_unique_train <- length(uniq_train)
    
    # proporção de zeros NO TREINO
    prop_zero_train <- mean(x_train == 0, na.rm = TRUE)
    
    # se poucos níveis distintos no treino -> tratar como factor (não criar bins)
    if (n_unique_train <= min_unique_to_bin) {
      message(sprintf("[%s] poucos níveis no treino (%d) → convertendo para factor", 
                      col, n_unique_train))
      df_out <- df_out %>%
        mutate( !!sym(col) := as.factor(.data[[col]]) )
      cuts_list[[col]] <- NULL
      next
    }
    
    # Se existem zeros no treino, vamos criar um bin 'Zero' e calcular cortes só nos não-zero
    has_zero <- prop_zero_train > 0
    
    # vetor base para quantis: valores NÃO-ZERO do TREINO (se existirem)
    if (has_zero) {
      sample_nonzero <- x_train[!is.na(x_train) & x_train != 0]
    } else {
      sample_nonzero <- x_train[!is.na(x_train)]
    }
    
    # se não há dados não-zero suficientes no treino, tratar como factor
    if (length(unique(sample_nonzero)) <= 1 || length(sample_nonzero) < 10) {
      message(sprintf("[%s] quase sem valores não-zero ou pouca variabilidade no treino -> factor", col))
      df_out <- df_out %>%
        mutate( !!sym(col) := as.factor(.data[[col]]) )
      cuts_list[[col]] <- NULL
      next
    }
    
    # calcular cortes por quantis (em sample_nonzero do TREINO)
    probs <- seq(0, 1, length.out = k + 1)
    cuts <- unique(quantile(sample_nonzero, probs = probs, na.rm = TRUE, type = 7))
    
    # se os cortes colapsarem (muitos ties), reduzir k até conseguir pelo menos 2 intervals
    k_cur <- k
    while (length(cuts) <= 2 && k_cur > 2) {
      k_cur <- k_cur - 1
      probs <- seq(0, 1, length.out = k_cur + 1)
      cuts <- unique(quantile(sample_nonzero, probs = probs, na.rm = TRUE, type = 7))
    }
    # fallback se ainda falhar: criar 2 bins via range no TREINO
    if (length(cuts) <= 2) {
      rng <- range(sample_nonzero, na.rm = TRUE)
      cuts <- seq(rng[1], rng[2], length.out = 3)
    }
    
    # montar labels dos bins para não-zero
    n_bins_nonzero <- length(cuts) - 1
    labels_nonzero <- paste0("B", seq_len(n_bins_nonzero))
    
    # aplicar: criar coluna nova col_q (factor) em TODA a df_out usando cortes do treino
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
    df_out <- df_out %>%
      mutate( !!sym(new_col) := factor(.data[[new_col]],
                                       levels = levels_order,
                                       ordered = TRUE) )
    
    # salvar cortes para re-aplicação futura (em outro df se quiser)
    cuts_list[[col]] <- list(cuts = cuts,
                             has_zero = has_zero,
                             labels   = levels_order)
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

avaliar_bn <- function(dag,
                       df_train,
                       df_val,
                       target   = "default",
                       positive = "1") {
  # Ajustar parâmetros
  bn_model <- bn.fit(dag, data = df_train)
  
  # Predizer na validação com prob
  pred_probs <- predict(bn_model,
                        node = target,
                        data = df_val,
                        prob = TRUE)
  
  # Atributo "prob" contém matriz [classes x casos]
  probs_matrix <- attr(pred_probs, "prob")
  
  # Garantir que as linhas sejam as classes na mesma ordem dos levels
  if (is.null(rownames(probs_matrix))) {
    rownames(probs_matrix) <- levels(df_val[[target]])
  }
  
  # Probabilidade da classe positiva
  prob_pos <- as.numeric(probs_matrix[positive, ])
  
  # Verdadeiro
  y_true <- df_val[[target]]
  
  # AUC
  roc_obj <- pROC::roc(response = y_true,
                       predictor = prob_pos)
  auc_val <- as.numeric(pROC::auc(roc_obj))
  
  # Predição de classe com threshold 0.5
  y_pred_default <- ifelse(prob_pos >= 0.5, positive,
                           setdiff(levels(y_true), positive)[1])
  y_pred_default <- factor(y_pred_default, levels = levels(y_true))
  
  # Acurácia
  acc_default <- mean(y_pred_default == y_true)
  
  # F1 "padrão" com caret
  F1_default <- caret::F_meas(y_pred_default, y_true, relevant = positive)
  
  # Matriz de confusão
  conf_default <- table(Predito = y_pred_default, Real = y_true)
  
  # Varredura de thresholds para F1 ótimo
  ths <- seq(0, 1, by = 0.01)
  
  f1_vec <- sapply(ths, function(t) {
    y_pred <- ifelse(prob_pos >= t, positive,
                     setdiff(levels(y_true), positive)[1])
    y_pred <- factor(y_pred, levels = levels(y_true))
    
    conf <- table(Predito = y_pred, Real = y_true)
    
    # Proteger se faltar alguma combinação
    # (usa get0 com default = 0)
    get_conf <- function(i, j) {
      if (all(c(i, j) %in% rownames(conf), c(i, j) %in% colnames(conf))) {
        return(conf[i, j])
      } else {
        return(0)
      }
    }
    
    TP <- get_conf(positive, positive)
    FP <- sum(conf[positive, ]) - TP
    FN <- sum(conf[, positive]) - TP
    
    # Se não tem positivos preditos ou reais, F1 = 0
    if ((TP + FP) == 0 || (TP + FN) == 0) return(0)
    
    prec <- TP / (TP + FP)
    rec  <- TP / (TP + FN)
    
    if ((prec + rec) == 0) return(0)
    
    2 * prec * rec / (prec + rec)
  })
  
  best_idx <- which.max(f1_vec)
  best_th  <- ths[best_idx]
  best_f1  <- f1_vec[best_idx]
  
  list(
    dag      = dag,
    bn_model = bn_model,
    roc      = roc_obj,
    metrics  = list(
      auc            = auc_val,
      acc_default    = acc_default,
      F1_default     = F1_default,
      conf_default   = conf_default,
      best_threshold = best_th,
      F1_best        = best_f1
    )
  )
}


