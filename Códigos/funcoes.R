#### Funções auxiliares ####
pre_processar <- function(df) {
  vars_leakage <- c(
    # pagamentos já ocorridos
    "last_pymnt_d", "last_pymnt_amnt",
    "total_pymnt", "total_pymnt_inv", "total_rec_prncp",
    "total_rec_int", "total_rec_late_fee",
    "recoveries", "collection_recovery_fee",
    
    # saldos pós-andamento
    "out_prncp", "out_prncp_inv",
    "hardship_payoff_balance_amount", "hardship_last_payment_amount",
    "hardship_amount", "hardship_dpd", "hardship_loan_status",
    
    # delinquências e cobranças
    "num_tl_120dpd_2m", "num_tl_30dpd", "num_tl_90g_dpd_24m",
    "collections_12_mths_ex_med", "chargeoff_within_12_mths",
    "sec_app_chargeoff_within_12_mths",
    "mths_since_last_delinq", "mths_since_last_major_derog",
    "mths_since_last_record", "mths_since_recent_bc_dlq",
    "mths_since_recent_revol_delinq",
    "num_accts_ever_120_pd",
    "tot_coll_amt", "acc_now_delinq", "delinq_amnt",
    
    # hardship
    "hardship_flag", "hardship_type", "hardship_reason",
    "hardship_status", "hardship_start_date", "hardship_end_date",
    "payment_plan_start_date", "hardship_length",
    "orig_projected_additional_accrued_interest",
    
    # settlements
    "debt_settlement_flag", "debt_settlement_flag_date",
    "settlement_status", "settlement_date", "settlement_amount",
    "settlement_percentage", "settlement_term",
    
    # derivados do loan_status
    "pymnt_plan", "next_pymnt_d", "last_credit_pull_d"
  )
  
  # realizar preprocessamento no dataframe
  df_size <- nrow(df)
  
  df <- df %>% select(-any_of(vars_leakage)) %>% 
    select(-url, # inútil
           -id, # inútil
           -policy_code, # constante
           -earliest_cr_line, # inútil
           -funded_amnt_inv, # duplicada
           -title, # duplicada
           -application_type, # constante
           -disbursement_method # constante
           )
  
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

# avaliar_bn <- function(dag,
#                        df_train,
#                        df_val,
#                        target   = "default",
#                        positive = "1") {
#   # Ajustar parâmetros da BN no treino
#   bn_model <- bn.fit(dag, data = df_train)
#   
#   # Predizer na validação com probabilidade
#   pred_probs <- predict(bn_model,
#                         node = target,
#                         data = df_val,
#                         prob = TRUE)
#   
#   # Extrair matriz de probabilidades do atributo "prob"
#   probs_matrix <- attr(pred_probs, "prob")
#   if (is.null(probs_matrix)) {
#     stop("predict(..., prob = TRUE) não retornou atributo 'prob'.")
#   }
#   
#   # Verdadeiro y
#   y_true <- df_val[[target]]
#   if (!is.factor(y_true)) {
#     y_true <- factor(y_true)
#   }
#   levs <- levels(y_true)
#   
#   # Garantir nomes das linhas da matriz de probs
#   if (is.null(rownames(probs_matrix))) {
#     # assume que a ordem das linhas corresponde a levels(y_true)
#     rownames(probs_matrix) <- levs
#   }
#   
#   # Checar se a classe positiva existe
#   if (!(positive %in% rownames(probs_matrix))) {
#     stop(paste("Classe positiva", positive,
#                "não encontrada nas linhas de probs_matrix."))
#   }
#   
#   # Probabilidade da classe positiva
#   prob_pos <- as.numeric(probs_matrix[positive, ])
#   
#   # Classe negativa (assume problema binário)
#   negative <- setdiff(levs, positive)[1]
#   
#   # -------------------------
#   # Métricas com threshold padrão 0.5
#   # -------------------------
#   y_pred_default <- ifelse(prob_pos >= 0.5, positive, negative)
#   y_pred_default <- factor(y_pred_default, levels = levs)
#   
#   acc_default <- mean(y_pred_default == y_true)
#   F1_default  <- caret::F_meas(y_pred_default, y_true, relevant = positive)
#   conf_default <- table(Predito = y_pred_default, Real = y_true)
#   
#   # -------------------------
#   # AUC (usando pROC)
#   # -------------------------
#   roc_obj <- pROC::roc(response = y_true,
#                        predictor = prob_pos)
#   auc_val <- as.numeric(pROC::auc(roc_obj))
#   
#   # -------------------------
#   # Varredura de thresholds: precision, recall, F1
#   # -------------------------
#   ths <- seq(0, 1, by = 0.01)
#   
#   resultados <- sapply(ths, function(t) {
#     # Predição binária com threshold t
#     y_pred <- ifelse(prob_pos >= t, positive, negative)
#     y_pred <- factor(y_pred, levels = levs)
#     
#     conf <- table(Predito = y_pred, Real = y_true)
#     
#     # Como y_pred e y_true têm levels fixos, conf tem sempre as 2 x 2 combinações
#     TP <- conf[positive, positive]
#     FP <- sum(conf[positive, , drop = FALSE]) - TP
#     FN <- sum(conf[, positive, drop = FALSE]) - TP
#     
#     # Se não tem positivos preditos ou reais, define tudo como 0
#     if ((TP + FP) == 0 || (TP + FN) == 0) {
#       return(c(precision = 0, recall = 0, F1 = 0))
#     }
#     
#     prec <- TP / (TP + FP)
#     rec  <- TP / (TP + FN)
#     
#     if ((prec + rec) == 0) {
#       return(c(precision = 0, recall = 0, F1 = 0))
#     }
#     
#     F1 <- 2 * prec * rec / (prec + rec)
#     
#     c(precision = prec,
#       recall    = rec,
#       F1        = F1)
#   })
#   
#   # resultados é uma matriz 3 x length(ths)
#   f1_vec     <- resultados["F1", ]
#   prec_vec   <- resultados["precision", ]
#   recall_vec <- resultados["recall", ]
#   
#   # Melhor F1
#   best_idx_f1 <- which.max(f1_vec)
#   best_th_f1  <- ths[best_idx_f1]
#   best_f1     <- f1_vec[best_idx_f1]
#   
#   # Melhor precision
#   best_idx_prec <- which.max(prec_vec)
#   best_th_prec  <- ths[best_idx_prec]
#   best_prec     <- prec_vec[best_idx_prec]
#   
#   # Melhor recall
#   best_idx_rec <- which.max(recall_vec)
#   best_th_rec  <- ths[best_idx_rec]
#   best_recall  <- recall_vec[best_idx_rec]
#   
#   # -------------------------
#   # Retorno
#   # -------------------------
#   list(
#     dag      = dag,
#     bn_model = bn_model,
#     roc      = roc_obj,
#     metrics  = list(
#       auc                 = auc_val,
#       acc_default         = acc_default,
#       F1_default          = F1_default,
#       conf_default        = conf_default,
#       best_threshold_F1   = best_th_f1,
#       F1_best             = best_f1,
#       best_threshold_prec = best_th_prec,
#       prec_best           = best_prec,
#       best_threshold_rec  = best_th_rec,
#       recall_best         = best_recall
#     )
#   )
# }



