prepare_dataset <- function(register_path,  register_info_path, mno_path, zonizzazione_path, percentage_s=0.5, shuffle=FALSE, clustering=FALSE)
{
  require(readxl)
  require(dplyr)
  require(tidyr)
  
  
  register <- read_excel(register_path, sheet = 1)  
  
  
  mno <- read.csv(mno_path) 
  mno <- mno[mno$turismo_regione=="Emilia-Romagna",]
  mno <- mno[, names(mno) %in% c("anno", "mese", "turismo_provincia", "turismo_pro_com", "turismo_comune", "arrivi", "notti" )]
  mno <- aggregate(cbind(arrivi, notti) ~ ., data = mno, sum)
  
  register <- register[, !names(register) %in% c("COD_REGIONE", "REGIONE", "RESIDENZA", "COD_REGIONE_ESTERO")]
  register <- aggregate(cbind(ARR, PRE) ~ ., data = register, sum)
  register$turismo_pro_com <- as.integer(paste0(register$COD_PROVINCIA, register$COD_COMUNE))
  
  
  register_info <- suppressWarnings(read_excel(register_info_path, sheet = 1))  
  register_info <- register_info[register_info$`Codice Regione`=="08",] # 08 = Emilia-Romagna   
  
  register <- register %>%
    left_join(register_info, by = c("turismo_pro_com" = names(register_info)[3]))
  
  register_unmatched <- register %>%
    anti_join(register_info, by = c("turismo_pro_com" = names(register_info)[3]))
  
  
  
  U <- merge(U, mno[, c( "turismo_pro_com", "anno", "mese", "notti", "arrivi")], 
                         by.x = c("turismo_pro_com", "ANNO", "MESE"), 
                         by.y = c("turismo_pro_com", "anno", "mese"), 
                         all.x = TRUE)

  colnames(U)[colnames(U) == "PRE"]    <- "y"  # PRE è da register
  colnames(U)[colnames(U) == "notti"]  <- "x"  # notti è da MNO
  colnames(U)[colnames(U) == "arrivi"] <- "x_arrivi"  # arrivi è da MNO
  
  
  U <- U[!is.na(U$x), ]
  U <- U[!is.na(U$x_arrivi), ]
  
  
  if(clustering == TRUE)
  {  
    cluster_result <- find_similar_turismo_pro_com_hclust(U, distance_threshold_percentage = 0.1, return_only_codes = TRUE, mese = month_of_interest)
    U <- U %>%
         filter(turismo_pro_com %in% cluster_result[[1]]) %>%
         select(turismo_pro_com, DENOM_COMUNE) %>%
         distinct()
  }
  
  
  
  
  colonne_register_info <- colnames(register_info)
  colonne_register_info <- setdiff(colonne_register_info, c("Codice Istat del Comune \r\n(alfanumerico)", "Codice Istat del Comune \r\n(numerico)", "Codice Regione", "Denominazione (Italiana e straniera)", "Denominazione altra lingua" )) # elimina colonne che danno problemi
  
  colonne_da_tenere <- c("turismo_pro_com", "DENOM_COMUNE", "ANNO", "MESE", "y", "x", "x_arrivi", "COD_PROVINCIA" , colonne_register_info)
  U <- U[, colonne_da_tenere, drop = FALSE]
  
  U$COD_PROVINCIA <- as.factor(U$COD_PROVINCIA)
  
  colnames(U)[colnames(U) == "Grado di urbanizzazione"] <- "Grado.di.urbanizzazione"
  U$Grado.di.urbanizzazione <- as.factor(U$Grado.di.urbanizzazione)
  
  U <- U %>%  arrange(DENOM_COMUNE, ANNO, MESE) # Ordino in modo comodo
  
  
  register_yearly_aggregate <- aggregate( # serve per estrarre i comuni con più abitanti, per simulare i take-all
    cbind(ARR, PRE) ~ DENOM_COMUNE, 
    data = register, 
    sum
  )
  register_yearly_aggregate <- register_yearly_aggregate[order(register_yearly_aggregate$PRE, decreasing = TRUE), ]
  n_comuni     <- length(register_yearly_aggregate$DENOM_COMUNE)
  top_n_comuni <- ceiling(n_comuni * percentage_s) 
  
  if (shuffle) {
    comuni_s <- sample(register_yearly_aggregate$DENOM_COMUNE, top_n_comuni)  
  } else {
    comuni_s <- register_yearly_aggregate[1:top_n_comuni,]$DENOM_COMUNE
  }
  
  U$YYYYMM <- U$ANNO * 100 + U$MESE
  U <- U[, !names(U) %in% "ARR"] 
  
  
  U <- U %>%
    arrange(turismo_pro_com, YYYYMM) %>%
    group_by(turismo_pro_com) %>%
    mutate(y_lag1   = lag(y, n = 1)) %>%
    mutate(y_lag2   = lag(y, n = 2)) %>%
    mutate(y_lag3   = lag(y, n = 3)) %>%
    mutate(y_lag4   = lag(y, n = 4)) %>%
    mutate(y_lag5   = lag(y, n = 5)) %>%
    mutate(y_lag6   = lag(y, n = 6)) %>%
    mutate(y_lag7   = lag(y, n = 7)) %>%
    mutate(y_lag8   = lag(y, n = 8)) %>%
    mutate(y_lag9   = lag(y, n = 9)) %>%
    mutate(y_lag10  = lag(y, n = 10)) %>%
    mutate(y_lag11  = lag(y, n = 11)) %>%
    mutate(y_lag12  = lag(y, n = 12)) %>%
    mutate(y_lag13  = lag(y, n = 13)) %>%
    mutate(x_lag1   = lag(x, n = 1)) %>%
    mutate(x_lag2   = lag(x, n = 2)) %>%
    mutate(x_lag3   = lag(x, n = 3)) %>%
    mutate(x_lag4   = lag(x, n = 4)) %>%
    mutate(x_lag5   = lag(x, n = 5)) %>%
    mutate(x_lag6   = lag(x, n = 6)) %>%
    mutate(x_lag7   = lag(x, n = 7)) %>%
    mutate(x_lag8   = lag(x, n = 8)) %>%
    mutate(x_lag9   = lag(x, n = 9)) %>%
    mutate(x_lag10  = lag(x, n = 10)) %>%
    mutate(x_lag11  = lag(x, n = 11)) %>%
    mutate(x_lag12  = lag(x, n = 12)) %>%
    mutate(x_lag13  = lag(x, n = 13)) %>%
    mutate(x_arrivi_lag1  = lag(x_arrivi, n = 1)) %>%
    mutate(x_arrivi_lag2  = lag(x_arrivi, n = 2)) %>%
    mutate(x_arrivi_lag12 = lag(x_arrivi, n = 12)) %>%
    ungroup()

  U$Bologna_municipality <- as.integer(U$DENOM_COMUNE == "Bologna") # Dummy == 1 se comune = Bologna
  
  
  U$STAGIONE <- factor( 
    cut(U$MESE, 
        breaks = c(0, 3, 6, 9, 12),   
        labels = c("inverno", "primavera", "estate", "autunno")
    )
  )
  
  U <- U %>%
    arrange(turismo_pro_com, YYYYMM) %>%
    mutate(STAGIONE_lag1   = lag(STAGIONE, n = 1)) %>%
    ungroup()
  

  zonizzazione <- read.csv2(zonizzazione_path, stringsAsFactors = FALSE)
  U <- U %>% left_join(zonizzazione %>% select(CODICE.ISTAT, NOME.ZONA), by = c("turismo_pro_com" = "CODICE.ISTAT"))
  U$NOME.ZONA[U$`Comune litoraneo` == 1] <- "Litorale"   # Modifica NOME_ZONA se "Comune litoraneo" è 1
  U$NOME.ZONA <- factor(U$NOME.ZONA)
  
  
  
  
  U <- U[complete.cases(U[, c("y", "x", "x_arrivi")]), ]
  
  
  s <- U[  U$DENOM_COMUNE %in% comuni_s, ]
  R <- U[! U$DENOM_COMUNE %in% comuni_s, ]
  
  
  return(list("U" = U, "R" = R, "s" = s))
}  






fill_na_y_with_previous_d_month_if_present <- function(r, d, fill_also_x = TRUE) {
  na_rows_y <- is.na(r$y)  
  
  
  r$YYYYMM_prev <- with(r, ifelse(
    MESE > d, 
    YYYYMM - d, 
    (ANNO - 1) * 100 + (MESE - d + 12) 
  ))
  
  cols_to_merge <- c("DENOM_COMUNE", "YYYYMM", "y")
  if (fill_also_x) {
    cols_to_merge <- c(cols_to_merge, "x", "x_lag1", "x_lag2", "x_lag3", "x_lag4", "x_lag5", "x_lag6", "x_lag7", "x_lag8", "x_lag9", "x_lag10", "x_lag11", "x_lag12", "x_lag13", "y_lag1", "y_lag2", "y_lag3", "y_lag4", "y_lag5", "y_lag6", "y_lag7", "y_lag8", "y_lag9", "y_lag10", "y_lag11", "y_lag12", "y_lag13")  # Aggiungere x se richiesto
    if ("x_arrivi" %in% colnames(r)) {
      cols_to_merge <- c(cols_to_merge, "x_arrivi") 
    }
  }

  r <- merge(
    r, 
    r[, cols_to_merge], 
    by.x = c("DENOM_COMUNE", "YYYYMM_prev"), 
    by.y = c("DENOM_COMUNE", "YYYYMM"), 
    all.x = TRUE, 
    suffixes = c("", "_prev")
  )
  
  na_rows_y <- is.na(r$y) 
  r$y <- ifelse(is.na(r$y), r$y_prev, r$y)
  
  
  if (fill_also_x) {
    for (col in cols_to_merge) {
      if (col != "DENOM_COMUNE" && col != "YYYYMM") {  
        r[[col]][na_rows_y] <- r[[paste0(col, "_prev")]][na_rows_y]
      }
    }
  }
  
  r$YYYYMM_prev <- NULL  
  r$y_prev <- NULL       
  if (fill_also_x) {
    for (col in cols_to_merge) {
      if (col != "DENOM_COMUNE" && col != "YYYYMM") {
        r[[paste0(col, "_prev")]] <- NULL
      }
    }
  }
  
  return(r)
}





fill_na_y_with_previous_year_and_d_month <- function(r, d, fill_also_x = TRUE) {
  r_12 <- fill_na_y_with_previous_d_month_if_present(r, d = 12, fill_also_x) %>%
    mutate(filled_source = "year_before")
  
  r_d <- fill_na_y_with_previous_d_month_if_present(r, d = d, fill_also_x) %>%
    mutate(filled_source = "d_months_before")
  

  r_filled <- bind_rows(r_12, r_d) %>%
    select(-filled_source)  
  
  return(r_filled)
}



fill_na_y_with_previous_d_month_if_present_adjusted <- function(r, d, fill_also_x = TRUE) {
  na_rows_y <- is.na(r$y)  
  
  
  r$YYYYMM_prev <- with(r, ifelse(
    MESE > d, 
    YYYYMM - d, 
    (ANNO - 1) * 100 + (MESE - d + 12) 
  ))
  
  cols_to_merge <- c("DENOM_COMUNE", "YYYYMM", "y")
  if (fill_also_x) {
    cols_to_merge <- c(cols_to_merge, "x", "x_lag1", "x_lag2", "x_lag3", "x_lag4", "x_lag5", "x_lag6", "x_lag7", "x_lag8", "x_lag9", "x_lag10", "x_lag11", "x_lag12", "x_lag13", "y_lag1", "y_lag2", "y_lag3", "y_lag4", "y_lag5", "y_lag6", "y_lag7", "y_lag8", "y_lag9", "y_lag10", "y_lag11", "y_lag12", "y_lag13")  # Aggiungere x se richiesto
    if ("x_arrivi" %in% colnames(r)) {
      cols_to_merge <- c(cols_to_merge, "x_arrivi")  # Aggiungere x_arrivi se presente
    }
  }
  

  
  r <- merge(
    r, 
    r[, cols_to_merge], 
    by.x = c("DENOM_COMUNE", "YYYYMM_prev"), 
    by.y = c("DENOM_COMUNE", "YYYYMM"), 
    all.x = TRUE, 
    suffixes = c("", "_prev")
  )
  
  na_rows_y <- is.na(r$y) 
  r$y <- ifelse(is.na(r$y), r$y_prev*0.03, r$y)
  
  
  if (fill_also_x) {
    for (col in cols_to_merge) {
      if (col != "DENOM_COMUNE" && col != "YYYYMM") { 
        r[[col]][na_rows_y] <- r[[paste0(col, "_prev")]][na_rows_y]
      }
    }
  }
  
  r$YYYYMM_prev <- NULL  
  r$y_prev <- NULL       
  if (fill_also_x) {
    for (col in cols_to_merge) {
      if (col != "DENOM_COMUNE" && col != "YYYYMM") {
        r[[paste0(col, "_prev")]] <- NULL
      }
    }
  }
  
  return(r)
}



























prepare_U <- function(register_path,  register_info_path, mno_path, zonizzazione_path)
{
  require(readxl)
  require(dplyr)
  require(tidyr)
  
  
  register <- read_excel(register_path, sheet = 1)  
  
  
  mno <- read.csv(mno_path) 
  mno <- mno[mno$turismo_regione=="Emilia-Romagna",]
  mno <- mno[, names(mno) %in% c("anno", "mese", "turismo_provincia", "turismo_pro_com", "turismo_comune", "arrivi", "notti" )]
  mno <- aggregate(cbind(arrivi, notti) ~ ., data = mno, sum)
  
  register <- register[, !names(register) %in% c("COD_REGIONE", "REGIONE", "RESIDENZA", "COD_REGIONE_ESTERO")]
  register <- aggregate(cbind(ARR, PRE) ~ ., data = register, sum)
  register$turismo_pro_com <- as.integer(paste0(register$COD_PROVINCIA, register$COD_COMUNE))
  
  
  register_info <- suppressWarnings(read_excel(register_info_path, sheet = 1))  
  register_info <- register_info[register_info$`Codice Regione`=="08",] # 08 = Emilia-Romagna   
  
  register <- register %>%
    left_join(register_info, by = c("turismo_pro_com" = names(register_info)[3]))
  
  register_unmatched <- register %>%
    anti_join(register_info, by = c("turismo_pro_com" = names(register_info)[3]))
  
  
  U <- register # U è la popolazione
  U <- merge(U, mno[, c( "turismo_pro_com", "anno", "mese", "notti", "arrivi")], 
             by.x = c("turismo_pro_com", "ANNO", "MESE"), 
             by.y = c("turismo_pro_com", "anno", "mese"), 
             all.x = TRUE)
  
  colnames(U)[colnames(U) == "PRE"]    <- "y"  # PRE is from register
  colnames(U)[colnames(U) == "notti"]  <- "x"  # notti= nights is from MNO
  colnames(U)[colnames(U) == "arrivi"] <- "x_arrivi"  # arrivi=arrivals from MNO
  
  
  
  U <- U[!is.na(U$x), ]
  U <- U[!is.na(U$x_arrivi), ]
  

  colonne_register_info <- colnames(register_info)
  colonne_register_info <- setdiff(colonne_register_info, c("Codice Istat del Comune \r\n(alfanumerico)", "Codice Istat del Comune \r\n(numerico)", "Codice Regione", "Denominazione (Italiana e straniera)", "Denominazione altra lingua" )) # elimina colonne che danno problemi
  
  colonne_da_tenere <- c("turismo_pro_com", "DENOM_COMUNE", "ANNO", "MESE", "y", "x", "x_arrivi", "COD_PROVINCIA" , colonne_register_info)
  U <- U[, colonne_da_tenere, drop = FALSE]
  
  U$COD_PROVINCIA <- as.factor(U$COD_PROVINCIA)

  U <- U %>%  arrange(DENOM_COMUNE, ANNO, MESE) # Ordino in modo comodo
  
  
  register_yearly_aggregate <- aggregate( # serve per estrarre i comuni con più abitanti, per simulare i take-all
    cbind(ARR, PRE) ~ DENOM_COMUNE, 
    data = register, 
    sum
  )

  
  U$YYYYMM <- U$ANNO * 100 + U$MESE 
  U <- U[, !names(U) %in% "ARR"] 
  

  U <- U %>%
    arrange(turismo_pro_com, YYYYMM) %>%
    group_by(turismo_pro_com) %>%
    mutate(y_lag1   = lag(y, n = 1)) %>%
    mutate(y_lag2   = lag(y, n = 2)) %>%
    mutate(y_lag3   = lag(y, n = 3)) %>%
    mutate(y_lag4   = lag(y, n = 4)) %>%
    mutate(y_lag5   = lag(y, n = 5)) %>%
    mutate(y_lag6   = lag(y, n = 6)) %>%
    mutate(y_lag7   = lag(y, n = 7)) %>%
    mutate(y_lag8   = lag(y, n = 8)) %>%
    mutate(y_lag9   = lag(y, n = 9)) %>%
    mutate(y_lag10  = lag(y, n = 10)) %>%
    mutate(y_lag11  = lag(y, n = 11)) %>%
    mutate(y_lag12  = lag(y, n = 12)) %>%
    mutate(y_lag13  = lag(y, n = 13)) %>%
    mutate(x_lag1   = lag(x, n = 1)) %>%
    mutate(x_lag2   = lag(x, n = 2)) %>%
    mutate(x_lag3   = lag(y, n = 3)) %>%
    mutate(x_lag4   = lag(x, n = 4)) %>%
    mutate(x_lag5   = lag(x, n = 5)) %>%
    mutate(x_lag6   = lag(x, n = 6)) %>%
    mutate(x_lag7   = lag(x, n = 7)) %>%
    mutate(x_lag8   = lag(x, n = 8)) %>%
    mutate(x_lag9   = lag(x, n = 9)) %>%
    mutate(x_lag10  = lag(x, n = 10)) %>%
    mutate(x_lag11  = lag(x, n = 11)) %>%
    mutate(x_lag12  = lag(x, n = 12)) %>%
    mutate(x_lag12  = lag(x, n = 12)) %>%
    mutate(x_lag13  = lag(x, n = 13)) %>%
    mutate(x_arrivi_lag1  = lag(x_arrivi, n = 1)) %>%
    mutate(x_arrivi_lag2  = lag(x_arrivi, n = 2)) %>%
    mutate(x_arrivi_lag12 = lag(x_arrivi, n = 12)) %>%
    ungroup()
  
  
  U <- U[complete.cases(U[, c("y", "x", "x_arrivi")]), ]
  
  

  U$Bologna_municipality <- as.integer(U$DENOM_COMUNE == "Bologna") # Dummy == 1 se comune = Bologna
  
  
  U$STAGIONE <- factor( 
    cut(U$MESE, 
        breaks = c(0, 3, 6, 9, 12),  
        labels = c("inverno", "primavera", "estate", "autunno")
    )
  )
  
  U <- U %>%
    arrange(turismo_pro_com, YYYYMM) %>%
    group_by(turismo_pro_com) %>%
    mutate(STAGIONE_lag1   = lag(STAGIONE, n = 1)) %>%
    ungroup()
  

  zonizzazione <- read.csv2(zonizzazione_path, stringsAsFactors = FALSE)
  U <- U %>% left_join(zonizzazione %>% select(CODICE.ISTAT, NOME.ZONA), by = c("turismo_pro_com" = "CODICE.ISTAT"))
  U$NOME.ZONA[U$`Comune litoraneo` == 1] <- "Litorale"   # Modifica NOME_ZONA se "Comune litoraneo" è 1
  U$NOME.ZONA <- factor(U$NOME.ZONA)
  
  colnames(U)[colnames(U) == "Grado di urbanizzazione"] <- "Grado.di.urbanizzazione"
  U$Grado.di.urbanizzazione <- as.factor(U$Grado.di.urbanizzazione)
  
  return(U)
}  


prepare_folds <- function(U, folds) {
  fold_list <- vector("list", length(folds)) # Inizializza la lista vuota
  
  for (i in seq_along(folds)) {
    test_codici <- folds[[i]]  
    R_fold <- U[U$turismo_pro_com %in% test_codici, ]  
    s_fold <- U[!U$turismo_pro_com %in% test_codici, ] 
    
    fold_list[[i]] <- list(s_fold = s_fold, R_fold = R_fold)
  }
  
  return(fold_list)
}


  
add_missing_levels <- function(data, factor_col, debug=FALSE) {
  data[[factor_col]] <- factor(data[[factor_col]], levels = levels(data[[factor_col]]))
  
  present_levels <- unique(data[[factor_col]])
  

  
  missing_levels <- setdiff(levels(data[[factor_col]]), present_levels)
  
  if (length(missing_levels) > 0) {
    for (lvl in missing_levels) {
      riga_sample <- data[sample(nrow(data), 1), , drop = FALSE]
      riga_sample[[factor_col]] <- lvl  
      data <- rbind(data, riga_sample)  
    }
  }
  
  return(data)
}


extract_independent_vars_from_formula <- function(formula) {

  
  terms_info <- terms(formula)
  vars <- attr(terms_info, "term.labels")  # Ottiene solo i termini della parte destra
  
  vars_with_dollar <- grep("\\$", vars, value = TRUE)
  
  external_vars <- unique(sub("^.*[ (]([a-zA-Z0-9_.]+)\\$.*$", "\\1", vars_with_dollar))
  
  all_vars <- all.vars(formula)
  
  clean_vars <- setdiff(all_vars, external_vars)
  clean_vars <- clean_vars[-1]
  
  return(clean_vars)
}




split_data <- function(U, percentuale_test = 0.5) {

  turismo_unici <- unique(U$turismo_pro_com)
  
  num_R <- round(length(turismo_unici) * percentuale_test)
  
  turismo_R <- sample(turismo_unici, num_R)
  
  R <- U[U$turismo_pro_com %in% turismo_R, ]
  s <- U[!U$turismo_pro_com %in% turismo_R, ]
  
  return(list(s = s, R = R))
}

prepare_train_test <- function(U, test_ratio) {
  require(dplyr)

  comuni_unici <- U %>%
    select(turismo_pro_com) %>%
    distinct()

  comuni_unici <- comuni_unici %>%
    mutate(test_set = turismo_pro_com %in% sample(turismo_pro_com, size = round(n() * test_ratio)))

  U <- U %>%
    left_join(comuni_unici, by = "turismo_pro_com")

  R <- U %>% filter(test_set)  # Test set (test_ratio of the minicipalities)
  s <- U %>% filter(!test_set) # Training set (remaining municipalities)

  R <- select(R, -test_set)
  s <- select(s, -test_set)

  return(list(train = s, test = R))
}

# With stratification (not used yet)
prepare_train_test_stratified <- function(U, test_ratio) {
  require(dplyr)
  
  comuni_unici <- U %>%
    select(turismo_pro_com, NOME.ZONA) %>%
    distinct()
  
  comuni_unici <- comuni_unici %>%
    group_by(NOME.ZONA) %>%
    mutate(test_set = turismo_pro_com %in% sample(turismo_pro_com, size = round(n() * test_ratio))) %>%
    ungroup() %>%
    select(-NOME.ZONA)  
  
  
  U <- U %>%
    left_join(comuni_unici, by = "turismo_pro_com")
  
  R <- U %>% filter(test_set) 
  s <- U %>% filter(!test_set) 
  
  R <- select(R, -test_set)
  s <- select(s, -test_set)
  
  return(list(train = s, test = R))
}



