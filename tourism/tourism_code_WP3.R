setwd(dirname(rstudioapi::getSourceEditorContext()$path))

set.seed(1001) # To reproduce results

options(scipen = 999)



library(sf)
library(ggplot2)
library(dplyr)
library(purrr)
library(performance)
library(randomForest)

source("prepare_dataset.R")

############################## DATASET PREPARATION #############################

register_path      <- "yourpath"
mno_path           <- "yourpath"
register_info_path <- "yourpath"
comuni_italiani    <- st_read("yourpath\\Com01012023_g_WGS84.shp")

comuni_er <- comuni_italiani %>% filter(COD_REG == 8)  # Emilia-Romagna ha codice ISTAT 08
rm(comuni_italiani)


U <- prepare_U(register_path, register_info_path, mno_path)  # ordina i comuni per numero di pernottamenti, poi prende il primo <percentage_s>% di essi e li mette in s


# save(U, comuni_er,  file = "UandComuniER.RData")

################################################################################


################################################################################
#         FOR MODEL COEFFICIENTS PRINT AND TEST ON A SPECIFIC SAMPLE           # 
################################################################################

filter_median <- TRUE

if(filter_median)
{
  U <- U %>%
    filter(y > median(U$y, na.rm = TRUE))
}

split_data <- prepare_train_test(U, test_ratio = 0.5)
s <- split_data$train
R <- split_data$test
################################################################################



names(s) <-  make.names(names(s), unique = TRUE)
names(R) <-  make.names(names(R), unique = TRUE)
names(U) <-  make.names(names(U), unique = TRUE)



b <- 202308 
month_of_interest <-  9 # 9 = september 2023 set to NA, 10 = october 2023 set to NA
R_no_month_of_interest <- R
R_no_month_of_interest[which(R_no_month_of_interest$MESE == month_of_interest  & R_no_month_of_interest$ANNO == 2023), "y"] <- NA
R_no_month_of_interest <- R_no_month_of_interest[!(R_no_month_of_interest$ANNO == 2023 & R_no_month_of_interest$MESE > month_of_interest), ] # Rimuovo le osservazioni successive a settembre (fingo l'ultimo mese sia settembre)
R <- R_no_month_of_interest
re_fit_model_for_maps <- TRUE



#################################### 
#     Specify model formula        #
####################################

formula_mu           <-  y ~  y_lag12 + y_lag11 + y_lag1 + x  + x_lag12 + x_lag11 + x_lag1 + + Popolazione.residente.al.31.12.2023 + Comune.litoraneo + Altitudine.del.centro..metri. + Grado.di.urbanizzazione + Superficie.territoriale..kmq..al.01.01.2024 + COD_PROVINCIA #-1
formula_g_simplistic <-  formula_mu
formula_g_augmented  <-  formula_mu
formula_g_TL         <-  update(formula_g_augmented, . ~ . + mu_res)

######################################
# NA substitution                    #
######################################                  

r_star_last_year    <- fill_na_y_with_previous_d_month_if_present(R, d=12, fill_also_x = FALSE) # fill_also_x se TRUE prende la x del passato, altrimenti lascia quella presente
r_star_last_4_month <- fill_na_y_with_previous_d_month_if_present(R, d=4 , fill_also_x = FALSE)
r_star_mix          <- fill_na_y_with_previous_year_and_d_month  (R, d=1 , fill_also_x = FALSE)
r_star <- r_star_last_year 

q <- rbind(s, R)
s_star <- rbind(r_star, s)
s_star <- s_star[complete.cases(s_star[, c("x", "x_arrivi")]),]




#*******************************************************************************
#*                               TRANSFER SCHEME B                             *
#*******************************************************************************


s_star_b <- s_star[s_star$YYYYMM %in% b,]  
q_b      <- q     [q$YYYYMM      %in% b,]  



# Removes rows with NA in variables used by the formula
vars <- extract_independent_vars_from_formula(formula_mu)
s_star_b <- s_star_b[complete.cases(s_star_b[, vars]), ]


mu_s_star_b <- randomForest(
  formula_mu, 
  data = s_star_b, 
  ntree = 500,     # Number of trees
)    



# Removes rows with NA in variables used by the formula
vars <- extract_independent_vars_from_formula(formula_mu)
s_star <- s_star[complete.cases(s_star[, vars]), ]


# Forecasting on transformed scale
s_star$mu_res <- predict(mu_s_star_b, newdata = s_star)


# Removes rows with NA in variables used by the formula
vars <- extract_independent_vars_from_formula(formula_mu)
q_b <- q_b[complete.cases(q_b[, vars]), ]



mu_q_b <- randomForest(
  formula_mu, 
  data = q_b, 
  ntree = 500,     
)  


# Removes rows with NA in variables used by the formula
vars <- extract_independent_vars_from_formula(formula_mu)
q <- q[complete.cases(q[, vars]), ]

q$mu_res <- predict(mu_q_b, newdata = q)

s_star$MESE <- as.factor(s_star$MESE)
q$MESE      <- as.factor(q$MESE)


# Removes rows with NA in variables used by the formula
vars <- extract_independent_vars_from_formula(formula_g_TL)
s_star <- s_star[complete.cases(s_star[, c(vars,"y")]), ]


g_hat <- randomForest(
  formula_g_TL, 
  data = s_star,
  ntree = 500,     
)  

g_hat_augmented <- randomForest(
  formula_g_augmented, 
  data = s_star,
  ntree = 500,     
)  



importance(g_hat) # Increase of node purity including the variable
varImpPlot(g_hat)


q$y_pred <- predict(g_hat, newdata = q)
q$y_pred <- round(q$y_pred)

q$y_pred_augmented <- predict(g_hat_augmented, newdata = q)
q$y_pred_augmented <- round(q$y_pred_augmented)

q <- q[, c(setdiff(names(U), c("y", "y_pred", "y_pred_augmented")), "y", "y_pred", "y_pred_augmented")] # I put y e y_pred vicine, at the end of the dataframe


################################ COMPARISON ####################################

vars <- extract_independent_vars_from_formula(formula_g_simplistic)
s <- s[complete.cases(s[, vars]), ]


g_hat_simplistic <- randomForest(
  formula_g_simplistic, 
  data = s,  
  ntree = 500,    
)  


vars <- extract_independent_vars_from_formula(g_hat_simplistic)
q <- q[complete.cases(q[, vars]), ]

q$y_pred_simplistic <- predict(g_hat_simplistic, newdata = q)
q$y_pred_simplistic <- round(q$y_pred_simplistic)

test_set     <- R[is.na(R$y), ]   
q <- q[q$turismo_pro_com %in% test_set$turismo_pro_com & q$YYYYMM %in% test_set$YYYYMM, ]
q <- merge(q, U[, c("YYYYMM", "turismo_pro_com", "y")], 
           by = c("YYYYMM", "turismo_pro_com"), 
           all.x = TRUE)
q$y <- q$y.y 

q$y.y <- NULL
q$y.x <- NULL


q <- na.omit(q)



# 3. Compute Absolute Errors
q$AD_simplistic <- abs(q$y_pred_simplistic - q$y)
q$AD_TL         <- abs(q$y_pred - q$y)
q$AD_augmented  <- abs(q$y_pred_augmented - q$y)



MAD_simplistic <- mean(q$AD_simplistic)
MAD_TL         <- mean(q$AD_TL)
MAD_augmented  <- mean(q$AD_augmented)

TAD_simplistic <- sum(q$AD_simplistic)
TAD_TL         <- sum(q$AD_TL)
TAD_augmented  <- sum(q$AD_augmented)

epsilon <- 1e-6  # Small value added to avoid distorsion in divisions by zero
AD_diff            <-   TAD_simplistic /  (TAD_TL + epsilon) 
AD_diff_augmented  <-   TAD_simplistic /  (TAD_augmented + epsilon) 


total_y                 <- sum(q$y)
total_y_pred_TL         <- sum(q$y_pred)
total_y_pred_simplistic <- sum(q$y_pred_simplistic)
total_y_pred_augmented  <- sum(q$y_pred_augmented)


total_y
total_y_pred_TL
total_y_pred_simplistic
total_y_pred_augmented




################################################################################
#                     TEST ON THE WHOLE REGION K-FOLD                          #
################################################################################


# Load dataset
load("UandComuniER.RData")

if(filter_median)
{
  U <- U %>%
    filter(y > median(U$y, na.rm = TRUE))
}


U <- U %>%
  mutate(`Comune litoraneo` = as.integer(`Comune litoraneo`)) %>%  mutate(`Zona altimetrica` = factor(`Zona altimetrica`, levels = c("1","3", "4", "5")))

names(U) <-  make.names(names(U), unique = TRUE)


# U <- U[U$x > 700, ]


codici_univoci <- unique(U$turismo_pro_com)

k_outer <- 10 
results <- list("MAD_simplistic"=c(), "TAD_simplistic"=c(), "Perc_err_simplistic"=c() , "Perc_err_on_total_simplistic"=c() ,"MAD_TL"=c(), "TAD_TL"=c(), "Perc_err_TL"=c(), "Perc_err_on_total_TL"=c(), "q_finals" = list())


for(kk in 1:k_outer)
{  
  # browser()
  
  print(paste("Repetition number",kk))
  
  U <- U %>% mutate(row_id = row_number())
  k_inner <- 2 #4 
  
  # Obtains unique codes from municipalities and for their zone
  codici_univoci <- U %>%
    select(turismo_pro_com, NOME.ZONA) %>%
    distinct()
  
  codici_univoci <- codici_univoci %>%
    mutate(fold_id = sample(rep(1:k_inner, length.out = n()))) %>%
    ungroup()
  
  # Assign every row of U to a fold, basing on its municipality
  U <- U %>%
    left_join(codici_univoci %>% select(turismo_pro_com, NOME.ZONA), by = "turismo_pro_com") %>%
    select(-NOME.ZONA.x) %>%  
    rename(NOME.ZONA = NOME.ZONA.y) 
  
  # Creates fold list
  folds <- split(codici_univoci$turismo_pro_com, codici_univoci$fold_id)
  
  # browser()
  
  folds_data <- prepare_folds(U, folds)
  
  
  MADs_simplistic <- c()
  MADs_TL         <- c()
  MADs_augmented  <- c()
  
  TADs_simplistic <- c()
  TADs_TL         <- c()
  TADs_augmented  <- c()
  
  q_final         <- data.frame() 
  perc_err_unit_TLs <- c()
  perc_err_unit_simplistics <- c()
  perc_err_unit_augmenteds  <- c()
  
  
  for(i in seq_along(folds))
  {
    
    
    s <- folds_data[[i]]$s
    R <- folds_data[[i]]$R
    
  
    R_no_month_of_interest <- R
    R_no_month_of_interest[which(R_no_month_of_interest$MESE == month_of_interest  & R_no_month_of_interest$ANNO == 2023), "y"] <- NA
    R_no_month_of_interest <- R_no_month_of_interest[!(R_no_month_of_interest$ANNO == 2023 & R_no_month_of_interest$MESE > month_of_interest), ] # Rimuovo le osservazioni successive a settembre (fingo l'ultimo mese sia settembre)
    R <- R_no_month_of_interest
    
    
    # ################################################################################## 
    # # Specify model formula. Commented because I used the formula defined previously #
    # ##################################################################################
    # 
    # formula_mu           <- y ~ x + Popolazione.residente.al.31.12.2023 + Comune.litoraneo + Altitudine.del.centro..metri. + Grado.di.urbanizzazione + Superficie.territoriale..kmq..al.01.01.2024 + COD_PROVINCIA + y_lag1 + x_lag1 #+ y_lag2
    # formula_g_simplistic <- update(formula_mu, . ~ . + MESE)
    # formula_g_TL         <- update(formula_g_simplistic, . ~ . +  mu_res)
    # 
    
    r_star_last_year    <- fill_na_y_with_previous_d_month_if_present(R, d=12, fill_also_x = FALSE) # fill_also_x se TRUE prende la x del passato, altrimenti lascia quella presente
    r_star_last_4_month <- fill_na_y_with_previous_d_month_if_present(R, d=4 , fill_also_x = FALSE)
    r_star_mix          <- fill_na_y_with_previous_year_and_d_month  (R, d=1 , fill_also_x = FALSE)
    r_star <- r_star_last_year 
    
    q <- rbind(s, R)
    s_star <- rbind(r_star, s)
    s_star <- s_star[complete.cases(s_star[, c("x", "x_arrivi")]),]
    
    
    #*******************************************************************************
    #*                               TRANSFER SCHEME B                             *
    #*******************************************************************************
    
    
    s_star_b <- s_star[s_star$YYYYMM %in% b,]  
    q_b      <- q     [q$YYYYMM      %in% b,]  #s_star[s_star$YYYYMM == b & complete.cases(q[, c("y", "x")]),]      # removes observations with NA in x or y
    
    
    
    # Removes rows with NA in variables used by the formula
    vars <- extract_independent_vars_from_formula(formula_mu)
    s_star_b <- s_star_b[complete.cases(s_star_b[, c(vars,"y")]), ]
    q_b      <- q_b[complete.cases(q_b[, c(vars,"y")]), ]
    
    
    
    if(re_fit_model_for_maps)
    {  
      mu_s_star_b <- randomForest(
        formula_mu,
        data = s_star_b,
        ntree = 500,     
      )
    }
    
    # Removes rows with NA in variables used by the formula
    vars <- extract_independent_vars_from_formula(formula_mu)
    s_star <- s_star[complete.cases(s_star[, c(vars, "y")]), ]
    
    
    s_star$mu_res <- predict(mu_s_star_b, newdata = s_star)
    

    if(re_fit_model_for_maps)
    {
      mu_q_b <- randomForest(
        formula_mu,
        data = q_b,
        ntree = 500,     
      )
    }
    

    q$mu_res <- predict(mu_q_b, newdata = q)
    
    s_star$MESE <- as.factor(s_star$MESE)
    q$MESE      <- as.factor(q$MESE)
    
    
    # Removes rows with NA in variables used by the formula
    vars <- extract_independent_vars_from_formula(formula_g_TL)
    s_star <- s_star[complete.cases(s_star[, c(vars,"y")]), ]
    
    
    if(re_fit_model_for_maps)
    {
      g_hat <- randomForest(
        formula_g_TL,
        data = s_star,
        ntree = 500,     
      )
    }
    
    if(re_fit_model_for_maps)
    {
      g_hat_augmented <- randomForest(
        formula_g_augmented,
        data = s_star,
        ntree = 500,     
      )
    }
    
    q$y_pred <- predict(g_hat, newdata = q)
    q$y_pred <- round(q$y_pred)
    
    q$y_pred_augmented <- predict(g_hat_augmented, newdata = q)
    q$y_pred_augmented <- round(q$y_pred_augmented)
    
    q <- q[, c(setdiff(names(U), c("y", "y_pred", "y_pred_augmented")), "y", "y_pred", "y_pred_augmented")] # metto y e y_pred vicine, a fine dataframe
    
    summary(g_hat)
    
    ################################ COMPARISON ####################################
    vars <- extract_independent_vars_from_formula(formula_g_simplistic)
    s <- s[complete.cases(s[, vars]), ]
    
    if(re_fit_model_for_maps)
    {
      g_hat_simplistic <- randomForest(
        formula_g_simplistic,
        data = s,#s_star,
        ntree = 500,    
      )
    }
    
    vars <- extract_independent_vars_from_formula(g_hat_simplistic)
    q <- q[complete.cases(q[, vars]), ]
    
    q$y_pred_simplistic <- predict(g_hat_simplistic, newdata = q)
    q$y_pred_simplistic <- round(q$y_pred_simplistic)
    
    test_set     <- R[is.na(R$y), ]  
    q <- q[q$turismo_pro_com %in% test_set$turismo_pro_com & q$YYYYMM %in% test_set$YYYYMM, ]
    q <- merge(q, U[, c("YYYYMM", "turismo_pro_com", "y")], 
               by = c("YYYYMM", "turismo_pro_com"), 
               all.x = TRUE)
    q$y <- q$y.y 
    
    q$y.y <- NULL
    q$y.x <- NULL
    
    
    # browser()
    q <- na.omit(q)
    # browser()
    
    
    # 3. Computes Absolute Errors
    q$AD_simplistic <- abs(q$y_pred_simplistic - q$y)
    q$AD_TL         <- abs(q$y_pred - q$y)
    q$AD_augmented <- abs(q$y_pred_augmented - q$y)
    
    

    q$perc_err_unit_TL <- ifelse(
      q$y == 0, 
      abs(q$y - q$y_pred)/1,  # Case y=0, relative Abs. Error with respect to 1
      abs(q$y - q$y_pred) / q$y
    )
    
    q$perc_err_unit_simplistic <- ifelse(
      q$y == 0, 
      abs(q$y - q$y_pred_simplistic)/1, 
      abs(q$y - q$y_pred_simplistic) / q$y
    )
    
    q$perc_err_unit_augmented <- ifelse(
      q$y == 0, 
      abs(q$y - q$y_pred_augmented)/1, 
      abs(q$y - q$y_pred_augmented) / q$y
    )
    

    MAD_simplistic    <- mean(q$AD_simplistic)
    MAD_TL            <- mean(q$AD_TL)
    MAD_augmented     <- mean(q$AD_augmented)
    
    
    TAD_simplistic <- sum(q$AD_simplistic)
    TAD_TL         <- sum(q$AD_TL)
    TAD_augmented  <- sum(q$AD_augmented)
    
    
    perc_err_unit_TL         <- mean(q$perc_err_unit_TL)
    perc_err_unit_simplistic <- mean(q$perc_err_unit_simplistic)
    perc_err_unit_augmented  <- mean(q$perc_err_unit_augmented)
    
    
    q$AD_diff           <-   q$AD_simplistic /  (q$AD_TL + epsilon)
    q$AD_diff_augmented <-   q$AD_simplistic /  (q$AD_augmented + epsilon) 
    
    q_final <- rbind(q_final, q)
    
    
    MADs_simplistic <- c(MAD_simplistic, MADs_simplistic)
    MADs_TL         <- c(MAD_TL, MADs_TL)
    MADs_augmented  <- c(MAD_augmented, MADs_augmented)
    
    TADs_simplistic <- c(TAD_simplistic, TADs_simplistic)
    TADs_TL         <- c(TAD_TL, TADs_TL)
    TADs_augmented  <- c(TAD_augmented, TADs_augmented)
    
    perc_err_unit_TLs <- c(perc_err_unit_TL, perc_err_unit_TLs)
    perc_err_unit_simplistics <- c(perc_err_unit_simplistic, perc_err_unit_simplistics)
    perc_err_unit_augmenteds <- c(perc_err_unit_augmented, perc_err_unit_augmenteds)
    
    
  }

  MAD_simplistic_res <- sum(MADs_simplistic)
  MAD_TL_res         <- sum(MADs_TL)
  MAD_augmented_res  <- sum(MADs_augmented)
  
  
  TAD_simplistic_res <- sum(TADs_simplistic)
  TAD_TL_res         <- sum(TADs_TL)
  TAD_augmented_res  <- sum(TADs_augmented)
  
  perc_err_on_total_TL         <- sum(TADs_TL) / sum(q_final$y) 
  perc_err_on_total_simplistic <- sum(TADs_simplistic)/sum(q_final$y)
  perc_err_on_total_augmented  <- sum(TADs_augmented)/sum(q_final$y)
  
  

  perc_err_TL_res         <-   mean(perc_err_unit_TLs) 
  perc_err_Simplistic_res <-   mean(perc_err_unit_simplistics)
  perc_err_augmented_res  <-   mean(perc_err_unit_augmented)
  
  
  # Filling the results list
  results$MAD_simplistic <- c(results$MAD_simplistic, MAD_simplistic_res)
  results$MAD_TL         <- c(results$MAD_TL, MAD_TL_res)
  results$MAD_augmented  <- c(results$MAD_augmented, MAD_augmented_res)
  
  
  results$TAD_simplistic <- c(results$TAD_simplistic, TAD_simplistic_res)
  results$TAD_TL         <- c(results$TAD_TL, TAD_TL_res)
  results$TAD_augmented  <- c(results$TAD_augmented, TAD_augmented_res)
  
  
  results$Perc_err_simplistic <- c(results$Perc_err_simplistic, perc_err_Simplistic_res)
  results$Perc_err_TL         <- c(results$Perc_err_TL, perc_err_TL_res)
  results$Perc_err_augmented  <- c(results$Perc_err_augmented, perc_err_augmented_res)
  
  q_final$perc_err_simplistic <- perc_err_Simplistic_res
  q_final$perc_err_TL         <- perc_err_TL_res
  q_final$perc_err_augmented  <- perc_err_augmented_res
  
  results$Perc_err_on_total_TL  <- c(results$Perc_err_on_total_TL, perc_err_on_total_TL)
  results$Perc_err_on_total_simplistic <- c(results$Perc_err_on_total_simplistic, perc_err_on_total_simplistic)
  results$Perc_err_on_total_augmented  <- c(results$Perc_err_on_total_augmented, perc_err_on_total_augmented)
  
  
  results$q_finals <- c(results$q_finals, list(q_final)) 
  
  

}    



q_final_final <- results$q_finals %>%
  bind_rows() %>%  # Merge all the dataframe in one
  group_by(turismo_pro_com, DENOM_COMUNE, ANNO, MESE, YYYYMM, y) %>%  # Group by on the unique key
  summarise(
    AD_diff = mean(AD_diff, na.rm = TRUE),
    AD_diff_augmented = mean(AD_diff_augmented, na.rm = TRUE),
    AD_simplistic = mean(AD_simplistic, na.rm = TRUE),
    AD_TL = mean(AD_TL, na.rm = TRUE),
    AD_augmented = mean(AD_augmented, na.rm = TRUE),
    y_pred = mean(y_pred, na.rm = TRUE),
    y_pred_simplistic = mean(y_pred_simplistic, na.rm = TRUE),
    y_pred_augmented = mean(y_pred_augmented, na.rm = TRUE),
    mean_perc_err_simplistic    = mean(perc_err_simplistic),
    mean_perc_err_TL            = mean(perc_err_TL),
    mean_perc_err_augmented     = mean(perc_err_augmented),
    .groups = "drop"  # avoid to mantain the grouping
  )



q_final_final <- q_final_final %>%
  mutate(y_pred_best = if_else(AD_diff > 1, y_pred, y_pred_simplistic))


#         Confidence interval computation for each municipality            


#Merging of all the simulation in one dataframe
q_full <- results$q_finals %>%
  bind_rows() %>%
  select(turismo_pro_com, DENOM_COMUNE, ANNO, MESE, YYYYMM, y_pred, y_pred_simplistic, y_pred_augmented)

# Confidence intervals computation
q_confidence <- q_full %>%
  group_by(turismo_pro_com, DENOM_COMUNE, ANNO, MESE, YYYYMM) %>%
  summarise(
    upper_confidence_interval = quantile(y_pred, 0.975, na.rm = TRUE),
    lower_confidence_interval = quantile(y_pred, 0.025, na.rm = TRUE),
    upper_confidence_interval_simplistic = quantile(y_pred_simplistic, 0.975, na.rm = TRUE),
    lower_confidence_interval_simplistic = quantile(y_pred_simplistic, 0.025, na.rm = TRUE),
    upper_confidence_interval_augmented  = quantile(y_pred_augmented,  0.975, na.rm = TRUE),
    lower_confidence_interval_augmented  = quantile(y_pred_augmented,  0.025, na.rm = TRUE),
    .groups = "drop"
  )


# Final Merge
q_final_final <- q_final_final %>%
  left_join(q_confidence, by = c("turismo_pro_com", "DENOM_COMUNE", "ANNO", "MESE", "YYYYMM"))

q_final_final <- q_final_final %>%
  mutate(
    confidence_ratio_TL = ifelse(
      y_pred != 0, 
      (upper_confidence_interval - lower_confidence_interval)/(y_pred + epsilon),
      NA
    ),
    confidence_ratio_simplistic = ifelse(
      y_pred_simplistic != 0, 
      (upper_confidence_interval_simplistic - lower_confidence_interval_simplistic) / (y_pred_simplistic + epsilon), 
      NA
    ),
    confidence_ratio_augmented = ifelse(
      y_pred_augmented != 0, 
      (upper_confidence_interval_augmented - lower_confidence_interval_augmented) / (y_pred_augmented + epsilon), 
      NA
    )
  )

q_final_final <- q_final_final %>%
  mutate(
    confidence_ratio_comparison = confidence_ratio_simplistic / (confidence_ratio_TL + epsilon),
    confidence_ratio_comparison_augmented = confidence_ratio_simplistic / (confidence_ratio_augmented + epsilon)
  )


q_final_final[] <- lapply(q_final_final, function(col) { # per sistemare i confidence intervals
  if (is.numeric(col)) col[is.nan(col)] <- 1
  return(col)
})




# Merges geographic data with q
mappa_er <- comuni_er %>%
  left_join(q_final_final, by = c("PRO_COM" = "turismo_pro_com"))


min_value <- quantile(mappa_er$AD_diff, 0.01, na.rm = TRUE)   # 1° percentile
mappa_er$AD_diff <- pmax(mappa_er$AD_diff, min_value) # Troncatura sopra il 1° percentile per togliere i valori -Inf
max_value <- quantile(mappa_er$AD_diff, 0.99, na.rm = TRUE)   # 99° percentile
mappa_er$AD_diff <- pmin(mappa_er$AD_diff, max_value) # Troncatura sopra il 99° percentile per togliere i valori Inf

mappa_er$AD_diff_log_adj <- mappa_er$AD_diff


max_abs <- max(abs(mappa_er$AD_diff_log_adj), na.rm = TRUE)

max_neg <- abs(min(mappa_er$AD_diff_log_adj, na.rm = TRUE))

max_pos <- max(mappa_er$AD_diff_log_adj, na.rm = TRUE)

clipping_value <- 2
midpoint <- 1 # 1 if simple division, 0 if log
delta <- clipping_value-midpoint


# if max_pos is more than 3*max_neg, truncate the values
if (max_pos > clipping_value * max_neg) {
  mappa_er$AD_diff_log_adj <- pmin(pmax(mappa_er$AD_diff_log_adj, -clipping_value+1), clipping_value)
  max_abs <- clipping_value
}


if (max_neg > clipping_value * max_pos) {
  mappa_er$AD_diff_log_adj <- pmin(pmax(mappa_er$AD_diff_log_adj, -clipping_value+1), clipping_value)
  max_abs <- clipping_value
}
delta <- max_abs - midpoint



ggplot(mappa_er) +
  geom_sf(aes(fill = AD_diff_log_adj), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = midpoint,
                       limits = c(midpoint-delta, midpoint+delta),  
                       name = "Abs.Err_Simplistic / Abs.Err_QTL") +
  theme_minimal() +
  labs(title = "Performance QTL vs Simplistic approach in Emilia-Romagna municipalities",
       subtitle = "Green: QTL is better | Red: Simplistic is better")



############################## Mappa augmented #################################

min_value_augmented <- quantile(mappa_er$AD_diff_augmented, 0.01, na.rm = TRUE)   # 1° percentile
mappa_er$AD_diff_augmented <- pmax(mappa_er$AD_diff_augmented, min_value_augmented) # Truncate under 1st percentile to remove -Inf
max_value_augmented <- quantile(mappa_er$AD_diff_augmented, 0.99, na.rm = TRUE)   # 99° percentile
mappa_er$AD_diff_augmented <- pmin(mappa_er$AD_diff_augmented, max_value_augmented) # Truncate over 99th ercentile to remove Inf

mappa_er$AD_diff_log_adj_augmented <- mappa_er$AD_diff_augmented


max_abs_augmented <- max(abs(mappa_er$AD_diff_log_adj_augmented), na.rm = TRUE)

max_neg_augmented <- abs(min(mappa_er$AD_diff_log_adj_augmented, na.rm = TRUE))

max_pos_augmented <- max(mappa_er$AD_diff_log_adj_augmented, na.rm = TRUE)

clipping_value <- 2
midpoint <- 1 
delta <- clipping_value-midpoint


if (max_pos_augmented > clipping_value * max_neg_augmented) {
  mappa_er$AD_diff_log_adj_augmented <- pmin(pmax(mappa_er$AD_diff_log_adj_augmented, -clipping_value+1), clipping_value)
  max_abs_augmented <- clipping_value
}


if (max_neg_augmented > clipping_value * max_pos_augmented) {
  mappa_er$AD_diff_log_adj_augmented <- pmin(pmax(mappa_er$AD_diff_log_adj_augmented, -clipping_value+1), clipping_value)
  max_abs_augmented <- clipping_value
}
delta_augmented <- max_abs_augmented - midpoint


# Map plot
ggplot(mappa_er) +
  geom_sf(aes(fill = AD_diff_log_adj_augmented), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = midpoint,
                       limits = c(midpoint-delta_augmented, midpoint+delta_augmented),  # Scala simmetrica
                       name = "Abs.Err_Simplistic / Abs.Err_AL") +
  theme_minimal() +
  labs(title = "Performance AL vs Simplistic approach in Emilia-Romagna municipalities",
       subtitle = "Green: AL is better | Red: Simplistic is better")



################################################################################




results_list <- list("MAE_simplistic"=mean(results$MAD_simplistic), "MAE_QTL"=mean(results$MAD_TL), "MAE_augmented"=mean(results$MAD_augmented),
                     "TAE_simplistic"=mean(results$TAD_simplistic), "TAE_QTL"=mean(results$TAD_TL), "TAE_augmented"=mean(results$TAD_augmented),
                     "MAE_simplistic_percentage"=mean(results$MAD_simplistic)/mean(q_final_final$y)*100, "MAE_QTL_percentage"=mean(results$MAD_TL)/mean(q_final_final$y)*100, "MAE_augmented_percentage"=mean(results$MAD_augmented)/mean(q_final_final$y)*100,
                     "Total_y"=sum(q_final_final$y), "Total_y_pred_QTL"=sum(q_final_final$y_pred), "Total_y_pred_augmented"=sum(q_final_final$y_pred_augmented),
                     "Total_y_pred_simplistic"=sum(q_final_final$y_pred_simplistic), "Total_y_pred_best"=sum(q_final_final$y_pred_best))




########################### Confidence interval plots ##########################



mappa_er$confidence_interval_amplitude_comparison <- (mappa_er$upper_confidence_interval_simplistic - mappa_er$lower_confidence_interval_simplistic) / ((mappa_er$upper_confidence_interval - mappa_er$lower_confidence_interval )+epsilon)

max_abs <- max(abs(mappa_er$confidence_interval_amplitude_comparison), na.rm = TRUE)

max_neg <- abs(min(mappa_er$confidence_interval_amplitude_comparison, na.rm = TRUE))

max_pos <- max(mappa_er$confidence_interval_amplitude_comparison, na.rm = TRUE)

clipping_value <- 2
midpoint <- 1 
delta <- clipping_value-midpoint

if (max_pos > clipping_value * max_neg) {
  mappa_er$confidence_interval_amplitude_comparison <- pmin(pmax(mappa_er$confidence_interval_amplitude_comparison, -clipping_value+1), clipping_value)
  max_abs <- clipping_value
}
if (max_neg > clipping_value * max_pos) {
  mappa_er$confidence_interval_amplitude_comparison <- pmin(pmax(mappa_er$confidence_interval_amplitude_comparison, -clipping_value+1), clipping_value)
  max_abs <- clipping_value
}

delta <- max_abs - midpoint

ggplot(mappa_er) +
  # geom_sf(aes(fill = confidence_ratio_comparison), color = "black", size = 0.1) +
  geom_sf(aes(fill = confidence_interval_amplitude_comparison), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = midpoint,
                       limits = c(midpoint-delta, midpoint+delta),  # Scala simmetrica
                       name = "Amplitude Conf.Int_Simplistic/ Amplitude Conf.Int_QTL") +
  theme_minimal() +
  labs(title = "Confidence Intervals amplitude QTL vs Simplistic approach in Emilia-Romagna municipalities",
       subtitle = "Green: AL is better | Red: Simplistic is better")


###################### Confidence intervals augmented ##########################

mappa_er$confidence_interval_amplitude_comparison_augmented <- (mappa_er$upper_confidence_interval_simplistic - mappa_er$lower_confidence_interval_simplistic) / ((mappa_er$upper_confidence_interval_augmented - mappa_er$lower_confidence_interval_augmented )+epsilon)

max_abs_augmented <- max(abs(mappa_er$confidence_interval_amplitude_comparison_augmented), na.rm = TRUE)

max_neg_augmented <- abs(min(mappa_er$confidence_interval_amplitude_comparison_augmented, na.rm = TRUE))

max_pos_augmented <- max(mappa_er$confidence_interval_amplitude_comparison_augmented, na.rm = TRUE)

clipping_value <- 2
midpoint <- 1 
delta <- clipping_value-midpoint

if (max_pos > clipping_value * max_neg) {
  mappa_er$confidence_interval_amplitude_comparison_augmented <- pmin(pmax(mappa_er$confidence_interval_amplitude_comparison_augmented, -clipping_value+1), clipping_value)
  max_abs <- clipping_value
}
if (max_neg > clipping_value * max_pos) {
  mappa_er$confidence_interval_amplitude_comparison_augmented <- pmin(pmax(mappa_er$confidence_interval_amplitude_comparison_augmented, -clipping_value+1), clipping_value)
  max_abs <- clipping_value
}

delta <- max_abs - midpoint

ggplot(mappa_er) +
  # geom_sf(aes(fill = confidence_ratio_comparison), color = "black", size = 0.1) +
  geom_sf(aes(fill = confidence_interval_amplitude_comparison_augmented), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = midpoint,
                       limits = c(midpoint-delta, midpoint+delta),  # Scala simmetrica
                       name = "Amplitude Conf.Int_Simplistic/ Amplitude Conf.Int_AL") +
  theme_minimal() +
  labs(title = "Confidence Intervals amplitude AL vs Simplistic approach in Emilia-Romagna municipalities",
       subtitle = "Green: AL is better | Red: Simplistic is better")


confidence_ratio_means <- q_final_final %>%
  summarise(
    mean_confidence_ratio_simplistic = mean(confidence_ratio_simplistic, na.rm = TRUE),
    mean_confidence_ratio_TL = mean(confidence_ratio_TL, na.rm = TRUE),
    mean_confidence_ratio_augmented = mean(confidence_ratio_augmented, na.rm = TRUE)
    
  )

confidence_ratio_means




if(filter_median)
{
  y_median <- original_median
  
}else{
  y_median <- 0
} 

# Print the median
print(paste("La mediana di y è:", y_median))

# Filter the municipalities with y > median and computes the MAPE for each observation
results_temp <- q_final_final %>%
  filter(y > y_median) %>%  
  mutate(sum_y = sum(y),
         sum_y_pred_simplistic = sum(y_pred_simplistic),
         sum_y_pred_AL  = sum(y_pred_augmented),
         sum_y_pred_QTL = sum(y_pred),
         mae_individual_QTL = abs(y - y_pred),
         mae_individual_AL = abs(y - y_pred_augmented),
         mae_individual_simplistic = abs(y - y_pred_simplistic),
         mape_individual_QTL = abs(y - y_pred) / (abs(y) + 1) * 100,
         mape_individual_AL = abs(y - y_pred_augmented) / (abs(y) + 1) * 100,
         mape_individual_simplistic = abs(y - y_pred_simplistic) / (abs(y) + 1) * 100)

# Compute the average MAPE, the mean of y, and the mean of y_pred for each municipality (grouped by DENOM_COMUNE).
mape_per_comune <- results_temp %>%
  group_by(DENOM_COMUNE) %>%
  summarise(mape_mean_QTL = mean(mape_individual_QTL, na.rm = TRUE),
            y_mean = mean(y, na.rm = TRUE),
            y_pred_QTL_mean = mean(y_pred, na.rm = TRUE),
            y_pred_AL_mean  = mean(y_pred_augmented, na.rm = TRUE),
            y_pred_simplistic_mean = mean(y_pred_simplistic, na.rm = TRUE)) %>%
  arrange(mape_mean_QTL)  # Ordinato dal MAPE più basso al più alto

# shows the first 350 municipalities, ordered by average MAPE
print(n = 350, mape_per_comune)

# "Calculate and print the overall MAE and MAPE (averaged over the entire dataset) for each type of prediction
mae_complessivo_QTL        <- mean(results_temp$mae_individual_QTL, na.rm = TRUE)
mae_complessivo_AL         <- mean(results_temp$mae_individual_AL, na.rm = TRUE)
mae_complessivo_simplistic <- mean(results_temp$mae_individual_simplistic, na.rm = TRUE)
mape_complessivo_QTL        <- mean(results_temp$mape_individual_QTL, na.rm = TRUE)
mape_complessivo_AL         <- mean(results_temp$mape_individual_AL, na.rm = TRUE)
mape_complessivo_simplistic <- mean(results_temp$mape_individual_simplistic, na.rm = TRUE)

# Compute and print the overall mean for y, y_pred and y_pred_augmented
y_complessivo                <- round(sum(results_temp$y, na.rm = TRUE))
y_complessivo_simplistic     <- round(sum(results_temp$y_pred_simplistic, na.rm = TRUE))
y_pred_complessivo_QTL       <- round(sum(results_temp$y_pred, na.rm = TRUE))
y_pred_complessivo_AL        <- round(sum(results_temp$y_pred_augmented, na.rm = TRUE))

# Print the result

print(paste("MAE complessivo Simplistic:", round(mae_complessivo_simplistic, 2)))
print(paste("MAE complessivo QTL:", round(mae_complessivo_QTL, 2)))
print(paste("MAE complessivo AL:", round(mae_complessivo_AL, 2)))

print(paste("MAPE complessivo Simplistic:", round(mape_complessivo_simplistic, 2)))
print(paste("MAPE complessivo QTL:", round(mape_complessivo_QTL, 2)))
print(paste("MAPE complessivo AL:", round(mape_complessivo_AL, 2)))

print(paste("y complessivo:",                round(y_complessivo, 2)))
print(paste("y complessivo Simplistic:",     round(y_complessivo_simplistic, 2)))
print(paste("y complessivo QTL:",       round(y_pred_complessivo_QTL, 2)))
print(paste("y complessivo AL:", round(y_pred_complessivo_AL, 2)))

# save.image("workspace_Random_Forest_TransferLearning_september.RData")
# save.image("workspace_Linear_Model_TransferLearning_october_STAGIONE.RData")
