setwd(dirname(rstudioapi::getSourceEditorContext()$path))


options(max.print = 100000)


# Function to compute u_tilde_list based on alpha
compute_u_tilde <- function(alpha, beta_hat, x_list, Y_star)
{
  K <- length(Y_star)
  u_tilde_list <- numeric(K)
  for (k in 1:K) 
  {
    x_k      <- x_list[[k]]
    Y_k_star <- Y_star[k]
    
    u_tilde_list[k] <- (alpha / (1 + alpha)) * (Y_k_star - sum(x_k * beta_hat))
  }
  
  return(u_tilde_list)
}



# Function to estimate optimal alpha minimizing the distance
estimate_optimal_alpha <- function(u_hat, beta_hat, x_list, Y_star, distance_func, alpha_values, D)
{
  loss_values <- numeric(length(alpha_values))
  
  u_hat_D  <- u_hat[D]
  x_list_D <- x_list[D]
  Y_star_D <- Y_star[D]
  
  for (i in seq_along(alpha_values)) 
  {
    alpha <- alpha_values[i]
    
    u_tilde_D   <- compute_u_tilde(alpha, beta_hat, x_list_D, Y_star_D)

    loss_values[i] <- distance_func(u_hat_D, u_tilde_D)
  }
  
  alpha_optimal   <- alpha_values[which.min(loss_values)]
  
  return(alpha_optimal)
}



# Dot product as a distance
dot_product <- function(u, v) {
  return(sum(u * v))
}

# Euclidean distance function
euclidean_distance <- function(u_hat, u_tilde)
{
  sum((u_hat - u_tilde)^2)
}

# Cosine distance function with norm checks
cosine_distance <- function(u_hat, u_tilde)
{
  # Calculate the dot product between the two vectors
  dot_product <- sum(u_hat * u_tilde)
  
  # Calculate the norms (magnitudes) of the two vectors
  norm_u_hat   <- sqrt(sum(u_hat^2))
  norm_u_tilde <- sqrt(sum(u_tilde^2))
  
  #Check if either of the vectors has a zero norm
  if (norm_u_hat == 0 || norm_u_tilde == 0) {
    # If either vector is zero, return a maximum distance of 1
    return(1)
  }
  
  # Calculate the cosine distance
  cosine_dist <- 1 - (dot_product / (norm_u_hat * norm_u_tilde))
  
  return(cosine_dist)
}

# L1-Distance
manhattan_distance <- function(u_hat, u_tilde) {
  
  manhattan_dist <- sum(abs(u_hat - u_tilde))
  return(manhattan_dist)
}




load("yourpath//modelli_out_FH.RData")
fh_fit <- fh3

load("yourpath//REGISTRO_dist.RData")
registro<-REGISTRO_dist[,c(1,4:8,16)]
registro$orig<-as.numeric(as.character(registro$ORIG))

registro<-aggregate(cbind(N_studio , N_lavoro  , N_lavorosubord ,
                          N_tot_SL , flusso, NUM_SIM)~orig,data=registro,sum)
REGISTRO<-registro

REGISTRO$ORIG   <- as.numeric(as.character(REGISTRO$orig))
REGISTRO$DOMAIN <- REGISTRO$ORIG
REGISTRO$lMNO <- log(REGISTRO$NUM_SIM +1) 
REGISTRO$lflusso2011   <- log(REGISTRO$flusso+1)
REGISTRO$lAdmin<-log(REGISTRO$N_tot_SL +1)

# Indexes for OD_Toscana  
keys_OD_Toscana  <-REGISTRO$DOMAIN
D1_OD_Toscana    <- match(fh_fit$ind$Domain, keys_OD_Toscana) # cambiare nome in D1_fh_fit?
D1_NA_OD_Toscana <- which(is.na(D1_OD_Toscana)) # Values in fit which are not in OD_Toscana have index = NA
REGISTRO_temp    <- 1:length(REGISTRO[[1]])
D0_OD_Toscana    <- setdiff(REGISTRO_temp, D1_OD_Toscana)
D_length         <- length(D1_OD_Toscana)+length(D0_OD_Toscana)


data <- list(x_list = vector("list", D_length), u_hat = numeric(D_length), Y_star = numeric(D_length), orig=numeric(D_length) )


############################## D1 preparation ##################################

D1 <- 1:length(fh_fit$framework$combined_data$lMNO)
for (k in D1)
{
  data$orig[k]     <- fh_fit$framework$combined_data$orig[k]
  data$domain[k]   <- fh_fit$framework$combined_data$orig[k]
  data$x_list[[k]] <- c(1, fh_fit$framework$combined_data$lMNO[k])
  
  data$u_hat[k]   <- fh_fit$model$random_effects[k] # no log(?)
  

  data$Y_star[k]  <- fh_fit$framework$combined_data$lflusso2011[k]
  
}

idx_data_flussi_not_found <- c()

#browser()

D0 <-c()
for (k in 1:length(D0_OD_Toscana))
{
  kk <- length(D1) + k  # Offset to place the index at the end of `D1`
  D0 <- c(D0,kk)
  
  if (!is.na(D0_OD_Toscana[k]))
  {
    data$orig[kk] <- REGISTRO[D0_OD_Toscana[k], "ORIG"]
    data$x_list[[kk]] <- c(1, REGISTRO$lMNO[k]) #log? #+1 ????
    data$domain[kk]   <- REGISTRO[D0_OD_Toscana[k], "DOMAIN"]
    
    data$Y_star[kk]   <- REGISTRO[D0_OD_Toscana[k], "lflusso2011"]
    
  } else
  {
    print("I must not go here, please check")
  }
  
  data$u_hat [kk] <- NA
  
  

}


beta_hat <- fh_fit$model$coefficients$coefficients
P        <- length(beta_hat)

# exclude the data without flussi 

D_length    <- length(data$x_list)
D1_length   <- length(D1)  # - sum(idx_removed <= length(D1_OD_Toscana))

# indexes for data
D  <- 1:D_length
D1 <- 1:D1_length            #D1 <- 1:length(D1_OD_Toscana)
D0 <- (D1_length+1):D_length #(length(D1_OD_Toscana)+1):D_length


# Define alpha values to search for the optimal one
#alpha_values <- seq(0, 1, by = 0.001)  
alpha_values <- seq(1, 30, by = 0.01)
#alpha_values <- seq(0, 1, by = 0.1) 

# Estimate optimal alpha and corresponding u_tilde_list using only D1
#
#alpha_optimal <- 0.2215
alpha_optimal <- estimate_optimal_alpha(data$u_hat, beta_hat, data$x_list, data$Y_star, euclidean_distance, alpha_values, D1)   # 0.2215
#alpha_optimal <- estimate_optimal_alpha(data$u_hat, beta_hat, data$x_list, data$Y_star, manhattan_distance, alpha_values, D1)  # 0.1791
#alpha_optimal <- estimate_optimal_alpha(data$u_hat, beta_hat, data$x_list, data$Y_star, cosine_distance, alpha_values, D1)     # 0.0262  
#alpha_optimal <- estimate_optimal_alpha(data$u_hat, beta_hat, data$x_list, data$Y_star, dot_product, alpha_values, D1)         # 0


# Display the optimal alpha and the corresponding u_tilde_list
alpha_optimal
u_tilde_0 <- compute_u_tilde(alpha_optimal, beta_hat, data$x_list[D0], data$Y_star[D0] ) 
u_tilde   <- compute_u_tilde(alpha_optimal, beta_hat, data$x_list, data$Y_star) 


# Calculate Y_hat_0 for indices in D0
Y_hat_0 <- sapply(D0, function(k) sum(data$x_list[[k]] * beta_hat) + u_tilde[k])

Y_hat_1 <- fh_fit$model$fitted
data$Y_hat <- Y_hat_1
data$Y_hat[(length(Y_hat_1)+1):(length(Y_hat_1)+length(Y_hat_0))] <- Y_hat_0

# Display Y_hat_0
alpha_optimal
u_tilde_0
Y_hat_0

#convert into dataframe
df_data_no_x <- as.data.frame(data[setdiff(names(data), "x_list")])
df_xlist <- do.call(rbind, lapply(data$x_list, function(x) unlist(x)))
df_data <- cbind(df_xlist, df_data_no_x)
colnames(df_data)[1:2] <- c("X_intercept", "X1")
rm(list = c("df_data_no_x", "df_xlist"))

Xbeta_D0 <- sapply(D0, function(k) sum(data$x_list[[k]] * beta_hat))
Xbeta_D1 <- sapply(D1, function(k) sum(data$x_list[[k]] * beta_hat))

save(df_data, D1, D0, beta, Xbeta_D0, Xbeta_D1, alpha_optimal, u_tilde, file = "TL_results_out_fitLMNO.RData")

df_data$synt<-c(Xbeta_D1,Xbeta_D0)
ggplot()+
  geom_line(data = df_data, aes(x = orig, y = Y_hat, color = "adj"))+
  geom_line(data = df_data, aes(x = orig, y = synt , color = "synth"))+
  geom_abline()+
  ylab('estimates & covariates')+xlab('municipalities')+
  theme_minimal()+
  labs(
    colour = "estimates & covariates",
  )

ts_data<-as.ts((df_data[,c(2,4,7,8)]))

plot.ts(ts_data[,3:4],col = 1:2 ,plot.type = "single")

plot.ts(as.ts(df_data$synt),  col="blue",add=T)

ts.plot(time,gpars= list(col=rainbow(10)))
plot(df_data$Y_hat, lty=1)
points(c(Xbeta_D1,Xbeta_D0), lty=1 , col="blue",pch=4)
points(df_data$Y_hat, col="red",pch=4)
points(df_data$X1, col="green",pch=8)

################################################################################
#       Transfer Learning from "Disaggregation of trips using MNO data"        #
################################################################################

load("yourpath//StimeQR.RData")

QR_length <- length(StimeQR[[1]])

df_data_QR        <- list(Y_hat = numeric(QR_length), X=numeric(QR_length), p_hat = numeric(QR_length), q = numeric(QR_length), orig=numeric(QR_length) , dest=numeric(QR_length))
df_data_QR        <- as.data.frame(df_data_QR)

df_data_QR$domain <- paste(StimeQR$orig, StimeQR$dest) 
df_data_QR$orig   <- StimeQR$orig
df_data_QR$dest   <- StimeQR$dest
df_data_QR$Y_hat  <- StimeQR$YhatM_prov
df_data_QR$X      <- df_data$X1[match(df_data_QR$domain, df_data$domain)]

# Check
#match(df_data_QR$domain, df_data$domain)
#df_data_QR[1,]
#df_data[5121,]

Y_hat <- sum(df_data_QR$Y_hat)
X_hat <- sum(df_data_QR$X)

for(i in 1:QR_length)
{
  df_data_QR$p_hat[i] <- df_data_QR$Y_hat[i]/Y_hat    
  df_data_QR$q[i]     <- df_data_QR$X[i]/X_hat
}  


p_hat_avg <- mean(df_data_QR$p_hat)
V_hat_p_hat <- (1/(QR_length-1))*sum((df_data_QR$p_hat-p_hat_avg)^2)

tau_hat_u <- (1/QR_length)*sum((df_data_QR$p_hat- df_data_QR$q)^2-V_hat_p_hat)
tau_hat_e <- (1/QR_length)*sum(V_hat_p_hat)
phi <- tau_hat_u/(tau_hat_u + tau_hat_e)

p_point <-  phi * df_data_QR$p_hat + (1-phi) * df_data_QR$q
df_data_QR$p_point <- p_point


################################################################################
#                              Robust estimation                               #
################################################################################

Y_hat_avg   <- mean(df_data_QR$Y_hat)
V_hat_Y_hat <- (1/(QR_length-1))*sum((df_data_QR$Y_hat-Y_hat_avg)^2)
B=100

set.seed(123)
Y_star_hat_bootstrap_replica <- list()
p_star_array <- c()
mse_array    <- c()
Y_star_tot <- sum(df_data_QR$Y_hat)
K<-length(df_data_QR$Y_hat)


for(k in 1:K)  
{
  Y_star_k <- df_data_QR$Y_hat[k]
  Y_star_hat_bootstrap_replica[[k]] <- rnorm(n=B, mean=Y_star_k, sd = sqrt(V_hat_Y_hat))
  
  p_star <- Y_star_k / Y_star_tot
  df_data_QR$p_star[k] <- p_star
  
  p_star_array  <- c(p_star_array, p_star)
  
}


p_star_hat_replica <- vector("list", K)
sum_Y_star_at_b <- numeric(B) 

# Computation of the sum of every k in position b
for (b in 1:B) {
  sum_Y_star_at_b[b] <- sum(sapply(1:K, function(k) Y_star_hat_bootstrap_replica[[k]][b]))
}

# Computation of p_star_hat_replica[k][b] and of mse for each k
for (k in 1:K) {
  p_star_hat_replica[[k]] <- sapply(1:B, function(b) Y_star_hat_bootstrap_replica[[k]][b] / sum_Y_star_at_b[b])
  
  mse <- (1/B)*sum((p_star_hat_replica[[k]]-p_star_array[k])^2)
  mse_array <- c(mse_array, mse)
}

################################################################################
#            5.2 Combining synthetic regression and MNO breakdown              #
################################################################################
mu_hat <- rnorm(n=K)  
mu     <- rnorm(n=K) 

Y_tot <- sum(df_data$Y_star)

w <- sum((df_data_QR$p_hat - df_data_QR$q)^2) / (  sum(((df_data_QR$p_hat - mu)/Y_tot)^2) + sum((df_data_QR$p_hat - df_data_QR$q)^2)  )

p_hat_MX <- w*mu_hat + (1-w) * df_data_QR$q