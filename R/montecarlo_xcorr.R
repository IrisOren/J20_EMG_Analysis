#Function to compute a cross correlation null distribution with CI by montecarlo simulation 
#Inputs: 
# -vectors x, y for determining cross correlation
# - runs: number of runs of mc
# - quantiles to compute
# Output list:
# [1]: acf coefficients for all runs of mc
# [2]: dataframe of "lag" x columns. Rows [mean, q1, median, q2] 
montecarlo_xcorr <- function(x, y, runs, quantile1, quantile2) {
  ccf_permute_all <- c() 
  for (i in 1:runs) {
    #Permute x
    x_permute <- sample(x)
    #take ccf of permuted x and original y
    ccf_permute_temp <- ccf(x_permute, y, plot=FALSE)
    #create df of all permuted ccf outputs
    ccf_permute_all <- cbind(ccf_permute_all, ccf_permute_temp$acf)
  }
  #compute mean and quantiles for each lag=dim(ccf_permute_all)[1] and write to matrix
  mean_quantiles <- c()
  for (j in 1:dim(ccf_permute_all)[1]){
    temp<-ccf_permute_all[j,]
    q <- quantile(temp, c(quantile1, 0.5, quantile2), names=FALSE)
    m <- mean(temp)
    mean_quantiles_j <- c(m, q)
    mean_quantiles <- cbind(mean_quantiles, mean_quantiles_j)
    
  }
  montecarlo_xcorr_out <- list(ccf_permute_all, mean_quantiles)
  return(montecarlo_xcorr_out)
}