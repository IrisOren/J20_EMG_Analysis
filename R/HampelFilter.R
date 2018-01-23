#####################################
#Remove outliers from timeseries using Hampel median filter:
#http://exploringdatablog.blogspot.co.uk/2012/01/moving-window-filters-and-pracma.html
#If the central data point lies more than t MAD scale estimate values from the median, it is replaced with the median; otherwise, it is left unchanged.
# Here, with samples of 8sec, k=5 corresponds to 80 second window for median evaluation. 
#Original HampelFilter excluded first and last k points. IO edited to take one-sided asymetric median in the start and end windows

HampelFilter <- function (x, k=5,t0=3){
  n <- length(x)
  y <- x
  ind <- c()
  L <- 1.4826
  for (j in 1:(k)){
    x0 <- median(x[(j):(j + k)])
    S0 <- L * median(abs(x[(j):(j + k)] - x0))
    if (abs(x[j] - x0) > t0 * S0) {
      y[j] <- x0
      ind <- c(ind, j)
    }
  }
  for (j in (k + 1):(n - k)) {
    x0 <- median(x[(j - k):(j + k)])
    S0 <- L * median(abs(x[(j - k):(j + k)] - x0))
    if (abs(x[j] - x0) > t0 * S0) {
      y[j] <- x0
      ind <- c(ind, j)
    }
  }
  for (j in (n-k+1):(n)){
    x0 <- median(x[(j-k):(j)])
    S0 <- L * median(abs(x[(j - k):(j)] - x0))
    if (abs(x[j] - x0) > t0 * S0) {
      y[j] <- x0
      ind <- c(ind, j)
    }
  }
  list(y = y, ind = ind)
}