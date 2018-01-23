#Exclude intervals:
# a)  classified as L
# b) with theta and delta power approx 0 (ie. transmitters off)

ExcludeLossIntervals <- function(DF, Threshold){
  DF <-filter(DF , EpochClass!="L")
  DF <- filter(DF, Theta>0.00001 & Delta>0.00001)
  return(DF)
  
  
  
  
}