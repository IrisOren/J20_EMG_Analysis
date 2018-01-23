#Exclude intervals:
# a)  with Loss > LossThreshold
# b) with theta and delta power approx 0 (ie. transmitters off)
# c) Excessive delta power which we classify as artifacts. 
ExcludeIntervals <- function(DF, LossThreshold, DeltaPowerThreshold){
  DF <-filter(DF , Loss > LossThreshold)
  DF <- filter(DF, Theta>0.00001 & Delta>0.00001)
  DF <- filter(DF, Delta > DeltaPowerThreshold)
return(DF)
  
  
  
  
}