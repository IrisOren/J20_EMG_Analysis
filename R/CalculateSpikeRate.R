#Function to calculate average spike rate across all intervals included in DataFrame.
#IntervalLength in s
CalculateSpikeRate<-function(DataFrame, IntervalLength){
  TotalSpikes<-sum(DataFrame$SpikeCount)
  TotalIntervalNumber<-nrow(DataFrame)
  SpikeRate<-TotalSpikes/(TotalIntervalNumber*IntervalLength)
  return(SpikeRate)
  }