SpikeRateStats<-function(SpikeCount){
  SpikeRateStatsReturn<-list()
  SpikeRateStatsDF<-data.frame()
 # SpikeRateStatsDF$Mean<-mean(SpikeCount)
  #SpikeRateStatsDF$SD<-sd(SpikeCount)
 # SpikeRateStatsDF$Max<-max(SpikeCount)
  ECDF<-ecdf(SpikeCount)
#  SpikeRateStatsReturn[[ECDF]]<-ECDF
#  SpikeRateStatsReturn[[SpikeRateStatsDF]]
  return(ECDF)
}