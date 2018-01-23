#This script is used to test the EventTriggeredAveragingOfThetaDelta chunk
ETWList<-list()

ETAv<-rep(0,13)
SpikeCounterForAveraging<-0
for(StartTime in 1:4){
  TriggeredWindowTime<-seq(StartTime, StartTime+12, 1)
  TriggeredWindowData<-c(rep(1, 5), rep(2,5), 1,1, 1 )
  ETW<-data.frame(TriggeredWindowTime, TriggeredWindowData)
  
  if(nrow(ETW)==NoOfIntervalsForAveraging+1){
    ETAv<-ETAv+ETW$TriggeredWindowData
    ETWList[[SpikeIndex]]<-ETW
    SpikeCounterForAveraging<-SpikeCounterForAveraging+1
  }else{
    #print(SpikeIndex)
  }
}
if(SpikeCounterForAveraging>0){
  ETAv<-ETAv/SpikeCounterForAveraging
}
######################
NETWList<-list()

NETAv<-rep(0,13)
SpikeCounterForAveraging<-0
for(StartTime in 1:4){
  TriggeredWindowTime<-seq(StartTime, StartTime+12, 1)
  TriggeredWindowData<-c(1, 1, 1, rep(1, 5), rep(2,5))
  NETW<-data.frame(TriggeredWindowTime, TriggeredWindowData)
  
  if(nrow(NETW)==NoOfIntervalsForAveraging+1){
    NETAv<-NETAv+NETW$TriggeredWindowData
    NETWList[[SpikeIndex]]<-NETW
    SpikeCounterForAveraging<-SpikeCounterForAveraging+1
  }else{
    # print(SpikeIndex)
  }
}
if(SpikeCounterForAveraging>0){
  NETAv<-NETAv/SpikeCounterForAveraging
}