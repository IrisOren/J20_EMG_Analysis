
# EventTriggeredWindow takes the time of an event and extracts the triggered window of measure.
# EventTime = Time of event for max of triggered window
# WindowLength = Time preceding EventTime for window
# Dataframe should have InitialisedTime and measure for windowing 
# Returns a vector of data in window for averaging

#This modified script takes the window from the event and precedes by windowlength

#Tests for EventTriggeredWindow
#Time<-seq(from=100, to=200, by=1)
# Data<-cos(2*pi*Time/100)
#WindowLength=20
# EventTime<-140
#TW<-EventTriggeredWindow(EventTime, WindowLength, TimeVector=Time, DataVector = Data)
#plot(TW$TriggeredWindowTime, TW$TriggeredWindowData)

EventTriggeredWindowMod<-function(EventTime, WindowLength, TimeVector, DataVector){
  Tmin<-EventTime-WindowLength
  Tmax<-EventTime
  TriggeredWindowPoints<-which(((Tmin)<=TimeVector) &
                                 ((Tmax)>=TimeVector))
  TriggeredWindowTime<-TimeVector[TriggeredWindowPoints]
  TriggeredWindowData<-DataVector[TriggeredWindowPoints]
  
  TriggeredWindow<-data.frame(TriggeredWindowTime, TriggeredWindowData)
  return(TriggeredWindow)
  }