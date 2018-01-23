#Function to unix time to time of day in hours and decimal
#Input: Time vector in unix time
#Output: Dataframe with time in date-time format and hours and decimals of full hour

ConvertUnixSecondsToTimeOfDay<-function(TimeSeconds){
  require(lubridate)
  TimeOfDay<-as.POSIXct(TimeSeconds, origin="1970-01-01")
  TimeHours<-hour(TimeOfDay)
  TimeMinutes<-minute(TimeOfDay)
  TimeSeconds<-second(TimeOfDay)
  TimeOfDayTimeFormat<-TimeHours+TimeMinutes/60+TimeSeconds/3600
  TimeReturn<-data.frame(TimeOfDay, TimeOfDayTimeFormat)
  return(TimeReturn) 
}