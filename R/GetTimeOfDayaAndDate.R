#INPUT: dataframe imported by ImportData with FileName variable
#OPERATION: Extacts POSIXct from ndf filename and adds time information to df
#OUTPUT: Adds InitialisedTime as starttime of file + seconds, and TimeOfDay with date info
#v1:300716
#Requires library(stringr), library(lubridate), library(dplyr)
GetTimeOfDayAndDate<-function(SCPP_DF, time_zone){
  #Extract numeric component from filename to determine starttime
  match_regex<-"[0-9]+"
  StartTime<-str_match(SCPP_DF$FileName, match_regex)
  StartTime<-as.numeric(StartTime)
  SCPP_DF<-mutate(SCPP_DF, InitialisedTime=StartTime+Seconds)
  
  #Convert InititalisedTime to TimeOfDay
  SCPP_DF<-mutate(SCPP_DF, TimeOfDay=as.POSIXct(InitialisedTime, tz=time_zone, origin="1970-01-01"))
  return(SCPP_DF) 
}