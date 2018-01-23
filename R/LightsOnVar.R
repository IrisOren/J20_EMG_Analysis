#Function determines whether time_of_day is between light_on or
#and light_off. Returns logical TRUE FALSE
#Input: date time object POSIXct format, light_on and light_off in between 1 and 24
#050816

LightsOnVar <- function(time_of_day, light_on, light_off){
  hour_of_event<-hour(time_of_day)
  LightsOnVar<-hour_of_event %in% seq(light_on, light_off)
  return(LightsOnVar)
}