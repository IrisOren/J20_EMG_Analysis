#Function to convert time in seconds to radians
#Input: Time vector in seconds
#Output: Vector in radians

ConvertToRadians<-function(TimesVector){
  Angles<-TimesVector*2*pi/(60*60*24)
  #Apend column with angles as circular object
  Angles<- circular(Angles, units="radians", modulo="2pi", zero=0, rotation="clock")

  return(Angles) 
}