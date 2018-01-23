#Function to convert radians to time in seconds 
#Input: Angles vector in radians 0-2Pi
#Output: Times vector in seconds

ConvertToSeconds<-function(AnglesVector){
  Times<-AnglesVector*(60*60*24)/(2*pi)
 
  return(Times) 
}