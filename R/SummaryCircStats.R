#Function calculating summary circular stats from a vector of angles
#Input: Angles: vector of angles in radians. Circular object
#Output: dataframe with summary stats in single row
#Requires library(circular)
SummaryCircStats<-function(Angles){

  
  
  #Define function to return Rayleigh R and p as separate values
  
  Rayleigh.R<-function(x){
    R<-rayleigh.test(x)
    return(R$statistic)
  }
  
  Rayleigh.p<-function(x){
    R<-rayleigh.test(x)
    return(R$p)
  }
  #Circular summary statistics by Animal_ID
              MeanAngle = mean(Angles)
              Rho=rho.circular(Angles)
              SDAngle=sd.circular(Angles)
              RayleighR=Rayleigh.R(Angles)
              RayleighP=Rayleigh.p(Angles)
    
              AnglesCircSummary<-data.frame(
      MeanAngle=MeanAngle,
      Rho=Rho,
      SDAngle=SDAngle,
      RayleighR=RayleighR,
      RayleighP=RayleighP)
      
  
  
  return(AnglesCircSummary)
  
}
 

