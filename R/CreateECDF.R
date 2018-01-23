CreateECDF<-function(DF, GenotypeFactor, AnimalIDFactor){
SpikeCountDF<-data.frame(SpikeCount=DF$SpikeCount)

SpikeCountDF <- SpikeCountDF %>% group_by(SpikeCount) %>%  
  dplyr::summarise(NumIntervals=n())

SpikeCountDF <- SpikeCountDF %>%  
  arrange(SpikeCount) %>% 
  mutate(CumFrequency=cumsum(NumIntervals))
#And Normalise CumFrequency by max of CumFrequency
  SpikeCountDF<- SpikeCountDF %>%
    mutate(CumFrequency=CumFrequency/max(CumFrequency))

#Add Genotype
if(GenotypeFactor=="WT"){
  SpikeCountDF <- SpikeCountDF %>% 
    mutate(Genotype="WT")
}
else{
  SpikeCountDF <- SpikeCountDF %>% 
    mutate(Genotype="J20")
}

 
  SpikeCountDF$Genotype<-as.factor(SpikeCountDF$Genotype)
  
  #Add animalID
  SpikeCountDF<- SpikeCountDF %>%
    mutate(AnimalIDVariable=AnimalIDFactor)


return(SpikeCountDF) 
}

