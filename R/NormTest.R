NormTesting<-function(DataForTest, QQFilename){
  SWilk<-shapiro.test(DataForTest)
  PathAndFilename<-paste("./Output/",QQFilename,".jpg", sep="")
  jpeg(PathAndFilename)
  QQPlot<-qqnorm(DataForTest, main=QQFilename)
  qqline_WT<-qqline(DataForTest)
  dev.off()
  
 return(SWilk) 
}

  
