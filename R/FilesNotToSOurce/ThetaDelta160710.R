##Script to import plot and save output of Theta_Measure.tcl
##V1 10/07/16
##Set directory to contain text outputs with suffix Theta_Measure.txt 
##Operation
##For each Theta_Measure.txt 
## 1) imports data
## 2) Determines the start time and date of recording
## 3) Runs modified Hampel Filter to exclude outliers which exceed 3*scale factor (1.4826) away from median of +-5 points
## 4) Plots
## 5) Saves dataframe to .csv
#######################

library(lubridate)
library(dplyr)
library(ggplot2)
library(cowplot)
#####################################
#Remove outliers from timeseries using Hampel median filter:
#http://exploringdatablog.blogspot.co.uk/2012/01/moving-window-filters-and-pracma.html
#If the central data point lies more than t MAD scale estimate values from the median, it is replaced with the median; otherwise, it is left unchanged.
# Here, with samples of 8sec, k=5 corresponds to 80 second window for median evaluation. 
#Original HampelFilter excluded first and last k points. IO edited to take one-sided asymetric median in the start and end windows

HampelFilter <- function (x, k,t0=3){
  n <- length(x)
  y <- x
  ind <- c()
  L <- 1.4826
  for (j in 1:(k)){
    x0 <- median(x[(j):(j + k)])
    S0 <- L * median(abs(x[(j):(j + k)] - x0))
    if (abs(x[j] - x0) > t0 * S0) {
      y[j] <- x0
      ind <- c(ind, j)
    }
  }
  for (j in (k + 1):(n - k)) {
    x0 <- median(x[(j - k):(j + k)])
    S0 <- L * median(abs(x[(j - k):(j + k)] - x0))
    if (abs(x[j] - x0) > t0 * S0) {
      y[j] <- x0
      ind <- c(ind, j)
    }
  }
  for (j in (n-k+1):(n)){
    x0 <- median(x[(j-k):(j)])
    S0 <- L * median(abs(x[(j - k):(j)] - x0))
    if (abs(x[j] - x0) > t0 * S0) {
      y[j] <- x0
      ind <- c(ind, j)
    }
  }
  list(y = y, ind = ind)
}
##############
WorkingDirectory<-"~/Data/Neuroachiver/Telemetry J20s cerebellum reference/NDFwithVideo_JF220_JF221/JF220_Chan2/Theta_Measure_050716"
###########################


setwd(WorkingDirectory)
FileList<-list.files(pattern='Theta_Measure.txt') #List of all files in working directory
EndFile<-length(FileList)  #for testing purposes. This will be changed to length of FileList

for (i in 1:EndFile){
ThetaDelta_DF<-read.table(FileList[i], sep=" ")
names(ThetaDelta_DF)<-c("FileName", "Seconds", "Chan", "ThetaVal", "Reference", "ThetaMeasure")
ThetaDelta_DF<-select(ThetaDelta_DF, FileName, Seconds, Chan,  ThetaVal, Reference, ThetaMeasure)  #Selects only columns with data
CurrentNDF<-as.character(ThetaDelta_DF$FileName[i])
CurrentNDF<-sub(".ndf", "", CurrentNDF)  #Drop the .ndf suffix


#Get StartTime and reinitialise DF to run from StartTime
StartTime<-sub("M", "", CurrentNDF)  #Drop 'M' prefix
StartTime<-as.numeric(StartTime)
ThetaDelta_DF<-mutate(ThetaDelta_DF, InitialisedTime=StartTime+Seconds)


ThetaMeasureNoOut<-HampelFilter(ThetaDelta_DF$ThetaMeasure, k=5) 
ThetaMeasureNoOut<-ThetaMeasureNoOut$y

ThetaNoOut<-HampelFilter(ThetaDelta_DF$ThetaVal, k=5)
ThetaNoOut<-ThetaNoOut$y

ReferenceNoOut<-HampelFilter(ThetaDelta_DF$Reference, k=5)
ReferenceNoOut<-ReferenceNoOut$y


#ThetaOverDeltaNoOut is ratio of ThetaNoOut/ReferenceNoOut with resulting outliers excluded
ThetaOverDelta=ThetaNoOut/ReferenceNoOut
ThetaOverDelta<-ifelse(is.nan(ThetaOverDelta), 0, ThetaOverDelta)
ThetaOverDeltaNoOut<-HampelFilter(ThetaOverDelta, k=5)
ThetaOverDeltaNoOut<-ThetaOverDeltaNoOut$y

#Add ThetaOverDelta as calculated with no outliers
ThetaDelta_DF<-mutate(ThetaDelta_DF, ThetaMeasureNoOut, ThetaNoOut, ReferenceNoOut, ThetaOverDelta, ThetaOverDeltaNoOut)


#Convert InititalisedTime to TimeOfDay
ThetaDelta_DF<-mutate(ThetaDelta_DF, TimeOfDay=as.POSIXct(InitialisedTime, origin="1970-01-01"))

#plotting and saving
require(cowplot)
FontSize=14

#Plot ThetaMeasure outliers excluded
TitleString<-paste(CurrentNDF, " ", as.character(date(ThetaDelta_DF$TimeOfDay)), "Theta Measure")
ThetaMeasureNoOutPlot<-ggplot()
ThetaMeasureNoOutPlot<-ThetaMeasureNoOutPlot+
  geom_point(data=ThetaDelta_DF, aes(x=TimeOfDay,y=ThetaMeasureNoOut), color="green")+
  theme_minimal() + 
  ylab("Theta Measure") + 
  ggtitle(TitleString) + 
  theme(axis.title.y=element_text (size = FontSize))+
  theme(axis.text.y =element_text (size = FontSize))+
  theme(axis.text.x =element_blank())+
  theme(axis.title.x=element_blank())

#Plot ThetaOverDelta outliers excluded
TitleString<-paste(CurrentNDF, " ", as.character(date(ThetaDelta_DF$TimeOfDay)), "Theta/Delta")
ThetaOverDeltaNoOutPlot<-ggplot()
ThetaOverDeltaNoOutPlot<-ThetaOverDeltaNoOutPlot+
  geom_point(data=ThetaDelta_DF, aes(x=TimeOfDay,y=ThetaOverDeltaNoOut), color="blue")+
  theme_minimal() + 
  ylab("Theta over Delta") + 
  ggtitle(TitleString) + 
  theme(axis.title.y=element_text (size = FontSize))+
  theme(axis.text.y =element_text (size = FontSize))+
  theme(axis.text.x =element_blank())+
  theme(axis.title.x=element_blank())



#Plot of Theta outliers excluded
TitleString<-paste(CurrentNDF, " ", as.character(date(ThetaDelta_DF$TimeOfDay)), "4-12Hz")
ThetaNoOutPlot<-ggplot()
ThetaNoOutPlot<-ThetaNoOutPlot+
  geom_point(data=ThetaDelta_DF, aes(x=TimeOfDay,y=ThetaNoOut), color="red")+
  theme_minimal() + 
  ylab("Power") + 
  ggtitle(TitleString) + 
  theme(axis.title.y=element_text (size = FontSize))+
  theme(axis.text.y =element_text (size = FontSize))+
  theme(axis.text.x =element_blank())+
  theme(axis.title.x=element_blank())

#Plot delta outliers excluded
TitleString<-paste(CurrentNDF, " ", as.character(date(ThetaDelta_DF$TimeOfDay), "0.1-3.9 Hz"))
DeltaNoOutPlot<-ggplot()
DeltaNoOutPlot<-DeltaNoOutPlot+
geom_point(data=ThetaDelta_DF, aes(x=TimeOfDay,y=ReferenceNoOut), color="black")+
  theme_minimal() + 
  ylab("Power") + 
  ggtitle(TitleString) + 
  theme(axis.title.y=element_text (size = FontSize))+
  theme(axis.text.y =element_text (size = FontSize))+
  theme(axis.text.x =element_text (size = FontSize, angle=90))

PlotAll<-plot_grid(
  ThetaMeasureNoOutPlot, 
  ThetaOverDeltaNoOutPlot, 
  ThetaNoOutPlot, 
  DeltaNoOutPlot,
  labels = "AUTO", ncol = 1, align = 'v')


FileSaveName<-paste(CurrentNDF, ".pdf")
save_plot(
  FileSaveName,
  PlotAll, 
  base_height = 8,
  base_aspect_ratio = 1.1
  )

#Save to Mxxxxxxxxx.csv
FileSaveName<-paste(CurrentNDF, ".csv")
write.csv(ThetaDelta_DF, FileSaveName)
}







