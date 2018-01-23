#This script is used to export the characeteristics files for indiviidual animals
#The output EX.txt file will be used as input to the toolmaker script on LWDAQ to create event list
#Animal ID and Filename need to be set
#Creates .txt file in SCPP4V1/JFXXX subdirectory

AnimalIDCurrent<-"J0460"
Filename<-"J0460_CharacteristicsAll.txt"

#SCPPDataframeAll<-read.csv("./Output/SCPPDataframeAll.csv")

Temp<-filter(SCPPDataframeAll, AnimalIDVariable==AnimalIDCurrent)
Temp<-Temp[,2:9]
FilenameWithPath<-paste("~/Dropbox/ANALYSIS/Neuroarchiver/J20EEG/SCPP4V1/", AnimalIDCurrent,"/", Filename, sep="")
write.table(Temp, file=FilenameWithPath, col.names=FALSE, row.names = FALSE, quote=FALSE, sep=" ")
