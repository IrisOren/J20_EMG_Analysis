#Function to copy SCPP1V1 processor output to local R directory
#For working directory IIS_ThetaNThetaR: 
#input_path = Relative to project working directory = '../IIS_ThetaNTheta/JFXXX_ChanY/SCPP1V1_TCL'
#output_path= Relative to project working directory= 'Data/JFXXX_ChanY/SCPP1V1_TCL'. Output path needs to be created in main routine
#V1: 300716. 
CopyToLocalDataDir<- function(input_path, suffix, output_path) {
  file_list<-list.files(path=input_path, pattern=suffix, full.names = TRUE)
  for (file_name in file_list){
    file.copy(file_name, output_path)
    }
}