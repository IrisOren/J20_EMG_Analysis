#Function which writes the behavioural scoring variable from "VideoScore" to each row of SCPP1Time using VideoTime
AddVideoScoreToSCPPData<-function(SCPPTime, VideoTime, VideoScore){
  j<-1
  #SCPP1Score<-vector(length=length(SCPP1Time)) #Initialise vector for video score
  SCPPScore<-c()
  SCPPScore<-as.character(SCPPScore)
  i<-1
  for(i in 1:length(VideoTime)){
    VideoScoreCurrent<-VideoScore[i]
    if(i<length(VideoTime)){ #condition and break needed to make sure that j!>length(VideoTime-1) so that while condition can be evaluated
        while(SCPPTime[j]<=VideoTime[i+1]){
          if(j<length(SCPPTime)){
                SCPPScore[j]<-VideoScoreCurrent
                j<-j+1
          } else {
            break
            }
    
        } 
    }else{
        repeat{
          SCPPScore[j]<-VideoScoreCurrent
          j<-j+1
        if(j==length(SCPPTime)+1){
          break
        }
      }
      }
        
    
  }
return(SCPPScore)
}