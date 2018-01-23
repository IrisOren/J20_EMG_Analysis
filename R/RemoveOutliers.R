#Remove outliers in a column of a data frame using HampelFilter
#INPUT: dataframe, column to smooth (string), new column (string)
#OUTPUT: returns dataframe with new column appended
#v1:310716

RemoveOutliers <- function(DF, column, new_col){
  #use [[]] to pass column name to evaluate column variable
  column_to_smooth<-DF[[column]]
  #calls hampel filter with 5 point median smoothing, and a threshold value of 3.
  temp_vec<-HampelFilter(column_to_smooth, 5, 3)
  #Hampel filter returns a list. Append the smoothed vector (y) as a column of df
  DF[[new_col]]<-temp_vec$y
  return(DF)
  }
    
