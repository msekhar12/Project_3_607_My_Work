awards_bayes <- function(df,cat_1, cat_2, year)
{
  #This function calculates the P(cat_1 | cat_2_nominated) = P(cat_2_nominated | cat_1) . P(cat_1)/P(cat_2)
  #P(cat_2_nominated | cat_1) is calculated from old data
  #P(cat_1) is calculated using the current year's data
  #P(cat_2) is calculated using the old data
  
  #Finding P(cat_1):
  P_of_cat_1_current_year <- 
    sum(awards_re_modified$year == year & !is.na(awards_re_modified[,cat_1])) / nrow(awards_re_modified[awards_re_modified$year == year,])
  
  #Finding P(cat_2): 
  #P(cat_2) = Probability of getting a movie nominated, using the history data
  # ==> # of times cat_2 is 1 or 0/number of rows of cat_2 including NA
    P_of_cat_2_history_data <- sum(awards_re_modified$year < year & !is.na(awards_re_modified[,c2])) /nrow(awards_re_modified[awards_re_modified$year < year,])
  
  #Finding P(cat_2_nominated | cat_1)
  #From the history data, find the number of times a movie was nominated for editing, and has also won the best picture award. and divide this count by
  #Number of times a movie has been nominated
  
}