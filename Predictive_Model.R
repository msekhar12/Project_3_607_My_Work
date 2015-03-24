#Data Analysis
#Sekhar
#Work in progress - 2/22/2015

library(dplyr)
library(tidyr)

setwd("C:/Users/Sekhar/Documents/Github/Project_3_607_My_Work")
awards_df <- read.csv("Awards_File.csv",stringsAsFactors=F)
awards_modified <- awards_df[,c(1,3,4,6)]
head(awards_modified)

#The awards_modified data frame has some movies, which are nominated for the same category more than once, and has also won for one of its nominations (see below example) 
awards_modified[awards_modified$movie_id==1288,]

#movie_id year category_id won
#1288 1984           1   0
#1288 1984           1   1
#1288 1984           6   1
#1288 1984           7   0
#1288 1984           8   1
#1288 1984           9   1
#1288 1984          12   0
#1288 1984          13   1
#1288 1984          16   1
#1288 1984          19   1
#1288 1984          22   1

#We have to group by movie_id, year, category_id, and max(won)
#such group by will include just the winning nomination, if a film is nominated multiple times in the same category, and one of the nominations win in that category.

awards_modified <- awards_modified %>%
  group_by(movie_id,year,category_id) %>%
  summarise(won=max(won))

#The above transformation has eliminated all the duplicate rows, where the same movie has been nominated in the same category more than once and one of them wins.
#It just includes the winning nomination. If none of the multiple nominations win, then only one of the nominations are included.
#The below command confirms that for the movie_id=1536, only the winning nomination is inluded
awards_modified[awards_modified$movie_id==1288,]
movie_id year category_id won
#1288 1984           1   1
#1288 1984           6   1
#1288 1984           7   0
#1288 1984           8   1
#1288 1984           9   1
#1288 1984          12   0
#1288 1984          13   1
#1288 1984          16   1
#1288 1984          19   1
#1288 1984          22   1



#Now the spread() function can be applied to awards_modified data frame.
awards_re_modified <- spread((awards_modified),category_id,won)

#The above command will produce another data frame called awards_re_modified, with the following columns:
#movie_id - Movie IDentifier
#Year - Year of award
#Award categories from 1 to 23
#Wherever a movie wins award, the respective category will have 1, wherever a movie is nominated, then the corresponding category will have 0, and wherever the movie is 
#neither nominated nor wins, we will have NA

head(awards_re_modified)

#To get the category names (since the above dataframe contains the categories as numbers), let us create a separate data frame:

award_categories <- unique(data.frame(category_id=awards_df$category_id,category_name=awards_df$category_name))
rownames(award_categories) <- NULL

award_categories


#Since the variables are not allowed to start with numeric values, let us rename the column names of the data frame awards_re_modified to character variables.
#For example, the variable name "1" represents the award category_ID 1. We will change this to c1, to represent category 1.

names(awards_re_modified) <- c("movie_id", "year",  "c1",  "c2",  "c3",  "c4",  "c5", "c6",  "c7",  "c8",  "c9", "c10", "c11", "c12",
                               "c13", "c14", "c15","c16", "c17", "c18", "c19", "c20", "c21", "c22", "c23")


#The "Best Picture" category_id is 16 and "Film Editing" category_id is 12
#We have to identify which categories can help us to predict if a film gets the best picture, and determine if film_editing has the maximum probability.

#So finally we have the following data frames to work/use for our analysis:
#award_categories
#awards_re_modified

#Let us get a conditional probability matrix, for the following:
#Probability of getting Best Movie, given that it is nominated in a paricular category?

# P(Best Movie | It is nominated in x category) = P( that the movie won in Best Movie and also nominated in x category) / P(that a movie is nominated in x category)
  p[i] <- (p_of_best_pic_and_get_nom_in_i / p_of_getting_nom_in_i)

#Let us create a temp datafame:
#The last column will have the c16 category, the best picture category. This way we can iterate among the columns of temp easily, to get cond probs

temp <- data.frame()
temp <- data.frame(awards_re_modified[,3:25])
temp$c0 <- temp$c16
temp$c16 <- NULL
names(temp)[23] <- "c16"
probability_if_nominated <- data.frame(norow=22,ncol=2)

head(temp)
p <- vector(length=22)
category_id <- vector(length=22)
category_name <- vector(length=22)
category_name <- NULL
for(i in 1:22)
{

  x <- temp[,c(i,23)]
  #Get the probability of getting a movie nominated in category i
  #First get the number of movies, and then get the number of movies for which the category_i is 1 or 0
  
  category_nominated_count <- sum(!is.na(x[,1])) #if category_i is 0 or 1, then the movie is nominated
  Exhaustive_movies_count <- nrow(x)
  #Hence the probability of getting a movie nominated is ...
  p_of_getting_nom_in_i <- (category_nominated_count / Exhaustive_movies_count)
  
  
  #Now get the count of movies which won the best movie given that they were nominated in category_i
  p_of_best_pic_and_get_nom_in_i <- (sum(!is.na((x[which(x[,2] == 1),])[1])) / nrow(x))
      
  #Now, the probability of getting best movie given that it is nominated in category_i
  p[i] <- (p_of_best_pic_and_get_nom_in_i / p_of_getting_nom_in_i)
  category_id[i] <- as.numeric(substr(names(temp)[i],2,3))
  category_name[i] <- as.vector(award_categories[award_categories$category_id==(category_id[i]),2])
  
  
}
prob_of_best_pic_given_cat_nom <- data.frame(category_id=category_id,category_name=category_name,prob_percent=(p*100))
prob_of_best_pic_given_cat_nom 




##------------Now probability of getting best picture, given that the film wins in the respective category








for(i in 1:22)
{
  
  x <- temp[,c(i,23)]
  #Get the number of times a movie won in cat_i
  num_of_wins_in_cat_i <- sum(x[which(!is.na(x[,1])),1] == 1)
  
  #Get the number of times a movie got Best movie and also won cat_i
  num_of_best_movies_given_cat_i_win <- sum(x[which(x[,2] == 1),1] == 1,na.rm=T)
  
  #Probability of winning Best pic, given a picture wins in cat_i
  p[i] <- (num_of_best_movies_given_cat_i_win / num_of_wins_in_cat_i)
  
  category_id[i] <- as.numeric(substr(names(temp)[i],2,3))
  category_name[i] <- as.vector(award_categories[award_categories$category_id==(category_id[i]),2])
  
}


prob_of_best_pic_given_a_cat_wins <- data.frame(category_id=category_id,category_name=category_name,prob_percent=(p*100))
prob_of_best_pic_given_a_cat_wins
