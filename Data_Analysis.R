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
#> awards_modified[2720:2721,]

#movie_id year category_id won row_num 
#2720     1536 1984           1   0     
#2721     1536 1984           1   1     

#We have to group by movie_id, year, category_id, and max(won)
#such group by will include just the winning nomination, if a film is nominated multiple times in the same category, and one of the nominations win in that category.

awards_modified <- awards_modified %>%
  group_by(movie_id,year,category_id) %>%
  summarise(won=max(won))

#The above transformation has eliminated all the duplicate rows, where the same movie has been nominated in the same category more than once and one of them wins.
#It just includes the winning nomination. If none of the multiple nominations win, then only one of the nominations are included.
#The below command confirms that for the movie_id=1536, only the winning nomination is inluded
awards_modified[awards_modified$movie_id==1536,]


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



dim(awards_re_modified)
#table(awards_re_modified[awards_re_modified$c16 == 0 &!is.na(awards_re_modified$c16),][,c("c12","c16")])

prop.table(table(awards_re_modified[,c("c12","c16")],useNA=c("no")))
prop.table(table(awards_re_modified[,c("c12")],useNA=c("always")))
prop.table(table(awards_re_modified[,c("c1","c16")],useNA=c("no")))


head(awards_re_modified,150)


awards_re_modified[awards_re_modified$movie_id == 3,]

awards_modified[awards_modified$movie_id == 3,]
awards_modified

awards_re_modified[awards_re_modified$year==2001,]


#Applying Bayes theorem:
#To find the probability of getting best pucture, given that it is nominated for editing
#P(BM | EN)_current = P(EN|BM)_old . P(BM)_current/P(EN)_old

#P(BM)_current = 1/No. of Best movie nominations in current year
#P(EN|BM) _old = get the no. of times a movie was nominated for editing, given that it obtained best movie in the history/No. of movies nominated for editing
#P(EN)_old = #of movies nominated for editing in the history/total no. of movies in the hist.
#P(EN)_old = sum((!is.na(awards_re_modified$c16[awards_re_modified$year < 2010])))/sum(awards_re_modified$year < 2010)


#To make the analysis simple, let us collect the variables c12 and c16 to a separate data frame, along with the year.
#We will use the Bayes theorem to find the probability of a film winning the best picture, given that it is nominated for editing.
temp <- awards_re_modified[,c("year", "c12","c16")]

#Let us pretend that we had data till the year 2009, and we also know which films are nominated for editing in 2010, and also which films are nominated for best pic


#P(c16|c12) = p(c12|c16).p(c16)/p(c12)

#Let x = P(c16|c12)
#a = p(c12|c16)
#b = p(c16)
#c = p(c12)

#p(c12) = No. of films nominated for editing in the past (<=2009)/no. of all movies in the past (<=2009)
p_c12 <- sum(!is.na(temp$c12[temp$year <= 2009]) )/nrow(temp[temp$year<=2009,])


#p(c16) - No. of films nominated for best picture in the current year/No. of pictures in the current year
p_c16 <- sum(!is.na(temp$c16[temp$year == 2010]) ) / nrow(temp[temp$year==2010,])

#p(c12|c16) - No. of films, which won the best pic and also was nominated for editing in the past / No. of films nominated for edit in the past
p_c12_given_c16 <- 