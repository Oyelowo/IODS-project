
# Read the students2014 data into R either from your local folder 
# (if you completed the Data wrangling part) or from this url: 
# http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/learning2014.txt. 
# (The separator is a comma "," and the file includes a header). Explore the 
# structure and the dimensions of the data and describe the dataset briefly, 
# assuming the reader has no previous knowledge of it. There is information 
# related to the data here. (0-2 points)

lrn14<- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/learning2014.txt ",
                   header=T, sep = ",")
# lrn14<- read.table("C:/Users/oyeda/Desktop/OPEN_DATA_SCIENCE/IODS-project/data/learning2014.txt ",
#                    header=T, sep = "\t")
# colnames(lrn14)[4:6]<-c("age","attitude","points")

#structure of the data
str(lrn14)

#dimension of the data
dim(lrn14)

#The data includes 7 variables which describes several attributes of students:
# Age      Age (in years) derived from the date of birth
# Attitude Global attitude toward statistics
# Points   Exam points
# gender   Gender: M (Male), F (Female)
# Deep Deep Approach 
# Surf Surface approach 
# Stra Strategic approach


library(ggplot2)
# Show a graphical overview of the data and show summaries of the 
# variables in the data. Describe and interpret the outputs, commenting 
# on the distributions of the variables and the relationships between them.
# (0-3 points)

#install.packages("GGally")
library(GGally)
ggpairs(lrn14, aes(col=gender), lower = list(combo = wrap("facethist", bins = 20)))
summary(lrn14)
#By and large, we have almost twice as much female responsdent out of 166 students
#aside those with examination point 0. The average age is about 26 years with
#the youngest being 17 years and an old student of age 55. The ages of the 
#respondents are mainly skewed to the right, showing they are mostly young students
#below 26 years. We can also see from the box plot that the age the age is non-normal
#and skewed to the right with some outliers. The attitude, points and deep learning approach
#also have few outliers. Generally, the pairs show weak correaltions. However,
#attitude seems to be the most strongly correlated(although still weak) with points, for b
#male and female. Men generally seem to have a slightly better attitude than their
#female counterpart. although, some male students show very poor attitude towards learning.
#Both genders also appear to have average to very deep learning approach.
#Female students have a slightly better strategic approach. However, there appears to 
#be not much distinctions between examinations points of male and female students.
#Most students seem to have more above average score while a few students showed very
#poor performance.


#Choose three variables as explanatory variables and fit a regression model
#where exam points is the target (dependent) variable. Show a summary of 
#the fitted model and comment and interpret the results. Explain and interpret
#the statistical test related to the model parameters. If an explanatory 
#variable in your model does not have a statistically significant relationship
#with the target variable, remove the variable from the model and fit the model
#again without it. (0-4 points)

#I attempted to use all the variables for the model and stepwise regression
#to help remove the redundant variables.
l_model<-lm(points~gender+age+attitude+deep+stra+surf, data = lrn14)
summary(l_model)
library(MASS)
mm<-stepAIC(l_model, direction = "both")

#After using stepwise regression, I ended up with three explanatory variables namely:
#'age','attitude' and 'stra'

#now, let's refit the model with these three variables
l_model2<-lm(points ~ age + attitude + stra, data = lrn14)
summary(l_model2)

#As shown by the model, the p value of 'attitude' is very low below 0.05 and with
#higher confidence that 'attitude' significantly affects the examination points.
#However, the coeffient of determination(Multiple R-squared) of the model is just (0.2182)21.82% which is 
#relatively low. The standard error of age is quite low and p value is not 
#so high either. Hence, this can be incorporated into the model.
#However, even though, the standard error of strategic approach is relatively high with 0.53
#compared with age with 0.05, the p-value is lower. Based on this, I will remove the age because it is less significant.
#Finally, I am left with predictors; 'attitude' and 'strategic approach'.
#age seems to have a slightly negative effect while attitude and strategic approach has a positive effect.
#Just as points and age showed a low negative correlation earlier, it can be seen that
#age has a slight negative effect(-0.08821) on the examination poitns while attitude, which
#showed slightly higher positive correlation with points earlier, has  a positive 
#effect(3.48143) on the examination points

#Final model
lm_final<-lm(points~stra+attitude, data = lrn14)
summary(lm_final)

#Although, the coefficient of determination(Multiple R-Squared) reduced slightly to 20.48% from 21.82%
#and the standard error only increased from 5.26 to 5.289. The difference difference
#is not so drastic. The p-value is also well below 0.05.
#with the 'ättitude' and 'strategeic approach', I got a higher Multiple R-squared of 0.2048 compared to
#0.2011 when I used age and attitude. Thus, I decided to use 'attitude' and 'strategic approach'.

par(mfrow=c(2,2))
plot(lm_final, which = c(1,2,5))

#As seen in the Normal Q-Q plot, the points fit well to the line, aside few deviations
#at the beginning and end. The errors can thus, be said to be normally distributed.
#for the residuals vs fitted, it can be seen that there is a slight change as the values inceas.
#However, no observation seems to have clear leverage over others.
