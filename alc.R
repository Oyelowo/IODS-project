"Name: OYEDAYO OYELOWO"
"sTUDENT'S NUMBER: 014717208"

# Data Set Information:
#data from: http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/alc.txt 
#   

# access the tidyverse libraries tidyr, dplyr, ggplot2
#install.packages("tidyr")
library(tidyr); library(dplyr); library(ggplot2); library(GGally)

alc<- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/alc.txt",
                 sep = ",", header = T)


# glimpse at the alc data
colnames(alc)
summary(alc)
dim(alc)
str(alc)

#Describe the data briefly here.
#The data includes students' secondary education accomplishment of two portuguese schools.
# The data attributes include student grades, demographic, social and school related features') 
# a'nd it was collected by using school reports and questionnaires. Two datasets are 
# provided regarding the performance in two distinct subjects: Mathematics (mat) and 
# Portuguese language (por). In [Cortez and Silva, 2008], the two datasets were modeled 
# under binary/five-level classification and regression tasks. Important note: the target 
# attribute G3 has a strong correlation with attributes G2 and G1. This occurs because G3 
# is the final year grade (issued at the 3rd period), while G1 and G2 correspond to the 
# 1st and 2nd period grades. It is more difficult to predict G3 without G2 and G1, but 
# such prediction is much more useful (see paper source for more details).
#source: https://archive.ics.uci.edu/ml/datasets/Student+Performance


attach(alc)   #attach all the variables in the dataframe
#####
#hypotheses.
#age is not related to alcohol consumption
#sex is not related to alcohol consumption
#absence from school is not related to alcóhol use
#freetime after school is not related to alcohol consumption


# define a new logical column 'high_use'
# alc$high_use<- alc$alc_use > 2
# alc <- mutate(alc, high_use = alc_use > 2)

#subsetting my chosen variables
hyp<- alc[,c("age", "sex",  "absences","freetime","alc_use")]

#exploring my chosen variables
# draw a bar plot of each variable
hyp<-gather(hyp) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free")
hyp + geom_bar()
#The above shows the distribution of the chosen predictors and also scale of alcohol
#consumption. The ages of the students are mainly within 15-19 with few students aged 20 and 22.
#Generally, there are relatively few chronic alcohol consumers amongst the students.
#The students seem to have quite enough free time. The respondents include 198 female students
#and 184 male students.


#show the overview of the predictors and response variable(non-binomial)
plot_hyp <- ggpairs(hyp, mapping = aes(col=sex, alpha=0.3), lower = list(combo = wrap("facethist", bins = 20)))
plot_hyp   #draw the plot

#Here, we can see that the female students seem to be generally quite older and there
#are both male and female students that are much older than the general sample of the
#students.  The absences are not so high but some chronic absentees amongst the
#the students(both male and female). Most of the students are quite free but a few of the
#students are very busy.
#Largely, all the predictors show no correlation, hence, the issue of multicolinearity
#is not a problem here. However, these predictors still show very weak correlation 
#with alcohol consumption. 
#Nevertheless, it should be recalled that correlation is not causation. Hence, I will
#dig further to understand the effects of these predictors on alcohol consumption
#amongst students and see if they are significant.


#The below are crosstabs that compare the predictors with alcohol consumtion
#using the binomial response(high_use)

#it is also possible to use simple crosstabs e.g
#xtabs(high_use~age, data = alc)
#but below is a more comprehensive crosstab.
#Crosstabs
summarise(group_by(alc, age,high_use), count=n(), mean_grade=mean(G3))
#an alternative and preferrable way of doing the above
#alc %>% group_by(age, high_use) %>% summarise(count=n(), mean_grade =  mean(G3))

summarise(group_by(alc, sex,high_use), count=n(), mean_grade=mean(G3))
summarise(group_by(alc, absences,high_use), count=n(), mean_grade=mean(G3))
summarise(group_by(alc, freetime,high_use), count=n(), mean_grade=mean(G3))


############
#The age distribution by sex
# initialize a plot of 'age'
sex_age <- ggplot(data = alc, aes(x=age, col=sex))

# draw a bar plot of age by sex
sex_age + geom_bar() + facet_wrap("sex")



#high alcohol consumption by sex
# initialize a plot of 'high_use'
hu_sex <- ggplot(data = alc, aes(x=sex, col=sex))

# draw a bar plot of high_use by sex
hu_sex + geom_bar() + facet_wrap("high_use")
#Here, we can see that there are more female who do are not high consumers of alcohol
#compared to men. Men consume more alchols than their female counterparts.



#######################################3
#High use vs freetime
# initialize a plot of 'high_use'
hu_ft <- ggplot(data = alc, aes(x=freetime, col=sex))

# draw a bar plot of 'high_use' by freetime
hu_ft + geom_bar() + facet_wrap("high_use")
#Most students with more freetime seem not to consume alcohol as one would expect

#######################################
#High use vs absences
# initialize a plot of 'high_use'
hu_ab <- ggplot(data = alc, aes(x=absences, col=sex))

# draw a bar plot of 'high_use' by freetime
hu_ab + geom_bar() + facet_wrap("high_use")
#few of the absentees are alcoholic but many are not.

#######################################
#High use vs age
# initialize a plot of 'high_use'
hu_ag <- ggplot(data = alc, aes(x=age, col=sex))

# draw a bar plot of 'high_use' by freetime
hu_ag + geom_bar() + facet_wrap("high_use")
#some of the young students below 20 years are high consumers of alcohol,
#but less so of those that are not high alchol consumers.

#######################################
#High use vs romantic
# initialize a plot of 'high_use'
hu_rom <- ggplot(data = alc, aes(x=romantic, col=sex))

# draw a bar plot of 'high_use' by freetime
hu_rom + geom_bar() + facet_wrap("high_use")
#For students in romantic relationships, they generally have lower number of them that
#are high consumers of alcohol. This is same with those in no romatic relationships.

############################
############################

#Below are boxplots to give clearer pictures, as explained earlier.
#relationship of alcohol consumption with absences
# initialise a plot of high_use and absences
h_ab<- ggplot(alc, aes(x=high_use, y=absences,col=sex))

# define the plot as a boxplot and draw it
h_ab + geom_boxplot() + ggtitle("Student absences vs alcohol consumption")
#there are a few chronic absentees amongst male and female students, but 
#generally, most of the students have low absences.

#############################
#relationship of alcohol consumption with age
# initialise a plot of high_use and age
h_ag<- ggplot(alc, aes(x=high_use, y=age,col=sex))

# define the plot as a boxplot and draw it
h_ag + geom_boxplot() + ggtitle("Students' age vs alcohol consumption")


#############################
#relationship of alcohol consumption with freetime
# initialise a plot of high_use and freetime
h_fr<- ggplot(alc, aes(x=high_use, y=freetime,col=sex))

# define the plot as a boxplot and draw it
h_fr + geom_boxplot() + ggtitle("Student's freetime vs alcohol consumption")



#############################
#relationship of alcohol consumption with sex
# initialise a plot of high_use and sex
alc_sex<- ggplot(alc, aes(y=alc_use, x=sex,col=sex))

# define the plot as a boxplot and draw it
alc_sex + geom_boxplot() + ggtitle("Student's alcohol consumption by sex")
#There are a few female students with quite high alcohol consumption.'
#but the alcohol consumption levels do not vary as much as they do amongst
#the male students.



###########################################################################
#Logistic regression

#fitting the glm model
high_use_mod1<- glm(high_use~ age + sex + absences + freetime , data= alc, family = "binomial")
summary(high_use_mod1)


# coef(high_use_mod1)
# coefficients(high_use_mod1)
#The predictors "age" and "freetime "are not significant. Hence, these could be 
#considered as redundant variables and not have consierable effect on alcohol 
#consumption compared with asences and sex which are significant.
#hence, I could accept the null hypothesis that age and freetime are not 
#related to alcohol consumption.
#On the other hand, I reject the null hypothesis that male sex and absences are not 
#related to alcohol consumption. They both have positive effects on alcohol consumption.

high_use_mod2<- glm(high_use~  sex + absences, data= alc, family = "binomial")
summary(high_use_mod2)
#options("contrasts")


#check the overall effect of the variable by performing a likelihood ratio test
anova(high_use_mod1, high_use_mod2, test="LRT")
#there seems to be no significant difference between the two models.  
#Hence, 'sex' and 'absences' are significant enough without the redundant variables.


#let's also see if "sex" is alos significant in the prediction
high_use_mod3<- glm(high_use~  absences, data= alc, family = "binomial")
anova(high_use_mod2, high_use_mod3, test="LRT")
#The test shows that sex is important and would remain in the model.

#Hence, final model is: high_use~  sex + absences

#calculating the odds ratio
# compute odds ratios (OR)
odds_ra <- exp(coef(high_use_mod2))
#odds_ra <- coef(high_use_mod2) %>% exp     #alternaive

# compute confidence intervals (conf_int)
conf_int <- exp(confint(high_use_mod2)) 
#Conf_Int <- high_use_mod2 %>%  confint() %>% exp   #alternative

# print out the odds ratios with their confidence intervals
cbind(odds_ra, conf_int)
#Here, we can see that Male sex and absences are positively associated with
#success and are more likely to affect alcohol consumption.
#the confident interval of Male sex is quite high compared to "absences"
#which is narrower and much more likely to affect alcohol consumption.


#Using the variables which, according to your logistic regression model, 
#had a statistical relationship with high/low alcohol consumption, explore
#the predictive power of you model. Provide a 2x2 cross tabulation of 
#predictions versus the actual values and optionally display a graphic 
#visualizing both the actual values and the predictions. Compute the 
#total proportion of inaccurately classified individuals (= the training error)
#and comment on all the results. Compare the performance of the model with 
#performance achieved by some simple guessing strategy. (0-3 points)

# fit the model


# predict() the probability of high_use
probs<- predict(high_use_mod2, type = "response")

# add the predicted probabilities to 'alc'
alc$prob_high_use <- probs
#alc <- mutate(alc, probability = probabilities)

# use the probabilities to make a prediction of high_use, setting 0.5 as threshold
alc$predict_high_use<- (alc$prob_high_use)>0.5
#alc <- mutate(alc, prediction = prob_high_use>0.5)

# see the first ten and last ten original classes, predicted probabilities, and class predictions
head(alc[,c("failures", "absences", "sex", "high_use", "prob_high_use", "predict_high_use")], n=10)
select(alc, failures, absences, sex, high_use, prob_high_use, predict_high_use) %>% tail(10)


# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$predict_high_use)
#The model rightly predicted 263 False high use and 23 True high_use of alcohol.
#It wrongly predicted 89 True high_use and 7 False high_use

####################################################################

# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = prob_high_use, y = high_use, col= predict_high_use))

# define the geom as points and draw the plot
g + geom_point()
#The wrong preditctions were quite much

# tabulate the target variable versus the predictions
conf_mat<-table(high_use = alc$high_use, prediction = alc$predict_high_use)
conf_mat<-prop.table(conf_mat)
addmargins(conf_mat)

#Alternatively, this can be done as shown below:
#addmargins(prop.table(table(high_use = alc$high_use, prediction = alc$predict_high_use)))
#table(high_use = alc$high_use, prediction = alc$predict_high_use) %>%  prop.table() %>% addmargins()


#mean error of the prediction
mean(abs(alc$high_use-alc$prob_high_use)>0.5)
#My model has slightly lower error  of 0.25 compared with the one on datacamp with 0.26 mean error.

#The below is an alternative way by firstly defining the function to calculate the mean error.
# define a loss function (mean prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}
# call loss_func to compute the average number of wrong predictions in the (training) data
loss_func(class = alc$high_use, prob = alc$prob_high_use)



# K-fold cross-validation
library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = high_use_mod2, K = 10)

# average number of wrong predictions in the cross validation
cv$delta[1]
