rm(list = ls())

# Access GGally
library(GGally)
library(corrplot)
library(tidyr)
library(ggplot2)
library(FactoMineR)

#install.packages("FactoMineR")




# Load the 'human' data into R. Explore the structure and the dimensions
#of the data and describe the dataset briefly, assuming the reader has no '
#previous knowledge of it (this is now close to the reality, since you 
#have named the variables yourself). (0-1 point)
#the file path of the data
fpath<- "C:/Users/oyeda/Desktop/OPEN_DATA_SCIENCE/IODS-project/data/assignment5/human.txt"

#load the data
human<- read.table(fpath, sep=",", header = T)

dim(human)  #the dimension of the data
str(human)    #the structure of the data
#Description of the data set
# data originally from: http://hdr.undp.org/en/content/human-development-index-hdi
# Modified and analysed by Oyedayo Oyelowo 2017

#The data used in this analysis consists of various development indicators in
#many countries across the globe. 
#the data consists of 8 variables and 155 observations. All the variables 
#are numerical(all being float exept the Gross National Income per capita and
#Maternal mortality ratio which are integer).

# #Below are the column names and their full meanings
# "Country" = Country name
# 
# # Health and knowledge
# 
# "GNI" = Gross National Income per capita
# "Life.Exp" = Life expectancy at birth
# "Edu.Exp" = Expected years of schooling 
# "Mat.Mor" = Maternal mortality ratio
# "Ado.Birth" = Adolescent birth rate
# 
# # Empowerment
# 
# "Parli.F" = Percetange of female representatives in parliament
# "Edu2.F" = Proportion of females with at least secondary education
# "Edu2.M" = Proportion of males with at least secondary education
# "Labo.F" = Proportion of females in the labour force
# "Labo.M" " Proportion of males in the labour force
# 
# "Edu2.FM" = Edu2.F / Edu2.M
# "Labo.FM" = Labo2.F / Labo2.M



# Show a graphical overview of the data and show summaries of the variables
#in the data. Describe and interpret the outputs, commenting on the 
#distributions of the variables and the relationships between them. (0-2 points)
# visualize the 'human_' variables
ggpairs(human)

# compute the correlation matrix and visualize it with corrplot
cor(human)%>%corrplot()

summary(human)

#Here, we can see that life expectancy at birth and expected years of 
#schooling have slightly high positive correlation. Maternal mortality ratio has
#and life expectancy at birth have strong negative correlation. 
#Also, maternal mortality ratio has a quite high correlation of 0.76 with 
#adolescent birth rate. The above graph gives more detail.
#Maternal mortality rate, although, generally on the low side, is very high
#in some countries, with the maximum of 11000 which is much more than the 
#average of 149.1. The summary and graph above show more informatin
#about the distribution of this and other variables




# Perform principal component analysis (PCA) on the not standardized 
# human data. Show the variability captured by the principal components. 
# Draw a biplot displaying the observations by the first two principal 
# components (PC1 coordinate in x-axis, PC2 coordinate in y-axis), along 
# with arrows representing the original variables. (0-2 points)

###########################################################################
#Some information from the IODS datacamp exercise for reference purpose:
# PCA with R
# Principal Component Analysis (PCA) can be performed by two sightly 
#different matrix decomposition methods from linear algebra: 
#the Eigenvalue Decomposition and the Singular Value Decomposition (SVD).
# There are two functions in the default package distribution of R that 
#can be used to perform PCA: princomp() and prcomp(). The prcomp() function 
#uses the SVD and is the preferred, more numerically accurate method
# Both methods quite literally decompose a data matrix into a product of 
#smaller matrices, which let's us extract the underlying principal 
#components. This makes it possible to approximate a lower dimensional 
#representation of the data by choosing only a few principal components.
############################################################################

#perform principal component analysis on the unstandardised data
pca_unstd_human<- prcomp(human)

#save the summary of the analysis into an object
s_hum<-summary(pca_unstd_human)

#print the summary
s_hum

#get the proportion of variance which is in the second row and make 
#it in percentage
pca_prop<- round(100*s_hum$importance[2,], digits = 3)
pca_prop


# create object pc_lab to be used as axis labels
pc_label<-paste0(names(pca_prop), " (", pca_prop, "%)")

# draw a biplot
biplot(pca_unstd_human, cex = c(0.8, 1), col = c("grey40", "red"), xlab = pc_label[1], ylab = pc_label[2])
#deeppink2


# Standardize the variables in the human data and repeat the above 
# analysis. Interpret the results of both analysis (with and without 
# standardizing). Are the results different? Why or why not? Include 
# captions in you plots where you describe the results by using not 
# just your variable names, but the actual phenomenons they relate to. 
# (0-4 points)
#scale the data "human"
human_sca<- scale(human)

#check the summary to see that the variables have all been scaled
summary(human_sca)

#perform a principal component analysis (SVD) on the scaled data
pca_human_sca<- prcomp(human_sca)

s_hum2<-summary(pca_human_sca)
s_hum2

#get the proportion of variance which is in the second row and make it in percentage
pca_prop2<- round(100*s_hum2$importance[2,], digits = 3)
pca_prop2


# create object pc_lab to be used as axis labels
pc_label2<-paste0(names(pca_prop2), " (", pca_prop2, "%)")

par(mfrow=c(2,1))
# draw a biplot
biplot(pca_human_sca, cex = c(0.7, 0.7), col = c("grey", "red"), xlab = pc_label2[1], ylab = pc_label2[2])

biplot(pca_unstd_human, cex = c(0.8, 1), col = c("grey40", "red"), xlab = pc_label[1], ylab = pc_label[2])


#We can see that the unstandardised data differ from standardised. The GNI 
#in the unstandardised has more leverage because it has huge values
#and had not been scaled to be on the same level with other variables.
#However, this was addressed in the scale data before the analysis was performed
#Here, Proportion of females in the labour force and Percetange of female representatives in parliament
#are almost orthogonal(at right angle) and less related to Maternal mortality ratio,
#Adolescent birth rate, Expected years of schooling and Life expectancy at birth.

#Also, while, Percetange of female representatives in parliament and Proportion 
#of males in the labour force mostly contributed to the second principal component
#others contributed to the first, as shown in the biplot.





# Give your personal interpretations of the first two principal component
# dimensions based on the biplot drawn after PCA on the standardized human 
# data. (0-2 points)
####!!!!!!!!Give interpretation?


# Load the tea dataset from the package Factominer. Explore the data 
# briefly: look at the structure and the dimensions of the data and visualize 
# it. Then do Multiple Correspondence Analysis on the tea data (or to a 
# certain columns of the data, it's up to you). Interpret the results of 
# the MCA and draw at least the variable biplot of the analysis. You can 
# also explore other plotting options for MCA. Comment on the output of the 
# plots. (0-4 points)
# 
library(Factominer)
data(tea)     #load the tea dataset
str(tea)
dim(tea)

# column names to keep in the dataset
colnames(tea)
keep_columns <- c("Tea", "How", "how", "sugar", "where","lunch", "dinner", "always")


# select the 'keep_columns' to create a new dataset
tea_time <- tea[, keep_columns]
#tea_time <- select(tea, keep_columns)

# look at the summaries and structure of the data
summary(tea_time)
str(tea_time)


# visualize the dataset
gather(tea_time) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar() 



# multiple correspondence analysis
mca <- MCA(tea_time, graph = T)

# table of eigenvalues
mca$eig


# summary of the model
summary(mca)
dimdesc(mca)

# visualize MCA
plot(mca, invisible=c("ind"), habillage="quali")

#The below is incase I want to see the observations that contribute the most
#For instance, the below shows the 3 observations that contribute the most.
plot(mca, invisible=c("ind"), habillage="quali", selectMod = "contrib 3")
mca
#plot.MCA(mca)
#The above plot looks a bit flavorless. 
#Hence, I'll create a more interesting plot using ggplot2.
#other packages can be used. [See](http://www.gastonsanchez.com/visually-enforced/how-to/2012/10/13/MCA-in-R/)


# number of categories per variable
categ = apply(tea_time, 2, function(x) nlevels(as.factor(x)))

categ
rep("c", 3)
# data frame with variable coordinates
mca_vars_df = data.frame(mca$var$coord, Variable = rep(names(categ), categ))
mca_vars_df

# data frame with observation coordinates
mca_obs_df = data.frame(mca$ind$coord)


# to spice the graph up,  I have decided to overlay the the categories on
#the observations. I also used geom_density2d() to show the density curves
#of the areas where individuals might be overlapping. This  is like an isoline
#which shows the areas that are highly concentrated

# MCA plot of observations and categories
ggplot(data = mca_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca_vars_df), colour = Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR") +
  scale_colour_discrete(name = "Variable")

