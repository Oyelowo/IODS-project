#CREATED BY: OYEDAYO OYELOWO
#DATE: 11/11/2017
#DESCRIPTION: WRANGLING OF DATA
#Data source: http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-meta.txt

#The script was produced to wrangle the data for further analysis.

#read file
data<-read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt",
                 sep = "\t", header = T)
#check the dimension of the data
dim(data)

#structure of the data
str(data)

#The below are the excerpts from the data page and shows information about 
#the aggregated variables. See the above data page for more information
# d_sm Seeking Meaning ~ D03 + D11 + D19 + D27
# d_ri Relating Ideas ~ D07 + D14 + D22 + D30
# d_ue Use of Evidence ~ D06 + D15 + D23 + D31
# su_lp Lack of Purpose ~ SU02 + SU10 + SU18 + SU26
# su_um Unrelated Memorizing ~ SU05 + SU13 + SU21 + SU29
# su_sb Syllabus-boundness ~ SU08 + SU16 + SU24 + SU32
# st_os Organized Studying ~ ST01 + ST09 + ST17 + ST25
# st_tm Time Management ~ ST04 + ST12 + ST20 + ST28
# Deep Deep Approach ~ d_sm + d_ri + d_ue
# Surf Surface approach ~ su_lp + su_um + su_sb
# Stra Strategic approach ~ st_os + st_tm
# Deep_adj Deep_adjusted ~ Deep / 12
# Surf_adj Surface_adjusted ~ Surf / 12
# Stra_adj Strategic_adjusted ~ Stra / 8
#Attitude Global attitude towards statistics ~ Da + Db + Dc + Dd + De + Df + Dg + Dh + Di + Dj


deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

#load package
library(dplyr)

#create an empty dataframe with same number of rows as the original data,
#where the variables will be calculated into.
learn14<-data.frame(matrix(nrow=nrow(data),ncol=0))


###########select columns for variables
#this is one way the selection can be done
deep_cols<-select(data, one_of(deep_questions))
#the selection can also be done as below:
deep_cols<-data[, deep_questions]

#create new column 'deep' where the questions are aggregated and round off to 2
learn14$deep <- round(rowMeans(deep_cols),2)

#create new column for surface questions
surf_q<- select(data, one_of(surface_questions))

#insert the mean of rows into the newly created dataframe
learn14$surf <- round(rowMeans(surf_q), 2)

#select rows with surface question
str_q<-data[, strategic_questions]
#insert the aggregaten into the new dataframe
learn14$stra<-round(rowMeans(str_q),2)

#include other attributes from the data inro the new dataframe
others<-c('Age', 'Attitude', 'Points', 'gender' )
#combine the new attribute with the dataframe
learn14<- cbind(learn14,data[,others])

#scale the attitude by dividing by the number of quesions
learn14$Attitude<-(learn14$Attitude)/10

#exclude points with 0
learn14<- learn14[learn14$Points!=0,]
#This can also be done by:
#learn14<- filter(learn14, Points!=0)

#change the column names to make all the variables start with small letters(not necessary)
colnames(learn14)[4:6]<-c("age","attitude","points")
head(learn14)

#Change the columns names to start with small letters
colnames(lrn14)[4:6]<-c("age","attitude","points")

#set working directory
setwd("C:/Users/oyeda/Desktop/OPEN_DATA_SCIENCE/IODS-project/data")

#check that the working directory has been set properly
getwd()

#export the data
write.table(learn14, file="learning2014.txt", sep = '\t')

#Check the exported data
lrn14<-read.table("C:/Users/oyeda/Desktop/OPEN_DATA_SCIENCE/IODS-project/data/learning2014.txt", sep = "\t")
str(lrn14) #Structure of the data
dim(lrn14)  #dimension of the data
head(lrn14, n=4)  #check the first four rows of the data
