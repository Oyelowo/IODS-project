#NAME:  OYEDAYO OYELOWO
#DESCRIPTION: DATA WRANGLING FOR HUMAN DEVELOPMENT INDICES
#LINK TO DATA: http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt
library(stringr)
library(dplyr)
rm(list = ls())

link<-"http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt"
human<-read.table(link, sep = ",", header = T)

# Mutate the data: transform the Gross National Income (GNI) variable to 
# numeric (Using string manipulation. Note that the mutation of 'human' 
# was not done on DataCamp). (1 point)
human$GNI<-str_replace(human$GNI, pattern = ",", replacement = "")%>%as.numeric()


# Exclude unneeded variables: keep only the columns matching the following 
# variable names (described in the meta file above):  "Country", "Edu2.FM", 
# "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F" 
# (1 point)
keep_cols <- c("Country", "Edu2.FM","Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F") 
human<- dplyr::select(human, one_of(keep_cols))
#human<- human[, keep_cols]  #alternative


# Remove all rows with missing values (1 point).
human <- filter(human, complete.cases(human))
#human<-data.frame(human, complete.cases(human))



# Remove the observations which relate to regions instead of countries. (1 point)
tail(human, 8)
human<-head(human, nrow(human)-7)
#human<- human[1:(nrow(human)-7), ]  #alternative

# Define the row names of the data by the country names and remove the 
# country name column from the data. The data should now have 155 observations 
# and 8 variables. Save the human data in your data folder including the 
# row names. You can overwrite your old 'human' data. (1 point)
#make the country names the index/row names
rownames(human)<-human$Country

#remove the country column
human_ <- select(human, -Country)

#alternative
country_col<- grep("Country", colnames(human)) 
human<- human[,-country_col]

colnames(human)

fpath<- "C:/Users/oyeda/Desktop/OPEN_DATA_SCIENCE/IODS-project/data/assignment5/"
write.table(human, file = paste0(fpath, "human.txt"), sep = ",")
              