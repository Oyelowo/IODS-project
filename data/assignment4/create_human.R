#The datasets below are Human Development Index(hd) and Gender Inequality Index(gii)

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")
setwd("C:/Users/oyeda/Desktop/OPEN_DATA_SCIENCE/IODS-project/data/assignment4")
#column names of the datasets
colnames(hd)
colnames(gii)

#Structure of hd
str(hd)

#dimension
dim(hd)

#summary
summary(hd)

#structure of gii
str(gii)

#dimension
dim(gii)

#summary
summary(gii)


#change the column names into something shorter and descriptive
colnames(hd) <-c("HDIrank", "Country", "HDI", "LifeExpBirth","ExpYrsEdu", "MeanYrsEdu",
                 "GNIperCap", "GNIperCapMinusHDIrank") 

colnames(gii) <- c("GIIrank","Country","GII","MMR","ABR","PRParl", "edu2F","edu2m","labF","labM" )


#create new columns for ratio of female secondary education to male
gii$eduFM <- gii$edu2F/gii$edu2m

#create a a new column for the ratio of the female percentage of labour force participation to male
gii$labFM <- gii$labF / gii$labM

#join the two datasets
human <- inner_join(x=hd, y=gii, by="Country", suffix=c("hd", "gii"))

#save the file
write.table(human, file = "C:/Users/oyeda/Desktop/OPEN_DATA_SCIENCE/IODS-project/data/assignment4/human.txt", sep = ",", col.names = T)
