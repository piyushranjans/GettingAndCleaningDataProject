# Clean up The workspace
rm(list=ls())

# Set the working directory
setwd("/Volumes/Piyush/DataScience/R/getting-cleaning-data")

# Download the data 
library(httr) 
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
File <- "dataset.zip"
if(!file.exists(File)){
  print("Downloading the data file")
  download.file(Url, File, method="curl")
}

# Unzip the data file
datafolder <- "UCI HAR Dataset"
resultsfolder <- "results"

if(!file.exists(datafolder)){
  print("Unzipping the File")
  unzip(File, list = FALSE, overwrite = TRUE)
} 

if(!file.exists(resultsfolder)){
  print("Creating The Results Folder")
  dir.create(resultsfolder)
}

# Function to Read a text file and convert it into a data frame
textfiletodataframe <- function (filename,cols = NULL){
  print(paste("Reading File:", filename))
  f <- paste(datafolder,filename,sep="/")
  data <- data.frame()
  if(is.null(cols)){
    data <- read.table(f,sep="",stringsAsFactors=F)
  } else {
    data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
  }
  data
}

# Function to read the data and build into a database
# typw woul dbe test or train
# feature is a data frame
getdata <- function(type, feature){
  print(paste("Getting data", type))
  subject_data <- textfiletodataframe(paste(type,"/","subject_",type,".txt",sep=""),"id")
  y_data <- textfiletodataframe(paste(type,"/","y_",type,".txt",sep=""),"activity")
  x_data <- textfiletodataframe(paste(type,"/","X_",type,".txt",sep=""),feature$V2)
  return (cbind(subject_data,y_data,x_data))
}


# Call the functions to get the train and test data set from features.txt file
featuredata <- textfiletodataframe("features.txt")
testdata <- getdata("test", featuredata)
traindata <- getdata("train", featuredata)


# Merge the train and test data into one data set
library(plyr)
data <- rbind(traindata, testdata)
data <- arrange(data, id)

# Find Mean and Std Dev for Each Measurement
MeanAndStdDev <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]

# Write the mean and std dev into a text file in results folder
print("Writing File: ./results/MeanStd.txt")
write.table(MeanAndStdDev, './results/MeanStd.txt',row.names=FALSE,sep='\t');


# this uses descriptive activity names to name the activities
# and labels the data set with descriptive variable names
activity_labels <- textfiletodataframe("activity_labels.txt")
data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

# this creates the tidy data set with the average of each variable for each activity and
# subject and writes the result into TidyData.txt file in results folder
TidyDataset <- ddply(MeanAndStdDev, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(TidyDataset)[-c(1:2)] <- paste(colnames(TidyDataset)[-c(1:2)], "_mean", sep="")
print("Writing File: ./results/TidyData.txt")
write.table(TidyDataset, './results/TidyData.txt',row.names=FALSE,sep='\t');

