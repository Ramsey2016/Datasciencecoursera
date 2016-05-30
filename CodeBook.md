# README: Explanation of my project

# PROJECT
# 1 Fist, I perform merging of the  training and the test data sets to create single data set.
# 2 Extracts  measurements on the mean and standard deviation  .
# 3 Uses  desc. activity names to name all activities found in the the data set
# 4 Labels data in dataset 
# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Set the environment variables
library(reshape2)
library(dplyr)
setwd("C:/Users/RemziK/Desktop/Courserastuff/Getting and cleaning data/R session")
filename <- "getdata_dataset.zip"

## Download the file locally
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

# Generate labels from the dataset
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
activityLabels[,2] <- as.character(activityLabels[,2])
features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])

# Produce only mean and standard deviation
myfeatures <- grep(".*mean.*|.*std.*", features[,2])
myfeatures.names <- features[myfeatures,2]
myfeatures.names = gsub('-mean', 'Mean', myfeatures.names)
myfeatures.names = gsub('-std', 'Std', myfeatures.names)
myfeatures.names <- gsub('[-()]', '', myfeatures.names)

# Binding...
train <- read.table("UCI HAR Dataset/train/X_train.txt")[myfeatures]
Activities <- read.table("UCI HAR Dataset/train/Y_train.txt")
Subjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
train <- cbind(Subjects, Activities, train)
test <- read.table("UCI HAR Dataset/test/X_test.txt")[myfeatures]
testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
test <- cbind(testSubjects, testActivities, test)

# Unite labels and data
myData <- rbind(train, test)
colnames(myData) <- c("subject", "activity", myfeatures.names)

#  eGenerat levels by turning aubjects and activities
myData$activity <- factor(myData$activity, levels = activityLabels[,1], labels = activityLabels[,2])
myData$subject <- as.factor(myData$subject)

 
combinedFinal <- melt(myData, id = c("subject", "activity"))
myData.mean <- dcast(combinedFinal, subject + activity ~ variable, mean)

# Produce clean output
write.table(myData.mean, "tidyData.txt", row.names = FALSE, quote = FALSE)

