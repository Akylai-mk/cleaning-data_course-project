##Programming assignment, Getting and Cleaning Data Course

## 1. Merge the training and the test sets to create one data set.
## 1.1. download all files
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./getdata_projectfiles_UCI_Dataset.zip")

## 1.2. reading test files
Xtest <- read.table(unz("getdata_projectfiles_UCI_Dataset.zip", "./UCI HAR Dataset/test/X_test.txt"))
Ytest <- read.table(unz("getdata_projectfiles_UCI_Dataset.zip", "./UCI HAR Dataset/test/Y_test.txt"))

## 1.3. reading train files
Xtrain <- read.table(unz("getdata_projectfiles_UCI_Dataset.zip", "./UCI HAR Dataset/train/X_train.txt"))
Ytrain <- read.table(unz("getdata_projectfiles_UCI_Dataset.zip", "./UCI HAR Dataset/train/Y_train.txt"))

## 1.4. reading subject files
subjectTrain <- read.table(unz("getdata_projectfiles_UCI_Dataset.zip", "./UCI HAR Dataset/train/subject_train.txt"))
subjectTest <- read.table(unz("getdata_projectfiles_UCI_Dataset.zip", "./UCI HAR Dataset/test/subject_test.txt"))

## 1.5. reading features file
features <- read.table(unz("getdata_projectfiles_UCI_Dataset.zip", "./UCI HAR Dataset/features.txt"))

## 1.6. read activity label file
activityLabels <- read.table(unz("getdata_projectfiles_UCI_Dataset.zip", "./UCI HAR Dataset/activity_labels"))

## 1.7. naming all columnts properly
colnames(Xtest) <- features[,2]
colnames(Xtrain) <- features[,2]
colnames(Ytest) <- "activity"
colnames(Ytrain) <- "activity"
colnames(subjectTest) <- "ID"
colnames(subjectTrain) <- "ID"

##1.8. merge test and train for sets (X files)
train <- cbind(Xtrain, subjectTrain, Ytrain)
test <- cbind(Xtest, subjectTest, Ytest)
dataX <- rbind (test, train)

## 2. extracting mean and standard deviations for each measurement
## 2.1. TRUE/FALSE for variables if they measure mean or standard deviation 
featuredMeasures <- grepl("mean", names(dataX)) | grepl("std", names(dataX)) | grepl ("ID", names(dataX))
## 2.2. subsetting only columns for mean and standard deviation (they are TRUE)
setforFeaturedMeasures <- dataX[ , featuredMeasures == TRUE]

##3. Use descriptive activity names to name the activities in the data set
setWithActivityNames <- merge(setforFeaturedMeasures, activityLabels, 
                              by.setforFeaturedMeasures = "activity", 
                              by.activityLabels = "V1")

##4. Appropriately labels the data set with descriptive variable names
## Already done in 1.7

## 5. From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.
# 5.1 Making a tidy data set
library(dplyr)
tidySet <- setWithActivityNames %>% group_by(V2, ID) %>% summarize_each(mean)
tidySet <- tidySet[order(tidySet$ID, tidySet$V1), ]

# 5.2 Writing a tidy data set into a txt file
write.table(tidySet, "tidySet.txt", row.names = FALSE)
