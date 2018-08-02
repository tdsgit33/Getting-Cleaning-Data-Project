## Getting and Cleaning Data Course Project
## 1.	Merge the training and the test sets to create one data set.
## 2.	Extract only the measurements on the mean and standard deviation for each measurement.
## 3.	Use descriptive activity names to name the activities in the data set
## 4.	Appropriately label the data set with descriptive variable names.
## 5.	From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject. 
##


buildtidy <- function() {
  
  # Download the zip data file for this project
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl, destfile = "w4data.zip", mode="wb")
  unzip("w4data.zip")
  
  # Extract the needed files
  trainset <- read.table("./UCI HAR Dataset/train/X_train.txt")
  testset <- read.table("./UCI HAR Dataset/test/X_test.txt")
  trainlbl <- read.table("./UCI HAR Dataset/train/y_train.txt")
  testlbl <- read.table("./UCI HAR Dataset/test/y_test.txt")
  actlbl <- read.table("./UCI HAR Dataset/activity_labels.txt")
  featurelbl <- read.table("./UCI HAR Dataset/features.txt")
  trainsub <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  testsub <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  
  # Append label columns to the data sets
  trainset$Subject_id <- trainsub$V1
  testset$Subject_id <- testsub$V1
  trainset$Activity <- trainlbl$V1
  testset$Activity <- testlbl$V1
  
  # No. 1  Merge the testset and trainset
  projdata <- rbind(trainset, testset)
  
  # No. 4 Provide descriptive variable names and add the additional two columsn
  # where featurelbl provides the list of variable names
  names(projdata) <- c(as.character(featurelbl$V2),"Subject_id","Activity")
  
  # No. 2  Identify means and std dev colums
  meancols <- grep("mean()", names(projdata), fixed=TRUE)
  stdcols <- grep("std()", names(projdata), fixed=TRUE)
  subcols <- grep("Subject_id", names(projdata), fixed=TRUE)
  actcols <- grep("Activity", names(projdata), fixed=TRUE)
  mscols <- sort(c(meancols, stdcols, subcols, actcols))
  # Keep only the mean and std colums along with the Subject_id and Activity columns
  projsub <- projdata[,mscols]
  
  # No. 3  Replace the activity codes with the actual activity descriptions
  projsub$Activity <- as.character(actlbl[projsub$Activity,2])
  
  # No. 5 New table with mean of each variable by subject by activity
  tidyrows <- length(unique(projsub$Subject_id)) * length(unique(actlbl$V2))
  tidydata <- data.frame(matrix(ncol = length(names(projsub)), nrow = tidyrows))
  
  subids <- sort(unique(projsub$Subject_id))
  actids <- sort(unique(actlbl$V2))
  tidycola <- length(names(projsub))
  tidycols <- tidycola - 2
  
  rcnt = 1
  
  for (i in 1:length(subids)) {
    for (j in 1:length(actids)) {
      tidydata[rcnt,1] <- subids[i]
      tidydata[rcnt,2] <- as.character(actids[j])
      loopset <- projsub[(projsub$Activity == actids[j] & projsub$Subject_id == subids[i]),1:tidycols]
      tidydata[rcnt,3:tidycola] <- colMeans(loopset)
      rcnt <- rcnt + 1
    }
  }
  
  tidynames <- names(projsub[ ,1:tidycols])
  
  names(tidydata) <- c("Subject_id", "Activity", as.character(tidynames))
  
# head(tidydata[,1:5],20)
  
  tidydata
}