# Getting-Cleaning-Data-Project
Coursera Getting and Cleaning Data class project 
Getting and Cleaning Data Course Project:  README.md
TS
8/2/18

Project Instructions
Create one R script called run_analysis.R that does the following.
1.	Merge the training and the test sets to create one data set.
2.	Extract only the measurements on the mean and standard deviation for each measurement.
3.	Use descriptive activity names to name the activities in the data set
4.	Appropriately label the data set with descriptive variable names.
5.	From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject. 

Data Acquisition
Downloaded the data set from:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
and will save the zip file to the local directory and extract the needed files.

One of the extracted files is titled ‘README.txt’ and contains a description of the extracted files.  It includes the following file descriptions.

=========================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

Additional files are included in the .zip file but are not needed so will be excluded from this project.

# Allows user to run the function from the command line in RStudio
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

# No. 4 Provide descriptive variable names and add the additional two columns
# where featurelbl provides the list of variable names
names(projdata) <- c(as.character(featurelbl$V2),"Subject_id","Activity")

# No. 2  Identify means and std dev columns
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

# Determines number of rows in final table by multiplying the number of subjects times the number of 
# activities per subject
tidyrows <- length(unique(projsub$Subject_id)) * length(unique(actlbl$V2))

# Build an empty data.frame to load the tidy data into
tidydata <- data.frame(matrix(ncol = length(names(projsub)), nrow = tidyrows))

# Prepare some variables to help loop through the data
subids <- sort(unique(projsub$Subject_id))
actids <- sort(unique(actlbl$V2))
tidycola <- length(names(projsub))
tidycols <- tidycola – 2  # Will use this to help with moving Subject_id and Activity to columns 1 and 2
  
rcnt = 1  # Keeps track of which row is being written to
  
for (i in 1:length(subids)) {      # Loop through each subject
    for (j in 1:length(actids)) {   # Loop through each activity per subject
      tidydata[rcnt,1] <- subids[i]
      tidydata[rcnt,2] <- as.character(actids[j])
     
      # Subset the data by subject and activity so the mean of each variable for this subset can be calc’d
      loopset <- projsub[(projsub$Activity == actids[j] & projsub$Subject_id == subids[i]),1:tidycols]
      
      # Calculate the mean of each variable for the ith subject and jth activity 
      tidydata[rcnt,3:tidycola] <- colMeans(loopset)  
      rcnt <- rcnt + 1
    }
}
 
# Add column names to tidydata 
# All column names except “Subject_id” and “Activity”
tidynames <- names(projsub[ ,1:tidycols])  
# Set “Subject_id” and “Activity” as the first two column names
names(tidydata) <- c("Subject_id", "Activity", as.character(tidynames))

tidydata  #returns the final data set

}  # End buildtidy


