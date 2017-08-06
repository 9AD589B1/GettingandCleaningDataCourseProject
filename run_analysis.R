#This R script will load, clean, merge and perform calculations on a test and training data set from the Human Activity Recognition Using Smartphones Data Set 
#There are four main blocks to this script. The first block prepares the column headers for both the Test and Training sets,
#the second block prepares the test data, the third block prepares the training data, and the fourth block combines
#creates an independent data set by combining both the Test and Training sets and performing calculations on specific variables

#Remove all variables
rm(list=ls(all=TRUE)) 

#Install and load required libraries for data cleaning and shaping
install.packages("reshape2")
install.packages("dplyr")
library(reshape2)
library(dplyr)

##################################################
# Create Column Headers for Measurement Variables#
##################################################

#Load Test and Training Data variable names set
HeadersRaw <- read.table("C:/Users/DK/Documents/UCI HAR Dataset/features.txt")

#Assign column names to the variable names
colnames(HeadersRaw) <- c("ColIndex", "ColName")

#Explicitly convert variable names to a data frame to ensure manipulation operations will compile
HeadersRaw <- data.frame(HeadersRaw)

#Remove non-alphanumeric characters from the variable names to tidy the column names
HeadersRaw <- mutate(HeadersRaw, ColName = gsub("[^[:alnum:]]","", ColName))

#Select the second column which is the descriptive column names. This column
#will be used to create the headers in both the Test and Training sets
Headers <- HeadersRaw[[2]]

#####################
# Load the Test Set #
#####################

#Load the Test Data set
TestData <- read.table("C:/Users/DK/Documents/UCI HAR Dataset/test/X_test.txt")

#Assign column names to the Test Data set using the Headers vector
colnames(TestData) <- Headers

#Explicitly convert the Test Data set to a data frame to ensure manipulation operations will compile
TestData <- data.frame(TestData)

#Add an ID column to the Test Data set, as this will be required in subsequent merge operation
TestData <- mutate(TestData, ID = rownames(TestData))

#Load the Test Set Activity Codes
TestActivityIDRaw <- read.table("C:/Users/DK/Documents/UCI HAR Dataset/test/y_test.txt")

#Assign a column name to the Test Set Activity Codes
colnames(TestActivityIDRaw) <- c("ActivityID")

#Explicitly convert the Activity Code set to a data frame to ensure manipulation operations will compile
TestActivityIDRaw <- data.frame(TestActivityIDRaw)

#Add the activity description and an ID to the Activity Code set based on the provided decode from activity_labels.txt below
#1 WALKING
#2 WALKING_UPSTAIRS
#3 WALKING_DOWNSTAIRS
#4 SITTING
#5 STANDING
#6 LAYING
TestActivityDescriptions <- mutate(TestActivityIDRaw, ActivityDescription = case_when(ActivityID == 1 ~ "Walking", ActivityID == 2 ~ "Walking Upstairs", ActivityID == 3 ~ "Walking Downstairs", ActivityID == 4 ~ "Sitting", ActivityID == 5 ~ "Standing", ActivityID == 6 ~ "Laying"), ID = rownames(TestActivityIDRaw))

#Load the Subject IDs
TestSubjectIDRaw <- read.table("C:/Users/DK/Documents/UCI HAR Dataset/test/subject_test.txt")

#Assign a column name to the Subject ID set
colnames(TestSubjectIDRaw) <- c("SubjectID")

#Explicitly convert the Subject ID set to a data frame to ensure manipulation operations will compile
TestSubjects <- data.frame(TestSubjectIDRaw)

#Add an ID column to TestSubjects
TestSubjects <- mutate(TestSubjects, ID = rownames(TestSubjects))

#Join the Activity Descriptions set and the Test Data set
TestDataTemp <- merge(TestActivityDescriptions, TestData, "ID")

#Join the Subject ID set to the TestDataTemp set

TestData <- merge(TestSubjects, TestDataTemp, "ID")

#########################
# Load the Training Set #
#########################

#Load the Training Data set
TrainingData <- read.table("C:/Users/DK/Documents/UCI HAR Dataset/train/X_train.txt")

#Assign column names to the Training Data set using the Headers vector
colnames(TrainingData) <- Headers

#Explicitly convert the Training Data set to a data frame to ensure manipulation operations will compile
TrainingData <- data.frame(TrainingData)

#Add an ID column to the Training Data set, as this will be required in subsequent merge operation
TrainingData <- mutate(TrainingData, ID = rownames(TrainingData))

#Load the Training Set Activity Codes
TrainingActivityIDRaw <- read.table("C:/Users/DK/Documents/UCI HAR Dataset/train/y_train.txt")

#Assign a column name to the Training Set Activity Codes
colnames(TrainingActivityIDRaw) <- c("ActivityID")

#Explicitly convert the Activity Code set to a data frame to ensure manipulation operations will compile
TrainingActivityIDRaw <- data.frame(TrainingActivityIDRaw)

#Add the activity description and an ID to the Activity Code set based on the provided decode from activity_labels.txt below
#1 WALKING
#2 WALKING_UPSTAIRS
#3 WALKING_DOWNSTAIRS
#4 SITTING
#5 STANDING
#6 LAYING
TrainingActivityDescriptions <- mutate(TrainingActivityIDRaw, ActivityDescription = case_when(ActivityID == 1 ~ "Walking", ActivityID == 2 ~ "Walking Upstairs", ActivityID == 3 ~ "Walking Downstairs", ActivityID == 4 ~ "Sitting", ActivityID == 5 ~ "Standing", ActivityID == 6 ~ "Laying"), ID = rownames(TrainingActivityIDRaw))

#Load the Subject IDs
TrainingSubjectIDRaw <- read.table("C:/Users/DK/Documents/UCI HAR Dataset/train/subject_train.txt")

#Assign a column name to the Subject ID set
colnames(TrainingSubjectIDRaw) <- c("SubjectID")

#Explicitly convert the Subject ID set to a data frame to ensure manipulation operations will compile
TrainingSubjects <- data.frame(TrainingSubjectIDRaw)

#Add an ID column to TrainingSubjects
TrainingSubjects <- mutate(TrainingSubjects, ID = rownames(TrainingSubjects))

#Join the Activity Descriptions set and the Training Data set
TrainingDataTemp <- merge(TrainingActivityDescriptions, TrainingData, "ID")

#Join the Subject ID set to the TrainingDataTemp set

TrainingData <- merge(TrainingSubjects, TrainingDataTemp, "ID")

# Merge the Test and Training Sets and calculate
TestandTrainingUnion <- rbind(TestData, TrainingData)

###############################################################################################################
# Select only Mean and Standard Deviation measurements and calculate the mean grouped by Activity and Subject #
###############################################################################################################

#Subset the unioned Test and Training sets by selecting the grouping columns and the desired measurements
TestandTrainingUnionSubset <- select(TestandTrainingUnion, ActivityDescription, SubjectID, matches("mean|std"))

#Group the Subset by ActivityDescription and SubjectID
TestandTrainingUnionSubsetGrouped <- group_by (TestandTrainingUnionSubset, ActivityDescription, SubjectID)

#Summarize the grouped set by applying the mean to all pre-selected measurements
TestandTrainingMeanSummary <- summarise_all(TestandTrainingUnionSubsetGrouped, funs(mean))

