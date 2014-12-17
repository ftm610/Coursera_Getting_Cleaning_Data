# Coursera Getting and Cleaning Data
# Course Project
# 
# One of the most exciting areas in all of data science right now is wearable computing - 
# see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to 
# develop the most advanced algorithms to attract new users. The data linked to from the 
# course website represent data collected from the accelerometers from the Samsung Galaxy S 
# smartphone. A full description is available at the site where the data was obtained:
# 
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
# Here are the data for the project:
# 
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 
# You should create one R script called run_analysis.R that does the following. 
# 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# 
# From the data set in step 4, creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject.

#############################################################################################
# setup

# set working directory
path_wd = "C:\\Users\\Frank\\Documents\\Coursera\\Getting_And_Cleaning_Data\\CourseProject"
setwd(path_wd)

# load required packages
library(dplyr)
library(reshape2)

#############################################################################################
# download and extract datasets

# download raw data
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, "dataset.zip")
dateDownloaded <- date()
list.files("./")

# unzip downloaded data
unzip("dataset.zip")
list.files("./")  

# set paths to unzipped raw data
path_test = paste(path_wd,"\\UCI HAR Dataset\\test", sep = "")
path_train = paste(path_wd,"\\UCI HAR Dataset\\train", sep = "")

#############################################################################################
# read in datasets

# set paths to common files
file_activity <- paste(path_wd, "\\UCI HAR Dataset\\activity_labels.txt", sep = "")
file_features <- paste(path_wd, "\\UCI HAR Dataset\\features.txt", sep = "")

# read in common files
activity <- read.table(file_activity)
# activity <- tbl_df(activity)
features <- read.table(file_features)
# features <- tbl_df(features)

# set paths to test files
file_subject_test = paste(path_test, "\\subject_test.txt", sep = "")
file_x_test = paste(path_test, "\\X_test.txt", sep = "")
file_y_test = paste(path_test,"\\y_test.txt", sep = "")

# set paths to train files
file_subject_train = paste(path_train, "\\subject_train.txt", sep = "")
file_x_train = paste(path_train, "\\X_train.txt", sep = "")
file_y_train = paste(path_train,"\\y_train.txt", sep = "")

# read in test data
data <- read.table(file_subject_test)
data_subject_test <- tbl_df(data)
data <- read.table(file_x_test)
data_x_test <- tbl_df(data)
data <- read.table(file_y_test)
data_y_test <- tbl_df(data)

# read in train data
data <- read.table(file_subject_train)
data_subject_train <- tbl_df(data)
data <- read.table(file_x_train)
data_x_train <- tbl_df(data)
data <- read.table(file_y_train)
data_y_train <- tbl_df(data)

# clean up interim data
rm(data)
rm(path_test)
rm(path_train)
rm(file_activity)
rm(file_features)
rm(file_subject_test)
rm(file_x_test)
rm(file_y_test)
rm(file_subject_train)
rm(file_x_train)
rm(file_y_train)

#############################################################################################
# format datasets

# assign subject variable name and source dataset value
names(data_subject_test) <- "subject"
data_subject_test <- mutate(data_subject_test, dataset = "test")
names(data_subject_train) <- "subject"
data_subject_train <- mutate(data_subject_train, dataset = "train")

# assign x data variable names from features.txt
var_names <- make.names(features$V2, unique = TRUE)
names(data_x_test) <- var_names
names(data_x_train) <- var_names

# document original and modified variable names for codebook.txt
column_names <- cbind(features, var_names)
names(column_names) <- c("features.txt_column_number", "features.txt_variable_name", "tidy.txt_variable_name")
column_names <- tbl_df(column_names)

# join y data and activity labels
data_y_test_join_activity <- inner_join(data_y_test, activity)
data_y_train_join_activity <- inner_join(data_y_train, activity)

# assign y data variable names
names(data_y_test_join_activity) <- c("activity_id", "activity") 
names(data_y_train_join_activity) <- c("activity_id", "activity") 

# combine subject, x and y datasets
data_test_bind <- cbind(data_subject_test, data_y_test_join_activity, data_x_test)
data_train_bind <- cbind(data_subject_train, data_y_train_join_activity, data_x_train)

# merge formatted test and train datasets
data_all <- rbind(data_test_bind, data_train_bind)

# clean up interim data
rm(activity)
rm(features)
rm(var_names)
rm(data_subject_test)
rm(data_subject_train)
rm(data_x_test)
rm(data_x_train)
rm(data_y_test)
rm(data_y_train)
rm(data_y_test_join_activity)
rm(data_y_train_join_activity)
rm(data_test_bind)
rm(data_train_bind)

#############################################################################################
# filter merged dataset

# select data columns "mean" and "std" only [as in features.txt variable names mean() and std()]
data_all_select <- select(data_all, subject, activity, contains("mean"), contains("std"), 
                             -contains("freq"), -contains("angle"))
data_all_select <- tbl_df(data_all_select)

# match variables selected to original file names for README.txt
column_names_select <- data.frame(names(data_all_select))
names(column_names_select) <- "tidy.txt_variable_name"
column_names_selected <- left_join(column_names_select, column_names)

# clean up interim data
rm(column_names_select)
rm(data_all)

#############################################################################################
# summarize and output tidy data

# for each combination of subject and activity: calculate the mean of each variable
data_melt <- melt(data_all_select, c("subject", "activity"))
data_summary <- dcast(data_melt, activity + subject ~ variable, mean)

# save out summarized data to text file "tidy.txt" and variable names "codebook.txt" [delimiter = " "]
path_file_out <- paste(path_wd, "\\UCI HAR Dataset", sep = "")
write.table(data_summary, paste(path_file_out, "\\tidy.txt", sep = ""), row.names = FALSE)
write.table(column_names_selected, paste(path_file_out, "\\codebook.txt", sep = ""), row.names = FALSE)
            
# clean up interim data
rm(data_melt)

#############################################################################################
# review output data

# output_data <- read.table(paste(path_file_out, "\\tidy.txt", sep = ""), header = TRUE)
# output_code <- read.table(paste(path_file_out, "\\codebook.txt", sep = ""), header = TRUE)