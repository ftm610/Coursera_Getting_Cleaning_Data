Coursera_Getting_Cleaning_Data
==============================

Repo for Course Project of Coursera: Getting and Cleaning Data 

Coursera Getting and Cleaning Data
Course Project

run_analysis.R

Developed on: 
Windows 7 Professional 64-bit SP1 
R version 3.1.2 (2014-10-31) -- "Pumpkin Helmet"
RStudio version 0.98.1087

R packages required:
dplyr version 0.3.0.2
reshape2 version 1.4.1

To run this code, first update the path (path_wd) to your preferred working directory.

This code performs the following actions:
1. Downloads and extracts raw zipped data to user defined working directory.
	1a. Downloads raw zipped data to working directory from web: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
	1b. Extracts files to working directory
	1c. Records date and time of download to the "dateDownloaded" variable

2. Merges the training and the test sets to create one data set.
	2a. Reads in, formats and combines the following common, test and train text files: activity_labels, features, subject_test, X_test, y_test, subject_train, X_train, y_train
	2b. Subject data is assigned to "subject" variable.  Source dataset (test or train) is recorded in the "dataset" variable.   
	2c. Generates a single tidy data frame "data_all" with dataset (test or train), subject, activity (label) and the variables selected in Step 3 for further analysis.  Each row is a single observation, each column is a single variable.  
	*See Also: 4a, 5a and code comments for more detail

3. Extracts only the measurements on the mean and standard deviation for each measurement. 
	3a. Selects only variables named like mean() and std() in original features.txt file
	3b. Original and converted variable names are recorded in the column_names_select (selected from column_names) data frame generated by this code for comparison to downloaded README and features_info files. 
	*See Also: 5a-b
	
4. Uses descriptive activity names to name the activities in the data set
	4a. Assigns the descriptive labels in activity_labels.txt to the values in the y_test and y_train files to the variable "activity".   

5. Appropriately labels the data set with descriptive variable names. 
	5a. Assigns variable names to test and train data frames from the downloaded features.txt file.  Features.txt contains duplicate and invalid variable names. Unique (and valid) variable names are generated using make.names with unique = TRUE.  
	5b. Original and converted variable names are recorded in the column_names data frame generated by this code for comparison to downloaded README and features_info files.
	
6. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
	6a. Summarizes each variable by activity and subject using reshape2: melt and dcast 
	6b. Writes out a space delimited text file "tidy.txt" using write.table with row.names = FALSE)
	6c. Code to properly read output file back into R is included (commented out at end)
	*See Also: 4b, 5b and CodeBook.md for description of variables
	
