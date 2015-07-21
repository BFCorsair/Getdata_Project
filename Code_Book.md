# Code Book for run_analysis.txt file
for Coursera "Getting and Cleaning Data" course

## Overview

The data analyzed represents fitness and  data collected from the accelerometers from the Samsung Galaxy S smartphones
A full description is available at the site where the data was obtained: 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the raw data for the project: 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

The R script run_analysis.R does the following. 
- Merges the training and the test sets to create one data set.
- Extracts only the measurements on the mean and standard deviation for each measurement. 
- Uses descriptive activity names to name the activities in the data set
- Appropriately labels the data set with descriptive variable names. 
- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

The output file is: run_analysis.txt
The first two columns contain the activity type and the subject ID. 
The remain columns are the means of all the measurements by activty type and subject

