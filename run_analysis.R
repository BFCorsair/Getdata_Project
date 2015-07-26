run_analysis <- function(){

	# STEP 1: Merges the training and the test sets to create one data set.
	# Get test and training data
	train_df <- read.table("UCI-HAR-Dataset/train/X_train.txt",header=FALSE)
	test_df <- read.table("UCI-HAR-Dataset/test/X_test.txt",header=FALSE)
	# Merge test and training data 
	all_data_df <- rbind(train_df,test_df)


	# STEP 2: Extract only the measurements on the mean and standard deviation for each measurement. 
	# Get the list of features/measurements
	all_features <- read.table("UCI-HAR-Dataset/features.txt",header=FALSE)
	colnames(all_features) <- c("index","Feature_Label")
	# Extract  the features that are mean and standard deviation of the measurements
	features_std <- all_features[grep("std", all_features$Feature_Label),]  # 33 
	features_mean <- all_features[grep("mean", all_features$Feature_Label),]  # 46
	features<-rbind(features_mean, features_std)
	col_labels <- features[,2]

	# Keep only the measurements that we want 
	data_df <- all_data_df[,col_labels]
	rm(all_data_df) # reclaim memory



	# STEP 3 Use descriptive activity names to name the activities in the data set
	# Get the list of activities
	activity <- read.table("UCI-HAR-Dataset/activity_labels.txt",header=FALSE)
	colnames(activity) <- c("index","Activity_Label")
	# Read & merge the activity indices for train & test
	activity_test <- fread("UCI-HAR-Dataset/test/y_test.txt", header=FALSE)
	activity_train <- fread("UCI-HAR-Dataset/train/y_train.txt", header=FALSE)
	activity_values <- c(activity_train$V1,activity_test$V1)  # Need the V1 to get a vector vs a list
	activity_factor <- factor(activity_values, levels=activity$index, labels=activity$Activity_Label)

	# Add activity labels to the data
	data_df <- cbind(data_df, activity_factor)


	# STEP 4 Appropriately labels the data set with descriptive variable names. 
	# remove the ()
	col_labels <- features[,2]
	col_labels <- sub("()","",col_labels, fixed=TRUE)
	# sub replaces the first match - which is good since t & f are at start of word
	col_labels <- sub("t","Time_",col_labels, fixed=TRUE) 
	col_labels <- sub("f","Frequency_",col_labels, fixed=TRUE)
	# replace all '-' with "_"
	col_labels <- gsub("-","_",col_labels, fixed=TRUE)
	col_labels <- c(col_labels, "Activity")  # add Activity for the extra column
	colnames(data_df) <- col_labels  # re-apply the names of the columns
	# data_df

 	# STEP 5 From the data set in step 4, creates a second, independent
 	# tidy data set with the average of each variable for each activity and each subject.
	# Read the subject indices for train & test and merge them

	subject_test <- fread("UCI-HAR-Dataset/test/subject_test.txt", header=FALSE)
	subject_train <- fread("UCI-HAR-Dataset/train/subject_train.txt", header=FALSE)
	subject_values <- c(subject_train$V1,subject_test$V1)  # Need the V1 to get a vector vs a list
	subject_factor <- factor(subject_values)
	# Add Subject  labels to the data
	data_df <- cbind(data_df, subject_factor)
	col_labels <- c(col_labels, "Subject_ID")  # add Subject_ID for the extra column
	colnames(data_df) <- col_labels  # re-apply the names of the columns
	# data_df

	# Average by activity and subject
	final_df <- aggregate(. ~ Activity+Subject_ID, data =data_df, mean)

	# Write out the results to the final
	write.table(final_df, file="run_analysis.txt", row.name=FALSE)
	final_df  # output the data for quick validation
}