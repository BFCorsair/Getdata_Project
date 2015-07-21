run_analysis <- function(){

	# STEP 1: Merges the training and the test sets to create one data set.
	# Get the list of activities
	activity <- read.table("UCI-HAR-Dataset/activity_labels.txt",header=FALSE)
	colnames(activity) <- c("index","Activity_Label")
	# activity

	# Get the list of features
	all_features <- read.table("UCI-HAR-Dataset/features.txt",header=FALSE)
	colnames(all_features) <- c("index","Feature_Label")
	# all_features

	# STEP 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
	# Extract  the features that aremean and standard deviation of the measurements
	features_std <- all_features[grep("std", all_features$Feature_Label),]  # 33 
	features_mean <- all_features[grep("mean", all_features$Feature_Label),]  # 46
	features<-rbind(features_mean, features_std)


	# Get and merge test and training data
	train_df <- read.table("UCI-HAR-Dataset/train/X_train.txt",header=FALSE)
	test_df <- read.table("UCI-HAR-Dataset/test/X_test.txt",header=FALSE)
	all_data_df <- rbind(train_df,test_df)
	data_df <- all_data_df[,features[,2]]
	rm(all_data_df) # reclaim memory


	# STEP 4 Appropriately labels the data set with descriptive variable names. 
	# remove the ()
	features$Feature_Label <- sub("()","",features$Feature_Label, fixed=TRUE)
	# sub replaces the first match - which is good since t & f are at start of word
	features$Feature_Label <- sub("t","Time_",features$Feature_Label, fixed=TRUE) 
	features$Feature_Label <- sub("f","Frequency_",features$Feature_Label, fixed=TRUE)
	# replace all '-' with "_"
	features$Feature_Label <- gsub("-","_",features$Feature_Label, fixed=TRUE)
	colnames(data_df) <- features[,2]  # re-apply the names of the columns
	# data_df

	# STEP 3 Uses descriptive activity names to name the activities in the data set
	# Read the activity indices for train & test
	activity_test <- fread("UCI-HAR-Dataset/test/y_test.txt", header=FALSE)
	activity_train <- fread("UCI-HAR-Dataset/train/y_train.txt", header=FALSE)
	activity_values <- c(activity_train$V1,activity_test$V1)  # Need the V1 to get a vector vs a list
	activity_factor <- factor(activity_values, levels=activity$index, labels=activity$Activity_Label)
	# activity_factor

	# Add activity labels to the data
	data_df <- cbind(data_df, activity_factor)
	colnames(data_df)[ncol(data_df)] <- "Activity" # Rename the new column
	# data_df

 	# STEP 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
	# Read the subject indices for train & test and merge them

	subject_test <- fread("UCI-HAR-Dataset/test/subject_test.txt", header=FALSE)
	subject_train <- fread("UCI-HAR-Dataset/train/subject_train.txt", header=FALSE)
	subject_values <- c(subject_train$V1,subject_test$V1)  # Need the V1 to get a vector vs a list
	subject_factor <- factor(subject_values)
	# Add Subject  labels to the data
	data_df <- cbind(data_df, subject_factor)
	colnames(data_df)[ncol(data_df)] <- "Subject_ID" # Rename the new column
	# data_df

	# Average by activity and subject
	final_df <- aggregate(. ~ Activity+Subject_ID, data =data_df, mean)

	# Write out the results to the final
	write.table(final_df, file="run_analysis.txt", row.name=FALSE)
	final_df  # output the data for quick validation
}