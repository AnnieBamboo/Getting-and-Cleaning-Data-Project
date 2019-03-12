## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement
## 3. Uses descriptive activity names to name the activities in the data set.
## 4. Appropriately labels the data set with descriptive variable names.
## 5. Creates a second, independent tidy data set with the average of each variable for each.

library(reshape2)

## Get Data 

filename <"getdata_dataset.zip"

if (!file.exists(filename)) {
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl, filename, method="curl")
}
if (!file.exists("UCI HAR Dataset")) {
  unzip(filename)
}

## Read Data

# training data
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
sub_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# test data
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
sub_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# feature vector
features <- read.table("./UCI HAR Dataset/features.txt")

# data description
variable_names <- read.table("./UCI HAR Dataset/features.txt")

# activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

## 1.Merges the training and the test sets to create one data set.

x_FeaturesData <- rbind(x_train, x_test)
y_ActivityData <- rbind(y_train, y_test)
subjectData <- rbind(sub_train, sub_test)

# Assigns column names
colnames(sub_test) = "subject";
colnames(x_test) = features[,2];
colnames(y_test) = "activity";

merged_dataset <- cbind(x_FeaturesData, y_ActivityData, subjectData)

colnames(merged_dataset) = c("subject", features[, 2], "activity")


## 2.Extracts only the measurements on the mean and standard deviation for each measurement.

features.selected <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
x_FeaturesData <- x_FeaturesData[, features.selected]
names(x_FeaturesData) <-features[features.selected, 2]
names(x_FeaturesData) <- gsub("\\(|\\)", "", names(x_FeaturesData))
names(x_FeaturesData) <- gsub("\\-", "", names(x_FeaturesData))

## 3. Use descriptive activity names to name the activities in the data set.
colnames(y_ActivityData) <- "activity"
y_ActivityData$activitylabel <- factor(y_ActivityData$activity, labels = as.character(activity_labels[,2]))
activitylabel <- y_ActivityData

## 4. Appropriately labels the data set with descriptive names.
colnames(x_FeaturesData) <- variable_names[features.selected, 2]


## 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

colnames(subjectData) <- "subject"
total <- cbind(x_FeaturesData, activitylabel, subjectData)
total_mean <- total %>% group_by(activitylabel, subject) %>% summarize_all(funs(mean))

write.table(total_mean, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)



