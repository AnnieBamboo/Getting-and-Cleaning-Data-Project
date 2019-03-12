# Code Book

This is a code book that describes the variables, the data, and any transformations or work that was performed to clean up the data.

## The data source

* Original data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
* Original description of the dataset: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## Data Set Information

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.


## Transformation details

There are 5 parts:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names.
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## How ```run_analysis.R``` implements the above steps:

* Require ```reshape2``` library.
* Load both test and train data
* Load the features and activity labels.
* Extract the mean and standard deviation column names and data.
* Process the data. There are two parts processing test and train data respectively.
* Merge data set.

## ID Fields

* `subject` - The participant ("subject") ID
* `activity` - The label of the activity performed when the corresponding measurements were taken

## Extracted Feature Fields

* `tBodyAcc-mean()-X` (column `1`)
* `tBodyAcc-mean()-Y` (column `2`)
* `tBodyAcc-mean()-Z` (column `3`)
* `tBodyAcc-std()-X` (column `4`)
* `tBodyAcc-std()-Y` (column `5`)
* `tBodyAcc-std()-Z` (column `6`)
* `tGravityAcc-mean()-X` (column `41`)
* `tGravityAcc-mean()-Y` (column `42`)
* `tGravityAcc-mean()-Z` (column `43`)
* `tGravityAcc-std()-X` (column `44`)
* `tGravityAcc-std()-Y` (column `45`)
* `tGravityAcc-std()-Z` (column `46`)
* `tBodyAccJerk-mean()-X` (column `81`)
* `tBodyAccJerk-mean()-Y` (column `82`)
* `tBodyAccJerk-mean()-Z` (column `83`)
* `tBodyAccJerk-std()-X` (column `84`)
* `tBodyAccJerk-std()-Y` (column `85`)
* `tBodyAccJerk-std()-Z` (column `86`)
* `tBodyGyro-mean()-X` (column `121`)
* `tBodyGyro-mean()-Y` (column `122`)
* `tBodyGyro-mean()-Z` (column `123`)
* `tBodyGyro-std()-X` (column `124`)
* `tBodyGyro-std()-Y` (column `125`)
* `tBodyGyro-std()-Z` (column `126`)
* `tBodyGyroJerk-mean()-X` (column `161`)
* `tBodyGyroJerk-mean()-Y` (column `162`)
* `tBodyGyroJerk-mean()-Z` (column `163`)
* `tBodyGyroJerk-std()-X` (column `164`)
* `tBodyGyroJerk-std()-Y` (column `165`)
* `tBodyGyroJerk-std()-Z` (column `166`)
* `tBodyAccMag-mean()` (column `201`)
* `tBodyAccMag-std()` (column `202`)
* `tGravityAccMag-mean()` (column `214`)
* `tGravityAccMag-std()` (column `215`)
* `tBodyAccJerkMag-mean()` (column `227`)
* `tBodyAccJerkMag-std()` (column `228`)
* `tBodyGyroMag-mean()` (column `240`)
* `tBodyGyroMag-std()` (column `241`)
* `tBodyGyroJerkMag-mean()` (column `253`)
* `tBodyGyroJerkMag-std()` (column `254`)
* `fBodyAcc-mean()-X` (column `266`)
* `fBodyAcc-mean()-Y` (column `267`)
* `fBodyAcc-mean()-Z` (column `268`)
* `fBodyAcc-std()-X` (column `269`)
* `fBodyAcc-std()-Y` (column `270`)
* `fBodyAcc-std()-Z` (column `271`)
* `fBodyAccJerk-mean()-X` (column `345`)
* `fBodyAccJerk-mean()-Y` (column `346`)
* `fBodyAccJerk-mean()-Z` (column `347`)
* `fBodyAccJerk-std()-X` (column `348`)
* `fBodyAccJerk-std()-Y` (column `349`)
* `fBodyAccJerk-std()-Z` (column `350`)
* `fBodyGyro-mean()-X` (column `424`)
* `fBodyGyro-mean()-Y` (column `425`)
* `fBodyGyro-mean()-Z` (column `426`)
* `fBodyGyro-std()-X` (column `427`)
* `fBodyGyro-std()-Y` (column `428`)
* `fBodyGyro-std()-Z` (column `429`)
* `fBodyAccMag-mean()` (column `503`)
* `fBodyAccMag-std()` (column `504`)
* `fBodyBodyAccJerkMag-mean()` (column `516`)
* `fBodyBodyAccJerkMag-std()` (column `517`)
* `fBodyBodyGyroMag-mean()` (column `529`)
* `fBodyBodyGyroMag-std()` (column `530`)
* `fBodyBodyGyroJerkMag-mean()` (column `542`)
* `fBodyBodyGyroJerkMag-std()` (column `543`)

## Activity Labels

* `WALKING` (value `1`): subject was walking during the test
* `WALKING_UPSTAIRS` (value `2`): subject was walking up a staircase during the test
* `WALKING_DOWNSTAIRS` (value `3`): subject was walking down a staircase during the test
* `SITTING` (value `4`): subject was sitting during the test
* `STANDING` (value `5`): subject was standing during the test
* `LAYING` (value `6`): subject was laying down during the test 

## Extracted Features Vector

```R
c(1, 2, 3, 4, 5, 6, 41, 42, 43, 44, 45, 46, 81, 82, 83, 84, 85, 86, 121, 122, 123, 124, 125, 126, 161, 162, 163, 164, 165, 166, 201, 202, 214, 215, 227, 228, 240, 241, 253, 254, 266, 267, 268, 269, 270, 271, 345, 346, 347, 348, 349, 350, 424, 425, 426, 427, 428, 429, 503, 504, 516, 517, 529, 530, 542, 543)
```

## Extracted Feature Names Vector

```R
c("tBodyAcc-mean()-X", "tBodyAcc-mean()-Y", "tBodyAcc-mean()-Z", "tBodyAcc-std()-X", "tBodyAcc-std()-Y", "tBodyAcc-std()-Z", "tGravityAcc-mean()-X", "tGravityAcc-mean()-Y", "tGravityAcc-mean()-Z", "tGravityAcc-std()-X", "tGravityAcc-std()-Y", "tGravityAcc-std()-Z", "tBodyAccJerk-mean()-X", "tBodyAccJerk-mean()-Y", "tBodyAccJerk-mean()-Z", "tBodyAccJerk-std()-X", "tBodyAccJerk-std()-Y", "tBodyAccJerk-std()-Z", "tBodyGyro-mean()-X", "tBodyGyro-mean()-Y", "tBodyGyro-mean()-Z", "tBodyGyro-std()-X", "tBodyGyro-std()-Y", "tBodyGyro-std()-Z", "tBodyGyroJerk-mean()-X", "tBodyGyroJerk-mean()-Y", "tBodyGyroJerk-mean()-Z", "tBodyGyroJerk-std()-X", "tBodyGyroJerk-std()-Y", "tBodyGyroJerk-std()-Z", "tBodyAccMag-mean()", "tBodyAccMag-std()", "tGravityAccMag-mean()", "tGravityAccMag-std()", "tBodyAccJerkMag-mean()", "tBodyAccJerkMag-std()", "tBodyGyroMag-mean()", "tBodyGyroMag-std()", "tBodyGyroJerkMag-mean()", "tBodyGyroJerkMag-std()", "fBodyAcc-mean()-X", "fBodyAcc-mean()-Y", "fBodyAcc-mean()-Z", "fBodyAcc-std()-X", "fBodyAcc-std()-Y", "fBodyAcc-std()-Z", "fBodyAccJerk-mean()-X", "fBodyAccJerk-mean()-Y", "fBodyAccJerk-mean()-Z", "fBodyAccJerk-std()-X", "fBodyAccJerk-std()-Y", "fBodyAccJerk-std()-Z", "fBodyGyro-mean()-X", "fBodyGyro-mean()-Y", "fBodyGyro-mean()-Z", "fBodyGyro-std()-X", "fBodyGyro-std()-Y", "fBodyGyro-std()-Z", "fBodyAccMag-mean()", "fBodyAccMag-std()", "fBodyBodyAccJerkMag-mean()", "fBodyBodyAccJerkMag-std()", "fBodyBodyGyroMag-mean()", "fBodyBodyGyroMag-std()", "fBodyBodyGyroJerkMag-mean()", "fBodyBodyGyroJerkMag-std()")
```

## Activities Vector

```R
c(1, 2, 3, 4, 5, 6)
```

## Activity Names Vector

```R
c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
```