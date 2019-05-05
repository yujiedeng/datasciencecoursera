### This is a codebook for the script: run analysis: assignment getting and cleaning data
# Purpose of this project 
- train the ability to collect, work with, and clean a data set
- to prepare tidy data that can be used for later analysis

# Preliminary materials

One of the most exciting areas in all of data science right now is wearable computing. Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 

More details:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Data for the project:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


Data Set Information:

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.


## preparation part: garbage collection and loading necessary libraries, set directories
`< 
rm(list=ls())
gc()
library(data.table)
library(dplyr)
setwd("~/ds/DSJHU/getting_cleaning_data") 
w_dir <- setwd("~/ds/DSJHU/getting_cleaning_data") 
folder <- "/UCI HAR Dataset" #define name of the folder
>`

## Cleaning and Preparing the data
loading necessary variables: activities, subjects, training and testing data
`<features <- read.table(paste0(w_dir,
                              folder, "/features.txt"))
act_labels <- read.table(paste0(w_dir,folder, "/activity_labels.txt"))
colnames(act_labels) <- c("id_label", "act")

x_train <-read.table(paste0(w_dir,folder, "/train/X_train.txt"))
y_train <-read.table(paste0(w_dir,folder, "/train/Y_train.txt"))
sbj_train <-read.table(paste0(w_dir,folder, "/train/subject_train.txt"))
colnames(sbj_train) <- c("id_sbj")


x_test <-read.table(paste0(w_dir,folder, "/test/X_test.txt"))
y_test <-read.table(paste0(w_dir,folder, "/test/Y_test.txt"))
sbj_test <-read.table(paste0(w_dir,folder, "/test/subject_test.txt"))
colnames(sbj_test) <- c("id_sbj")

colnames(x_train) <- features$V2 #reset the col names 
colnames(x_test) <-  features$V2 #reset the col names 
y_train$V1 <- as.numeric(sub("-","",y_train$V1))
colnames(y_train) <- c("id_label")
colnames(y_test) <- c("id_label")

# merging training and testing datasets
df_train <- cbind(x_train, y_train, sbj_train)
df_test <- cbind(x_test,y_test,sbj_test)
df_merge <- rbind(df_train, df_test)
>`

Extracts only the measurements on the mean and standard deviation for each measurement
`<col <- names(df_merge)
vector <- (grepl("^id_label",col) | grepl("^id_sbj",col)
           |grepl("-mean..",col) & !grepl("-meanFreq..",col) & !grepl("mean..-",col) 
           |grepl("-std..",col) & !grepl("-std()..-",col) )

df_mean_st <- df_merge[vector==TRUE]

##########
# 3.Uses descriptive activity names to name the activities in the data set
df_mean_st_act <- merge(df_mean_st, act_labels, on="id_labels")


##########
# 4. Appropriately labels the data set with descriptive variable names.

new_col <- colnames(df_mean_st_act)

for (i in 1:length(new_col)){
      temp <- new_col[i]
      temp <- gsub("\\()","",temp)
      temp <- gsub("-std$","StandardDeviation",temp)
      temp <- gsub("-mean","Mean",temp)
      temp <- gsub("^(t)","Time",temp)
      temp <- gsub("^(f)","Freq",temp)
      temp <- gsub("([Gg]ravity)","Gravity",temp)
      temp <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",temp)
      temp <- gsub("[Gg]yro","Gyro",temp)
      temp <- gsub("AccMag","AccMagnitude",temp)
      temp <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",temp)
      temp <- gsub("JerkMag","JerkMagnitude",temp)
      temp <- gsub("GyroMag","GyroMagnitude",temp)
      new_col[i] <- temp
}

colnames(df_mean_st_act) <- new_col
df_mean_st_act <- subset(df_mean_st_act, select = -id_label)

##########
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
df_agg <- aggregate(df_mean_st_act[,names(df_mean_st_act) 
                                    != c('id_sbj','act')],by=list
                      (activityId=df_mean_st_act$act,
                        subjectId=df_mean_st_act$id_sbj),mean);

# Export tidyData set 
write.table(df_agg, './FinalTidyData.txt',row.names=FALSE,sep='\t')
>`
