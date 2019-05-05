# @author: Yujie Deng, on Mon. 29 Apr, 2019
# Getting and Cleaning Data
# Assignment for week 4

# clean the repos
rm(list=ls())
gc()

#loading libraries
library(data.table)
library(dplyr)

# set the working directories
setwd("~/ds/DSJHU/getting_cleaning_data") 
w_dir <- setwd("~/ds/DSJHU/getting_cleaning_data") 
folder <- "/UCI HAR Dataset" #define name of the folder

##############
#1. Merge the training and the test sets to create one data set
# load the input datasets
# commun part
features <- read.table(paste0(w_dir,
                              folder, "/features.txt"))
act_labels <- read.table(paste0(w_dir,folder, "/activity_labels.txt"))
colnames(act_labels) <- c("id_label", "act")

#training part
x_train <-read.table(paste0(w_dir,folder, "/train/X_train.txt"))
y_train <-read.table(paste0(w_dir,folder, "/train/Y_train.txt"))
sbj_train <-read.table(paste0(w_dir,folder, "/train/subject_train.txt"))
colnames(sbj_train) <- c("id_sbj")

# testing part
x_test <-read.table(paste0(w_dir,folder, "/test/X_test.txt"))
y_test <-read.table(paste0(w_dir,folder, "/test/Y_test.txt"))
sbj_test <-read.table(paste0(w_dir,folder, "/test/subject_test.txt"))
colnames(sbj_test) <- c("id_sbj")


#pre-processing the training data
colnames(x_train) <- features$V2 #reset the col names 
colnames(x_test) <-  features$V2 #reset the col names 
y_train$V1 <- as.numeric(sub("-","",y_train$V1))
colnames(y_train) <- c("id_label")
colnames(y_test) <- c("id_label")

# merging training and testing datasets
df_train <- cbind(x_train, y_train, sbj_train)
df_test <- cbind(x_test,y_test,sbj_test)
df_merge <- rbind(df_train, df_test)


###########
#2. Extracts only the measurements on the mean and standard deviation for each measurement
col <- names(df_merge)
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
