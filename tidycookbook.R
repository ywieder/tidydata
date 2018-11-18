#read in the test and training datasets
test.set <- read.table("./UCI HAR Dataset/test/X_test.txt", header=FALSE)
train.set <- read.table("./UCI HAR Dataset/train/X_train.txt", header=FALSE)

#read in the features, and set the column names in the train and test
#datasets to the feature names
features <- read.table("./UCI HAR Dataset/features.txt", header=FALSE)
colnames(test.set) <- features$V2
colnames(train.set) <- features$V2

#read in the subjects, and add the subjects as a new variable in the
#train and test datasets
subject.test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header=FALSE)
subject.train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header=FALSE)
test.set$subject <- subject.test[,1]
train.set$subject <- subject.train[,1]

#read in the activities, and add the activities as a new variable in the
#train and test datasets
activity.test <- read.table("./UCI HAR Dataset/test/y_test.txt", header=FALSE)
activity.train <- read.table("./UCI HAR Dataset/train/y_train.txt", header=FALSE)
test.set$activity <- activity.test[,1]
train.set$activity <- activity.train[,1]

#indicator for whether the data came from the test or training dataset
test.set$type <- "TRAIN"
train.set$type <- "TEST"

#use the activity label mappings provided to assign an activity label
#based on the activity number
test.set$activity.label <- ifelse(test.set$activity==1, "WALKING", 
                                  ifelse(test.set$activity==2, "WALKING_UPSTAIRS",
                                  ifelse(test.set$activity==3, "WALKING_DOWNSTAIRS",
                                  ifelse(test.set$activity==4, "SITTING",
                                  ifelse(test.set$activity==5, "STANDING",
                                  ifelse(test.set$activity==6, "LAYING", "NA"))))))

train.set$activity.label <- ifelse(train.set$activity==1, "WALKING", 
                                  ifelse(train.set$activity==2, "WALKING_UPSTAIRS",
                                  ifelse(train.set$activity==3, "WALKING_DOWNSTAIRS",
                                  ifelse(train.set$activity==4, "SITTING",
                                  ifelse(train.set$activity==5, "STANDING",
                                  ifelse(train.set$activity==6, "LAYING", "NA"))))))

#merges the train and test datasets
full.data.set <- rbind(test.set, train.set)

#get the indices of the variables that contain
#'mean' or 'std' in their names
mean.sd.col.indices <- grep("mean|std",names(full.data.set))

#create new dataset based on only the variables with 'mean' or 'sd' in their names
#and the subject/activity.label variables
mean.sd.data.set <- full.data.set[,c(mean.sd.col.indices, 562,565)]

#rename the new datasets variables to align with the variable names in the original
#full dataset
colnames(mean.sd.data.set) <- names(full.data.set)[c(mean.sd.col.indices, 562, 565)]

#group the dataset by activity.label and subject
#then calculate the average value for each measurement
#grouped by activity.label and subject
library(dplyr)
mean.sd.grouped <- group_by(mean.sd.data.set, activity.label, subject)
average.grouped <- summarize_all(mean.sd.grouped, funs(mean))

#write the tidy dataset to a csv
write.csv(average.grouped, "./cleandataset.csv", row.names=FALSE)
