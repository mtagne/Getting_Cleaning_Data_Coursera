#URL to download from
fileUrla <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#dowload the zip file
download.file(fileUrla, destfile = "./data/data.zip", method="curl")
#unzip the file to data2 directory
unzip("./data/data.zip", exdir = "./data2")
# Training Data Set dataframe build up
#utility function to extract data from dataframe.
first_elt <- function (x){x[1]}
second_elt <- function (x){x[2]}

# read training activities
sub.train.act <- read.table("./data2/train/y_train.txt", sep = "\n", stringsAsFactors = FALSE)
sub.train.act <- sub.train.act$V1

#read training subjects
subj.train <- read.table("./data2/train/subject_train.txt", sep = "\n", stringsAsFactors = FALSE)
subj.train <- subj.train$V1

##read training data 
train.data <- read.table("./data2/train/X_train.txt", sep = "\n", stringsAsFactors = FALSE)
# add training to dataframe
train.data <- train.data$V1
#fill in data training
traindf <- data.frame(matrix(ncol = 561, nrow = length(train.data)))
#parse training data to extract variable values
for (i in 1:length(train.data)) {
  traindf[i, ] <- unlist(strsplit(sub("^\\s+", "",train.data[i]), "[[:space:]]+"))
}
#convert to numeric
traindf <- as.data.frame(sapply(traindf, as.numeric))
#add subject and activity columns
traindf$activity <- sub.train.act
traindf$subject <- subj.train
train.df <- traindf

# test data dataframe construction
# read test activity
sub.test.act <- read.table("./data2/test/y_test.txt", sep = "\n", stringsAsFactors = FALSE)
sub.test.act <- sub.test.act$V1

#get test subject
subj.test <- read.table("./data2/test/subject_test.txt", sep = "\n", stringsAsFactors = FALSE)
subj.test <- subj.test$V1

#read test data
test.data <- read.table("./data2/test/X_test.txt", sep = "\n", stringsAsFactors = FALSE)
testdf <- data.frame(matrix(ncol = 561, nrow = length(test.data)))
# add test to test dataframe
test.data <- test.data$V1

#parse test data set to extract variable values
for (i in 1:length(test.data)) {
  testdf[i, ] <- unlist(strsplit(sub("^\\s+", "",test.data[i]), "[[:space:]]+"))
}
#convert to numeric
testdf <- as.data.frame(sapply(testdf, as.numeric))
#add subject and activity columns
testdf$activity <- sub.test.act
testdf$subject <- subj.test
test.df <- testdf

##1. merge datasets
train_test.df <- rbind(train.df, test.df)


##4. Appropriately labels the data set with descriptive variable names.
labels <- read.table("./data2/features.txt", sep = "\n", stringsAsFactors = FALSE)
labels <- strsplit(labels$V1, " ") 
labels <- sapply(labels, second_elt)
##make unique name labels, call make.names and make.unique
labels<- make.unique(make.names(labels))
labels <- c(labels, "activity", "subject")
colnames(train_test.df) <- labels
#
##2. Extracts only the measurements on the mean and standard deviation for each measurement.
#remove duplicate before using dplyr library

library(dplyr)
tr_test.df <- tbl_df(train_test.df)
rm("train_test.df")
# exclude "Mean" and "meanFreq
mean_std <- select(tr_test.df, contains("std", ignore.case = FALSE), contains("mean", ignore.case = FALSE), -contains("meanFreq"))

##3. Uses descriptive activity names to name the activities in the data set
#Map activity number to name using mapvalues from plyr lib.
library(plyr)
# read activity labels and map them : 
#[1, 2, 3, 4 and 5] map to [WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING]

activities <- read.table("./data2/activity_labels.txt", sep = "\n", stringsAsFactors = FALSE)
act <- strsplit(activities$V1, " ")
act.name <-  sapply(act, second_elt)
act.number <-  sapply(act, first_elt)
activities$number <- act.number
activities$name <- act.name
#remove original columns.
activities <- activities[, !(names(activities) == "V1")]
tr_test.df$activity <- mapvalues(tr_test.df$activity, activities$number, activities$name)

##4 : see above                        
##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
DF_avg_act_subj <- tr_test.df %>%
  group_by(activity, subject) %>%
  summarise_each(funs(mean)) %>%
  select(contains("activity"), contains("subject"), contains("std", ignore.case = FALSE), contains("mean", ignore.case = FALSE), -contains("meanFreq"))
#write dataset to a file
write.table(DF_avg_act_subj, "DF_avg_act_subj.txt", row.name=FALSE)
