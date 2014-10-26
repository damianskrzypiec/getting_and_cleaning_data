# Project for "Getting and cleaning data" @ Coursera.org

# Reading traning data and their labels
traning_data <- read.table("~/Documents/R/getting_and_cleaning_data/data/train/X_train.txt", quote="\"")
traning_labels <- read.table("~/Documents/R/getting_and_cleaning_data/data/train/y_train.txt", quote="\"")

# Reading test data and their labels
test_data <- read.table("~/Documents/R/getting_and_cleaning_data/data/test/X_test.txt", quote="\"")
test_labels <- read.table("~/Documents/R/getting_and_cleaning_data/data/test/y_test.txt", quote="\"")


# 1. Merging data in one data set
data <- rbind(traning_data, test_data)

# Setting names for variables
names <- read.table("~/Documents/R/getting_and_cleaning_data/data/features.txt", quote="\"")
names(data) <- names[,2]



# 2. Extracting only measurements that depend on mean and standard deviation
# At first we are checking if i-th variable's name contain "mean" or "std"
logical <- c(1:561)

for (i in 1:561){
  if ((length(grep("mean", names(data)[i])) >= 1) || (length(grep("std", names(data)[i])) >= 1)) {logical[i] <- 1}
  else {logical[i] <- 0}
}

# Finale step in extracting
data_ex <- data[, which(logical == 1)]


# 3. Making descriptive activity names
# Merging labels
activities <- rbind(traning_labels, test_labels)

# Adding activities variable to the data set and it's name
data_ex$act_code <- activities
names(data_ex$act_code) <- "act_code"

# Setting "real" name of activities for every observation
data_ex$activities <- rep("", dim(data_ex)[1])

# using which() function we are going to set appropriate names of activities
data_ex[which(data_ex$act_code == 1),]$activities <- rep("WALKING", length(which(data_ex$act_code == 1)))
data_ex[which(data_ex$act_code == 2),]$activities <- rep("WALKING-UPSTAIRS", length(which(data_ex$act_code == 2)))
data_ex[which(data_ex$act_code == 3),]$activities <- rep("WALKING-DOWNSTAIRS", length(which(data_ex$act_code == 3)))
data_ex[which(data_ex$act_code == 4),]$activities <- rep("SITTING", length(which(data_ex$act_code == 4)))
data_ex[which(data_ex$act_code == 5),]$activities <- rep("STANDING", length(which(data_ex$act_code == 5)))
data_ex[which(data_ex$act_code == 6),]$activities <- rep("LAYING", length(which(data_ex$act_code == 6)))

# Dropping useless column (it's useless since we have "real" names of activities)
data_ex <- subset(data_ex, select = -act_code )

# Adding "subject" variable to the data table
# Reading data for train and test sets

subject_train <- read.table("~/Documents/R/getting_and_cleaning_data/data/train/subject_train.txt", quote="\"")
subject_test <- read.table("~/Documents/R/getting_and_cleaning_data/data/test/subject_test.txt", quote="\"")

# Merging
subjects <- as.vector(as.matrix(rbind(subject_train, subject_test)))
data_ex$subjects <- subjects

# 5. Creating new tidy data frame with average for each activity and each subject

library(dplyr)
# We are applying function summarise_each on grouped data, with function mean
output <- summarise_each(group_by(data_ex, activities, subjects),funs(mean))

