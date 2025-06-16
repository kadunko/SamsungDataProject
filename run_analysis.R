#Coursera Project for Getting and Cleaning Smartphones Dataset
# Author: Selvakkadunko Selvaratnam
# Date: June 15, 2025
library(plyr)
library(dplyr)
library(stringr)
#1. Merges the training and the test sets to create one data set.
# Read X_train, X_test, features names;
# Define column names;
# Combine data frames.
X_train <- read.delim("X_train.txt", sep = "", header = FALSE)
X_test <- read.delim("X_test.txt", sep = "", header = FALSE)
features <- read.delim("features.txt", sep = "", header = FALSE)
feature <- features[,2]
Xdata <- rbind(X_train, X_test)
colnames(Xdata) <- feature

#Read y data
# Read y_train, y_test;
# Define column name as Label;
# Combine data frames.
y_train <- read.delim("y_train.txt", sep = "", header = FALSE)
y_test <- read.delim("y_test.txt", sep = "", header = FALSE)
ydata <- rbind(y_train, y_test)
colnames(ydata) <- c("Label")

#Read Subject
# Read subject identifiers;
# Define column name as Subject,
# Combine identifiers.
sub_train <- read.delim("subject_train.txt", sep = "", header = FALSE)
sub_test <- read.delim("subject_test.txt", sep = "", header = FALSE)
subject <- rbind(sub_train, sub_test)
colnames(subject) <- c("Subject")

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# Extract features that contain the word mean;
# Extract features that contain the word std;
# Combine and sort required features;
# Combine columns of features, Subject, and Label.
feat1 <- str_match(feature, "mean")
ind1 <- which(!is.na(feat1[,1]))
feat2 <- str_match(feature, "std")
ind2 <- which(!is.na(feat2[,1]))
ind <- c(ind1, ind2)
ind <- sort(ind)

rfeature <- feature[ind] #Required Features
rdata <- Xdata[,rfeature] #Extract required columns 
reqdata <- cbind(subject, ydata, rdata) #Required Data Set

#3. Uses descriptive activity names to name the activities in the data set
#Recode Label
reqdata$Label <- as.character(reqdata$Label)
reqdata$Label <- recode(reqdata$Label, "1" = "WALKING", 
                        "2" = "WALKING_UPSTAIRS", 
                        "3" = "WALKING_DOWNSTAIRS", "4" = "SITTING", 
                        "5" = "STANDING", "6" = "LAYING")


#4. Appropriately labels the data set with descriptive variable names. 
#It was done in #1.

#5. From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable 
#for each activity and each subject.
# Compute group means
reqdata$Subject <- as.character(reqdata$Subject)
tidyData <- aggregate(. ~ Subject + Label, data = reqdata, FUN = mean)
tidyData <- tidyData[order(tidyData$Subject, tidyData$Label),]

tFeatures <- names(tidyData)
FeaturesV <- tFeatures[c(-1, -2)]

pastefun <- function(V) paste("AveSA", V, sep = "_")
AveSA_FeaturesV <- unlist(lapply(FeaturesV, pastefun))
AveSA_Features <- c(tFeatures[1], tFeatures[2], AveSA_FeaturesV)
colnames(tidyData) <- AveSA_Features

# Write tidy features and tidy data on the folder
write.table(AveSA_Features, file = "tidyFeatures.txt", row.names = FALSE, col.names = FALSE)
write.table(tidyData, file = "tidyData.txt", row.names = FALSE, col.names = FALSE)
