# Step1. Merges the training and the test sets to create one data set.
# setwd("~/Desktop/Online Coursera/Coursera-Getting-and-Cleaning-Data/peer_assessment/")
trainData <- read.table("./data/train/X_train.txt")
#dim(trainData)  7352*561
trainLabel <- read.table("./data/train/y_train.txt")
table(trainLabel)
trainSubject <- read.table("./data/train/subject_train.txt")
testData <- read.table("./data/test/X_test.txt")
#dim(testData) # 2947*561
testLabel <- read.table("./data/test/y_test.txt") 
table(testLabel) 
testSubject <- read.table("./data/test/subject_test.txt")
joinData <- rbind(trainData, testData)
#dim(joinData) # 10299*561
joinLabel <- rbind(trainLabel, testLabel)
#dim(joinLabel) # 10299*1
joinSubject <- rbind(trainSubject, testSubject)
#dim(joinSubject) # 10299*1
#######################################################################
# Step2. Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("./data/features.txt")
dim(features)  # 561*2
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) # 66
joinData <- joinData[, meanStdIndices]
dim(joinData) # 10299*66
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 
###############################################################################
#Step3. Uses descriptive activity names to name the activities in the data set
activity <- read.table("./data/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel<-merge(joinLabel,activity, by="V1")
#table(labelActivity$"V1",labelActivity$"V2")
activity <- activityLabel[,2]
#names(joinActivity) <- "activity" --not needed as its a vector
##############################################################################
# Step4. Appropriately labels the data set with descriptive activity names. 
names(joinSubject) <- "subject"
cleanData <- cbind(joinSubject,activity, joinData)
#dim(cleanData) # 10299*68
#write.table(cleanData, "merged_data.txt") # write out the 1st dataset
#############################################################################
# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject.
aggregateData<-aggregate(.~subject+activity, FUN=mean, data=cleanData)
#table(cleanData$subject,cleanData$activity)
write.table(aggregateData, "aggregateData.txt")