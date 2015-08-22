# Step1 is to merge training and test datasets and create one set.
# setwd("C:/Users/sufiyanansari/Coursera/GettingAndCleaningData/")
# My data files are under folder "UCI HAR Dataset"
trainDataSet <- read.table("./UCI HAR Dataset/train/X_train.txt")
dim(trainDataSet) 
head(trainDataSet)
trainDataSetY <- read.table("./UCI HAR Dataset/train/y_train.txt")
table(trainDataSetY)
trainSubjectDataSet <- read.table("./UCI HAR Dataset/train/subject_train.txt")
testDataSet <- read.table("./UCI HAR Dataset/test/X_test.txt")
dim(testDataSet) 
testDataSetY <- read.table("./UCI HAR Dataset/test/y_test.txt") 
table(testDataSetY) 
testSubjectDataSet <- read.table("./UCI HAR Dataset/test/subject_test.txt")
joinDataSet <- rbind(trainDataSet, testDataSet)
dim(joinDataSet) 
joinLabel <- rbind(trainDataSetY, testDataSetY)
dim(joinLabel) 
joinSubject <- rbind(trainSubjectDataSet, testSubjectDataSet)
dim(joinSubject) 

# Step2. Extracts only the measurements on the mean and standard data
# deviation for each measurement. 
features <- read.table("./UCI HAR Dataset/features.txt")
dim(features) 
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) 
joinDataSet <- joinDataSet[, meanStdIndices]
dim(joinDataSet) 
names(joinDataSet) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # removes "()"
names(joinDataSet) <- gsub("mean", "Mean", names(joinDataSet)) # Upper Case M
names(joinDataSet) <- gsub("std", "Std", names(joinDataSet)) # Upper Case S
names(joinDataSet) <- gsub("-", "", names(joinDataSet)) # remove hyphens "-" from column names 

# Step3. Uses descriptive activity names to name the activities in 
# the data set
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity 
# names. 
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinDataSet)
dim(cleanedData) 
write.table(cleanedData, "MergedDataSet.txt") # write 1st Dataset to File 

# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
subjectLen <- length(table(joinSubject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
head(result)

write.table(result,"DataSet_means.txt",row.names = FALSE) # write 2nd Dataset to File 
