## merge the train and test data
testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")
testX <- read.table("UCI HAR Dataset/test/X_test.txt")
testY <- read.table("UCI HAR Dataset/test/Y_test.txt")

trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
trainX <- read.table("UCI HAR Dataset/train/X_train.txt")
trainY <- read.table("UCI HAR Dataset/train/Y_train.txt")


testData <- cbind(testSubject, testY, testX)
trainData <- cbind(trainSubject, trainY, trainX)

data <- rbind(trainData, testData)

## Appropriately labels the data set with descriptive variable names. 
features <- read.table("UCI HAR Dataset/features.txt")
measureNames <- as.character(features$V2)
colnames(data) <- c("subject", "activity", measureNames)


## extract the mean and standard deviation
measureMean <- c()
measureSD <- c()

for (i in 3:ncol(data)){
    measureMean[i-2] <- mean(data[,i])
    measureSD[i-2] <- sd(data[,i])
}


## Uses descriptive activity names to name the activities in the data set
data$activity[data$activity == 1] <- "WALKING"
data$activity[data$activity == 2] <- "WALKING_UPSTAIRS"
data$activity[data$activity == 3] <- "WALKING_DOWNSTAIRS"
data$activity[data$activity == 4] <- "SITTING"
data$activity[data$activity == 5] <- "STANDING"
data$activity[data$activity == 6] <- "LAYING"


## Creates a second, independent tidy data set with the average 
## of each variable for each activity and each subject. 

labelData <- read.table("UCI HAR Dataset/activity_labels.txt")
label <- labelData$V2

newData <- data.frame(matrix(NA, ncol = 563))
colnames(newData) = c("subject", "activity", measureNames)
entry <- newData
tmpData0 <- NULL
tmpData <- NULL

for (i in 1:30){
    for (j in 1:length(label)){
        tmpData0 <- data[data$subject == i, ]
        tmpData <- tmpData0[tmpData0$activity == label[j], ]
        entry$subject[1] <- i
        entry$activity[1] <- as.character(label[j])
        for (k in 3:563){
            average <- mean(tmpData[,k])
            entry[,k] <- average
        }
        newData <- rbind(newData, entry)
    }
}
newData <- na.omit(newData)

write.csv(newData, file = "newTidyData.csv")


