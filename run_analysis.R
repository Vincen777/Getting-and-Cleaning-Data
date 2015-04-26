## Read data sets 
testData <- read.table("test/X_test.txt")
trainData <- read.table("train/X_train.txt")
X <- rbind(testData, trainData)
rm(testData)
rm(trainData)

## Read subject_test and subject_train
testSub <- read.table("test/subject_test.txt")
trainSub <- read.table("train/subject_train.txt")
S <- rbind(testSub, trainSub)
rm(testSub)
rm(trainSub)

testLabel <- read.table("test/y_test.txt")
trainLabel <- read.table("train/y_train.txt")
Y <- rbind(testLabel, trainLabel)
rm(testLabel)
rm(trainLabel)

featuresList <- read.table("features.txt", stringsAsFactors=FALSE)
features <- featuresList$V2

keepColumns <- grepl("(std|mean[^F])", features, perl=TRUE)
X <- X[, keepColumns]
names(X) <- features[keepColumns]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

## Read ActivityList 
activities <- read.table("activity_labels.txt")
activities[,2] = gsub("_", "", tolower(as.character(activities[,2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity" ## Add activity label

## Set Appropriately labels the data set with descriptive variable names. 
names(S) <- "subject"
tidyData <- cbind(S, Y, X)
write.table(tidyData, "tidyData.txt")

## Creates a second, independent tidy data set with the average of each variable for each activity and each subject
uS = unique(S)[,1]
nS = length(uS)
nA = length(activities[,1])
nC = length(names(tidyData))
td = tidyData[ 1:(nS*nA), ]

row = 1
for (s in 1:nS) {
	for (a in 1:nA) {
		td[row,1] = uS[s]
		td[row,2] = activities[a, 2]
		tmp <- tidyData[tidyData$subject==s & tidyData$activity==activities[a,2],]
		td[row, 3:nC] <- colMeans(tmp[, 3:nC])
		row = row + 1
	}
}

write.table(td, "tidyData2.txt")
