#Mary Rose G.Liguan
#BS in Statistics-3
#CMSC197-3
#SecondMiniProjectInR(run_analysis)


setwd("C:/Users/ASUS/Desktop/specdata/UCI HAR Dataset")
Files<-list.files("C:/Users/ASUS/Desktop/specdata/UCI HAR Dataset", recursive=T, full.names=T, all.files=T)


#Merges the training and the test sets to create one data set

testActivityData  <- read.table(file.path("C:/Users/ASUS/Desktop/specdata/UCI HAR Dataset", "test" , "Y_test.txt" ),header = FALSE)
trainActivityData <- read.table(file.path("C:/Users/ASUS/Desktop/specdata/UCI HAR Dataset", "train", "Y_train.txt"),header = FALSE)
testSubjectData  <- read.table(file.path("C:/Users/ASUS/Desktop/specdata/UCI HAR Dataset", "test" , "subject_test.txt"),header = FALSE)
trainSubjectData <- read.table(file.path("C:/Users/ASUS/Desktop/specdata/UCI HAR Dataset", "train", "subject_train.txt"),header = FALSE)
testFeaturesData  <- read.table(file.path("C:/Users/ASUS/Desktop/specdata/UCI HAR Dataset", "test" , "X_test.txt" ),header = FALSE)
trainFeaturesData <- read.table(file.path("C:/Users/ASUS/Desktop/specdata/UCI HAR Dataset", "train", "X_train.txt"),header = FALSE)

Activity<- rbind(testActivityData, trainActivityData)
Subject <- rbind(testSubjectData, trainSubjectData)
Features<- rbind(testFeaturesData, trainFeaturesData)

readFeaturesData <- read.table(file.path("C:/Users/ASUS/Desktop/specdata/UCI HAR Dataset", "features.txt"),head=FALSE)
names(Subject)<-c("Subject")
names(Activity)<- c("Activity")
names(Features)<- readFeaturesData$V2


SubActMergedData <- cbind(Subject, Activity)

MergedData <- cbind(Features, SubActMergedData)

str(MergedData)

# Extracts only the measurements on the mean and standard deviation for each measurement

FeaturesData2<-readFeaturesData$V2[grep("mean\\(\\)|std\\(\\)", readFeaturesData$V2)]

FeatData2Char<-c(as.character(FeaturesData2), "Subject", "Activity" )

MergedData<-subset(MergedData,select=FeatData2Char)

str(MergedData)

#Uses descriptive activity names to name the activities in the dataset
ActivityDesc <- read.table(file.path(getwd(), "activity_labels.txt"),header = FALSE)

MergedData$Activity <- as.character(MergedData$Activity)
for (i in 1:6){
      MergedData$Activity[MergedData$Activity == i] <- as.character(ActivityDesc[i,2])
   }
MergedData$Activity <- as.factor(MergedData$Activity)

head(MergedData$Activity,30)

#Appropriately labels the data set with descriptive variable names
names(MergedData)<-gsub("^t", "time", names(MergedData))
names(MergedData)<-gsub("^f", "frequency", names(MergedData))
names(MergedData)<-gsub("Acc", "Accelerometer", names(MergedData))
names(MergedData)<-gsub("Gyro", "Gyroscope", names(MergedData))
names(MergedData)<-gsub("Mag", "Magnitude", names(MergedData))
names(MergedData)<-gsub("BodyBody", "Body", names(MergedData)) 
names(MergedData)

#From the data set in step 4, create a second, independent tidy data set with the average of each variable
on each activity and each subject

install.packages("plyr")
library(plyr)
IndData<-aggregate(. ~Subject + Activity, MergedData, mean)
IndData<-IndData[order(IndData$Subject,IndData$Activity),]
write.table(IndData, file = "tidydata.txt",row.name=FALSE)
