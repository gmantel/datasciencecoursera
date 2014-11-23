# reading feature names and trimming numeric headers
features <- scan("features.txt",sep = "\n",what=character())
features <- sub("^.*[0-9] ","",features)
# construct a vector with features to retain
extractVariables <- grep("std()|mean()",features)
# getting subject data
subjecttrain <- read.table("train/subject_train.txt", col.names="subjectID")
subjecttest <- read.table("test/subject_test.txt", col.names="subjectID")
# getting observation data
trainingset <- read.table("train/X_train.txt",col.names=features)
testset <- read.table("test/X_test.txt",col.names=features)
# getting activity data
actlabels <- read.table("activity_labels.txt",col.names=c("activityID","activityName"))
activitiestest <- read.table("test/y_test.txt",col.names="activityID")
activitiestrain <- read.table("train/y_train.txt",col.names="activityID")
# binding the data together while selecting only the proper features
essai <- rbind(cbind(subjecttrain,activitiestrain,trainingset[,extractVariables]),cbind(subjecttest,activitiestest,testset[,extractVariables]))
# adding the activity names by merging
result <- merge(actlabels,essai)
# creating another variable to compute the mean by aggregating by subject and activity
means <- aggregate(result[,3] ~ subjectID + activityID, data = result, FUN = mean)
# looping over the retained features to compute the mean
for (i in 4:ncol(result)){
  means[,i] <- aggregate( result[,i] ~ subjectID + activityID, data = result, FUN = mean )[,3]
}
# setting the column names properly
colnames(means)[4:82]<-features[extractVariables]
means=select(means,-3)
write.table(means,"result.txt",row.name=FALSE)