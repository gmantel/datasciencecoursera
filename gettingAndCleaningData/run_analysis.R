
features <- scan("features.txt",sep = "\n",what=character())
features <- sub("^.*[0-9] ","",features)
extractVariables <- grep("std()|mean()",features)
subjecttrain <- read.table("train/subject_train.txt", col.names="subjectID")
subjecttest <- read.table("test/subject_test.txt", col.names="subjectID")
trainingset <- read.table("train/X_train.txt",col.names=features)
testset <- read.table("test/X_test.txt",col.names=features)
actlabels <- read.table("activity_labels.txt",col.names=c("activityID","activityName"))
activitiestest <- read.table("test/y_test.txt",col.names="activityID")
activitiestrain <- read.table("train/y_train.txt",col.names="activityID")
essai <- rbind(cbind(subjecttrain,activitiestrain,trainingset[,extractVariables]),cbind(subjecttest,activitiestest,testset[,extractVariables]))
result <- merge(actlabels,essai)
write.table(essai,"result.txt",row.name=FALSE)
means <- aggregate(result[,3] ~ subjectID + activityID, data = result, FUN = mean)
for (i in 4:ncol(result)){
  means[,i] <- aggregate( result[,i] ~ subjectID + activityID, data = result, FUN = mean )[,3]
}
colnames(means)[4:82]<-features[extractVariables]