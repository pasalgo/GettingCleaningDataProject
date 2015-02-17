#load de required librarys
library(data.table)
library(dplyr)

#here you can specify your data in the hard drive
testInX<-"UCI HAR Dataset/test/X_test.txt"
trainInX<-"UCI HAR Dataset/train/X_train.txt"
testInY<-"UCI HAR Dataset/test/y_test.txt"
trainInY<-"UCI HAR Dataset/train/y_train.txt"
featuresIn<-"UCI HAR Dataset/features.txt"

#read the features names and save them in variable features
features<-read.table(featuresIn,sep=" ")

#read the train and test datasets into train and test data.table objects
train<-data.table(read.table(trainInX,col.names = features[,2]))
test<-data.table(read.table(testInX,col.names = features[,2]))

#read the train and test activity datasets into trainy and testy 
trainy<-read.table(trainInY)
testy<-read.table(testInY)

#add the activity datasets as a new column in train and test called "activity"
train[,Activity:=trainy]
test[,Activity:=testy]

#merge the train and test data into a large dataset with 10299 rows in a table called tabla
tabla<-rbind(train,test)

#extract the features positions related with means() and std() values
meansActivity<-grep("mean()",features[,2],fixed=TRUE)
stdActivity<-grep("std()",features[,2],fixed=TRUE)

#create a new table with only the columns realted to the means, stdevs and activity
tablamean<-select(tabla,meansActivity,stdActivity,Activity)

#rename the values of activity with the correspondings activity names i.e.:
#1 WALKING
#2 WALKING_UPSTAIRS
#3 WALKING_DOWNSTAIRS
#4 SITTING
#5 STANDING
#6 LAYING
tableAct <- tablamean %>% 
    mutate(Activity = factor(Activity, levels=c(1:6),
                             labels = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")))


#rename tableAct names in order to remove "."
names(tableAct)<-gsub(".","",names(tableAct),fixed = TRUE)
#rename tableAct names in order to replace prefix t by time
names(tableAct)<-gsub("tBody","timeBody",names(tableAct),fixed = TRUE)
names(tableAct)<-gsub("tGravity","timeGravity",names(tableAct),fixed = TRUE)
#rename tableAct names in order to replace prefix f by FFT
names(tableAct)<-gsub("fBody","fftBody",names(tableAct),fixed = TRUE)

#group by Activity
actGroup<-group_by(tableAct,Activity)

#average the values of each column grouping by activity
meanval<-summarise_each(actGroup,funs(mean))

#write the results in a text file
write.table(meanval,"table_out.txt",row.name=FALSE)

