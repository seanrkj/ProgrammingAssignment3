

## Coursera Getting and Cleaning Data Course Project - SeanJ

# This script will perform the following steps:
# 1. Merge the training and the test sets.

# 2. Pull only measurements on the mean and SD. 

# 3. Name the activities.

# 4. Label the data set. 

# 5. Create a tidy data set.
##############

# Read the data from files
features = read.table('./features.txt',header=FALSE); 

activityType = read.table('./activity_labels.txt',header=FALSE); 

SubTrain = read.table('./train/subject_train.txt',header=FALSE); 

X_Train = read.table('./train/x_train.txt',header=FALSE); 

Y_Train = read.table('./train/y_train.txt',header=FALSE); 

# 1. Merge the training and the test sets.

ColumnNames(activityType) = c('activityId','activityType');
ColumnNames(SubTrain) = "subjectId";
ColumnNames(X_Train) = features[,2]; 
ColumnNames(Y_Train) = "activityId";

# Create the final training set
TrainData = cbind(Y_Train,SubTrain,X_Train);

# Read and assign names to the test data
SubTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
X_Test = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
Y_Test = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt
ColumnNames(SubTest) = "subjectId";
ColumnNames(X_Test) = features[,2]; 
ColumnNames(Y_Test) = "activityId";


# Merging
TestData = cbind(Y_Test,SubTest,X_Test);

FinalData = rbind(TrainData,TestData);

# Create a vector for the column names from the FinalData, which will be used
# to select the desired mean() & stddev() columns
ColumnNames  = ColumnNames(FinalData); 


# 2. Pull only measurements on the mean and SD. 

LogVector = (grepl("activity..",ColumnNames) | grepl("subject..",ColumnNames) | grepl("-mean..",ColumnNames) & !grepl("-meanFreq..",ColumnNames) & !grepl("mean..-",ColumnNames) | grepl("-std..",ColumnNames) & !grepl("-std()..-",ColumnNames));


FinalData = FinalData[LogVector==TRUE];


# 3. Name the activities.

FinalData = merge(FinalData,activityType,by='activityId',all.x=TRUE);


ColumnNames = ColumnNames(FinalData); 

# 4. Label the data set. 

# Cleaning up the variable names
for (i in 1:length(ColumnNames)) 
{
  ColumnNames[i] = gsub("\\()","",ColumnNames[i])
  ColumnNames[i] = gsub("-std$","StdDev",ColumnNames[i])
  ColumnNames[i] = gsub("-mean","Mean",ColumnNames[i])
  ColumnNames[i] = gsub("^(t)","time",ColumnNames[i])
  ColumnNames[i] = gsub("^(f)","freq",ColumnNames[i])
  ColumnNames[i] = gsub("([Gg]ravity)","Gravity",ColumnNames[i])
  ColumnNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",ColumnNames[i])
  ColumnNames[i] = gsub("[Gg]yro","Gyro",ColumnNames[i])
  ColumnNames[i] = gsub("AccMag","AccMagnitude",ColumnNames[i])
  ColumnNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",ColumnNames[i])
  ColumnNames[i] = gsub("JerkMag","JerkMagnitude",ColumnNames[i])
  ColumnNames[i] = gsub("GyroMag","GyroMagnitude",ColumnNames[i])
};


ColumnNames(FinalData) = ColumnNames;

# 5. Create a tidy data set.

FinalDataNoActivityType = FinalData[,names(FinalData) != 'activityType'];


TidyData = aggregate(FinalDataNoActivityType[,names(FinalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=FinalDataNoActivityType$activityId,subjectId = FinalDataNoActivityType$subjectId),mean);


TidyData = merge(TidyData,activityType,by='activityId',all.x=TRUE);


write.table(TidyData, './TidyData.txt',row.names=TRUE,sep='\t');
