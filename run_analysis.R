# It is important to source data.table package
run_analysis <-function(){

#Read feature set

    #run_analysis script and the features.txt both are present in same folder.    
    
    features <- data.table(read.table("features.txt", sep = ""))
    featurenames <- features$V2
    
    
# Read all the test data sets
     
    #Read the test data set values.
    
    X_test <- data.table(read.table("./test/X_test.txt", sep = "", colClasses = c("numeric")))
    
    #Modify the column names as per feature names.
    #Appropriately labels the data set with descriptive variable names.
    
    X_test <- setNames( X_test , as.character(featurenames))
    
    #Read training labels
    y_test <- data.table(read.table("./test/Y_test.txt", sep = "", colClasses = c("numeric")))
    
    #Read Subject identifiers
    subject_test <- data.table(read.table("./test/subject_test.txt", sep = ""))
    
# Read all the train data sets
    
    X_train <- data.table(read.table("./train/X_train.txt", sep = "", colClasses = c("numeric")))
    
    #Read training labels
    y_train <- data.table(read.table("./train/Y_train.txt", sep = "", colClasses = c("numeric")))
    
    #Read Subject identifiers
    subject_train <- data.table(read.table("./train/subject_train.txt", sep = ""))
    
#Merging the X, Y and Subject List
    
    Xmerged <- rbindlist(list(X_test,X_train))
    ymerged <- rbindlist(list(y_test, y_train))
    subjectMerged <- rbindlist(list(subject_test,subject_train))
    names(subjectMerged) <- c("SubjectID")
    
#Attach descriptive activity names to name the activities in the data set    
    activityFactor <- factor(ymerged$V1, levels = c(1,2,3,4,5,6), labels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS","SITTING", "STANDING", "LAYING"))
    ymerged$activityname <- activityFactor
    names(ymerged) <- c("activity", "activityname")
    

#Extracts only the measurements on the mean and standard deviation for each measurement
    meanandSD <- Xmerged[, 1:6]
    
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
#for each activity and each subject      
    
    average <- cbind(Xmerged, ymerged)
    average <- cbind(average, subjectMerged)
    averageGroup <- average[,lapply(.SD, mean(na.rm=TRUE)),by=c("SubjectID","activityname")]
    
   
    write.table(averageGroup, "averagegroup.txt", row.names = FALSE)
    write.table(Xmerged, "Xmerged.txt", row.names = FALSE)
    write.table(ymerged, "ymerged.txt", row.names = FALSE)
    write.table(subjectMerged, "subjectmerged.txt", row.names = FALSE)
}