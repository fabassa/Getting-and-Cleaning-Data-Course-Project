#0. Load packages
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)


#1. Download data from web
data.Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
folder.Name <- paste(getwd(), "/data", sep = "")
data.Path <- paste(getwd(), "/data", ".zip", sep = "")

if(!file.exists(data.Path)) {
    download.file(data.Url, data.Path)
}

if(!file.exists("./data")) {
    dir.create("./data")
    unzip(zipfile = data.Path, exdir = "./data")
}


#2. Load activity labels and features
activityLabels <- fread(file.path(folder.Name, "UCI HAR Dataset/activity_labels.txt")
                        , col.names = c("classLabels", "activityName"))
features <- fread(file.path(folder.Name, "UCI HAR Dataset/features.txt")
                  , col.names = c("index", "featureNames"))
featuresWanted <- grep("(mean|std)\\(\\)", features[, featureNames])
measurements <- features[featuresWanted, featureNames]
measurements <- gsub('[()]', '', measurements)

#3. Load train datasets
train <- fread(file.path(folder.Name, "UCI HAR Dataset/train/X_train.txt"))[, featuresWanted, with = FALSE]
data.table::setnames(train, colnames(train), measurements)
trainActivities <- fread(file.path(folder.Name, "UCI HAR Dataset/train/Y_train.txt")
                         , col.names = c("Activity"))
trainSubjects <- fread(file.path(folder.Name, "UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("SubjectNum"))
train <- cbind(trainSubjects, trainActivities, train)

#4. Load test datasets
test <- fread(file.path(folder.Name, "UCI HAR Dataset/test/X_test.txt"))[, featuresWanted, with = FALSE]
data.table::setnames(test, colnames(test), measurements)
testActivities <- fread(file.path(folder.Name, "UCI HAR Dataset/test/Y_test.txt")
                        , col.names = c("Activity"))
testSubjects <- fread(file.path(folder.Name, "UCI HAR Dataset/test/subject_test.txt")
                      , col.names = c("SubjectNum"))
test <- cbind(testSubjects, testActivities, test)

#5. Merge datasets and add labels
combined <- rbind(train, test)

#6. Convert classLabels to activityName basically. More explicit. 
combined[["Activity"]] <- factor(combined[, Activity]
                                 , levels = activityLabels[["classLabels"]]
                                 , labels = activityLabels[["activityName"]])

combined[["SubjectNum"]] <- as.factor(combined[, SubjectNum])
combined <- reshape2::melt(data = combined, id = c("SubjectNum", "Activity"))
combined <- reshape2::dcast(data = combined, SubjectNum + Activity ~ variable, fun.aggregate = mean)

write.table(combined, "./tidy_dataset.txt", row.names = FALSE, quote = FALSE)