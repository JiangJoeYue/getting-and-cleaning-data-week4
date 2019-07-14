#Merges the training and the test sets to create one data set.
trainset_raw <- read.table('D:/dataset/UCI Dataset/train/X_train.txt')
train_activity <- read.csv('D:/dataset/UCI Dataset/train/y_train.txt', header = FALSE, sep = ' ')
train_subject <- read.csv('D:/dataset/UCI Dataset/train/subject_train.txt',header = FALSE, sep = ' ')
features <- read.csv('D:/dataset/UCI Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,2])
train_set <-  data.frame(train_subject, train_activity, trainset_raw)
names(train_set) <- c(c('subject', 'activity'), features)
testset_raw <- read.table('D:/dataset/UCI Dataset/test/X_test.txt')
test_activity <- read.csv('D:/dataset/UCI Dataset/test/y_test.txt', header = FALSE, sep = ' ')
test_subject <- read.csv('D:/dataset/UCI Dataset/test/subject_test.txt', header = FALSE, sep = ' ')
test_set <-  data.frame(test_subject, test_activity, testset_raw)
names(test_set) <- c(c('subject', 'activity'), features)
dataset <- rbind(train_set, test_set)

#Extracts only the measurements on the mean and standard deviation for each measurement.
mean_std <- grep('mean|std', features)
data_extracted <- dataset[,c(1,2,mean_std.select + 2)]

#Uses descriptive activity names to name the activities in the data set
labels <- read.table('D:/dataset/UCI Dataset/activity_labels.txt', header = FALSE)
labels <- as.character(labels[,2])
data_extracted$activity <- labels[data_extracted$activity]

#Appropriately labels the data set with descriptive variable names.
new <- names(data_extracted)
new <- gsub("[(][)]", "", new)
new <- gsub("^t", "TimeDomain_", new)
new <- gsub("^f", "FrequencyDomain_", new)
new <- gsub("Acc", "Accelerometer", new)
new <- gsub("Gyro", "Gyroscope", new)
new <- gsub("Mag", "Magnitude", new)
new <- gsub("-mean-", "_Mean_", new)
new <- gsub("-std-", "_StandardDeviation_", new)
new <- gsub("-", "_", new)
names(data_extracted) <- new

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
data_tidy <- aggregate(data_extracted[,3:81], by = list(activity = data_extracted$activity, subject = data_extracted$subject),FUN = mean)
write.table(x = data_tidy, file = "D:/dataset/data_tidy.txt", row.names = FALSE)