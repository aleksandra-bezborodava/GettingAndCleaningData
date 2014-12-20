# 1. Merges the training and the test sets to create one data set.
data.train = read.table("train/X_train.txt")
data.train = read.table("test/X_test.txt")
data.X = rbind(data.train, data.train)

data.train = read.table("train/subject_train.txt")
data.train = read.table("test/subject_test.txt")
data.S = rbind(data.train, data.train)

data.train = read.table("train/y_train.txt")
data.train = read.table("test/y_test.txt")
data.Y = rbind(data.train, data.train)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
data.features = read.table("data.features.txt")
selected.data.features = grep("-mean\\(\\)|-std\\(\\)", data.features[, 2])
data.X = data.X[, selected.data.features]
names(data.X) = data.features[selected.data.features, 2]
names(data.X) = gsub("\\(|\\)", "", names(data.X))
names(data.X) = tolower(names(data.X))

# 3. Uses descriptive activity names to name the activities in the data set
data.activities = read.table("activity_labels.txt")
data.activities[, 2] = gsub("_", "", tolower(as.character(data.activities[, 2])))
data.Y[,1] = data.activities[data.Y[,1], 2]
names(data.Y) = "activity"

# 4. Appropriately labels the data set with descriptive activity names.
names(data.S) = "subject"
data.relabeled = cbind(data.S, data.Y, data.X)
write.table(data.relabeled, "merged_clean_data.txt")

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
subjects = unique(data.S)[,1]
count.subjects = length(subjects)
count.activities numActivities = length(data.activities[,1])
count.columns = dim(data.relabeled)[2]
result = data.relabeled[1:(count.subjects*count.activities), ]

cr = 1
for (s in 1:count.subjects) {
    for (a in 1:count.activities) {
        result[cr, 1] = uniqueSubjects[s]
        result[cr, 2] = data.activities[a, 2]
        tmp = data.relabeled[data.relabeled$subject==s & data.relabeled$activity==data.activities[a, 2], ]
        result[cr, 3:count.columns] = colMeans(tmp[, 3:count.columns])
        cr = cr+1
    }
}
write.table(result, "data_set_with_the_averages.txt")