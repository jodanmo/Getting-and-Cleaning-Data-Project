#This script creates a tidy data set with the average of each variable for each 
#activity and each subject.

#Read in Data
x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")
subject_train <- read.table("train/subject_train.txt")
x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")
subject_test <- read.table("test/subject_test.txt")
features <- read.table("features.txt")
label <- read.table("activity_labels.txt")

#merge training and  test data
library (dplyr)

subjects <- bind_rows(subject_train, subject_test)
x <- bind_rows(x_train, x_test)
y <- bind_rows(y_train,y_test)

#name columns
valid_column_names <- make.names(names=features[,2], unique=TRUE, allow_ = TRUE)
names(x) <- valid_column_names

#Extracts only the measurements on the mean and standard deviation
measurements <- grepl("mean|std", features[,2], ignore.case=TRUE)

X_mean_std <- select(x,which(measurements))

#rename y table from activity table
activity <- left_join(label, y, by="V1")
activity <- select(activity, activity = V2)

# merge subject, activity, and test/training data
data <- tbl_df(bind_cols(subjects,activity, X_mean_std ))
data <- rename(data, subject=V1)

#group by activity and subject and get means
grouped <- group_by(data, subject, activity)

tidy <- summarise_each(grouped,  funs(mean) )

write.table(tidy, file ="tidy.txt",row.name=FALSE)
