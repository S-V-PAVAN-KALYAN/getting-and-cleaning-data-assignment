# Load data.table library
library(data.table)
library(dplyr)
# Set name of datafolder
datafolder <- "UCI HAR Dataset"
inertialsignals <- F
# Function to read the files
readfile <- function(...) {
    fread(file.path(datafolder, ...))
}
# Read Activity labels and Features
activity_labels <- readfile("activity_labels.txt")
features <- readfile("features.txt")
# Read Subject test, X test and y test
subject_test <- readfile("test", "subject_test.txt")
X_test <- readfile("test", "X_test.txt")
y_test <- readfile("test", "y_test.txt")
# Read Subject train, X train and y train
subject_train <- readfile("train", "subject_train.txt")
X_train <- readfile("train", "X_train.txt")
y_train <- readfile("train", "y_train.txt")
# Create the data.table
dt <- rbind(X_test, X_train)
rm(X_test, X_train)
# Filter columns with mean() and std()
features <- unlist(features[,2])
mscols <- grep("mean\\(\\)|std\\(\\)", features)
dt <- select(dt, mscols)
# Add names of columns in X_test
colnames(dt) <- features[mscols]
rm(features)
# Add subject, activity and variable type to data.table
dt[,Subject := rbind(subject_test, subject_train)]
dt[,Activity := activity_labels[[2]][unlist(rbind(y_test, y_train))]]
dt[,VariableType := c(rep_len("test", dim(subject_test)[1]), rep_len("train", dim(subject_train)[1]))]
rm(y_test, y_train, subject_test, subject_train)
if (inertialsignals) {
    ncolIS <- 128
    placeholderstr <- "~PLACEH~"
#Function to extract the file names of the Inertial Signals folders
    getfilebases <- function(subdir) {
        unlist(lapply(list.files(file.path(datafolder, subdir, "Inertial Signals")),
               function(x) {sub("test|train", placeholderstr, x)}))
    }
# Check both Inertial Signals folders to make sure they contain the same file names
    filebase <- getfilebases("test")
    if (!setequal(filebase, getfilebases("train"))) {
        stop("The \"Inertial Signals\" files for test and train do not match.")
    }
    # Function to read Inertial Signals files
    readIS <- function(subdir, thiscolname) {
        fread(file.path(datafolder, subdir, "Inertial Signals", sub(placeholderstr, subdir, thiscolname)))
    }
    # Loop over Inertial Signals files and add them to the data.table
    for (filename in filebase) {
        thiscolname <- strsplit(filename, paste(".", placeholderstr, sep = ""))[[1]][1]
        dt[,paste(rep(thiscolname, ncolIS), 1:ncolIS, sep = ".") :=
               rbind(readIS("test", filename), readIS("train", filename))]
    }
}
# Aggregate means of variables over Activity and Subject
sdt <- aggregate(dt[, 1:(dim(dt)[2] - 3)], list(dt$Subject, dt$Activity), mean)
colnames(sdt)[c(1,2)] <- c("Subject", "Activity")
# Write data to file
write.table(dt, "tidydata.txt", row.names = F)
write.table(sdt, "summarytidydata.txt", row.names = F)
# Write feature names to .md file:
writeLines(unlist(lapply(names(dt), function(x) {paste("- ", x)})), "features.md")
