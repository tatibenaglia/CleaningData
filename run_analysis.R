## You should create one R script called run_analysis.R that does the following.

## 1. Merges the training and the test sets to create one data set. 

## 2. Extracts only the measurements on the mean and standard deviation for each
## measurement. 

## 3. Uses descriptive activity names to name the activities in the
## data set 

## 4. Appropriately labels the data set with descriptive variable 
## names. 

## 5. From the data set in step 4, creates a second, independent tidy 
## data set with the average of each variable for each activity and each 
## subject.


## Downloading the data
if(!any(file.exists("./getdata-projectfiles-UCI HAR Dataset.zip", "./dataset.zip")))
    {
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(url=fileURL, destfile = "./dataset.zip", method = "curl")
    
    ## Unzip the downloaded data
    unzip("./dataset.zip")
}

## Reading training data
setwd("./UCI HAR Dataset/train")
trainfiles <- list.files(pattern="\\.txt$", recursive = FALSE)

training <- do.call(cbind, 
                    lapply(trainfiles[c(1, 3, 2)], read.table, header=FALSE))


## Reading test data
setwd("../test")
testfiles <- list.files(pattern="\\.txt$", recursive = FALSE)

test <- do.call(cbind,
                lapply(testfiles[c(1, 3, 2)], read.table, header=FALSE))

## Reading the name of the variables
varnames <- read.table("../features.txt", header=FALSE, 
                       colClasses = c("integer", "character"))[,2]


## 1. Merging training and test datasets
data <- rbind(training, test)
colnames(data) <- c("Subject", "Activity", varnames)
data <- data[ , !duplicated(colnames(data))]


## 2. Extracts only the measurements on the mean and standard deviation for each
## measurement
library(dplyr)
data <- tbl_df(data)

dataselected <- data %>%
    select(matches("(mean|std)\\(\\)"))


col_index <- grep("(mean|std)\\(\\)", colnames(data))
dataselected <- data[, c(1, 2, col_index)]



## 3. Uses descriptive activity names to name the activities in the data set
library(dplyr)
dataselected <- tbl_df(dataselected)
activity_labels <- tolower(read.table("../activity_labels.txt", header=FALSE)[, 2])

final_data <- dataselected %>%
    mutate(Activity = activity_labels[Activity])


## 4. Appropriately labels the data set with descriptive variable names. 
names(final_data) <- sub("()", "", names(final_data), fixed=TRUE)


## 5. From the data set in step 4, creates a second, independent tidy 
## data set with the average of each variable for each activity and each 
## subject.
tidy_data <- final_data %>% 
    group_by(Subject, Activity) %>% 
    summarise_each(funs(mean))

## write.table(tidy_data, "../../tidy_data.txt", sep=" ", row.name=FALSE) 