setwd("C:/Users/kempj2/Documents/Documents/Nationwide/coursera/DataScientist/Cleaning_Data/UCI HAR Dataset")

library("stringr")
library("dplyr")
library("data.table")
library("reshape2")

#download files#
fileUrl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
d <-"Dataset.zip"
if (!file.exists("./data")) {dir.create("./data")}
download.file(fileUrl,file.path("./data",d))

#Unzip files#
unzip("./data/Dataset.zip")

#set reusable input path#
pathmain <- file.path(getwd())
#list.files(pathmain, recursive = TRUE)

#pull in the files
dtsubject_train <- data.table(read.table(file.path(pathmain, "train", "subject_train.txt")))
dtsubject_test <- data.table(read.table(file.path(pathmain, "test", "subject_test.txt")))
dtfeatures <- data.table(read.table(file.path(pathmain, "features.txt")))
dtactivities <- data.table(read.table(file.path(pathmain, "activity_labels.txt")))
dtx_test <- data.table(read.table(file.path(pathmain, "test", "X_test.txt")))
dty_test <- data.table(read.table(file.path(pathmain, "test", "Y_test.txt")))
dtx_train <- data.table(read.table(file.path(pathmain, "train", "X_train.txt")))
dty_train <- data.table(read.table(file.path(pathmain, "train", "y_train.txt")))

# Set up the merge
dtsub_all <- rbind(dtsubject_test, dtsubject_train)
dtact_all <- rbind(dty_test, dty_train)
dtsub_act_all <- cbind(dtsub_all, dtact_all)
names(dtsub_act_all) <- c('Subject', 'ActivityNumber')
dt <- rbind(dtx_test, dtx_train)
dt_all <- cbind(dtsub_act_all, dt)
setkey(dt_all,Subject,ActivityNumber)

# mean and std for each measurement
names(dtfeatures) <- c('FeatureNumber', 'FeatureName')
dtfeatures2 <- dtfeatures[grepl("mean|std", FeatureName, ignore.case = TRUE)]
dtfeatures2$FeatureLable <- paste('V', dtfeatures2$FeatureNumber, sep = "")
#table(grepl("mean|std", dtfeatures$FeatureName,ignore.case = TRUE))

dt_allfiltered <- dt_all[,c(key(dt_all), dtfeatures2$FeatureLable), with=F]

# add in better names
setnames(dt_allfiltered, old = dtfeatures2$FeatureLable, new = as.character(dtfeatures2$FeatureName))
names(dtactivities) <- c('ActivityNumber', 'ActivityName')
dt_allfiltered_activity <- merge(dtactivities, dt_allfiltered, by='ActivityNumber')
setkey(dt_allfiltered_activity, ActivityName, ActivityNumber, Subject)

# better labels

dt_allfiltered_activity_melt <- data.table(melt(dt_allfiltered_activity, key(dt_allfiltered_activity), variable.name = "featurecode"))
dt_allfiltered_activity_melt$Activity < - factor(dt_allfiltered_activity_melt$ActivityName)
dt_allfiltered_activity_melt$Feature <- factor(dt_allfiltered_activity_melt$featurecode)

# second tidy set

dt_tidy <- dt_allfiltered_activity_melt %>% group_by(ActivityName, Subject, featurecode) %>% summarise_all(mean)
dt_tidy$Feature <- NULL
dt_tidy$FeatureDomain <- NULL
dt_tidy$ActivityNumber <- NULL
dt_tidy <- dcast(dt_tidy, dt_tidy$ActivityName+dt_tidy$Subject ~ dt_tidy$featurecode, mean)

write.table(dt_tidy, file.path(pathmain, 'tidy.txt'), row.names = FALSE)
save(dt_tidy, file = paste0(getwd(), "/tidy.RData"))

