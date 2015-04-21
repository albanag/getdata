library(reshape2)
library(dplyr)

get_obs_table <- function() {
                            # read test data
                            test_tab = read.table("UCI\ HAR\ Dataset/test/X_test.txt", header=FALSE, sep="")
                            dim(test_tab)
                            # read train data
                            train_tab = read.table("UCI\ HAR\ Dataset/train/X_train.txt", header=FALSE, sep="")
                            dim(train_tab)                            
                            # merge train and test data
                            obs_table = rbind(test_tab, train_tab)
                            # read features
                            features = read.table("UCI\ HAR\ Dataset/features.txt", header=FALSE, sep="")
                            # extract only the measurements concerning the mean and standard deviation for each measurement.
                            indexes = which(grepl( "*mean\\(\\)*|*std\\(\\)*", features$V2))
                            sub_obs_table = select(obs_table, indexes)
                            # get the descriptions of the measurements
                            descriptions = features[which(grepl( "*mean\\(\\)*|*std\\(\\)*", features$V2)),][2]
                            names(sub_obs_table) = descriptions$V2
                            return(sub_obs_table)            
}

get_activities_table <- function() {                            
                            # read test data
                            test_activity_tab = read.table("UCI\ HAR\ Dataset/test/y_test.txt", header=FALSE, sep="")
                            dim(test_activity_tab)
                            # read train data
                            train_activity_tab = read.table("UCI\ HAR\ Dataset/train/y_train.txt", header=FALSE, sep="")
                            dim(train_activity_tab)    
                            # merge train and test data
                            activities_table = rbind(test_activity_tab, train_activity_tab)
                            # read the activity labels
                            activity_labels = read.table("UCI\ HAR\ Dataset/activity_labels.txt", header=FALSE, sep="")
                            # join activity_labels with the activity table to assign a label to each activity code
                            complete_activity = merge(activities_table, activity_labels, by="V1")
                            # name the columns
                            names(complete_activity) = c("ActivityCode", "ActivityDescription")
                            return(complete_activity)
}

get_subjects_table <- function() {
                            # read test data
                            test_subj_tab = read.table("UCI\ HAR\ Dataset/test/subject_test.txt", header=FALSE, sep="")
                            dim(test_subj_tab)
                            # read train data
                            train_subj_tab = read.table("UCI\ HAR\ Dataset/train/subject_train.txt", header=FALSE, sep="")
                            dim(train_subj_tab)
                            # merge train and test data 
                            subjects_table = rbind(test_subj_tab, train_subj_tab)
                            # name the subject table
                            names(subjects_table) = c("SubjectID")
                            return(subjects_table)
}

observations_table = get_obs_table()
activities_table = get_activities_table()
subjects_table = get_subjects_table()

# bind by column the three tables
complete_table = cbind(subjects_table, activities_table, observations_table)
head(complete_table)

# reshape to transform the table into a long one
long_table <- melt(complete_table, id.vars=c("SubjectID", "ActivityCode","ActivityDescription"), variable.name="Measurement")
head(long_table)

# grouping the observations by subject, activity and measurement
grouped = group_by(long_table, SubjectID, ActivityCode, ActivityDescription, Measurement)
head(grouped)

# compute the mean of the each measurement
final = summarise(grouped, meanVal=mean(value))
head(final)

# write to file
write.table(final, file="tidy_data.txt", row.names=FALSE)
print(final)
