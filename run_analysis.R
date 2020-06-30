## This R script does the following:
#1. Merge the training and the test sets to create one data set.
#2. Extract only the measurements on the mean and standard deviation for 
#   each measurement.
#3. Use descriptive activity names to name the activities in the data set
#4. Appropriately label the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy dataset
#   set with the average of each variable for each activity and each subject.

#tidyverse includes dplyr for filtering
library(tidyverse)

# set the working environment (path to test and train directories)
# For example, if test/ and train/ are in the UCI HAR Dataset/ directory, 
# setwd("/path/to/UCI HAR Dataset")
#setwd("/set/working/directory/")
setwd("/Users/swade/Documents/Learning/Online/Coursera/Data Science Foundations using R/3 Getting and Cleaning Data/Week4/Final Project")
#read and combine test set columns
X_test <- read.table("./X_test.txt")
subject_test <- read.table("./subject_test.txt")
y_test <- read.table("./y_test.txt")
temp_test <- cbind(subject_test,y_test)
names(temp_test)[1] <- "subject"
names(temp_test)[2] <- "activity"
testing <- cbind(temp_test, X_test)

#read and combine training set columns
X_train <- read.table("./X_train.txt")
subject_train <- read.table("./subject_train.txt")
y_train <- read.table("./y_train.txt")
temp_train <- cbind(subject_train, y_train)
names(temp_train)[1] <- "subject"
names(temp_train)[2] <- "activity"
training <- cbind(temp_train, X_train)

# combine test and training sets
test_and_train <- rbind(testing, training)

#Extract all columns with 'mean' or 'std' in the name
test_and_train_mean_std <- subset(test_and_train, select=c(subject,activity,V1:V6,V41:V46,V81:V86,V121:V126,V161:V166,V201,V202,V214,V215,V227,V228,V240,V241,V253,V254,V266:V271,V294:V296,V345:V350,V373:V375,V424:V429,V452:V454,V503,V504,V513,V516,V517,V526,V529,V530,V539,V542,V543,V552))

#replace activities 1-6 with their string equivalents
test_and_train_mean_std$activity <- as.character(test_and_train_mean_std$activity)
test_and_train_mean_std$activity[test_and_train_mean_std$activity == "1"] <- "walking"
test_and_train_mean_std$activity[test_and_train_mean_std$activity == "2"] <- "walking_upstairs"
test_and_train_mean_std$activity[test_and_train_mean_std$activity == "3"] <- "walking_downstairs"
test_and_train_mean_std$activity[test_and_train_mean_std$activity == "4"] <- "sitting"
test_and_train_mean_std$activity[test_and_train_mean_std$activity == "5"] <- "standing"
test_and_train_mean_std$activity[test_and_train_mean_std$activity == "6"] <- "laying"

#replace V# column names with meaningful names 
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V1"] <- "mean_time_body_acceleration_X"         
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V2"] <- "mean_time_body_acceleration_Y"         
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V3"] <- "mean_time_body_acceleration_Z"         
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V41"] <- "mean_time_gravity_acceleration_X"     
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V42"] <- "mean_time_gravity_acceleration_Y"     
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V43"] <- "mean_time_gravity_acceleration_Z"     
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V81"] <- "mean_time_body_acceleration_jerk_X"   
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V82"] <- "mean_time_body_acceleration_jerk_Y"   
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V83"] <- "mean_time_body_acceleration_jerk_Z"   
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V121"] <- "mean_time_body_angular_velocity_X"          
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V122"] <- "mean_time_body_angular_velocity_Y"          
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V123"] <- "mean_time_body_angular_velocity_Z"          
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V161"] <- "mean_time_body_angular_velocity_jerk_X"     
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V162"] <- "mean_time_body_angular_velocity_jerk_Y"     
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V163"] <- "mean_time_body_angular_velocity_jerk_Z"     
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V201"] <- "mean_time_body_acceleration_magnitude"  
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V214"] <- "mean_time_gravity_acceleration_magnitude"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V227"] <- "mean_time_body_acceleration_jerk_magnitude"  
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V240"] <- "mean_time_body_angular_velocity_magnitude"             
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V253"] <- "mean_time_body_angular_velocity_jerk_magnitude"        
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V266"] <- "mean_frequency_body_acceleration_X"                
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V267"] <- "mean_frequency_body_acceleration_Y"             
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V268"] <- "mean_frequency_body_acceleration_Z"             
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V294"] <- "mean_of_mean_frequency_body_acceleration_X"     
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V295"] <- "mean_of_mean_frequency_body_acceleration_Y"     
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V296"] <- "mean_of_mean_frequency_body_acceleration_Z"     
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V345"] <- "mean_frequency_body_acceleration_jerk_X"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V346"] <- "mean_frequency_body_acceleration_jerk_Y"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V347"] <- "mean_frequency_body_acceleration_jerk_Z"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V373"] <- "mean_of_mean_frequency_body_acceleration_jerk_X"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V374"] <- "mean_of_mean_frequency_body_acceleration_jerk_Y"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V375"] <- "mean_of_mean_frequency_body_acceleration_jerk_Z"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V424"] <- "mean_frequency_body_angular_velocity_X"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V425"] <- "mean_frequency_body_angular_velocity_Y"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V426"] <- "mean_frequency_body_angular_velocity_Z"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V452"] <- "mean_of_mean_frequency_body_angular_velocity_X"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V453"] <- "mean_of_mean_frequency_body_angular_velocity_Y"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V454"] <- "mean_of_mean_frequency_body_angular_velocity_Z"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V503"] <- "mean_frequency_body_acceleration_magnitude"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V513"] <- "mean_of_mean_frequency_body_acceleration_magnitude"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V516"] <- "mean_frequency_body_body_acceleration_jerk_magnitude"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V526"] <- "mean_of_mean_frequency_body_body_acceleration_jerk_magnitude"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V529"] <- "mean_frequency_body_body_angular_velocity_magnitude"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V539"] <- "mean_of_mean_frequency_body_body_angular_velocity_magnitude"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V542"] <- "mean_frequency_body_body_angular_velocity_jerk_magnitude"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V552"] <- "mean_of_mean_frequency_body_body_angular_velocity_jerk_magnitude"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V4"] <- "std_dev_time_body_acceleration_X"      
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V5"] <- "std_dev_time_body_acceleration_Y"      
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V6"] <- "std_dev_time_body_acceleration_Z"      
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V44"] <- "std_dev_time_gravity_acceleration_X"  
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V45"] <- "std_dev_time_gravity_acceleration_Y"  
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V46"] <- "std_dev_time_gravity_acceleration_Z"  
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V84"] <- "std_dev_body_acceleration_jerk_X"     
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V85"] <- "std_dev_body_acceleration_jerk_Y"     
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V86"] <- "std_dev_body_acceleration_jerk_Z"     
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V124"] <- "std_dev_time_body_angular_velocity_X"       
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V125"] <- "std_dev_time_body_angular_velocity_Y"       
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V126"] <- "std_dev_time_body_angular_velocity_Z"       
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V164"] <- "std_dev_time_body_angular_velocity_jerk_X"  
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V165"] <- "std_dev_time_body_angular_velocity_jerk_Y"  
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V166"] <- "std_dev_time_body_angular_velocity_jerk_Z"  
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V202"] <- "std_dev_time_body_acceleration_magnitude" 
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V215"] <- "std_dev_time_gravity_acceleration_magnitude" 
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V228"] <- "std_dev_time_body_acceleration_jerk_magnitude"  
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V241"] <- "std_dev_time_body_angular_velocity_magnitude"          
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V254"] <- "std_dev_time_body_angular_velocity_jerk_magnitude"     
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V269"] <- "std_dev_frequency_body_acceleration_X"          
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V270"] <- "std_dev_frequency_body_acceleration_Y"          
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V271"] <- "std_dev_frequency_body_acceleration_Z"          
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V348"] <- "std_dev_frequency_body_acceleration_jerk_X"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V349"] <- "std_dev_frequency_body_acceleration_jerk_Y"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V350"] <- "std_dev_frequency_body_acceleration_jerk_Z"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V427"] <- "std_dev_frequency_body_angular_velocity_X"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V428"] <- "std_dev_frequency_body_angular_velocity_Y"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V429"] <- "std_dev_frequency_body_angular_velocity_Z"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V504"] <- "std_dev_frequency_body_acceleration_magnitude"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V517"] <- "std_dev_frequency_body_body_acceleration_jerk_magnitude"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V530"] <- "std_dev_frequency_body_body_angular_velocity_magnitude"
names(test_and_train_mean_std)[names(test_and_train_mean_std) == "V543"] <- "std_dev_frequency_body_body_angular_velocity_jerk_magnitude"

#save off column names
names <- names(test_and_train_mean_std)

#calculate the mean of each variable
#add to a data frame in this format:
#subject    activity             mean_time_body_acceleration_X   mean_time_body_acceleration_Y  mean_time_body_acceleration_Z ...
# 1         walking               average of all vals               average of all vals               average of all vals
# 1         walking_upstairs           "      "                        "            "                    "           "
# ...
# 30        standing                0.2771127                      -0.01701639                  -0.1087562
# 30        laying

#The following code loops through 30 participants with 6 activities each and calculates the mean 
#for every column in the dataset (excluding the columns containing the participant number and activity)
#The code builds a list for each participant:activity containing the mean of each set of numbers 
#and appends the list as a row in a data frame

activities <- c("walking","walking_upstairs","walking_downstairs","sitting", "standing","laying")
l <- vector("list", length=81)
df <- data.frame()
#names declared previously
for (n in names) df[[n]] <- as.numeric()

for (i in 1:30) {
  for (j in 1:length(activities)) {
    #by_subj_activity contains all values for all columns for each subject and activity
    by_subj_activity <- test_and_train_mean_std %>% filter(subject == i & test_and_train_mean_std$activity == activities[j])
    names(l) <- colnames(by_subj_activity)
    for(k in colnames(by_subj_activity)) {
      if (k == "subject") { 
        l[[k]] <- by_subj_activity[2,1]
      } else if (k == "activity") {
        l[[k]] <- by_subj_activity[2,2]
      } else {
        l[[k]] <- mean(by_subj_activity[,k])
      }
      #print(l)
    }
    #after calculating the mean for each user:activity set, add as row to the data frame
    df <- rbind(df,l,stringsAsFactors=FALSE)
  }
}

#write.csv(as.data.frame(df),"./tidydata/Week4tidydataset.csv")
write.table(df,"Week4tidydataset.txt", row.name=FALSE )






