library(dplyr)

# read in the data
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "HumanActivity.zip")
unzip("HumanActivity.zip")

# get the column names
# fix them: seems that those given have characters not acceptable for column names in R
col_names <- read.table("UCI HAR Dataset/features.txt",header = FALSE)
col_names <- gsub("\\(\\)","", col_names$V2)
col_names <- gsub("\\-","_", col_names)
col_names <- gsub(",","_", col_names)
col_names <- gsub("\\(","_", col_names)
col_names <- gsub("\\)","", col_names)
col_names <- tolower(col_names)

# read in the labels for the activities

activity_data <- read.table("UCI HAR Dataset/activity_labels.txt")
activity_data <- tolower(activity_data$V2)

# get the test data

test_data <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE, col.names = col_names)
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE, col.names = "subject")
test_activity <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE, col.names = "activity")

test_data$activity = test_activity$activity
test_data$subject = test_subject$subject

# get the training data

train_data <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE, col.names = col_names)
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE, col.names = "subject")
train_activity <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE, col.names = "activity")

train_data$activity = train_activity$activity
train_data$subject = train_subject$subject

# combine the two datasets

cmb_data <- rbind(train_data, test_data)

# find our columns of interest. add 'activity' and subject' to them

cols_of_interest <- grep("mean|std",col_names,value = TRUE)

cols_of_interest <- append(cols_of_interest, "subject", after = 0)
cols_of_interest <- append(cols_of_interest, "activity", after = 1)

# subset our data to only get the columns of interest

wearable_data <- select(cmb_data,cols_of_interest)

# change the values of the activities to be their labels

for(i in 1:length(activity_data)) {
        wearable_data$activity <- gsub(i, activity_data[i], wearable_data$activity)
}

# there should be a way to do this in a loop. but I'm not sure how to format it
# we're putting together the separate table that will summarize the data

wearable_data <- wearable_data %>% group_by(subject, activity) %>%
        summarise(time_body_accelerometer_meanx = mean(tbodyacc_mean_x, na.rm = TRUE),
                  time_body_accelerometer_meany = mean(tbodyacc_mean_y, na.rm = TRUE),
                  time_body_accelerometer_meanz = mean(tbodyacc_mean_z, na.rm = TRUE),
                  std_time_body_accelerometer_meanx = mean(tbodyacc_std_x, na.rm = TRUE),
                  std_time_body_accelerometer_meany = mean(tbodyacc_std_y, na.rm = TRUE),
                  std_time_body_accelerometer_meanz = mean(tbodyacc_std_z, na.rm = TRUE),
                  time_gravity_accelerometer_meanx = mean(tgravityacc_mean_x, na.rm = TRUE),
                  time_gravity_accelerometer_meany = mean(tgravityacc_mean_y, na.rm = TRUE),
                  time_gravity_accelerometer_meanz = mean(tgravityacc_mean_z, na.rm = TRUE),
                  std_time_gravity_accelerometer_meanx = mean(tgravityacc_std_x, na.rm = TRUE),
                  std_time_gravity_accelerometer_meany = mean(tgravityacc_std_y, na.rm = TRUE),
                  std_time_gravity_accelerometer_meanz = mean(tgravityacc_std_z, na.rm = TRUE),
                  time_body_accelerometer_jerk_meanx = mean(tbodyaccjerk_mean_x, na.rm = TRUE),
                  time_body_accelerometer_jerk_meany = mean(tbodyaccjerk_mean_y, na.rm = TRUE),
                  time_body_accelerometer_jerk_meanz = mean(tbodyaccjerk_mean_z, na.rm = TRUE),
                  std_time_body_accelerometer_jerk_meanx = mean(tbodyaccjerk_std_x, na.rm = TRUE),
                  std_time_body_accelerometer_jerk_meany = mean(tbodyaccjerk_std_y, na.rm = TRUE),
                  std_time_body_accelerometer_jerk_meanz = mean(tbodyaccjerk_std_z, na.rm = TRUE),
                  time_body_gyroscope_meanx = mean(tbodygyro_mean_x, na.rm = TRUE),
                  time_body_gyroscope_meany = mean(tbodygyro_mean_y, na.rm = TRUE),
                  time_body_gyroscope_meanz = mean(tbodygyro_mean_z, na.rm = TRUE),
                  std_time_body_gyroscope_meanx = mean(tbodygyro_std_x, na.rm = TRUE),
                  std_time_body_gyroscope_meany = mean(tbodygyro_std_y, na.rm = TRUE),
                  std_time_body_gyroscope_meanz = mean(tbodygyro_std_z, na.rm = TRUE),
                  time_body_gyroscope_jerk_meanx = mean(tbodygyrojerk_mean_x, na.rm = TRUE),
                  time_body_gyroscope_jerk_meany = mean(tbodygyrojerk_mean_y, na.rm = TRUE),
                  time_body_gyroscope_jerk_meanz = mean(tbodygyrojerk_mean_z, na.rm = TRUE),
                  std_time_body_gyroscope_jerk_meanx = mean(tbodygyrojerk_std_x, na.rm = TRUE),
                  std_body_gyroscope_jerk_meany = mean(tbodygyrojerk_std_y, na.rm = TRUE),
                  std_body_gyroscope_jerk_meanz = mean(tbodygyrojerk_std_z, na.rm = TRUE),
                  time_body_accelerometer_magnitude_mean = mean(tbodyaccmag_mean, na.rm = TRUE),
                  std_time_body_accelerometer_magnitude_mean = mean(tbodyaccmag_std, na.rm = TRUE),
                  time_body_gyroscope_magnitude_mean = mean(tgravityaccmag_mean, na.rm = TRUE),
                  std_time_body_gyroscope_magnitude_mean = mean(tgravityaccmag_std, na.rm = TRUE),
                  time_body_accelerometer_jerk_magnitude_mean = mean(tbodyaccjerkmag_mean, na.rm = TRUE),
                  std_time_body_accelerometer_jerk_magnitude_mean = mean(tbodyaccjerkmag_std, na.rm = TRUE),
                  time_body_gyroscope_magnitude_mean = mean(tbodygyromag_mean, na.rm = TRUE),
                  std_time_body_gyroscope_magnitude_mean = mean(tbodygyromag_std, na.rm = TRUE),
                  time_body_gyroscope_jerk_magnitude_mean = mean(tbodygyrojerkmag_mean, na.rm = TRUE),
                  std_time_body_gyroscope_jerk_magnitude_mean = mean(tbodygyrojerkmag_std, na.rm = TRUE),
                  fft_body_accelerometer_meanx = mean(fbodyacc_mean_x, na.rm = TRUE),
                  fft_body_accelerometer_meany = mean(fbodyacc_mean_y, na.rm = TRUE),
                  fft_body_accelerometer_meanz = mean(fbodyacc_mean_z, na.rm = TRUE),
                  std_fft_body_accelerometer_meanx = mean(fbodyacc_std_x, na.rm = TRUE),
                  std_fft_body_accelerometer_meany = mean(fbodyacc_std_y, na.rm = TRUE),
                  std_fft_body_accelerometer_meanz = mean(fbodyacc_std_z, na.rm = TRUE),
                  fft_body_accelerometer_mean_frequencyx = mean(fbodyacc_meanfreq_x, na.rm = TRUE),
                  fft_body_accelerometer_mean_frequencyY = mean(fbodyacc_meanfreq_y, na.rm = TRUE),
                  fft_body_accelerometer_mean_frequencyZ = mean(fbodyacc_meanfreq_z, na.rm = TRUE),
                  fft_body_accelerometer_jerk_meanx = mean(fbodyaccjerk_mean_x, na.rm = TRUE),
                  fft_body_accelerometer_jerk_meany = mean(fbodyaccjerk_mean_y, na.rm = TRUE),
                  fft_body_accelerometer_jerk_meanz = mean(fbodyaccjerk_mean_z, na.rm = TRUE),
                  std_fft_body_accelerometer_jerk_meanx = mean(fbodyaccjerk_std_x, na.rm = TRUE),
                  std_fft_body_accelerometer_jerk_meany = mean(fbodyaccjerk_std_y, na.rm = TRUE),
                  std_fft_body_accelerometer_jerk_meanz = mean(fbodyaccjerk_std_z, na.rm = TRUE),
                  fft_body_accelerometer_jerk_mean_frequencyx = mean(fbodyaccjerk_meanfreq_x, na.rm = TRUE),
                  fft_body_accelerometer_jerk_mean_frequencyy = mean(fbodyaccjerk_meanfreq_y, na.rm = TRUE),
                  fft_body_accelerometer_jerk_mean_frequencyz = mean(fbodyaccjerk_meanfreq_z, na.rm = TRUE),
                  fft_body_gyroscope_meanx = mean(fbodygyro_mean_x, na.rm = TRUE),
                  fft_body_gyroscope_meany = mean(fbodygyro_mean_y, na.rm = TRUE),
                  fft_body_gyroscope_meanz = mean(fbodygyro_mean_z, na.rm = TRUE),
                  std_fft_body_gyroscope_meanx = mean(fbodygyro_std_x, na.rm = TRUE),
                  std_fft_body_gyroscope_meany = mean(fbodygyro_std_y, na.rm = TRUE),
                  std_fft_body_gyroscope_meanz = mean(fbodygyro_std_z, na.rm = TRUE),
                  fft_body_gyroscope_mean_frequencyx = mean(fbodygyro_meanfreq_x, na.rm = TRUE),
                  fft_body_gyroscope_mean_frequencyy = mean(fbodygyro_meanfreq_y, na.rm = TRUE),
                  fft_body_gyroscope_mean_frequencyz = mean(fbodygyro_meanfreq_z, na.rm = TRUE),
                  fft_body_accelerometer_magnitude_mean = mean(fbodyaccmag_mean, na.rm = TRUE),
                  std_body_accelerometer_magnitude_mean = mean(fbodyaccmag_std, na.rm = TRUE),
                  fft_body_accelerometer_magnitude_mean_frequency = mean(fbodyaccmag_meanfreq, na.rm = TRUE),
                  fft_body2accelerometer_jerk_magnitude_mean = mean(fbodybodyaccjerkmag_mean, na.rm = TRUE),
                  std_fft_body2_accelermeter_jerk_magnitude_mean = mean(fbodybodyaccjerkmag_std, na.rm = TRUE),
                  fft_body2_accelerometer_jerk_magnitude_mean_frequency = mean(fbodybodyaccjerkmag_meanfreq, na.rm = TRUE),
                  fft_body_body_gyroscope_magnitude_mean = mean(fbodybodygyromag_mean, na.rm = TRUE),
                  std_fft_body_body_gyroscope_magnitude_mean = mean(fbodybodygyromag_mean, na.rm = TRUE),
                  std_fft_body_body_gyroscope_magnitude_mean = mean(fbodybodygyromag_std, na.rm = TRUE),
                  fft_body_body_gyroscope_magnitude_mean_frequency = mean(fbodybodygyromag_meanfreq, na.rm = TRUE),
                  fft_body_body_gyroscope_jerk_magnitude_mean = mean(fbodybodygyrojerkmag_mean, na.rm = TRUE),
                  std_fft_body_body_gyroscope_jerk_magnitude_mean = mean(fbodybodygyrojerkmag_std, na.rm = TRUE),
                  fft_body_body_gyro_jerk_magnitude_mean_frequency = mean(fbodybodygyrojerkmag_meanfreq, na.rm = TRUE),
                  angle_time_body_accelerometer_mean_gravity = mean(angle_tbodyaccmean_gravity, na.rm = TRUE),
                  angle_time_body_accelerometer_jerk_mean_gravity_mean = mean(angle_tbodyaccjerkmean_gravitymean, na.rm = TRUE),
                  angle_time_gyroscope_mean_gravity_mean = mean(angle_tbodygyromean_gravitymean, na.rm = TRUE),
                  angle_time_body_gyroscope_jerk_mean_gravity_mean = mean(angle_tbodygyrojerkmean_gravitymean, na.rm = TRUE),
                  angle_x_gravity_mean = mean(angle_x_gravitymean, na.rm = TRUE),
                  angle_y_gravity_mean = mean(angle_y_gravitymean, na.rm = TRUE),
                  angle_z_gravity_mean = mean(angle_z_gravitymean, na.rm = TRUE))
                         