#Getting and Cleaning Data Course Project CodeBook
This file describes the variables, the data, and any transformations or work that I have performed to clean up the data.

 A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project: 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

 1. The run_analysis.R script performs
    the following steps to clean the
    data: Read X_train.txt, y_train.txt
    and subject_train.txt from the
    "./data/train" folder and store them
    in trainData, trainLabel and
    trainSubject variables respectively.
    

 2. Read X_test.txt, y_test.txt and
    subject_test.txt from the
    "./data/test" folder and store them
    in testData, testLabel and
    testsubject variables respectively.
 3. Concatenate testData to trainData to
    generate a 10299x561 data frame,
    joinData; concatenate testLabel to
    trainLabel to generate a 10299x1
    data frame, joinLabel; concatenate
    testSubject to trainSubject to
    generate a 10299x1 data frame,
    joinSubject.
    

 4. Read the features.txt file from the
    "/data" folder and store the data in
    a variable called features. We only
    extract the measurements on the mean
    and standard deviation. This results
    in a 66 indices list. We get a
    subset of joinData with the 66
    corresponding columns.
    

 5. Clean the column names of the
    subset. We remove the "()" and "-"
    symbols in the names, as well as
    make the first letter of "mean" and
    "std" a capital letter "M" and "S"
    respectively. Read the
    activity_labels.txt file from the
    "./data"" folder and store the data
    in a variable called activity.
    

 6. Clean the activity names in the
    second column of activity. We first
    make all names to lower cases. If
    the name has an underscore between
    letters, we remove the underscore
    and capitalize the letter
    immediately after the underscore.
    

 7. Transform the values of joinLabel
    according to the activity data
    frame. Combine the joinSubject,
    joinLabel and joinData by column to
    get a new cleaned 10299x68 data
    frame, cleanedData. Properly name
    the first two columns, "subject" and
    "activity". The "subject" column
    contains integers that range from 1
    to 30 inclusive; the "activity"
    column contains 6 kinds of activity
    names; the last 66 columns contain
    measurements that range from -1 to 1
    exclusive.
    
 8. From the data set in previous step, create a second, independent tidy data set with the average of each variable for each activity and each subject.  The resulting dataset written to "aggregateData.txt" file in current working directory.