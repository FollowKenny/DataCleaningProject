#!/usr/bin/R

# File Desc including purpose, args and outputs

library(readr)
library(dplyr)
library(data.table)
 
SamsungMerge <- function() {
  # Get the data and construct the merged train and test tbdl_df from the
  # Samsung Experiment
  #
  # Returns :
  #   train and test data merged in one tbl_df

  ReadData <- function(category) {
    # Generic function to read the wanted data files in a given directory
    #
    # Args :
    #   category (train/test) of the data to be read
    #
    # Returns :
    #   tbl_df of the given category datafile
    
    cat.files <- file.path(data.dir, "UCI HAR Dataset", category, 
                           c(paste("X_", category, ".txt",  sep = ""),
                             paste("y_", category, ".txt", sep = ""),
                             paste("subject_", category, ".txt", sep = "")
                            )
                          )
    for (i in seq_along(cat.files)) {
      system.cmd <- paste('/usr/bin/dos2unix "', cat.files[[i]], '"', sep = "")
      system(system.cmd)
    }
      
    sapply(cat.files, read_table, F)
  }
    
  # Download  
  data.dir <- readline("Enter the directory in which the data will be downloaded :\n")
  data.dir <<- data.dir
  data.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  data.zip <- file.path(data.dir, "Dataset.zip")
  # TODO : uncomment
  #download.file(data.url, data.zip)

  # Read and Merge
  train.data.list <- ReadData("train")
  test.data.list <- ReadData("test")
  train.tbl <- mutate(train.data.list[[1]],
                      activity = as.numeric(train.data.list[[2]][[1]]),
                      subject = as.numeric(train.data.list[[3]][[1]])
                     )
  test.tbl <- mutate(test.data.list[[1]], 
                     activity = as.numeric(test.data.list[[2]][[1]]), 
                     subject = as.numeric(test.data.list[[3]][[1]])
                    )
  bind_rows(test.tbl, train.tbl)
}

SamsungSelect <- function(exp.tbl) {
  # Select only the wanted data from the Samsung Experiment, namely :
  # std and mean related values.
  #
  # Args :
  #   Concatenated table of all the experiment data
  #
  # Returns :
  #   tbl_df of the wanted values only with properly named columns
  
  # Read the feature table and get the right features position
  feature.file <- file.path(data.dir, "UCI HAR Dataset", "features.txt")
  system.cmd <- paste('/usr/bin/dos2unix "', feature.file, '"', sep = "")
  system(system.cmd)
  feature.tbl <- read_delim(feature.file, delim = " ", col_names = F)
  col.keep <- grep("mean|std", feature.tbl[[2]])
  
  # select the right columns and name it accordingly
  activity.stdmean <- select(exp.tbl, activity, subject, col.keep) %>%
                        setnames(old=3:81, new=feature.tbl[col.keep, 2][[1]])
  activity.stdmean
}

SamsungLabelActivity <- function(stdmean) {
  # Transform activity code into activity label in the Samsung dataset
  #
  # Args :
  #   Fully prepared table
  #
  # Returns :
  #   table with activity variable containing activity label
  
  # Read the activity label file and use it to transform the table
  label.file <- file.path(data.dir, "UCI HAR Dataset", "activity_labels.txt")
  system.cmd <- paste('/usr/bin/dos2unix "', label.file, '"', sep = "")
  system(system.cmd)
  label.tbl <- read_delim(label.file, delim = " ", col_names = F)
  stdmean[1] <- label.tbl[stdmean[[1]], 2]
  stdmean
}
  

Main <- function() {
  exp.tbl <- SamsungMerge()
  stdmean <- SamsungSelect(exp.tbl)
  final.tbl <- SamsungLabelActivity(stdmean)
  final.tbl %>% group_by(activity, subject) %>% summarise_each(funs = "mean")
}

hate <- Main()