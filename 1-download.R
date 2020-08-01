
## download the dataset and load it into the workspace

if(!file.exists("./data")){
    dir.create("./data")
}

#fileUrl <- "https://github.com/estevamteixeira/RepData_PeerAssessment1/blob/master/activity.zip"
#download.file(fileUrl, destfile = "./data/activity.zip", method = "curl")

## unzip the file

unzip(zipfile = "C:/Users/estev/Downloads/activity.zip",
      exdir = "./data")

## unzipped files are in the folder EDA---Week1---Project
## get the list of files in this folder

path <- file.path("./data")
# file.path("./data", "UCI HAR Dataset/")
files <- list.files(path, recursive = TRUE) # list files inside directories
files

##-----------------
## Loading the Data
##-----------------

## This first line will likely take a few seconds. Be patient!

activity <- read.csv(file.path(path, files[1]))
