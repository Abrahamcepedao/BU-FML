### WORKING DIRECTORY SETTING AND GETTING ###
#This code example introduces how to setup a directory 
#structure


###This gives the path where R is currently looking for files
getwd()

#Save the working directory for later
original_dir = getwd()

original_dir


## Change the path where R is looking for files to another location
#setwd("C:/Users/vkr0/Documents/")

getwd()


#must use forward slash or double backslash, 
#otherwise R interprets this as a special character
#setwd("C:\Users\vkr0\Documents")


#Reset back to the original working directory
setwd(original_dir)

getwd()


### Reading data into R - make sure the example data is located at this path###
days <- read.csv("Example_Data.csv", header = FALSE)

#Change the column names of the dataset
colnames(days) = paste("Day",seq(1,ncol(days)),sep="_")

### Data with a header
golf = read.csv("GolfBals.csv",header=T)

#Write the dataset back to a csv file without including row names and including quotes
write.csv(golf,file="GolfBallsNew.csv",row.names=F,quote=T)

### INSTALLING PACKAGES ####


###Installs the package from the CRAN mirror (must be connected to the internet)

#Only install the package if we don't already have it installed
if(!require(swirl))
{
  install.packages("swirl")
}

if(!require(reader))
{
  install.packages("reader")
}

#Library loads the specified package
library(reader)

###Loads the package to be used!
library(swirl)

### To see all installed packages
library()


###Quits out of R and asks if you want to save your workspace
q()



###Save the workspace
save.image(file="Current_Workspace.RData")

###Remove all objects in the current workspace
rm(list=ls())

#Remove a specified object
rm(days)

#Calls the garbage collector to free memory
gc()

#Re-load the workspace
load("Current_Workspace.RData")

###Save just two dataframes
save(days,golf,file="All_Data.RData")


#Empty workspace
rm(list=ls())

load(file="All_Data.RData")

###Removes only the specified variables
rm(days,golf)

###Getting help
?rm

###Deletes the file
unlink("All_Data.RData")
