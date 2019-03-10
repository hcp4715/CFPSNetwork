################## Preprocessing CFPS child 2016 data ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    19-03-09         The first version
# 
# 
#
###### input######
# 2016child.csv
# 2016adult.csv
# 2016family.csv
#
###### output #####
# data2016.csv
#
#
#
######################## Start of the script ###########################
### clean the memory to avoid unnecessary errors:
rm(list = ls())

### set directory to the folder of analytic data

# Get the directory of the current R script
curWD <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory to the directory where this script is 
setwd(curWD)

# load the packages needed, if not exist, download from cran
if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); require(tidyverse)}
if (!require(dplyr)) {install.packages("dplyr",repos = "http://cran.us.r-project.org"); require(dplyr)}
if (!require(psych)) {install.packages("psych",repos = "http://cran.us.r-project.org"); require(psych)}
library("psych")
library("dplyr")
library("tidyverse")

#read data
data2010 <- read.csv("data2010.csv", header=T)
data2012 <- read.csv("data2012.csv", header=T)
data2014 <- read.csv("data2014.csv", header=T)
data2016 <- read.csv("data2016.csv", header=T)

#merge 4 years!
data10_12 <- merge(data2010, data2012, by = "fid")
data14_16 <- merge(data2014, data2016, by = "fid") 
data <- merge(data14_16, data10_12, by = "fid") 
colSums(is.na(data))

data <- drop_na(data)
