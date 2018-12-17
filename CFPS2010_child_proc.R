################## Processing CFPS data of SES and mental health ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    18-10-09         First version
# HCP         18-10-11         Add code to remove the rows with NA and suggestion for coding
# Cai, Y-Q    18-10-20         Divide by gender
# Cai, Y-Q    18-10-24         Change dimension of depr and fair
###### input######
#sesMH.csv
#
#
#
#
#
#
###### output #####
# images
#
#
#
######################## Start of the script ###########################
#
### clean the memory to avoid unnecessary errors:
rm(list = ls())

# Get the directory of the current R script
curWD <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory to the directory where this script is 
setwd(curWD)

# Loading the necessary libraries (if not exist, install and then load)

if (!require(qgraph)) {install.packages("qgraph",repos = "http://cran.us.r-project.org"); require(qgraph)}
library("qgraph")
if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); require(tidyverse)}
library("tidyverse")  
if (!require(parcor)) {install.packages("parcor",repos = "http://cran.us.r-project.org"); require(parcor)}
library("parcor")  
if (!require(Matrix)) {install.packages("Matrix",repos = "http://cran.us.r-project.org"); require(Matrix)}
library("Matrix")
if (!require(psych)) {install.packages("psych",repos = "http://cran.us.r-project.org"); require(psych)}
library("psych")
if (!require(dplyr)) {install.packages("dplyr",repos = "http://cran.us.r-project.org"); require(dplyr)}
library("dplyr")


#set directory to the folder of analytic data
data <- read.csv("sesMH.csv", header=T)     
summary(data)