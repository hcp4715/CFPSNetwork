################## Processing CFPS data of SES and mental health ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    18-10-09         First version
# HCP         18-10-11         Add code to remove the rows with NA and suggestion for coding
#
#
###### input######
#sesMH.csv
#
#
#
#
#
#
###### output #####
#
#
#
#
######################## Start of the script ###########################
#
# Loading the necessary libraries (if not exist, install and then load)

if (!require(qgraph)) {install.packages("qgraph",repos = "http://cran.us.r-project.org"); require(qgraph)}
library("qgraph")

if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); require(tidyverse)}
library("tidyverse")  ## hcp: use tidyverse style for the code

library("parcor")
library("Matrix")
library("psych")
library("dplyr")
library("ggplot2")

#set directory to the folder of analytic data
setwd("/Users/apple/Desktop/CFPS/5_ProcessData") ## hcp: why set working directory here?
getwd()
data<-read.csv("sesMH.csv",header=T)     ## hcp: once you change to the right directoy, don't use the directory information to read the file.
# data$pid<-NULL
summary(data)

drop.cols <- 'pid' #  the column(s) need to be dropped for later analysis

data.complete <- data %>%     
      filter(complete.cases(data)) %>%   # only the complete cases (i.e., no NA in every row) 
      select(-drop.cols)                 # drop the column(s)

#generate correlation network of all the data
groups <- factor(c(rep("hCap", 3), rep("mCap", 4), rep("pCap", 1), rep("mHealth", 9), rep("mCap", 1)))
netCor <- qgraph(cor(data.complete), layout = "spring", labels = colnames(data), groups = groups)

#export image
jpeg(file = "all.jpeg") ## hcp: need to specify more parameter to get ahigh resolution graph.
plot(netCor)
dev.off()

#generate correlation network of SES
SESname <- c("educ","wordtest","mathtest","qg307isco","qk601","income") 
## hcp: please rename the column name so that the names are meaningful

ses <- data.complete[,SESname]
groups <- factor(c(rep("SES", 6)))
netCor <- qgraph(cor(ses), layout = "spring", labels = colnames(ses), groups = groups)
summary(ses)
#export image
jpeg(file = "/Users/apple/Desktop/CFPS/5_ProcessData/ses.jpeg")
plot(netCor)
dev.off()

#generate correlation network of depression
deprname=c("qq601","qq602","qq603","qq604","qq605","qq606")
depr=data[,deprname]
groups <- factor(c(rep("mental", 6)))
netCor <- qgraph(cor(depr), layout = "spring", labels = colnames(depr), groups = groups)
summary(depr)
#export image
jpeg(file = "/Users/apple/Desktop/CFPS/5_ProcessData/depr.jpeg")
plot(netCor)
dev.off()

#generate correlation network of MH
MHname=c("qq601","qq602","qq603","qq604","qq605","qq606","qk802", "qm403","qm404")
MH=data[,MHname]
groups <- factor(c(rep("mHealth", 9)))
netCor <- qgraph(cor(MH), layout = "spring", labels = colnames(MH), groups = groups)
#export image
jpeg(file = "/Users/apple/Desktop/CFPS/5_ProcessData/MH.jpeg")
plot(netCor)
dev.off()