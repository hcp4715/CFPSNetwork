################## Processing CFPS data of SES and mental health ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    18-10-09         First version
# HCP         18-10-11         Add code to remove the rows with NA and suggestion for coding
# Cai, Y-Q    18-10-22         Divide by gender
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
library("tidyverse")  ## hcp: use tidyverse style for the code

library("parcor")
library("Matrix")
library("psych")
library("dplyr")
library("ggplot2")

#set directory to the folder of analytic data
data<-read.csv("sesMH.csv",header=T)     
#setwd("/Users/apple/Desktop/CFPS/5_ProcessData") 
#getwd()

summary(data)
##NOTICE: over 10000 missing values for occupation coding and 1865 for income, thousands missing for attributional style and fairness
data.complete <- data %>%     
      dplyr::filter(complete.cases(data)) %>%  # only the complete cases (i.e., no NA in every row) 
      dplyr::select(-fid, -pid, -gender, -qa1age) # drop the columns for later analysis

# label for the nodes of network
labels <- c("attri1", "attri2", "attri3", "attri4", "attri5", "attri6", "attri7", 
            "fair1", "fair2", "fair3", "fair4", "fair5", "fair6", "fair7", "fair8", 
            "educ", "word", "math", "occu1", "occu2", "income", "pol",
            "depr1", "depr2", "depr3", "depr4", "depr5", "depr6", "happ", "satis", "confi")

##NOTICE: data.complete has 9419 obs
#generate partial correlation network of all the data

groups <- factor(c(rep("attri", 7), rep("fair", 8), 
                   rep("hCap", 3), rep("mCap", 3), rep("pCap", 1), 
                   rep("deprMH", 6), rep("otherMH", 3)))

# hcp: I have error message here, because here we have more labels than the data (19 columns)
netPcor <- qgraph::qgraph(cor(data.complete), layout = "spring", 
                    labels = labels, groups = groups, graph = "concentration")
# export image
jpeg(file = "all.jpeg")
plot(netPcor)
dev.off()

# data without those variables that have too many missing values
data.more <- data %>%
      dplyr::select(qq601, qq602, qq603, qq604, qq605, qq606, qm403, qm404, qk802, educ, wordtest, mathtest, qa7_s_1) %>%
      dplyr::filter(complete.cases(data))
summary(data.more)

# generate partial correlation network of male, 30-50
data.male <- data %>%
      dplyr::filter(complete.cases(data)) %>% 
      dplyr::filter(gender == 1) %>% # male
      dplyr::filter(qa1age >= 30 & qa1age <=50) %>% # age:30-50
      dplyr::group_by(fid) %>% 
      dplyr::filter(pid == max(pid)) %>%  # one each family
      dplyr::select(-pid, -gender, -qa1age)  # drop the columns for later analysis
data.male$fid <- NULL# drop the columns for later analysis 
summary(data.male)

groups <- factor(c(rep("attri", 7), rep("fair", 8), 
                   rep("hCap", 3), rep("mCap", 3), rep("pCap", 1), 
                   rep("deprMH", 6), rep("otherMH", 3)))
netPcor_m <- qgraph(cor(data.male), layout = "spring", 
                  labels = labels, groups = groups, graph = "concentration")

# export image
jpeg(file = "/Users/apple/Desktop/CFPS/5_ProcessData/Male.jpeg")
plot(netPcor)
dev.off()

#LASSO-male
set.seed(100)
adls <- adalasso.net(data.male) 
network <- as.matrix(forceSymmetric(adls$pcor.adalasso)) 
lasso <- qgraph(network, layout = "spring", labels = labels, groups = groups)
# export image
jpeg(file = "/Users/apple/Desktop/CFPS/5_ProcessData/male_lasso.jpeg")
plot(lasso)
dev.off()

# Centrality
centrality <- centrality_auto(netPcor)
nc <- centrality$node.centrality
ebc <- centrality$edge.betweenness.centrality
central <- centralityPlot(netPcor)
jpeg(file = "/Users/apple/Desktop/CFPS/5_ProcessData/Male_central.jpeg")
plot(central)
dev.off()

#cluster
clustcoef <- clustcoef_auto(netPcor)
cluster <- clusteringPlot(netPcor, signed = TRUE)
jpeg(file = "/Users/apple/Desktop/CFPS/5_ProcessData/Male_cluster.jpeg")
plot(cluster)
dev.off()



# generate partial correlation network of female, 30-50
data.female <- data %>%
  filter(complete.cases(data)) %>%
  filter(gender == 0) %>% # male
  filter(qa1age >= 30 & qa1age <= 50) %>% # age:30-50
  group_by(fid) %>% 
  filter(pid == max(pid)) %>% # one each family
  select(-pid, -gender, -qa1age) # drop the columns for later analysis
data.female$fid <- NULL

groups <- factor(c(rep("attri", 7), rep("fair", 8), 
                   rep("hCap", 3), rep("mCap", 3), rep("pCap", 1), 
                   rep("deprMH", 6), rep("otherMH", 3)))
netPcor <- qgraph(cor(data.female), layout = "spring", labels = labels,
                  groups = groups, graph = "concentration")

#export image
jpeg(file = "/Users/apple/Desktop/CFPS/5_ProcessData/Female.jpeg")
plot(netPcor)
dev.off()

#LASSO-female
set.seed(100)
adls <- adalasso.net(data.female) 
network <- as.matrix(forceSymmetric(adls$pcor.adalasso)) 
lasso <- qgraph(network, layout = "spring", labels = labels, groups = groups)

# export image
jpeg(file = "/Users/apple/Desktop/CFPS/5_ProcessData/Female_lasso.jpeg")
plot(lasso)
dev.off()

# Centrality
centrality <- centrality_auto(netPcor)
nc <- centrality$node.centrality
ebc <- centrality$edge.betweenness.centrality
central <- centralityPlot(netPcor)
jpeg(file = "/Users/apple/Desktop/CFPS/5_ProcessData/Female_central.jpeg")
plot(central)
dev.off()

#cluster
clustcoef <- clustcoef_auto(netPcor)
cluster <- clusteringPlot(netPcor, signed = TRUE)
jpeg(file = "/Users/apple/Desktop/CFPS/5_ProcessData/Female_cluster.jpeg")
plot(cluster)
dev.off()