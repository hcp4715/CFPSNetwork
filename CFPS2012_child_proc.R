################## Processing CFPS data of SES and mental health ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    18-10-09         First version
# HCP         18-10-11         Add code to remove the rows with NA and suggestion for coding
# Cai, Y-Q    18-10-20         Divide by gender
# Cai, Y-Q    18-10-24         Change dimension of depr and fair
# Cai, Y-Q    19-01-03         Change into intergenerational transmission
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
data <- read.csv("sesMHc2012.csv", header=T)     
summary(data)

#data extraction
data <- data %>%
  dplyr::select(-pid_c, -fid12, -pid_f, -pid_m, -age_c, -gender_c, -pidc.x, -pidc.y)
summary(data)
#partial correlation
data[data < 0] <- NA
data <-drop_na(data)
labels <- c("deprc1", "deprc2","deprc3","deprc4","deprc5","deprc6","deprc7","deprc8","deprc9","deprc10",
            "deprc11","deprc12","deprc13","deprc14","deprc15","deprc16","deprc17","deprc18","deprc19","deprc20",
  "deprf1", "deprf2","deprf3","deprf4","deprf5","deprf6","deprf7","deprf8","deprf9","deprf10",
            "deprf11","deprf12","deprf13","deprf14","deprf15","deprf16","deprf17","deprf18","deprf19","deprf20",
            "satis_f", "confi_f", "edu_f", "income_f","pidc", "deprm1", "deprm2","deprm3","deprm4","deprm5","deprm6","deprm7","deprm8","deprm9","deprm10",
  "deprm11","deprm12","deprm13","deprm14","deprm15","deprm16","deprm17","deprm18","deprm19","deprm20",
  "satis_m", "confi_m", "edu_m", "income_m")
groups <- factor(c(rep("childrenM", 20), rep("fatherM", 22), rep("fatherSES", 2), rep("motherM", 22), rep("motherSES", 2)))
netPcor <- qgraph::qgraph(cor(data), layout = "spring", labels = labels, groups = groups, graph = "concentration")
#lasso
adls <- adalasso.net(data) 
network <- as.matrix(forceSymmetric(adls$pcor.adalasso)) 
lasso <- qgraph(network, layout = "spring", labels = labels, groups = groups)

# Centrality
centrality <- centrality_auto(network)
nc <- centrality$node.centrality
ebc <- centrality$edge.betweenness.centrality
central <- centralityPlot(network)

#cluster
clustcoef <- clustcoef_auto(network)
cluster <- clusteringPlot(network, signed = TRUE)

