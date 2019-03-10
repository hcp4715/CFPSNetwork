################## Processing CFPS 2010 child proc ##############################
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
data <- read.csv("sesMHc.csv", header=T)     
summary(data)

#network of father mental health, mother mental health and chidlren mental health
##data extraction
MHtrans <- data %>%
  dplyr::select(wn401, wn402, wn403, wn404, wn405, wn406, wm302, wm303, wz201, wz207,  # child: depression x6,  happiness, confidence in future, observation-cognitive, observation-intelligence
         qm403f, qm404f, qk802f, qq601f, qq602f, qq603f, qq604f, qq605f, qq606f, wordtestf, mathtestf, #father: happiness, life satisfaction, confidence in future, depression X6, cognition
         qm403m, qm404m, qk802m, qq601m, qq602m, qq603m, qq604m, qq605m, qq606m, wordtestm, mathtestm) #mother: happiness, life satisfaction, confidence in future, depression X6, cognition
MHtrans <- drop_na(MHtrans)
#partial correlation
labels <- c("depressed", "nervous", "angry", "hopless", "hard", "meaningless","happi", "confi", "cogc1", "cogc2",
            "satisf","confif", "happif", "depressedf", "nervousf", "angryf", "hoplessf", "hardf", "meaninglessf", "wordf", "mathf",
            "satism","confim", "happim", "depressedm", "nervousm", "angrym", "hoplessm", "hardm", "meaninglessm", "wordm", "mathm")
groups <- factor(c(rep("children", 10), rep("father", 11), rep("mother", 11)))
netPcor <- qgraph::qgraph(cor(MHtrans), layout = "spring", 
                          labels = labels, groups = groups, graph = "concentration")

#lasso
adls <- adalasso.net(MHtrans) 
network <- as.matrix(forceSymmetric(adls$pcor.adalasso)) 
lasso <- qgraph(network, layout = "spring", labels = labels, groups = groups)

# Centrality
centrality <- centrality_auto(netPcor)
nc <- centrality$node.centrality
ebc <- centrality$edge.betweenness.centrality
central <- centralityPlot(netPcor)

#cluster
clustcoef <- clustcoef_auto(netPcor)
cluster <- clusteringPlot(netPcor, signed = TRUE)

