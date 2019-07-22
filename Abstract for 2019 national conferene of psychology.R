################## Preprocessing CFPS child 2016 preproc ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    19-07-19         The first version
# 
# 
#
###### input######
# data2012.csv
# data2016.csv
# 
#
###### output #####
# 
#
#
######################### Start of the script ###########################
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
if (!require(caret)) {install.packages("caret",repos = "http://cran.us.r-project.org"); require(caret)}
if (!require(DMwR)) {install.packages("DMwR",repos = "http://cran.us.r-project.org"); require(DMwR)}
library("DMwR")
library("caret")
library("psych")
library("dplyr")
library("tidyverse")

#read data
##CYQ: data2012 and data2014 have been modified, see CFPS2012_child_preproc and CFPS2016_child_prepro
data2012 <- read.csv("data2012.csv", header=T)
data2016 <- read.csv("data2016.csv", header=T)

############################################
#network analysis for 2012
#father and child
father_child<- data2012 %>%
  dplyr::select("deprc2_6","deprc2_7", "deprc2_11", "deprc2_12", "deprc2_14","deprc2_16","deprc2_18","deprc2_20",
                "deprf2_6","deprf2_7", "deprf2_11", "deprf2_12", "deprf2_14","deprf2_16","deprf2_18","deprf2_20")
names(father_child) <- c("dc1", "dc2","dc3","dc4","dc5","dc6","dc7","dc8",
                  "df1", "df2","df3","df4","df5","df6","df7","df8")
library("bootnet")
groups <- factor(c(rep("C", 8), rep("F",8)))
network_father_child <- bootnet::estimateNetwork(father_child, default = "EBICglasso") 
network_father_child_p <- plot(network_father_child,
                        title="2012 father and child", 
                        maximum=.47,
                        labels = TRUE,
                        #pie = pred1_all$error$R2,
                        layout = "spring",
                        groups = groups,
                        color = c("lightblue", "lightsalmon","antiquewhite"),
                        vsize=4, label.cex =2,
                        legend.cex = .35,
                        border.width=2, border.color='#555555')      # max=0.47
library("qgraph")
qgraph::smallworldness(network_father_child)

#mother and child
mother_child<- data2012 %>%
  dplyr::select("deprc2_6","deprc2_7", "deprc2_11", "deprc2_12", "deprc2_14","deprc2_16","deprc2_18","deprc2_20",
                "deprm2_6","deprm2_7", "deprm2_11", "deprm2_12", "deprm2_14","deprm2_16","deprm2_18","deprm2_20")
names(mother_child) <- c("dc1", "dc2","dc3","dc4","dc5","dc6","dc7","dc8",
                         "dm1", "dm2","dm3","dm4","dm5","dm6","dm7","dm8")
groups <- factor(c(rep("C", 8), rep("M",8)))
network_mother_child <- bootnet::estimateNetwork(mother_child, default = "EBICglasso") 
network_mother_child_p <- plot(network_mother_child,
                               title="2012 mother and child", 
                               maximum=.47,
                               labels = TRUE,
                               #pie = pred1_all$error$R2,
                               layout = "spring",
                               groups = groups,
                               color = c("lightblue", "lightsalmon","antiquewhite"),
                               vsize=4, label.cex =2,
                               legend.cex = .35,
                               border.width=2, border.color='#555555')      # max=0.47

#smallworldness for transitivity and aSPL
qgraph::smallworldness(network_mother_child)

############################################
#second part "left home or not"
#merge 2 years into one data frame
data2012_2016 <- merge(data2012, data2016, by = "fid")
str(data2012_2016)

#data used in the abstract 
data_abs <- data2012_2016 %>%
  dplyr::select(deprc2_6,deprc2_7, deprc2_11, deprc2_12, deprc2_14,deprc2_16,deprc2_18,deprc2_20,
                deprf2_6,deprf2_7, deprf2_11, deprf2_12, deprf2_14,deprf2_16,deprf2_18,deprf2_20,
                deprm2_6,deprm2_7, deprm2_11, deprm2_12, deprm2_14,deprm2_16,deprm2_18,deprm2_20,
                deprc6_6,deprc6_7, deprc6_11, deprc6_12, deprc6_14,deprc6_16,deprc6_18,deprc6_20,
                deprf6_6,deprf6_7, deprf6_11, deprf6_12, deprf6_14,deprf6_16,deprf6_18,deprf6_20,
                deprm6_6,deprm6_7, deprm6_11, deprm6_12, deprm6_14,deprm6_16,deprm6_18,deprm6_20,
                depr_sumc2, depr_summ2, depr_sumf2,
                depr_sumc6, depr_summ6, depr_sumf6, left2, left6, agec6, agec2) %>%
  dplyr::filter(left2 == 1) %>%
  dplyr::filter(agec6 >= 18) %>%
  dplyr::filter(agec2 >=13)
#check number of subject
table(data_abs$left6)
table(data_abs$agec2)

#correlation
#divide into group left and stay in 2016
data_left<- data_abs %>%
  dplyr::filter(left6 == 0) %>%
  dplyr::select(depr_sumc2, depr_summ2, depr_sumf2,
                depr_sumc6, depr_summ6, depr_sumf6, deprc2_6,deprc2_7, deprc2_11, deprc2_12, deprc2_14,deprc2_16,deprc2_18,deprc2_20,
                deprf2_6,deprf2_7, deprf2_11, deprf2_12, deprf2_14,deprf2_16,deprf2_18,deprf2_20,
                deprm2_6,deprm2_7, deprm2_11, deprm2_12, deprm2_14,deprm2_16,deprm2_18,deprm2_20,
                deprc6_6,deprc6_7, deprc6_11, deprc6_12, deprc6_14,deprc6_16,deprc6_18,deprc6_20,
                deprf6_6,deprf6_7, deprf6_11, deprf6_12, deprf6_14,deprf6_16,deprf6_18,deprf6_20,
                deprm6_6,deprm6_7, deprm6_11, deprm6_12, deprm6_14,deprm6_16,deprm6_18,deprm6_20)
data_stay<- data_abs %>%
  dplyr::filter(left6 == 1) %>%
  dplyr::select(depr_sumc2, depr_summ2, depr_sumf2,
                depr_sumc6, depr_summ6, depr_sumf6,deprc2_6,deprc2_7, deprc2_11, deprc2_12, deprc2_14,deprc2_16,deprc2_18,deprc2_20,
                deprf2_6,deprf2_7, deprf2_11, deprf2_12, deprf2_14,deprf2_16,deprf2_18,deprf2_20,
                deprm2_6,deprm2_7, deprm2_11, deprm2_12, deprm2_14,deprm2_16,deprm2_18,deprm2_20,
                deprc6_6,deprc6_7, deprc6_11, deprc6_12, deprc6_14,deprc6_16,deprc6_18,deprc6_20,
                deprf6_6,deprf6_7, deprf6_11, deprf6_12, deprf6_14,deprf6_16,deprf6_18,deprf6_20,
                deprm6_6,deprm6_7, deprm6_11, deprm6_12, deprm6_14,deprm6_16,deprm6_18,deprm6_20)
# fill up NAs
data_left <- DMwR::knnImputation(data_left, k = 10, scale = T, meth = "median", distData = NULL) 
data_stay <- DMwR::knnImputation(data_stay, k = 10, scale = T, meth = "median", distData = NULL) 
summary(data_left)
#correlation 
##left
#2012
cor(data_left[,c("depr_sumc2", "depr_sumf2", "depr_summ2")], method = "pearson")
cor.test(data_left$depr_sumc2, data_left$depr_sumf2)
cor.test(data_left$depr_sumc2, data_left$depr_summ2)
#2016
cor(data_left[,c("depr_sumc6", "depr_sumf6", "depr_summ6")], method = "pearson")
cor.test(data_left$depr_sumc6, data_left$depr_sumf6)
cor.test(data_left$depr_sumc6, data_left$depr_summ6)

##stay
cor(data_stay[,c("depr_sumc2", "depr_sumf2", "depr_summ2")], method = "pearson")
cor.test(data_stay$depr_sumc2, data_stay$depr_sumf2)
cor.test(data_stay$depr_sumc2, data_stay$depr_summ2)
cor(data_stay[,c("depr_sumc6", "depr_sumf6", "depr_summ6")], method = "pearson")
cor.test(data_stay$depr_sumc6, data_stay$depr_sumf6)
cor.test(data_stay$depr_sumc6, data_stay$depr_summ6)

##network analysis
###2012 children left home in 2016
network_left2<- data_left %>%
  dplyr::select("deprc2_6","deprc2_7", "deprc2_11", "deprc2_12", "deprc2_14","deprc2_16","deprc2_18","deprc2_20",
                "deprf2_6","deprf2_7", "deprf2_11", "deprf2_12", "deprf2_14","deprf2_16","deprf2_18","deprf2_20",
                "deprm2_6","deprm2_7", "deprm2_11", "deprm2_12", "deprm2_14","deprm2_16","deprm2_18","deprm2_20")
names(network_left2) <- c("dc1", "dc2","dc3","dc4","dc5","dc6","dc7","dc8",
                  "df1", "df2","df3","df4","df5","df6","df7","df8",
                  "dm1", "dm2","dm3","dm4","dm5","dm6","dm7","dm8")
library("bootnet")
groups <- factor(c(rep("C", 8), rep("F",8), rep("M",8)))
network_left2_glasso <- bootnet::estimateNetwork(network_left2, default = "EBICglasso") 
network_left2_p <- plot(network_left2_glasso,gamma = 0.1,
                        title="2012 child left home in 2016", 
                        maximum=.47,
                        labels = TRUE,
                        #pie = pred1_all$error$R2,
                        layout = "spring",
                        groups = groups,
                        color = c("lightblue", "lightsalmon","antiquewhite"),
                        vsize=4, label.cex =2,
                        legend.cex = .35,
                        border.width=2, border.color='#555555')      # max=0.47

#2016 child left home in 2016
network_left6<- data_left %>%
  dplyr::select("deprc6_6","deprc6_7", "deprc6_11", "deprc6_12", "deprc6_14","deprc6_16","deprc6_18","deprc6_20",
                "deprf6_6","deprf6_7", "deprf6_11", "deprf6_12", "deprf6_14","deprf6_16","deprf6_18","deprf6_20",
                "deprm6_6","deprm6_7", "deprm6_11", "deprm6_12", "deprm6_14","deprm6_16","deprm6_18","deprm6_20")
names(network_left6) <- c("dc1", "dc2","dc3","dc4","dc5","dc6","dc7","dc8",
                          "df1", "df2","df3","df4","df5","df6","df7","df8",
                          "dm1", "dm2","dm3","dm4","dm5","dm6","dm7","dm8")
network_left6_glasso <- bootnet::estimateNetwork(network_left6, default = "EBICglasso") 
network_left6_p <- plot(network_left6_glasso,gamma = 0.1,
                        title="2016 child left home in 2016", 
                        maximum=.47,
                        labels = TRUE,
                        #pie = pred1_all$error$R2,
                        layout = "spring",
                        groups = groups,
                        color = c("lightblue", "lightsalmon","antiquewhite"),
                        vsize=4, label.cex =2,
                        legend.cex = .35,
                        border.width=2, border.color='#555555')      # max=0.47

##2012 child stay home in 2016
network_stay2<- data_stay %>%
  dplyr::select("deprc2_6","deprc2_7", "deprc2_11", "deprc2_12", "deprc2_14","deprc2_16","deprc2_18","deprc2_20",
                "deprf2_6","deprf2_7", "deprf2_11", "deprf2_12", "deprf2_14","deprf2_16","deprf2_18","deprf2_20",
                "deprm2_6","deprm2_7", "deprm2_11", "deprm2_12", "deprm2_14","deprm2_16","deprm2_18","deprm2_20")
names(network_stay2) <- c("dc1", "dc2","dc3","dc4","dc5","dc6","dc7","dc8",
                          "df1", "df2","df3","df4","df5","df6","df7","df8",
                          "dm1", "dm2","dm3","dm4","dm5","dm6","dm7","dm8")
network_stay2_glasso <- bootnet::estimateNetwork(network_stay2, default = "EBICglasso") 
network_stay2_p <- plot(network_stay2_glasso, gamma = 0.1,
                        title="2012 child stay home in 2016", 
                        maximum=.47,
                        labels = TRUE,
                        #pie = pred1_all$error$R2,
                        layout = "spring",
                        groups = groups,
                        color = c("lightblue", "lightsalmon","antiquewhite"),
                        vsize=4, label.cex =2,
                        legend.cex = .35,
                        border.width=2, border.color='#555555')      # max=0.47

#2016 child stay home in 2016
network_stay6<- data_stay %>%
  dplyr::select("deprc6_6","deprc6_7", "deprc6_11", "deprc6_12", "deprc6_14","deprc6_16","deprc6_18","deprc6_20",
                "deprf6_6","deprf6_7", "deprf6_11", "deprf6_12", "deprf6_14","deprf6_16","deprf6_18","deprf6_20",
                "deprm6_6","deprm6_7", "deprm6_11", "deprm6_12", "deprm6_14","deprm6_16","deprm6_18","deprm6_20")
names(network_stay6) <- c("dc1", "dc2","dc3","dc4","dc5","dc6","dc7","dc8",
                          "df1", "df2","df3","df4","df5","df6","df7","df8",
                          "dm1", "dm2","dm3","dm4","dm5","dm6","dm7","dm8")
network_stay6_glasso <- bootnet::estimateNetwork(network_stay6, default = "EBICglasso") 
network_stay6_p <- plot(network_stay6_glasso,gamma = 0.1,
                        title="2016 child stay home in 2016", 
                        maximum=.47,
                        labels = TRUE,
                        #pie = pred1_all$error$R2,
                        layout = "spring",
                        groups = groups,
                        color = c("lightblue", "lightsalmon","antiquewhite"),
                        vsize=4, label.cex =2,
                        legend.cex = .35,
                        border.width=2, border.color='#555555')      # max=0.47
#NetworkComparisonTest
if(!require(NetworkComparisonTest)) {install.packages("NetworkComparisonTest",repos = "http://cran.us.r-project.org"); require(NetworkComparisonTest)}
library("NetworkComparisonTest")

#compare network_left2 and network_left6
NCT(network_left2, network_left6, 
    gamma= 0.1,   it = 100, binary.data=FALSE, 
    paired=FALSE, weighted=TRUE, AND=TRUE, abs=TRUE,
    test.edges=FALSE, edges="all", 
    progressbar=TRUE, make.positive.definite=TRUE,
    p.adjust.methods= c("none"), 
    test.centrality=FALSE, 
    centrality=c("strength"),nodes="all",
    communities=NULL,useCommunities="all",
    verbose = TRUE)
NCT(network_stay2, network_stay6,
    gamma=0.1, it = 100, binary.data=FALSE, 
    paired=FALSE, weighted=TRUE, AND=TRUE, abs=TRUE,
    test.edges=FALSE, edges="all", 
    progressbar=TRUE, make.positive.definite=TRUE,
    p.adjust.methods= c("none"), 
    test.centrality=FALSE, 
    centrality=c("strength"),nodes="all",
    communities=NULL, useCommunities="all",
    verbose = TRUE)

#smallworldness
smallworldness(network_left2_glasso)
smallworldness(network_left6_glasso)
smallworldness(network_stay2_glasso)
smallworldness(network_stay6_glasso)
