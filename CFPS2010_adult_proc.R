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


##NOTICE: over 10000 missing values for occupation coding and 1865 for income, thousands missing for attributional style and fairness
data.complete <- data %>%     
  dplyr::filter(complete.cases(data)) #%>% # only the complete cases (i.e., no NA in every row) 
  dplyr::mutate(kinsupport = sib + parent+ spouse + children + dep_c + qz204 + fc1,
                nonkinsupport = sup_edu + sup_rel + sup_commu +  sup_social  +  sup_net+qg3+qg405) %>%
  dplyr::select( educ, wordtest, mathtest, qg307isco, qg307isei, income, qa7_s_1, qm403, qm404, qk802,
                 qq601, qq602, qq603, qq604,qq605,qq606,
                 migrant, fairsum, kinsupport, nonkinsupport, pid, fid, qa1age, gender)
summary(data.complete)
# label for the nodes of network
#labels <- c("educ", "word", "math", "occu1", "occu2", "income", "pol",
            "satis","confi", "happi", "depressed", "nervous", "angry", "hopless", "hard", "meaningless", 
            "migrant", "fair", "kinsup", "nonkinsup")
            #"sk_sib", "sk_par", "sk_spou", "sk_c", "sk_dep", "sk_famhor", "sk_rela", 
            #"sn_edu", "sn_rel", "sn_commu", "sn_social", "sn_net", "sn_emp", "sn_super")
##NOTICE: data.complete has 10276 obs
#generate partial correlation network of all the data
mentalhealth <- data %>%
  dplyr::filter(complete.cases(data)) %>%
  dplyr::select(qm403, qm404, qk802, qq601, qq602, qq603, qq604, qq605, qq606) 
labels <- c("satis","confi", "happi", "depressed", "nervous", "angry", "hopless", "hard", "meaningless")
groups <- factor(c(rep("others", 3), rep("depression", 6)))
netPcor <- qgraph::qgraph(cor(mentalhealth), layout = "spring", 
                          labels = labels, groups = groups, graph = "concentration")
# export image
jpeg(file = "all.jpeg")
plot(netPcor)
dev.off()

# generate partial correlation network of male, 30-50
data.male <- data.complete %>%
  dplyr::filter(gender == 1) %>% # male
  dplyr::filter(qa1age >= 30 & qa1age <=50) %>% # age:30-50
  dplyr::group_by(fid) %>% 
  dplyr::filter(pid == max(pid)) %>%  # one each family
  dplyr::select(-pid, -gender, -qa1age)  # drop the columns for later analysis 
data.male$fid <- NULL
summary(data.male)

groups <- factor(c(rep("hCap", 3), rep("mCap", 3), rep("pCap", 1), 
                   rep("MH", 9), rep("migrant", 1), rep("fair", 1), rep("social support", 2)))
netPcor_m <- qgraph(cor(data.male), layout = "spring", 
                    labels = labels, groups = groups, graph = "concentration")

#LASSO-male
set.seed(100)
adls <- adalasso.net(mentalhealth) 
network <- as.matrix(forceSymmetric(adls$pcor.adalasso)) 
lasso <- qgraph(network, layout = "spring", labels = labels, groups = groups)



qgraph::smallworldness(lasso)
# Centrality
centrality <- centrality_auto(netPcor)
nc <- centrality$node.centrality
ebc <- centrality$edge.betweenness.centrality
central <- centralityPlot(netPcor)


#cluster
clustcoef <- clustcoef_auto(netPcor)
cluster <- clusteringPlot(netPcor, signed = TRUE)

# generate partial correlation network of female, 30-50
data.female <- data.complete %>%
  dplyr:: filter(gender == 0) %>% # male
  dplyr::filter(qa1age >= 30 & qa1age <= 50) %>% # age:30-50
  dplyr::group_by(fid) %>% 
  dplyr::filter(pid == max(pid)) %>% # one each family
  dplyr::select(-pid, -gender, -qa1age) # drop the columns for later analysis
data.female$fid <- NULL

groups <- factor(c(rep("hCap", 3), rep("mCap", 3), rep("pCap", 1), 
                   rep("MH", 9), rep("migrant", 1), rep("fair", 1), rep("social support", 2)))
netPcor <- qgraph(cor(data.female), layout = "spring", labels = labels,
                  groups = groups, graph = "concentration")

#LASSO-female
set.seed(100)
adls <- adalasso.net(mentalhealth) 
network <- as.matrix(forceSymmetric(adls$pcor.adalasso)) 
lasso <- qgraph(mentalhealth, layout = "spring", labels = labels, groups = groups)

# Centrality
centrality <- centrality_auto(netPcor)
nc <- centrality$node.centrality
ebc <- centrality$edge.betweenness.centrality
central <- centralityPlot(netPcor)

#cluster
clustcoef <- clustcoef_auto(netPcor)
cluster <- clusteringPlot(netPcor, signed = TRUE)