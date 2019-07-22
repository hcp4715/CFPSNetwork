################## Processing CFPS 2012 child proc ##############################
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
data2012 <- read.csv("/Users/apple/Desktop/大三下/CFPSNetwork/data2012.csv", header=T)     
summary(data2012) 
info <- data2012 %>%
  dplyr::select(pid, fid, pid_m, pid_f, agec2, agem2, agef2, gender)
summary(info)

####data process about SES, not used yet
#SES <- data2012 %>%
#  dplyr::select(eduf2, edum2, incomef2, incomem2, fparty2, mparty2, sSESf2_1, sSESf2_2, sSESm2_1, sSESm2_2)
#summary(SES)

#SES <- SES %>%
#  dplyr::mutate(edulevelf2 = recode_factor(eduf2, "1" = 0, "2" = 0, "3" = 0, .default = 1),
#                edulevelm2 = recode_factor(edum2, "1" = 0, "2" = 0, "3" = 0, .default = 1))
#SES$incomelevelf2[SES$incomef2 <= mean(SES$incomef2, na.rm = TRUE)] <- 0
#SES$incomelevelf2[SES$incomef2 > mean(SES$incomef2, na.rm = TRUE)] <- 1
#SES$incomelevelm2[SES$incomem2 <= mean(SES$incomem2, na.rm = TRUE)] <- 0
#SES$incomelevelm2[SES$incomem2 > mean(SES$incomem2, na.rm = TRUE)] <- 1
#SES <- SES %>%
#  dplyr::mutate(edulevelf2 = recode(eduf2, "1" = 0, "2" = 0, "3" = 0, .default = 1),
#                edulevelm2 = recode(edum2, "1" = 0, "2" = 0, "3" = 0, .default = 1))%>%
#  dplyr::mutate(SESlevelf2 = edulevelf2 + incomelevelf2,
#                SESlevelm2 = edulevelm2 + incomelevelm2)
#
MH <- data2012 %>%
  dplyr::select(deprf2_1, deprf2_2,deprf2_3,deprf2_4,deprf2_5,deprf2_6,deprf2_7,deprf2_8,deprf2_9,deprf2_10,
                deprf2_11, deprf2_12,deprf2_13,deprf2_14,deprf2_15,deprf2_16,deprf2_17,deprf2_18,deprf2_19,deprf2_20,
                deprm2_1, deprm2_2,deprm2_3,deprm2_4,deprm2_5,deprm2_6,deprm2_7,deprm2_8,deprm2_9,deprm2_10,
                deprm2_11, deprm2_12,deprm2_13,deprm2_14,deprm2_15,deprm2_16,deprm2_17,deprm2_18,deprm2_19,deprm2_20,
                deprc2_1, deprc2_2,deprc2_3,deprc2_4,deprc2_5,deprc2_6,deprc2_7,deprc2_8,deprc2_9,deprc2_10,
                deprc2_11, deprc2_12,deprc2_13,deprc2_14,deprc2_15,deprc2_16,deprc2_17,deprc2_18,deprc2_19,deprc2_20,
                satisf2, confif2, satism2, confim2, depr_sumc2, depr_sumf2, depr_summ2, posi_sumf2, posi_summ2)
MH <-drop_na(MH)                
summary(MH)

###################################################
###################################################
#network analysis
################independent group
#compare mental health network between children, father, mother
#"children" does not have positive mental health items, therefore, only compare depression part
#father
MH_f <- MH %>%
  dplyr::select(deprf2_1, deprf2_2,deprf2_3,deprf2_4,deprf2_5,deprf2_6,deprf2_7,deprf2_8,deprf2_9,deprf2_10,
                deprf2_11, deprf2_12,deprf2_13,deprf2_14,deprf2_15,deprf2_16,deprf2_17,deprf2_18,deprf2_19,deprf2_20)
#partial correlation
labels <- c("df1", "df2", "df3", "df4", "df5", "df6", "df7", "df8", "df9", "df10",
            "df11", "df12", "df13", "df14", "df15", "df16", "df17", "df18", "df19", "df20")
groups <- factor(c(rep("depr_f", 20)))
netPcor <- qgraph::qgraph(cor(MH_f), layout = "spring", labels = labels, groups = groups, graph = "pcor")
#lasso
netLASSO <- qgraph::qgraph(cor(MH_f), layout = "spring", labels = labels, groups = groups, graph = "glasso", sampleSize = 2105)

#mother
    MH_m <- MH %>%
      dplyr::select(deprm2_1, deprm2_2,deprm2_3,deprm2_4,deprm2_5,deprm2_6,deprm2_7,deprm2_8,deprm2_9,deprm2_10,
                    deprm2_11, deprm2_12,deprm2_13,deprm2_14,deprm2_15,deprm2_16,deprm2_17,deprm2_18,deprm2_19,deprm2_20)
    #partial correlation
    labels <- c("dm1", "dm2", "dm3", "dm4", "dm5", "dm6", "dm7", "dm8", "dm9", "dm10",
                "dm11", "dm12", "dm13", "dm14", "dm15", "dm16", "dm17", "dm18", "dm19", "dm20")
    
    groups <- factor(c(rep("depr_m", 20)))
    netPcor <- qgraph::qgraph(cor(MH_m), layout = "spring", labels = labels, groups = groups, graph = "pcor")
#lasso
netLASSO <- qgraph::qgraph(cor(MH_m), layout = "spring", labels = labels, groups = groups, graph = "glasso", sampleSize = 2105)

#children
MH_c <- MH %>%
  dplyr::select(deprc2_1, deprc2_2,deprc2_3,deprc2_4,deprc2_5,deprc2_6,deprc2_7,deprc2_8,deprc2_9,deprc2_10,
                deprc2_11, deprc2_12,deprc2_13,deprc2_14,deprc2_15,deprc2_16,deprc2_17,deprc2_18,deprc2_19,deprc2_20)
#partial correlation
labels <- c("dc1", "dc2", "dc3", "dc4", "dc5", "dc6", "dc7", "dc8", "dc9", "dc10",
            "dc11", "dc12", "dc13", "dc14", "dc15", "dc16", "dc17", "dc18", "dc19", "dc20",
            "pc1", "pc2")

groups <- factor(c(rep("depr_c", 20)))
netPcor <- qgraph::qgraph(cor(MH_c), layout = "spring", labels = labels, groups = groups, graph = "pcor")
#lasso
netLASSO <- qgraph::qgraph(cor(MH_c), layout = "spring", labels = labels, groups = groups, graph = "glasso", sampleSize = 2105)

################transimission
#father-mother-children
#partial correlation
labels <- c("df1", "df2", "df3", "df4", "df5", "df6", "df7", "df8", "df9", "df10",
            "df11", "df12", "df13", "df14", "df15", "df16", "df17", "df18", "df19", "df20",
            "dm1", "dm2", "dm3", "dm4", "dm5", "dm6", "dm7", "dm8", "dm9", "dm10",
            "dm11", "dm12", "dm13", "dm14", "dm15", "dm16", "dm17", "dm18", "dm19", "dm20",
            "dc1", "dc2", "dc3", "dc4", "dc5", "dc6", "dc7", "dc8", "dc9", "dc10",
            "dc11", "dc12", "dc13", "dc14", "dc15", "dc16", "dc17", "dc18", "dc19", "dc20",
            "pf1", "pf2", "pm1", "pm2")

groups <- factor(c(rep("depr_f", 20), rep("depr_m", 20), rep("depr_c", 20), rep("posi_f", 2), rep("posi_m", 2)))
netPcor <- qgraph::qgraph(cor(MH), layout = "spring", labels = labels, groups = groups, graph = "pcor")
#lasso
netLASSO <- qgraph::qgraph(cor(MH), layout = "spring", labels = labels, groups = groups, graph = "glasso", sampleSize = 2105)

#father-child
MH_fc <- MH %>%
  dplyr::select(deprf2_1, deprf2_2,deprf2_3,deprf2_4,deprf2_5,deprf2_6,deprf2_7,deprf2_8,deprf2_9,deprf2_10,
                deprf2_11, deprf2_12,deprf2_13,deprf2_14,deprf2_15,deprf2_16,deprf2_17,deprf2_18,deprf2_19,deprf2_20,
                deprc2_1, deprc2_2,deprc2_3,deprc2_4,deprc2_5,deprc2_6,deprc2_7,deprc2_8,deprc2_9,deprc2_10,
                deprc2_11, deprc2_12,deprc2_13,deprc2_14,deprc2_15,deprc2_16,deprc2_17,deprc2_18,deprc2_19,deprc2_20)
labels <- c("df1", "df2", "df3", "df4", "df5", "df6", "df7", "df8", "df9", "df10",
            "df11", "df12", "df13", "df14", "df15", "df16", "df17", "df18", "df19", "df20",
            "dc1", "dc2", "dc3", "dc4", "dc5", "dc6", "dc7", "dc8", "dc9", "dc10",
            "dc11", "dc12", "dc13", "dc14", "dc15", "dc16", "dc17", "dc18", "dc19", "dc20",
            "pf1", "pf2")
groups <- factor(c(rep("depr_f", 20),  rep("depr_c", 20), rep("posi_f", 2)))
netPcor <- qgraph::qgraph(cor(MH_fc), layout = "spring", labels = labels, groups = groups, graph = "pcor")
#lasso
netLASSO <- qgraph::qgraph(cor(MH_fc), layout = "spring", labels = labels, groups = groups, graph = "glasso", sampleSize = 2105)

#mother-child
MH_mc <- MH %>%
  dplyr::select(deprm2_1, deprm2_2,deprm2_3,deprm2_4,deprm2_5,deprm2_6,deprm2_7,deprm2_8,deprm2_9,deprm2_10,
                deprm2_11, deprm2_12,deprm2_13,deprm2_14,deprm2_15,deprm2_16,deprm2_17,deprm2_18,deprm2_19,deprm2_20,
                deprc2_1, deprc2_2,deprc2_3,deprc2_4,deprc2_5,deprc2_6,deprc2_7,deprc2_8,deprc2_9,deprc2_10,
                deprc2_11, deprc2_12,deprc2_13,deprc2_14,deprc2_15,deprc2_16,deprc2_17,deprc2_18,deprc2_19,deprc2_20,
                satism2, confim2)
labels <- c("dm1", "dm2", "dm3", "dm4", "dm5", "dm6", "dm7", "dm8", "dm9", "dm10",
            "dm11", "dm12", "dm13", "dm14", "dm15", "dm16", "dm17", "dm18", "dm19", "dm20",
            "dc1", "dc2", "dc3", "dc4", "dc5", "dc6", "dc7", "dc8", "dc9", "dc10",
            "dc11", "dc12", "dc13", "dc14", "dc15", "dc16", "dc17", "dc18", "dc19", "dc20",
            "pm1", "pm2")
groups <- factor(c(rep("depr_m", 20),  rep("depr_c", 20), rep("posi_m", 2)))
netPcor <- qgraph::qgraph(cor(MH_mc), layout = "spring", labels = labels, groups = groups, graph = "pcor")
#lasso
netLASSO <- qgraph::qgraph(cor(MH_mc), layout = "spring", labels = labels, groups = groups, graph = "glasso", sampleSize = 2105)


# Centrality
centrality <- centrality_auto(netLASSO)
nc <- centrality$node.centrality
ebc <- centrality$edge.betweenness.centrality
central <- centralityPlot(netLASSO)

#cluster
clustcoef <- clustcoef_auto(network)
cluster <- clusteringPlot(network, signed = TRUE)

