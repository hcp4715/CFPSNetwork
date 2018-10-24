################## Preprocessing CFPS child data ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    18-10-24         The first version
# 
# 
#
###### input######
# [CFPS Public Data] 2010 child Data (ENG).tab
#
#
#
###### output #####
#sesMHchild.csv
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
dfc <- read.table("/Users/apple/Desktop/CFPS/4_Analysis/[CFPS Public Data] 2010 Child Data (ENG).tab", sep="\t",header=T)#adult 2010
summary(dfc == -8)

#ID information
##to distinguish child from adult, data and variable are coded as e.g.persinfoc
persinfoc <- dfc %>%
  dplyr::select(gender, wa1age, fid, pid, childgroup)
summary(persinfoc)
persinfoc[persinfoc == -8] <-NA
##NOTICE: childgroup = 1, age<1; 2, 1<=age<3; 3, 3<=age<6; 4, 6<=age<16
#age:10-15
child10_15 <- dfc %>%
  dplyr::filter(wa1age >= 10) #select children older than 10(with both 自答 and 代答)

##SES: human
hCapc <- child10_15 %>%
  dplyr::select(edu2010, wordtest, mathtest, wh9, wd2) # education level in 2010, wordtest, mathtest, education expactancy of child, education expactancy of child's parents
hCapc$wh9[hCap$wh9 == 9] <- 1 # education expectancy, 9=不必念书, 2-8=小学-博士
hCapc[hCap < 0 ] <- NA
summary(hCap)
##SES: material
mCapc <- child10_15 %>%
  dplyr::select(moccupisco, foccupiso) # mother occupation, father occupation, family income
父母的教育期望：d2
attri: 家长E4,自答m401-m407
访员观察：z201理解能力，z207智力
目标：n501-n511
抑郁：n401-n406，fdepression
m302幸福
m303未来信心
m301，m304人际交往
？只有一年年龄段的儿童有的数据暂时忽略（如价值观？）
pol：mparty，fparty
父母职业：moccupisco，foccupiso