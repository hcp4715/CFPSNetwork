################## Preprocessing CFPS adult data ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    18-09-29         The first version
# Hu, C-P     18-09-30         Add notes, generalizing to other env.
# Cai, Y-Q    18-10-09         Only include SES and mental health, ignore physical ones
# Cai, Y-Q    18-10-21         Add fairness and attributional styles
# Hu, C-P     18-10-22         Review the code, change the absolute path to relative path, and other comments
# Cai, Y-Q    18-10-24         Confirm the file and change some dimensions
###### input######
# [CFPS Public Data] 2010 Adult Data (ENG).tab
# [CFPS Public Data] 2010 Family Data (ENG).tab
# [CFPS Public Data] 2010 Family Roster Data (ENG).tab

###### output #####
#sesMH.csv
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
dfa <- read.table("/Users/apple/Desktop/CFPS/4_Analysis/[CFPS Public Data] 2010 Adult Data (ENG).tab", sep="\t",header=T)#adult 2010

###### get related variables
#ID information
persinfo <- dfa %>%
  dplyr::select(gender, qa1age)

#summarize personal informations
summary(dfa$qa1age)  #age
summary(dfa$gender)  #gender
summary(dfa$qa2==1)  #hukou: agriculture
summary(dfa$qa2==3)  #hukou: non-agriculture

# Select and recode SES
SES <- dfa %>%
  dplyr::select(fid, pid, educ, wordtest, mathtest, qg307isco, qg307isei, qk601,qa7_s_1) %>%
  dplyr::mutate(qa7_s_1 = recode_factor(qa7_s_1, '1' = 1, .default = 0)) #leave only communist members
SES[SES == -8] <- NA #missing values
summary(SES) # check SES

# set qa7_s_1 = 1 if any family member is a communist party member, else = 0
pCap <- SES %>%
  dplyr::select(qa7_s_1, fid) %>%
  dplyr::mutate(qa7_s_1 = recode_factor(qa7_s_1, '1' = 1, .default = 0)) %>%
  dplyr::filter(qa7_s_1 == 1)
SES <- SES %>%
  dplyr::select(-qa7_s_1) %>%
  dplyr::left_join(pCap, SES, by = "fid")
SES$qa7_s_1[is.na(SES$qa7_s_1)] <- 0

#delate repatitive rows
SES <- SES %>%
  dplyr::group_by(pid) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::ungroup()

# recode income using log10, if income =0, recode it as 0 
SES <- SES %>%
  dplyr::mutate(qk601 = log10(qk601)) %>%
  dplyr::mutate(qk601 = recode_factor(qk601, '-Inf' = 0 ))

# select and recode mental health, fairness and attributional style
##MH
MH <- dfa %>%
  dplyr::select(qm403, qm404, qk802, depression)
MH[MH == -8] <- NA
MH$qm403[MH$qm403 <0] <- NA
MH$qm404[MH$qm404 <0] <- NA
summary(MH)

# reliability of depression scale
depr <- dfa %>%
  dplyr::select(qq601, qq602, qq603, qq604, qq605, qq606)
depr[depr == -8 ] <- NA
depr[depr == -2 ] <- NA
depr[depr == -1] <- NA
summary(depr)
psych::alpha(depr)
depr <- depr %>%
  dplyr::filter(complete.cases(depr))
#???
stats::factanal(depr, factors=2) 
scree(depr)

##fairness
fair <- dfa %>%
  dplyr::select(qn201, qn202, qn203, qn204, qn205, qn206, qn207, qn208) 
fair[fair < 0] <- NA
fair[fair > 5] <- NA
fair[fair == 5] <- 0 # encounter with unfair affairs? 1 = yes, 5 = no
scree(fair)
summary(fair)
library(psych)
fair  %>%
  dplyr::filter(complete.cases(fair)) %>%
  # factor analysis?
  stats::factanal(fair, factors=4)
# reliability of fairness scale
psych::alpha(fair)

# sum of fairness, add to MH
MH$fairsum <- rowSums(fair)

##attribute
attri <- dfa %>% 
  dplyr::select(qn501, qn502, qn503, qn504, qn505, qn506, qn507)  # 1-4, strongly disagree - strongly agree
summary(attri)

### recode attribute 1-4= strongly diagree, disagree, agree, strongly agree, 5=not agree not disagree, 6=i don't know, -8=missing value
attri <- attri %>%
  dplyr::mutate(qn501 = recode(qn501, '1' = -2, '2' = -1 ,'5' = 0, '3' = 1, '4' = 2, .default = -8),
                qn502 = recode(qn502, '1' = -2, '2' = -1 ,'5' = 0, '3' = 1, '4' = 2, .default = -8),
                qn503 = recode(qn503, '1' = -2, '2' = -1 ,'5' = 0, '3' = 1, '4' = 2, .default = -8),
                qn504 = recode(qn504, '1' = -2, '2' = -1 ,'5' = 0, '3' = 1, '4' = 2, .default = -8),
                qn505 = recode(qn505, '1' = -2, '2' = -1 ,'5' = 0, '3' = 1, '4' = 2, .default = -8),
                qn506 = recode(qn506, '1' = -2, '2' = -1 ,'5' = 0, '3' = 1, '4' = 2, .default = -8),
                qn507 = recode(qn507, '1' = -2, '2' = -1 ,'5' = 0, '3' = 1, '4' = 2, .default = -8))
attri  %>%
  dplyr::filter(complete.cases(attri)) %>%
  # factor analysis?
  stats::factanal(attri, factors=2) 
# reliability of fairness scale
psych::alpha(attri)


#reliability of attribute scale
psych::alpha(attri)

# combine all the related data
all <- base::cbind(persinfo, SES, MH, attri)
# write the table
write.csv(all, file = "sesMH.csv", row.names = FALSE)
