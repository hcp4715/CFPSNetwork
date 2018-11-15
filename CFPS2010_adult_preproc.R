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
dfa <- read.table("data/[CFPS Public Data] 2010 Adult Data (ENG).tab", sep="\t",header=T) #adult 2010
dff <- read.table("data/[CFPS Public Data] 2010 Family Data (ENG).tab", sep="\t",header=T) #family 2010
###### get related variables
#ID information
persinfo <- dfa %>%
  dplyr::select(gender, qa1age)

#summarize personal informations
summary(dfa$qa1age)  #age
table(dfa$gender)    #gender: 0 = female; 1 = male.
table(dfa$qa2)       #hukou: 1 = agriculture 3 = non-agriculture; other numbers = not appliable

# Select and recode SES
SES <- dfa %>%
  dplyr::select(fid, pid, educ, wordtest, mathtest, qg307isco, qg307isei, qk601, qa7_s_1) %>%
  dplyr::mutate(qa7_s_1 = recode_factor(qa7_s_1, '1' = 1, .default = 0)) #leave only communist members
SES[SES == -8] <- NA #missing values
summary(SES) # check SES

####### political captial #######
# set qa7_s_1 = 1 if any family member is a communist party member, else = 0
pCap <- SES %>%
  dplyr::select(qa7_s_1, fid) %>%
  dplyr::mutate(qa7_s_1 = recode_factor(qa7_s_1, '1' = 1, .default = 0)) %>%
  dplyr::filter(qa7_s_1 == 1)
SES <- SES %>%
  dplyr::select(-qa7_s_1) %>%
  dplyr::left_join(pCap, SES, by = "fid")
SES$qa7_s_1[is.na(SES$qa7_s_1)] <- 0

# delate repatitive rows
SES <- SES %>%
  dplyr::group_by(pid) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::ungroup()

# recode income using log10, if income =0, recode it as 0 
# hcp: plot the raw data using hist() or density() to have a look first. 
SES <- SES %>%
  dplyr::mutate(income = log10(qk601)) %>%
  dplyr::mutate(income = recode_factor(income, '-Inf' = 0 )) %>%
  dplyr::select(-qk601)

# select and recode mental health, fairness and attributional style
##MH
MH <- dfa %>%
  dplyr::select(qm403, qm404, qk802, qq601, qq602, qq603, qq604, qq605, qq606)
MH[MH < 0] <- NA # MH = -8/-2 not appliable
summary(MH)

# reliability of depression scale
depr <- dfa %>%
  dplyr::select(qq601, qq602, qq603, qq604, qq605, qq606)
depr[depr == -8 ] <- NA
depr[depr == -2 ] <- NA
depr[depr == -1] <- NA
summary(depr)
psych::alpha(depr)

##fairness
fair <- dfa %>%
  dplyr::select(qn201, qn202, qn203, qn204, qn205, qn206, qn207, qn208) 
fair[fair < 0] <- NA
fair[fair > 5] <- NA
fair[fair == 5] <- 0 # encounter with unfair affairs? 1 = yes, 5 = no
fair <- fair %>%
  dplyr::mutate(fairsum = qn201+qn202+qn203+qn204+qn205+qn206+qn207+qn208) %>%
  dplyr::select(fairsum)
library(psych)

# reliability of fairness scale
psych::alpha(fair)

#migrant
migr <- dfa %>%
  dplyr::select(qa3, qa4) %>% #moved when 3; moved when 12
  dplyr::mutate(migrant = qa3 + qa4) %>%
  dplyr::mutate(migrant = recode(migrant, "1" = 1, "2" = 1, .default = 0)) %>%
  dplyr::select(migrant)
  
#social support
##kin support
sup_kin1 <- dfa %>%
  dplyr::select(fid, pid, qb304_a_1, qb304_a_2, qb304_a_3, qb304_a_4, qb304_a_5, qb304_a_6, qb304_a_7, qb304_a_8, qb304_a_9, qb304_a_10, qb304_a_11, qb304_a_12, qb304_a_13, qb304_a_14, qb304_a_15, #siblings alive
                qb411_s_1, #father alive
                qb511_s_1, #mother alive
                qe1, #mariage status 1=Never married 2=Married 3=Cohabitation 4=Divorced 5=Widowed 
                qf1_a_1,qf1_a_2, qf1_a_3, qf1_a_4, qf1_a_5, qf1_a_6, qf1_a_7, qf1_a_8, qf1_a_9, qf1_a_10, #children number
                qm3, qm301, qm302, qm303, qm304, #dependence on children 1 or 0
                qz204) %>% #family harmony 1-7 
  dplyr::mutate(qb411_s_1 = recode(qb411_s_1, '-8' = 0, .default = 1),
                qb511_s_1 = recode(qb511_s_1, '-8' = 0, .default = 1))

sup_kin2 <- dff %>% #relatives visited in Spring festival
  dplyr::select(fid, fc1)
sup_kin2[sup_kin2 < 0] <- NA

#join all social support, kin
sup_kin <- sup_kin1 %>%
  left_join(sup_kin2, sup_kin1, by = "fid")

#replace NA with 0
sup_kin[sup_kin < 0] <- 0
        
#recode kin support
sup_kin <- sup_kin %>%
  dplyr::mutate(sib = qb304_a_1+ qb304_a_2+ qb304_a_3 +qb304_a_4 + qb304_a_5 +  qb304_a_6 +  qb304_a_7 + qb304_a_8 + qb304_a_9 + qb304_a_10 + qb304_a_11 + qb304_a_12 + qb304_a_13 +  qb304_a_14 +  qb304_a_15) %>% #sib = number of siblings
  dplyr::mutate(parent =  qb411_s_1 + qb511_s_1) %>% # parents alive
  dplyr::mutate(spouse = recode(qe1, '2' = 1, .default = 0)) %>% # with spouse or not
  dplyr::mutate(children = qf1_a_1 + qf1_a_2 + qf1_a_3 + qf1_a_4+qf1_a_5+ qf1_a_6+ qf1_a_7+ qf1_a_8+ qf1_a_9+ qf1_a_10) %>%
  dplyr::mutate(dep_c1 = recode(qm3, '2' = 1, '4' = 1, .default = 0),
                dep_c2 = recode(qm301, '2' = 1, '4' = 1, .default = 0),
                dep_c3 = recode(qm302, '2' = 1, '4' = 1, .default = 0),
                dep_c4 = recode(qm303, '2' = 1, '4' = 1, .default = 0),
                dep_c5 = recode(qm304, '2' = 1, '4' = 1, .default = 0)) %>%
  dplyr::mutate(dep_c = dep_c1 + dep_c2 + dep_c3 + dep_c4 + dep_c5) %>% #dependence on children, 2 = daughter, 4 = son
  dplyr::select(sib, parent, spouse, children, dep_c, qz204, fc1)
summary(sup_kin)

##non-kin support
sup_nonkin <- dfa %>%
  dplyr::select(kt303_a_1, kt303_a_2, #informal education
         kt408_a_1, kt408_a_2, #religious activity
         kt407_a_1, kt407_a_2, #community service
         kt406_a_1, kt406_a_2, #social activity
         qa7_s_1, qa7_s_2, qa7_s_3,qa7_s_4, #networking organization
         qg3, qg405)  %>% #employed & supervise other people
  dplyr::mutate(sup_edu = kt303_a_1 + kt303_a_2, #recode
         sup_rel = kt408_a_1 + kt408_a_2,
         sup_commu = kt407_a_1 + kt407_a_2,
         sup_social = kt406_a_1 + kt406_a_2,
         qa7_s_1 = recode(qa7_s_1, '9' = 1, .default = 0),
         qa7_s_2 = recode(qa7_s_2, '9' = 1, .default = 0),
         qa7_s_3 = recode(qa7_s_3, '9' = 1, .default = 0),
         qa7_s_4 = recode(qa7_s_4, '9' = 1, .default = 0),
         qg3 = recode(qg3, '1' = 1, .default = 0),
         qg405 = recode(qg405, '1' = 1, .default = 0)) %>%
  dplyr::select(sup_edu, sup_rel, sup_commu, sup_social, qa7_s_1, qa7_s_2, qa7_s_3, qa7_s_4, qg3, qg405) %>% #select recoded
  dplyr::mutate(sup_edu = recode(sup_edu, '-16' = 0, .default = 1), #recode rest
                sup_rel = recode(sup_rel, '-16' = 0, .default = 1),
                sup_commu = recode(sup_commu, '-16' = 0, .default = 1),
                sup_social = recode(sup_social, '-16' = 0, .default = 1),
                sup_net = qa7_s_1 + qa7_s_2 + qa7_s_3 + qa7_s_4) %>%
  dplyr::select(sup_edu, sup_rel, sup_commu, sup_social, sup_net, qg3, qg405)


##attribute
#attri <- dfa %>% 
#  dplyr::select(qn501, qn502, qn503, qn504, qn505, qn506, qn507)  # 1-4, strongly disagree - strongly agree
#summary(attri)

### recode attribute 1-4= strongly diagree, disagree, agree, strongly agree, 5=not agree not disagree, 6=i don't know, -8=missing value
#attri <- attri %>%
# dplyr::mutate(qn501 = recode(qn501, '1' = -2, '2' = -1 ,'5' = 0, '3' = 1, '4' = 2, .default = -8),
#                qn502 = recode(qn502, '1' = -2, '2' = -1 ,'5' = 0, '3' = 1, '4' = 2, .default = -8),
#                qn503 = recode(qn503, '1' = -2, '2' = -1 ,'5' = 0, '3' = 1, '4' = 2, .default = -8), #               qn504 = recode(qn504, '1' = -2, '2' = -1 ,'5' = 0, '3' = 1, '4' = 2, .default = -8),
#                qn505 = recode(qn505, '1' = -2, '2' = -1 ,'5' = 0, '3' = 1, '4' = 2, .default = -8),
#                qn506 = recode(qn506, '1' = -2, '2' = -1 ,'5' = 0, '3' = 1, '4' = 2, .default = -8),
#                qn507 = recode(qn507, '1' = -2, '2' = -1 ,'5' = 0, '3' = 1, '4' = 2, .default = -8))

# combine all the related data
all <- base::cbind(persinfo, SES, MH, migr, fair, sup_kin, sup_nonkin)
# write the table
write.csv(all, file = "sesMH.csv", row.names = FALSE)
