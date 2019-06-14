################## Preprocessing CFPS 2010 child preproc ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    18-10-24         The first version
# Cai, Y-Q    19-03-09         New version
# 
#
###### input######
# 2010child.csv
# 2010adult.csv
# 2010family.csv
#
###### output #####
# data2010.csv
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
dfc0 <- read.csv("data/2010child.csv", header=T)
dfa0 <- read.csv("data/2010adult.csv", header=T)
dff0 <- read.csv("data/2010family.csv", header=T)
child_per <- read.csv("child_per.csv", header=T)
####################children
#select children from 2010 children
c0 <- dfc0 %>%
  dplyr::filter(wa1age >= 4) %>% #select children older than 4 (with both answer themselves and parents answer for them)
  dplyr::filter(pid_f != -8, pid_m != -8) #select children whose father and mother have questionnaire
c0 <- c0 %>%
  dplyr::select(gender, wa1age, fid, pid, pid_f, pid_m) 
names(c0)[1:6] <- c("gender0", "age0", "fid0", "pid", "pid_f0", "pid_m0")
#select adults from 2010 (age 16-20)
a0 <- dfa0 %>%
  dplyr::filter(qa1age <= 22) %>%
  dplyr::filter(pid_f != -8, pid_m != -8)  %>%
  dplyr::group_by(fid) %>% #select one children in each family
  dplyr::filter(row_number() == 1) %>%
  dplyr::ungroup()
a0 <- a0 %>%
  dplyr::select(gender,qa1age,  fid, pid, pid_f, pid_m)
names(a0)[1:6] <- c("gender0", "age0", "fid0", "pid", "pid_f0", "pid_m0")
# age 4-22 as child participants
par0 <-rbind(c0, a0)
par0 <- par0 %>%
  dplyr::group_by(fid0) %>% #select one children in each family
  dplyr::filter(row_number() == 1) %>%
  dplyr::ungroup()

#child related variables
child0_c <- subset(dfc0, pid %in% par0$pid)
child0_c <- child0_c %>%
  dplyr::select(wn401, wn402, wn403, wn404, wn405, wn406, wz207, wz201, #depression1-6, intelligence, cognitive
                #wm201, wm202, wm203, wm204, wm205, wm206, wm207, wm208, wm209, wm210, wm211, wm211, wm212, wm213, wm214, #parent-child relationship (too many NAs)
                pid, wa4, fid, #personal information
                fparty, mparty, pid_f, pid_m) %>%
  dplyr::mutate(mparty = recode(mparty, "1" = 1, .default = 0),
                fparty = recode(fparty, "1" = 1, .default = 0))
child0_c[child0_c < 0] <- NA
child0_c <- child0_c %>%
 dplyr::mutate(depr_sum = wn401+ wn402+ wn403+ wn404+ wn405+ wn406)
names(child0_c)[1:16] <- c("deprc0_1","deprc0_2", "deprc0_3", "deprc0_4", "deprc0_5","deprc0_6", "intellc0", "cogc0", "pid","hukouc0","fid", "fparty0", "mparty0", "pid_f0", "pid_m0", "depr_sumc0")

#children in adult questionnaires
adult0 <- subset(dfa0, pid %in% par0$pid)
child0_a <- adult0 %>%
  dplyr::select(qq601, qq602, qq603, qq604, qq605, qq606, qz207, qz208,  #depression
                pid, qa2, fid,
                fparty, mparty, pid_f, pid_m) %>%
  dplyr::mutate(mparty = recode(mparty, "1" = 1, .default = 0),
                fparty = recode(fparty, "1" = 1, .default = 0))
child0_a[child0_a<0]<-NA
child0_a <- child0_a %>%
  dplyr::mutate(depr_sum = qq601+ qq602+ qq603+qq604+ qq605+ qq606)
names(child0_a)[1:16] <- c("deprc0_1","deprc0_2", "deprc0_3", "deprc0_4", "deprc0_5","deprc0_6", "intellc0", "cogc0", "pid","hukouc0","fid", "fparty0", "mparty0", "pid_f0", "pid_m0","deprc0_sum")
child0 <- rbind(child0_c, child0_a)
summary(child0)



#########################parents
#adult related variables
##father
dataf2010 <- subset(dfa0, pid %in% par0$pid_f0)
dataf2010 <- dataf2010 %>%
  dplyr::select(edu2010, income, qm401, qm402, #education 1-8, income, subjective SES
                qq601, qq602, qq603, qq604, qq605, qq606, #depression
                qm403, qm404, qk802, #satisfaction, confidence, happiness
                wordtest, mathtest, #cognitive
                qa1age, qa2, fid, pid) #personal information
dataf2010[dataf2010 < 0] <- NA
dataf2010 <- dataf2010 %>%
  dplyr::mutate(depr_sum = qq601+ qq602+ qq603+qq604+ qq605+ qq606,
                posi_sum = qm403+ qm404+ qk802)
summary(dataf2010)
names(dataf2010)[1:21] <- c("eduf0", "incomef0", "sSESf0_1", "sSESf0_2",
                            "deprf0_1","deprf0_2", "deprf0_3", "deprf0_4", "deprf0_5","deprf0_6", 
                            "satisf0", "confif0", "happif0", 
                            "wordf0", "mathf0",
                            "agef0", "hukouf0", "fid", "pid_f0", "depr_sumf0", "posi_sumf0")

##mother
datam2010 <- subset(dfa0, pid %in% par0$pid_m0)
datam2010 <- datam2010 %>%
  dplyr::select(edu2010, income,qm401, qm402, #education 1-8
                qq601, qq602, qq603, qq604, qq605, qq606, #depression
                qm403, qm404, qk802, #satisfaction, confidence, happiness
                wordtest, mathtest, #cognitive
                qa1age, qa2, fid, pid) #personal information
datam2010[datam2010 < 0] <- NA
datam2010 <- datam2010 %>%
  dplyr::mutate(depr_sum = qq601+ qq602+ qq603+qq604+ qq605+ qq606,
                posi_sum = qm403+ qm404+ qk802)
summary(datam2010)
names(datam2010)[1:21] <- c("edum0", "incomem0", "sSESm0_1", "sSESm0_2",
                            "deprm0_1","deprm0_2", "deprm0_3", "deprm0_4", "deprm0_5","deprm0_6", 
                            "satism0", "confim0", "happim0", 
                            "wordm0", "mathm0",
                            "agem0", "hukoum0", "fid", "pid_m0", "depr_summ0", "posi_summ0")

#family related data
datafam2010 <- subset(dff0, fid %in% par0$fid0)
datafam2010 <- datafam2010 %>%
  dplyr::select(fincome, savings, familysize, #family income, family saving, family size
                fid)
datafam2010[datafam2010 < 0] <- NA
summary(datafam2010)
names(datafam2010)[1:4] <- c("incomefam0", "savingfam0", "sizefam0", 
                            "fid")
summary(datafam2010$fincome)
summary(datafam2010$savings)
#merge all relevant variable together

datap2010 <- merge(dataf2010, datam2010, by = "fid")
datap_fam2010 <- merge(datap2010, datafam2010, by="fid")
data2010 <- merge(datap_fam2010, child0, by=c("fid", "pid_f0", "pid_m0"))
summary(data2010)
labels(data2010)

# write table
write.csv(data2010, file = "data2010.csv", row.names = FALSE)


