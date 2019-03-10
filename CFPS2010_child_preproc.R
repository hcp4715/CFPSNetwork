################## Preprocessing CFPS 2010 child data ##############################
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

#select children from 2010 children
datac2010 <- subset(dfc0, pid %in% child_per$pid)

#child related variables
datac2010 <- datac2010 %>%
  dplyr::select(wn401, wn402, wn403, wn404, wn405, wn406, wz207, wz201, #depression1-6, intelligence, cognitive
                #wm201, wm202, wm203, wm204, wm205, wm206, wm207, wm208, wm209, wm210, wm211, wm211, wm212, wm213, wm214, #parent-child relationship (too many NAs)
                gender, wa1age, fid, pid, wa4, #personal information
                fparty, mparty) %>%
  dplyr::mutate(mparty = recode(mparty, "1" = 1, .default = 0),
                fparty = recode(fparty, "1" = 1, .default = 0)) 
summary(datac2010)
datac2010[datac2010 < 0] <- NA
names(datac2010)[1:15] <- c("deprc0_1","deprc0_2", "deprc0_3", "deprc0_4", "deprc0_5","deprc0_6", "intellc0", "cogc0", "genderc", "agec0", "fid", "pid","hukouc0", "fparty0", "mparty0")

#adult related variables
##father
dataf2010 <- subset(dfa0, pid %in% child_per$pid_f)
dataf2010 <- dataf2010 %>%
  dplyr::select(edu2010, income, #education 1-8
                qq601, qq602, qq603, qq604, qq605, qq606, #depression
                qm403, qm404, qk802, #satisfaction, confidence, happiness
                wordtest, mathtest, #cognitive
                qa1age, qa2, fid, pid) #personal information
dataf2010[dataf2010 < 0] <- NA
summary(dataf2010)
names(dataf2010)[1:17] <- c("eduf0", "incomef0",
                            "deprf0_1","deprf0_2", "deprf0_3", "deprf0_4", "deprf0_5","deprf0_6", 
                            "satisf0", "confif0", "happif0", 
                            "wordf0", "mathf0",
                            "agef0", "hukouf0", "fid", "pid_f")
##mother
datam2010 <- subset(dfa0, pid %in% child_per$pid_m)
datam2010 <- datam2010 %>%
  dplyr::select(edu2010, income, #education 1-8
                qq601, qq602, qq603, qq604, qq605, qq606, #depression
                qm403, qm404, qk802, #satisfaction, confidence, happiness
                wordtest, mathtest, #cognitive
                qa1age, qa2, fid, pid) #personal information
datam2010[datam2010 < 0] <- NA
summary(datam2010)
names(datam2010)[1:17] <- c("edum0", "incomem0",
                            "deprm0_1","deprm0_2", "deprm0_3", "deprm0_4", "deprm0_5","deprm0_6", 
                            "satism0", "confim0", "happim0", 
                            "wordm0", "mathm0",
                            "agem0", "hukoum0", "fid", "pid_m")

#family related data
datafam2010 <- subset(dff0, fid %in% child_per$fid)
datafam2010 <- datafam2010 %>%
  dplyr::select(fincome, savings, familysize, #family income, family saving, family size
                fid)
datafam2010[datafam2010 < 0] <- NA
summary(datafam2010)
names(datafam2010)[1:4] <- c("incomefam0", "savingfam0", "sizefam0", 
                            "fid")

#merge all relevant variable together
datap2010 <- merge(dataf2010, datam2010, by = "fid")
datap_fam2010 <- merge(datap2010, datafam2010, by="fid")
data2010 <- merge(datap_fam2010, datac2010, by="fid")
summary(data2010)
# write table
write.csv(data2010, file = "data2010.csv", row.names = FALSE)
