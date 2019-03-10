################## Preprocessing CFPS 2014 child preproc ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    19-03-09         The first version
# 
# 
#
###### input######
# 2014child.csv
# 2014adult.csv
# 2014family.csv
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
dfc4 <- read.csv("data/2014child.csv", header=T)
dfa4 <- read.csv("data/2014adult.csv", header=T)
dff4 <- read.csv("data/2014family.csv", header=T)
child_per <- read.csv("child_per.csv", header=T)

#select children from 2010 children
datac2014_c <- subset(dfc4, pid %in% child_per$pid)

#child related variables
##still child in 2014
datac2014_c <- datac2014_c %>%
  dplyr::select(wq601, wq602, wq603, wq604, wq605, wq606, 
                kz207_b_2, kz201_b_2, #depression1-6, intelligence, cognitive
                #wm101m, wm102m, wm103m, wm104m, wm105m, wm106m, wm107m, wm108m, wm109m, wm110m, wm111m, wm111m, wm112m, wm113m, wm114m, #parent-child relationship (too many NAs)
                cfps2014_age, fid14, wa4) #personal information
summary(datac2014_c)
datac2014_c[datac2014_c < 0] <- NA
names(datac2014_c)[1:11] <- c("deprc4_1","deprc4_2", "deprc4_3", "deprc4_4", "deprc4_5","deprc4_6", 
                              "intellc4", "cogc4",  "agec4", "fid","hukouc4")

##become adult in 2014
datac2014_a <- subset(dfa4, pid %in% child_per$pid)
datac2014_a <- datac2014_a %>%
  dplyr::select(qq601, qq602, qq603, qq604, qq605, qq606, #depression,
                qz207, qz201, #intelligence, cognitive
                cfps2014_age, fid14, qa301) #personal information
datac2014_c[datac2014_c < 0] <- NA
summary(datac2014_c)
names(datac2014_a)[1:11] <- c("deprc4_1","deprc4_2", "deprc4_3", "deprc4_4", "deprc4_5","deprc4_6", 
                              "intellc4", "cogc4",  "agec4", "fid","hukouc4")
datac2014 <- rbind(datac2014_a, datac2014_c)

#adult related variables
##father
dataf2014 <- subset(dfa4, pid %in% child_per$pid_f)
dataf2014 <- dataf2014 %>%
  dplyr::select(cfps2014edu, p_income, qn401_s_1, #education 1-8
                qq601, qq602, qq603, qq604, qq605, qq606, #depression
                qn12012, qn12014, qm2012, #satisfaction, confidence, happiness
                wordtest14, mathtest14, #word, math test
                cfps2014_age, fid14, qa301)%>% #personal information
  dplyr::mutate(qn401_s_1 = recode(qn401_s_1, "1" = 1, .default = 0))
dataf2014[dataf2014 < 0] <- NA
summary(dataf2014)
names(dataf2014)[1:17] <- c("eduf4", "incomef4", "fparty4",
                            "deprf4_1","deprf4_2", "deprf4_3", "deprf4_4", "deprf4_5","deprf4_6", 
                            "satisf4", "confif4", "happif4", 
                            "wordf4", "mathf4",
                            "agef4",  "fid", "hukouf4")
##mother
datam2014 <- subset(dfa4, pid %in% child_per$pid_m)
datam2014 <- datam2014 %>%
  dplyr::select(cfps2014edu, p_income, qn401_s_1, #education 1-8
                qq601, qq602, qq603, qq604, qq605, qq606, #depression
                qn12012, qn12014, qm2012, #satisfaction, confidence, happiness
                wordtest14, mathtest14, #word, math test
                cfps2014_age, fid14, qa301)%>% #personal information
  dplyr::mutate(qn401_s_1 = recode(qn401_s_1, "1" = 1, .default = 0))
datam2014[datam2014 < 0] <- NA
summary(datam2014)
names(datam2014)[1:17] <- c("edum4", "incomem4", "mparty4",
                            "deprm4_1","deprm4_2", "deprm4_3", "deprm4_4", "deprm4_5","deprm4_6", 
                            "satism4", "confim4", "happim4", 
                            "wordm4", "mathm4",
                            "agem4",  "fid", "hukoum4")

#family related data ?MANY NA
#datafam2014 <- subset(dff4, fid14 %in% child_per$fid)
#datafam2014 <- datafam2014 %>%
#  dplyr::select(fincome2, ft1, familysize, #family income, family saving, family size #many NA
#                fid14)
#datafam2014[datafam2014 < 0] <- NA
#summary(datafam2014)
#names(datafam2014)[1:4] <- c("incomefam4", "savingfam4", "sizefam4", 
#                             "fid")

#merge all relevant variable together
datap2014 <- merge(dataf2014, datam2014, by = "fid")
#datap_fam2014 <- left_join(datap2014, datafam2014, by="fid")
data2014 <- left_join(datac2014, datap2014, by="fid")
summary(data2014)
# write table
write.csv(data2014, file = "data2014.csv", row.names = FALSE)
