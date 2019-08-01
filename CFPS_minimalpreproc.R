################## Preprocessing CFPS 2012 child preproc ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    19-03-09         The first version
# 
# 
#
###### input######
# 2012child.csv
# 2012adult.csv
# 2012family.csv
#
###### output #####
# data2012.csv
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
dfc0 <- read.csv("data/csv/2010child.csv", header=T)
dfa0 <- read.csv("data/csv/2010adult.csv", header=T)
dff0 <- read.csv("data/csv/2010family.csv", header=T)
dffr0 <- read.csv("data/csv/2010familyroster.csv", header = T)
dfc2 <- read.csv("data/csv/2012child.csv", header=T)
dfa2 <- read.csv("data/csv/2012adult.csv", header=T)
dff2 <- read.csv("data/csv/2012family.csv", header=T)
dffr2 <- read.csv("data/csv/2012familyroster.csv", header = T)
dfc4 <- read.csv("data/csv/2014child.csv", header=T)
dfa4 <- read.csv("data/csv/2014adult.csv", header=T)
dff4 <- read.csv("data/csv/2014family.csv", header=T)
dffr4 <- read.csv("data/csv/2014familyroster.csv", header = T)
dfc6 <- read.csv("data/csv/2016child.csv", header=T)
dfa6 <- read.csv("data/csv/2016adult.csv", header=T)
dff6 <- read.csv("data/csv/2016family.csv", header=T)
dffr6 <- read.csv("data/csv/2016familyroster.csv", header = T)

###############################################################
#preprocess of 2010
#extract children data
child0_c <- dfc0 %>%
  dplyr::filter(wa1age >= 4) %>% #select children older than 4 (with both answer themselves and parents answer for them)
  dplyr::filter(pid_f != -8, pid_m != -8) %>% #select children whose father and mother have questionnaire
  dplyr::select(gender, wa1age, fid, pid, pid_f, pid_m, 
                wn401, wn402, wn403, wn404, wn405, wn406, wz207, wz201, #depression1-6, intelligence, cognitive
                #wm201, wm202, wm203, wm204, wm205, wm206, wm207, wm208, wm209, wm210, wm211, wm211, wm212, wm213, wm214, #parent-child relationship (too many NAs)
                wa4, #personal information
                fparty, mparty, pid_f, pid_m) %>%
  dplyr::mutate(mparty = recode(mparty, "1" = 1, .default = 0),
                fparty = recode(fparty, "1" = 1, .default = 0))
names(child0_c) <- c("gender0", "age0", "fid", "pid", "pid_f", "pid_m",
               "deprc0_1","deprc0_2", "deprc0_3", "deprc0_4", "deprc0_5","deprc0_6", 
               "intellc0", "cogc0", 
               "hukouc0","fparty0", "mparty0")
#select adults from 2010 (age 16-22)
child0_a <- dfa0 %>%
  dplyr::filter(qa1age <= 22) %>%
  dplyr::filter(pid_f != -8, pid_m != -8) %>%
  dplyr::select(gender, qa1age,  fid, pid, pid_f, pid_m,
                qq601, qq602, qq603, qq604, qq605, qq606, qz207, qz208,  #depression
                 qa2, 
                fparty, mparty) %>%
  dplyr::mutate(mparty = recode(mparty, "1" = 1, .default = 0),
                fparty = recode(fparty, "1" = 1, .default = 0))
names(child0_a) <- c("gender0", "age0", "fid", "pid", "pid_f", "pid_m",
                    "deprc0_1","deprc0_2", "deprc0_3", "deprc0_4", "deprc0_5","deprc0_6", 
                    "intellc0", "cogc0", 
                    "hukouc0", "fparty0", "mparty0")

#combine children questionaire
child0 <- rbind(child0_a, child0_c)

#extract children's parents from adults questionnaire
##father
dataf2010 <- subset(dfa0, pid %in% child0$pid_f)
dataf2010 <- dataf2010 %>%
  dplyr::select(edu2010, income, qm401, qm402, #education 1-8, income, subjective SES
                qq601, qq602, qq603, qq604, qq605, qq606, #depression
                qm403, qm404, qk802, #satisfaction, confidence, happiness
                wordtest, mathtest, #cognitive
                qa1age, qa2, fid, pid) #personal information
dataf2010[dataf2010 < 0] <- NA
summary(dataf2010)
names(dataf2010) <- c("eduf0", "incomef0", "sSESf0_1", "sSESf0_2",
                      "deprf0_1","deprf0_2", "deprf0_3", "deprf0_4", "deprf0_5","deprf0_6", 
                      "satisf0", "confif0", "happif0", 
                      "wordf0", "mathf0",
                      "agef0", "hukouf0", "fid", "pid_f")

##mother
datam2010 <- subset(dfa0, pid %in% child0$pid_m)
datam2010 <- datam2010 %>%
  dplyr::select(edu2010, income,qm401, qm402, #education 1-8
                qq601, qq602, qq603, qq604, qq605, qq606, #depression
                qm403, qm404, qk802, #satisfaction, confidence, happiness
                wordtest, mathtest, #cognitive
                qa1age, qa2, fid, pid) #personal information
datam2010[datam2010 < 0] <- NA
summary(datam2010)
names(datam2010) <- c("edum0", "incomem0", "sSESm0_1", "sSESm0_2",
                            "deprm0_1","deprm0_2", "deprm0_3", "deprm0_4", "deprm0_5","deprm0_6", 
                            "satism0", "confim0", "happim0", 
                            "wordm0", "mathm0",
                            "agem0", "hukoum0", "fid", "pid_m")
#family related data
datafam2010 <- subset(dff0, fid %in% child0$fid)
datafam2010 <- datafam2010 %>%
  dplyr::select(fincome, savings, familysize, #family income, family saving, family size
                fid)
datafam2010[datafam2010 < 0] <- NA
summary(datafam2010)
names(datafam2010)[1:4] <- c("incomefam0", "savingfam0", "sizefam0", 
                             "fid")
#familyroster
datafamR2010 <- subset(dffr0, fid %in% child0$fid)
summary(datafamR2010)
datafamR2010 <- datafamR2010 %>%
  dplyr::select(tb6_a_p, pid) #still live in this family
names(datafamR2010)[1:2] <- c("left2", "pid")

#merge together
datap2010 <- merge(dataf2010, datam2010, by = "fid")
datap_fam2010 <- merge(datap2010, datafam2010, by= "fid")
datap_fam_c2010 <- merge(datap_fam2010, child0, by=c("pid_f", "pid_m", "fid"))
data2010 <- merge(datap_fam_c2010, datafamR2010, by= "pid")
str(data2010)

# write table
write.csv(data2010, file = "data2010.csv", row.names = FALSE)

###############################################################
#preprocess of 2012
#child related variables
child2_c <- dfc2 %>%
  dplyr::filter(cfps2012_age >= 4) %>% #select children older than 6 (with both answer themselves and parents answer for them)
  dplyr::filter(pid_f != -8, pid_m != -8) %>%
  dplyr::select(wn401, wn402, wn403, wn404, wn405, wn406, wn407, wn408, wn409, wn410, 
                wn411, wn412, wn413, wn414, wn415, wn416, wn417, wn418, wn419, wn420, #CES
                #wm101, wm102, wm103, wm104, wm105, wm106, wm107, wm108, wm110, wm111, wm112,wm113, wm114, #esteem (350NA and NA in adult)
                kz207_b_2, kz201_b_2, # intelligence, cognitive
                #wm201, wm202, wm203, wm204, wm205, wm206, wm207, wm208, wm209, wm210, wm211, wm211, wm212, wm213, wm214, #parent-child relationship (too many NAs)
                cfps2012_age, fid12, wa4, pid, gender2, pid_f, pid_m) #personal informatio
child2_c[child2_c< 0] <- NA
summary(child2_c)
names(child2_c)[1:29] <- c("deprc2_1","deprc2_2","deprc2_3","deprc2_4","deprc2_5","deprc2_6","deprc2_7","deprc2_8","deprc2_9","deprc2_10",
                           "deprc2_11", "deprc2_12","deprc2_13","deprc2_14","deprc2_15","deprc2_16","deprc2_17","deprc2_18","deprc2_19","deprc2_20",
                           "intellc2", "cogc2",
                           "agec2", "fid", "hukouc2", "pid", "gender", "pid_f", "pid_m")

#chose children in adult questionnaire as 2010
child2_a <- dfa2 %>%
  dplyr::filter(cfps2012_age <= 24) %>%
  dplyr::filter(pid_f != -8, pid_m != -8) %>%
  dplyr::select(qq6011, qq6012, qq6013,qq6014,qq6015,qq6016,qq6017,qq6018,qq6019,
                qq60110,qq60111,qq60112, qq60113,qq60114,qq60115,qq60116,qq60117,qq60118,qq60119,qq60120, #CES
                qz207, qz201, #intelligence, cognitve
                cfps2012_age, fid12, qa301, pid, cfps2012_gender, pid_f, pid_m)
child2_a[child2_a < 0] <- NA
summary(child2_a)
names(child2_a)[1:29] <- c("deprc2_1","deprc2_2","deprc2_3","deprc2_4","deprc2_5","deprc2_6","deprc2_7","deprc2_8","deprc2_9","deprc2_10",
                           "deprc2_11", "deprc2_12","deprc2_13","deprc2_14","deprc2_15","deprc2_16","deprc2_17","deprc2_18","deprc2_19","deprc2_20",
                           "intellc2", "cogc2",
                           "agec2", "fid", "hukouc2", "pid", "gender", "pid_f", "pid_m2")
child2 <- rbind(child2_c, child2_a)
str(child2)

#adult related variables
##father
dataf2012 <- subset(dfa2, pid %in% child2$pid_f)
dataf2012 <- dataf2012 %>%
  dplyr::select(edu2012, income_adj, qn401_s_1, qn8011, qn8012, #education 1-8, income, politics, subjective ses
                qq6011, qq6012, qq6013,qq6014,qq6015,qq6016,qq6017,qq6018,qq6019,
                qq60110,qq60111,qq60112, qq60113,qq60114,qq60115,qq60116,qq60117,qq60118,qq60119,qq60120, #depression
                qn12012, qn12014, #satisfaction, confidence
                iwr, dwr, #ns_w, #cognitive: memory1, memory2, number series(many NA)
                cfps2012_age, fid12, qa301, pid)%>% #personal information
  dplyr::mutate(qn401_s_1 = recode(qn401_s_1, "1" = 1, .default = 0))
dataf2012[dataf2012 < 0] <- NA
summary(dataf2012)
names(dataf2012) <- c("eduf2", "incomef2", "fparty2", "sSESf2_1", "sSESf2_2",
                            "deprf2_1","deprf2_2","deprf2_3","deprf2_4","deprf2_5","deprf2_6","deprf2_7","deprf2_8","deprf2_9","deprf2_10",
                            "deprf2_11", "deprf2_12","deprf2_13","deprf2_14","deprf2_15","deprf2_16","deprf2_17","deprf2_18","deprf2_19","deprf2_20",
                            "satisf2", "confif2", 
                            "memoryf2_1", "memoryf2_2",
                            "agef2","fid", "hukouf2", "pid_f")
dataf2012$sSESf2_1[dataf2012$sSESf2_1 == 79] <-NA

##mother
datam2012 <- subset(dfa2, pid %in% child2$pid_m)
datam2012 <- datam2012 %>%
  dplyr::select(edu2012, income_adj, qn401_s_1, qn8011, qn8012, #education 1-8, income, politics, subjective ses
                qq6011, qq6012, qq6013,qq6014,qq6015,qq6016,qq6017,qq6018,qq6019,
                qq60110,qq60111,qq60112, qq60113,qq60114,qq60115,qq60116,qq60117,qq60118,qq60119,qq60120, #depression
                qn12012, qn12014, #satisfaction, confidence
                iwr, dwr, #ns_w, #cognitive: memory1, memory2, number series(many NA)
                cfps2012_age, fid12, qa301,pid) %>%  #personal information
  dplyr::mutate(qn401_s_1 = recode(qn401_s_1, "1" = 1, .default = 0))
datam2012[datam2012 < 0] <- NA
summary(datam2012)
names(datam2012)<- c("edum2", "incomem2", "mparty2", "sSESm2_1", "sSESm2_2",
                            "deprm2_1","deprm2_2","deprm2_3","deprm2_4","deprm2_5","deprm2_6","deprm2_7","deprm2_8","deprm2_9","deprm2_10",
                            "deprm2_11", "deprm2_12","deprm2_13","deprm2_14","deprm2_15","deprm2_16","deprm2_17","deprm2_18","deprm2_19","deprm2_20",
                            "satism2", "confim2", 
                            "memorym2_1", "memorym2_2",
                            "agem2", "fid", "hukoum2", "pid_m" )
datam2012$sSESm2_1[datam2012$sSESm2_1 == 79] <-NA
datam2012$sSESm2_2[datam2012$sSESm2_2 == 79] <-NA

#family related data ?many NA
datafam2012 <- subset(dff2, fid12 %in% data2010$fid)
datafam2012 <- datafam2012 %>%
  dplyr::select(fincome2, ft1, familysize, #family income(net income can be compared with2010), family saving, family size
                fid12)
datafam2012[datafam2012 < 0] <- NA
summary(datafam2012)
names(datafam2012)[1:4] <- c("incomefam2","savingfam2", "sizefam2", 
                             "fid")

#familyroster: extract data "still live in the household"
summary(dffr2)
datafamR2012 <- subset(dffr2, fid12 %in% child2$fid)
datafamR2012 <- datafamR2012 %>%
  dplyr::select(TB6_A12_p, pid)
names(datafamR2012)[1:2] <- c("left2", "pid")

#merge all relevant variable together
datap2012 <- merge(dataf2012, datam2012, by = "fid")
datap_fam2012 <- merge(datap2012, datafam2012, by= "fid")
datap_fam_c2012 <- merge(datap_fam2012, child2, by="fid")
data2012 <- merge(datap_fam_c2012, datafamR2012, by= "pid")
str(data2012)

# write table
write.csv(data2012, file = "data2012.csv", row.names = FALSE)

###############################################################
#preprocess of 2014
#child related variables
##child in child questionnaire in 2014
child4_c <- dfc4 %>%
  dplyr::filter(cfps2014_age >= 4) %>% #select children older than 8 (with both answer themselves and parents answer for them)
  dplyr::filter(pid_f != -8, pid_m != -8) %>%
  dplyr::select(wq601, wq602, wq603, wq604, wq605, wq606, 
                kz207_b_2, kz201_b_2, #depression1-6, intelligence, cognitive
                #wm101m, wm102m, wm103m, wm104m, wm105m, wm106m, wm107m, wm108m, wm109m, wm110m, wm111m, wm111m, wm112m, wm113m, wm114m, #parent-child relationship (too many NAs)
                cfps2014_age, fid14, wa4, pid, pid_f, pid_m) #personal information
summary(child4_c)
child4_c[child4_c < 0] <- NA
names(child4_c) <- c("deprc4_1","deprc4_2", "deprc4_3", "deprc4_4", "deprc4_5","deprc4_6", 
                              "intellc4", "cogc4",  "agec4", "fid","hukouc4", "pid","pid_f", "pid_m")

##child in adult questionnaire in 2014
child4_a <- dfa4 %>%
  dplyr::filter(cfps2014_age <= 26) %>% #select children older than 8 (with both answer themselves and parents answer for them)
  dplyr::filter(pid_f != -8, pid_m != -8) %>%
  dplyr::select(qq601, qq602, qq603, qq604, qq605, qq606, #depression,
                qz207, qz201, #intelligence, cognitive
                cfps2014_age, fid14, qa301, pid, pid_f, pid_m) #personal information
child4_a[child4_a < 0] <- NA
summary(child4_a)
names(child4_a) <- c("deprc4_1","deprc4_2", "deprc4_3", "deprc4_4", "deprc4_5","deprc4_6", 
                              "intellc4", "cogc4",  "agec4", "fid","hukouc4","pid", "pid_f", "pid_m")
child4 <- rbind(child4_a, child4_c)
#adult related variables
##father
dataf2014 <- subset(dfa4, pid %in% child4$pid_f)
dataf2014 <- dataf2014 %>%
  dplyr::select(cfps2014edu, p_income, qn401_s_1, qn8011, qn8012, #education 1-8, income, politics, subjective SES
                qq601, qq602, qq603, qq604, qq605, qq606, #depression
                qn12012, qn12014, qm2012, #satisfaction, confidence, happiness
                wordtest14, mathtest14, #word, math test
                cfps2014_age, fid14, qa301, pid)%>% #personal information
  dplyr::mutate(qn401_s_1 = recode(qn401_s_1, "1" = 1, .default = 0))
dataf2014[dataf2014 < 0] <- NA
summary(dataf2014)
names(dataf2014)[1:20] <- c("eduf4", "incomef4", "fparty4","sSESf4_1", "sSESf4_2",
                            "deprf4_1","deprf4_2", "deprf4_3", "deprf4_4", "deprf4_5","deprf4_6", 
                            "satisf4", "confif4", "happif4", 
                            "wordf4", "mathf4",
                            "agef4",  "fid", "hukouf4","pid_f")
dataf2014$sSESf4_1[dataf2014$sSESf4_1 ==79]<-NA
dataf2014 <- dataf2014%>%
  dplyr::mutate(depr_sumf4 = deprf4_1 +deprf4_2 +deprf4_3 +deprf4_4 +deprf4_5 +deprf4_6,
                posi_sumf4 = satisf4+ confif4 + happif4)

##mother
datam2014 <- subset(dfa4, pid %in% data2012$pid_m)
datam2014 <- datam2014 %>%
  dplyr::select(cfps2014edu, p_income, qn401_s_1, qn8011, qn8012, #education 1-8,income, politics, subjective SES
                qq601, qq602, qq603, qq604, qq605, qq606, #depression
                qn12012, qn12014, qm2012, #satisfaction, confidence, happiness
                wordtest14, mathtest14, #word, math test
                cfps2014_age, fid14, qa301,pid)%>% #personal information
  dplyr::mutate(qn401_s_1 = recode(qn401_s_1, "1" = 1, .default = 0))
datam2014[datam2014 < 0] <- NA
names(datam2014)[1:20] <- c("edum4", "incomem4", "mparty4", "sSESm4_1", "sSESm4_2",
                            "deprm4_1","deprm4_2", "deprm4_3", "deprm4_4", "deprm4_5","deprm4_6", 
                            "satism4", "confim4", "happim4", 
                            "wordm4", "mathm4",
                            "agem4",  "fid", "hukoum4", "pid_m")
summary(datam2014)
datam2014$sSESm4_1[datam2014$sSESm4_1 == 79] <- NA
#family related data 
datafam2014 <- subset(dff4, fid14 %in% child4$fid)
datafam2014 <- datafam2014 %>%
  dplyr::select(fincome2, ft1, familysize, #family income, family saving, family size #many NA
                fid14)
datafam2014[datafam2014 < 0] <- NA
summary(datafam2014)
names(datafam2014)[1:4] <- c("incomefam4", "savingfam4", "sizefam4", 
                             "fid")
#familyroster: extract data "still live in the household"
summary(dffr4)
datafamR2014 <- subset(dffr4, fid14 %in%  child4$fid)
datafamR2014 <- datafamR2014 %>%
  dplyr::select(tb6_a14_p, pid)
names(datafamR2014) <- c("left4", "pid")

#merge all relevant variable together
datap2014 <- merge(dataf2014, datam2014, by = "fid")
datap_fam2014 <- merge(datap2014, datafam2014, by= "fid")
datap_fam_c2014 <- merge(datap_fam2014, child4, by= c("fid","pid_f","pid_m"))
data2014 <- merge(datap_fam_c2014, datafamR2014, by= "pid")
str(data2014)

# write table
write.csv(data2014, file = "data2014.csv", row.names = FALSE)

###############################################################
#preprocess of 2016
#family roster
#child related variables
# children in children questionniare in 2016
child6_c <- dfc6 %>%
  dplyr::filter(cfps_age >= 4) %>% #select children older than 8 (with both answer themselves and parents answer for them)
  dplyr::filter(pid_f != -8, pid_m != -8) %>%
  dplyr::select(pn406, pn407, pn411, pn412, pn414, pn416, pn418, pn420,
                #pn401, pn402, pn403, pn404, pn405, pn406, pn407, pn408, pn409, pn410, 
                #pn411, pn412, pn413, pn414, pn415, pn416, pn417, pn418, pn419, pn420, #CES
                #pm101m, pm102m, pm103m, pm104m, pm105m, pm106m, pm107m, pm108m, pm110m, #esteem 
                kz207_b_2, kz201_b_2, # intelligence, cognitive
                cfps_age, fid16, pa301, pid) #personal information
child6_c[child6_c < 0] <- NA
summary(child6_c)
names(child6_c) <- c("deprc6_6","deprc6_7","deprc6_11","deprc6_12","deprc6_14","deprc6_16","deprc6_18","deprc6_20",
                              "intellc6", "cogc6",
                              "agec6", "fid", "hukouc6","pid")

##became adult in 2016
child6_a <- dfa6 %>%
  dplyr::filter(cfps_age <= 28) %>% #select children older than 8 (with both answer themselves and parents answer for them)
  dplyr::select(pn406, pn407, pn411, pn412, pn414, pn416, pn418, pn420,
                #pn401, pn402, pn403, pn404, pn405,pn408, pn409, pn410,pn413,pn415, pn417, pn419, #CES  ###Many participants uses the simplified version(6, 7, 11, 12, 14, 16,18, 20) #many NAs  
                #pm101m, pm102m, pm103m, pm104m, pm105m, pm106m, pm107m, pm108m, pm110m, #many NA
                qz207, qz201, #intelligence, cognitve
                cfps_age, fid16, pa301, pid)
child6_a[child6_a < 0] <- NA
summary(child6_a)
names(child6_a) <- c("deprc6_6","deprc6_7","deprc6_11","deprc6_12","deprc6_14","deprc6_16","deprc6_18","deprc6_20",
                              "intellc6", "cogc6",
                              "agec6", "fid", "hukouc6","pid")
child6 <- rbind(child6_a, child6_c)


#family roster: extract variable "pid still live in the household"; pid_f, pid_m for children in adult questionnaire
summary(dffr6)
datafamR2016 <- subset(dffr6, pid %in% child6$pid)
datafamR2016 <- datafamR2016 %>%
  dplyr::select(tb6_a16_p, pid, pid_f, pid_m)
names(datafamR2016) <- c("left6","pid","pid_f", "pid_m")
datafamR2016[datafamR2016 < 0] <-NA
#merge child6 with datafamR2016
datac_famR6 <- merge(child6, datafamR2016, by = "pid")
#adult related variables
##father
dataf2016 <- subset(dfa6, pid %in% datac_famR6$pid_f)
dataf2016 <- dataf2016 %>%
  dplyr::select(cfps2016edu, incomeb_imp, incomea, incomeb, qn4001,qn8011, qn8012, #education 1-8, income, politics
                pn406, pn407, pn411, pn412, pn414, pn416, pn418, pn420,
                #pn401, pn402, pn403, pn404, pn405,pn408, pn409, pn410,pn413,pn415, pn417, pn419, #CES  ###Many participants uses the simplified version(6, 7, 11, 12, 14, 16,18, 20) #many NAs  
                qn12012, qn12014, #qm2014, #satisfaction, confidence, happiness(many NA)
                iwr, dwr, ns_w, #cognitive: memory1, memory2, number series(many NA)
                cfps_age, fid16, pa301) #personal information
dataf2016[dataf2016 < 0] <- NA
str(dataf2016)
names(dataf2016) <- c("eduf6", "incomef6_b_imp","incomef6_a","incomef6_b",
                            "fparty6","sSESf6_1", "sSESf6_2",
                            "deprf6_6","deprf6_7", "deprf6_11", "deprf6_12", "deprf6_14","deprf6_16","deprf6_18","deprf6_20",
                            "satisf6", "confif6", 
                            "memoryf6_1", "memoryf6_2","numberf6",
                            "agef6","fid", "hukouf6")

##mother
datam2016 <- subset(dfa6, pid %in% datac_famR6$pid_m)
datam2016 <- datam2016 %>%
  dplyr::select(cfps2016edu, incomeb_imp, incomea, incomeb, qn4001, qn8011, qn8012, #education 1-8, income, politics
                pn406, pn407, pn411, pn412, pn414, pn416, pn418, pn420,
                #pn401, pn402, pn403, pn404, pn405,pn408, pn409, pn410,pn413,pn415, pn417, pn419, #CES  ###Many participants uses the simplified version(6, 7, 11, 12, 14, 16,18, 20) #many NAs  
                qn12012, qn12014, #qm2014, #satisfaction, confidence, happiness(many NA)
                iwr, dwr, ns_w, #cognitive: memory1, memory2, number series(many NA)
                cfps_age, fid16, pa301) #personal information
datam2016[datam2016 < 0] <- NA
summary(datam2016)
names(datam2016) <- c("edum6", "incomem6_b_imp", "incomem6_a", "incomem6_b",
                            "mparty6","sSESm6_1", "sSESm6_2",
                            "deprm6_6","deprm6_7", "deprm6_11", "deprm6_12", "deprm6_14","deprm6_16","deprm6_18","deprm6_20",
                            "satism6", "confim6", "memorym6_1", "memorym6_2",
                            "numberm6",
                            "agem6","fid", "hukoum6")
dataf2016$sSESf6_1[dataf2016$sSESf6_1 ==79]<-NA
datam2016$sSESm6_1[datam2016$sSESm6_1 ==79]<-NA
#family related data
datafam2016 <- subset(dff6, fid16 %in% child6$fid)
datafam2016 <- datafam2016 %>%
  dplyr::select(fincome2, ft1, familysize16, #family income(net income can be compared with2010), family saving, family size
                fid16)
datafam2016[datafam2016 < 0] <- NA
summary(datafam2016)
names(datafam2016)[1:4] <- c("incomefam6","savingfam6", "sizefam6", 
                             "fid")
#merge all relevant variable together
datap2016 <- merge(dataf2016, datam2016, by = "fid")
datap_fam2016 <- merge(datap2016, datafam2016, by= "fid")
names(datap_fam2016)
names(datac_famR6)
data2016 <- merge(datap_fam2016, datac_famR6, by = "fid")
summary(data2016)
# write table
write.csv(data2016, file = "data2016.csv", row.names = FALSE)