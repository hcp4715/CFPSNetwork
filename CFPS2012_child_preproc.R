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
dfc2 <- read.csv("data/csv/2012child.csv", header=T)
dfa2 <- read.csv("data/csv/2012adult.csv", header=T)
dff2 <- read.csv("data/csv/2012family.csv", header=T)
dffr2 <- read.csv("data/csv/2012familyroster.csv", header = T)
data2010 <- read.csv("data2010.csv", header=T)

#######################children
#child related variables
#chose the same participants in children questionnaire as 2010
child2_c <- subset(dfc2, pid %in% data2010$pid)
child2_c <- child2_c %>%
  dplyr::select(wn401, wn402, wn403, wn404, wn405, wn406, wn407, wn408, wn409, wn410, 
                wn411, wn412, wn413, wn414, wn415, wn416, wn417, wn418, wn419, wn420, #CES
                #wm101, wm102, wm103, wm104, wm105, wm106, wm107, wm108, wm110, wm111, wm112,wm113, wm114, #esteem (350NA and NA in adult)
                kz207_b_2, kz201_b_2, # intelligence, cognitive
                #wm201, wm202, wm203, wm204, wm205, wm206, wm207, wm208, wm209, wm210, wm211, wm211, wm212, wm213, wm214, #parent-child relationship (too many NAs)
                cfps2012_age, fid12, wa4, pid, gender2) #personal informatio
child2_c[child2_c< 0] <- NA
summary(child2_c)
names(child2_c)[1:27] <- c("deprc2_1","deprc2_2","deprc2_3","deprc2_4","deprc2_5","deprc2_6","deprc2_7","deprc2_8","deprc2_9","deprc2_10",
                              "deprc2_11", "deprc2_12","deprc2_13","deprc2_14","deprc2_15","deprc2_16","deprc2_17","deprc2_18","deprc2_19","deprc2_20",
                              "intellc2", "cogc2",
                              "agec2", "fid", "hukouc2", "pid", "gender")

#chose the same participants in adult questionnaire as 2010
child2_a <- subset(dfa2, pid %in% data2010$pid)
child2_a <- child2_a %>%
  dplyr::select(qq6011, qq6012, qq6013,qq6014,qq6015,qq6016,qq6017,qq6018,qq6019,
                qq60110,qq60111,qq60112, qq60113,qq60114,qq60115,qq60116,qq60117,qq60118,qq60119,qq60120, #CES
                qz207, qz201, #intelligence, cognitve
                cfps2012_age, fid12, qa301, pid, cfps2012_gender)
child2_a[child2_a < 0] <- NA
summary(child2_a)
names(child2_a)[1:27] <- c("deprc2_1","deprc2_2","deprc2_3","deprc2_4","deprc2_5","deprc2_6","deprc2_7","deprc2_8","deprc2_9","deprc2_10",
                              "deprc2_11", "deprc2_12","deprc2_13","deprc2_14","deprc2_15","deprc2_16","deprc2_17","deprc2_18","deprc2_19","deprc2_20",
                              "intellc2", "cogc2",
                              "agec2", "fid", "hukouc2", "pid", "gender")
child2 <- rbind(child2_c, child2_a)
str(child2)
#recode depression 
child2 <- child2 %>%
  dplyr::mutate(deprc2_4 = recode(deprc2_4, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                deprc2_8 = recode(deprc2_8, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                deprc2_12 = recode(deprc2_12, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                deprc2_16 = recode(deprc2_16, '1' = 4, '2'= 3, '3' = 2, '4' = 1)) %>%
  dplyr::mutate(depr_sumc2 = deprc2_1+deprc2_2+deprc2_3+deprc2_4+deprc2_5+deprc2_6+deprc2_7+deprc2_8+deprc2_9+deprc2_10
               +deprc2_11+deprc2_12+deprc2_13+deprc2_14+deprc2_15+deprc2_16+deprc2_17+deprc2_18+deprc2_19+deprc2_20)

#adult related variables
##father
dataf2012 <- subset(dfa2, pid %in% data2010$pid_f)
dataf2012 <- dataf2012 %>%
  dplyr::select(edu2012, income_adj, qn401_s_1, qn8011, qn8012, #education 1-8, income, politics, subjective ses
                qq6011, qq6012, qq6013,qq6014,qq6015,qq6016,qq6017,qq6018,qq6019,
                qq60110,qq60111,qq60112, qq60113,qq60114,qq60115,qq60116,qq60117,qq60118,qq60119,qq60120, #depression
                qn12012, qn12014, #satisfaction, confidence
                iwr, dwr, #ns_w, #cognitive: memory1, memory2, number series(many NA)
                cfps2012_age, fid12, qa301, pid) #personal information
dataf2012[dataf2012 < 0] <- NA
summary(dataf2012)
dataf2012 <-dataf2012 %>% 
  dplyr::mutate(memory = iwr + dwr,
                qn401_s_1 = recode(qn401_s_1, '1' = 1, .default = 0)) %>%
  dplyr::select(-iwr, -dwr)

names(dataf2012)[1:32] <- c("eduf2", "incomef2", "fparty2", "sSESf2_1", "sSESf2_2",
                            "deprf2_1","deprf2_2","deprf2_3","deprf2_4","deprf2_5","deprf2_6","deprf2_7","deprf2_8","deprf2_9","deprf2_10",
                            "deprf2_11", "deprf2_12","deprf2_13","deprf2_14","deprf2_15","deprf2_16","deprf2_17","deprf2_18","deprf2_19","deprf2_20",
                            "satisf2", "confif2", 
                            "agef2","fid", "hukouf2", "pid_f","memoryf2")
dataf2012$sSESf2_1[dataf2012$sSESf2_1 == 79] <-NA
dataf2012 <- dataf2012 %>%
  dplyr::mutate(deprf2_4 = recode(deprf2_4, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                deprf2_8 = recode(deprf2_8, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                deprf2_12 = recode(deprf2_12, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                deprf2_16 = recode(deprf2_16, '1' = 4, '2'= 3, '3' = 2, '4' = 1))%>%
  dplyr::mutate(depr_sumf2 = deprf2_1+deprf2_2+deprf2_3+deprf2_4+deprf2_5+deprf2_6+deprf2_7+deprf2_8+deprf2_9+deprf2_10
                +deprf2_11+deprf2_12+deprf2_13+deprf2_14+deprf2_15+deprf2_16+deprf2_17+deprf2_18+deprf2_19+deprf2_20,
                posi_sumf2 = satisf2 +confif2)


##mother
datam2012 <- subset(dfa2, pid %in% data2010$pid_m)
datam2012 <- datam2012 %>%
  dplyr::select(edu2012, income_adj, qn401_s_1, qn8011, qn8012, #education 1-8, income, politics, subjective ses
                qq6011, qq6012, qq6013,qq6014,qq6015,qq6016,qq6017,qq6018,qq6019,
                qq60110,qq60111,qq60112, qq60113,qq60114,qq60115,qq60116,qq60117,qq60118,qq60119,qq60120, #depression
                qn12012, qn12014, #satisfaction, confidence
                iwr, dwr, #ns_w, #cognitive: memory1, memory2, number series(many NA)
                cfps2012_age, fid12, qa301,pid)  #personal information
datam2012[datam2012 < 0] <- NA

summary(datam2012)
datam2012 <-datam2012 %>% 
  dplyr::mutate(memory = iwr + dwr,
                qn401_s_1 = recode(qn401_s_1, "1" = 1, .default = 0)) %>%
  dplyr::select(-iwr, -dwr) 
names(datam2012)[1:32] <- c("edum2", "incomem2", "mparty2", "sSESm2_1", "sSESm2_2",
                            "deprm2_1","deprm2_2","deprm2_3","deprm2_4","deprm2_5","deprm2_6","deprm2_7","deprm2_8","deprm2_9","deprm2_10",
                            "deprm2_11", "deprm2_12","deprm2_13","deprm2_14","deprm2_15","deprm2_16","deprm2_17","deprm2_18","deprm2_19","deprm2_20",
                            "satism2", "confim2", 
                            "agem2", "fid", "hukoum2", "pid_m", "memorym2")
datam2012$sSESm2_1[datam2012$sSESm2_1 == 79] <-NA
datam2012$sSESm2_2[datam2012$sSESm2_2 == 79] <-NA
datam2012 <- datam2012 %>%
  dplyr::mutate(deprm2_4 = recode(deprm2_4, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                deprm2_8 = recode(deprm2_8, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                deprm2_12 = recode(deprm2_12, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                deprm2_16 = recode(deprm2_16, '1' = 4, '2'= 3, '3' = 2, '4' = 1))%>%
  dplyr::mutate(depr_summ2 = deprm2_1+deprm2_2+deprm2_3+deprm2_4+deprm2_5+deprm2_6+deprm2_7+deprm2_8+deprm2_9+deprm2_10
              +deprm2_11+deprm2_12+deprm2_13+deprm2_14+deprm2_15+deprm2_16+deprm2_17+deprm2_18+deprm2_19+deprm2_20,
              posi_summ2 = satism2 + confim2)

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
datafamR2012 <- subset(dffr2, pid %in% data2010$pid)
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
