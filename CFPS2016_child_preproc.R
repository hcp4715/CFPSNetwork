################## Preprocessing CFPS child 2016 preproc ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    19-03-09         The first version
# 
# 
#
###### input######
# 2016child.csv
# 2016adult.csv
# 2016family.csv
#
###### output #####
# data2016.csv
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
dfc6 <- read.csv("data/csv/2016child.csv", header=T)
dfa6 <- read.csv("data/csv/2016adult.csv", header=T)
dff6 <- read.csv("data/csv/2016family.csv", header=T)
dffr6 <- read.csv("data/csv/2016familyroster.csv", header = T)
data2014 <- read.csv("data2014.csv", header=T)

#child related variables
##still child in 2012 (only 5, many all NA)
datac2016_c <- subset(dfc6, pid %in% data2014$pid)
datac2016_c <- datac2016_c %>%
  dplyr::select(pn406, pn407, pn411, pn412, pn414, pn416, pn418, pn420,
                #pn401, pn402, pn403, pn404, pn405, pn406, pn407, pn408, pn409, pn410, 
                #pn411, pn412, pn413, pn414, pn415, pn416, pn417, pn418, pn419, pn420, #CES
                #pm101m, pm102m, pm103m, pm104m, pm105m, pm106m, pm107m, pm108m, pm110m, #esteem 
                kz207_b_2, kz201_b_2, # intelligence, cognitive
                cfps_age, fid16, pa301, pid) #personal information
datac2016_c[datac2016_c < 0] <- NA
summary(datac2016_c)
names(datac2016_c)[1:14] <- c("deprc6_6","deprc6_7","deprc6_11","deprc6_12","deprc6_14","deprc6_16","deprc6_18","deprc6_20",
                              "intellc6", "cogc6",
                              "agec6", "fid", "hukouc6","pid")

##became adult in 2016
datac2016_a <- subset(dfa6, pid %in% data2014$pid)
datac2016_a <- datac2016_a %>%
  dplyr::select(pn406, pn407, pn411, pn412, pn414, pn416, pn418, pn420,
                #pn401, pn402, pn403, pn404, pn405,pn408, pn409, pn410,pn413,pn415, pn417, pn419, #CES  ###Many participants uses the simplified version(6, 7, 11, 12, 14, 16,18, 20) #many NAs  
                #pm101m, pm102m, pm103m, pm104m, pm105m, pm106m, pm107m, pm108m, pm110m, #many NA
                qz207, qz201, #intelligence, cognitve
                cfps_age, fid16, pa301, pid)
datac2016_a[datac2016_a < 0] <- NA
summary(datac2016_a)
names(datac2016_a)[1:14] <- c("deprc6_6","deprc6_7","deprc6_11","deprc6_12","deprc6_14","deprc6_16","deprc6_18","deprc6_20",
                              "intellc6", "cogc6",
                              "agec6", "fid", "hukouc6","pid")
datac2016 <- rbind(datac2016_a, datac2016_c)

#recode depression 
datac2016 <- datac2016 %>%
  dplyr::mutate(#deprc6_4 = recode(deprc6_4, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                #deprc6_8 = recode(deprc6_8, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                deprc6_12 = recode(deprc6_12, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                deprc6_16 = recode(deprc6_16, '1' = 4, '2'= 3, '3' = 2, '4' = 1))%>%
  dplyr::mutate(depr_sumc6 = deprc6_6 +deprc6_7 +deprc6_11 +deprc6_12 +deprc6_14 +deprc6_16 +deprc6_18 +deprc6_20)


#adult related variables
##father
dataf2016 <- subset(dfa6, pid %in% data2014$pid_f)
dataf2016 <- dataf2016 %>%
  dplyr::select(cfps2016edu, incomeb_imp, incomea, incomeb, qn4001,qn8011, qn8012, #education 1-8, income, politics
                pn406, pn407, pn411, pn412, pn414, pn416, pn418, pn420,
                #pn401, pn402, pn403, pn404, pn405,pn408, pn409, pn410,pn413,pn415, pn417, pn419, #CES  ###Many participants uses the simplified version(6, 7, 11, 12, 14, 16,18, 20) #many NAs  
                qn12012, qn12014, #qm2014, #satisfaction, confidence, happiness(many NA)
                iwr, dwr, ns_w, #cognitive: memory1, memory2, number series(many NA)
                cfps_age, fid16, pa301) #personal information
dataf2016[dataf2016 < 0] <- NA
summary(dataf2016)
incomef <- dataf2016 %>%
  dplyr::select(incomeb_imp, incomea, incomeb)
incomef$sumf <- rowSums(incomef, na.rm = TRUE)
dataf2016_income <- cbind(dataf2016, incomef$sum)
dataf2016 <- dataf2016_income %>% 
  dplyr::mutate(memory = iwr + dwr,
                qn4001 = recode(qn4001, "1" = 1, .default = 0)) %>%
  dplyr::select(-iwr, -dwr, -incomeb_imp, -incomea, -incomeb)
str(dataf2016)
names(dataf2016)[1:20] <- c("eduf6", "fparty6","sSESf6_1", "sSESf6_2",
                            "deprf6_6","deprf6_7", "deprf6_11", "deprf6_12", "deprf6_14","deprf6_16","deprf6_18","deprf6_20",
                            "satisf6", "confif6", "numberf6",
                            "agef6","fid", "hukouf6", "incomef6", "memoryf6")
dataf2016 <- dataf2016 %>%
  dplyr::mutate(#deprf6_4 = recode(deprf6_4, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                #deprf6_8 = recode(deprf6_8, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                deprf6_12 = recode(deprf6_12, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                deprf6_16 = recode(deprf6_16, '1' = 4, '2'= 3, '3' = 2, '4' = 1))%>%
  dplyr::mutate(depr_sumf6 = deprf6_6 +deprf6_7 +deprf6_11 +deprf6_12 +deprf6_14 +deprf6_16 +deprf6_18 +deprf6_20,
                posi_sumf6 = satisf6+ confif6)

##mother
datam2016 <- subset(dfa6, pid %in% data2014$pid_m)
datam2016 <- datam2016 %>%
  dplyr::select(cfps2016edu, incomeb_imp, incomea, incomeb, qn4001, qn8011, qn8012, #education 1-8, income, politics
                pn406, pn407, pn411, pn412, pn414, pn416, pn418, pn420,
                #pn401, pn402, pn403, pn404, pn405,pn408, pn409, pn410,pn413,pn415, pn417, pn419, #CES  ###Many participants uses the simplified version(6, 7, 11, 12, 14, 16,18, 20) #many NAs  
                qn12012, qn12014, #qm2014, #satisfaction, confidence, happiness(many NA)
                iwr, dwr, ns_w, #cognitive: memory1, memory2, number series(many NA)
                cfps_age, fid16, pa301) #personal information
datam2016[datam2016 < 0] <- NA
summary(datam2016)
incomem <- datam2016 %>%
  dplyr::select(incomeb_imp, incomea, incomeb)
incomem$sum <- rowSums(incomem, na.rm = TRUE)
datam2016_income <- cbind(datam2016, incomem$sum)
datam2016 <- datam2016_income %>% 
  dplyr::mutate(memory = iwr + dwr,
                qn4001 = recode(qn4001, "1" = 1, .default = 0)) %>%
  dplyr::select(-iwr, -dwr, -incomeb_imp, -incomea, -incomeb)
names(datam2016)[1:20] <- c("edum6", "mparty6","sSESm6_1", "sSESm6_2",
                            "deprm6_6","deprm6_7", "deprm6_11", "deprm6_12", "deprm6_14","deprm6_16","deprm6_18","deprm6_20",
                            "satism6", "confim6", "numberm6",
                            "agem6","fid", "hukoum6", "incomem6", "memorym6")

datam2016 <- datam2016 %>%
  dplyr::mutate(#deprm6_4 = recode(deprm6_4, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                #deprm6_8 = recode(deprm6_8, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                deprm6_12 = recode(deprm6_12, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                deprm6_16 = recode(deprm6_16, '1' = 4, '2'= 3, '3' = 2, '4' = 1))%>%
  dplyr::mutate(depr_summ6 = deprm6_6 +deprm6_7 +deprm6_11 +deprm6_12 +deprm6_14 +deprm6_16 +deprm6_18 +deprm6_20,
                posi_summ6 = satism6+ confim6)
dataf2016$sSESf6_1[dataf2016$sSESf6_1 ==79]<-NA
datam2016$sSESm6_1[datam2016$sSESm6_1 ==79]<-NA
#family related data
datafam2016 <- subset(dff6, fid16 %in% data2014$fid)
datafam2016 <- datafam2016 %>%
  dplyr::select(fincome2, ft1, familysize16, #family income(net income can be compared with2010), family saving, family size
                fid16)
datafam2016[datafam2016 < 0] <- NA
summary(datafam2016)
names(datafam2016)[1:4] <- c("incomefam6","savingfam6", "sizefam6", 
                             "fid")

#family roster: extract variable "pid still live in the household"
summary(dffr6)
datafamR2016 <- subset(dffr6, pid %in% data2012$pid)
datafamR2016 <- datafamR2016 %>%
  dplyr::select(tb6_a16_p, pid)
names(datafamR2016)[1:2] <- c("left6","pid")


#merge all relevant variable together
datap2016 <- merge(dataf2016, datam2016, by = "fid")
datap_fam2016 <- merge(datap2016, datafam2016, by= "fid")
datap_fam_c2016 <- merge(datap_fam2016, datac2016, by="fid")
data2016 <- merge(datap_fam_c2016, datafamR2016, by = "pid")
summary(data2016)
# write table
write.csv(data2016, file = "data2016.csv", row.names = FALSE)
summary(data2016)
