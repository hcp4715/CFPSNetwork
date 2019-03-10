################## Preprocessing CFPS child data ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    
# 
# 
#
###### input######
# 
#
#
#
###### output #####
#
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
dfc2 <- read.csv("data/2012child.csv", header=T)
dfa2 <- read.csv("data/2012adult.csv", header=T)
dfc4 <- read.csv("data/2014child.csv", header=T)
dfa4 <- read.csv("data/2014adult.csv", header=T)
dfc6 <- read.csv("data/2016child.csv", header=T)
dfa6 <- read.csv("data/2016adult.csv", header=T)

#select children from 2010 (age 10-15)
child0 <- dfc0 %>%
  dplyr::filter(wa1age >= 10) %>% #select children older than 10(with both answer themselves and parents answer for them)
  dplyr::filter(pid_f != -8, pid_m != -8) #select children whose father and mother have questionnaire
child0 <- child0 %>%
  dplyr::select(gender, wa1age, fid, pid, pid_f, pid_m) 
names(child0)[1:6] <- c("gender0", "age0", "fid0", "pid", "pid_f0", "pid_m0")
#select same children & adults as 2010 from 2012
child2_c <- subset(dfc2, pid %in% child0$pid) 
summary(child2_c$pid_f)
summary(child2_c$pid_m)
child2_c <- child2_c %>%
  dplyr::select(cfps2012_gender, cfps2012_age, fid12, pid, pid_f, pid_m)
child2_a <- subset(dfa2, pid %in% child0$pid) 
summary(child2_a$pid_f)
summary(child2_a$pid_m)
child2_a <- child2_a %>%
  dplyr::select(cfps2012_gender, cfps2012_age, fid12, pid, pid_f, pid_m)
child2 <- rbind(child2_a, child2_c)
names(child2)[1:6] <- c("gender2", "age2", "fid2", "pid", "pid_f2", "pid_m2")

#select same children & adults as 2012 from 2014
child4_c <- subset(dfc4, pid %in% child2$pid) 
summary(child4_c$pid_f)
summary(child4_c$pid_m)
child4_c <- child4_c %>%
  dplyr::select(cfps_gender, cfps2014_age, fid14, pid, pid_f, pid_m)
child4_a <- subset(dfa4, pid %in% child2$pid) 
summary(child4_a$pid_f)
summary(child4_a$pid_m)
child4_a <- child4_a %>%
  dplyr::select(cfps_gender, cfps2014_age, fid14, pid, pid_f, pid_m)
child4 <- rbind(child4_a, child4_c)
names(child4)[1:6] <- c("gender4", "age4", "fid4", "pid", "pid_f4", "pid_m4")

#select same children & adults as 2014 from 2016
child6_c <- subset(dfc6, pid %in% child4$pid) 
summary(child6_c$pid_f)
summary(child6_c$pid_m)
child6_c <- child6_c %>%
  dplyr::select(cfps_gender, cfps_age, fid16, pid, pid_f, pid_m)
child6_a_incomplete <- subset(dfa6, pid %in% child4$pid) 
child6_a_incomplete <- child6_a_incomplete %>%
  dplyr::select(cfps_gender, cfps_age, fid16, pid)
##no idea why 2016 adult doesn't have pid_f and pid_m so extract their pid from 2014adult
pid_p6 <- subset(child4, pid %in% child6_a_incomplete$pid)
pid_p6 <- pid_p6 %>%
  dplyr::select(pid, pid_f4, pid_m4)
child6_a <- merge.data.frame(child6_a_incomplete, pid_p6, by = 'pid')
names(child6_a)[1:6] <- c("pid", "gender6", "age6", "fid6", "pid_f6", "pid_m6")
names(child6_c)[1:6] <- c("gender6", "age6", "fid6", "pid", "pid_f6", "pid_m6")
child6 <- rbind(child6_a, child6_c)

#merge all 4 table
#merge 16 and 14
child_6_4 <- merge(child6, child4, by= "pid")
##check if gender, fid, pid_f/m are identical in 16 and 14
child_6_4 <- child_6_4 %>%
  dplyr::mutate(c1 = gender6 - gender4,
                c2 = fid6 - fid4,
                c3 = pid_f6 - pid_f4,
                c4 = pid_m6 - pid_m6) %>%
  dplyr::filter(c1 == 0, c2 == 0, c3 == 0, c4 == 0) %>%
  dplyr::select(pid, age6, age4, pid_f6, pid_m6, gender6, fid6)

#merge 12 and 10
child_0_2 <- merge(child0, child2, by= "pid")
##check if gender, fid, pid_f/m are identical in 10 and 12
child_0_2 <- child_0_2 %>%
  dplyr::mutate(c1 = gender0 - gender2,
                c2 = fid0 - fid2,
                c3 = pid_f0 - pid_f2,
                c4 = pid_m0 - pid_m2) %>%
  dplyr::filter(c1 == 0, c2 == 0, c3 == 0, c4 == 0) %>%
  dplyr::select(pid, age0, age2, pid_f0, pid_m0, gender0, fid0)

#merge 16/14 and 12/10
child_per <- merge(child_6_4, child_0_2, by= "pid")
##check if gender, fid, pid_f/m are identical in 16(14) and 10(12)
child_per <- child_per %>%
  dplyr::mutate(c1 = gender0 - gender6,
                c2 = fid0 - fid6,
                c3 = pid_f0 - pid_f6,
                c4 = pid_m0 - pid_m6) %>%
  dplyr::filter(c1 == 0, c2 == 0, c3 == 0, c4 == 0) %>%
  dplyr::select(pid, age0, age2, age4, age6, pid_f0, pid_m0, gender0, fid0)
names(child_per)[1:9] <- c("pid", "age0", "age2", "age4", "age6", "pid_f", "pid_m", "gender", "fid")

#select one child from one family
child_per <- child_per %>%
  dplyr::group_by(fid) %>% #select one children in each family
  dplyr::filter(row_number() == 1) %>%
  dplyr::ungroup()
#write csv
write.csv(child_per, file = "child_per.csv", row.names = FALSE)

