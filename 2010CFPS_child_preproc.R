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
dfc <- read.table("data/[CFPS Public Data] 2010 Child Data (ENG).tab", sep="\t",header=T)#adult 2010
dff <- read.table("data/[CFPS Public Data] 2010 Family Data (ENG).tab", sep="\t",header=T)#adult 2010
dfa <-  read.table("data/[CFPS Public Data] 2010 Adult Data (ENG).tab", sep="\t",header=T)#adult 2010
summary(dfc == -8)

##NOTICE: childgroup = 1, age<1; 2, 1<=age<3; 3, 3<=age<6; 4, 6<=age<16
#age:10-15
child10_15 <- dfc %>%
  dplyr::filter(wa1age >= 10)%>% #select children older than 10(with both answer themselves and parents answer for them)
  dplyr::filter(pid_f != -8, pid_m != -8) %>% #select children whose father and mother have questionnaire
  dplyr::group_by(fid) %>% #select one children in each family
  dplyr::filter(row_number() == 1) %>%
  dplyr::ungroup()
#ID information
##to distinguish child from adult, data and variable are coded as e.g.persinfoc
persinfoc <- child10_15 %>%
  dplyr::select(gender, wa1age, fid, pid) 
summary(persinfoc)

##family SES: human- education of father and mother
hCapc <- child10_15 %>%
  dplyr::select(tb4_a_f, tb4_a_m) # highest education level of father, of mother
hCapc[hCapc < 0 ] <- NA 
summary(hCapc)

##family SES: material
#family income
incomef <- dff %>%
  dplyr::select(ff601, ff701, fid) 
incomef[incomef < 0 ] <- NA
incomef <- incomef %>%
  dplyr::mutate(incomef = log10(ff601 + ff701)) %>% # calculate total income of the family
  dplyr::mutate(incomef = recode_factor(incomef, '-Inf' = 0)) %>%
  dplyr::select(incomef, fid)
incomef <- merge.data.frame(incomef, persinfoc, by= 'fid')
#individual income (father and mother, incomep)
incomep <- dfa %>%
  dplyr::select(fid, pid, qk601) %>%
  dplyr::mutate(income = log10(qk601)) %>%
  dplyr::mutate(income = recode_factor(income, '-Inf' = 0 )) %>%
  dplyr::select(-qk601)
pidf <- child10_15 %>%
  dplyr::select(pid_f)
pidm <- child10_15 %>%
  dplyr::select(pid_m)
incomefather <- merge.data.frame(incomep, pidf, by.x = 'pid', by.y = 'pid_f')
names(incomefather)[1:3] <- c("pidf", "fid", "incomefa")
incomemother <- merge.data.frame(incomep, pidm, by.x = 'pid', by.y = 'pid_m')
names(incomemother)[1:3] <- c("pidm", "fid", "incomemo")
incomeparents <- left_join(incomefather, incomemother)
incomefamily <- left_join(incomeparents, incomef)
#occupation father and mother
occupc <- child10_15 %>%
  dplyr::select(moccupisco, foccupisco, fid) # mother occupation, father occupation, family income
mCapc <- merge.data.frame(incomefamily, occupc, by = "fid") # merge income and occupation

##SES: political
pCapc <- child10_15 %>%
  dplyr::select(pid, mparty, fparty) %>%
  dplyr::mutate(mparty = recode(mparty, "1" = 1, .default = 0),
         fparty = recode(fparty, "1" = 1, .default = 0)) 
#pCapc <- pCapc %>%
#  dplyr::mutate(polc = fparty + mparty) %>%
#  dplyr::select(polc)

#Mental health
MHc <- child10_15 %>%
# depression x6,  happiness, confidence in future, observation-cognitive, observation-intelligence
  dplyr::select(wn401, wn402, wn403, wn404, wn405, wn406, wm302, wm303, wz201, wz207) 
MHc[MHc < 0] <- NA
summary(MHc)
#parents mental health
MHa <- dfa %>% #happiness, life satisfaction, confidence in future, depression X6, cognition
  dplyr::select(pid, qm403, qm404, qk802, qq601, qq602, qq603, qq604, qq605, qq606, wordtest, mathtest)
MHa[MHa < 0] <- NA # MH = -8/-2 not appliable
summary(MHa)

MHfather <- merge.data.frame(MHa, pidf, by.x = 'pid', by.y = 'pid_f')
names(MHfather)[1:12] <- c("pidf", "qm403f", "qm404f", "qk802f", "qq601f", "qq602f", "qq603f", "qq604f", "qq605f", "qq606f", "wordtestf", "mathtestf")
MHmother <- merge.data.frame(MHa, pidm, by.x = 'pid', by.y = 'pid_m')
names(MHmother)[1:12] <- c("pidm", "qm403m", "qm404m", "qk802m", "qq601m", "qq602m", "qq603m", "qq604m", "qq605m", "qq606m", "wordtestm", "mathtestm")

#attribution
#attric <- child10_15 %>%
#  dplyr::select(wm401, wm402, wm403, wm404, wm405, wm406, wm407,  #children
#                we401, we402, we403, we404, we405, we406, we407)  #parents answer for children
#attric[attric < 0] <- NA
#summary(attric) #####NOTICE: many missing values

#Goal
#goalc <- child10_15 %>%
#  dplyr::select(wn501, wn502, wn503, wn504, wn505, wn506, wn507, wn508, wn509, wn510, wn511)
#goalc[goalc < 0] <- NA
#summary(goalc) #####NOTIC: many missing values

#merge all relevant variable together
allc <- cbind(persinfoc, hCapc, mCapc, pCapc, MHc, MHfather, MHmother)
summary(allc)
str(allc)
allc$pid <-NULL
allc$pid <-NULL
allc$pidm <-NULL
allc$pidf <-NULL
allc$fid <-NULL

# write table
summary(allc)
write.csv(allc, file = "sesMHc.csv", row.names = FALSE)
