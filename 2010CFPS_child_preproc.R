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
dfc <- read.table("/Users/apple/Desktop/CFPS/4_Analysis/[CFPS Public Data] 2010 Child Data (ENG).tab", sep="\t",header=T)#adult 2010
summary(dfc == -8)

#ID information
##to distinguish child from adult, data and variable are coded as e.g.persinfoc
persinfoc <- dfc %>%
  dplyr::select(gender, wa1age, fid, pid, childgroup)
summary(persinfoc)
persinfoc[persinfoc == -8] <-NA
##NOTICE: childgroup = 1, age<1; 2, 1<=age<3; 3, 3<=age<6; 4, 6<=age<16
#age:10-15
child10_15 <- dfc %>%
  dplyr::filter(wa1age >= 10) #select children older than 10(with both 自答 and 代答)

##SES: human
hCapc <- child10_15 %>%
  dplyr::select(edu2010, wordtest, mathtest, wh9, wd2) # education level in 2010, wordtest, mathtest, education expactancy of child, education expactancy of child's parents
hCapc$wh9[hCapc$wh9 == 9] <- 1 # education expectancy, 9=不必念书, 2-8=小学-博士
hCapc[hCap < 0 ] <- NA
summary(hCapc)
##SES: material
mCapc <- child10_15 %>%
  dplyr::select(moccupisco, foccupisco) # mother occupation, father occupation, family income
#INCOME?
##SES: political
pCapc <- child10_15 %>%
  dplyr::select(mparty, fparty) %>%
  dplyr::mutate(mparty = recode(mparty, "1" = 1, .default = 0),
         fparty = recode(fparty, "1" = 1, .default = 0)) 

#MH
MHc <- child10_15 %>%
# depression, social1, social2, happiness, confidence in future, observation-cognitive, observation-intelligence
  dplyr::select(depression, wm301, wm304, wm302, wm303, wz201, wz207) 
MHc[MHc < 0] <- NA
summary(MHc)

#reliability of depression
depr <- child10_15 %>%
  dplyr::select(wn401, wn402, wn403, wn404, wn405, wn406) 
psych::alpha(depr)

#attribution
attric <- child10_15 %>%
  dplyr::select(wm401, wm402, wm403, wm404, wm405, wm406, wm407,  #children
                we401, we402, we403, we404, we405, we406, we407)  #parents answer for children
attric[attric < 0] <- NA
summary(attric) #####NOTICE: many missing values

#Goal
goalc <- child10_15 %>%
  dplyr::select(wn501, wn502, wn503, wn504, wn505, wn506, wn507, wn508, wn509, wn510, wn511)
goalc[goalc < 0] <- NA
summary(goalc) #####NOTIC: many missing values

##?father's pid and mother's pid--parents SES and child's happiness
