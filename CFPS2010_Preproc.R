################## Preprocessing CFPS data ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    18-09-29         The first version
# Hu, C-P     18-09-30         Add notes, generalizing to other env.
# Cai, Y-Q    18-10-09         Only include SES and mental health, ignore physical ones
# Cai, Y-Q    18-10-21         Add fairness and attributional styles
# Hu, C-P     18-10-22         Review the code, change the absolute path to relative path, and other comments
#
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
#setwd("/Users/apple/Desktop/CFPS/4_analysis")

# load the packages needed, if not exist, download from cran
if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); require(tidyverse)}
if (!require(skimr)) {install.packages("skimr",repos = "http://cran.us.r-project.org"); require(skimr)}
if (!require(userfriendlyscience)) {install.packages("userfriendlyscience",repos = "http://cran.us.r-project.org"); require(userfriendlyscience)}

#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("skimr")
#library("skimr")
#library("dplyr")
#library("tidyverse")

#read data
dfa <- read.table("[CFPS Public Data] 2010 Adult Data (ENG).tab", sep="\t",header=T)#adult 2010

###### get related variables
#ID information
persinfo <- dfa %>%
      select(gender, qa1age)
select

#summarize personal informations
summary(dfa$qa1age)  #age
summary(dfa$gender)  #gender
summary(dfa$qa2==1)  # HCP: what does this mean?
summary(dfa$qa2==3)  #hukou

# Select and recode SES
SES <- dfa %>%
      select(fid, pid, educ, wordtest, mathtest, qg307isco, qg307isei, qk601,qa7_s_1) %>%
      mutate(qa7_s_1 = recode_factor(qa7_s_1, '1' = 1, .default = 0)) #leave only communist members
SES[SES == -8] <- NA #missing values
summary(SES)

# set qa7_s_1 = 1 if any family member is a communist party member, else = 0
pCap <- SES %>%
      select(qa7_s_1, fid) %>%
      mutate(qa7_s_1 = recode_factor(qa7_s_1, '1' = 1, .default = 0)) %>%
      filter(qa7_s_1 == 1)
SES <- SES %>%
      select(-qa7_s_1) %>%
      left_join(pCap, SES, by = "fid")
SES$qa7_s_1[is.na(SES$qa7_s_1)] <- 0

#delate repatitive rows
SES <- SES %>%
      group_by(pid) %>%
      filter(row_number() == 1) %>%
      ungroup()

# recode income using log10, if income =0, recode it as 0 
SES <- SES %>%
  mutate(qk601 = log10(qk601)) %>%
  mutate(qk601 = recode_factor(qk601, '-Inf' = 0 ))

# select and recode mental health, fairness and attributional style
##MH
MH <- dfa %>%
  select(qq601, qq602, qq603, qq604, qq605, qq606, qk802, qm403, qm404)
MH[MH == -8] <- NA

# reliability of depression scale
depr <- MH %>%
  select(qq601, qq602, qq603, qq604, qq605, qq606)

reliability(depr,
            items = NULL,
            itemDiagnostics = FALSE,
            digits = 2)

##fairness
fair <- dfa %>%
  select(qn201, qn202, qn203, qn204, qn205, qn206, qn207, qn208)
fair[fair == 79] <- NA
fair[fair == 5] <- -1 # encounter with unfair affairs? 1 = yes, -1 = no

# reliability of fairness scale
# library("userfriendlyscience")
reliability(fair,     ### Hcp: as for reliability, I suggest you can also try "psych"
            items = NULL,
            itemDiagnostics = FALSE,
            digits = 2)
##attribute
attri <- dfa %>% 
  select(qn501, qn502, qn503, qn504, qn505, qn506, qn507) # 1-4, strongly disagree - strongly agree

### hcp: i think there is a function called 'recode', you can use one line of code for the following codes
attri[attri == -8] <- NA
attri[attri == 1] <- -2 # 1 = strongly disagree
attri[attri == 2] <- -1 # 2 = disagree
attri[attri == 5] <- 0 # 5 = not agree not disagree
attri[attri == 3] <- 1 # 2 = agree
attri[attri == 4] <- 2 # 2 = strongly agree
attri[attri == 6] <- NA # 6 = I don't know

#reliability of attribute scale
reliability(attri,
            items = NULL,
            itemDiagnostics = FALSE,
            digits = 2)
# combine all the related data
all <- cbind(persinfo, SES, MH, attri, fair)
# write the table
write.csv(all, file = "sesMH.csv", row.names = FALSE)
