################## Preprocessing CFPS data ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    18-09-29         The first version
# Hu, C-P     18-09-30         Add notes, generalizing to other env.
# Cai, Y-Q    18-10-09         Only include SES and mental health, ignore physical ones
#
#
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
#set directory to the folder of analytic data

# Get the current working directory in Rstudio
curWD <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curWD)
setwd("/Users/apple/Desktop/CFPS/4_analysis")
# getwd()
install.packages("dply")
#read data
dfa<-read.table("/Users/apple/Desktop/CFPS/4_analysis/[CFPS Public Data] 2010 Adult Data (ENG).tab", sep="\t",header=T)#adult 2010
dff<-read.table("/Users/apple/Desktop/CFPS/4_analysis/[CFPS Public Data] 2010 Family Data (ENG).tab", sep="\t",header=T)#family 2010
dfrf<-read.table("/Users/apple/Desktop/CFPS/4_analysis/[CFPS Public Data] 2010 Family Roster Data (ENG).tab", sep="\t",header=T)#family roster

#ID information
summary(dfa$qa1age)#age
summary(dfa$qa2==1)
summary(dfa$qa2==3)#hukou

###### get related variables
# list of variables and corresponding column names
idName <- c("pid","fid","qa1age")
hCapname <- c("educ","wordtest","mathtest") #human capital: education and cognitive
mCapname <- c("pid","qg307isco","qg307isei","qg307siops", "qk601") #material capital: occupation and income
pCapname <- c("qa7_s_1")
#memberinfoname <- c("t1_c1","t1_c2","t1_c3","t1_c4","t1_c5","t1_c6","t1_c7","t1_c8","t1_c9","t1_c10","t1_f","t1_m","t1_s") #pid, fid, father, mother, spouse and children
Health_m_name <- c("qq601","qq602","qq603","qq604","qq605","qq606","qk802","qm404","qm403")

# select data
df.sesMHname = c(idName, hCapname, mCapname, pCapname, Health_m_name)
df.sesMH<- dfa[, df.sesMHname]

#choose the first member of the same family
x<-df.sesMH$pid-df.sesMH$fid*1000
df.sesMH$member<-x
df.sesMH_1 <-filter(df.sesMH, member=="101") 

#ignore none appliable data
df.sesMH_1[df.sesMH_1<0]<-NA

  ### SES variables
## Human Capital:  education and cognitive
## Material captial
df.sesMH_1$qk601[df.sesMH_1$qk601==0]<-NA
quantile(df.sesMH_1$qk601, probs = c(0.25, 0.5, 0.75), na.rm=TRUE) #find quantiles and median
#divide income into 4 levels
df.sesMH_1$income[df.sesMH_1$qk601>=15600]<-4
df.sesMH_1$income[df.sesMH_1$qk601<15600 & df.sesMH_1$qk601>7500]<-3
df.sesMH_1$income[df.sesMH_1$qk601<7500 & df.sesMH_1$qk601>=2000]<-2
df.sesMH_1$income[df.sesMH_1$qk601<2000]<-1

####if member of the family is required, unannotate following
# calculate the income per capita/member
#memberinfoname <- c("t1_c1","t1_c2","t1_c3","t1_c4","t1_c5","t1_c6","t1_c7","t1_c8","t1_c9","t1_c10","t1_f","t1_m","t1_s","pid","fid") #pid, fid, father, mother, spouse and children
#memberinfo <- dfrf[,memberinfoname] #extract data
#membername <- c("t1_c1","t1_c2","t1_c3","t1_c4","t1_c5","t1_c6","t1_c7","t1_c8","t1_c9","t1_c10","t1_f","t1_m","t1_s") # father, mother, spouse and children
#member <- dfrf[,membername]
#summary(member) #check member
#memberinfo$number <- rowSums(member,na.rm=TRUE,dims=1)+1 #count family member, include the participant

## political captial
#pCap=c("pid","fid","qa7_s_1")#political
df.sesMH_1$qa7_s_1[df.sesMH_1$qa7_s_1!=1]<-0 #leave only communist party

###if calculate all family member's political capital, unannotate following
# add politic by family
#x<-aggregate(qa7_s_1~fid,data=pCap,sum)
#merge x with politic
#pol_f <- merge(x, pCap, by="fid")
#extract politic variables
#politicfamilyname=c("fid","pid","qa7_s_1.x")
#politicfamily=pol_f[,c("fid","pid","qa7_s_1.x")]#extract data
#summary(politicfamily) #check politicfamily

###################################################
###################################################
##health variables
#physical
#Health_p_name <- c("qp3","qx3","qx4","qx5","qx6","qz202","qq2","qq3","pid","fid") #self-reated, limitation, observed, smoke and drink
#phy=dfa[,physicalname] #extract data
#phy[phy<0]<-NA #clear non-existed data
#summary(phy) #check physical
#mental
#reliability of depression scale
#install.packages("userfriendlyscience")##if necessary, delete "#"
library("userfriendlyscience")
deprname=c("qq601","qq602","qq603","qq604","qq605","qq606")
depr=df.sesMH_1[,deprname]
reliability(depr,
              items = NULL,
              itemDiagnostics = FALSE,
              digits = 2)
#write the table
df.sesMH_1$fid<-NULL
df.sesMH_1$qa1age<-NULL
df.sesMH_1$pid.1<-NULL
df.sesMH_1$member<-NULL
write.csv(df.sesMH_1, file="sesMH.csv", row.names = FALSE)
