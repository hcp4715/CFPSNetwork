################## Preprocessing CFPS data ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    18-09-29         The first version
# Hu, C-P     18-09-30         Add notes, generalizing to other env.
# 
#
#
#
###### input######
# [CFPS Public Data] 2010 Adult Data (ENG).tab
# [CFPS Public Data] 2010 Family Data (ENG).tab
# [CFPS Public Data] 2010 Family Roster Data (ENG).tab

###### output #####
#
#
#
#
######################## Start of the script ###########################
#set directory to the folder of analytic data

# Get the current working directory in Rstudio
curWD <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curWD)
# setwd("/Users/apple/Desktop/CFPS/4_analysis")
# getwd()

#read data
dfa<-read.table("[CFPS Public Data] 2010 Adult Data (ENG).tab", sep="\t",header=T)#adult 2010
dff<-read.table("[CFPS Public Data] 2010 Family Data (ENG).tab", sep="\t",header=T)#family 2010
dfrf<-read.table("[CFPS Public Data] 2010 Family Roster Data (ENG).tab", sep="\t",header=T)#family roster

#ID information
summary(dfa$qa1age)#age
summary(dfa$qa2==1)
summary(dfa$qa2==3)#hukou

###### get related variables
# list of variables and corresponding column names
idName <- c("pid","fid")
hCapname <- c("educ","wordtest","mathtest") #human: education and cognitive
mCapname1 <- c("qg307isei","qg307siops","qg307egp") #material capitl - occupation
mCapname2=c("fid","ff601","ff701","ff8")#material-income
memberinfoname <- c("t1_c1","t1_c2","t1_c3","t1_c4","t1_c5","t1_c6","t1_c7","t1_c8","t1_c9","t1_c10","t1_f","t1_m","t1_s") #pid, fid, father, mother, spouse and children
Health_p_name <- c("qp3","qx3","qx4","qx5","qx6","qz202","qq2","qq3") #self-reated, limitation, observed, smoke and drink
Health_m_name <- c("qq601","qq602","qq603","qq604","qq605","qq606","qk802","qm404","qm403")

# select data
df.sesMH <- 
      
### SES variables
## Human Capital:  education and cognitive
#humanCapname <- c("pid","fid","educ","wordtest","mathtest") #human: education and cognitive
humanCap <- dfa[, c("pid","fid","educ","wordtest","mathtest")]#extract data
humanCap$wordtest[humanCap$wordtest==-8]<-NA
humanCap$mathtest[humanCap$mathtest==-8]<-NA #replace none existed data
summary(humanCap) #check human

## Material captial
# Material captial 1: occupation
mCapname1=c("pid","fid","qg307isei","qg307siops","qg307egp") #material capitl - occupation
mCap1 <- dfa[,mCapname1] #extractdata
mCap1$qg307egp[mCap1$qg307egp<0]<-NA
mCap1$qg307egp[mCap1$qg307egp>11]<-NA #replace none existed data
summary(mCap1) #check material1

# Material captial 2: income
mCapname2=c("fid","ff601","ff701","ff8")#material-income
mCap2 <- dff[,mCapname2] #extract data
mCap2$ff601[mCap2$ff601<0]<-NA
mCap2$ff701[mCap2$ff701<0]<-NA
mCap2$ff8[mCap2$ff8<0]<-NA #replace none existed data
summary(mCap2) #check material2

# calculate the income per capita/member
memberinfoname <- c("t1_c1","t1_c2","t1_c3","t1_c4","t1_c5","t1_c6","t1_c7","t1_c8","t1_c9","t1_c10","t1_f","t1_m","t1_s","pid","fid") #pid, fid, father, mother, spouse and children
memberinfo <- dfrf[,memberinfoname] #extract data
membername <- c("t1_c1","t1_c2","t1_c3","t1_c4","t1_c5","t1_c6","t1_c7","t1_c8","t1_c9","t1_c10","t1_f","t1_m","t1_s") # father, mother, spouse and children
member <- dfrf[,membername]
member[member==-8]<-NA #replace none existed data
summary(member) #check member
memberinfo$number <- rowSums(member,na.rm=TRUE,dims=1)+1 #count family member, include the participant

#merge member with material2
data <- merge(mCap2, memberinfo, by="fid")

#calculate income per capita
incomename <- c("ff601","ff701","ff8") #income variables
income <- data[,incomename]#extract data
data$allincome <- rowSums(income,na.rm=T,dims=1)#sum income
data$perincome <- data$allincome/data$number #per capita income
summary(data$perincome) #check perincome
# tempname<-  #perincome variables
temp <- data[,c("fid","pid","perincome")]#extract data
mCap <-merge(mCap1,temp, by="pid") #merge occupation and income
mCap$fid=mCap$fid.x #replace fid.x fid.y with fid
mCap$fid.x<-NULL
mCap$fid.y<-NULL #delete fid.x and fid.y
summary(mCap)#check material

## political captial
#pCap=c("pid","fid","qa7_s_1")#political
pCap <- dfa[,c("pid","fid","qa7_s_1")] #extract data
pCap$qa7_s_1[pCap$qa7_s_1!=1]<-0 #leave only communist party
summary(pCap) #check politic

# add politic by family
x<-aggregate(qa7_s_1~fid,data=pCap,sum)
#merge x with politic
pol_f <- merge(x, pCap, by="fid")
#extract politic variables
#politicfamilyname=c("fid","pid","qa7_s_1.x")
politicfamily=pol_f[,c("fid","pid","qa7_s_1.x")]#extract data
summary(politicfamily) #check politicfamily

#merge SES
SES <- merge(humanCap,mCap,by="pid")
SES <- merge(SES,politicfamily, by="pid")
#clear useless variables
SES$fid.x<-NULL
SES$fid.y<-NULL

###################################################
###################################################
##health variables
#physical
Health_p_name <- c("qp3","qx3","qx4","qx5","qx6","qz202","qq2","qq3","pid","fid") #self-reated, limitation, observed, smoke and drink
phy=dfa[,physicalname] #extract data
phy[phy<0]<-NA #clear non-existed data
summary(phy) #check physical
#mental
mentalname=c("qq601","qq602","qq603","qq604","qq605","qq606","qk802","qm404","qm403","pid","fid")
mental=dfa[,mentalname]#extract data
mental[mental<0]<-NA #clear non-existed data
summary(mental)#check mental
#merge health
health<- merge(mental,phy, by=c("pid","fid"))
#merge SES and health
sesandhealth<-merge(SES, health, by=c("pid","fid"))
#export table
write.csv(sesandhealth, file="SESandHEALTH.csv",row.names=FALSE)
#reliability of depression scale
#install.packages("userfriendlyscience")##if necessary, delete "#"
library("userfriendlyscience")
deprname=c("qq601","qq602","qq603","qq604","qq605","qq606")
depr=dfa[,deprname]
reliability(depr,
            items = NULL,
            itemDiagnostics = FALSE,
            digits = 2)