install.packages("openxlsx")
library("openxlsx")
hospital_cost<-read.xlsx("1555054100_hospitalcosts.xlsx")
summary(hospital_cost)

#To record the patient statistics, the agency wants to find the age category of people who frequent the hospital and has the maximum expenditu

hist(hospital_cost$AGE,main="Histogram of frequency of the patients",
     xlab="Age",ylab="Frequency")
age=as.factor(hospital_cost$AGE)
summary(age)
which.max(summary(age)) #gives age which visits frequent to the hospital
expnd=aggregate(TOTCHG~AGE,FUN=sum,data=hospital_cost)
expnd
max(expnd) #gives maximum expenditure


#In order of severity of the diagnosis and treatments and to find out the expensive treatments, the agency wants to find the diagnosis-related group that has maximum hospitalization and expenditure.

hist(hospital_cost$APRDRG,main="Histogram of Diagnosis Related Groups",
     xlab="Treatment",ylab="Frequency" )
dgroup=as.factor(hospital_cost$APRDRG)
summary(dgroup)
which.max(summary(dgroup)) #gives the type of APRDRG which has maximum hospitalisation.
expnd2=aggregate(TOTCHG~APRDRG,FUN=sum,data=hospital_cost)
expnd2
max(expnd2) #gives maximum expenditure 
expnd2[which.max(expnd2$TOTCHG),] #gives index of maximum expenditure 

#To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs.
race=as.factor(hospital_cost$RACE)
summary(race)
hospital_cost=na.omit(hospital_cost)
Aov=aov(TOTCHG~RACE,data=hospital_cost)
summary(Aov)

#To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and gender for the proper allocation of resources.
gender=as.factor(hospital_cost$FEMALE)
reg1=lm(formula=TOTCHG~AGE+FEMALE,data=hospital_cost)
summary(reg1)
summary(gender)

#Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can be predicted from age, gender, and race.
race=as.factor(hospital_cost$RACE)
reg2=lm(formula=LOS~AGE+FEMALE+RACE,data=hospital_cost)
summary(reg2)

#To perform a complete analysis, the agency wants to find the variable that mainly affects hospital costs.
reg3=lm(formula=TOTCHG~.,data=hospital_cost)
summary(reg3)

  