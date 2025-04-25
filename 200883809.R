#Load data and libraries
library(foreign)
bsa16<-read.dta("bsa16_to_ukda.dta")
library(psych)
library(MASS)
library(car)
library(ggplot2)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(sjstats)

#Run table to see the distribution of national identity in England
table(bsa16$NatId)

#Create new variable with national identity in 3 categories
bsa16$NatID3Cat <- NA
bsa16$NatID3Cat[bsa16$NatId=="English not British"|bsa16$NatId=="More English than British"] <- 1
bsa16$NatID3Cat[bsa16$NatId=="More British than English"|bsa16$NatId=="British not English"] <- 2
bsa16$NatID3Cat[bsa16$NatId=="Equally English and British"] <- 3
bsa16$NatID3Cat[bsa16$NatId=="Not applicable"|bsa16$NatId=="Other description (WRITE IN)"|
                  bsa16$NatId=="(None of these)"|bsa16$NatId=="Don't know"|bsa16$NatId=="Refusal"] <- NA
bsa16$NatID3Cat <- as.factor(bsa16$NatID3Cat)
levels(bsa16$NatID3Cat)<-c("English", "British", "Equally English and British")
table(bsa16$NatID3Cat)

#Run table to see the distribution of 'What should Britain's long-term policy be?' 
table(bsa16$ECPolicy2)

#create new variable 'ECPolicy' in 2 categories
bsa16$ECPolicy <- NA
bsa16$ECPolicy[bsa16$ECPolicy2=="stay in the EU and try to reduce the EU powers"|
                 bsa16$ECPolicy2=="stay in the EU and try to keep the EU powers as they are"|
                 bsa16$ECPolicy2=="stay in the EU and try to increase the EU powers"] <- 1
bsa16$ECPolicy[bsa16$ECPolicy2=="leave the European Union,"] <- 2
bsa16$ECPolicy[bsa16$ECPolicy2=="Schedule not applicable"|bsa16$ECPolicy2=="Item not applicable"|
                 bsa16$ECPolicy2=="work for the formation of a single European government"|
                 bsa16$ECPolicy2=="Don't Know"|bsa16$ECPolicy2=="Refusal"] <- NA
bsa16$ECPolicy <- as.factor(bsa16$ECPolicy)
levels(bsa16$ECPolicy)<-c("Stay in the EU", "Leave the EU")
table(bsa16$ECPolicy)

#Run table to see the distribution of age categories
table(bsa16$RAgecat3)

#recode age to exclude DK/Refusal 
bsa16$RAgecat3[bsa16$RAgecat3=="DK/Ref"] <-NA 
bsa16$RAgecat3<-droplevels(bsa16$RAgecat3)
table(bsa16$RAgecat3)

#run table to see the distribution of occupation
table(bsa16$RClassGp)

#recode to exclude 'Not aplicable' and 'Not classifiable'
bsa16$RClassGp[bsa16$RClassGp=="Not classifiable" |bsa16$RClassGp=="Not applicable"] <-NA
bsa16$RClassGp<-droplevels(bsa16$RClassGp)
table(bsa16$RClassGp)

#run table to see the distribution of current concern about immigration
table(bsa16$CrPImm)
#recode occupation to define missing values
bsa16$CrPImm[bsa16$CrPImm=="Schedule not applicable" |bsa16$CrPImm=="Item not applicable"|
bsa16$CrPImm=="Don't Know"|bsa16$CrPImm=="Refusal"] <-NA
bsa16$CrPImm<-droplevels(bsa16$CrPImm)
table(bsa16$CrPImm)

#run table to see the distribution of employment status
table(bsa16$REconPos)
#create new variable for employment status with 6 categories
bsa16$EcoPos6 <- NA
bsa16$EcoPos6[bsa16$REconPos=="Employee (full-time)"|bsa16$REconPos=="Employee (part-time)"|
bsa16$REconPos=="In work (status not known)"]<-1
bsa16$EcoPos6[bsa16$REconPos=="Self-employed (p-t)"|bsa16$REconPos=="Self-employed (f-t)"]<-2
bsa16$EcoPos6[bsa16$REconPos=="Unemployed"|bsa16$REconPos=="Waiting to take up work"]<-3
bsa16$EcoPos6[bsa16$REconPos=="Looking after the home"]<-4
bsa16$EcoPos6[bsa16$REconPos=="In f-t education"]<-5
bsa16$EcoPos6[bsa16$REconPos=="Retired"]<-6
bsa16$EcoPos6[bsa16$REconPos=="Other"|bsa16$REconPos=="Don't know"|bsa16$REconPos=="Refusal"]<-NA
bsa16$EcoPos6 <- as.factor(bsa16$EcoPos6)
levels(bsa16$EcoPos6)<-c("Employee","Self-employed","Unemployed","Looking after home",
"In f-t education","Retired") 
table(bsa16$EcoPos6)

#run table to see the distribution of education
table(bsa16$HEdQual3)
#recode education to define missing values
bsa16$HEdQual3[bsa16$HEdQual3=="DK/Refusal/NA"]<-NA
bsa16$HEdQual3<-droplevels(bsa16$HEdQual3)
table(bsa16$HEdQual3)

#run table to see the distribution of party identification
table(bsa16$PartyID3)
#recode to define missing values
bsa16$PartyID3[bsa16$PartyID3=="Not applicable"|bsa16$PartyID3=="Other/DK/Ref"|
bsa16$PartyID3=="Other party"|bsa16$PartyID3=="None"]<-NA
bsa16$PartyID3<-droplevels(bsa16$PartyID3)
table(bsa16$PartyID3)

#run table to see the distribution of where people live
table(bsa16$ResPres)
#recode to define missing values
bsa16$ResPres[bsa16$ResPres=="Schedule not applicable"|bsa16$ResPres=="Item not applicable"|
bsa16$ResPres=="Don't Know"|bsa16$ResPres=="Refusal"|bsa16$ResPres=="(Other answer (WRITE IN))"]<-NA
bsa16$ResPres<-droplevels(bsa16$ResPres)
table(bsa16$ResPres)

#Univariate Analysis

#create object called tabECPolicy which contains the univariate distribution of variable ECPolicy
tabECPolicy<-table(bsa16$ECPolicy)
tabECPolicy
addmargins(tabECPolicy)
#as a proportion
prop.table(tabECPolicy)
#as a percentage
prop.table(tabECPolicy)*100
#as a bar chart which excludes missing values
ggplot(bsa16[!is.na(bsa16$ECPolicy),],aes(x=ECPolicy, y= ..prop.., group = 1))+
geom_bar(stat = "Count") + xlab("Britain's long term policy")+ ylab("Proportion of respondents")

#create object called tabID which contains the univariate distribution of variable NatID3Cat
tabID<-table(bsa16$NatID3Cat)
tabID
addmargins(tabID)
#as a proportion
prop.table(tabID)
#as a percentage
prop.table(tabID)*100
ggplot(bsa16[!is.na(bsa16$NatID3Cat),], aes(x = NatID3Cat,y= ..prop.., group = 1)) + geom_bar(stat = "count")+
xlab("Do you think yourself as more English or British?")+ ylab("Proportion of respondents")

#create an object called tabAge3 which contains the univariate distribution of variable AgeCat3
tabAge3<-table(bsa16$RAgecat3)
tabAge3
addmargins(tabAge3)
#as a proportion
prop.table(tabAge3)
#as a percentage
prop.table(tabAge3)*100

#create an object called tabOcc which contains the univariate distribution of variable RClassGp
tabRClassGp<-table(bsa16$RClassGp)
tabRClassGp
addmargins(tabRClassGp)
#as a proportion
prop.table(tabRClassGp)
#as a percentage
prop.table(tabRClassGp)*100

#create an object called tab which contains the univariate distribution of variable CrPImm
tabImm<-table(bsa16$CrPImm)
tabImm
addmargins(tabImm)
#as a proportion
prop.table(tabImm)
#as a percentage
prop.table(tabImm)*100

#create an object called tabEcoP which contains the univariate distribution of variable EcoPos6
tabEcoP<-table(bsa16$EcoPos6)
tabEcoP
addmargins(tabEcoP)
#as a proportion
prop.table(tabEcoP)
#as a percentage
prop.table(tabEcoP)*100

#create an object called tabEdu which contains the univariate distribution of variable HedQual3
tabEdu<-table(bsa16$HEdQual3)
tabEdu
addmargins(tabEdu)
#as a proportion
prop.table(tabEdu)
#as a percentage
prop.table(tabEdu)*100

#create an object called tabPartID which contains the univariate distribution of variable PartID3
tabPartyID<-table(bsa16$PartyID3)
tabPartyID
addmargins(tabPartyID)
#as a proportion
prop.table(tabPartyID)
#as a percentage
prop.table(tabPartyID)*100

#create an object called tabRes which contains the univariate distribution of variable ResPres
tabRes<-table(bsa16$ResPres)
tabRes
addmargins(tabRes)
#as a proportion
prop.table(tabRes)
#as a percentage
prop.table(tabRes)*100

#Bivariate Analysis
#Analysis of dependant variable ECPolicy and main predictor NatID3Cat

#create a crosstab which represents the disribution of NatID3Cat and ECPolicy
IDEC<-table(bsa16$NatID3Cat, bsa16$ECPolicy)
addmargins(IDEC)
#as a percentage
prop.table(IDEC,1)*100

#stacked bar chart
ggplot(data = bsa16[!is.na(bsa16$NatID3Cat)& !is.na(bsa16$ECPolicy),],aes(x=NatID3Cat))+
geom_bar(stat = "Count", aes(fill=ECPolicy), position = "fill")+xlab("Do you think of yourself as English
or British")+ylab("Percentage of Respondents")

#run chi-squared test
chi_IDEC<-chisq.test(IDEC, correct = F)
chi_IDEC

#Analysis of dependent variable across the independent variables

#create a crosstab which represents the disribution of ECPolicy and RAgeCat3
AgeEC<-table(bsa16$ECPolicy, bsa16$RAgecat3)
addmargins(AgeEC)
#as a percentage
prop.table(AgeEC,2)*100
#run chi-squared test
chi_AgeEC<-chisq.test(AgeEC, correct = F)
chi_AgeEC

#create a crosstab which represents the disribution of ECPolicy and RClassGp
EClass<-table(bsa16$ECPolicy, bsa16$RClassGp)
addmargins(EClass)
#as a percentage
prop.table(EClass,2)*100
#run chi-squared test
chi_EClass<-chisq.test(EClass, correct = F)
chi_EClass

#create a crosstab which represents the disribution of ECPolicy and CrPImm
ImEC<-table(bsa16$ECPolicy, bsa16$CrPImm)
addmargins(ImEC)
#as a percentage
prop.table(ImEC,1)*100
#run chi-squared test
chi_ImEC<-chisq.test(ImEC, correct = F)
chi_ImEC

#create a crosstab which represents the disribution of ECPolicy and EcoPos6 
ECPos<-table(bsa16$ECPolicy, bsa16$EcoPos6)
addmargins(ECPos)
#as a percentage
prop.table(ECPos,2)*100 
#run chi-squared test
chi_ECPos<-chisq.test(ECPos, correct = F)
chi_ECPos

#create a crosstab which represents the disribution of ECPolicy and HedQual3
EduEC<-table(bsa16$ECPolicy, bsa16$HEdQual3)
addmargins(EduEC)
#as a percentage
prop.table(EduEC,2)*100 
#run chi-squared test
chi_EduEC<-chisq.test(EduEC, correct = F)
chi_EduEC

#create a crosstab which represents the disribution of ECPolicy and PartyID3
ECPID<-table(bsa16$ECPolicy, bsa16$PartyID3)
addmargins(ECPID)
#as a percentage
prop.table(ECPID,2)*100
#clustered bar chart
ggplot(data = bsa16[!is.na(bsa16$ECPolicy)& !is.na(bsa16$PartyID3),],aes(x=PartyID3))+
  geom_bar(stat = "Count", aes(fill=ECPolicy), position = "fill")+xlab("Party Identification")+ylab("Percentage of Respondents")
#run chi-squared test
chi_ECPID<-chisq.test(ECPID, correct = F)
chi_ECPID

#create a crosstab which represents the disribution of ECPolicy and ResPres
ECRes<-table(bsa16$ECPolicy, bsa16$ResPres)
addmargins(ECRes)
#as a percentage
prop.table(ECRes,2)*100
#run chi-squared test
chi_ECRes<-chisq.test(ECRes, correct = F)
chi_ECRes

#Analysis of the main predictor across the controlled variables

#create a crosstab which represents the disribution of NatID3Cat and RAgeCat3
AgeID<-table(bsa16$NatID3Cat, bsa16$RAgecat3)
addmargins(AgeID)
#as a percentage
prop.table(AgeID,2)*100
#run chi-squared test
chi_AgeID<-chisq.test(AgeID, correct = F)
chi_AgeID

#create a crosstab which represents the disribution of NatID3Cat and RClassGp
IDOC<-table(bsa16$NatID3Cat, bsa16$RClassGp)
addmargins(IDOC)
#as a percentage
prop.table(IDOC,2)*100
#run chi-squared test
chi_IDOC<-chisq.test(IDOC, correct = F)
chi_IDOC

#create a crosstab which represents the disribution of NatID3Cat and CrPImm
ImmID<-table(bsa16$NatID3Cat, bsa16$CrPImm)
addmargins(ImmID)
#as a percentage
prop.table(ImmID,1)*100
#run chi-squared test
chi_ImmID<-chisq.test(ImmID, correct = F)
chi_ImmID

#create a crosstab which represents the disribution of NatID3Cat and EcoPos6 
EcoP<-table(bsa16$NatID3Cat, bsa16$EcoPos6)
addmargins(EcoP)
#as a percentage
prop.table(EcoP,2)*100 
#run chi-squared test
chi_EcoP<-chisq.test(EcoP, correct = F)
chi_EcoP

#create a crosstab which represents the disribution of NatID3Cat and HedQual3
Edu<-table(bsa16$NatID3Cat, bsa16$HEdQual3)
addmargins(Edu)
#as a percentage
prop.table(Edu,2)*100 
#run chi-squared test
chi_Edu<-chisq.test(Edu, correct = F)
chi_Edu

#create a crosstab which represents the disribution of NatID3Cat and PartyID3
PID<-table(bsa16$NatID3Cat, bsa16$PartyID3)
addmargins(PID)
#as a percentage
prop.table(PID,2)*100
#clustered bar chart
ggplot(data = bsa16[!is.na(bsa16$NatID3Cat)& !is.na(bsa16$PartyID3),],aes(x=PartyID3))+
geom_bar(stat = "Count", aes(fill=NatID3Cat), position = "fill")+xlab("Party Identification")+ylab("Percentage of Respondents")
#run chi-squared test
chi_PID<-chisq.test(PID, correct = F)
chi_PID

#create a crosstab which represents the disribution of NatID3Cat and ResPres
Res<-table(bsa16$NatID3Cat, bsa16$ResPres)
addmargins(Res)
#as a percentage
prop.table(Res,2)*100
#run chi-squared test
chi_Res<-chisq.test(Res, correct = F)
chi_Res

#Modelling - Logistic Regression

#restrict the data to only include complete cases
bsa16<-bsa16[complete.cases(bsa16$NatID3Cat, bsa16$ECPolicy,bsa16$RAgecat3, bsa16$RClassGp, 
bsa16$CrPImm, bsa16$EcoPos6, bsa16$HEdQual3, bsa16$PartyID3, bsa16$ResPres),]

#model 1 - analysis of dependent variable and main predictor
model1 <-glm(ECPolicy~NatID3Cat, data = bsa16, family = "binomial")
summary(model1)
exp(cbind(OR=coef(model1), confint(model1)))


#model 2 - analysis of dependent variable, main predictor and all the conrolled variables
model2 <-glm(ECPolicy~NatID3Cat + RAgecat3 + RClassGp + CrPImm + EcoPos6 + HEdQual3 + 
PartyID3 + ResPres, data = bsa16, family = "binomial")
summary(model2)

#model 3 - analysis of dependent variable, main predictor and significant variables
model3 <-glm(ECPolicy~NatID3Cat+ CrPImm + HEdQual3 + PartyID3, 
             data = bsa16, family = "binomial")
summary(model3)
exp(cbind(OR=coef(model3), confint(model3)))

model1$aic
model2$aic
model3$aic




