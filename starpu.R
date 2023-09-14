library(tidyverse)
library(dplyr)
library(foreign)
library(MASS)
library(mgcv) #smooth through glm models
library(car)
library(gbm)
library(pROC)
library(caret)
library(stargazer)
antibiotic<-read.csv(file.choose(),header = T)#y indicators
antibiotic_a<-as_tibble(antibiotic)
antibiotic_q3<-filter(antibiotic_a, Time.period== "2021 Q3",Indicator.ID==91900,Area.Type!="England", Area.Code!="U99999")
antibiotic_q4<-filter(antibiotic_a, Time.period== "2021 Q4",Indicator.ID==91900,Area.Type!="England", Area.Code!="U99999")
antibiotic_q1<-filter(antibiotic_a, Time.period== "2022 Q1",Indicator.ID==91900,Area.Type!="England", Area.Code!="U99999")
antibiotic_q2<-filter(antibiotic_a, Time.period== "2022 Q2",Indicator.ID==91900,Area.Type!="England", Area.Code!="U99999")
antibiotic_summary<-tibble(antibiotic_q3$Value,antibiotic_q4$Value,antibiotic_q1$Value,antibiotic_q2$Value)

antibiotic_value<-apply(antibiotic_summary,1,mean)
antibiotic_numerator<-tibble(antibiotic_q3$Count,antibiotic_q4$Count,antibiotic_q1$Count,antibiotic_q2$Count)
antibiotic_value_count<-apply(antibiotic_numerator, 1, mean)
antibiotic_denominator<-tibble(antibiotic_q3$Denominator,antibiotic_q4$Denominator,antibiotic_q1$Denominator,antibiotic_q2$Denominator)
antibiotic_value_denominator<-apply(antibiotic_denominator, 1, mean)
antibiotic_summary_final<-tibble(Area.Code=antibiotic_q3$Area.Code, Area.Name=antibiotic_q3$Area.Name,Time.period=antibiotic_q3$Time.period,antibiotic_value,Count=antibiotic_value_count,Denominator=antibiotic_value_denominator)
##

#residuals from the model, from 2021 to 2022
#over-dispersion
#diabate
diabate<-read.csv(file.choose(),header = T)#diabetes indicators
diabate_a<-as_tibble(diabate)
diabate_b<-filter(diabate_a,Time.period=="2021/22",Indicator.ID==241,Area.Type!="England",Area.Code!="U99999")
diabate_final<-diabate_b[,c(5,6,12,13)]
position_na_diabate<-which(is.na(diabate_final$Value)) ## 0

##mental health
depression<-read.csv(file.choose(),header = T)#depression indicators
depression_a<-as_tibble(depression)
depression_b<-filter(depression_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
depression_final<-depression_b[,c(5,6,12,13)]
position_na_depression<-which(is.na(depression_final$Value))

#cancer
cancer<-read.csv(file.choose(),header = T)#depression indicators
cancer_a<-as_tibble(cancer)
cancer_b<-filter(cancer_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
cancer_final<-cancer_b[,c(5,6,12,13)]


##Two-week wait referrals
Two_week_wait_referrals<-read.csv(file.choose(),header = T)
Two_week_wait_referrals_a<-as_tibble(Two_week_wait_referrals)
Two_week_wait_referrals_b<-filter(Two_week_wait_referrals_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
Two_week_wait_referrals_final<-Two_week_wait_referrals_b[,c(5,6,12,13)]


#Respiratory Disease Chronic Obstructive Pulmonary Disease
Respiratory_Disease<-read.csv(file.choose(),header = T)#depression indicators
Respiratory_Disease_a<-as_tibble(Respiratory_Disease)
Respiratory_Disease_b<-filter(Respiratory_Disease_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
Respiratory_Disease_final<-Respiratory_Disease_b[,c(5,6,12,13)]

## stroke CVD
stroke<-read.csv(file.choose(),header = T)#depression indicators
stroke_a<-as_tibble(stroke)
stroke_b<-filter(stroke_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
stroke_final<-stroke_b[,c(5,6,12,13)]

#Coronary Heart Disease CVD
coronary_heart_disease<-read.csv(file.choose(),header = T)#depression indicators
coronary_heart_disease_a<-as_tibble(coronary_heart_disease)
coronary_heart_disease_b<-filter(coronary_heart_disease_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
coronary_heart_disease_final<-coronary_heart_disease_b[,c(5,6,12,13)]

## heart disease and Atrial Fibrillation CVD
heart_disease<-read.csv(file.choose(),header = T)#depression indicators
heart_disease_a<-as_tibble(heart_disease)
heart_disease_b<-filter(heart_disease_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
heart_disease_final<-heart_disease_b[,c(5,6,12,13)]

## risk factors for cvd
risk_factors<-read.csv(file.choose(),header = T)#depression indicators
risk_factors_a<-as_tibble(risk_factors)
risk_factors_b<-filter(risk_factors_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
risk_factors_final<-risk_factors_b[,c(5,6,12,13)]

#Maternal and Child Health 0-18 years
maternal<-read.csv(file.choose(),header = T)
maternal_a<-as_tibble(maternal)
maternal_b<-filter(maternal_a,Time.period== 2021,Area.Type!="England",Area.Code!="U99999")
maternal_final<-maternal_b[,c(5,6,12,13)]
##Musculoskeletal Conditions, Osteoporosis
muscle<-read.csv(file.choose(),header = T)
muscle_a<-as_tibble(muscle)
muscle_b<-filter(muscle_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
muscle_final<-muscle_b[,c(5,6,12,13)]

#CKD
ckd<-read.csv(file.choose(),header = T)
ckd_a<-as_tibble(ckd)
ckd_b<-filter(ckd_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
ckd_final<-ckd_b[,c(5,6,12,13)]

#Epilepsy 7720
Epilepsy<-read.csv(file.choose(),header = T)
Epilepsy_a<-as_tibble(Epilepsy)
Epilepsy_b<-filter(Epilepsy_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
Epilepsy_final<-Epilepsy_b[,c(5,6,12,13)]

#asthma 7720
asthma<-read.csv(file.choose(),header = T)
asthma_a<-as_tibble(asthma)
asthma_b<-filter(asthma_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
asthma_final<-asthma_b[,c(5,6,12,13)]
##Rheumatoid Arthritis 7720
Rheumatoid<-read.csv(file.choose(),header = T)
Rheumatoid_a<-as_tibble(Rheumatoid)
Rheumatoid_b<-filter(Rheumatoid_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
Rheumatoid_final<-Rheumatoid_b[,c(5,6,12,13)]
##PAD 7720
PAD<-read.csv(file.choose(),header = T)
PAD_a<-as_tibble(PAD)
PAD_b<-filter(PAD_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
PAD_final<-PAD_b[,c(5,6,12,13)]
##dementia #7720
Dementia<-read.csv(file.choose(),header = T)
Dementia_a<-as_tibble(Dementia)
Dementia_b<-filter(Dementia_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
Dementia_final<-Dementia_b[,c(5,6,12,13)]
##obesity 18+ years 7719
obesity<-read.csv(file.choose(),header = T)
obesity_a<-as_tibble(obesity)
obesity_b<-filter(obesity_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
obesity_final<-obesity_b[,c(5,6,12,13)]
##smoking 15+ years 7720
smoking<-read.csv(file.choose(),header = T)
smoking_a<-as_tibble(smoking)
smoking_b<-filter(smoking_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
smoking_final<-smoking_b[,c(5,6,12,13)]

##Atrial fibrillation 7721
Atrial<-read.csv(file.choose(),header = T)
Atrial_a<-as_tibble(Atrial)
Atrial_b<-filter(Atrial_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
Atrial_final<-Atrial_b[,c(5,6,12,13)]

#elderly
elderly<-read.csv(file.choose(),header = T)
elderly_a<-as_tibble(elderly)
elderly_b<-filter(elderly_a,Time.period=="2021",Area.Type!="England",Area.Code!="U99999")
elderly_final<-elderly_b[,c(5,6,12,13)]

#copd
copd<-read.csv(file.choose(),header = T)
copd_a<-as_tibble(copd)
copd_b<-filter(copd_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
copd_final<-copd_b[,c(5,6,12,13)]

#skin cancer
skin_cancer<-read.csv(file.choose(),header = T)
skin_cancer_a<-as_tibble(skin_cancer)
skin_cancer_b<-filter(skin_cancer_a,Time.period=="2021/22",Area.Type!="England",Area.Code!="U99999")
skin_cancer_final<-skin_cancer_b[,c(5,6,12,13)]

## test differences on gps round 1 remove two_week
## with conclusion different on y, maternal and two_week
test1<-antibiotic_summary_final$Area.Code
test2<-depression_final$Area.Code
test3<-cancer_final$Area.Code
test4<-diabate_final$Area.Code
test5<-Two_week_wait_referrals_final$Area.Code
test6<-chd_final$Area.Code
test7<-Respiratory_Disease_final$Area.Code
test8<-stroke_final$Area.Code
test9<-coronary_heart_disease_final$Area.Code
test10<-heart_disease_final$Area.Code
test11<-risk_factors_final$Area.Code
test12<-maternal_final$Area.Code
test13<-muscle_final$Area.Code
test14<-ckd_final$Area.Code
test15<-Epilepsy_final$Area.Code
test16<-copd_final$Area.Code
test17<-skin_cancer_final$Area.Code

c1<-setdiff(test4,test5)
len1<-length(c1)
initial1<-rep(0,len1)
for (i in 1:len1) {
  initial1[i]<-which(test4==c1[i])
}
test_diabate_1<-diabate_final[-initial1,]
test_cancer_1<-cancer_final[-initial1,]
test_depression_1<-depression_final[-initial1,]
test_chd_1<-chd_final[-initial1,]
test_respiratory_1<-Respiratory_Disease_final[-initial1,]
test_stroke_1<-stroke_final[-initial1,]
test_coronary_1<-coronary_heart_disease_final[-initial1,]
test_heart_1<-heart_disease_final[-initial1,]
test_risk_1<-risk_factors_final[-initial1,]
test_muscle_1<-muscle_final[-initial1,]
test_ckd_1<-ckd_final[-initial1,]
test_epilepsy_1<-Epilepsy_final[-initial1,]
test_asthma_1<-asthma_final[-initial1,]
test_Rheumatoid_1<-Rheumatoid_final[-initial1,]
test_PAD_1<-PAD_final[-initial1,]
test_dementia_1<-Dementia_final[-initial1,]
test_copd_1<-copd_final[-initial1,]

c2<-setdiff(test5,test4) #both two directions
len2<-length(c2)
initial2<-rep(0,len2)
for (i in 1:len2) {
  initial2[i]<-which(test5==c2[i])
}
test_two_week_1<-Two_week_wait_referrals_final[-initial2,]
test_skin_cancer_1<-skin_cancer_final[-initial2,]
#test differences on maternal
test2a<-test_risk_1$Area.Code
test2b<-maternal_final$Area.Code
c1a<-setdiff(test2a,test2b)
len1a<-length(c1a)
initial1a<-rep(0,len1a)
for (i in 1:len1a) {
  initial1a[i]<-which(test2a==c1a[i])
}
test_diabate_2<-test_diabate_1[-initial1a,]
test_cancer_2<-test_cancer_1[-initial1a,]
test_depression_2<-test_depression_1[-initial1a,]
test_chd_2<-test_chd_1[-initial1a,]
test_respiratory_2<-test_respiratory_1[-initial1a,]
test_stroke_2<-test_stroke_1[-initial1a,]
test_coronary_2<-test_coronary_1[-initial1a,]
test_heart_2<-test_heart_1[-initial1a,]
test_risk_2<-test_risk_1[-initial1a,]
test_muscle_2<-test_muscle_1[-initial1a,]
test_two_week_2<-test_two_week_1[-initial1a,]
test_ckd_2<-test_ckd_1[-initial1a,]
test_epilepsy_2<-test_epilepsy_1[-initial1a,]
test_asthma_2<-test_asthma_1[-initial1a,]
test_Rheumatoid_2<-test_Rheumatoid_1[-initial1a,]
test_PAD_2<-test_PAD_1[-initial1a,]
test_dementia_2<-test_dementia_1[-initial1a,]
test_copd_2<-test_copd_1[-initial1a,]
test_skin_cancer_2<-test_skin_cancer_1[-initial1a,]

c2a<-setdiff(test2b,test2a) #both two directions
len2a<-length(c2a)
initial2a<-rep(0,len2a)
for (i in 1:len2a) {
  initial2a[i]<-which(test2b==c2a[i])
}
test_maternal_2<-maternal_final[-initial2a,]
test_elderly_2<-elderly_final[-initial2a,]

## difference on y
test3a<-test_cancer_2$Area.Code
test3b<-antibiotic_summary_final$Area.Code
c1b<-setdiff(test3a,test3b)
len1b<-length(c1b)
initial1b<-rep(0,len1b)
for (i in 1:len1b) {
  initial1b[i]<-which(test3a==c1b[i])
}
test_diabate_3<-test_diabate_2[-initial1b,]
test_cancer_3<-test_cancer_2[-initial1b,]
test_depression_3<-test_depression_2[-initial1b,]
test_chd_3<-test_chd_2[-initial1b,]
test_respiratory_3<-test_respiratory_2[-initial1b,]
test_stroke_3<-test_stroke_2[-initial1b,]
test_coronary_3<-test_coronary_2[-initial1b,]
test_heart_3<-test_heart_2[-initial1b,]
test_risk_3<-test_risk_2[-initial1b,]
test_muscle_3<-test_muscle_2[-initial1b,]
test_two_week_3<-test_two_week_2[-initial1b,]
test_maternal_3<-test_maternal_2[-initial1b,]
test_ckd_3<-test_ckd_2[-initial1b,]
test_epilepsy_3<-test_epilepsy_2[-initial1b,]
test_asthma_3<-test_asthma_2[-initial1b,]
test_Rheumatoid_3<-test_Rheumatoid_2[-initial1b,]
test_PAD_3<-test_PAD_2[-initial1b,]
test_dementia_3<-test_dementia_2[-initial1b,]
test_elderly_3<-test_elderly_2[-initial1b,]
test_copd_3<-test_copd_2[-initial1b,]
test_skin_cancer_3<-test_skin_cancer_2[-initial1b,]

c2b<-setdiff(test3b,test3a)
len2b<-length(c2b)
initial2b<-rep(0,len2b)
for (i in 1:len2b) {
  initial2b[i]<-which(test3b==c2b[i])
}
test_antibiotic_3<-antibiotic_summary_final[-initial2b,]

#difference on smoking
test4b<-smoking_final$Area.Code
test4a<-test_muscle_3$Area.Code
c1c<-setdiff(test4a,test4b) #character 0


c2c<-setdiff(test4b,test4a)
len2c<-length(c2c)
initial2c<-rep(0,len2c)
for (i in 1:len2c) {
  initial2c[i]<-which(test4b==c2c[i])
}
test_smoking_3<-smoking_final[-initial2c,]

##difference on atrial flibration
test5a<-test_cancer_3$Area.Code
test5b<-Atrial_final$Area.Code
c1d<-setdiff(test5a,test5b)
len1d<-length(c1d) ##len1d=0


c2d<-setdiff(test5b,test5a)
len2d<-length(c2d)
initial2d<-rep(0,len2d)
for (i in 1:len2d) {
  initial2d[i]<-which(test5b==c2d[i])
}
test_atrial<-Atrial_final[-initial2d,]

## total data set
Time.period<-test_depression_3$Time.period
Area.name<-test_depression_3$Area.Name
antibiotic.value<-test_antibiotic_3$antibiotic_value*100
depression.value<-test_depression_3$Value
cancer.value<-test_cancer_3$Value
diabete.value<-test_diabate_3$Value
referral.value<-test_two_week_3$Value/100 #per 10000 patients
skin_cancer.value<-test_skin_cancer_3$Value/100 #per 10000 patients
Respiratory_Disease.value<-test_respiratory_3$Value
stroke.value<-test_stroke_3$Value
coronary.value<-test_coronary_3$Value
heart_failure.value<-test_heart_3$Value
hypertension.value<-test_risk_3$Value
child.value<-test_maternal_3$Value
muscular.value<-test_muscle_3$Value
Area.code<-test_muscle_3$Area.Code
antibiotic.count<-test_antibiotic_3$Count
antibiotic.denominator<-test_antibiotic_3$Denominator
ckd.value<-test_ckd_3$Value
epilepsy.value<-test_epilepsy_3$Value
asthma.value<-test_asthma_3$Value
Rheumatoid.value<-test_Rheumatoid_3$Value
PAD.value<-test_PAD_3$Value
Dementia.value<-test_dementia_3$Value
smoking.value<-test_smoking_3$Value
elderly.value<-test_elderly_3$Value
atrial.value<-test_atrial$Value
antibiotic_count_round<-round(antibiotic.count)


total_data<-tibble(Time.period,
                   Area.code,
                   Area.name,
                   antibiotic.value,
                   antibiotic.count,
                   antibiotic.denominator,
                   depression.value,
                   cancer.value,
                   diabete.value,
                   referral.value,
                   Respiratory_Disease.value,
                   stroke.value,
                   coronary.value,
                   heart_failure.value,
                   hypertension.value,
                   child.value,
                   elderly.value,
                   muscular.value,
                   ckd.value,
                   epilepsy.value,
                   asthma.value,
                   Rheumatoid.value,
                   PAD.value,
                   Dementia.value,
                   smoking.value,
                   atrial.value,
)

total_data_1<-total_data[,4:26]
total_data_2<-tibble(
  rate=antibiotic.value,
  depression=depression.value,
  cancer=cancer.value,
  diabete=diabete.value,
  referral=referral.value,
  COPD=Respiratory_Disease.value,
  stroke=stroke.value,
  coronary=coronary.value,
  heart_failure=heart_failure.value,
  hypertension=hypertension.value,
  child=child.value,
  elderly=elderly.value,
  muscular=muscular.value,
  ckd=ckd.value,
  epilepsy=epilepsy.value,
  asthma=asthma.value,
  rheumatoid=Rheumatoid.value,
  pad=PAD.value,
  dementia=Dementia.value,
  smoking=smoking.value,
  atrial=atrial.value
)
total_data_3<-tibble(
  antibiotic_count_round,
  antibiotic.denominator,
  depression.value,
  cancer.value,
  diabete.value,
  referral.value,
  Respiratory_Disease.value,
  stroke.value,
  coronary.value,
  heart_failure.value,
  hypertension.value,
  child.value,
  elderly.value,
  muscular.value,
  ckd.value,
  epilepsy.value,
  asthma.value,
  Rheumatoid.value,
  PAD.value,
  Dementia.value,
  smoking.value,
  atrial.value
)

total_data_4<-tibble(
  count=antibiotic_count_round,
  population=antibiotic.denominator,
  depression=depression.value,
  cancer=cancer.value,
  diabete=diabete.value,
  referral=referral.value,
  COPD=Respiratory_Disease.value,
  coronary=coronary.value,
  heart_failure=heart_failure.value,
  hypertension=hypertension.value,
  child=child.value,
  elderly=elderly.value,
  muscular=muscular.value,
  ckd=ckd.value,
  epilepsy=epilepsy.value,
  asthma=asthma.value,
  rheumatoid=Rheumatoid.value,
  pad=PAD.value,
  dementia=Dementia.value,
  smoking=smoking.value
)

total_data_5<-tibble(
  rate=antibiotic.value,
  depression=depression.value,
  cancer=cancer.value,
  diabete=diabete.value,
  referral=referral.value,
  COPD=Respiratory_Disease.value,
  coronary=coronary.value,
  heart_failure=heart_failure.value,
  hypertension=hypertension.value,
  child=child.value,
  elderly=elderly.value,
  muscular=muscular.value,
  ckd=ckd.value,
  epilepsy=epilepsy.value,
  asthma=asthma.value,
  rheumatoid=Rheumatoid.value,
  pad=PAD.value,
  dementia=Dementia.value,
  smoking=smoking.value
)
#boxplot
boxplot(total_data_5$rate)

#removing extreme data
location50<-which(antibiotic.value>50)
total_data<-total_data[-location50,]
total_data_1<-total_data_1[-location50,]
total_data_2<-total_data_2[-location50,]
total_data_3<-total_data_3[-location50,]
total_data_4<-total_data_4[-location50,]
total_data_5<-total_data_5[-location50,]

#80% quantile
quantile80<-quantile(total_data_2$rate,0.8)
position80<-which(total_data_2$rate>=quantile80)

#exploratory analysis plots between y and x
par(mfrow=c(2,2))
plot(total_data_1$depression.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="depression")
plot(total_data_1$cancer.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="cancer")
plot(total_data_1$diabate.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="diabete")
plot(total_data_1$Respiratory_Disease.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="Respiratory")
##
par(mfrow=c(2,2))
plot(total_data_1$stroke.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="stroke")
plot(total_data_1$coronary.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="coronary")
plot(total_data_1$heart_disease.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="heart failure")
plot(total_data_1$risk_factors.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="Hypertension")
##
par(mfrow=c(2,2))
plot(total_data_1$child.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="child")
plot(total_data_1$muscle.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="Muscular")
plot(total_data_1$ckd.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="CKD")
plot(total_data_1$epilepsy.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="Epilepsy")
###
par(mfrow=c(2,2))
plot(total_data_1$asthma.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="asthma")
plot(total_data_1$Rheumatoid.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="Rheumatoid")
plot(total_data_1$PAD.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="PAD")
plot(total_data_1$Dementia.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="Dementia")
###
plot(total_data_1$Two_week_wait_referrals.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="referral")
#pick out 4 largest
par(mfrow=c(2,2))
plot(total_data_1$Respiratory_Disease.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="COPD")
plot(total_data_1$asthma.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="asthma")
plot(total_data_1$epilepsy.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="Epilepsy")
plot(total_data_1$coronary.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="coronary")
#pick out 4 smallest 
par(mfrow=c(2,2))
plot(total_data_1$muscular.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="Muscular")
plot(total_data_1$child.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="child")
plot(total_data_1$Dementia.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="Dementia")
plot(total_data_1$smoking.value,total_data_1$antibiotic.value,ylab = "prescription rate", xlab="Smoking")



#poisson test
poisson_model<-glm(antibiotic_count_round~.-antibiotic.denominator,offset = log(antibiotic.denominator),data = total_data_3,family = poisson)
summary(poisson_model)
c<-deviance(poisson_model)/df.residual(poisson_model)
c

#aliased coefficients 
cor(total_data_1[4:23])
##test
lm.nb<-lm(antibiotic.value~.-antibiotic.count-antibiotic.denominator,data = total_data_1)
vif(lm.nb)
## negative binomial regression
model_full<-glm.nb(count~.-population+offset(log(population)),data=total_data_4)
summary(model_full)
model_null<-glm.nb(antibiotic_count_round~offset(log(antibiotic.denominator)),data=total_data_3)
summary(model_null)
##cross validation
library(plyr)
CVgroup <- function(k,datasize,seed){
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k,ceiling(datasize/k))[1:datasize]   
  temp <- sample(n,datasize)   
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x,function(x) dataseq[temp==x])  
  return(cvlist)
}

cvlist <- CVgroup(k = 10,datasize = nrow(total_data_3),seed = 1206)
aic_nb_10000<-rep(0,10)
mse_train_nb_10000<-rep(0,10)
mse_test_nb_10000<-rep(0,10)
for (i in 1:10) {
  fold_test <- total_data_4[cvlist[[i]],] 
  fold_train <- total_data_4[-cvlist[[i]],] 
  rate_nb_10000_test<-total_data_5[cvlist[[i]],]$rate
  rate_nb_10000_train<-total_data_5[-cvlist[[i]],]$rate
  model_full_nb_10000<-glm.nb(count~.-population+offset(log(population)),data=fold_train)
  step.model_nb_10000<-stepAIC(model_full_nb_10000, direction = "both", 
                               trace = FALSE,k=4)
  nb.fitted_10000<-(step.model_nb_10000$fitted.values/fold_train$population)*100
  prediction.nb_10000<-predict.glm(step.model_nb_10000,newdata = fold_test,type = "response")
  prediction.value_nb_10000<-(prediction.nb_10000/fold_test$population)*100
  mse_test_nb_10000[i]<-sum((prediction.value_nb_10000-rate_nb_10000_test)^2)/nrow(fold_test)
  mse_train_nb_10000[i]<-sum((nb.fitted_10000-rate_nb_10000_train)^2)/nrow(fold_train)
  aic_nb_10000[i]<-step.model_nb_10000$aic
}

#step model
step.model <- stepAIC(model_full, direction = "both", 
                      trace = FALSE,k=4)
summary(step.model)
xtable(step.model)
par(mfrow=c(2,2))
plot(step.model)
nb.fitted<-(step.model$fitted.values/total_data_4$population)*100
sum((nb.fitted-total_data_5$rate)^2)/nrow(total_data_5) #training mse
plot(model_null$residuals,step.model$residuals,xlab = "Residuals of null model", ylab = "Residuals of stepwise model")

100*with(summary(step.model),1-step.model$deviance/step.model$null.deviance) #deviance explained
fittedstep80<-(step.model$fitted.values/total_data_4$population)*100
quantile80new <- quantile(fittedstep80, 0.8)
position80new <- which(fittedstep80 >= quantile80new)
length(which(fittedstep80 >= quantile80new))
sum(length(which(fittedstep80>quantile80)))

length(intersect(position80, position80new))
#xlim ylim
plot(model_null$residuals,step.model$residuals,ylab = "Residuals of stepwise model (%)", xlab = "Residuals of null model(%)")

#mgam experiments
mgam_full<-gam(count~s(depression)+s(cancer)+s(diabete)+s(referral)+s(COPD)+s(coronary)+s(heart_failure)
               +s(hypertension)+s(child)+s(muscular)+s(ckd)
               +s(epilepsy)+s(elderly)+s(asthma)+s(rheumatoid)+s(pad)+s(dementia)+s(smoking),offset=log(population),family=nb(),data=total_data_4,model=T)
summary(mgam_full)
texreg(mgam_full,include.ci=F)
#after removal of insignificant variables
mgam1<-gam(count~s(depression)+s(cancer)+s(diabete)+s(referral)+s(COPD)+s(coronary)+s(heart_failure)
           +s(hypertension)+s(child)+s(muscular)
           +s(epilepsy)+s(elderly)+s(asthma)+s(rheumatoid)+s(pad)+s(dementia)+s(smoking),offset=log(population),family=nb(),data=total_data_4,model=T)
fittedstep80_gam_1<-(mgam1$fitted.values/total_data_4$population)*100
quantile80new_gam_1 <- quantile(fittedstep80_gam_1, 0.8)
position80new_gam_1 <- which(fittedstep80_gam_1 >= quantile80new_gam_1)
length(intersect(position80, position80new_gam_1))
#the number of fitting function
#final selection model
mgam_candidate_1<-gam(count~s(referral)+s(COPD)+s(coronary)+s(child)
                      +s(epilepsy)+s(asthma),offset=log(population),family=nb(),data=total_data_4,model=T)
summary(mgam_candidate_1)
gam.check(mgam_candidate_1)
plot(model_null$residuals,mgam_candidate_1$residuals,xlab = "Residuals of null model", ylab = "Residuals of GAM")

model_null_fit<-(model_null$fitted.values/total_data_4$population)*100
fittedstep80_gam<-(mgam_candidate_1$fitted.values/total_data_4$population)*100
plot(fittedstep80_gam,model_null_fit,xlab = "Fitted values", ylab = "Response",main = "Response vs. Fitted Values")

quantile80new_gam <- quantile(fittedstep80_gam, 0.8)
position80new_gam <- which(fittedstep80_gam >= quantile80new_gam)
length(intersect(position80, position80new_gam))

#cross validation
mse_train_gam<-rep(0,10)
mse_test_gam<-rep(0,10)
for (i in 1:10) {
  fold_test_gam <- total_data_4[cvlist[[i]],] 
  fold_train_gam <- total_data_4[-cvlist[[i]],]
  rate_gam_test<-total_data_5[cvlist[[i]],]$rate
  rate_gam_train<-total_data_5[-cvlist[[i]],]$rate
  gam_antibiotic_cv_offset<-gam(count~s(referral)+s(COPD)+s(coronary)+s(child)
                                +s(epilepsy)+s(asthma),offset=log(population),family=nb(),data=fold_train_gam,model=T)
  gam_fitted<-predict.gam(gam_antibiotic_cv_offset,newdata = fold_train_gam,type = "response")*100
  gam_test<-predict.gam(gam_antibiotic_cv_offset,newdata = fold_test_gam,type = "response")*100
  mse_test_gam[i]<-sum((gam_test-rate_gam_test)^2)/nrow(fold_test_gam)
  mse_train_gam[i]<-sum((gam_fitted-rate_gam_train)^2)/nrow(fold_train_gam)
}


plot(mgam_candidte_1, select = 1, pch = 20, se = TRUE, rug = TRUE, residuals = TRUE)
plot(mgam_candidte_1, select = 2, pch = 20, se = TRUE, rug = TRUE, residuals = TRUE)
plot(mgam_candidte_1, select = 3, pch = 20, se = TRUE, rug = TRUE, residuals = TRUE)
plot(mgam_candidte_1, select = 4, pch = 20, se = TRUE, rug = TRUE, residuals = TRUE)
plot(mgam_candidte_1, select = 5, pch = 20, se = TRUE, rug = TRUE, residuals = TRUE)
plot(mgam_candidte_1, select = 6, pch = 20, se = TRUE, rug = TRUE, residuals = TRUE)


#exploratory analysis on the response
plot(total_data_1$antibiotic.count)
qqnorm(total_data_1$antibiotic.count)
qqline(total_data_1$antibiotic.count)




# general additive model
#offset "number of star-pu"s denominator
#log denominator as the offset
plot(model_null$residuals,mgam_test_1$residuals)
##gbm

gbm_antibiotic_offset<-gbm(count~.-population+offset(log(population)),distribution = "poisson",data = total_data_4,n.trees = 1000,interaction.depth = 2,cv.folds = 10)
best.iter_offset<-gbm.perf(gbm_antibiotic_offset,method = "cv")
summary(gbm_antibiotic_offset,n.trees = best.iter_offset)
##intersection
fitted_gbm_step80<-predict.gbm(gbm_antibiotic_offset,newdata=total_data_4,n.trees = best.iter_offset,type = "response")*100
quantile80gbm <- quantile(fitted_gbm_step80, 0.8)
position80gbm <- which(fitted_gbm_step80 >= quantile80gbm)
length(intersect(position80, position80gbm))


gbm_fitted_offset<-predict.gbm(gbm_antibiotic_offset,newdata=total_data_4,n.trees = best.iter_offset,type = "response")*100


plot(x=total_data_5$rate,y=gbm_fitted_offset,xlab="Observed prescriptions per STAR-PU", ylab ="Expected prescriptions per STAR-PU",main = "Comparision of fitted values")
#partial dependence
plot(gbm_antibiotic_offset,i.var = 5,lwd=2,ylab="log(prescription/STAR-PU)",xlab="COPD (%)",n.trees = best.iter_offset)
plot(gbm_antibiotic_offset,i.var = 6,lwd=2,ylab="log(prescription/STAR-PU)",xlab="Coronary (%)",n.trees = best.iter_offset)
plot(gbm_antibiotic_offset,i.var = 14,lwd=2,ylab="log(prescription/STAR-PU)",xlab="Asthma (%)",n.trees = best.iter_offset)
### 3
plot(gbm_antibiotic_offset,i.var = 3,lwd=2,ylab="log(prescription/STAR-PU)",xlab="Diabete (%)",n.trees = best.iter_offset)
plot(gbm_antibiotic_offset,i.var = 13,lwd=2,ylab="log(prescription/STAR-PU)",xlab="Epilespsy (%)",n.trees = best.iter_offset)
plot(gbm_antibiotic_offset,i.var = 9,lwd=2,ylab="log(prescription/STAR-PU)",xlab="Child (%)",n.trees = best.iter_offset)



plot(gbm_antibiotic,i.var = 14,lwd=2,ylab="prescription/population",xlab=" Asthma (%)",n.trees = best.iter)

plot(gbm_antibiotic,i.var = 14,lwd=2,ylab="prescription/population",xlab=" Epilepsy (%)",n.trees = best.iter)
plot(gbm_antibiotic,i.var = 20,lwd=2,ylab="prescription/population",xlab=" Atrial fibrillation (%)",n.trees = best.iter)

#mse prediction
#cross validation
mse_train_gbm<-rep(0,10)
mse_test_gbm<-rep(0,10)
for (i in 1:10) {
  fold_test_gbm <- total_data_4[cvlist[[i]],] 
  fold_train_gbm <- total_data_4[-cvlist[[i]],]
  rate_gbm_test<-total_data_5[cvlist[[i]],]$rate
  rate_gbm_train<-total_data_5[-cvlist[[i]],]$rate
  gbm_antibiotic_cv_offset<-gbm(count~.-population+offset(log(population)),distribution = "poisson",data = fold_train_gbm,n.trees = 1000,interaction.depth = 2,cv.folds = 10)
  best.iter_cv<-gbm.perf(gbm_antibiotic_cv_offset,method = "cv")
  gbm_fitted<-predict.gbm(gbm_antibiotic_cv_offset,newdata=fold_train_gbm,type = "response",n.trees = best.iter_cv)*100
  gbm_test<-predict.gbm(gbm_antibiotic_cv_offset,newdata=fold_test_gbm,type = "response",n.trees = best.iter_cv)*100
  mse_test_gbm[i]<-sum((gbm_test-rate_gbm_test)^2)/nrow(fold_test_gbm)
  mse_train_gbm[i]<-sum((gbm_fitted-rate_gbm_train)^2)/nrow(fold_train_gbm)
}

