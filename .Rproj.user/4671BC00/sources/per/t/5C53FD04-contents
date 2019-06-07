
library(tidyverse)
Group =c(rep(c("DM","DM+SCH","DM+HYPOTHYROID"),each=25))

Group= factor(Group,levels = c("DM","DM+SCH","DM+HYPOTHYROID"))
set.seed(123)
library(truncnorm) # simulate normal with limits

TSH= c(
  rtruncnorm(25,0.4,4,3,0.8),
  rtruncnorm(25,5,20,8.3,0.9),
  rtruncnorm(25,5,50,20,3.7)
)

#T3 60-200 ng.dl
T3= c(
  rtruncnorm(25,80,200,120,11),
  rtruncnorm(25,80,200,110,10),
  rtruncnorm(25,0,60,40,10)
)

#4.5-122 mcg/l
T4= c(
  rtruncnorm(25,4.5,12,8.2,1.1),
  rtruncnorm(25,4.5,12,8,1),
  rtruncnorm(25,0,4.5,2.5,0.8)
)

#

FBS= c(
  rtruncnorm(25,126,300,170,20),
  rtruncnorm(25,126,300,172,21),
  rtruncnorm(25,126,300,174,20)
)

PPBS= c(
  rtruncnorm(25,140,400,220,30),
  rtruncnorm(25,140,400,222,31),
  rtruncnorm(25,126,400,222,31)
)

Hb= c(
  rtruncnorm(25,8,14,12,1.5),
  rtruncnorm(25,8,15,11,0.8),
  rtruncnorm(25,8,15,10.6,0.6)
)





HbA1c = simcorone(FBS,ymean = 8.5,ysd=0.7,correlation = 0.9)

BMI= c(
  rtruncnorm(25,22,35,26,2),
  rtruncnorm(25,22,35,26.2,2.5),
  rtruncnorm(25,22,35,26.3,2.5)
)

Weight = simcorone(TSH,ymean = 64,ysd=5,correlation = 0.9)

Urea= c(
  rtruncnorm(25,20,50,30,5),
  rtruncnorm(25,20,50,28,6),
  rtruncnorm(25,20,50,30,5.3)
)

Creatinine = simcorone(Urea,ymean = 1.3,ysd=0.2,correlation = 0.7)

ALT= 22+0.3*TSH+0.06*FBS+0.01*FBS*TSH+rnorm(75,0,3.5)

AST= 15+0.2*TSH+0.02*FBS+rnorm(75,0,3)

summary(aov(ALT~Group))

cor(TSH,ALT)
cor(FBS,ALT)

summary(lm(Weight~Group,data=data.frame(Weight,TSH,FBS,Group)))

LDL= 130+3*TSH+0.1*FBS+0.01*FBS*TSH+rnorm(75,0,3)

library(tidyverse)
library(skimr)

data.frame(LDL,Group) %>% group_by(Group) %>% skim(LDL)

options(scipen=999)

summary(lm(LDL~TSH+FBS+TSH*FBS,data=data.frame(Weight,LDL,TSH,FBS,Group)))


TG= 150+0.5*TSH+0.3*FBS+0.0075*FBS*TSH+rnorm(75,0,10)

library(tidyverse)
library(skimr)

data.frame(TG,Group) %>% group_by(Group) %>% skim(TG)

summary(lm(TG~TSH+FBS+TSH*FBS,data=data.frame(TG,Weight,LDL,TSH,FBS,Group)))

##VLDL (2-30)
VLDL= 20+0.8*TSH+0.07*FBS+0.00075*FBS*TSH+rnorm(75,0,5)

library(tidyverse)
library(skimr)

data.frame(VLDL,Group) %>% group_by(Group) %>% skim(VLDL)

summary(lm(VLDL~TSH+FBS+TSH*FBS,data=data.frame(VLDL,TG,Weight,LDL,TSH,FBS,Group)))


##HDL

HDL = 45-0.8*TSH-0.03*FBS+0.0007*FBS*TSH+rnorm(75,0,5)

data.frame(HDL,Group) %>% group_by(Group) %>% skim(HDL)


##
TC= LDL+HDL+TG/5

cor(TSH,TG)

cor(LDL,TSH)

#######

LipoproteinA = simcorone(TSH,ymean = 26,ysd=10,correlation = 0.7)

LipoproteinA = 26+0.7*TSH-0.02*FBS+0.0007*FBS*TSH+rnorm(75,0,5)

data.frame(LipoproteinA,Group) %>% group_by(Group) %>% skim(LipoproteinA)

###

Age= c(
  rtruncnorm(25,18,65,35,10),
  rtruncnorm(25,18,65,34,11),
  rtruncnorm(25,18,65,37,11)
)

set.seed()

Sex=c( rep(c("Male","Female"),times=c(10,15)),
       rep(c("Male","Female"),times=c(10,15)),
       rep(c("Male","Female"),times=c(10,15)))

data.frame(Sex,Group) %>% count(Group,Sex)


dsh= data.frame(Age,Sex,ALT,AST,Urea,Creatinine,Weight,BMI,FBS,PPBS,HbA1c,Group,TSH,T3,T4,TC,LDL,HDL,TG,VLDL,LipoproteinA)

dsh %>% mutate(Smoker=
                 case_when(Sex=="Male"~sample(c("Smoker","Non-smoker"),75,replace=TRUE,prob=c(0.3,0.7)),
                           TRUE~sample(c("Smoker","Non-smoker"),75,replace=TRUE,prob=c(0.1,0.9)))) ->dsh 
 

#round digits
dsh %>% mutate_at(vars(Age,ALT,AST,Urea,Weight,FBS,PPBS,HbA1c,TC,LDL,HDL,TG,VLDL,LipoproteinA),round,0) %>% 
  mutate_if(is.numeric,round,1) ->dsh

#random sample
rows=sample(nrow(dsh))

dsh-> dsh[rows,]

#########
library(tidyverse)
library(truncnorm)

#round to nearest even

black=0
dsh %>% arrange(Group) %>% mutate(SBP= c(rtruncnorm(25,110,190,140,10),
                                  rtruncnorm(25,110,190,142,12),
                                  rtruncnorm(25,110,190,146,12)) %>% round(digits = 0),
                                  DBP=simcorone(SBP,ymean = 90,ysd=10,correlation = 0.7) %>% round(digits=0),
                                  cursmoker=ifelse(Smoker=="Smoker",1,0) , SBP=2*round(SBP/2),
                                  DBP=2*round(DBP/2) , trtbp=rbinom(75,1,0.4),
                                  ASCVD=case_when(
                                    Sex=="Male"~ascvdam(age=Age,sysbp=SBP,rxbp=trtbp,totchol=TC,hdlc=HDL,cursmoke=cursmoker,dm=1),
                                    Sex=="Female"~ascvdaf(age=Age,sysbp=SBP,rxbp=trtbp,totchol=TC,hdlc=HDL,cursmoke=cursmoker,dm=1)) %>% round(digits=1)
                                  ) -> dsh


dsh %>% group_by(Group) %>% summarise(mean(FBS))

dsh %>% arrange(Group) %>% mutate(HbA1c= simcorone(FBS,ymean = 8.0,ysd=0.7,correlation = 0.9) %>% round(digits = 1)
)->dsh


TukeyHSD(aov(HbA1c~Group))

library(googledrive)

ascvdaf(age=33,sysbp = 144,rxbp=0,totchol = 235,hdlc=39,cursmoke = 0,dm=1)

library(skimr)

dsh %>% group_by(Group) %>% skim(TSH)

####datareport
makeDataReport(dsh, replace=TRUE,  render = FALSE, openResult = FALSE)

summary(dsh$Group)
levels(dsh$Group)

#########
library(ggplot2)
cut(dsh$Age,breaks=seq(
  (min(dsh$Age)%/%10)*10,(max(dsh$Age)%/%10+1)*10,
  by=10))%>%
  
  str_remove_all("\\(|]")%>%
  str_replace_all(",","-") %>%as.factor()->dsh$agec

dsh %>% group_by(Group,agec,Sex) %>% summarise(n=n(),mean(LDL),sd(LDL)) %>% arrange(Group,agec) %>% 
  ungroup() %>% rename(`Age Group`=2,Mean=5,SD=6) %>% replace_na(list(SD=0))

#random sample
rows=sample(nrow(dsh))

dsh-> dsh[rows,]

write.csv(dsh,'dsh.csv')

#######
library(tidyverse)

dsh2=dsh1 %>% mutate(LDL= 130+0.5*TSH+0.6*FBS+0.01*FBS*TSH+0.2*Age+rnorm(75,0,15))

library(broom)
set.seed(123)
dsh2 %>%
  options(scipen = 999)
  summary(lm(LDL~TSH*FBS,data=dsh)) 
  
  library(magrittr)
  library(skimr)

  dsh2 %$% cor.test(HDL,TSH)
  
  dsh2$Group=factor(dsh2$Group,levels=c("DM","DM+SCH","DM+HYPOTHYROID"))

  dsh2 %>% group_by(Group) %>% skim(LDL)
  
  TukeyHSD(aov(LDL~Group,data=dsh2))
  
  #########
  sig <- matrix(c(1.0, 0.8, 0.5, 0.2,
                  0.8, 1.0, 0.5, 0.5,
                  0.5, 0.5, 1.0, 0.5,
                  0.2, 0.5, 0.5, 1.0), nrow = 4)
  library(MASS)
  df.4 <- data.frame(mvrnorm(n = 1000, mu = rep(0, 4), Sigma = sig, empirical = TRUE))
  detach("package:MASS")
  summary(df.4)
  ncol(df.4)
  nrow(df.4)
  cor.test(df.4$X1,df.4$X2)