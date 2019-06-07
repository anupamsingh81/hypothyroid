

########ASCVD#################3


#code smok c,diab as 1,0




ascvdf= function(age,numTC,hdl,numBP,smokc,diab){
  cal=-29.799*log(age)+4.884*log(age)*log(age)+13.540*log(numTC)-3.114*log(age)*log(numTC)-13.578*log(hdl)+3.149*log(age)*log(hdl)+
    1.957*log(numBP)+7.574*smokc-1.665*log(age)*smokc+0.661*diab
  
  
  ASCVD<-round(100*(1-(0.9665^exp(cal+29.18))),2)
  ASCVD}

ascvdm = function(age,numTC,hdl,numBP,smokc,diab){
  cal=12.344*log(age)+11.853*log(numTC)-2.664*log(age)*log(numTC)-7.990*log(hdl)+1.769*log(age)*log(hdl)+
    1.764*log(numBP)+7.837*smokc-1.795*log(age)*smokc+0.658*diab
  
  ASCVD<-round(100*(1-(0.9144^exp(cal-61.18))),2)
  ASCVD
}



##############WHO###############

#Step 1: Install whoishRisk package
> library(devtools)
> install_github("DylanRJCollins/whoishRisk")
#Step 2: Load whoishRisk package into workspace
> library(whoishRisk)
#Step 3: Load risk factor data
> Age <- c(40, 87, 65, 53, 71) #Age in years
> Gender <- c(0,0,0,1,1) #0=female, 1=male
> Smoking <- c(1,1,0,1,0) #0=non-smoker 1=smoker
> Systolic_Blood_Pressure <- c(129, 157, 134, 189, 141) #SBP in mmHg
> Diabetes <- c(1,1,1,0,1) #0=not diabetic 1=diabetic
> Total_Cholesterol <- c(0, 5.1, 4.5, 0, 8.3) #Total cholesterol (mmol/L, 0=unknown cholesterol)
#Step 4: Pass the risk factor vectors to the WHO_ISH_Risk() function, and set subregion equal to the name of the appropriate epidemiological subregion (e.g. “EMR_B”). This will return a vector of WHO/ISH risk scores.
> WHO_ISH_Risk(Age, Gender, Smoking, Systolic_Blood_Pressure, Diabetes, Total_Cholesterol,"EMR_B")
[1] "<10%" ">=40%" "<10%" ">=40%" ">=40%" 


who_risk = WHO_ISH_Risk(Age, Gender, Smoking, Systolic_Blood_Pressure, Diabetes, Total_Cholesterol,"SEAR_D")


library(devtools)
install_github("DylanRJCollins/whoishRisk") 

library(tidyverse)
library(magrittr)
library(whoishRisk)

cohort$who_risk=cohort %>% mutate(
  Gender=ifelse(sex=="male",1,0),
  Smoking=ifelse(Smoker=="smoker",1,0),
  Diabetes=ifelse(Diabetes=="Diabetes",1,0),
  Total_Cholesterol= Total_Cholesterol*0.02586
) %$% WHO_ISH_Risk(Age,Gender,Smoking, SBP, Diabetes, Total_Cholesterol,"SEAR_D")



###############################FRS##############333


df= tibble(
  gender=c('m','f'),
  time=c(10,20),
  
  age=c(35,45),
  bmi=c(24.3,28.3),
  sbp=c(122,142),
  ht_treat=c('False','True'),
  smk=c('True','False'),
  dia=c('False','True'))

write.csv(df,'df.csv')


######python

library(reticulate)

py_discover_config()

use_python("/usr/bin/python3", required=TRUE)

py_config()

repl_python()


from frs import frs

import numpy as np


import pandas as pd
df = pd.read_csv('df.csv')
df.drop(columns=['Unnamed: 0'])

df['Framingham'] = np.vectorize(frs)(df['gender'], df['time'],df['age'], df['bmi'],df['sbp'], df['ht_treat'],df['smk'], df['dia'])

df.to_csv('framingham.csv')

df$framingham = read_csv('framingham.csv')$Framingham


#########3333


df3 =
  tibble(
    gender= ifelse(cohort$sex=='male','m','f'),
    time=
      
  )

##############333 FRS###



frss= function(Age,TotalChol,HDLChol,SysBP,Cig,DM,sex,trt){
  
  if(sex=="female"){
    AgeFactor = 2.32888; 
    TotalCholFactor = 1.20904; HDLCholFactor = -0.70833; AvgRisk = 26.1931 ;RiskPeriodFactor = 0.95012;
    SysBPFactor=ifelse(trt==1,2.822,2.761);
    smokefactor=0.528;diabetesfactor=0.691
    
  }
  
  if(sex=="male"){AgeFactor = 3.06117; TotalCholFactor = 1.12370; HDLCholFactor = -0.93263; AvgRisk = 23.9802 ;RiskPeriodFactor = 0.88936;
  SysBPFactor=ifelse(trt==1,1.998,1.933);
  smokefactor=0.654;diabetesfactor=0.573
  }
  RiskFactors = (log(Age) * AgeFactor) + (log(TotalChol) * TotalCholFactor) + (log(HDLChol) * HDLCholFactor) + (log(SysBP) * SysBPFactor) + smokefactor*Cig + diabetesfactor*DM - AvgRisk
  Risk = 100 * (1 - RiskPeriodFactor^exp(RiskFactors))
  Risk
  
}

frss(sex= "female",Age= 45,TotalChol = 220,HDLChol = 40,SysBP= 150,Cig = 0,DM = 0,trt=1)






############33333





frss(sex= "male",Age= 34,TotalChol = 191,HDLChol = 62,SysBP= 152,Cig = 0,DM = 1,trt=1)

ascvdm(age=34,numTC=191,hdl=62,numBP=152,smokc=0,diab=1)



frss1(sex= "male",Age= 45,TotalChol = 220,HDLChol = 40,SysBP= 150,Cig = 0,DM = 1,trt=1)

##augustino

frss(sex= "female",Age= 61,TotalChol = 180,HDLChol = 47,SysBP= 124,Cig = 1,DM = 0,trt=0)

frss(sex= "male",Age= 53,TotalChol = 161,HDLChol = 55,SysBP= 125,Cig = 0,DM = 1,trt=1)


########################################3



############

frss(sex= "female",Age= 61,TotalChol = 180,HDLChol = 47,SysBP= 124,Cig = 1,DM = 0,trt=0)






##############33333

frssf= function(Age,TotalChol,HDLChol,SysBP,Cig,DM,trt){
  
  
  AgeFactor = 2.32888; 
  TotalCholFactor = 1.20904; HDLCholFactor = -0.70833; AvgRisk = 26.1931 ;RiskPeriodFactor = 0.95012;
  SysBPFactor=ifelse(trt==1,2.822,2.761);
  smokefactor=0.528;diabetesfactor=0.691
  RiskFactors = (log(Age) * AgeFactor) + (log(TotalChol) * TotalCholFactor) + (log(HDLChol) * HDLCholFactor) + (log(SysBP) * SysBPFactor) + smokefactor*Cig + diabetesfactor*DM - AvgRisk
  Risk = 100 * (1 - RiskPeriodFactor^exp(RiskFactors))
  Risk
}

frssm= function(Age,TotalChol,HDLChol,SysBP,Cig,DM,trt){
  
  
  AgeFactor = 3.06117; TotalCholFactor = 1.12370; HDLCholFactor = -0.93263; AvgRisk = 23.9802 ;RiskPeriodFactor = 0.88936;
  SysBPFactor=ifelse(trt==1,1.998,1.933);
  smokefactor=0.654;diabetesfactor=0.573
  
  RiskFactors = (log(Age) * AgeFactor) + (log(TotalChol) * TotalCholFactor) + (log(HDLChol) * HDLCholFactor) + (log(SysBP) * SysBPFactor) + smokefactor*Cig + diabetesfactor*DM - AvgRisk
  Risk = 100 * (1 - RiskPeriodFactor^exp(RiskFactors))
  Risk
  
}


###new pooled CE basu#######

ascvdaf= function(age,sysbp,rxbp,totchol,hdlc,cursmoke,dm){
  
  risk <- 100 / (1.0 + exp( - (
    -12.823110 +
      0.106501 * as.numeric(age) +
      0.432440 * as.numeric(black) +
      0.000056 * (as.numeric(  sysbp) ^ 2) +
      0.017666 * as.numeric( sysbp) +
      0.731678 * as.numeric( rxbp) +
      0.943970 * as.numeric( dm) +
      1.009790 * as.numeric( cursmoke) +
      0.151318 * (as.numeric( totchol) / as.numeric( hdlc)) +
      -0.008580 * as.numeric( age) * as.numeric( black) +
      -0.003647 * as.numeric( sysbp) * as.numeric( rxbp) +
      0.006208 * as.numeric( sysbp) * as.numeric( black) +
      0.152968 * as.numeric( black) * as.numeric( rxbp) +
      -0.000153 * as.numeric( age) * as.numeric( sysbp) +
      0.115232 * as.numeric( black) * as.numeric( dm) +
      -0.092231 * as.numeric( black) * as.numeric( cursmoke) +
      0.070498 * as.numeric( black) * (as.numeric( totchol) / as.numeric( hdlc)) +
      -0.000173 * as.numeric( black)  * as.numeric( sysbp) * as.numeric( rxbp) +
      -0.000094 * as.numeric( age) * as.numeric( sysbp) * as.numeric( black)
  )))
  
  risk}



ascvdam= function(age,sysbp,rxbp,totchol,hdlc,cursmoke,dm){
  
  risk <- 100 / (1.0 + exp( - (
    -11.679980 +
      0.064200 * as.numeric( age) +
      0.482835 * as.numeric( black) +
      -0.000061 * (as.numeric( sysbp) ^ 2) +
      0.038950 * as.numeric( sysbp) +
      2.055533 * as.numeric( rxbp) +
      0.842209 * as.numeric( dm) +
      0.895589 * as.numeric( cursmoke) +
      0.193307 * (as.numeric( totchol) / as.numeric( hdlc)) +
      -0.014207 * as.numeric( sysbp) * as.numeric( rxbp) +
      0.011609 * as.numeric( sysbp) * as.numeric( black) +
      -0.119460 * as.numeric( rxbp) * as.numeric( black) +
      0.000025 * as.numeric( age) * as.numeric( sysbp) +
      -0.077214 * as.numeric( black) * as.numeric( dm) +
      -0.226771 * as.numeric( black) * as.numeric( cursmoke) +
      -0.117749 * (as.numeric( totchol) / as.numeric( hdlc)) * as.numeric( black) +
      0.004190 * as.numeric( black) * as.numeric( rxbp) * as.numeric( sysbp) +
      -0.000199 * as.numeric( black) * as.numeric( age) * as.numeric( sysbp)
  )))
  
  risk
}


##################

###############33 final calc######



black=0
cohort %>% 
  mutate(FRS=case_when(
    sex=="male"~frssm(Age=Age,TotalChol = Total_Cholesterol,HDLChol = HDL,SysBP = SBP,Cig=Cig,DM=DM,trt = trt),
    sex=="female"~frssf(Age=Age,TotalChol = Total_Cholesterol,HDLChol = HDL,SysBP = SBP,Cig=Cig,DM=DM,trt = trt)))%>% 
  
  
  mutate(ASCVD=case_when(
    sex=="male"~ascvdm(age=Age,numTC=Total_Cholesterol,hdl=HDL,numBP=SBP,smokc=Cig,diab=DM),
    sex=="female"~ascvdf(age=Age,numTC=Total_Cholesterol,hdl=HDL,numBP=SBP,smokc=Cig,diab=DM))) %>% #slice(6) %>% 
  
  mutate(asc=case_when(
    sex=="male"~ascvdam(age=Age,sysbp=SBP,rxbp=trt,totchol=Total_Cholesterol,hdlc=HDL,cursmoke=Cig,dm=DM),
    sex=="female"~ascvdaf(age=Age,sysbp=SBP,rxbp=trt,totchol=Total_Cholesterol,hdlc=HDL,cursmoke=Cig,dm=DM)))  %>% 
  
  select(Age,Total_Cholesterol,HDL,SBP,trt,Cig,DM,sex,FRS,asc,ASCVD) %>% # filter(Age>50) 
  mutate(diff=ASCVD-asc) %>% filter(diff>75)

mutate(diff=FRS-asc) %>% #filter(diff>75)
  
  ggplot(aes(y=diff,x=asc))+geom_point()


cohort$who_risk=cohort %>% mutate(
  Gender=ifelse(sex=="male",1,0),
  Smoking=ifelse(Smoker=="smoker",1,0),
  Diabetes=ifelse(Diabetes=="Diabetes",1,0),
  Total_Cholesterol= Total_Cholesterol*0.02586
) %$% WHO_ISH_Risk(Age,Gender,Smoking, SBP, Diabetes, Total_Cholesterol,"SEAR_D")

