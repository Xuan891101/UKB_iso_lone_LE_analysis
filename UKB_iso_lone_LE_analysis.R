

library(haven)
library(readxl)
library(car)
library(SciViews)
library(survival) 
library(boot)


#====== Read in data ========    

all <- read_sas("C:/Users/analysis.sas7bdat")

all$age_grp  <- recode(all$age,"lo:39=NA;40:49=2;50:59=3;60:69=4;70:75=5")

all$agediag7 <- c(all$age + (all$dis7_date-all$base_date)/365.25)
all$agediag7_grp <- recode(all$agediag7,"lo:65=0;65:75=1;75:hi=2")

all$agediag5 <- c(all$age + (all$dis5_date-all$base_date)/365.25)
all$agediag5_grp <- recode(all$agediag5,"lo:65=0;65:75=1;75:hi=2")

all$agediag2 <- c(all$age + (all$dis2_date-all$base_date)/365.25)
all$agediag2_grp <- recode(all$agediag2,"lo:65=0;65:75=1;75:hi=2")

all$agediagcvd <- c(all$age + (all$cvd_date-all$base_date)/365.25)
all$agediagcvd_grp <- recode(all$agediagcvd,"lo:65=0;65:75=1;75:hi=2")

all$agediagdia <- c(all$age + (all$dia_date-all$base_date)/365.25)
all$agediagdia_grp <- recode(all$agediagdia,"lo:65=0;65:75=1;75:hi=2")

all$agediagcan <- c(all$age + (all$can_date-all$base_date)/365.25)
all$agediagcan_grp <- recode(all$agediagcan,"lo:65=0;65:75=1;75:hi=2")

all$agediagcrd <- c(all$age + (all$crd_date-all$base_date)/365.25)
all$agediagcrd_grp <- recode(all$agediagcrd,"lo:65=0;65:75=1;75:hi=2")

all$agediagneu <- c(all$age + (all$neu_date-all$base_date)/365.25)
all$agediagneu_grp <- recode(all$agediagneu,"lo:65=0;65:75=1;75:hi=2")

all$agediagdep <- c(all$age + (all$dep_date-all$base_date)/365.25)
all$agediagdep_grp <- recode(all$agediagdep,"lo:65=0;65:75=1;75:hi=2")

all$agediaganx <- c(all$age + (all$anx_date-all$base_date)/365.25)
all$agediaganx_grp <- recode(all$agediaganx,"lo:65=0;65:75=1;75:hi=2")

mal <- subset(all,sex==1)
fem <- subset(all,sex==0)



weightedRR <-function(pp0,pp1,HR1,HR2,HR3,y){
  
  m1_0 <- 1/(pp0[ ,1]+pp0[ ,2]*HR1[1]+pp0[ ,3]*HR1[2]+pp0[ ,4]*HR1[3])
  m1_1 <- m1_0*HR1[1]
  m1_2 <- m1_0*HR1[2]
  m1_3 <- m1_0*HR1[3]
  mm1 <- cbind(m1_0, m1_1, m1_2, m1_3)
  mm1 <- round(mm1[rep(1:nrow(mm1),c(10,10,10,31)),],5)
  
  m2_0 <- 1/(pp0[ ,1]+pp0[ ,2]*HR2[1]+pp0[ ,3]*HR2[2]+pp0[ ,4]*HR2[3])
  m2_1 <- m2_0*HR2[1]
  m2_2 <- m2_0*HR2[2]
  m2_3 <- m2_0*HR2[3]
  mm2 <- cbind(m2_0, m2_1, m2_2, m2_3)
  mm2 <- round(mm2[rep(1:nrow(mm2),c(10,10,10,31)),],5)
  
  m3_0 <- 1/(pp1[ ,1]+pp1[ ,2]*HR3[1]+pp1[ ,3]*HR3[2]+pp1[ ,4]*HR3[3])
  m3_1 <- m3_0*HR3[1]
  m3_2 <- m3_0*HR3[2]
  m3_3 <- m3_0*HR3[3]
  mm3 <- cbind(m3_0, m3_1, m3_2, m3_3)
  mm3 <- round(mm3[rep(1:nrow(mm3),c(10,10,10,31)),],5)
  
  RR1 <- mm1*y$T1
  RR2 <- mm2*y$T2
  RR3 <- mm3*y$T3

  gg0 <- cbind(B=RR1[,1],C=RR2[,1],F=RR3[,1])
  gg1 <- cbind(B=RR1[,2],C=RR2[,2],F=RR3[,2])
  gg2 <- cbind(B=RR1[,3],C=RR2[,3],F=RR3[,3])
  gg3 <- cbind(B=RR1[,4],C=RR2[,4],F=RR3[,4])

  grp0 = life.table(gg0)
  grp1 = life.table(gg1)
  grp2 = life.table(gg2)
  grp3 = life.table(gg3)
  Dif1 = grp0-grp1
  Dif2 = grp0-grp2
  Dif3 = grp0-grp3
  aaa<-cbind(grp0, grp1, grp2,grp3,Dif1,Dif2,Dif3)
  
  return(aaa)
}


#======= Fig1 and SuppFig 2-3========
#=======LE.dis7
LE.dis7 <- function(x,y){
  
  #======Prevalence
  #Without disease
  p0 <-table(x$age_grp,x$joint)
  p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
  pp0 <-cbind(p0[,1]/p0[,5],p0[,2]/p0[,5],p0[,3]/p0[,5],p0[,4]/p0[,5])
  
  #With disease
  data1 <- subset(x,dis7_icd10_inc==1)
  p1 <- table(data1$age_grp,data1$joint)
  p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
  pp1 <-cbind(p1[,1]/p1[,5],p1[,2]/p1[,5],p1[,3]/p1[,5],p1[,4]/p1[,5])
  
  #======HR
  #Trans1 (non-disease to disease)
  fit1 <- coxph(Surv((dis7_date-base_date),dis7_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group) +factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR1 <-round(summary(fit1)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans2 (non-disease to death)
  fit2 <- coxph(Surv((dis7_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR2 <-round(summary(fit2)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans3 (disease to death)
  fit3 <- coxph(Surv((death_Date-dis7_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag7_grp)
                +factor(joint)
                ,data=data1,method=("breslow"))
  HR3 <-round(summary(fit3)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  gg <-weightedRR(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

dis7_fem <-LE.dis7(fem,T_fem7)
dis7_mal <-LE.dis7(mal,T_mal7)
dis7 <- cbind(dis7_fem,dis7_mal)
write.csv(dis7,file="Fig1_dis7.csv",quote=F,row.names=T)


#=======LE.dis5
LE.dis5 <- function(x,y){
  
  #======Prevalence
  #Without disease
  p0 <-table(x$age_grp,x$joint)
  p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
  pp0 <-cbind(p0[,1]/p0[,5],p0[,2]/p0[,5],p0[,3]/p0[,5],p0[,4]/p0[,5])
  
  #With disease
  data1 <- subset(x,dis5_icd10_inc==1)
  p1 <- table(data1$age_grp,data1$joint)
  p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
  pp1 <-cbind(p1[,1]/p1[,5],p1[,2]/p1[,5],p1[,3]/p1[,5],p1[,4]/p1[,5])
  
  #======HR
  #Trans1 (non-disease to disease)
  fit1 <- coxph(Surv((dis5_date-base_date),dis5_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR1 <-round(summary(fit1)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans2 (non-disease to death)
  fit2 <- coxph(Surv((dis5_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR2 <-round(summary(fit2)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans3 (disease to death)
  fit3 <- coxph(Surv((death_Date-dis5_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag5_grp)
                +factor(joint)
                ,data=data1,method=("breslow"))
  HR3 <-round(summary(fit3)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  gg <-weightedRR(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

dis5_fem <-LE.dis5(fem,T_fem5)
dis5_mal <-LE.dis5(mal,T_mal5)
dis5 <- cbind(dis5_fem,dis5_mal)
write.csv(dis5,file="Fig1_dis5.csv",quote=F,row.names=T)


#=======LE.dis2
LE.dis2 <- function(x,y){
  
  #======Prevalence
  #Without disease
  p0 <-table(x$age_grp,x$joint)
  p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
  pp0 <-cbind(p0[,1]/p0[,5],p0[,2]/p0[,5],p0[,3]/p0[,5],p0[,4]/p0[,5])
  
  #With disease
  data1 <- subset(x,dis2_icd10_inc==1)
  p1 <- table(data1$age_grp,data1$joint)
  p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
  pp1 <-cbind(p1[,1]/p1[,5],p1[,2]/p1[,5],p1[,3]/p1[,5],p1[,4]/p1[,5])
  
  #======HR
  #Trans1 (non-disease to disease)
  fit1 <- coxph(Surv((dis2_date-base_date),dis2_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR1 <-round(summary(fit1)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans2 (non-disease to death)
  fit2 <- coxph(Surv((dis2_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR2 <-round(summary(fit2)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans3 (disease to death)
  fit3 <- coxph(Surv((death_Date-dis2_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag2_grp)
                +factor(joint)
                ,data=data1,method=("breslow"))
  HR3 <-round(summary(fit3)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  gg <-weightedRR(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

dis2_fem <-LE.dis2(fem,T_fem2)
dis2_mal <-LE.dis2(mal,T_mal2)
dis2 <- cbind(dis2_fem,dis2_mal)
write.csv(dis2,file="Fig1_dis2.csv",quote=F,row.names=T)



#======= Fig2 ========
#=======LE.cvd

LE.cvd <- function(x,y){
  
  #======Prevalence
  #Without disease
  p0 <-table(x$age_grp,x$joint)
  p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
  pp0 <-cbind(p0[,1]/p0[,5],p0[,2]/p0[,5],p0[,3]/p0[,5],p0[,4]/p0[,5])
  
  #With disease
  data1 <- subset(x,cvd_icd10_inc==1)
  p1 <- table(data1$age_grp,data1$joint)
  p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
  pp1 <-cbind(p1[,1]/p1[,5],p1[,2]/p1[,5],p1[,3]/p1[,5],p1[,4]/p1[,5])
  
  #======HR
  #Trans1 (non-disease to disease)
  fit1 <- coxph(Surv((cvd_date-base_date),cvd_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR1 <-round(summary(fit1)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans2 (non-disease to death)
  fit2 <- coxph(Surv((cvd_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR2 <-round(summary(fit2)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans3 (disease to death)
  fit3 <- coxph(Surv((death_Date-cvd_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income) +factor(agediagcvd_grp)
                +factor(joint)
                ,data=data1,method=("breslow"))
  HR3 <-round(summary(fit3)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  gg <-weightedRR(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

cvd_fem <-LE.cvd(fem,T_femcvd)
cvd_mal <-LE.cvd(mal,T_malcvd)
cvd <- cbind(cvd_fem,cvd_mal)
write.csv(cvd,file="Fig2_cvd.csv",quote=F,row.names=T)


#=======LE.dia

LE.dia <- function(x,y){
  
  #======Prevalence
  #Without disease
  p0 <-table(x$age_grp,x$joint)
  p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
  pp0 <-cbind(p0[,1]/p0[,5],p0[,2]/p0[,5],p0[,3]/p0[,5],p0[,4]/p0[,5])
  
  #With disease
  data1 <- subset(x,dia_icd10_inc==1)
  p1 <- table(data1$age_grp,data1$joint)
  p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
  pp1 <-cbind(p1[,1]/p1[,5],p1[,2]/p1[,5],p1[,3]/p1[,5],p1[,4]/p1[,5])
  
  #======HR
  #Trans1 (non-disease to disease)
  fit1 <- coxph(Surv((dia_date-base_date),dia_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR1 <-round(summary(fit1)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans2 (non-disease to death)
  fit2 <- coxph(Surv((dia_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR2 <-round(summary(fit2)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans3 (disease to death)
  fit3 <- coxph(Surv((death_Date-dia_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediagdia_grp)
                +factor(joint)
                ,data=data1,method=("breslow"))
  HR3 <-round(summary(fit3)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  gg <-weightedRR(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

dia_fem <-LE.dia(fem,T_femdia)
dia_mal <-LE.dia(mal,T_maldia)
dia <- cbind(dia_fem,dia_mal)
write.csv(dia,file="Fig2_dia.csv",quote=F,row.names=T)


#=======LE.can

LE.can <- function(x,y){
  
  #======Prevalence
  #Without disease
  p0 <-table(x$age_grp,x$joint)
  p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
  pp0 <-cbind(p0[,1]/p0[,5],p0[,2]/p0[,5],p0[,3]/p0[,5],p0[,4]/p0[,5])
  
  #With disease
  data1 <- subset(x,can_icd10_inc==1)
  p1 <- table(data1$age_grp,data1$joint)
  p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
  pp1 <-cbind(p1[,1]/p1[,5],p1[,2]/p1[,5],p1[,3]/p1[,5],p1[,4]/p1[,5])
  
  #======HR
  #Trans1 (non-disease to disease)
  fit1 <- coxph(Surv((can_date-base_date),can_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR1 <-round(summary(fit1)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans2 (non-disease to death)
  fit2 <- coxph(Surv((can_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR2 <-round(summary(fit2)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans3 (disease to death)
  fit3 <- coxph(Surv((death_Date-can_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediagcan_grp)
                +factor(joint)
                ,data=data1,method=("breslow"))
  HR3 <-round(summary(fit3)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  gg <-weightedRR(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

can_fem <-LE.can(fem,T_femcan)
can_mal <-LE.can(mal,T_malcan)
can <- cbind(can_fem,can_mal)
write.csv(can,file="Fig2_can.csv",quote=F,row.names=T)


#=======LE.crd

LE.crd <- function(x,y){
  
  #======Prevalence
  #Without disease
  p0 <-table(x$age_grp,x$joint)
  p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
  pp0 <-cbind(p0[,1]/p0[,5],p0[,2]/p0[,5],p0[,3]/p0[,5],p0[,4]/p0[,5])
  
  #With disease
  data1 <- subset(x,crd_icd10_inc==1)
  p1 <- table(data1$age_grp,data1$joint)
  p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
  pp1 <-cbind(p1[,1]/p1[,5],p1[,2]/p1[,5],p1[,3]/p1[,5],p1[,4]/p1[,5])
  
  #======HR
  #Trans1 (non-disease to disease)
  fit1 <- coxph(Surv((crd_date-base_date),crd_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR1 <-round(summary(fit1)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans2 (non-disease to death)
  fit2 <- coxph(Surv((crd_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR2 <-round(summary(fit2)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans3 (disease to death)
  fit3 <- coxph(Surv((death_Date-crd_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediagcrd_grp)
                +factor(joint)
                ,data=data1,method=("breslow"))
  HR3 <-round(summary(fit3)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  gg <-weightedRR(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

crd_fem <-LE.crd(fem,T_femcrd)
crd_mal <-LE.crd(mal,T_malcrd)
crd <- cbind(crd_fem,crd_mal)
write.csv(crd,file="Fig2_crd.csv",quote=F,row.names=T)


#=======LE.neu

LE.neu <- function(x,y){
  
  #======Prevalence
  #Without disease
  p0 <-table(x$age_grp,x$joint)
  p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
  pp0 <-cbind(p0[,1]/p0[,5],p0[,2]/p0[,5],p0[,3]/p0[,5],p0[,4]/p0[,5])
  
  #With disease
  data1 <- subset(x,neu_icd10_inc==1)
  p1 <- table(data1$age_grp,data1$joint)
  p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
  pp1 <-cbind(p1[,1]/p1[,5],p1[,2]/p1[,5],p1[,3]/p1[,5],p1[,4]/p1[,5])
  
  #======HR
  #Trans1 (non-disease to disease)
  fit1 <- coxph(Surv((neu_date-base_date),neu_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR1 <-round(summary(fit1)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans2 (non-disease to death)
  fit2 <- coxph(Surv((neu_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR2 <-round(summary(fit2)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans3 (disease to death)
  fit3 <- coxph(Surv((death_Date-neu_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediagneu_grp)
                +factor(joint)
                ,data=data1,method=("breslow"))
  HR3 <-round(summary(fit3)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  gg <-weightedRR(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

neu_fem <-LE.neu(fem,T_femneu)
neu_mal <-LE.neu(mal,T_malneu)
neu <- cbind(neu_fem,neu_mal)
write.csv(neu,file="Fig2_neu.csv",quote=F,row.names=T)


#=======LE.dep

LE.dep <- function(x,y){
  
  #======Prevalence
  #Without disease
  p0 <-table(x$age_grp,x$joint)
  p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
  pp0 <-cbind(p0[,1]/p0[,5],p0[,2]/p0[,5],p0[,3]/p0[,5],p0[,4]/p0[,5])
  
  #With disease
  data1 <- subset(x,dep_icd10_inc==1)
  p1 <- table(data1$age_grp,data1$joint)
  p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
  pp1 <-cbind(p1[,1]/p1[,5],p1[,2]/p1[,5],p1[,3]/p1[,5],p1[,4]/p1[,5])
  
  #======HR
  #Trans1 (non-disease to disease)
  fit1 <- coxph(Surv((dep_date-base_date),dep_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR1 <-round(summary(fit1)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans2 (non-disease to death)
  fit2 <- coxph(Surv((dep_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR2 <-round(summary(fit2)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans3 (disease to death)
  fit3 <- coxph(Surv((death_Date-dep_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediagdep_grp)
                +factor(joint)
                ,data=data1,method=("breslow"))
  HR3 <-round(summary(fit3)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  gg <-weightedRR(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

dep_fem <-LE.dep(fem,T_femdep)
dep_mal <-LE.dep(mal,T_maldep)
dep <- cbind(dep_fem,dep_mal)
write.csv(dep,file="Fig2_dep.csv",quote=F,row.names=T)



#=======LE.anx

LE.anx <- function(x,y){
  
  #======Prevalence
  #Without disease
  p0 <-table(x$age_grp,x$joint)
  p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
  pp0 <-cbind(p0[,1]/p0[,5],p0[,2]/p0[,5],p0[,3]/p0[,5],p0[,4]/p0[,5])
  
  #With disease
  data1 <- subset(x,anx_icd10_inc==1)
  p1 <- table(data1$age_grp,data1$joint)
  p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
  pp1 <-cbind(p1[,1]/p1[,5],p1[,2]/p1[,5],p1[,3]/p1[,5],p1[,4]/p1[,5])
  
  #======HR
  #Trans1 (non-disease to disease)
  fit1 <- coxph(Surv((anx_date-base_date),anx_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR1 <-round(summary(fit1)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans2 (non-disease to death)
  fit2 <- coxph(Surv((anx_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(joint)
                ,data=x,method=("breslow"))
  HR2 <-round(summary(fit2)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  #Trans3 (disease to death)
  fit3 <- coxph(Surv((death_Date-anx_date),death) ~ age +factor(white)  +Townsend_Index  +factor(edu_group)+factor(income)+factor(agediaganx_grp)
                +factor(joint)
                ,data=data1,method=("breslow"))
  HR3 <-round(summary(fit3)$conf.int[c("factor(joint)1","factor(joint)2","factor(joint)3"),1],3)
  
  gg <-weightedRR(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

anx_fem <-LE.anx(fem,T_femanx)
anx_mal <-LE.anx(mal,T_malanx)
anx <- cbind(anx_fem,anx_mal)
write.csv(anx,file="Fig2_anx.csv",quote=F,row.names=T)




#======= Table 2 and SuppFig 4========
#=======LE.dis7-loneliness

LE.dis7 <- function(x,y){
  
  #======Prevalence
  #Without disease
  p0 <-table(x$age_grp,x$jointllls)
  p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
  pp0 <-cbind(p0[,1]/p0[,7],p0[,2]/p0[,7],p0[,3]/p0[,7],p0[,4]/p0[,7],p0[,5]/p0[,7],p0[,6]/p0[,7])
  
  #With disease
  data1 <- subset(x,dis7_icd10_inc==1)
  p1 <- table(data1$age_grp,data1$jointllls)
  p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
  pp1 <-cbind(p1[,1]/p1[,7],p1[,2]/p1[,7],p1[,3]/p1[,7],p1[,4]/p1[,7],p1[,5]/p1[,7],p1[,6]/p1[,7])
  
  #======HR
  #Trans1 (non-disease to disease)
  fit1 <- coxph(Surv((dis7_date-base_date),dis7_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(jointllls) +iso_status
                ,data=x,method=("breslow"))
  HR1 <-round(summary(fit1)$conf.int[c("factor(jointllls)1","factor(jointllls)2","factor(jointllls)3",
                                       "factor(jointllls)4","factor(jointllls)5"),1],3)
  
  #Trans2 (non-disease to death)
  fit2 <- coxph(Surv((dis7_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(jointllls) +iso_status
                ,data=x,method=("breslow"))
  HR2 <-round(summary(fit2)$conf.int[c("factor(jointllls)1","factor(jointllls)2","factor(jointllls)3",
                                       "factor(jointllls)4","factor(jointllls)5"),1],3)
  
  #Trans3 (disease to death)
  fit3 <- coxph(Surv((death_Date-dis7_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag7_grp)
                +factor(jointllls) +iso_status
                ,data=data1,method=("breslow"))
  HR3 <-round(summary(fit3)$conf.int[c("factor(jointllls)1","factor(jointllls)2","factor(jointllls)3",
                                       "factor(jointllls)4","factor(jointllls)5"),1],3)
  
  gg <-weightedRR6(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

dis7_fem <-LE.dis7(fem,T_fem7)
dis7_mal <-LE.dis7(mal,T_mal7)
dis7 <- cbind(dis7_fem,dis7_mal)
write.csv(dis7,file="Tab2_lone_dis7.csv",quote=F,row.names=T)


#=======LE.dis5-loneliness

LE.dis5 <- function(x,y){
  
  #======Prevalence
  #Without disease
  p0 <-table(x$age_grp,x$jointllls)
  p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
  pp0 <-cbind(p0[,1]/p0[,7],p0[,2]/p0[,7],p0[,3]/p0[,7],p0[,4]/p0[,7],p0[,5]/p0[,7],p0[,6]/p0[,7])
  
  #With disease
  data1 <- subset(x,dis5_icd10_inc==1)
  p1 <- table(data1$age_grp,data1$jointllls)
  p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
  pp1 <-cbind(p1[,1]/p1[,7],p1[,2]/p1[,7],p1[,3]/p1[,7],p1[,4]/p1[,7],p1[,5]/p1[,7],p1[,6]/p1[,7])
  
  #======HR
  #Trans1 (non-disease to disease)
  fit1 <- coxph(Surv((dis5_date-base_date),dis5_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(jointllls) +iso_status
                ,data=x,method=("breslow"))
  HR1 <-round(summary(fit1)$conf.int[c("factor(jointllls)1","factor(jointllls)2","factor(jointllls)3",
                                       "factor(jointllls)4","factor(jointllls)5"),1],3)
  
  #Trans2 (non-disease to death)
  fit2 <- coxph(Surv((dis5_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(jointllls) +iso_status
                ,data=x,method=("breslow"))
  HR2 <-round(summary(fit2)$conf.int[c("factor(jointllls)1","factor(jointllls)2","factor(jointllls)3",
                                       "factor(jointllls)4","factor(jointllls)5"),1],3)
  
  #Trans3 (disease to death)
  fit3 <- coxph(Surv((death_Date-dis5_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag5_grp)
                +factor(jointllls) +iso_status
                ,data=data1,method=("breslow"))
  HR3 <-round(summary(fit3)$conf.int[c("factor(jointllls)1","factor(jointllls)2","factor(jointllls)3",
                                       "factor(jointllls)4","factor(jointllls)5"),1],3)
  
  gg <-weightedRR6(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

dis5_fem <-LE.dis5(fem,T_fem5)
dis5_mal <-LE.dis5(mal,T_mal5)
dis5 <- cbind(dis5_fem,dis5_mal)
write.csv(dis5,file="Tab2_lone_dis5.csv",quote=F,row.names=T)


#=======LE.dis2-loneliness

LE.dis2 <- function(x,y){
  
  #======Prevalence
  #Without disease
  p0 <-table(x$age_grp,x$jointllls)
  p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
  pp0 <-cbind(p0[,1]/p0[,7],p0[,2]/p0[,7],p0[,3]/p0[,7],p0[,4]/p0[,7],p0[,5]/p0[,7],p0[,6]/p0[,7])
  
  #With disease
  data1 <- subset(x,dis2_icd10_inc==1)
  p1 <- table(data1$age_grp,data1$jointllls)
  p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
  pp1 <-cbind(p1[,1]/p1[,7],p1[,2]/p1[,7],p1[,3]/p1[,7],p1[,4]/p1[,7],p1[,5]/p1[,7],p1[,6]/p1[,7])
  
  #======HR
  #Trans1 (non-disease to disease)
  fit1 <- coxph(Surv((dis2_date-base_date),dis2_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(jointllls) +iso_status
                ,data=x,method=("breslow"))
  HR1 <-round(summary(fit1)$conf.int[c("factor(jointllls)1","factor(jointllls)2","factor(jointllls)3",
                                       "factor(jointllls)4","factor(jointllls)5"),1],3)
  
  #Trans2 (non-disease to death)
  fit2 <- coxph(Surv((dis2_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(jointllls) +iso_status
                ,data=x,method=("breslow"))
  HR2 <-round(summary(fit2)$conf.int[c("factor(jointllls)1","factor(jointllls)2","factor(jointllls)3",
                                       "factor(jointllls)4","factor(jointllls)5"),1],3)
  
  #Trans3 (disease to death)
  fit3 <- coxph(Surv((death_Date-dis2_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag2_grp)
                +factor(jointllls) +iso_status
                ,data=data1,method=("breslow"))
  HR3 <-round(summary(fit3)$conf.int[c("factor(jointllls)1","factor(jointllls)2","factor(jointllls)3",
                                       "factor(jointllls)4","factor(jointllls)5"),1],3)
  
  gg <-weightedRR6(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

dis2_fem <-LE.dis2(fem,T_fem2)
dis2_mal <-LE.dis2(mal,T_mal2)
dis2 <- cbind(dis2_fem,dis2_mal)
write.csv(dis2,file="Tab2_lone_dis2.csv",quote=F,row.names=T)



#=======Table 3 and SuppFig 5========
#=======LE.dis7-isolation

LE.dis7 <- function(x,y){
  
  #======Prevalence
  #Without disease
  p0 <-table(x$age_grp,x$jointiiis)
  p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
  pp0 <-cbind(p0[,1]/p0[,7],p0[,2]/p0[,7],p0[,3]/p0[,7],p0[,4]/p0[,7],p0[,5]/p0[,7],p0[,6]/p0[,7])
  
  #With disease
  data1 <- subset(x,dis7_icd10_inc==1)
  p1 <- table(data1$age_grp,data1$jointiiis)
  p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
  pp1 <-cbind(p1[,1]/p1[,7],p1[,2]/p1[,7],p1[,3]/p1[,7],p1[,4]/p1[,7],p1[,5]/p1[,7],p1[,6]/p1[,7])
  
  #======HR
  #Trans1 (non-disease to disease)
  fit1 <- coxph(Surv((dis7_date-base_date),dis7_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(jointiiis) +lone_status
                ,data=x,method=("breslow"))
  HR1 <-round(summary(fit1)$conf.int[c("factor(jointiiis)1","factor(jointiiis)2","factor(jointiiis)3",
                                       "factor(jointiiis)4","factor(jointiiis)5"),1],3)
  
  #Trans2 (non-disease to death)
  fit2 <- coxph(Surv((dis7_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(jointiiis) +lone_status
                ,data=x,method=("breslow"))
  HR2 <-round(summary(fit2)$conf.int[c("factor(jointiiis)1","factor(jointiiis)2","factor(jointiiis)3",
                                       "factor(jointiiis)4","factor(jointiiis)5"),1],3)
  
  #Trans3 (disease to death)
  fit3 <- coxph(Surv((death_Date-dis7_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag7_grp)
                +factor(jointiiis) +lone_status
                ,data=data1,method=("breslow"))
  HR3 <-round(summary(fit3)$conf.int[c("factor(jointiiis)1","factor(jointiiis)2","factor(jointiiis)3",
                                       "factor(jointiiis)4","factor(jointiiis)5"),1],3)
  
  gg <-weightedRR6(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

dis7_fem <-LE.dis7(fem,T_fem7)
dis7_mal <-LE.dis7(mal,T_mal7)
dis7 <- cbind(dis7_fem,dis7_mal)
write.csv(dis7,file="Tab3_iso_dis7.csv",quote=F,row.names=T)


#=======LE.dis5-isolation

LE.dis5 <- function(x,y){
  
  #======Prevalence
  #Without disease
  p0 <-table(x$age_grp,x$jointiiis)
  p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
  pp0 <-cbind(p0[,1]/p0[,7],p0[,2]/p0[,7],p0[,3]/p0[,7],p0[,4]/p0[,7],p0[,5]/p0[,7],p0[,6]/p0[,7])
  
  #With disease
  data1 <- subset(x,dis5_icd10_inc==1)
  p1 <- table(data1$age_grp,data1$jointiiis)
  p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
  pp1 <-cbind(p1[,1]/p1[,7],p1[,2]/p1[,7],p1[,3]/p1[,7],p1[,4]/p1[,7],p1[,5]/p1[,7],p1[,6]/p1[,7])
  
  #======HR
  #Trans1 (non-disease to disease)
  fit1 <- coxph(Surv((dis5_date-base_date),dis5_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(jointiiis) +lone_status
                ,data=x,method=("breslow"))
  HR1 <-round(summary(fit1)$conf.int[c("factor(jointiiis)1","factor(jointiiis)2","factor(jointiiis)3",
                                       "factor(jointiiis)4","factor(jointiiis)5"),1],3)
  
  #Trans2 (non-disease to death)
  fit2 <- coxph(Surv((dis5_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(jointiiis) +lone_status
                ,data=x,method=("breslow"))
  HR2 <-round(summary(fit2)$conf.int[c("factor(jointiiis)1","factor(jointiiis)2","factor(jointiiis)3",
                                       "factor(jointiiis)4","factor(jointiiis)5"),1],3)
  
  #Trans3 (disease to death)
  fit3 <- coxph(Surv((death_Date-dis5_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag5_grp)
                +factor(jointiiis) +lone_status
                ,data=data1,method=("breslow"))
  HR3 <-round(summary(fit3)$conf.int[c("factor(jointiiis)1","factor(jointiiis)2","factor(jointiiis)3",
                                       "factor(jointiiis)4","factor(jointiiis)5"),1],3)
  
  gg <-weightedRR6(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

dis5_fem <-LE.dis5(fem,T_fem5)
dis5_mal <-LE.dis5(mal,T_mal5)
dis5 <- cbind(dis5_fem,dis5_mal)
write.csv(dis5,file="Tab3_iso_dis5.csv",quote=F,row.names=T)


#=======LE.dis2-isolation

LE.dis2 <- function(x,y){
  
  #======Prevalence
  #Without disease
  p0 <-table(x$age_grp,x$jointiiis)
  p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
  pp0 <-cbind(p0[,1]/p0[,7],p0[,2]/p0[,7],p0[,3]/p0[,7],p0[,4]/p0[,7],p0[,5]/p0[,7],p0[,6]/p0[,7])
  
  #With disease
  data1 <- subset(x,dis2_icd10_inc==1)
  p1 <- table(data1$age_grp,data1$jointiiis)
  p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
  pp1 <-cbind(p1[,1]/p1[,7],p1[,2]/p1[,7],p1[,3]/p1[,7],p1[,4]/p1[,7],p1[,5]/p1[,7],p1[,6]/p1[,7])
  
  #======HR
  #Trans1 (non-disease to disease)
  fit1 <- coxph(Surv((dis2_date-base_date),dis2_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(jointiiis) +lone_status
                ,data=x,method=("breslow"))
  HR1 <-round(summary(fit1)$conf.int[c("factor(jointiiis)1","factor(jointiiis)2","factor(jointiiis)3",
                                       "factor(jointiiis)4","factor(jointiiis)5"),1],3)
  
  #Trans2 (non-disease to death)
  fit2 <- coxph(Surv((dis2_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                +factor(jointiiis) +lone_status
                ,data=x,method=("breslow"))
  HR2 <-round(summary(fit2)$conf.int[c("factor(jointiiis)1","factor(jointiiis)2","factor(jointiiis)3",
                                       "factor(jointiiis)4","factor(jointiiis)5"),1],3)
  
  #Trans3 (disease to death)
  fit3 <- coxph(Surv((death_Date-dis2_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag2_grp)
                +factor(jointiiis) +lone_status
                ,data=data1,method=("breslow"))
  HR3 <-round(summary(fit3)$conf.int[c("factor(jointiiis)1","factor(jointiiis)2","factor(jointiiis)3",
                                       "factor(jointiiis)4","factor(jointiiis)5"),1],3)
  
  gg <-weightedRR6(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

dis2_fem <-LE.dis2(fem,T_fem2)
dis2_mal <-LE.dis2(mal,T_mal2)
dis2 <- cbind(dis2_fem,dis2_mal)
write.csv(dis2,file="Tab3_iso_dis2.csv",quote=F,row.names=T)


#======= SuppFig6 ===========
#=======Joint association for loneliness

LE.dis7 <- function(x,y,method=1){
  
  if (method==1){
    
    #======Prevalence
    #Without disease
    p0 <-table(x$age_grp,x$jointlll_smk)
    p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
    pp0 <-cbind(p0[,1]/p0[,7],p0[,2]/p0[,7],p0[,3]/p0[,7],p0[,4]/p0[,7],p0[,5]/p0[,7],p0[,6]/p0[,7])
    
    #With disease
    data1 <- subset(x,dis7_icd10_inc==1)
    p1 <- table(data1$age_grp,data1$jointlll_smk)
    p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
    pp1 <-cbind(p1[,1]/p1[,7],p1[,2]/p1[,7],p1[,3]/p1[,7],p1[,4]/p1[,7],p1[,5]/p1[,7],p1[,6]/p1[,7])
    
    #======HR
    #Trans1 (non-disease to disease)
    fit1 <- coxph(Surv((dis7_date-base_date),dis7_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointlll_smk) +iso_status
                  ,data=x,method=("breslow"))
    HR1 <-round(summary(fit1)$conf.int[c("factor(jointlll_smk)1","factor(jointlll_smk)2","factor(jointlll_smk)3",
                                         "factor(jointlll_smk)4","factor(jointlll_smk)5"),1],3)
    
    #Trans2 (non-disease to death)
    fit2 <- coxph(Surv((dis7_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointlll_smk) +iso_status
                  ,data=x,method=("breslow"))
    HR2 <-round(summary(fit2)$conf.int[c("factor(jointlll_smk)1","factor(jointlll_smk)2","factor(jointlll_smk)3",
                                         "factor(jointlll_smk)4","factor(jointlll_smk)5"),1],3)
    
    #Trans3 (disease to death)
    fit3 <- coxph(Surv((death_Date-dis7_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag7_grp)
                  +factor(jointlll_smk) +iso_status
                  ,data=data1,method=("breslow"))
    HR3 <-round(summary(fit3)$conf.int[c("factor(jointlll_smk)1","factor(jointlll_smk)2","factor(jointlll_smk)3",
                                         "factor(jointlll_smk)4","factor(jointlll_smk)5"),1],3)
  }
  
  if (method==2){
    
    #======Prevalence
    #Without disease
    p0 <-table(x$age_grp,x$jointlll_bmi)
    p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
    pp0 <-cbind(p0[,1]/p0[,7],p0[,2]/p0[,7],p0[,3]/p0[,7],p0[,4]/p0[,7],p0[,5]/p0[,7],p0[,6]/p0[,7])
    
    #With disease
    data1 <- subset(x,dis7_icd10_inc==1)
    p1 <- table(data1$age_grp,data1$jointlll_bmi)
    p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
    pp1 <-cbind(p1[,1]/p1[,7],p1[,2]/p1[,7],p1[,3]/p1[,7],p1[,4]/p1[,7],p1[,5]/p1[,7],p1[,6]/p1[,7])
    
    #======HR
    #Trans1 (non-disease to disease)
    fit1 <- coxph(Surv((dis7_date-base_date),dis7_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointlll_bmi) +iso_status
                  ,data=x,method=("breslow"))
    HR1 <-round(summary(fit1)$conf.int[c("factor(jointlll_bmi)1","factor(jointlll_bmi)2","factor(jointlll_bmi)3",
                                         "factor(jointlll_bmi)4","factor(jointlll_bmi)5"),1],3)
    
    #Trans2 (non-disease to death)
    fit2 <- coxph(Surv((dis7_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointlll_bmi) +iso_status
                  ,data=x,method=("breslow"))
    HR2 <-round(summary(fit2)$conf.int[c("factor(jointlll_bmi)1","factor(jointlll_bmi)2","factor(jointlll_bmi)3",
                                         "factor(jointlll_bmi)4","factor(jointlll_bmi)5"),1],3)
    
    #Trans3 (disease to death)
    fit3 <- coxph(Surv((death_Date-dis7_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag7_grp)
                  +factor(jointlll_bmi) +iso_status
                  ,data=data1,method=("breslow"))
    HR3 <-round(summary(fit3)$conf.int[c("factor(jointlll_bmi)1","factor(jointlll_bmi)2","factor(jointlll_bmi)3",
                                         "factor(jointlll_bmi)4","factor(jointlll_bmi)5"),1],3)
  }
  
  if (method==3){
    
    #======Prevalence
    #Without disease
    p0 <-table(x$age_grp,x$jointlll_pa)
    p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
    pp0 <-cbind(p0[,1]/p0[,7],p0[,2]/p0[,7],p0[,3]/p0[,7],p0[,4]/p0[,7],p0[,5]/p0[,7],p0[,6]/p0[,7])
    
    #With disease
    data1 <- subset(x,dis7_icd10_inc==1)
    p1 <- table(data1$age_grp,data1$jointlll_pa)
    p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
    pp1 <-cbind(p1[,1]/p1[,7],p1[,2]/p1[,7],p1[,3]/p1[,7],p1[,4]/p1[,7],p1[,5]/p1[,7],p1[,6]/p1[,7])
    
    #======HR
    #Trans1 (non-disease to disease)
    fit1 <- coxph(Surv((dis7_date-base_date),dis7_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointlll_pa) +iso_status
                  ,data=x,method=("breslow"))
    HR1 <-round(summary(fit1)$conf.int[c("factor(jointlll_pa)1","factor(jointlll_pa)2","factor(jointlll_pa)3",
                                         "factor(jointlll_pa)4","factor(jointlll_pa)5"),1],3)
    
    #Trans2 (non-disease to death)
    fit2 <- coxph(Surv((dis7_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointlll_pa) +iso_status
                  ,data=x,method=("breslow"))
    HR2 <-round(summary(fit2)$conf.int[c("factor(jointlll_pa)1","factor(jointlll_pa)2","factor(jointlll_pa)3",
                                         "factor(jointlll_pa)4","factor(jointlll_pa)5"),1],3)
    
    #Trans3 (disease to death)
    fit3 <- coxph(Surv((death_Date-dis7_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag7_grp)
                  +factor(jointlll_pa) +iso_status
                  ,data=data1,method=("breslow"))
    HR3 <-round(summary(fit3)$conf.int[c("factor(jointlll_pa)1","factor(jointlll_pa)2","factor(jointlll_pa)3",
                                         "factor(jointlll_pa)4","factor(jointlll_pa)5"),1],3)
  }
  
  if (method==4){
    
    #======Prevalence
    #Without disease
    p0 <-table(x$age_grp,x$jointlll_diet)
    p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
    pp0 <-cbind(p0[,1]/p0[,7],p0[,2]/p0[,7],p0[,3]/p0[,7],p0[,4]/p0[,7],p0[,5]/p0[,7],p0[,6]/p0[,7])
    
    #With disease
    data1 <- subset(x,dis7_icd10_inc==1)
    p1 <- table(data1$age_grp,data1$jointlll_diet)
    p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
    pp1 <-cbind(p1[,1]/p1[,7],p1[,2]/p1[,7],p1[,3]/p1[,7],p1[,4]/p1[,7],p1[,5]/p1[,7],p1[,6]/p1[,7])
    
    #======HR
    #Trans1 (non-disease to disease)
    fit1 <- coxph(Surv((dis7_date-base_date),dis7_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointlll_diet) +iso_status
                  ,data=x,method=("breslow"))
    HR1 <-round(summary(fit1)$conf.int[c("factor(jointlll_diet)1","factor(jointlll_diet)2","factor(jointlll_diet)3",
                                         "factor(jointlll_diet)4","factor(jointlll_diet)5"),1],3)
    
    #Trans2 (non-disease to death)
    fit2 <- coxph(Surv((dis7_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointlll_diet) +iso_status
                  ,data=x,method=("breslow"))
    HR2 <-round(summary(fit2)$conf.int[c("factor(jointlll_diet)1","factor(jointlll_diet)2","factor(jointlll_diet)3",
                                         "factor(jointlll_diet)4","factor(jointlll_diet)5"),1],3)
    
    #Trans3 (disease to death)
    fit3 <- coxph(Surv((death_Date-dis7_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag7_grp)
                  +factor(jointlll_diet) +iso_status
                  ,data=data1,method=("breslow"))
    HR3 <-round(summary(fit3)$conf.int[c("factor(jointlll_diet)1","factor(jointlll_diet)2","factor(jointlll_diet)3",
                                         "factor(jointlll_diet)4","factor(jointlll_diet)5"),1],3)
  }
  
  if (method==5){
    
    #======Prevalence
    #Without disease
    p0 <-table(x$age_grp,x$jointlll_slp)
    p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
    pp0 <-cbind(p0[,1]/p0[,7],p0[,2]/p0[,7],p0[,3]/p0[,7],p0[,4]/p0[,7],p0[,5]/p0[,7],p0[,6]/p0[,7])
    
    #With disease
    data1 <- subset(x,dis7_icd10_inc==1)
    p1 <- table(data1$age_grp,data1$jointlll_slp)
    p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
    pp1 <-cbind(p1[,1]/p1[,7],p1[,2]/p1[,7],p1[,3]/p1[,7],p1[,4]/p1[,7],p1[,5]/p1[,7],p1[,6]/p1[,7])
    
    #======HR
    #Trans1 (non-disease to disease)
    fit1 <- coxph(Surv((dis7_date-base_date),dis7_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointlll_slp) +iso_status
                  ,data=x,method=("breslow"))
    HR1 <-round(summary(fit1)$conf.int[c("factor(jointlll_slp)1","factor(jointlll_slp)2","factor(jointlll_slp)3",
                                         "factor(jointlll_slp)4","factor(jointlll_slp)5"),1],3)
    
    #Trans2 (non-disease to death)
    fit2 <- coxph(Surv((dis7_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointlll_slp) +iso_status
                  ,data=x,method=("breslow"))
    HR2 <-round(summary(fit2)$conf.int[c("factor(jointlll_slp)1","factor(jointlll_slp)2","factor(jointlll_slp)3",
                                         "factor(jointlll_slp)4","factor(jointlll_slp)5"),1],3)
    
    #Trans3 (disease to death)
    fit3 <- coxph(Surv((death_Date-dis7_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag7_grp)
                  +factor(jointlll_slp) +iso_status
                  ,data=data1,method=("breslow"))
    HR3 <-round(summary(fit3)$conf.int[c("factor(jointlll_slp)1","factor(jointlll_slp)2","factor(jointlll_slp)3",
                                         "factor(jointlll_slp)4","factor(jointlll_slp)5"),1],3)
  }
  
  
  gg <-weightedRR6(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

dis7_fem_smk <-LE.dis7(fem,T_fem7,method=1)
dis7_mal_smk <-LE.dis7(mal,T_mal7,method=1)
dis7_smk <- cbind(dis7_fem_smk,dis7_mal_smk)
write.csv(dis7_smk,file="SuppFig6_lone_smk.csv",quote=F,row.names=T)

dis7_fem_bmi <-LE.dis7(fem,T_fem7,method=2)
dis7_mal_bmi <-LE.dis7(mal,T_mal7,method=2)
dis7_bmi <- cbind(dis7_fem_bmi,dis7_mal_bmi)
write.csv(dis7_bmi,file="SuppFig6_lone_bmi.csv",quote=F,row.names=T)

dis7_fem_pa <-LE.dis7(fem,T_fem7,method=3)
dis7_mal_pa <-LE.dis7(mal,T_mal7,method=3)
dis7_pa <- cbind(dis7_fem_pa,dis7_mal_pa)
write.csv(dis7_pa,file="SuppFig6_lone_pa.csv",quote=F,row.names=T)

dis7_fem_diet <-LE.dis7(fem,T_fem7,method=4)
dis7_mal_diet <-LE.dis7(mal,T_mal7,method=4)
dis7_diet <- cbind(dis7_fem_diet,dis7_mal_diet)
write.csv(dis7_diet,file="SuppFig6_lone_diet.csv",quote=F,row.names=T)

dis7_fem_slp <-LE.dis7(fem,T_fem7,method=5)
dis7_mal_slp <-LE.dis7(mal,T_mal7,method=5)
dis7_slp <- cbind(dis7_fem_slp,dis7_mal_slp)
write.csv(dis7_slp,file="SuppFig6_lone_slp.csv",quote=F,row.names=T)


#=======Joint association for isolation

LE.dis7 <- function(x,y,method=1){
  
  if (method==1){
    
    #======Prevalence
    #Without disease
    p0 <-table(x$age_grp,x$jointiii_smk)
    p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
    pp0 <-cbind(p0[,1]/p0[,7],p0[,2]/p0[,7],p0[,3]/p0[,7],p0[,4]/p0[,7],p0[,5]/p0[,7],p0[,6]/p0[,7])
    
    #With disease
    data1 <- subset(x,dis7_icd10_inc==1)
    p1 <- table(data1$age_grp,data1$jointiii_smk)
    p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
    pp1 <-cbind(p1[,1]/p1[,7],p1[,2]/p1[,7],p1[,3]/p1[,7],p1[,4]/p1[,7],p1[,5]/p1[,7],p1[,6]/p1[,7])
    
    #======HR
    #Trans1 (non-disease to disease)
    fit1 <- coxph(Surv((dis7_date-base_date),dis7_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointiii_smk) +lone_status
                  ,data=x,method=("breslow"))
    HR1 <-round(summary(fit1)$conf.int[c("factor(jointiii_smk)1","factor(jointiii_smk)2","factor(jointiii_smk)3",
                                         "factor(jointiii_smk)4","factor(jointiii_smk)5"),1],3)
    
    #Trans2 (non-disease to death)
    fit2 <- coxph(Surv((dis7_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointiii_smk) +lone_status
                  ,data=x,method=("breslow"))
    HR2 <-round(summary(fit2)$conf.int[c("factor(jointiii_smk)1","factor(jointiii_smk)2","factor(jointiii_smk)3",
                                         "factor(jointiii_smk)4","factor(jointiii_smk)5"),1],3)
    
    #Trans3 (disease to death)
    fit3 <- coxph(Surv((death_Date-dis7_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag7_grp)
                  +factor(jointiii_smk) +lone_status
                  ,data=data1,method=("breslow"))
    HR3 <-round(summary(fit3)$conf.int[c("factor(jointiii_smk)1","factor(jointiii_smk)2","factor(jointiii_smk)3",
                                         "factor(jointiii_smk)4","factor(jointiii_smk)5"),1],3)
  }
  
  if (method==2){
    
    #======Prevalence
    #Without disease
    p0 <-table(x$age_grp,x$jointiii_bmi)
    p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
    pp0 <-cbind(p0[,1]/p0[,7],p0[,2]/p0[,7],p0[,3]/p0[,7],p0[,4]/p0[,7],p0[,5]/p0[,7],p0[,6]/p0[,7])
    
    #With disease
    data1 <- subset(x,dis7_icd10_inc==1)
    p1 <- table(data1$age_grp,data1$jointiii_bmi)
    p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
    pp1 <-cbind(p1[,1]/p1[,7],p1[,2]/p1[,7],p1[,3]/p1[,7],p1[,4]/p1[,7],p1[,5]/p1[,7],p1[,6]/p1[,7])
    
    #======HR
    #Trans1 (non-disease to disease)
    fit1 <- coxph(Surv((dis7_date-base_date),dis7_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointiii_bmi) +lone_status
                  ,data=x,method=("breslow"))
    HR1 <-round(summary(fit1)$conf.int[c("factor(jointiii_bmi)1","factor(jointiii_bmi)2","factor(jointiii_bmi)3",
                                         "factor(jointiii_bmi)4","factor(jointiii_bmi)5"),1],3)
    
    #Trans2 (non-disease to death)
    fit2 <- coxph(Surv((dis7_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointiii_bmi) +lone_status
                  ,data=x,method=("breslow"))
    HR2 <-round(summary(fit2)$conf.int[c("factor(jointiii_bmi)1","factor(jointiii_bmi)2","factor(jointiii_bmi)3",
                                         "factor(jointiii_bmi)4","factor(jointiii_bmi)5"),1],3)
    
    #Trans3 (disease to death)
    fit3 <- coxph(Surv((death_Date-dis7_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag7_grp)
                  +factor(jointiii_bmi) +lone_status
                  ,data=data1,method=("breslow"))
    HR3 <-round(summary(fit3)$conf.int[c("factor(jointiii_bmi)1","factor(jointiii_bmi)2","factor(jointiii_bmi)3",
                                         "factor(jointiii_bmi)4","factor(jointiii_bmi)5"),1],3)
  }
  
  if (method==3){
    
    #======Prevalence
    #Without disease
    p0 <-table(x$age_grp,x$jointiii_pa)
    p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
    pp0 <-cbind(p0[,1]/p0[,7],p0[,2]/p0[,7],p0[,3]/p0[,7],p0[,4]/p0[,7],p0[,5]/p0[,7],p0[,6]/p0[,7])
    
    #With disease
    data1 <- subset(x,dis7_icd10_inc==1)
    p1 <- table(data1$age_grp,data1$jointiii_pa)
    p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
    pp1 <-cbind(p1[,1]/p1[,7],p1[,2]/p1[,7],p1[,3]/p1[,7],p1[,4]/p1[,7],p1[,5]/p1[,7],p1[,6]/p1[,7])
    
    #======HR
    #Trans1 (non-disease to disease)
    fit1 <- coxph(Surv((dis7_date-base_date),dis7_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointiii_pa) +lone_status
                  ,data=x,method=("breslow"))
    HR1 <-round(summary(fit1)$conf.int[c("factor(jointiii_pa)1","factor(jointiii_pa)2","factor(jointiii_pa)3",
                                         "factor(jointiii_pa)4","factor(jointiii_pa)5"),1],3)
    
    #Trans2 (non-disease to death)
    fit2 <- coxph(Surv((dis7_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointiii_pa) +lone_status
                  ,data=x,method=("breslow"))
    HR2 <-round(summary(fit2)$conf.int[c("factor(jointiii_pa)1","factor(jointiii_pa)2","factor(jointiii_pa)3",
                                         "factor(jointiii_pa)4","factor(jointiii_pa)5"),1],3)
    
    #Trans3 (disease to death)
    fit3 <- coxph(Surv((death_Date-dis7_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag7_grp)
                  +factor(jointiii_pa) +lone_status
                  ,data=data1,method=("breslow"))
    HR3 <-round(summary(fit3)$conf.int[c("factor(jointiii_pa)1","factor(jointiii_pa)2","factor(jointiii_pa)3",
                                         "factor(jointiii_pa)4","factor(jointiii_pa)5"),1],3)
  }
  
  if (method==4){
    
    #======Prevalence
    #Without disease
    p0 <-table(x$age_grp,x$jointiii_diet)
    p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
    pp0 <-cbind(p0[,1]/p0[,7],p0[,2]/p0[,7],p0[,3]/p0[,7],p0[,4]/p0[,7],p0[,5]/p0[,7],p0[,6]/p0[,7])
    
    #With disease
    data1 <- subset(x,dis7_icd10_inc==1)
    p1 <- table(data1$age_grp,data1$jointiii_diet)
    p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
    pp1 <-cbind(p1[,1]/p1[,7],p1[,2]/p1[,7],p1[,3]/p1[,7],p1[,4]/p1[,7],p1[,5]/p1[,7],p1[,6]/p1[,7])
    
    #======HR
    #Trans1 (non-disease to disease)
    fit1 <- coxph(Surv((dis7_date-base_date),dis7_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointiii_diet) +lone_status
                  ,data=x,method=("breslow"))
    HR1 <-round(summary(fit1)$conf.int[c("factor(jointiii_diet)1","factor(jointiii_diet)2","factor(jointiii_diet)3",
                                         "factor(jointiii_diet)4","factor(jointiii_diet)5"),1],3)
    
    #Trans2 (non-disease to death)
    fit2 <- coxph(Surv((dis7_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointiii_diet) +lone_status
                  ,data=x,method=("breslow"))
    HR2 <-round(summary(fit2)$conf.int[c("factor(jointiii_diet)1","factor(jointiii_diet)2","factor(jointiii_diet)3",
                                         "factor(jointiii_diet)4","factor(jointiii_diet)5"),1],3)
    
    #Trans3 (disease to death)
    fit3 <- coxph(Surv((death_Date-dis7_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag7_grp)
                  +factor(jointiii_diet) +lone_status
                  ,data=data1,method=("breslow"))
    HR3 <-round(summary(fit3)$conf.int[c("factor(jointiii_diet)1","factor(jointiii_diet)2","factor(jointiii_diet)3",
                                         "factor(jointiii_diet)4","factor(jointiii_diet)5"),1],3)
  }
  
  if (method==5){
    
    #======Prevalence
    #Without disease
    p0 <-table(x$age_grp,x$jointiii_slp)
    p0 <-cbind(p0,c(sum(p0[1,]),sum(p0[2,]),sum(p0[3,]),sum(p0[4,])))
    pp0 <-cbind(p0[,1]/p0[,7],p0[,2]/p0[,7],p0[,3]/p0[,7],p0[,4]/p0[,7],p0[,5]/p0[,7],p0[,6]/p0[,7])
    
    #With disease
    data1 <- subset(x,dis7_icd10_inc==1)
    p1 <- table(data1$age_grp,data1$jointiii_slp)
    p1 <-cbind(p1,c(sum(p1[1,]),sum(p1[2,]),sum(p1[3,]),sum(p1[4,])))
    pp1 <-cbind(p1[,1]/p1[,7],p1[,2]/p1[,7],p1[,3]/p1[,7],p1[,4]/p1[,7],p1[,5]/p1[,7],p1[,6]/p1[,7])
    
    #======HR
    #Trans1 (non-disease to disease)
    fit1 <- coxph(Surv((dis7_date-base_date),dis7_icd10_inc) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointiii_slp) +lone_status
                  ,data=x,method=("breslow"))
    HR1 <-round(summary(fit1)$conf.int[c("factor(jointiii_slp)1","factor(jointiii_slp)2","factor(jointiii_slp)3",
                                         "factor(jointiii_slp)4","factor(jointiii_slp)5"),1],3)
    
    #Trans2 (non-disease to death)
    fit2 <- coxph(Surv((dis7_date-base_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)
                  +factor(jointiii_slp) +lone_status
                  ,data=x,method=("breslow"))
    HR2 <-round(summary(fit2)$conf.int[c("factor(jointiii_slp)1","factor(jointiii_slp)2","factor(jointiii_slp)3",
                                         "factor(jointiii_slp)4","factor(jointiii_slp)5"),1],3)
    
    #Trans3 (disease to death)
    fit3 <- coxph(Surv((death_Date-dis7_date),death) ~ age +factor(white)  +Townsend_Index +factor(edu_group)+factor(income)+factor(agediag7_grp)
                  +factor(jointiii_slp) +lone_status
                  ,data=data1,method=("breslow"))
    HR3 <-round(summary(fit3)$conf.int[c("factor(jointiii_slp)1","factor(jointiii_slp)2","factor(jointiii_slp)3",
                                         "factor(jointiii_slp)4","factor(jointiii_slp)5"),1],3)
  }
  
  
  gg <-weightedRR6(pp0,pp1,HR1,HR2,HR3,y)
  return(gg)
}

dis7_fem_smk <-LE.dis7(fem,T_fem7,method=1)
dis7_mal_smk <-LE.dis7(mal,T_mal7,method=1)
dis7_smk <- cbind(dis7_fem_smk,dis7_mal_smk)
write.csv(dis7_smk,file="SuppFig6_iso_smk.csv",quote=F,row.names=T)

dis7_fem_bmi <-LE.dis7(fem,T_fem7,method=2)
dis7_mal_bmi <-LE.dis7(mal,T_mal7,method=2)
dis7_bmi <- cbind(dis7_fem_bmi,dis7_mal_bmi)
write.csv(dis7_bmi,file="SuppFig6_iso_bmi.csv",quote=F,row.names=T)

dis7_fem_pa <-LE.dis7(fem,T_fem7,method=3)
dis7_mal_pa <-LE.dis7(mal,T_mal7,method=3)
dis7_pa <- cbind(dis7_fem_pa,dis7_mal_pa)
write.csv(dis7_pa,file="SuppFig6_iso_pa.csv",quote=F,row.names=T)

dis7_fem_diet <-LE.dis7(fem,T_fem7,method=4)
dis7_mal_diet <-LE.dis7(mal,T_mal7,method=4)
dis7_diet <- cbind(dis7_fem_diet,dis7_mal_diet)
write.csv(dis7_diet,file="SuppFig6_iso_diet.csv",quote=F,row.names=T)

dis7_fem_slp <-LE.dis7(fem,T_fem7,method=5)
dis7_mal_slp <-LE.dis7(mal,T_mal7,method=5)
dis7_slp <- cbind(dis7_fem_slp,dis7_mal_slp)
write.csv(dis7_slp,file="SuppFig6_iso_slp.csv",quote=F,row.names=T)


