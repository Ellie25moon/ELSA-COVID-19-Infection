#######################################################################################
######################### MISSING DATA IMPUTATION #####################################
#######################################################################################

setwd("~/Desktop/ELSA COVID19 Study/Study 2_Covid infection")

library("survey")
library("ggplot2")
library(RColorBrewer)
library(scales)
library("viridis")  
library(ggpubr)
library(readxl)
library(readstata13)
library(arsenal)
library(writexl)
library(foreign)

maindata=read.dta13('maindata_study2.dta') 

names(maindata)
summary(maindata)

## Recode sex and symptom variables to numeric, 0-1
v<-c("sex", "cvsymp01", "cvsymp02", "cvsymp03", "cvsymp04","cvsymp05","cvsymp06", "cvsymp07",
     "cvsymp08")

names(v)<-v

y=NULL
for (i in v) { 
  maindata[,i]<- as.numeric(droplevels(as.factor(maindata[,i])))
  maindata[,i] <- maindata[,i] -1
  y<-append(y, table(maindata[,i], exclude = F))
  
}

y

summary(maindata)

## Recode precovid finsit and social connection variables  to numeric, 1-4/5
v<-c("fins_precov", "cvfamcon_cvfamcon1_q_w10","cvfamcon_cvfamcon2_q_w10",
     "cvfamcon_cvfamcon3_q_w10", "cvfamcon_cvfamcon4_q_w10", "cvfrdcon_cvfrdcon1_q_w10" , 
     "cvfrdcon_cvfrdcon2_q_w10", "cvfrdcon_cvfrdcon3_q_w10", "cvfrdcon_cvfrdcon4_q_w10" )

names(v)<-v

y=NULL
for (i in v) { 
  maindata[,i]<- as.numeric(droplevels(as.factor(maindata[,i])))
  y<-append(y, table(maindata[,i], exclude = F))
  
}

y

summary(maindata)
str(maindata)

#remove attributes
attr(maindata, "expansion.fields") <- NULL

#rename vars with invalid names
library(janitor)
maindata <- maindata %>% clean_names()
names(maindata)

# select analytical sample only 
maindata_ana<-maindata[maindata$anasample==1,]

## imputation model 
library("mice")

init = mice(maindata_ana, maxit=0) 
init$loggedEvents
pred_mat <- init$pred#prediction matrix
pred_mat[,c("merge5", "anasample", "merge6")] <- 0
meth <- init$meth#formula to be used

#meth["cov19lwgtw2b"] <- "" #don't impute long weight

length(meth);dim(pred_mat)

imp.mi = mice(maindata_ana, method =meth, predictorMatrix = pred_mat,  diagnostics =T, m=20, maxit=5, set.seed(25)) 

colSums(is.na(complete(imp.mi)))#check number of missing data 
imp.mi$loggedEvents #check logged events

#create long dataframe
maindata_s2_imp<-mice::complete(imp.mi, "long", include = TRUE)

table(maindata_s2_imp$.imp)

save.image("imputation.study2_rev.RData")


# Export mi imputed long dataset to stata and derive variables 
library(foreign)
write.dta(maindata_s2_imp, "maindata_s2_imp_rev.dta")

#######################################################################################
######################### DESCRIPTIVE ANALYSES ########################################
#######################################################################################


## Sample characteristics at covid w1
maindata=read.dta13('maindata_s2_imp.dta') 

summary(maindata[,1:60])
summary(maindata[,61:120])

# transform to factor 
fac<-c("sex", "cvvuln", "heill_updated", "work", "ethnicity2",
       "partnership2_covid", "fins_worried_w10", "fins_worse_w10",
       "fins_worried_w11", "fins_worse_w11","cesd8_binary_w10", 
       "cesd8_binary_w11", "cesd8_binary_w9", "gad7_binary_w10",
       "gad7_binary_w11", "wealth_ter", "income_ter",
       "isolation_binary_w9", "frcont_fam_w10", "frcont_fri_w10", 
       "frcontreal_fam_w10", "frcontreal_fri_w10", "frcontreal_all_w10",
       "frcont_fam_w11", "frcontreal_fam_w11", "frcont_fri_w11", 
       "frcontreal_fri_w11", "frcontreal_all_w11", "covcase1", 
       "covcase2", "covcase3", "cov_w11", "agegr3")

maindata[,fac] <- lapply(maindata[,fac] , factor)
str(maindata)

maindata$imputed <- as.factor(ifelse(maindata$`_imp` > 0, 1,0))
table (maindata$impute, exclude = F)


#living alone 
table(maindata$cvnump, exclude =F)

maindata$alone<- maindata$cvnump
maindata$alone[maindata$cvnump >1]<-0
table(maindata$alone) #1=live alone
maindata$alone<-as.factor(maindata$alone)

save(maindata, file="maindata")
load("maindata")

## Table 1: Sample characteristics at COVID-19 wave 1

# weighted 
t1.weighted<- tableby( imputed ~ # covariates 
                         sex + includeNA(is.na(sex)) + agegr3 + includeNA(is.na(agegr3)) + 
                         ethnicity2 + includeNA(is.na(ethnicity2)) +  
                         partnership2_covid + includeNA(is.na(partnership2_covid)) + 
                         alone + includeNA(is.na(alone)) + 
                         education + includeNA(is.na(education)) +  
                         work + includeNA(is.na(work)) + 
                         wealth_ter + includeNA(is.na( wealth_ter)) +
                         hometenure + includeNA(is.na(hometenure)) + 
                         heill_updated + includeNA(is.na(heill_updated)) + 
                         cvvuln + includeNA(is.na(cvvuln)) + 
                         # covid infection definitions
                         covcase1 + includeNA(is.na(covcase1)) +
                         covcase2 + includeNA(is.na(covcase2)) +
                         covcase3 + includeNA(is.na(covcase3)) +
                         # outcomes w1
                         cesd8_binary_w10 + includeNA(is.na(cesd8_binary_w10)) +
                         gad7_binary_w10 + includeNA(is.na(gad7_binary_w10)) +
                         casp_tot_w10 + includeNA(is.na(casp_tot_w10)) +
                         cv_loneliness_w10 + includeNA(is.na(cv_loneliness_w10)) + 
                         fins_worried_w10 + includeNA(is.na(fins_worried_w10)) +
                         fins_worse_w10 + includeNA(is.na(fins_worse_w10)) + 
                         frcont_fam_w10 +     includeNA(is.na(frcont_fam_w10)) +
                         frcont_fri_w10 +     includeNA(is.na(frcont_fri_w10)) +
                         frcontreal_fam_w10 +    includeNA(is.na(frcontreal_fam_w10)) +
                         frcontreal_fri_w10 +   includeNA(is.na(frcontreal_fri_w10)) +
                         frcontreal_all_w10 +   includeNA(is.na(frcontreal_all_w10)) +
                         frcont_tot_w10     +   includeNA(is.na(frcont_tot_w10 )) , 
                       data=maindata, 
                       weights=cov19lwgtw2b)

t1.weighted<-as.data.frame(summary(t1.weighted))
write_xlsx(list(t1.weighted = t1.weighted), "table1_updated.xlsx")


## long dataset for descriptive analysis of changes in the outcomes 

#Reshape to long 

varying<- c("cesd8_binary_w10" ,
            "gad7_binary_w10" ,
            "casp_tot_w10" ,
            "cv_loneliness_w10" ,
            "fins_worried_w10" ,
            "fins_worse_w10" ,
            "frcont_fam_w10"   ,
            "frcont_fri_w10"    ,
            "frcontreal_fam_w10",
            "frcontreal_fri_w10" ,
            "frcontreal_all_w10" ,
            "frcont_tot_w10"   ,
            "cesd8_binary_w11",
            "gad7_binary_w11",
            "casp_tot_w11",
            "cv_loneliness_w11",
            "fins_worried_w11",
            "fins_worse_w11",
            "frcont_fam_w11"  ,
            "frcont_fri_w11"  ,
            "frcontreal_fam_w11",
            "frcontreal_fri_w11",
            "frcontreal_all_w11",
            "frcont_tot_w11"  )

maindata$impn<-maindata$`_imp`

maindata.long<- reshape(data = maindata, varying = varying, timevar = "wave", sep = "_w1", idvar = c("idauniq","impn"), direction = "long") #reshaping from wide to long 

summary(maindata.long)

maindata.long$wave<- as.factor(maindata.long$wave)

save (maindata.long, file="maindata.long")

## Table 2: outcomes at covid w1 and w2
t2.weighted<- tableby( wave ~ 
                         # outcomes w1
                         cesd8_binary  + 
                         gad7_binary  + 
                         casp_tot  + 
                         cv_loneliness  + 
                         fins_worried  + 
                         fins_worse  + 
                         frcont_fam  +    
                         frcont_fri  +    
                         frcont_tot    +
                         frcontreal_fam  +   
                         frcontreal_fri  +  
                         frcontreal_all  ,
                       data=maindata.long[maindata.long$impn>0,], 
                       weights=cov19lwgtw2b)

t2.weighted<-as.data.frame(summary(t2.weighted))


# export tables to Excel 
write_xlsx(list(t1.weighted = t1.weighted, 
                t2.weighted = t2.weighted), "tables.desc.study2.xlsx")

# Export mi imputed long dataset to stata
write.dta(maindata.long, 'maindata_s2_imp_long.dta')


#### descriptive stats by covid infection 

## Table 1: sample characteristics 
## imputed 
t1.imputed<- tableby( covcase1 ~ # covariates 
                        sex + includeNA(is.na(sex)) + agegr3 + includeNA(is.na(agegr3)) + 
                        ethnicity2 + includeNA(is.na(ethnicity2)) +  
                        partnership2_covid + includeNA(is.na(partnership2_covid)) + 
                        alone + includeNA(is.na(alone)) + 
                        education + includeNA(is.na(education)) +  
                        work + includeNA(is.na(work)) + 
                        wealth_ter + includeNA(is.na( wealth_ter)) +
                        hometenure + includeNA(is.na(hometenure)) + 
                        heill_updated + includeNA(is.na(heill_updated)) + 
                        cvvuln + includeNA(is.na(cvvuln)) + 
                        # outcomes w1
                        cesd8_binary_w10 + includeNA(is.na(cesd8_binary_w10)) +
                        gad7_binary_w10 + includeNA(is.na(gad7_binary_w10)) +
                        casp_tot_w10 + includeNA(is.na(casp_tot_w10)) +
                        cv_loneliness_w10 + includeNA(is.na(cv_loneliness_w10)) + 
                        fins_worried_w10 + includeNA(is.na(fins_worried_w10)) +
                        fins_worse_w10 + includeNA(is.na(fins_worse_w10)) + 
                        frcont_fam_w10 +     includeNA(is.na(frcont_fam_w10)) +
                        frcont_fri_w10 +     includeNA(is.na(frcont_fri_w10)) +
                        frcont_tot_w10     +   includeNA(is.na(frcont_tot_w10 )) +
                        frcontreal_fam_w10 +    includeNA(is.na(frcontreal_fam_w10)) +
                        frcontreal_fri_w10 +   includeNA(is.na(frcontreal_fri_w10)) , 
                      data=maindata[maindata$impn>0,], 
                      weights=cov19lwgtw2b)

t1.imputed<-as.data.frame(summary(t1.imputed))

## observed
t1.observed<- tableby( covcase1 ~ # covariates 
                         sex + includeNA(is.na(sex)) + agegr3 + includeNA(is.na(agegr3)) + 
                         ethnicity2 + includeNA(is.na(ethnicity2)) +  
                         partnership2_covid + includeNA(is.na(partnership2_covid)) + 
                         alone + includeNA(is.na(alone)) + 
                         education + includeNA(is.na(education)) +  
                         work + includeNA(is.na(work)) + 
                         wealth_ter + includeNA(is.na( wealth_ter)) +
                         hometenure + includeNA(is.na(hometenure)) + 
                         heill_updated + includeNA(is.na(heill_updated)) + 
                         cvvuln + includeNA(is.na(cvvuln)) + 
                         # outcomes w1
                         cesd8_binary_w10 + includeNA(is.na(cesd8_binary_w10)) +
                         gad7_binary_w10 + includeNA(is.na(gad7_binary_w10)) +
                         casp_tot_w10 + includeNA(is.na(casp_tot_w10)) +
                         cv_loneliness_w10 + includeNA(is.na(cv_loneliness_w10)) + 
                         fins_worried_w10 + includeNA(is.na(fins_worried_w10)) +
                         fins_worse_w10 + includeNA(is.na(fins_worse_w10)) + 
                         frcont_fam_w10 +     includeNA(is.na(frcont_fam_w10)) +
                         frcont_fri_w10 +     includeNA(is.na(frcont_fri_w10)) +
                         frcont_tot_w10     +   includeNA(is.na(frcont_tot_w10 )) +
                         frcontreal_fam_w10 +    includeNA(is.na(frcontreal_fam_w10)) +
                         frcontreal_fri_w10 +   includeNA(is.na(frcontreal_fri_w10)) +
                         frcontreal_all_w10 +   includeNA(is.na(frcontreal_all_w10)) , 
                       data=maindata[maindata$impn==0,], 
                       weights=cov19lwgtw2b)

t1.observed<-as.data.frame(summary(t1.observed))

### Table 2: outcomes by waves 
load("maindata.long")
t2.nocase<- tableby( wave ~ 
                       # outcomes w1
                       cesd8_binary  + 
                       gad7_binary  + 
                       casp_tot  + 
                       cv_loneliness  + 
                       fins_worried  + 
                       fins_worse  + 
                       frcont_fam  +    
                       frcont_fri  +    
                       frcont_tot    +
                       frcontreal_fam  +   
                       frcontreal_fri  +  
                       frcontreal_all  ,
                     data=maindata.long[maindata.long$impn>0 &maindata.long$covcase1==0,], 
                     weights=cov19lwgtw2b)

t2.nocase<-as.data.frame(summary(t2.nocase))

t2.case<- tableby( wave ~ 
                     # outcomes w1
                     cesd8_binary  + 
                     gad7_binary  + 
                     casp_tot  + 
                     cv_loneliness  + 
                     fins_worried  + 
                     fins_worse  + 
                     frcont_fam  +    
                     frcont_fri  +    
                     frcont_tot    +
                     frcontreal_fam  +   
                     frcontreal_fri  +  
                     frcontreal_all  ,
                   data=maindata.long[maindata.long$impn>0 &maindata.long$covcase1==1,], 
                   weights=cov19lwgtw2b)

t2.case<-as.data.frame(summary(t2.case))


write_xlsx(list(t1.imputed = t1.imputed, 
                t1.observed = t1.observed,
                t2.nocase = t2.nocase,
                t2.case = t2.case), "tables.desc.study2_bycovcase1.xlsx")

