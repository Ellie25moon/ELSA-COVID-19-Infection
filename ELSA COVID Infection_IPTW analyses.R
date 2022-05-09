
setwd ("C:/Users/rmjleio/iCloudDrive/Desktop/ELSA COVID19 Study/Study 2")

library(cobalt) 
library(readstata13)
library(mice)
library(MatchThem)
library(survey)
library(dbarts)
library(writexl)
library(ggpubr)

maindata=read.dta13('maindata_s2_imp.dta') 
colnames(maindata)[1]<-".imp"
colnames(maindata)[2]<-".id"

names(maindata)
str(maindata)


###1) Depression 
dep<- maindata[,c(".imp", ".id", "idauniq", "cesd8_binary_w10", "cesd8_binary_w11", "sex", "agegr3", "cesd8_binary_w9", 
                  "cvnump_cat", "work", "wealth_ter", "cvvuln", "heill_updated", "cov19lwgtw2b", "covcase1")]

dep.datasets<- as.mids(dep)


## weighting on imputed datasets 

weighted.datasets <- weightthem(covcase1 ~ sex + as.factor(agegr3) + cesd8_binary_w9 +
                  cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, 
                                dep.datasets, approach = 'within', method = 'ps', estimand = 'ATM')
summary(weighted.datasets, n=1)

## balance 

#a)love plot 

#names 
b1 <- bal.tab(covcase1 ~ sex + as.factor(agegr3) + cesd8_binary_w9 +
                cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, data = dep.datasets,
              int = F)
v1 <- var.names(b1, type = "vec")
v1["cesd8_binary_w9"] <- "Depression (wave 9/8)"
v1["sex"] <- "Sex"
v1["as.factor(agegr3)_52"] <- "Age: 52-59yrs"
v1["as.factor(agegr3)_60"] <- "Age: 60-74yrs"
v1["as.factor(agegr3)_75"] <- "Age: 75+yrs"
v1["cvnump_cat"] <- "Living alone"
v1["as.factor(work)_1"] <- "Work: Employed"
v1["as.factor(work)_2"] <- "Work: Retired"
v1["as.factor(work)_3"] <- "Work: Other not working"
v1["as.factor(wealth_ter)_1"] <- "Wealth: low"
v1["as.factor(wealth_ter)_2"] <- "Wealth: medium"
v1["as.factor(wealth_ter)_3"] <- "Wealth: high"
v1["cvvuln"] <- "Vulnerable to COVID-19"
v1["heill_updated"] <- "Limiting longstanding illness "
v1["cov19lwgtw2b"] <- "ELSA COVID-19 weight"


#create plot 
plot.dep<- love.plot(weighted.datasets, var.names = v1, title = "Depression") 
plot.dep

#b) table of absolute differences 
balance.dep<-bal.tab(weighted.datasets, abs = TRUE)
balance.dep<- balance.dep[[2]]
balance.dep$outcome<- "dep"


## run regressions 

#w1
weighted.models <- with(data = weighted.datasets, expr = svyglm(cesd8_binary_w10 ~ covcase1, family = binomial))

#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

dep_w1<- summary(weighted.results, conf.int = TRUE)
dep_w1$outcome<- "dep_w1"


#w2
weighted.models <- with(data = weighted.datasets, expr = svyglm(cesd8_binary_w11 ~ covcase1, family = binomial))

#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

dep_w2<- summary(weighted.results, conf.int = TRUE)
dep_w2$outcome<- "dep_w2"


### 2) Anxiety 
anx<- maindata[,c(".imp", ".id", "idauniq", "gad7_binary_w10", "gad7_binary_w11", "sex", "agegr3", "scovan_w9", 
                  "cvnump_cat", "work", "wealth_ter", "cvvuln", "heill_updated", "cov19lwgtw2b", "covcase1")]

anx.datasets<- as.mids(anx)


## weighting on imputed datasets 

weighted.datasets <- weightthem(covcase1 ~ sex + as.factor(agegr3) + scovan_w9 +
                                  cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, 
                                anx.datasets, approach = 'within', method = 'ps', estimand = 'ATM')
summary(weighted.datasets)

#a)love plot 

#names 
b1 <- bal.tab(covcase1 ~ sex + as.factor(agegr3) + scovan_w9 +
                cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, data = anx.datasets,
              int = F)
v1 <- var.names(b1, type = "vec")
v1["scovan_w9"] <- "Anxiety (wave 9/8)"
v1["sex"] <- "Sex"
v1["as.factor(agegr3)_52"] <- "Age: 52-59yrs"
v1["as.factor(agegr3)_60"] <- "Age: 60-74yrs"
v1["as.factor(agegr3)_75"] <- "Age: 75+yrs"
v1["cvnump_cat"] <- "Living alone"
v1["as.factor(work)_1"] <- "Work: Employed"
v1["as.factor(work)_2"] <- "Work: Retired"
v1["as.factor(work)_3"] <- "Work: Other not working"
v1["as.factor(wealth_ter)_1"] <- "Wealth: low"
v1["as.factor(wealth_ter)_2"] <- "Wealth: medium"
v1["as.factor(wealth_ter)_3"] <- "Wealth: high"
v1["cvvuln"] <- "Vulnerable to COVID-19"
v1["heill_updated"] <- "Limiting longstanding illness "
v1["cov19lwgtw2b"] <- "ELSA COVID-19 weight"


#create plot 
plot.anx<- love.plot(weighted.datasets, var.names = v1, title = "Anxiety") 
plot.anx

#b) table of absolute differences 
balance.anx<-bal.tab(weighted.datasets, abs = TRUE)
balance.anx<- balance.anx[[2]]
balance.anx$outcome<- "anx"


## run regressions 

#w1
weighted.models <- with(data = weighted.datasets, expr = svyglm(gad7_binary_w10 ~ covcase1, family = binomial))

#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

anx_w1<- summary(weighted.results, conf.int = TRUE)
anx_w1$outcome<- "anx_w1"


#w2
weighted.models <- with(data = weighted.datasets, expr = svyglm(gad7_binary_w11 ~ covcase1, family = binomial))

#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

anx_w2<- summary(weighted.results, conf.int = TRUE)
anx_w2$outcome<- "anx_w2"

###3) QoL
qol<- maindata[,c(".imp", ".id", "idauniq", "casp_tot_w10", "casp_tot_w11", "sex", "agegr3", "casp_tot_w9", 
                  "cvnump_cat", "work", "wealth_ter", "cvvuln", "heill_updated", "cov19lwgtw2b", "covcase1")]

qol.datasets<- as.mids(qol)


## weighting on imputed datasets 

weighted.datasets <- weightthem(covcase1 ~ sex + as.factor(agegr3) + casp_tot_w9 +
                                  cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, 
                                qol.datasets, approach = 'within', method = 'ps', estimand = 'ATM')
summary(weighted.datasets)

#a)love plot 

#names 
b1 <- bal.tab(covcase1 ~ sex + as.factor(agegr3) + casp_tot_w9 +
                cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, data = qol.datasets,
              int = F)
v1 <- var.names(b1, type = "vec")
v1["casp_tot_w9"] <- "QoL (wave 9/8)"
v1["sex"] <- "Sex"
v1["as.factor(agegr3)_52"] <- "Age: 52-59yrs"
v1["as.factor(agegr3)_60"] <- "Age: 60-74yrs"
v1["as.factor(agegr3)_75"] <- "Age: 75+yrs"
v1["cvnump_cat"] <- "Living alone"
v1["as.factor(work)_1"] <- "Work: Employed"
v1["as.factor(work)_2"] <- "Work: Retired"
v1["as.factor(work)_3"] <- "Work: Other not working"
v1["as.factor(wealth_ter)_1"] <- "Wealth: low"
v1["as.factor(wealth_ter)_2"] <- "Wealth: medium"
v1["as.factor(wealth_ter)_3"] <- "Wealth: high"
v1["cvvuln"] <- "Vulnerable to COVID-19"
v1["heill_updated"] <- "Limiting longstanding illness "
v1["cov19lwgtw2b"] <- "ELSA COVID-19 weight"


#create plot 
plot.qol<- love.plot(weighted.datasets, var.names = v1, title = "Poor Quality of Life") 
plot.qol

#b) table of absolute differences 
balance.qol<-bal.tab(weighted.datasets, abs = TRUE)
balance.qol<- balance.qol[[2]]
balance.qol$outcome<- "qol"


## run regressions 

#w1
weighted.models <- with(data = weighted.datasets, expr = svyglm(casp_tot_w10 ~ covcase1 )) #default family is gaussian 

#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

qol_w1<- summary(weighted.results, conf.int = TRUE)
qol_w1$outcome<- "qol_w1"


#w2
weighted.models <- with(data = weighted.datasets, expr = svyglm(casp_tot_w11 ~ covcase1 ))

#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

qol_w2<- summary(weighted.results, conf.int = TRUE)
qol_w2$outcome<- "qol_w2"


###4) Loneliness 
lone<- maindata[,c(".imp", ".id", "idauniq", "cv_loneliness_w10", "cv_loneliness_w11", "sex", "agegr3", "cv_loneliness_w9", 
                  "cvnump_cat", "work", "wealth_ter", "cvvuln", "heill_updated", "cov19lwgtw2b", "covcase1")]

lone.datasets<- as.mids(lone)


## weighting on imputed datasets 

weighted.datasets <- weightthem(covcase1 ~ sex + as.factor(agegr3) + cv_loneliness_w9 +
                                  cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, 
                                lone.datasets, approach = 'within', method = 'ps', estimand = 'ATM')
summary(weighted.datasets)

#a)love plot 

#names 
b1 <- bal.tab(covcase1 ~ sex + as.factor(agegr3) + cv_loneliness_w9 +
                cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, data = lone.datasets,
              int = F)
v1 <- var.names(b1, type = "vec")
v1["cv_loneliness_w9"] <- "Loneliness (wave 9/8)"
v1["sex"] <- "Sex"
v1["as.factor(agegr3)_52"] <- "Age: 52-59yrs"
v1["as.factor(agegr3)_60"] <- "Age: 60-74yrs"
v1["as.factor(agegr3)_75"] <- "Age: 75+yrs"
v1["cvnump_cat"] <- "Living alone"
v1["as.factor(work)_1"] <- "Work: Employed"
v1["as.factor(work)_2"] <- "Work: Retired"
v1["as.factor(work)_3"] <- "Work: Other not working"
v1["as.factor(wealth_ter)_1"] <- "Wealth: low"
v1["as.factor(wealth_ter)_2"] <- "Wealth: medium"
v1["as.factor(wealth_ter)_3"] <- "Wealth: high"
v1["cvvuln"] <- "Vulnerable to COVID-19"
v1["heill_updated"] <- "Limiting longstanding illness "
v1["cov19lwgtw2b"] <- "ELSA COVID-19 weight"


#create plot 
plot.lone<- love.plot(weighted.datasets, var.names = v1, title = "Loneliness") 
plot.lone

#b) table of absolute differences 
balance.lone<-bal.tab(weighted.datasets, abs = TRUE)
balance.lone<- balance.lone[[2]]
balance.lone$outcome<- "lone"


## run regressions 

#w1
weighted.models <- with(data = weighted.datasets, expr = svyglm(cv_loneliness_w10 ~ covcase1 )) #default family is gaussian 

#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

lone_w1<- summary(weighted.results, conf.int = TRUE)
lone_w1$outcome<- "lone_w1"


#w2
weighted.models <- with(data = weighted.datasets, expr = svyglm(cv_loneliness_w11 ~ covcase1 ))

#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

lone_w2<- summary(weighted.results, conf.int = TRUE)
lone_w2$outcome<- "lone_w2"

###5)Fin: worried 

fin_worried<- maindata[,c(".imp", ".id", "idauniq", "fins_worried_w10", "fins_worried_w11", "sex", "agegr3", "fins_precov", 
                   "cvnump_cat", "work", "wealth_ter", "cvvuln", "heill_updated", "cov19lwgtw2b", "covcase1")]

fin_worried.datasets<- as.mids(fin_worried)


## weighting on imputed datasets 

weighted.datasets <- weightthem(covcase1 ~ sex + as.factor(agegr3) + fins_precov +
                                  cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, 
                                fin_worried.datasets, approach = 'within', method = 'ps', estimand = 'ATM')
summary(weighted.datasets)

#a)love plot 

#names 
b1 <- bal.tab(covcase1 ~ sex + as.factor(agegr3) + fins_precov +
                cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, data = fin_worried.datasets,
              int = F)
v1 <- var.names(b1, type = "vec")
v1["fins_precov"] <- "Pre-pandemic financial situation"
v1["sex"] <- "Sex"
v1["as.factor(agegr3)_52"] <- "Age: 52-59yrs"
v1["as.factor(agegr3)_60"] <- "Age: 60-74yrs"
v1["as.factor(agegr3)_75"] <- "Age: 75+yrs"
v1["cvnump_cat"] <- "Living afin_worried"
v1["as.factor(work)_1"] <- "Work: Employed"
v1["as.factor(work)_2"] <- "Work: Retired"
v1["as.factor(work)_3"] <- "Work: Other not working"
v1["as.factor(wealth_ter)_1"] <- "Wealth: low"
v1["as.factor(wealth_ter)_2"] <- "Wealth: medium"
v1["as.factor(wealth_ter)_3"] <- "Wealth: high"
v1["cvvuln"] <- "Vulnerable to COVID-19"
v1["heill_updated"] <- "Limiting longstanding illness "
v1["cov19lwgtw2b"] <- "ELSA COVID-19 weight"


#create plot 
plot.fin_worried<- love.plot(weighted.datasets, var.names = v1, title = "Financial hardship, Worried") 
plot.fin_worried

#b) table of absolute differences 
balance.fin_worried<-bal.tab(weighted.datasets, abs = TRUE)
balance.fin_worried<- balance.fin_worried[[2]]
balance.fin_worried$outcome<- "fin_worried"


## run regressions 

#w1
weighted.models <- with(data = weighted.datasets, expr = svyglm(fins_worried_w10 ~ covcase1, family = binomial )) 
#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

fin_worried_w1<- summary(weighted.results, conf.int = TRUE)
fin_worried_w1$outcome<- "fin_worried_w1"


#w2
weighted.models <- with(data = weighted.datasets, expr = svyglm(fins_worried_w11 ~ covcase1, family = binomial ))

#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

fin_worried_w2<- summary(weighted.results, conf.int = TRUE)
fin_worried_w2$outcome<- "fin_worried_w2"

###6)Fin: worse off

fin_worse<- maindata[,c(".imp", ".id", "idauniq", "fins_worse_w10", "fins_worse_w11", "sex", "agegr3", "fins_precov", 
                          "cvnump_cat", "work", "wealth_ter", "cvvuln", "heill_updated", "cov19lwgtw2b", "covcase1")]

fin_worse.datasets<- as.mids(fin_worse)


## weighting on imputed datasets 

weighted.datasets <- weightthem(covcase1 ~ sex + as.factor(agegr3) + fins_precov +
                                  cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, 
                                fin_worse.datasets, approach = 'within', method = 'ps', estimand = 'ATM')
summary(weighted.datasets)

#a)love plot 

#names 
b1 <- bal.tab(covcase1 ~ sex + as.factor(agegr3) + fins_precov +
                cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, data = fin_worse.datasets,
              int = F)
v1 <- var.names(b1, type = "vec")
v1["fins_precov"] <- "Pre-pandemic financial situation"
v1["sex"] <- "Sex"
v1["as.factor(agegr3)_52"] <- "Age: 52-59yrs"
v1["as.factor(agegr3)_60"] <- "Age: 60-74yrs"
v1["as.factor(agegr3)_75"] <- "Age: 75+yrs"
v1["cvnump_cat"] <- "Living afin_worse"
v1["as.factor(work)_1"] <- "Work: Employed"
v1["as.factor(work)_2"] <- "Work: Retired"
v1["as.factor(work)_3"] <- "Work: Other not working"
v1["as.factor(wealth_ter)_1"] <- "Wealth: low"
v1["as.factor(wealth_ter)_2"] <- "Wealth: medium"
v1["as.factor(wealth_ter)_3"] <- "Wealth: high"
v1["cvvuln"] <- "Vulnerable to COVID-19"
v1["heill_updated"] <- "Limiting longstanding illness "
v1["cov19lwgtw2b"] <- "ELSA COVID-19 weight"


#create plot 
plot.fin_worse<- love.plot(weighted.datasets, var.names = v1, title = "Financial hardship, Worse off") 
plot.fin_worse

#b) table of absolute differences 
balance.fin_worse<-bal.tab(weighted.datasets, abs = TRUE)
balance.fin_worse<- balance.fin_worse[[2]]
balance.fin_worse$outcome<- "fin_worse"


## run regressions 

#w1
weighted.models <- with(data = weighted.datasets, expr = svyglm(fins_worse_w10 ~ covcase1, family = binomial )) 
#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

fin_worse_w1<- summary(weighted.results, conf.int = TRUE)
fin_worse_w1$outcome<- "fin_worse_w1"


#w2
weighted.models <- with(data = weighted.datasets, expr = svyglm(fins_worse_w11 ~ covcase1, family = binomial ))

#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

fin_worse_w2<- summary(weighted.results, conf.int = TRUE)
fin_worse_w2$outcome<- "fin_worse_w2"


###7) Social connections: family 

frcont_fam<- maindata[,c(".imp", ".id", "idauniq", "frcont_fam_w10", "frcont_fam_w11", "sex", "agegr3", "isolation_binary_w9", 
                        "cvnump_cat", "work", "wealth_ter", "cvvuln", "heill_updated", "cov19lwgtw2b", "covcase1")]

frcont_fam.datasets<- as.mids(frcont_fam)


## weighting on imputed datasets 

weighted.datasets <- weightthem(covcase1 ~ sex + as.factor(agegr3) + isolation_binary_w9 +
                                  cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, 
                                frcont_fam.datasets, approach = 'within', method = 'ps', estimand = 'ATM')
summary(weighted.datasets)

#a)love plot 

#names 
b1 <- bal.tab(covcase1 ~ sex + as.factor(agegr3) + isolation_binary_w9 +
                cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, data = frcont_fam.datasets,
              int = F)
v1 <- var.names(b1, type = "vec")
v1["isolation_binary_w9"] <- "Pre-pandemic social isolation"
v1["sex"] <- "Sex"
v1["as.factor(agegr3)_52"] <- "Age: 52-59yrs"
v1["as.factor(agegr3)_60"] <- "Age: 60-74yrs"
v1["as.factor(agegr3)_75"] <- "Age: 75+yrs"
v1["cvnump_cat"] <- "Living afrcont_fam"
v1["as.factor(work)_1"] <- "Work: Employed"
v1["as.factor(work)_2"] <- "Work: Retired"
v1["as.factor(work)_3"] <- "Work: Other not working"
v1["as.factor(wealth_ter)_1"] <- "Wealth: low"
v1["as.factor(wealth_ter)_2"] <- "Wealth: medium"
v1["as.factor(wealth_ter)_3"] <- "Wealth: high"
v1["cvvuln"] <- "Vulnerable to COVID-19"
v1["heill_updated"] <- "Limiting longstanding illness "
v1["cov19lwgtw2b"] <- "ELSA COVID-19 weight"


#create plot 
plot.frcont_fam<- love.plot(weighted.datasets, var.names = v1, title = "Infrequent contact with family") 
plot.frcont_fam

#b) table of absolute differences 
balance.frcont_fam<-bal.tab(weighted.datasets, abs = TRUE)
balance.frcont_fam<- balance.frcont_fam[[2]]
balance.frcont_fam$outcome<- "frcont_fam"


## run regressions 

#w1
weighted.models <- with(data = weighted.datasets, expr = svyglm(frcont_fam_w10 ~ covcase1, family = binomial )) 
#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

frcont_fam_w1<- summary(weighted.results, conf.int = TRUE)
frcont_fam_w1$outcome<- "frcont_fam_w1"


#w2
weighted.models <- with(data = weighted.datasets, expr = svyglm(frcont_fam_w11 ~ covcase1, family = binomial ))

#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

frcont_fam_w2<- summary(weighted.results, conf.int = TRUE)
frcont_fam_w2$outcome<- "frcont_fam_w2"

###8) Social connections
frcont_fri<- maindata[,c(".imp", ".id", "idauniq", "frcont_fri_w10", "frcont_fri_w11", "sex", "agegr3", "isolation_binary_w9", 
                         "cvnump_cat", "work", "wealth_ter", "cvvuln", "heill_updated", "cov19lwgtw2b", "covcase1")]
frcont_fri$sex_Case<-frcont_fri$covcase1*frcont_fri$sex
frcont_fri.datasets<- as.mids(frcont_fri)
frcont_fri.datasets

## weighting on imputed datasets 

weighted.datasets <- weightthem(covcase1 ~ sex + as.factor(agegr3) + isolation_binary_w9 +
                                  cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, 
                                frcont_fri.datasets, approach = 'within', method = 'ps', estimand = 'ATM')
summary(weighted.datasets)

#a)love plot 

#names 
b1 <- bal.tab(covcase1 ~ sex + as.factor(agegr3) + isolation_binary_w9 +
                cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, data = frcont_fri.datasets,
              int = F)
v1 <- var.names(b1, type = "vec")
v1["isolation_binary_w9"] <- "Pre-pandemic social isolation"
v1["sex"] <- "Sex"
v1["as.factor(agegr3)_52"] <- "Age: 52-59yrs"
v1["as.factor(agegr3)_60"] <- "Age: 60-74yrs"
v1["as.factor(agegr3)_75"] <- "Age: 75+yrs"
v1["cvnump_cat"] <- "Living afrcont_fri"
v1["as.factor(work)_1"] <- "Work: Employed"
v1["as.factor(work)_2"] <- "Work: Retired"
v1["as.factor(work)_3"] <- "Work: Other not working"
v1["as.factor(wealth_ter)_1"] <- "Wealth: low"
v1["as.factor(wealth_ter)_2"] <- "Wealth: medium"
v1["as.factor(wealth_ter)_3"] <- "Wealth: high"
v1["cvvuln"] <- "Vulnerable to COVID-19"
v1["heill_updated"] <- "Limiting longstanding illness "
v1["cov19lwgtw2b"] <- "ELSA COVID-19 weight"


#create plot 
plot.frcont_fri<- love.plot(weighted.datasets, var.names = v1, title = "Infrequent contact with friends") 
plot.frcont_fri

#b) table of absolute differences 
balance.frcont_fri<-bal.tab(weighted.datasets, abs = TRUE)
balance.frcont_fri<- balance.frcont_fri[[2]]
balance.frcont_fri$outcome<- "frcont_fri"


## run regressions 

#w1
weighted.models <- with(data = weighted.datasets, expr = svyglm(frcont_fri_w10 ~ covcase1 , family = binomial )) 
#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

frcont_fri_w1<- summary(weighted.results, conf.int = TRUE)
frcont_fri_w1$outcome<- "frcont_fri_w1"


#w2
weighted.models <- with(data = weighted.datasets, expr = svyglm(frcont_fri_w11 ~ covcase1 , family = binomial ))

#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

frcont_fri_w2<- summary(weighted.results, conf.int = TRUE)
frcont_fri_w2$outcome<- "frcont_fri_w2"


###9) Social connections: total 
frcont_tot<- maindata[,c(".imp", ".id", "idauniq", "frcont_tot_w10", "frcont_tot_w11", "sex", "agegr3", "isolation_binary_w9", 
                         "cvnump_cat", "work", "wealth_ter", "cvvuln", "heill_updated", "cov19lwgtw2b", "covcase1")]

frcont_tot.datasets<- as.mids(frcont_tot)


## weighting on imputed datasets 

weighted.datasets <- weightthem(covcase1 ~ sex + as.factor(agegr3) + isolation_binary_w9 +
                                  cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, 
                                frcont_tot.datasets, approach = 'within', method = 'ps', estimand = 'ATM')
summary(weighted.datasets)

#a)love plot 

#names 
b1 <- bal.tab(covcase1 ~ sex + as.factor(agegr3) + isolation_binary_w9 +
                cvnump_cat + as.factor(work) + as.factor(wealth_ter) + cvvuln + heill_updated + cov19lwgtw2b, data = frcont_tot.datasets,
              int = F)
v1 <- var.names(b1, type = "vec")
v1["isolation_binary_w9"] <- "Pre-pandemic social isolation"
v1["sex"] <- "Sex"
v1["as.factor(agegr3)_52"] <- "Age: 52-59yrs"
v1["as.factor(agegr3)_60"] <- "Age: 60-74yrs"
v1["as.factor(agegr3)_75"] <- "Age: 75+yrs"
v1["cvnump_cat"] <- "Living afrcont_tot"
v1["as.factor(work)_1"] <- "Work: Employed"
v1["as.factor(work)_2"] <- "Work: Retired"
v1["as.factor(work)_3"] <- "Work: Other not working"
v1["as.factor(wealth_ter)_1"] <- "Wealth: low"
v1["as.factor(wealth_ter)_2"] <- "Wealth: medium"
v1["as.factor(wealth_ter)_3"] <- "Wealth: high"
v1["cvvuln"] <- "Vulnerable to COVID-19"
v1["heill_updated"] <- "Limiting longstanding illness "
v1["cov19lwgtw2b"] <- "ELSA COVID-19 weight"


#create plot 
plot.frcont_tot<- love.plot(weighted.datasets, var.names = v1, title = "Infrequent contact (total score)") 
plot.frcont_tot

#b) table of absolute differences 
balance.frcont_tot<-bal.tab(weighted.datasets, abs = TRUE)
balance.frcont_tot<- balance.frcont_tot[[2]]
balance.frcont_tot$outcome<- "frcont_tot"


## run regressions 

#w1
weighted.models <- with(data = weighted.datasets, expr = svyglm(frcont_tot_w10 ~ covcase1 )) 
#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

frcont_tot_w1<- summary(weighted.results, conf.int = TRUE)
frcont_tot_w1$outcome<- "frcont_tot_w1"


#w2
weighted.models <- with(data = weighted.datasets, expr = svyglm(frcont_tot_w11 ~ covcase1))

#pool results 
weighted.results <- pool(weighted.models) 
summary(weighted.results, conf.int = TRUE)

frcont_tot_w2<- summary(weighted.results, conf.int = TRUE)
frcont_tot_w2$outcome<- "frcont_tot_w2"


##### Final operations 

#a) combine love plots 
fig <- ggarrange(plot.dep, plot.anx, plot.qol, plot.lone, plot.fin_worried, plot.fin_worse, 
                 plot.frcont_fam, plot.frcont_fri, plot.frcont_tot,common.legend = T, legend = "bottom",
                 ncol = 3, nrow = 3)
fig
ggsave("Love_plots.jpeg", device = "jpeg",dpi = 300,width =14, height =14, limitsize = F)


#b) combine table of mean differences 

balance.all<- rbind(balance.dep, balance.anx, balance.qol, balance.lone,
                    balance.fin_worried, balance.fin_worse, balance.frcont_fam,
                    balance.frcont_fri, balance.frcont_tot)

#c) combine tables of regression results 

regression_all <-rbind(dep_w1, dep_w2, anx_w1, anx_w2, 
                       qol_w1, qol_w2, lone_w1, lone_w2,
                       fin_worried_w1, fin_worried_w2, fin_worse_w1, fin_worse_w2,
                       frcont_fam_w1, frcont_fam_w2, frcont_fri_w1, frcont_fri_w2,
                       frcont_tot_w1, frcont_tot_w2)
regression_all$estimate_OR<- exp(regression_all$estimate)
regression_all$lci_OR<- exp(regression_all$`2.5 %`)
regression_all$uci_OR<- exp(regression_all$`97.5 %`)

write_xlsx(list(balance = balance.all, regression = regression_all), "PS_imputed.xlsx")
