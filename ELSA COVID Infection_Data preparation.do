********************************************************************************
************* Study 2: impact of covid-19 infection on  ************************
********** mental health, financial hardship, and social connections ***********
********************************************************************************


********************************************************************************
******************* Prepapre variables for the analysis ************************
********************************************************************************

cd "/Users/eiob/Desktop/ELSA COVID19 Study"

use "ELSA covid19_data2_w9_w8.dta", replace

*** COVID-19 wave 1 variables 

** Mental health 

* Depression (compute total score post-imputation)
sum cvmhced_cvmhced* 

* QoL
sum casp_tot_w10

* Loneliness 
sum cv_loneliness_w10

* Anxiety (compute binary score post-imputation)
sum gad7_w10

** Social connections (compute scores post-imputation)

* communication with family 
table cvfamcon_cvfamcon1_q
table cvfamcon_cvfamcon2_q
table cvfamcon_cvfamcon3_q
table cvfamcon_cvfamcon4_q
sum cvfamcon_cvfamcon*
rename cvfamcon_cvfamcon* cvfamcon_cvfamcon*_w10

* communication with friends 
table cvfrdcon_cvfrdcon1_q
table cvfrdcon_cvfrdcon2_q
table cvfrdcon_cvfrdcon3_q
table cvfrdcon_cvfrdcon4_q
sum cvfrdcon_cvfrdcon*
rename cvfrdcon_cvfrdcon* cvfrdcon_cvfrdcon*_w10

** Financial hardship 

* worried about future financial situation
tab cvfins_cvfins1_q
generate fins_worried_w10=(cvfins_cvfins1_q>=3) if cvfins_cvfins1_q!=.
tab cvfins_cvfins1_q fins_worried_w10
tab fins_worried_w10

* worse financial situation than before the pandemic 
tab cvpostfn, nolab
gen fins_worse_w10 = (cvpostfn <=2) if cvpostfn !=.
tab cvpostfn fins_worse_w10
tab fins_worse_w10

** covid infection 

* covid symptoms 
tab cvsymp01 , nolab missing 
tab cvsymp02, nolab missing 
tab cvsymp03, nolab missing 
tab cvsymp04, nolab missing 
tab cvsymp05, nolab missing 
tab cvsymp06, nolab missing 
tab cvsymp07, nolab missing 
tab cvsymp08, nolab missing 

* covid test
tab cvtestb_w10, nolab missing 
replace cvtestb_w10 = 0 if cvtestb_w10 > 1 | cvtestb_w10 == . 
tab cvtestb_w10, nolab

* covid hosp 
tab cvhosp_w10, nolab
replace cvhosp_w10 = 0 if cvhosp_w10 == 2 | cvhosp_w10 == . 
tab cvhosp_w10

** Covariates 

* sex 
tab sex

* age 
sum age

* partnership 
tab partnership2_covid, nolab 

* ethnicity2
tab ethnicity2

* education 
tab education

*home tenure 
tab hometenure 

* number of people in the house 
tab cvnump 

* employment status 
tab work

*limiting longstanding illness
tab heill_updated 
tab cvheself heill_updated 
replace heill_updated = 0 if heill_updated == 2 
tab heill_updated

* vulnerbale to covid 
tab cvvuln
replace cvvuln = 0 if cvvuln == 2

* pre-pandemic financial situation (3 months before pandemic started)
tab cvprefn, nolab
rename cvprefn fins_precov
tab fins_precov


*** Pre-pandemic wave 9 variables 

** Mental health

* Depression 
sum cesd*w9

* QoL
sum casp_tot_w9

* Loneliness
sum cv_loneliness_w9

* Anxiety (ONS) - higher scores indicate higher anxiety 
tab scovan_w9

** Financial situation 

* income 
sum eqtotinc_bu_s

* wealth 
sum nettotnhw_bu_s

save "ELSA covid19_data2_w9_w8.dta", replace

** Social connections 

** wave 9 variables 
cd "/Users/eiob/Desktop/ELSA COVID19 Study"
use "wave 9 data.dta",replace

** 4 indicators of social isolation 

* less than monthly contact with children (score >=4)
tab scchdh
tab scchdh ,nolab
tab scchdi 
tab scchdj 
tab scchdk

gen isochild_w9 = 0
replace isochild_w9 = 1 if scchdh >= 4 & scchdi >= 4 & scchdj >= 4 & scchdk >= 4 
replace isochild_w9 = . if scchdh < 1 & scchdi < 1 & scchdj < 1 & scchdk < 1
tab isochild_w9, missing 


* less than monthly contact with other family 
tab scfamh 
tab scfami 
tab scfamj 
tab scfamk

gen isofamily_w9 = 0
replace isofamily_w9 = 1 if scfamh >= 4 & scfami >= 4 & scfamj >= 4 & scfamk >= 4
replace isofamily_w9 = . if scfamh < 1 & scfami < 1 & scfamj < 1 & scfamk < 1
tab isofamily_w9, missing


* less than monthly contact with friends 
tab scfrdh 
tab scfrdi 
tab scfrdj 
tab scfrdk, nolab

gen isofriend_w9 = 0
replace isofriend_w9 = 1 if scfrdh >= 4 & scfrdi >= 4 & scfrdj >= 4 & scfrdk >= 4
replace isofriend_w9 = . if scfrdh < 1 & scfrdi < 1 & scfrdj < 1 & scfrdk < 1
tab isofriend_w9
tab isofriend_w9, missing 

* not a member of any social organisation 
tab scorg96, nolab

gen isomember_w9 =0
replace isomember_w9 = 1 if scorg96 == 1
replace isomember_w9 = . if  scorg96 < 0
tab isomember_w9

* select derived variables to merge with maindata 
keep idauniq isochild_w9 isofamily_w9 isofriend_w9 isomember_w9
sort idauniq

cd "/Users/eiob/Desktop/ELSA COVID19 Study/Study 2"
save "soccon_w9.dta", replace 


** merge w9 variables with maindata 

* wave 9 
use "ELSA covid19_data2_w9_w8.dta"

sort idauniq
merge 1:1 idauniq using "soccon_w9.dta", gen (_merge3)

*** _merge: 1=master only; 2=using only; 3=match
tab _merge3

*drop ps appearing only in using 
drop if _merge3 == 2

save "ELSA covid19_data2_w9_w8.dta", replace


** calculate social isolation index 
egen isolation_w9 = rowtotal(isochild_w9 isofamily_w9 isofriend_w9 isomember_w9), missing
tab isolation_w9
tab isolation_w9, missing 

save "ELSA covid19_data2_w9_w8.dta", replace


*** select variables for analysis and save 

keep idauniq cvmhced_cvmhced* casp_tot_w10 cv_loneliness_w10 gad7_w10 ///
 cvfamcon_cvfamcon* cvfrdcon_cvfrdcon* fins_worried_w10 fins_worse_w10 ///
 cvsymp01 cvsymp02 cvsymp03 cvsymp04 cvsymp05 cvsymp06 cvsymp07 cvsymp08 ///
 cvtestb_w10 cvhosp_w10 sex age partnership2_covid ethnicity2 education ///
 hometenure cvnump work heill_updated cvvuln fins_precov ///
 cesd*w9 casp_tot_w9 cv_loneliness_w9 scovan_w9 eqtotinc_bu_s ///
 nettotnhw_bu_s isolation_w9

save "maindata_study2.dta", replace
 

*** COVID-19 wave 2 variables 

cd "/Users/eiob/Desktop/ELSA COVID19 Study"
use "ELSA COVID-19 2_final.dta", replace

* longitudinal weight 
sum cov19lwgtw2b

** mental health 

* depression 
sum cesd*w11 
drop cesd8_w11 cesd8_binary_w11

* QoL 
sum casp_tot_w11 

* Loneliness 
sum cv_loneliness_w11

* Anxiety 
sum gad7_w11


** Social connections (compute scores post-imputation)

* communication with family 
table cvfamcon_cvfamcon1_q
table cvfamcon_cvfamcon2_q
table cvfamcon_cvfamcon3_q
table cvfamcon_cvfamcon4_q
sum cvfamcon_cvfamcon*
rename cvfamcon_cvfamcon* cvfamcon_cvfamcon*_w11
recode cvfamcon_cvfamcon* (-9/-1=.)
sum cvfamcon_cvfamcon*


* communication with friends 
table cvfrdcon_cvfrdcon1_q
table cvfrdcon_cvfrdcon2_q
table cvfrdcon_cvfrdcon3_q
table cvfrdcon_cvfrdcon4_q
sum cvfrdcon_cvfrdcon*
rename cvfrdcon_cvfrdcon* cvfrdcon_cvfrdcon*_w11
recode cvfrdcon_cvfrdcon* (-9/-1=.)
sum cvfrdcon_cvfrdcon*

** Financial hardship 

* worried about future financial situation
tab cvfins_cvfins1_q, nolab
tab cvfins_cvfins1_q
generate fins_worried_w11=(cvfins_cvfins1_q>=3) if cvfins_cvfins1_q!=.
replace fins_worried_w11 = . if cvfins_cvfins1_q < 1
tab cvfins_cvfins1_q fins_worried_w11
tab fins_worried_w11

* worse financial situation than before the pandemic 
tab cvpostfn, nolab
tab cvpostfn
replace cvpostfn = . if cvpostfn <1
gen fins_worse_w11 = (cvpostfn <=2) if cvpostfn !=.
tab cvpostfn fins_worse_w11
tab fins_worse_w11, missing 

** covid infection 

*covid test 
tab cvtestb_w11 
tab cvtestb_w11 , nolab 
replace cvtestb_w11 = 0 if cvtestb_w11 > 1 | cvtestb_w11 == -1
replace cvtestb_w11 = . if cvtestb_w11 < -1
tab cvtestb_w11, nolab

* covd hospitalisation 
tab cvhosp_w11 
tab cvhosp_w11 , nolab
replace cvhosp_w11 = 0 if cvhosp_w11 == 2
replace cvhosp_w11 = . if cvhosp_w11 < 0
tab cvhosp_w11, nolab

* long covid 
tab cvlongcovid
tab cvlongcovid, nolab
replace cvlongcovid = 0 if cvlongcovid == -1 | cvlongcovid == 2
tab cvlongcovid

** keep only variables to merge with maindata 
keep idauniq cov19lwgtw2b cesd*w11 casp_tot_w11 cv_loneliness_w11 gad7_w11 ///
cvfamcon_cvfamcon* cvfrdcon_cvfrdcon* fins_worried_w11 fins_worse_w11 ///
cvtestb_w11 cvhosp_w11 cvlongcovid 

sort idauniq 

cd "/Users/eiob/Desktop/ELSA COVID19 Study/Study 2"
save "wave2 variables"


** merge with maindata_study2

use "maindata_study2.dta"

use "ELSA covid19_data2_w9_w8.dta"

sort idauniq
merge 1:1 idauniq using "wave2 variables", gen (_merge5)

*** _merge: 1=master only; 2=using only; 3=match
tab _merge5

* variable for analytical sample 
gen anasample = 1 if cov19lwgtw2b !=.
replace anasample = 0 if cov19lwgtw2 ==.
tab anasample

save "maindata_study2.dta", replace 
use "maindata_study2.dta", replace

*** Revisions 

** add new health conditions at covid wave 1 
use "ELSA covid19_data2_w9_w8.dta"

keep idauniq cvhecond*

gen newcondcov1 = 0
replace newcondcov1 = 1 if cvhecond11 == 1 | cvhecond33 == 1 | cvhecond32 == 1 | ///
cvhecond31 == 1 | cvhecond30 == 1 | cvhecond29 == 1 | cvhecond28 == 1 | cvhecond27 == 1 | ///
cvhecond26 == 1 | cvhecond24 == 1 | cvhecond21 == 1 | cvhecond19 == 1 | cvhecond17== 1 | ///
cvhecond01 == 1 | cvhecond04 == 1 | cvhecond10 == 1 | cvhecond03 == 1 | cvhecond06 == 1 | ///
cvhecond09 == 1 |cvhecond07 == 1 | cvhecond08 == 1 | cvhecond02 == 1 | cvhecond05 == 1 

tab newcondcov1

save "healthcon_cov1.dta"
** Living with mental health condition 
** combine data on new/exisitng mental condition in covid wave 1 and wave 9 

* wave 9
set maxvar 10000
use "wave_9_elsa_data_eul_v1.dta", replace 

gen mhc_w9 = 0
replace mhc_w9 = 1 if hedbdps == 1 | heyrc == 1 | hedibps == 1

tab mhc_w9

keep idauniq mhc_w9

sort idauniq

save "mhc_w9.dta"

* new MH condition covid wave 
use "healthcon_cov1.dta", replace

tab cvhecond22, nolab

* merge w9 MH with covid data 

sort idauniq
merge 1:1 idauniq using "mhc_w9.dta", gen (_merge1)

*** _merge: 1=master only; 2=using only; 3=match
tab _merge1

drop if _merge1 == 2

* MH condition w9/covid1 

gen mhc_w9cov1 = 0
replace mhc_w9cov1 =1 if mhc_w9 == 1 | cvhecond22 == 1

tab mhc_w9cov1

* keep only vars to merge with main dataset 
keep idauniq newcondcov1 mhc_w9cov1
sort idauniq

save "healthcon_w9cov1.dta"

** merge with main data 

cd "/Users/eiob/Desktop/ELSA COVID19 Study/Study 2_Covid infection"

use "maindata_study2.dta", replace
cd "/Users/eiob/Desktop/ELSA COVID19 Study"


sort idauniq
merge 1:1 idauniq using "healthcon_w9cov1.dta", gen (_merge6)


tab mhc_w9cov1 if anasample == 1
tab newcondcov1 if anasample == 1

cd "/Users/eiob/Desktop/ELSA COVID19 Study/Study 2_Covid infection"

save "maindata_study2.dta", replace

**** Export data to R for multiple imputation of missing data ****

*******************************************************************************
******* Derive post-imputation variables in imputed dataset *******************
*******************************************************************************


** import imputed data from R

use "maindata_s2_imp.dta", replace 
use "maindata_s2_imp_rev.dta", replace 

*register dataset as mi
mi import flong, m(_imp) id(idauniq)

**** Outcomes 

*** mental health 

** Depression, w9

*total score, continuous 
egen cesd8_w9 = rowtotal(cesda_w9 cesdb_w9 cesdc_w9 cesdd_w9 cesde_w9 cesdf_w9 cesdh_w9 cesdg_w9), missing

sum cesd8_w9

*total score, binary 
gen cesd8_binary_w9 = (cesd8_w9>=4) if cesd8_w9!=.
tab cesd8_binary_w9


** Depression, w10

*total score, continuous 
egen cesd8_w10 = rowtotal(cvmhced_cvmhced1_q cvmhced_cvmhced2_q cvmhced_cvmhced3_q cvmhced_cvmhced4_q cvmhced_cvmhced5_q cvmhced_cvmhced6_q cvmhced_cvmhced8_q cvmhced_cvmhced7_q), missing

sum cesd8_w10

*total score, binary 
gen cesd8_binary_w10 = (cesd8_w10>=4) if cesd8_w10!=.
tab cesd8_binary_w10


** Depression, w11

*total score, continuous 
egen cesd8_w11 = rowtotal(cesda_w11 cesdb_w11 cesdc_w11 cesdd_w11 cesde_w11 cesdf_w11 cesdh_w11 cesdg_w11), missing

sum cesd8_w11

*total score, binary 
gen cesd8_binary_w11 = (cesd8_w11>=4) if cesd8_w11!=.
tab cesd8_binary_w11


** QoL, w9
sum casp_tot_w9

** QoL, w10
sum casp_tot_w10

** QoL, w11
sum casp_tot_w11


** Loneliness, w9
sum cv_loneliness_w9

** Loneliness, w10
sum cv_loneliness_w10

** Loneliness, w11
sum cv_loneliness_w11


** Anxiety, w9 (ONS question)
sum scovan_w9

** Anxiety, w10 (gad7)
sum gad7_w10

* binary score 
generate gad7_binary_w10=(gad7_w10>=10) if gad7_w10!=.
tab gad7_binary_w10

** Anxiety, w11 (gad7)
sum gad7_w11

* binary score 
generate gad7_binary_w11=(gad7_w11>=10) if gad7_w11!=.
tab gad7_binary_w11

save "maindata_s2_imp.dta", replace 
save "maindata_s2_imp_rev.dta", replace 


*** financial situation 

** wave 9 

*wealth 
sum nettotnhw_bu_s 

*tertiles 
egen wealth_ter = cut(nettotnhw_bu_s ), group(3) 
replace wealth_ter = wealth_ter +1
table wealth_ter
sum nettotnhw_bu_s if wealth_ter ==3

*income
sum eqtotinc_bu_s

*tertiles 
egen income_ter = cut(eqtotinc_bu_s), group(3)
replace income_ter = income_ter + 1
table income_ter


** wave 10

tab fins_worried_w10 

tab fins_worse_w10 

tab fins_precov


** wave 11

tab fins_worried_w11 

tab fins_worse_w11


*** social connections 

** wave 9
tab isolation_w9

*binary score 
gen isolation_binary_w9= (isolation_w9>=2) if isolation_w9 != .
tab isolation_w9 isolation_binary_w9
tab isolation_binary_w9

** wave 10 

* Infrequent real-time/written contact with family 
gen frcont_fam_w10 = (cvfamcon_cvfamcon1_q_w10 == 4 & cvfamcon_cvfamcon2_q_w10 == 4 & ///
cvfamcon_cvfamcon3_q_w10 == 4 & cvfamcon_cvfamcon4_q_w10 == 4) if ///
cvfamcon_cvfamcon1_q_w10  !=. | cvfamcon_cvfamcon2_q_w10  !=. | ///
cvfamcon_cvfamcon3_q_w10 !=. | cvfamcon_cvfamcon4_q_w10 !=.

tab frcont_fam_w10 
* 7%

* Infrequent real-time contact with family 
gen frcontreal_fam_w10 = (cvfamcon_cvfamcon1_q_w10 == 4 & cvfamcon_cvfamcon2_q_w10 == 4 ) if ///
cvfamcon_cvfamcon1_q_w10  !=. | cvfamcon_cvfamcon2_q_w10  !=. 

tab frcontreal_fam_w10
* 12.6%


* Infrequent real-time/written contact with friends 
gen frcont_fri_w10 = (cvfrdcon_cvfrdcon1_q_w10 == 4 & cvfrdcon_cvfrdcon2_q_w10 == 4 & ///
 cvfrdcon_cvfrdcon3_q_w10 == 4 & cvfrdcon_cvfrdcon4_q_w10 == 4) if ///
 cvfrdcon_cvfrdcon1_q_w10 !=. | cvfrdcon_cvfrdcon2_q_w10 !=. | ///
 cvfrdcon_cvfrdcon3_q_w10 !=. | cvfrdcon_cvfrdcon4_q_w10 !=. 

tab frcont_fri_w10
* 10%


* Infrequent real-time contact with friends 
gen frcontreal_fri_w10 = (cvfrdcon_cvfrdcon1_q_w10 == 4 & cvfrdcon_cvfrdcon2_q_w10 == 4 ) if ///
 cvfrdcon_cvfrdcon1_q_w10 !=. | cvfrdcon_cvfrdcon2_q_w10 !=. 

tab frcontreal_fri_w10
* 18%


* Infrequent real-time contact with family or friends 
gen frcontreal_all_w10 = (cvfamcon_cvfamcon1_q_w10 == 4 & cvfamcon_cvfamcon2_q_w10 == 4 & ///
cvfrdcon_cvfrdcon1_q_w10 == 4 & cvfrdcon_cvfrdcon2_q_w10 == 4) if ///
cvfamcon_cvfamcon1_q_w10  !=. | cvfamcon_cvfamcon2_q_w10  !=. | ///
cvfrdcon_cvfrdcon1_q_w10 !=. | cvfrdcon_cvfrdcon2_q_w10 !=. 

tab frcontreal_all_w10
*6.9%

* total score: all contact with family and friends 

egen frcont_tot_w10 = rowtotal (cvfamcon_cvfamcon1_q_w10 cvfamcon_cvfamcon2_q_w10 ///
 cvfamcon_cvfamcon3_q_w10 cvfamcon_cvfamcon4_q_w10 cvfrdcon_cvfrdcon1_q_w10 ///
 cvfrdcon_cvfrdcon2_q_w10 cvfrdcon_cvfrdcon3_q_w10 cvfrdcon_cvfrdcon4_q_w10), missing 
 
sum frcont_tot_w10
histogram frcont_tot_w10
* score approximately normally distributed 

 
** wave 11

* Infrequent real-time/written contact with family 
gen frcont_fam_w11 = (cvfamcon_cvfamcon1_q_w11 == 4 & cvfamcon_cvfamcon2_q_w11 == 4 & ///
cvfamcon_cvfamcon3_q_w11 == 4 & cvfamcon_cvfamcon4_q_w11 == 4) if ///
cvfamcon_cvfamcon1_q_w11  !=. | cvfamcon_cvfamcon2_q_w11  !=. | ///
cvfamcon_cvfamcon3_q_w11 !=. | cvfamcon_cvfamcon4_q_w11 !=.

tab frcont_fam_w11 
* 10%

* Infrequent real-time contact with family 
gen frcontreal_fam_w11 = (cvfamcon_cvfamcon1_q_w11 == 4 & cvfamcon_cvfamcon2_q_w11 == 4 ) if ///
cvfamcon_cvfamcon1_q_w11  !=. | cvfamcon_cvfamcon2_q_w11  !=. 

tab frcontreal_fam_w11
* 15.81%


* Infrequent real-time/written contact with friends 
gen frcont_fri_w11 = (cvfrdcon_cvfrdcon1_q_w11 == 4 & cvfrdcon_cvfrdcon2_q_w11 == 4 & ///
 cvfrdcon_cvfrdcon3_q_w11 == 4 & cvfrdcon_cvfrdcon4_q_w11 == 4) if ///
 cvfrdcon_cvfrdcon1_q_w11 !=. | cvfrdcon_cvfrdcon2_q_w11 !=. | ///
 cvfrdcon_cvfrdcon3_q_w11 !=. | cvfrdcon_cvfrdcon4_q_w11 !=. 

tab frcont_fri_w11
* 13.9%


* Infrequent real-time contact with friends 
gen frcontreal_fri_w11 = (cvfrdcon_cvfrdcon1_q_w11 == 4 & cvfrdcon_cvfrdcon2_q_w11 == 4 ) if ///
 cvfrdcon_cvfrdcon1_q_w11 !=. | cvfrdcon_cvfrdcon2_q_w11 !=. 

tab frcontreal_fri_w11
* 18%


* Infrequent real-time contact with family or friends 
gen frcontreal_all_w11 = (cvfamcon_cvfamcon1_q_w11 == 4 & cvfamcon_cvfamcon2_q_w11 == 4 & ///
cvfrdcon_cvfrdcon1_q_w11 == 4 & cvfrdcon_cvfrdcon2_q_w11 == 4) if ///
cvfamcon_cvfamcon1_q_w11  !=. | cvfamcon_cvfamcon2_q_w11  !=. | ///
cvfrdcon_cvfrdcon1_q_w11 !=. | cvfrdcon_cvfrdcon2_q_w11 !=. 

tab frcontreal_all_w11
*8.8%

* total score: all contact with family and friends 

egen frcont_tot_w11 = rowtotal (cvfamcon_cvfamcon1_q_w11 cvfamcon_cvfamcon2_q_w11 ///
 cvfamcon_cvfamcon3_q_w11 cvfamcon_cvfamcon4_q_w11 cvfrdcon_cvfrdcon1_q_w11 ///
 cvfrdcon_cvfrdcon2_q_w11 cvfrdcon_cvfrdcon3_q_w11 cvfrdcon_cvfrdcon4_q_w11), missing 
 
sum frcont_tot_w11
histogram frcont_tot_w11
* score approximately normally distributed 


*** Definition of probable COVID-19 infection 

** whether had one/two of the three key symptoms of covid 

*high temp
tab cvsymp01 , nolab missing 

*cough 
tab cvsymp02, nolab missing 

*loss smell/taste
tab cvsymp05, nolab missing 


egen keycovsymsum = rowtotal (cvsymp01 cvsymp02 cvsymp05 )
tab keycovsymsum, missing 

generate keycovsymp2=(keycovsymsum>=2) if keycovsymsum !=.
tab keycovsymp2

generate keycovsymp1=(keycovsymsum>=1) if keycovsymsum !=.
tab keycovsymp1

tab cvtestb_w10
tab cvhosp_w10

** Definition 1: positive covid test/ hospitalisation/ one of three core symptoms 

gen covcase1 =(cvtestb_w10 == 1 | cvhosp_w10 == 1 | keycovsymp1 == 1) if ///
cvtestb_w10 !=. | cvhosp_w10 !=. | keycovsymp1 !=.

tab covcase1 
*8.3%

** Definition 2: positive covid test/ hospitalisation/ two of three core symptoms 

gen covcase2 = ( cvtestb_w10 == 1 | cvhosp_w10 == 1 | keycovsymp2 == 1 ) if ///
cvtestb_w10 !=. | cvhosp_w10 !=. | keycovsymp2 !=.

tab covcase2 
*2.8%

** Definition 3: positive covid test/ hospitalisation/ loss of taste or smell 

gen covcase3 = ( cvtestb_w10 == 1 | cvhosp_w10 == 1 | cvsymp05 == 1 ) if ///
cvtestb_w10 !=. | cvhosp_w10 !=. | cvsymp05 !=. 

tab covcase3
*3.8%


*naughty regression 
mi estimate: logit cesd8_binary_w10 covcase1 sex age cesd8_binary_w9


*** COVID at wave 2 
tab cvlongcovid

gen cov_w11 = ( cvtestb_w11 == 1 | cvhosp_w11 == 1 | cvlongcovid == 1 ) if ///
cvtestb_w11 !=. | cvhosp_w11 !=. | cvlongcovid !=.


tab cov_w11

*** Definition 3 for revision: positive covid test/ hospitalisation/ one of two core symptoms, including cough and loss of sense of small/test

gen covcase3_rev =(cvtestb_w10 == 1 | cvhosp_w10 == 1 | cvsymp02 == 1 | cvsymp05 == 1) 

tab covcase3_rev



*** Covariates 

*sez 
tab sex 
*0=male; 1=female

*age 
sum age 

*age groups 
egen agegr3 = cut(age), at (52, 60, 75, 100)
tab agegr3 age 

*partner 
tab partnership2_covid
*1=no partner; 0=yes partner 

*ethnicity
tab ethnicity2

*education 
tab education

*home tenure 
tab hometenure

*work status 
tab work 

*limiting illness 
tab heill_updated
*1=yes; 0=no

*vulnerable to covid 
tab cvvuln

*living alone
tab cvnump

gen cvnump_cat = (cvnump<2) if cvnump !=.
tab cvnump cvnump_cat
tab cvnump_cat

** new wealth var for the analysis (1=richest;3=poorest)

gen wealth_ter_old = wealth_ter

replace wealth_ter = 1 if wealth_ter_old ==3 
replace wealth_ter = 3 if wealth_ter_old ==1 

tab wealth_ter wealth_ter_old


save "maindata_s2_imp.dta", replace 
save "maindata_s2_imp_rev.dta", replace 
