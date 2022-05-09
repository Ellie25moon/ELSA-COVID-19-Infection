********************************************************************************
************* Study 2: impact of covid-19 infection on  ************************
********** mental health, financial hardship, and social connections ***********
********************************************************************************

********************************************************************************
******************* Regression models ******************************************
********************************************************************************

* Set up main dataset for survey analysis 
use "maindata_s2_imp.dta", replace 

gen psu=idauniq
mi svyset [pweight=cov19lwgtw2b], psu(psu) 


save "maindata_s2_imp.dta", replace 


******************* Main imputed data analysis **********************************

*** Depression 

clear
save dep.imputed, emptyok
use "maindata_s2_imp.dta", replace 

local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

mi estimate, or: svy: logit cesd8_binary_w10 `o' sex i.agegr3 cesd8_binary_w9
regsave using dep.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit cesd8_binary_w10 `o' sex i.agegr3 cesd8_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using dep.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit cesd8_binary_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat cesd8_binary_w9 cvvuln heill_updated
regsave using dep.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit cesd8_binary_w11 `o' sex i.agegr3 cesd8_binary_w9 cov_w11
regsave using dep.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit cesd8_binary_w11 `o' sex i.agegr3 cesd8_binary_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using dep.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit cesd8_binary_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat cesd8_binary_w9 cov_w11 cvvuln heill_updated
regsave using dep.imputed, pval ci append  addlabel(outcome, `o' w11)

}

use dep.imputed, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "dep.imputed.xlsx", firstrow(variables)



*** Anxiety 
clear
save anx.imputed, emptyok
use "maindata_s2_imp.dta", replace 

local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

mi estimate, or: svy: logit gad7_binary_w10 `o' sex i.agegr3 scovan_w9
regsave using anx.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit gad7_binary_w10 `o' sex i.agegr3 scovan_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using anx.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit gad7_binary_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat scovan_w9 cvvuln heill_updated
regsave using anx.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit gad7_binary_w11 `o' sex i.agegr3 scovan_w9 cov_w11
regsave using anx.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit gad7_binary_w11 `o' sex i.agegr3 scovan_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using anx.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit gad7_binary_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat scovan_w9 cov_w11 cvvuln heill_updated
regsave using anx.imputed, pval ci append addlabel(outcome, `o' w11)

}

use anx.imputed, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "anx.imputed.xlsx", firstrow(variables)


*** Poor QoL

clear
save qol.imputed, emptyok
use "maindata_s2_imp.dta", replace 


local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

mi estimate: svy: reg casp_tot_w10 `o' sex i.agegr3 casp_tot_w9
regsave using qol.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate: svy: reg casp_tot_w10 `o' sex i.agegr3 casp_tot_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using qol.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate: svy: reg casp_tot_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat casp_tot_w9 cvvuln heill_updated
regsave using qol.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate: svy: reg casp_tot_w11 `o' sex i.agegr3 casp_tot_w9 cov_w11
regsave using qol.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate: svy: reg casp_tot_w11 `o' sex i.agegr3 casp_tot_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using qol.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate: svy: reg casp_tot_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat casp_tot_w9 cov_w11 cvvuln heill_updated
regsave using qol.imputed, pval ci append addlabel(outcome, `o' w11)

}

use qol.imputed, replace
export excel using "qol.imputed.xlsx", firstrow(variables)


*** Loneliness 

clear
save lone.imputed, emptyok
use "maindata_s2_imp.dta", replace 
 
local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

mi estimate: svy: reg cv_loneliness_w10 `o' sex i.agegr3 cv_loneliness_w9
regsave using lone.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate: svy: reg cv_loneliness_w10 `o' sex i.agegr3 cv_loneliness_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using lone.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate: svy: reg cv_loneliness_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat cv_loneliness_w9 cvvuln heill_updated
regsave using lone.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate: svy: reg cv_loneliness_w11 `o' sex i.agegr3 cv_loneliness_w9 cov_w11
regsave using lone.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate: svy: reg cv_loneliness_w11 `o' sex i.agegr3 cv_loneliness_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using lone.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate: svy: reg cv_loneliness_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat cv_loneliness_w9 cov_w11 cvvuln heill_updated
regsave using lone.imputed, pval ci append addlabel(outcome, `o' w11)

}

use lone.imputed, replace
export excel using "lone.imputed.xlsx", firstrow(variables)


*** financial situation: worried 

clear
save finworried.imputed, emptyok
use "maindata_s2_imp.dta", replace 

mi estimate, or: svy: logit fins_worried_w10 i.covcase1 sex i.agegr3 fins_precov

local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

mi estimate, or: svy: logit fins_worried_w10 `o' sex i.agegr3 fins_precov
regsave using finworried.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit fins_worried_w10 `o' sex i.agegr3 fins_precov ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using finworried.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit fins_worried_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat fins_precov  cvvuln heill_updated
regsave using finworried.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit fins_worried_w11 `o' sex i.agegr3 fins_precov cov_w11
regsave using finworried.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit fins_worried_w11 `o' sex i.agegr3 fins_precov cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using finworried.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit fins_worried_w11 `o'##i.sex `o'##i.agegr3  ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat fins_precov cov_w11  cvvuln heill_updated
regsave using finworried.imputed, pval ci append addlabel(outcome, `o' w11)

}

use finworried.imputed, replace

gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "finworried.imputed.xlsx", firstrow(variables)



*** financial situation: worse

clear
save finworse.imputed, emptyok
use "maindata_s2_imp.dta", replace 


local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

mi estimate, or: svy: logit fins_worse_w10 `o' sex i.agegr3 fins_precov
regsave using finworse.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit fins_worse_w10 `o' sex i.agegr3 fins_precov ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using finworse.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit fins_worse_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat fins_precov cvvuln heill_updated
regsave using finworse.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit fins_worse_w11 `o' sex i.agegr3 fins_precov cov_w11
regsave using finworse.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit fins_worse_w11 `o' sex i.agegr3 fins_precov cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using finworse.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit fins_worse_w11 `o'##i.sex `o'##i.agegr3  ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat fins_precov cov_w11 cvvuln heill_updated
regsave using finworse.imputed, pval ci append addlabel(outcome, `o' w11)

}

use finworse.imputed, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "finworse.imputed.xlsx", firstrow(variables)



*** social contact: family 

clear
save scfam.imputed, emptyok
use "maindata_s2_imp.dta", replace 


local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

mi estimate, or: svy: logit frcont_fam_w10 `o' sex i.agegr3 isolation_binary_w9
regsave using scfam.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcont_fam_w10 `o' sex i.agegr3 isolation_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using scfam.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcont_fam_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cvvuln heill_updated
regsave using scfam.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcont_fam_w11 `o' sex i.agegr3 isolation_binary_w9 ///
 cov_w11
regsave using scfam.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit frcont_fam_w11 `o' sex i.agegr3 isolation_binary_w9 ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using scfam.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit frcont_fam_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cov_w11 cvvuln heill_updated
regsave using scfam.imputed, pval ci append addlabel(outcome, `o' w11)

}

use scfam.imputed, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "scfam.imputed.xlsx", firstrow(variables)


*** social contact: friends 

clear
save scfrie.imputed, emptyok
use "maindata_s2_imp.dta", replace 


local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

mi estimate, or: svy: logit frcont_fri_w10 `o' sex i.agegr3 isolation_binary_w9
regsave using scfrie.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcont_fri_w10 `o' sex i.agegr3 isolation_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using scfrie.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcont_fri_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cvvuln heill_updated
regsave using scfrie.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcont_fri_w11 `o' sex i.agegr3 isolation_binary_w9 ///
 cov_w11
regsave using scfrie.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit frcont_fri_w11 `o' sex i.agegr3 isolation_binary_w9 ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using scfrie.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit frcont_fri_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cov_w11 cvvuln heill_updated
regsave using scfrie.imputed, pval ci append addlabel(outcome, `o' w11)

}

use scfrie.imputed, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "scfrie.imputed.xlsx", firstrow(variables)



*** social contact: all 
clear
save sctot.imputed, emptyok
use "maindata_s2_imp.dta", replace 


local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

mi estimate: reg frcont_tot_w10 `o' sex i.agegr3 isolation_binary_w9
regsave using sctot.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate: reg frcont_tot_w10 `o' sex i.agegr3 isolation_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using sctot.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate: reg frcont_tot_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cvvuln heill_updated
regsave using sctot.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate: reg frcont_tot_w11 `o' sex i.agegr3 isolation_binary_w9 ///
 cov_w11
regsave using sctot.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate: reg frcont_tot_w11 `o' sex i.agegr3 isolation_binary_w9 ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using sctot.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate: reg frcont_tot_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cov_w11 cvvuln heill_updated
regsave using sctot.imputed, pval ci append addlabel(outcome, `o' w11)

}

use sctot.imputed, replace
export excel using "sctot.imputed.xlsx", firstrow(variables)


*** social contact: real_all
clear
save screaltot.imputed, emptyok
use "maindata_s2_imp.dta", replace 


local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

mi estimate, or: svy: logit frcontreal_all_w10 `o' sex i.agegr3 isolation_binary_w9
regsave using screaltot.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcontreal_all_w10 `o' sex i.agegr3 isolation_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using screaltot.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcontreal_all_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cvvuln heill_updated
regsave using screaltot.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcontreal_all_w11 `o' sex i.agegr3 isolation_binary_w9 ///
 cov_w11
regsave using screaltot.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit frcontreal_all_w11 `o' sex i.agegr3 isolation_binary_w9 ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using screaltot.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit frcontreal_all_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cov_w11 cvvuln heill_updated
regsave using screaltot.imputed, pval ci append addlabel(outcome, `o' w11)

}

use screaltot.imputed, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "screaltot.imputed.xlsx", firstrow(variables)


*** social contact: real_fam
clear
save screalfam.imputed, emptyok
use "maindata_s2_imp.dta", replace 

local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

mi estimate, or: svy: logit frcontreal_fam_w10 `o' sex i.agegr3 isolation_binary_w9
regsave using screalfam.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcontreal_fam_w10 `o' sex i.agegr3 isolation_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using screalfam.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcontreal_fam_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cvvuln heill_updated
regsave using screalfam.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcontreal_fam_w11 `o' sex i.agegr3 isolation_binary_w9 ///
 cov_w11
regsave using screalfam.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit frcontreal_fam_w11 `o' sex i.agegr3 isolation_binary_w9 ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using screalfam.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit frcontreal_fam_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cov_w11 cvvuln heill_updated
regsave using screalfam.imputed, pval ci append addlabel(outcome, `o' w11)

}

use screalfam.imputed, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "screalfam.imputed.xlsx", firstrow(variables)



*** social contact: real_fri
clear
save screalfri.imputed, emptyok
use "maindata_s2_imp.dta", replace 

local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

mi estimate, or: svy: logit frcontreal_fri_w10 `o' sex i.agegr3 isolation_binary_w9
regsave using screalfri.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcontreal_fri_w10 `o' sex i.agegr3 isolation_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using screalfri.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcontreal_fri_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cvvuln heill_updated
regsave using screalfri.imputed, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcontreal_fri_w11 `o' sex i.agegr3 isolation_binary_w9 ///
 cov_w11
regsave using screalfri.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit frcontreal_fri_w11 `o' sex i.agegr3 isolation_binary_w9 ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using screalfri.imputed, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit frcontreal_fri_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cov_w11 cvvuln heill_updated
regsave using screalfri.imputed, pval ci append addlabel(outcome, `o' w11)

}

use screalfri.imputed, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "screalfri.imputed.xlsx", firstrow(variables)


******* Predicted values for the significant interactions

** Anxiety w1: work status 
use "maindata_s2_imp.dta", replace 


*Employed 

mi estimate, esampvaryok: svy: reg gad7_binary_w10 covcase1##i.sex covcase1##i.agegr3 ///
 covcase1##i.wealth_ter covcase1##i.cvnump_cat scovan_w9 cvvuln heill_updated ///
if work == 1


mimrgns if covcase1 == 0, esampvaryok

putexcel set "s2_predicted values_anx.xls", replace

putexcel A1= ("Outcome")
putexcel A2= ("Group")
putexcel A3= ("Category")
putexcel A4= ("Wave")
putexcel A5= ("Estimate")
putexcel A6= ("se")
putexcel A7= ("t")
putexcel A8= ("p")
putexcel A9= ("cil")
putexcel A10= ("ciu")

putexcel B1=("anx")
putexcel B2=("work")
putexcel B3=("Employed")
putexcel B4=("COVID-19 infection, No")
matrix a = r(table)
putexcel B5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_anx.xls", modify
putexcel C1=("anx")
putexcel C2=("work")
putexcel C3=("Employed")
putexcel C4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel C5=matrix(a)

*retired
mi estimate, esampvaryok: svy: reg gad7_binary_w10 covcase1##i.sex covcase1##i.agegr3 ///
 covcase1##i.wealth_ter covcase1##i.cvnump_cat scovan_w9 cvvuln heill_updated ///
if work == 2

mimrgns if covcase1 ==0  , esampvaryok
putexcel set "s2_predicted values_anx.xls", modify
putexcel D1=("anx")
putexcel D2=("work")
putexcel D3=("Retired")
putexcel D4=("COVID-19 infection, No")
matrix a = r(table)
putexcel D5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok
putexcel set "s2_predicted values_anx.xls", modify
putexcel E1=("anx")
putexcel E2=("work")
putexcel E3=("Retired")
putexcel E4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel E5=matrix(a)


*Other not working 
mi estimate, esampvaryok: svy: reg gad7_binary_w10 covcase1##i.sex covcase1##i.agegr3 ///
 covcase1##i.wealth_ter covcase1##i.cvnump_cat scovan_w9 cvvuln heill_updated ///
if work == 3

mimrgns if covcase1 ==0, esampvaryok
putexcel set "s2_predicted values_anx.xls", modify
putexcel F1=("anx")
putexcel F2=("work")
putexcel F3=("Other not working")
putexcel F4=("COVID-19 infection, No")
matrix a = r(table)
putexcel F5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok
putexcel set "s2_predicted values_anx.xls", modify
putexcel G1=("anx")
putexcel G2=("work")
putexcel G3=("Other not working")
putexcel G4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel G5=matrix(a)


*** QoL, w1: works status, wealth, sex 

use "maindata_s2_imp.dta", replace 

** work status 
*Employed 

mi estimate, esampvaryok: svy: reg casp_tot_w10 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.wealth_ter covcase1##i.cvnump_cat casp_tot_w9 cvvuln heill_updated ///
if work ==1


mimrgns if covcase1 == 0, esampvaryok

putexcel set "s2_predicted values_qol.xls", replace

putexcel A1= ("Outcome")
putexcel A2= ("Group")
putexcel A3= ("Category")
putexcel A4= ("Wave")
putexcel A5= ("Estimate")
putexcel A6= ("se")
putexcel A7= ("t")
putexcel A8= ("p")
putexcel A9= ("cil")
putexcel A10= ("ciu")

putexcel B1=("qol")
putexcel B2=("work")
putexcel B3=("Employed")
putexcel B4=("COVID-19 infection, No")
matrix a = r(table)
putexcel B5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_qol.xls", modify
putexcel C1=("qol")
putexcel C2=("work")
putexcel C3=("Employed")
putexcel C4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel C5=matrix(a)

*retired
mi estimate, esampvaryok: svy: reg casp_tot_w10 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.wealth_ter covcase1##i.cvnump_cat casp_tot_w9 cvvuln heill_updated ///
if work ==2

mimrgns if covcase1 ==0  , esampvaryok
putexcel set "s2_predicted values_qol.xls", modify
putexcel D1=("qol")
putexcel D2=("work")
putexcel D3=("Retired")
putexcel D4=("COVID-19 infection, No")
matrix a = r(table)
putexcel D5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok
putexcel set "s2_predicted values_qol.xls", modify
putexcel E1=("qol")
putexcel E2=("work")
putexcel E3=("Retired")
putexcel E4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel E5=matrix(a)


*Other not working 
mi estimate, esampvaryok: svy: reg casp_tot_w10 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.wealth_ter covcase1##i.cvnump_cat casp_tot_w9 cvvuln heill_updated ///
if work ==3

mimrgns if covcase1 ==0, esampvaryok
putexcel set "s2_predicted values_qol.xls", modify
putexcel F1=("qol")
putexcel F2=("work")
putexcel F3=("Other not working")
putexcel F4=("COVID-19 infection, No")
matrix a = r(table)
putexcel F5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok
putexcel set "s2_predicted values_qol.xls", modify
putexcel G1=("qol")
putexcel G2=("work")
putexcel G3=("Other not working")
putexcel G4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel G5=matrix(a)

** wealth 

*1st tertile 
mi estimate, esampvaryok: svy: reg casp_tot_w10 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.work covcase1##i.cvnump_cat casp_tot_w9 cvvuln heill_updated ///
if wealth_ter ==1


mimrgns if covcase1 == 0, esampvaryok
putexcel set "s2_predicted values_qol.xls", modify
putexcel H1=("qol")
putexcel H2=("wealth")
putexcel H3=("High")
putexcel H4=("COVID-19 infection, No")
matrix a = r(table)
putexcel H5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_qol.xls", modify
putexcel I1=("qol")
putexcel I2=("wealth")
putexcel I3=("High")
putexcel I4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel I5=matrix(a)


*2nd tertile 
mi estimate, esampvaryok: svy: reg casp_tot_w10 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.work covcase1##i.cvnump_cat casp_tot_w9 cvvuln heill_updated ///
if wealth_ter ==2


mimrgns if covcase1 == 0, esampvaryok
putexcel set "s2_predicted values_qol.xls", modify
putexcel J1=("qol")
putexcel J2=("wealth")
putexcel J3=("Medium")
putexcel J4=("COVID-19 infection, No")
matrix a = r(table)
putexcel J5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_qol.xls", modify
putexcel K1=("qol")
putexcel K2=("wealth")
putexcel K3=("Medium")
putexcel K4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel K5=matrix(a)


*3rd tertile 
mi estimate, esampvaryok: svy: reg casp_tot_w10 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.work covcase1##i.cvnump_cat casp_tot_w9 cvvuln heill_updated ///
if wealth_ter ==3


mimrgns if covcase1 == 0, esampvaryok
putexcel set "s2_predicted values_qol.xls", modify
putexcel L1=("qol")
putexcel L2=("wealth")
putexcel L3=("Low")
putexcel L4=("COVID-19 infection, No")
matrix a = r(table)
putexcel L5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_qol.xls", modify
putexcel M1=("qol")
putexcel M2=("wealth")
putexcel M3=("Low")
putexcel M4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel M5=matrix(a)

** sex

*men
mi estimate, esampvaryok: svy: reg casp_tot_w10 covcase1##i.wealth_ter covcase1##i.agegr3 ///
covcase1##i.work covcase1##i.cvnump_cat casp_tot_w9 cvvuln heill_updated ///
if sex == 0


mimrgns if covcase1 == 0, esampvaryok
putexcel set "s2_predicted values_qol.xls", modify
putexcel N1=("qol")
putexcel N2=("sex")
putexcel N3=("Men")
putexcel N4=("COVID-19 infection, No")
matrix a = r(table)
putexcel N5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_qol.xls", modify
putexcel O1=("qol")
putexcel O2=("sex")
putexcel O3=("Men")
putexcel O4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel O5=matrix(a)


*women 

mi estimate, esampvaryok: svy: reg casp_tot_w10 covcase1##i.wealth_ter covcase1##i.agegr3 ///
covcase1##i.work covcase1##i.cvnump_cat casp_tot_w9 cvvuln heill_updated ///
if sex == 1

mimrgns if covcase1 == 0, esampvaryok
putexcel set "s2_predicted values_qol.xls", modify
putexcel P1=("qol")
putexcel P2=("sex")
putexcel P3=("Women")
putexcel P4=("COVID-19 infection, No")
matrix a = r(table)
putexcel P5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_qol.xls", modify
putexcel Q1=("qol")
putexcel Q2=("sex")
putexcel Q3=("Women")
putexcel Q4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel Q5=matrix(a)


*** Loneliness: work status (w1) and age (w2)

use "maindata_s2_imp.dta", replace 

** work status 
*Employed 
mi estimate, esampvaryok: svy: reg cv_loneliness_w10 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.wealth_ter covcase1##i.cvnump_cat cv_loneliness_w9 cvvuln heill_updated ///
if work ==1


mimrgns if covcase1 == 0, esampvaryok

putexcel set "s2_predicted values_lone.xls", replace

putexcel A1= ("Outcome")
putexcel A2= ("Group")
putexcel A3= ("Category")
putexcel A4= ("Wave")
putexcel A5= ("Estimate")
putexcel A6= ("se")
putexcel A7= ("t")
putexcel A8= ("p")
putexcel A9= ("cil")
putexcel A10= ("ciu")

putexcel B1=("lone")
putexcel B2=("work")
putexcel B3=("Employed")
putexcel B4=("COVID-19 infection, No")
matrix a = r(table)
putexcel B5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_lone.xls", modify
putexcel C1=("lone")
putexcel C2=("work")
putexcel C3=("Employed")
putexcel C4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel C5=matrix(a)

*retired
mi estimate, esampvaryok: svy: reg cv_loneliness_w10 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.wealth_ter covcase1##i.cvnump_cat cv_loneliness_w9 cvvuln heill_updated ///
if work ==2

mimrgns if covcase1 ==0  , esampvaryok
putexcel set "s2_predicted values_lone.xls", modify
putexcel D1=("lone")
putexcel D2=("work")
putexcel D3=("Retired")
putexcel D4=("COVID-19 infection, No")
matrix a = r(table)
putexcel D5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok
putexcel set "s2_predicted values_lone.xls", modify
putexcel E1=("lone")
putexcel E2=("work")
putexcel E3=("Retired")
putexcel E4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel E5=matrix(a)


*Other not working 
mi estimate, esampvaryok: svy: reg cv_loneliness_w10 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.wealth_ter covcase1##i.cvnump_cat cv_loneliness_w9 cvvuln heill_updated ///
if work ==3

mimrgns if covcase1 ==0, esampvaryok
putexcel set "s2_predicted values_lone.xls", modify
putexcel F1=("lone")
putexcel F2=("work")
putexcel F3=("Other not working")
putexcel F4=("COVID-19 infection, No")
matrix a = r(table)
putexcel F5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok
putexcel set "s2_predicted values_lone.xls", modify
putexcel G1=("lone")
putexcel G2=("work")
putexcel G3=("Other not working")
putexcel G4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel G5=matrix(a)

** age

*50-59 
mi estimate, esampvaryok: svy: reg cv_loneliness_w11 covcase1##i.sex covcase1##i.work ///
covcase1##i.wealth_ter covcase1##i.cvnump_cat cv_loneliness_w9 cvvuln heill_updated ///
if agegr3 ==52


mimrgns if covcase1 == 0, esampvaryok
putexcel set "s2_predicted values_lone.xls", modify
putexcel H1=("lone")
putexcel H2=("age")
putexcel H3=("50-59")
putexcel H4=("COVID-19 infection, No")
matrix a = r(table)
putexcel H5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_lone.xls", modify
putexcel I1=("lone")
putexcel I2=("age")
putexcel I3=("50-59")
putexcel I4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel I5=matrix(a)


*60-74
mi estimate, esampvaryok: svy: reg cv_loneliness_w11 covcase1##i.sex covcase1##i.work ///
covcase1##i.wealth_ter covcase1##i.cvnump_cat cv_loneliness_w9 cvvuln heill_updated ///
if agegr3 ==60


mimrgns if covcase1 == 0, esampvaryok
putexcel set "s2_predicted values_lone.xls", modify
putexcel J1=("lone")
putexcel J2=("age")
putexcel J3=("60-74")
putexcel J4=("COVID-19 infection, No")
matrix a = r(table)
putexcel J5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_lone.xls", modify
putexcel K1=("lone")
putexcel K2=("age")
putexcel K3=("60-74")
putexcel K4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel K5=matrix(a)


*75+
mi estimate, esampvaryok: svy: reg cv_loneliness_w11 covcase1##i.sex covcase1##i.work ///
covcase1##i.wealth_ter covcase1##i.cvnump_cat cv_loneliness_w9 cvvuln heill_updated ///
if agegr3 ==75


mimrgns if covcase1 == 0, esampvaryok
putexcel set "s2_predicted values_lone.xls", modify
putexcel L1=("lone")
putexcel L2=("age")
putexcel L3=("75+")
putexcel L4=("COVID-19 infection, No")
matrix a = r(table)
putexcel L5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_lone.xls", modify
putexcel M1=("lone")
putexcel M2=("age")
putexcel M3=("75+")
putexcel M4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel M5=matrix(a)

*** Worries (w1) and Worse off (w2): work status 


use "maindata_s2_imp.dta", replace 

** worried: work status 
*Employed 

mi estimate, esampvaryok: svy: reg fins_worried_w10 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.wealth_ter covcase1##i.cvnump_cat fins_precov  cvvuln heill_updated if work ==1

mimrgns if covcase1 == 0, esampvaryok

putexcel set "s2_predicted values_fin.xls", replace

putexcel A1= ("Outcome")
putexcel A2= ("Group")
putexcel A3= ("Category")
putexcel A4= ("Wave")
putexcel A5= ("Estimate")
putexcel A6= ("se")
putexcel A7= ("t")
putexcel A8= ("p")
putexcel A9= ("cil")
putexcel A10= ("ciu")

putexcel B1=("fin_worried")
putexcel B2=("work")
putexcel B3=("Employed")
putexcel B4=("COVID-19 infection, No")
matrix a = r(table)
putexcel B5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_fin.xls", modify
putexcel C1=("fin_worried")
putexcel C2=("work")
putexcel C3=("Employed")
putexcel C4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel C5=matrix(a)

*retired
mi estimate, esampvaryok: svy: reg fins_worried_w10 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.wealth_ter covcase1##i.cvnump_cat fins_precov  cvvuln heill_updated /// 
if work ==2

mimrgns if covcase1 ==0  , esampvaryok
putexcel set "s2_predicted values_fin.xls", modify
putexcel D1=("fin_worried")
putexcel D2=("work")
putexcel D3=("Retired")
putexcel D4=("COVID-19 infection, No")
matrix a = r(table)
putexcel D5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok
putexcel set "s2_predicted values_fin.xls", modify
putexcel E1=("fin_worried")
putexcel E2=("work")
putexcel E3=("Retired")
putexcel E4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel E5=matrix(a)


*Other not working 
mi estimate, esampvaryok: svy: reg fins_worried_w10 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.wealth_ter covcase1##i.cvnump_cat fins_precov  cvvuln heill_updated /// 
if work ==3

mimrgns if covcase1 ==0, esampvaryok
putexcel set "s2_predicted values_fin.xls", modify
putexcel F1=("fin_worried")
putexcel F2=("work")
putexcel F3=("Other not working")
putexcel F4=("COVID-19 infection, No")
matrix a = r(table)
putexcel F5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok 
putexcel set "s2_predicted values_fin.xls", modify
putexcel G1=("fin_worried")
putexcel G2=("work")
putexcel G3=("Other not working")
putexcel G4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel G5=matrix(a)

** Worse off: work 

*Employed
mi estimate, esampvaryok: svy: reg fins_worse_w11 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.wealth_ter covcase1##i.cvnump_cat fins_precov cvvuln heill_updated ///
if work == 1


mimrgns if covcase1 == 0, esampvaryok
putexcel set "s2_predicted values_fin.xls", modify
putexcel H1=("fin_worse")
putexcel H2=("work")
putexcel H3=("Employed")
putexcel H4=("COVID-19 infection, No")
matrix a = r(table)
putexcel H5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_fin.xls", modify
putexcel I1=("fin_worse")
putexcel I2=("work")
putexcel I3=("Employed")
putexcel I4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel I5=matrix(a)


*Retired 
mi estimate, esampvaryok: svy: reg fins_worse_w11 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.wealth_ter covcase1##i.cvnump_cat fins_precov cvvuln heill_updated ///
if work == 2


mimrgns if covcase1 == 0, esampvaryok
putexcel set "s2_predicted values_fin.xls", modify
putexcel J1=("fin_worse")
putexcel J2=("work")
putexcel J3=("Retired")
putexcel J4=("COVID-19 infection, No")
matrix a = r(table)
putexcel J5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_fin.xls", modify
putexcel K1=("fin_worse")
putexcel K2=("work")
putexcel K3=("Retired")
putexcel K4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel K5=matrix(a)


*Other not working 
mi estimate, esampvaryok: svy: reg fins_worse_w11 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.wealth_ter covcase1##i.cvnump_cat fins_precov cvvuln heill_updated ///
if work == 3

mimrgns if covcase1 == 0, esampvaryok
putexcel set "s2_predicted values_fin.xls", modify
putexcel L1=("fin_worse")
putexcel L2=("work")
putexcel L3=("Other not working")
putexcel L4=("COVID-19 infection, No")
matrix a = r(table)
putexcel L5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_fin.xls", modify
putexcel M1=("fin_worse")
putexcel M2=("work")
putexcel M3=("Other not working")
putexcel M4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel M5=matrix(a)


*** Social connections 

** family w1: wealth 

*high 
mi estimate, esampvaryok: svy: reg frcont_fam_w10 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.work covcase1##i.cvnump_cat isolation_binary_w9 cvvuln heill_updated ///
if wealth_ter == 1

mimrgns if covcase1 == 0, esampvaryok

putexcel set "s2_predicted values_con.xls", replace

putexcel A1= ("Outcome")
putexcel A2= ("Group")
putexcel A3= ("Category")
putexcel A4= ("Wave")
putexcel A5= ("Estimate")
putexcel A6= ("se")
putexcel A7= ("t")
putexcel A8= ("p")
putexcel A9= ("cil")
putexcel A10= ("ciu")

putexcel B1=("con_family")
putexcel B2=("wealth")
putexcel B3=("High")
putexcel B4=("COVID-19 infection, No")
matrix a = r(table)
putexcel B5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_con.xls", modify
putexcel C1=("con_family")
putexcel C2=("wealth")
putexcel C3=("High")
putexcel C4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel C5=matrix(a)

*medium
mi estimate, esampvaryok: svy: reg frcont_fam_w10 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.work covcase1##i.cvnump_cat isolation_binary_w9 cvvuln heill_updated ///
if wealth_ter == 2

mimrgns if covcase1 ==0  , esampvaryok
putexcel set "s2_predicted values_con.xls", modify
putexcel D1=("con_family")
putexcel D2=("wealth")
putexcel D3=("Medium")
putexcel D4=("COVID-19 infection, No")
matrix a = r(table)
putexcel D5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok
putexcel set "s2_predicted values_con.xls", modify
putexcel E1=("con_family")
putexcel E2=("wealth")
putexcel E3=("Medium")
putexcel E4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel E5=matrix(a)


*low
mi estimate, esampvaryok: svy: reg frcont_fam_w10 covcase1##i.sex covcase1##i.agegr3 ///
covcase1##i.work covcase1##i.cvnump_cat isolation_binary_w9 cvvuln heill_updated ///
if wealth_ter == 3

mimrgns if covcase1 ==0, esampvaryok
putexcel set "s2_predicted values_con.xls", modify
putexcel F1=("con_family")
putexcel F2=("wealth")
putexcel F3=("Low")
putexcel F4=("COVID-19 infection, No")
matrix a = r(table)
putexcel F5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok
putexcel set "s2_predicted values_con.xls", modify
putexcel G1=("con_family")
putexcel G2=("wealth")
putexcel G3=("Low")
putexcel G4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel G5=matrix(a)


** Friends w1: gender 

*men
mi estimate, esampvaryok: svy: reg frcont_fri_w10 covcase1##i.wealth_ter covcase1##i.agegr3 ///
covcase1##i.work covcase1##i.cvnump_cat isolation_binary_w9 cvvuln heill_updated ///
if sex == 0

mimrgns if covcase1 == 0, esampvaryok
putexcel set "s2_predicted values_con.xls", modify
putexcel H1=("con_friend")
putexcel H2=("sex")
putexcel H3=("Men")
putexcel H4=("COVID-19 infection, No")
matrix a = r(table)
putexcel H5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_con.xls", modify
putexcel I1=("con_friend")
putexcel I2=("sex")
putexcel I3=("Men")
putexcel I4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel I5=matrix(a)


*Women
mi estimate, esampvaryok: svy: reg frcont_fri_w10 covcase1##i.wealth_ter covcase1##i.agegr3 ///
covcase1##i.work covcase1##i.cvnump_cat isolation_binary_w9 cvvuln heill_updated ///
if sex == 1

mimrgns if covcase1 == 0, esampvaryok
putexcel set "s2_predicted values_con.xls", modify
putexcel J1=("con_friend")
putexcel J2=("sex")
putexcel J3=("Women")
putexcel J4=("COVID-19 infection, No")
matrix a = r(table)
putexcel J5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_con.xls", modify
putexcel K1=("con_friend")
putexcel K2=("sex")
putexcel K3=("Women")
putexcel K4=("COVID-19 infection, Yes")
matrix a = r(table)
putexcel K5=matrix(a)


*** Overall predicted changes for all outcomes 

use "maindata_s2_imp_long2.dta", replace 

mi xtset idauniq wave


* depression 
mi estimate: xtreg cesd8_binary i.wave [pweight = cov19lwgtw2b] , fe 

putexcel set "s2_predicted values.traj.xls", replace 
putexcel A1= ("Outcome")
putexcel A2= ("Wave")
putexcel A3= ("Estimate")
putexcel A4= ("se")
putexcel A5= ("t")
putexcel A6= ("p")
putexcel A7= ("cil")
putexcel A8= ("ciu")

mimrgns if wave ==1
putexcel B1=("dep")
putexcel B2=("COVID-19 w1")
matrix a = r(table)
putexcel B3=matrix(a)

mimrgns if wave  == 2
putexcel C1=("dep")
putexcel C2=("COVID-19 w2")
matrix a = r(table)
putexcel C3=matrix(a)

* anxiety 

mi estimate: xtreg gad7_binary i.wave [pweight = cov19lwgtw2b] , fe 

mimrgns if wave ==1
putexcel D1=("anx")
putexcel D2=("COVID-19 w1")
matrix a = r(table)
putexcel D3=matrix(a)

mimrgns if wave  == 2
putexcel E1=("anx")
putexcel E2=("COVID-19 w2")
matrix a = r(table)
putexcel E3=matrix(a)

* QoL
mi estimate: xtreg casp_tot i.wave [pweight = cov19lwgtw2b] , fe 

mimrgns if wave ==1
putexcel F1=("casp")
putexcel F2=("COVID-19 w1")
matrix a = r(table)
putexcel F3=matrix(a)

mimrgns if wave  == 2
putexcel G1=("casp")
putexcel G2=("COVID-19 w2")
matrix a = r(table)
putexcel G3=matrix(a)

* lone 
mi estimate: xtreg cv_loneliness i.wave [pweight = cov19lwgtw2b] , fe 

mimrgns if wave ==1
putexcel H1=("lone")
putexcel H2=("COVID-19 w1")
matrix a = r(table)
putexcel H3=matrix(a)

mimrgns if wave  == 2
putexcel I1=("lone")
putexcel I2=("COVID-19 w2")
matrix a = r(table)
putexcel I3=matrix(a)

* fin worried 

mi estimate: xtreg fins_worried i.wave [pweight = cov19lwgtw2b] , fe 

mimrgns if wave ==1
putexcel J1=("worried")
putexcel J2=("COVID-19 w1")
matrix a = r(table)
putexcel J3=matrix(a)

mimrgns if wave  == 2
putexcel K1=("worried")
putexcel K2=("COVID-19 w2")
matrix a = r(table)
putexcel K3=matrix(a)

* fin worse 
mi estimate: xtreg fins_worse i.wave [pweight = cov19lwgtw2b] , fe 

mimrgns if wave ==1
putexcel L1=("worse")
putexcel L2=("COVID-19 w1")
matrix a = r(table)
putexcel L3=matrix(a)

mimrgns if wave  == 2
putexcel M1=("worse")
putexcel M2=("COVID-19 w2")
matrix a = r(table)
putexcel M3=matrix(a)

* contact family 

mi estimate: xtreg frcont_fam i.wave [pweight = cov19lwgtw2b] , fe 

mimrgns if wave ==1
putexcel N1=("contact_family")
putexcel N2=("COVID-19 w1")
matrix a = r(table)
putexcel N3=matrix(a)

mimrgns if wave  == 2
putexcel O1=("contact_family")
putexcel O2=("COVID-19 w2")
matrix a = r(table)
putexcel O3=matrix(a)

* contact friends 
mi estimate: xtreg frcont_fri i.wave [pweight = cov19lwgtw2b] , fe 

mimrgns if wave ==1
putexcel P1=("contact_friend")
putexcel P2=("COVID-19 w1")
matrix a = r(table)
putexcel P3=matrix(a)

mimrgns if wave  == 2
putexcel Q1=("contact_friend")
putexcel Q2=("COVID-19 w2")
matrix a = r(table)
putexcel Q3=matrix(a)

* contact total 

mi estimate: xtreg frcont_tot i.wave [pweight = cov19lwgtw2b] , fe 

mimrgns if wave ==1
putexcel R1=("contact_total")
putexcel R2=("COVID-19 w1")
matrix a = r(table)
putexcel R3=matrix(a)

mimrgns if wave  == 2
putexcel S1=("contact_total")
putexcel S2=("COVID-19 w2")
matrix a = r(table)
putexcel S3=matrix(a)


*** interaction with covid infection on change in social connections 

use "maindata_s2_imp_long2.dta", replace 

mi xtset idauniq wave


mi estimate: xtreg frcont_tot i.wave [pweight = cov19lwgtw2b] if covcase1 == 1 , fe 

mimrgns if wave == 1, esampvaryok
putexcel set "predicted values.contact.change.xls", replace

putexcel A1= ("Outcome")
putexcel A2= ("Group")
putexcel A3= ("Category")
putexcel A4= ("Wave")
putexcel A5= ("Estimate")
putexcel A6= ("se")
putexcel A7= ("t")
putexcel A8= ("p")
putexcel A9= ("cil")
putexcel A10= ("ciu")

putexcel B1=("contact")
putexcel B2=("infection")
putexcel B3=("COVID-19 infection, No")
putexcel B4=("COVID-19 w1")
matrix a = r(table)
putexcel B5=matrix(a)

mimrgns if wave ==2 , esampvaryok
putexcel set "predicted values.contact.change.xls", modify
putexcel C1=("contact")
putexcel C2=("infection")
putexcel C3=("COVID-19 infection, No")
putexcel C4=("COVID-19 w2")
matrix a = r(table)
putexcel C5=matrix(a)


mi estimate: xtreg frcont_tot i.wave [pweight = cov19lwgtw2b] if covcase1 == 2 , fe 

mimrgns if wave == 1, esampvaryok
putexcel D1=("contact")
putexcel D2=("infection")
putexcel D3=("COVID-19 infection, Yes")
putexcel D4=("COVID-19 w1")
matrix a = r(table)
putexcel D5=matrix(a)

mimrgns if wave ==2 , esampvaryok
putexcel E1=("contact")
putexcel E2=("infection")
putexcel E3=("COVID-19 infection, Yes")
putexcel E4=("COVID-19 w2")
matrix a = r(table)
putexcel E5=matrix(a)

*** find out break down of other not working
use "ELSA covid19_data2_w9_w8.dta"
keep idauniq  cvpred
sort idauniq
save work_breakdown


use "maindata_s2_imp.dta", replace 

mi extract 0
sort idauniq 
save maindata_s2_complete

merge 1:1 idauniq using "work_breakdown.dta" ,generate(_merge6) 
save maindata_s2_complete, replace

svy:tab cvpred work if anasample ==1 
svy:tab cvpred if anasample ==1 


*** Complete-case analyses 

*** Depression 

clear
save dep.complete, emptyok
use "maindata_s2_complete.dta", replace 

local exp i.covcase1 
foreach o of local exp {

svy: logit cesd8_binary_w10 `o' sex i.agegr3 cesd8_binary_w9
regsave using dep.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit cesd8_binary_w10 `o' sex i.agegr3 cesd8_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using dep.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit cesd8_binary_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat cesd8_binary_w9 cvvuln heill_updated
regsave using dep.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit cesd8_binary_w11 `o' sex i.agegr3 cesd8_binary_w9 cov_w11
regsave using dep.complete, pval ci append addlabel(outcome, `o' w11)

svy: logit cesd8_binary_w11 `o' sex i.agegr3 cesd8_binary_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using dep.complete, pval ci append addlabel(outcome, `o' w11)

svy: logit cesd8_binary_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat cesd8_binary_w9 cov_w11 cvvuln heill_updated
regsave using dep.complete, pval ci append  addlabel(outcome, `o' w11)

}

use dep.complete, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "dep.complete.xlsx", firstrow(variables)



*** Anxiety 
clear
save anx.complete, emptyok
use "maindata_s2_complete.dta", replace 

local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

svy: logit gad7_binary_w10 `o' sex i.agegr3 scovan_w9
regsave using anx.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit gad7_binary_w10 `o' sex i.agegr3 scovan_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using anx.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit gad7_binary_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat scovan_w9 cvvuln heill_updated
regsave using anx.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit gad7_binary_w11 `o' sex i.agegr3 scovan_w9 cov_w11
regsave using anx.complete, pval ci append addlabel(outcome, `o' w11)

svy: logit gad7_binary_w11 `o' sex i.agegr3 scovan_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using anx.complete, pval ci append addlabel(outcome, `o' w11)

svy: logit gad7_binary_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat scovan_w9 cov_w11 cvvuln heill_updated
regsave using anx.complete, pval ci append addlabel(outcome, `o' w11)

}

use anx.complete, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "anx.complete.xlsx", firstrow(variables)




*** Poor QoL

clear
save qol.complete, emptyok
use "maindata_s2_complete.dta", replace 


local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

svy: reg casp_tot_w10 `o' sex i.agegr3 casp_tot_w9
regsave using qol.complete, pval ci append addlabel(outcome, `o' w10)

svy: reg casp_tot_w10 `o' sex i.agegr3 casp_tot_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using qol.complete, pval ci append addlabel(outcome, `o' w10)

svy: reg casp_tot_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat casp_tot_w9 cvvuln heill_updated
regsave using qol.complete, pval ci append addlabel(outcome, `o' w10)

svy: reg casp_tot_w11 `o' sex i.agegr3 casp_tot_w9 cov_w11
regsave using qol.complete, pval ci append addlabel(outcome, `o' w11)

svy: reg casp_tot_w11 `o' sex i.agegr3 casp_tot_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using qol.complete, pval ci append addlabel(outcome, `o' w11)

svy: reg casp_tot_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat casp_tot_w9 cov_w11 cvvuln heill_updated
regsave using qol.complete, pval ci append addlabel(outcome, `o' w11)

}

use qol.complete, replace
export excel using "qol.complete.xlsx", firstrow(variables)



*** Loneliness 

clear
save lone.complete, emptyok
use "maindata_s2_complete.dta", replace 
 
local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

svy: reg cv_loneliness_w10 `o' sex i.agegr3 cv_loneliness_w9
regsave using lone.complete, pval ci append addlabel(outcome, `o' w10)

svy: reg cv_loneliness_w10 `o' sex i.agegr3 cv_loneliness_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using lone.complete, pval ci append addlabel(outcome, `o' w10)

svy: reg cv_loneliness_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat cv_loneliness_w9 cvvuln heill_updated
regsave using lone.complete, pval ci append addlabel(outcome, `o' w10)

svy: reg cv_loneliness_w11 `o' sex i.agegr3 cv_loneliness_w9 cov_w11
regsave using lone.complete, pval ci append addlabel(outcome, `o' w11)

svy: reg cv_loneliness_w11 `o' sex i.agegr3 cv_loneliness_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using lone.complete, pval ci append addlabel(outcome, `o' w11)

svy: reg cv_loneliness_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat cv_loneliness_w9 cov_w11 cvvuln heill_updated
regsave using lone.complete, pval ci append addlabel(outcome, `o' w11)

}

use lone.complete, replace
export excel using "lone.complete.xlsx", firstrow(variables)


*** financial situation: worried 

clear
save finworried.complete, emptyok
use "maindata_s2_complete.dta", replace 


local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

svy: logit fins_worried_w10 `o' sex i.agegr3 fins_precov
regsave using finworried.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit fins_worried_w10 `o' sex i.agegr3 fins_precov ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using finworried.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit fins_worried_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat fins_precov  cvvuln heill_updated
regsave using finworried.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit fins_worried_w11 `o' sex i.agegr3 fins_precov cov_w11
regsave using finworried.complete, pval ci append addlabel(outcome, `o' w11)

svy: logit fins_worried_w11 `o' sex i.agegr3 fins_precov cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using finworried.complete, pval ci append addlabel(outcome, `o' w11)

svy: logit fins_worried_w11 `o'##i.sex `o'##i.agegr3  ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat fins_precov cov_w11  cvvuln heill_updated
regsave using finworried.complete, pval ci append addlabel(outcome, `o' w11)

}

use finworried.complete, replace

gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "finworried.complete.xlsx", firstrow(variables)



*** financial situation: worse

clear
save finworse.complete, emptyok
use "maindata_s2_complete.dta", replace 


local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

svy: logit fins_worse_w10 `o' sex i.agegr3 fins_precov
regsave using finworse.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit fins_worse_w10 `o' sex i.agegr3 fins_precov ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using finworse.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit fins_worse_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat fins_precov cvvuln heill_updated
regsave using finworse.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit fins_worse_w11 `o' sex i.agegr3 fins_precov cov_w11
regsave using finworse.complete, pval ci append addlabel(outcome, `o' w11)

svy: logit fins_worse_w11 `o' sex i.agegr3 fins_precov cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using finworse.complete, pval ci append addlabel(outcome, `o' w11)

svy: logit fins_worse_w11 `o'##i.sex `o'##i.agegr3  ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat fins_precov cov_w11 cvvuln heill_updated
regsave using finworse.complete, pval ci append addlabel(outcome, `o' w11)

}

use finworse.complete, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "finworse.complete.xlsx", firstrow(variables)



*** social contact: family 

clear
save scfam.complete, emptyok
use "maindata_s2_complete.dta", replace 


local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

svy: logit frcont_fam_w10 `o' sex i.agegr3 isolation_binary_w9
regsave using scfam.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit frcont_fam_w10 `o' sex i.agegr3 isolation_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using scfam.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit frcont_fam_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cvvuln heill_updated
regsave using scfam.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit frcont_fam_w11 `o' sex i.agegr3 isolation_binary_w9 ///
 cov_w11
regsave using scfam.complete, pval ci append addlabel(outcome, `o' w11)

svy: logit frcont_fam_w11 `o' sex i.agegr3 isolation_binary_w9 ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using scfam.complete, pval ci append addlabel(outcome, `o' w11)

svy: logit frcont_fam_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cov_w11 cvvuln heill_updated
regsave using scfam.complete, pval ci append addlabel(outcome, `o' w11)

}

use scfam.complete, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "scfam.complete.xlsx", firstrow(variables)


*** social contact: friends 

clear
save scfrie.complete, emptyok
use "maindata_s2_complete.dta", replace 


local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

svy: logit frcont_fri_w10 `o' sex i.agegr3 isolation_binary_w9
regsave using scfrie.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit frcont_fri_w10 `o' sex i.agegr3 isolation_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using scfrie.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit frcont_fri_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cvvuln heill_updated
regsave using scfrie.complete, pval ci append addlabel(outcome, `o' w10)

svy: logit frcont_fri_w11 `o' sex i.agegr3 isolation_binary_w9 ///
 cov_w11
regsave using scfrie.complete, pval ci append addlabel(outcome, `o' w11)

svy: logit frcont_fri_w11 `o' sex i.agegr3 isolation_binary_w9 ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using scfrie.complete, pval ci append addlabel(outcome, `o' w11)

svy: logit frcont_fri_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cov_w11 cvvuln heill_updated
regsave using scfrie.complete, pval ci append addlabel(outcome, `o' w11)

}

use scfrie.complete, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "scfrie.complete.xlsx", firstrow(variables)



*** social contact: all 
clear
save sctot.complete, emptyok
use "maindata_s2_complete.dta", replace 


local exp i.covcase1 i.covcase2 i.covcase3

foreach o of local exp {

svy: reg frcont_tot_w10 `o' sex i.agegr3 isolation_binary_w9
regsave using sctot.complete, pval ci append addlabel(outcome, `o' w10)

svy: reg frcont_tot_w10 `o' sex i.agegr3 isolation_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using sctot.complete, pval ci append addlabel(outcome, `o' w10)

svy: reg frcont_tot_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cvvuln heill_updated
regsave using sctot.complete, pval ci append addlabel(outcome, `o' w10)

svy: reg frcont_tot_w11 `o' sex i.agegr3 isolation_binary_w9 ///
 cov_w11
regsave using sctot.complete, pval ci append addlabel(outcome, `o' w11)

svy: reg frcont_tot_w11 `o' sex i.agegr3 isolation_binary_w9 ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using sctot.complete, pval ci append addlabel(outcome, `o' w11)

svy: reg frcont_tot_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat isolation_binary_w9 cov_w11 cvvuln heill_updated
regsave using sctot.complete, pval ci append addlabel(outcome, `o' w11)

}

use sctot.complete, replace
export excel using "sctot.complete.xlsx", firstrow(variables)


*** Predicted outcome values for those with and without covid 

use "maindata_s2_imp.dta", replace 

** depression

*wave 1

mi estimate, or: svy: logit cesd8_binary_w10 covcase1 sex i.agegr3 ///
cesd8_binary_w9  i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated


mimrgns if covcase1 == 0, esampvaryok

putexcel set "s2_predicted values_covid_dep.xls", replace

putexcel A1= ("Outcome")
putexcel A2= ("Group")
putexcel A3= ("Category")
putexcel A4= ("Wave")
putexcel A5= ("Estimate")
putexcel A6= ("se")
putexcel A7= ("t")
putexcel A8= ("p")
putexcel A9= ("cil")
putexcel A10= ("ciu")

putexcel B1=("dep")
putexcel B2=("covid")
putexcel B3=("no")
putexcel B4=("w1")
matrix a = r(table)
putexcel B5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_covid_dep.xls", modify
putexcel C1=("dep")
putexcel C2=("covid")
putexcel C3=("yes")
putexcel C4=("w1")
matrix a = r(table)
putexcel C5=matrix(a)


*wave 2 
mi estimate, or: svy: logit cesd8_binary_w11 covcase1 sex i.agegr3 ///
cesd8_binary_w9  cov_w11  i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 ==0  , esampvaryok
putexcel set "s2_predicted values_covid_dep.xls", modify
putexcel D1=("dep")
putexcel D2=("covid")
putexcel D3=("no")
putexcel D4=("w2")
matrix a = r(table)
putexcel D5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok
putexcel set "s2_predicted values_covid_dep.xls", modify
putexcel E1=("dep")
putexcel E2=("covid")
putexcel E3=("yes")
putexcel E4=("w2")
matrix a = r(table)
putexcel E5=matrix(a)

** anxiety 

*wave 1

mi estimate, or: svy: logit gad7_binary_w10 covcase1 sex i.agegr3 scovan_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 == 0, esampvaryok

putexcel set "s2_predicted values_covid_anx.xls", replace

putexcel A1= ("Outcome")
putexcel A2= ("Group")
putexcel A3= ("Category")
putexcel A4= ("Wave")
putexcel A5= ("Estimate")
putexcel A6= ("se")
putexcel A7= ("t")
putexcel A8= ("p")
putexcel A9= ("cil")
putexcel A10= ("ciu")

putexcel B1=("anx")
putexcel B2=("covid")
putexcel B3=("no")
putexcel B4=("w1")
matrix a = r(table)
putexcel B5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_covid_anx.xls", modify
putexcel C1=("anx")
putexcel C2=("covid")
putexcel C3=("yes")
putexcel C4=("w1")
matrix a = r(table)
putexcel C5=matrix(a)


*wave 2 
mi estimate, or: svy: logit gad7_binary_w11 covcase1 sex i.agegr3 scovan_w9  ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 ==0  , esampvaryok
putexcel set "s2_predicted values_covid_anx.xls", modify
putexcel D1=("anx")
putexcel D2=("covid")
putexcel D3=("no")
putexcel D4=("w2")
matrix a = r(table)
putexcel D5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok
putexcel set "s2_predicted values_covid_anx.xls", modify
putexcel E1=("anx")
putexcel E2=("covid")
putexcel E3=("yes")
putexcel E4=("w2")
matrix a = r(table)
putexcel E5=matrix(a)

** QoL
*wave 1

mi estimate: svy: reg casp_tot_w10 covcase1 sex i.agegr3 casp_tot_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 == 0, esampvaryok

putexcel set "s2_predicted values_covid_qol.xls", replace

putexcel A1= ("Outcome")
putexcel A2= ("Group")
putexcel A3= ("Category")
putexcel A4= ("Wave")
putexcel A5= ("Estimate")
putexcel A6= ("se")
putexcel A7= ("t")
putexcel A8= ("p")
putexcel A9= ("cil")
putexcel A10= ("ciu")

putexcel B1=("qol")
putexcel B2=("covid")
putexcel B3=("no")
putexcel B4=("w1")
matrix a = r(table)
putexcel B5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_covid_qol.xls", modify
putexcel C1=("qol")
putexcel C2=("covid")
putexcel C3=("yes")
putexcel C4=("w1")
matrix a = r(table)
putexcel C5=matrix(a)


*wave 2 
mi estimate: svy: reg casp_tot_w11 covcase1 sex i.agegr3 casp_tot_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 ==0  , esampvaryok
putexcel set "s2_predicted values_covid_qol.xls", modify
putexcel D1=("qol")
putexcel D2=("covid")
putexcel D3=("no")
putexcel D4=("w2")
matrix a = r(table)
putexcel D5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok
putexcel set "s2_predicted values_covid_qol.xls", modify
putexcel E1=("qol")
putexcel E2=("covid")
putexcel E3=("yes")
putexcel E4=("w2")
matrix a = r(table)
putexcel E5=matrix(a)

** Loneliness

*wave 1

mi estimate: svy: reg cv_loneliness_w10 covcase1 sex i.agegr3 cv_loneliness_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 == 0, esampvaryok

putexcel set "s2_predicted values_covid_lone.xls", replace

putexcel A1= ("Outcome")
putexcel A2= ("Group")
putexcel A3= ("Category")
putexcel A4= ("Wave")
putexcel A5= ("Estimate")
putexcel A6= ("se")
putexcel A7= ("t")
putexcel A8= ("p")
putexcel A9= ("cil")
putexcel A10= ("ciu")

putexcel B1=("lone")
putexcel B2=("covid")
putexcel B3=("no")
putexcel B4=("w1")
matrix a = r(table)
putexcel B5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_covid_lone.xls", modify
putexcel C1=("lone")
putexcel C2=("covid")
putexcel C3=("yes")
putexcel C4=("w1")
matrix a = r(table)
putexcel C5=matrix(a)


*wave 2 
mi estimate: svy: reg cv_loneliness_w11 covcase1 sex i.agegr3 cv_loneliness_w9  ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 ==0  , esampvaryok
putexcel set "s2_predicted values_covid_lone.xls", modify
putexcel D1=("lone")
putexcel D2=("covid")
putexcel D3=("no")
putexcel D4=("w2")
matrix a = r(table)
putexcel D5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok
putexcel set "s2_predicted values_covid_lone.xls", modify
putexcel E1=("lone")
putexcel E2=("covid")
putexcel E3=("yes")
putexcel E4=("w2")
matrix a = r(table)
putexcel E5=matrix(a)

** financial hardship, worried 

*wave 1

mi estimate, or: svy: logit fins_worried_w10 covcase1 sex i.agegr3 fins_precov ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 == 0, esampvaryok predict(pr)

putexcel set "s2_predicted values_covid_worried.xls", replace

putexcel A1= ("Outcome")
putexcel A2= ("Group")
putexcel A3= ("Category")
putexcel A4= ("Wave")
putexcel A5= ("Estimate")
putexcel A6= ("se")
putexcel A7= ("t")
putexcel A8= ("p")
putexcel A9= ("cil")
putexcel A10= ("ciu")

putexcel B1=("worried")
putexcel B2=("covid")
putexcel B3=("no")
putexcel B4=("w1")
matrix a = r(table)
putexcel B5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok predict(pr)
putexcel set "s2_predicted values_covid_worried.xls", modify
putexcel C1=("worried")
putexcel C2=("covid")
putexcel C3=("yes")
putexcel C4=("w1")
matrix a = r(table)
putexcel C5=matrix(a)


*wave 2 
mi estimate, or: svy: logit fins_worried_w11 covcase1 sex i.agegr3 fins_precov ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 ==0  , esampvaryok predict(pr)
putexcel set "s2_predicted values_covid_worried.xls", modify
putexcel D1=("worried")
putexcel D2=("covid")
putexcel D3=("no")
putexcel D4=("w2")
matrix a = r(table)
putexcel D5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok predict(pr)
putexcel set "s2_predicted values_covid_worried.xls", modify
putexcel E1=("worried")
putexcel E2=("covid")
putexcel E3=("yes")
putexcel E4=("w2")
matrix a = r(table)
putexcel E5=matrix(a)

** financial hardship, worse 

*wave 1

mi estimate, or: svy: logit fins_worse_w10 covcase1 sex i.agegr3 fins_precov ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 == 0, esampvaryok predict(pr)

putexcel set "s2_predicted values_covid_worse.xls", replace

putexcel A1= ("Outcome")
putexcel A2= ("Group")
putexcel A3= ("Category")
putexcel A4= ("Wave")
putexcel A5= ("Estimate")
putexcel A6= ("se")
putexcel A7= ("t")
putexcel A8= ("p")
putexcel A9= ("cil")
putexcel A10= ("ciu")

putexcel B1=("worse")
putexcel B2=("covid")
putexcel B3=("no")
putexcel B4=("w1")
matrix a = r(table)
putexcel B5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok predict(pr)
putexcel set "s2_predicted values_covid_worse.xls", modify
putexcel C1=("worse")
putexcel C2=("covid")
putexcel C3=("yes")
putexcel C4=("w1")
matrix a = r(table)
putexcel C5=matrix(a)


*wave 2 
mi estimate, or: svy: logit fins_worse_w11 covcase1 sex i.agegr3 fins_precov ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 ==0  , esampvaryok predict(pr)
putexcel set "s2_predicted values_covid_worse.xls", modify
putexcel D1=("worse")
putexcel D2=("covid")
putexcel D3=("no")
putexcel D4=("w2")
matrix a = r(table)
putexcel D5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok predict(pr)
putexcel set "s2_predicted values_covid_worse.xls", modify
putexcel E1=("worse")
putexcel E2=("covid")
putexcel E3=("yes")
putexcel E4=("w2")
matrix a = r(table)
putexcel E5=matrix(a)


** Social contact, family 
*wave 1

mi estimate, or: svy: logit frcont_fam_w10 covcase1 sex i.agegr3  ///
isolation_binary_w9 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 == 0, esampvaryok

putexcel set "s2_predicted values_covid_cont_fam.xls", replace

putexcel A1= ("Outcome")
putexcel A2= ("Group")
putexcel A3= ("Category")
putexcel A4= ("Wave")
putexcel A5= ("Estimate")
putexcel A6= ("se")
putexcel A7= ("t")
putexcel A8= ("p")
putexcel A9= ("cil")
putexcel A10= ("ciu")

putexcel B1=("cont_fam")
putexcel B2=("covid")
putexcel B3=("no")
putexcel B4=("w1")
matrix a = r(table)
putexcel B5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_covid_cont_fam.xls", modify
putexcel C1=("cont_fam")
putexcel C2=("covid")
putexcel C3=("yes")
putexcel C4=("w1")
matrix a = r(table)
putexcel C5=matrix(a)


*wave 2 
mi estimate, or: svy: logit frcont_fam_w11 covcase1 sex i.agegr3  ///
isolation_binary_w9 cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 ==0  , esampvaryok
putexcel set "s2_predicted values_covid_cont_fam.xls", modify
putexcel D1=("cont_fam")
putexcel D2=("covid")
putexcel D3=("no")
putexcel D4=("w2")
matrix a = r(table)
putexcel D5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok
putexcel set "s2_predicted values_covid_cont_fam.xls", modify
putexcel E1=("cont_fam")
putexcel E2=("covid")
putexcel E3=("yes")
putexcel E4=("w2")
matrix a = r(table)
putexcel E5=matrix(a)

** Social contact, friends
*wave 1

mi estimate, or: svy: logit frcont_fri_w10 covcase1 sex i.agegr3  ///
isolation_binary_w9 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 == 0, esampvaryok

putexcel set "s2_predicted values_covid_cont_fri.xls", replace

putexcel A1= ("Outcome")
putexcel A2= ("Group")
putexcel A3= ("Category")
putexcel A4= ("Wave")
putexcel A5= ("Estimate")
putexcel A6= ("se")
putexcel A7= ("t")
putexcel A8= ("p")
putexcel A9= ("cil")
putexcel A10= ("ciu")

putexcel B1=("cont_fri")
putexcel B2=("covid")
putexcel B3=("no")
putexcel B4=("w1")
matrix a = r(table)
putexcel B5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_covid_cont_fri.xls", modify
putexcel C1=("cont_fri")
putexcel C2=("covid")
putexcel C3=("yes")
putexcel C4=("w1")
matrix a = r(table)
putexcel C5=matrix(a)


*wave 2 
mi estimate, or: svy: logit frcont_fri_w11 covcase1 sex i.agegr3  ///
isolation_binary_w9 cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 ==0  , esampvaryok
putexcel set "s2_predicted values_covid_cont_fri.xls", modify
putexcel D1=("cont_fri")
putexcel D2=("covid")
putexcel D3=("no")
putexcel D4=("w2")
matrix a = r(table)
putexcel D5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok
putexcel set "s2_predicted values_covid_cont_fri.xls", modify
putexcel E1=("cont_fri")
putexcel E2=("covid")
putexcel E3=("yes")
putexcel E4=("w2")
matrix a = r(table)
putexcel E5=matrix(a)

** Social contact, all
*wave 1

mi estimate: reg frcont_tot_w10 covcase1 sex i.agegr3 isolation_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 == 0, esampvaryok

putexcel set "s2_predicted values_covid_cont_tot.xls", replace

putexcel A1= ("Outcome")
putexcel A2= ("Group")
putexcel A3= ("Category")
putexcel A4= ("Wave")
putexcel A5= ("Estimate")
putexcel A6= ("se")
putexcel A7= ("t")
putexcel A8= ("p")
putexcel A9= ("cil")
putexcel A10= ("ciu")

putexcel B1=("cont_tot")
putexcel B2=("covid")
putexcel B3=("no")
putexcel B4=("w1")
matrix a = r(table)
putexcel B5=matrix(a)

mimrgns if covcase1 ==1 , esampvaryok 
putexcel set "s2_predicted values_covid_cont_tot.xls", modify
putexcel C1=("cont_tot")
putexcel C2=("covid")
putexcel C3=("yes")
putexcel C4=("w1")
matrix a = r(table)
putexcel C5=matrix(a)


*wave 2 
mi estimate: reg frcont_tot_w11 covcase1 sex i.agegr3 isolation_binary_w9 ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated

mimrgns if covcase1 ==0  , esampvaryok
putexcel set "s2_predicted values_covid_cont_tot.xls", modify
putexcel D1=("cont_tot")
putexcel D2=("covid")
putexcel D3=("no")
putexcel D4=("w2")
matrix a = r(table)
putexcel D5=matrix(a)

mimrgns if covcase1 ==1, esampvaryok
putexcel set "s2_predicted values_covid_cont_tot.xls", modify
putexcel E1=("cont_tot")
putexcel E2=("covid")
putexcel E3=("yes")
putexcel E4=("w2")
matrix a = r(table)
putexcel E5=matrix(a)



*** Dataset for new girl
use "maindata_s2_imp.dta", replace 

mi extract 0
sort idauniq 

save "covid elsa data_study2.dta"
use "covid elsa data_study2.dta", replace 


*** Sensitivity analysis: association of pre-pandemic mental health with covid-19 infection

clear
save sens_bidirectional, emptyok
use "maindata_s2_imp.dta", replace 

local outcome covcase1 

foreach o of local outcome {

mi estimate, or: svy: logit `o' cesd8_binary_w9 sex i.agegr3  ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using sens_bidirectional, pval ci append addlabel(exposure, dep)

mi estimate, or: svy: logit `o' scovan_w9 sex i.agegr3  ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using sens_bidirectional, pval ci append addlabel(exposure, anx)

mi estimate, or: svy: logit `o' cv_loneliness_w9 sex i.agegr3  ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using sens_bidirectional, pval ci append addlabel(exposure, lone)

mi estimate, or: svy: logit `o' casp_tot_w9 sex i.agegr3  ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated
regsave using sens_bidirectional, pval ci append addlabel(exposure, casp)


}

use sens_bidirectional, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "sens_bidirectional.xlsx", firstrow(variables)



***** New sensitivity analyses for the revision

* 1) adjustment and interactions with health conditions
* 2) associations of probable infection def 3 with the outcomes


* Set up main dataset for survey analysis 
use "maindata_s2_imp_rev.dta", replace 

gen psu=idauniq
mi svyset [pweight=cov19lwgtw2b], psu(psu) 

save "maindata_s2_imp_rev.dta", replace 


*** Depression 

clear
save dep.imputed_rev.sens1, emptyok
use "maindata_s2_imp_rev.dta", replace 

local exp i.covcase1 i.covcase3_rev

foreach o of local exp {

mi estimate, or: svy: logit cesd8_binary_w10 `o' sex i.agegr3 cesd8_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using dep.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit cesd8_binary_w10 `o' sex i.agegr3 cesd8_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using dep.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit cesd8_binary_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
cesd8_binary_w9 cvvuln heill_updated
regsave using dep.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit cesd8_binary_w11 `o' sex i.agegr3 cesd8_binary_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using dep.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit cesd8_binary_w11 `o' sex i.agegr3 cesd8_binary_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using dep.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit cesd8_binary_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
cesd8_binary_w9 cov_w11 cvvuln heill_updated
regsave using dep.imputed_rev.sens1, pval ci append  addlabel(outcome, `o' w11)

}

use dep.imputed_rev.sens1, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "dep.imputed_rev.sens1.xlsx", firstrow(variables)



*** Anxiety 
clear
save anx.imputed_rev.sens1, emptyok
use "maindata_s2_imp_rev.dta", replace 

local exp i.covcase1 i.covcase3_rev

foreach o of local exp {

mi estimate, or: svy: logit gad7_binary_w10 `o' sex i.agegr3 scovan_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using anx.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit gad7_binary_w10 `o' sex i.agegr3 scovan_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using anx.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit gad7_binary_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
scovan_w9 cvvuln heill_updated
regsave using anx.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit gad7_binary_w11 `o' sex i.agegr3 scovan_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using anx.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit gad7_binary_w11 `o' sex i.agegr3 scovan_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using anx.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit gad7_binary_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
scovan_w9 cov_w11 cvvuln heill_updated
regsave using anx.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

}

use anx.imputed_rev.sens1, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "anx.imputed_rev.sens1.xlsx", firstrow(variables)




*** Poor QoL

clear
save qol.imputed_rev.sens1, emptyok
use "maindata_s2_imp_rev.dta", replace 


local exp i.covcase1 i.covcase3_rev

foreach o of local exp {

mi estimate: svy: reg casp_tot_w10 `o' sex i.agegr3 casp_tot_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using qol.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate: svy: reg casp_tot_w10 `o' sex i.agegr3 casp_tot_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using qol.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate: svy: reg casp_tot_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
casp_tot_w9 cvvuln heill_updated
regsave using qol.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate: svy: reg casp_tot_w11 `o' sex i.agegr3 casp_tot_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using qol.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate: svy: reg casp_tot_w11 `o' sex i.agegr3 casp_tot_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using qol.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate: svy: reg casp_tot_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
casp_tot_w9 cov_w11 cvvuln heill_updated
regsave using qol.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

}

use qol.imputed_rev.sens1, replace
export excel using "qol.imputed_rev.sens1.xlsx", firstrow(variables)



*** Loneliness 

clear
save lone.imputed_rev.sens1, emptyok
use "maindata_s2_imp_rev.dta", replace 
 
local exp i.covcase1 i.covcase3_rev

foreach o of local exp {

mi estimate: svy: reg cv_loneliness_w10 `o' sex i.agegr3 cv_loneliness_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using lone.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate: svy: reg cv_loneliness_w10 `o' sex i.agegr3 cv_loneliness_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using lone.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate: svy: reg cv_loneliness_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
cv_loneliness_w9 cvvuln heill_updated
regsave using lone.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate: svy: reg cv_loneliness_w11 `o' sex i.agegr3 cv_loneliness_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using lone.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate: svy: reg cv_loneliness_w11 `o' sex i.agegr3 cv_loneliness_w9 cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using lone.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate: svy: reg cv_loneliness_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
cv_loneliness_w9 cov_w11 cvvuln heill_updated
regsave using lone.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

}

use lone.imputed_rev.sens1, replace
export excel using "lone.imputed_rev.sens1.xlsx", firstrow(variables)


*** Financial situation: worried 

clear
save finworried.imputed_rev.sens1, emptyok
use "maindata_s2_imp_rev.dta", replace 

local exp i.covcase1 i.covcase3_rev

foreach o of local exp {

mi estimate, or: svy: logit fins_worried_w10 `o' sex i.agegr3 fins_precov ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using finworried.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit fins_worried_w10 `o' sex i.agegr3 fins_precov ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using finworried.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit fins_worried_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
fins_precov  cvvuln heill_updated
regsave using finworried.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit fins_worried_w11 `o' sex i.agegr3 fins_precov cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using finworried.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit fins_worried_w11 `o' sex i.agegr3 fins_precov cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using finworried.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit fins_worried_w11 `o'##i.sex `o'##i.agegr3  ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
fins_precov cov_w11  cvvuln heill_updated
regsave using finworried.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

}

use finworried.imputed_rev.sens1, replace

gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "finworried.imputed_rev.sens1.xlsx", firstrow(variables)



*** financial situation: worse

clear
save finworse.imputed_rev.sens1, emptyok
use "maindata_s2_imp_rev.dta", replace 


local exp i.covcase1 i.covcase3_rev

foreach o of local exp {

mi estimate, or: svy: logit fins_worse_w10 `o' sex i.agegr3 fins_precov ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using finworse.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit fins_worse_w10 `o' sex i.agegr3 fins_precov ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using finworse.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit fins_worse_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
fins_precov cvvuln heill_updated
regsave using finworse.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit fins_worse_w11 `o' sex i.agegr3 fins_precov cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using finworse.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit fins_worse_w11 `o' sex i.agegr3 fins_precov cov_w11 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using finworse.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit fins_worse_w11 `o'##i.sex `o'##i.agegr3  ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
fins_precov cov_w11 cvvuln heill_updated
regsave using finworse.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

}

use finworse.imputed_rev.sens1, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "finworse.imputed_rev.sens1.xlsx", firstrow(variables)



*** social contact: family 

clear
save scfam.imputed_rev.sens1, emptyok
use "maindata_s2_imp_rev.dta", replace 


local exp i.covcase1 i.covcase3_rev

foreach o of local exp {

mi estimate, or: svy: logit frcont_fam_w10 `o' sex i.agegr3 isolation_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using scfam.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcont_fam_w10 `o' sex i.agegr3 isolation_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using scfam.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcont_fam_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
isolation_binary_w9 cvvuln heill_updated
regsave using scfam.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcont_fam_w11 `o' sex i.agegr3 isolation_binary_w9 ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using scfam.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit frcont_fam_w11 `o' sex i.agegr3 isolation_binary_w9 ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using scfam.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit frcont_fam_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
isolation_binary_w9 cov_w11 cvvuln heill_updated
regsave using scfam.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

}

use scfam.imputed_rev.sens1, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "scfam.imputed_rev.sens1.xlsx", firstrow(variables)


*** social contact: friends 

clear
save scfrie.imputed_rev.sens1, emptyok
use "maindata_s2_imp_rev.dta", replace 


local exp i.covcase1 i.covcase3_rev

foreach o of local exp {

mi estimate, or: svy: logit frcont_fri_w10 `o' sex i.agegr3 isolation_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using scfrie.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcont_fri_w10 `o' sex i.agegr3 isolation_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using scfrie.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcont_fri_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
isolation_binary_w9 cvvuln heill_updated
regsave using scfrie.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate, or: svy: logit frcont_fri_w11 `o' sex i.agegr3 isolation_binary_w9 ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using scfrie.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit frcont_fri_w11 `o' sex i.agegr3 isolation_binary_w9 ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using scfrie.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate, or: svy: logit frcont_fri_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
isolation_binary_w9 cov_w11 cvvuln heill_updated
regsave using scfrie.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

}

use scfrie.imputed_rev.sens1, replace
gen OR_coef= exp(coef)
gen OR_pval = pval
gen OR_ci_lower = exp(ci_lower)
gen OR_ci_upper = exp(ci_upper)

export excel using "scfrie.imputed_rev.sens1.xlsx", firstrow(variables)



*** social contact: all 
clear
save sctot.imputed_rev.sens1, emptyok
use "maindata_s2_imp_rev.dta", replace 


local exp i.covcase1 i.covcase3_rev

foreach o of local exp {

mi estimate: reg frcont_tot_w10 `o' sex i.agegr3 isolation_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using sctot.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate: reg frcont_tot_w10 `o' sex i.agegr3 isolation_binary_w9 ///
i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using sctot.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate: reg frcont_tot_w10 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
isolation_binary_w9 cvvuln heill_updated
regsave using sctot.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w10)

mi estimate: reg frcont_tot_w11 `o' sex i.agegr3 isolation_binary_w9 ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated 
regsave using sctot.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate: reg frcont_tot_w11 `o' sex i.agegr3 isolation_binary_w9 ///
cov_w11 i.cvnump_cat i.work i.wealth_ter cvvuln heill_updated mhc_w9cov1 newcondcov1
regsave using sctot.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

mi estimate: reg frcont_tot_w11 `o'##i.sex `o'##i.agegr3 ///
`o'##i.work `o'##i.wealth_ter `o'##i.cvnump_cat `o'##i.mhc_w9cov1 `o'##newcondcov1 ///
isolation_binary_w9 cov_w11 cvvuln heill_updated
regsave using sctot.imputed_rev.sens1, pval ci append addlabel(outcome, `o' w11)

}

use sctot.imputed_rev.sens1, replace
export excel using "sctot.imputed_rev.sens1.xlsx", firstrow(variables)



