set more off

*** Please set path ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/OA/Tables
* cd `folderpath'

* Output
local filename ../../../Output/OA/Tables/OA_Tab12.xls

* Read in data of all papers
use "../../../Data/Main/Pooled_cleaned.dta", clear


*** Store as local macros sets of covariates for the various regressions
local FixedEffects i.journal_factor i.year_factor i.journal_factor#i.year_factor
local AuthorVars pub1 pub2 pub3 pub45 pub6 auth2 auth3 auth4
local Fields fr_lab fr_labor fr_healthurblaw fr_dev fr_hist fr_pub fr_io fr_fin fr_macro missingfield fr_micro fr_other fr_theory fr_metrics

*** Table output for publication
local Model pub1 pub2 pub3 pub45 pub6 auth2 auth3 auth4


* Define generalized residual
probit notdeskrej `AuthorVars' `Fields' `FixedEffects' neditor_ndr_loo2, vce(cluster neditorid2)
predict p1
gen rho1 =  -(1-notdeskrej) * (normalden(invnormal(p1))/(1-p1)  )  +  notdeskrej * (normalden(invnormal(p1))/p1) 

*** Asinh(Citations) OLS Regressions
quietly regress asinh_cites rho1 notdeskrej `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model' notdeskrej neditor_ndr_loo2 rho1) nocons ///
replace ctitle(Asinh(Citations),Pooled) label noaster excel ///
addtext(Controls for Field of Paper, Yes, Indicators for Journal-Year Cohort, Yes) ///
title("NDR Stage Regressions (standard errors clustered by editor)")

quietly regress asinh_cites rho1 `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model' rho1) nocons ///
append ctitle(Asinh(Citations),Pooled) label noaster excel ///
addtext(Controls for Field of Paper, Yes, Indicators for Journal-Year Cohort, Yes)

quietly regress asinh_cites notdeskrej `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model' notdeskrej) nocons ///
append ctitle(Asinh(Citations),Pooled) label noaster excel ///
addtext(Controls for Field of Paper, Yes, Indicators for Journal-Year Cohort, Yes)

*** NDR Probit Regressions
quietly probit notdeskrej neditor_ndr_loo2 `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(`Model' neditor_ndr_loo2) nocons ///
append ctitle(NDR,Pooled) label noaster excel addstat(Pseudo-R^2, e(r2_p)) ///
addtext(Controls for Field of Paper, Yes, Indicators for Journal-Year Cohort, Yes) ///
sortvar(`AuthorVars' `Fields' `FixedEffects' notdeskrej rho1 neditor_ndr_loo2)
