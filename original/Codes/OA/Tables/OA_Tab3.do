set more off

*** Please set path ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/OA/Tables
* cd `folderpath'

* Output
local filename ../../../Output/OA/Tables/OA_Tab3.xls

*** Read in data and drop desk-rejected papers as well as those with only 1 referee assigned
use "../../../Data/Main/Pooled_cleaned.dta", clear
keep if notdeskrej
drop if noref==1

*** Store as local macros sets of covariates for the various regressions
local Reports frReject-frAccept
local FixedEffects i.journal_factor i.year_factor i.journal_factor#i.year_factor
local AuthorVars pub1 pub2 pub3 pub45 pub6 auth2 auth3 auth4
* local Fields fr_internat fr_lab fr_labor fr_healthurblaw fr_dev fr_hist fr_pub fr_io fr_fin fr_macro missingfield fr_micro fr_other fr_metrics
local Fields fr_internat fr_lab fr_labor fr_healthurblaw fr_dev fr_hist fr_pub fr_io fr_fin fr_macro fr_theory fr_micro fr_other fr_metrics


* Define generalized residual
probit rr `Reports' `AuthorVars' `Fields' `FixedEffects' neditor_rr_loo2, vce(cluster neditorid2)
predict p1
gen rho1 =  -(1-rr) * (normalden(invnormal(p1))/(1-p1)  )  +  rr * (normalden(invnormal(p1))/p1)

*** Asinh(Citations) OLS Regressions
quietly regress asinh_cites `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Fields') nocons ///
replace ctitle(Asinh(Citations),Pooled) label noaster excel ///
addtext(Control Function and R&R Indicator, No, Editor's leave-one-out R&R rate, -, Controls for Referee Reports, No, Controls for Authos Pubs, No, Indicators for Journal-Year, No) ///
title("Correlation between Field and Citations/R&R (standard errors clustered by editor)")

quietly regress asinh_cites rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Fields') nocons ///
addtext(Control Function and R&R Indicator, Yes, Editor's leave-one-out R&R rate, -, Controls for Referee Reports, Yes, Controls for Authos Pubs, Yes, Indicators for Journal-Year, Yes) ///
append ctitle(Asinh(Citations),Pooled) label noaster excel

*** R&R Probit Regressions
quietly probit rr `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(`Fields') nocons ///
addtext(Control Function and R&R Indicator, -, Editor's leave-one-out R&R rate, No, Controls for Referee Reports, No, Controls for Authos Pubs, No, Indicators for Journal-Year, No) ///
append ctitle(R&R,Pooled) label noaster excel addstat(Pseudo-R^2, e(r2_p))

quietly probit rr neditor_rr_loo2 `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(`Fields') nocons  ///
append ctitle(R&R,Pooled) label noaster excel addstat(Pseudo-R^2, e(r2_p)) ///
addtext(Control Function and R&R Indicator, -, Editor's leave-one-out R&R rate, Yes, Controls for Referee Reports, Yes, Controls for Authos Pubs, Yes, Indicators for Journal-Year, Yes) ///
sortvar( `Fields' `Reports' `AuthorVars' `FixedEffects' rho1 rr neditor_rr_loo2)
