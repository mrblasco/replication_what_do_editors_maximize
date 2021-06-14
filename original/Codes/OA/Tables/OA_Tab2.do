set more off

*** Please set path ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/OA/Tables
* cd `folderpath'

* Output
local filename ../../../Output/OA/Tables/OA_Tab2.xls

*** Read in data and drop desk-rejected papers as well as those with only 1 referee assigned
use "../../../Data/Main/Pooled_cleaned.dta", clear

keep if notdeskrej
drop if noref==1

*** Store as local macros sets of covariates for the various regressions
local Reports frReject-frAccept
local FixedEffects i.journal_factor i.year_factor i.journal_factor#i.year_factor
local AuthorVars pub1 pub2 pub3 pub45 pub6 auth2 auth3 auth4
local Fields fr_lab fr_labor fr_healthurblaw fr_dev fr_hist fr_pub fr_io fr_fin fr_macro missingfield fr_micro fr_other fr_theory fr_metrics

*** Table output for publication
local Model frReject-frAccept pub1 pub2 pub3 pub45 pub6

*** Asinh(Citations) OLS Regressions
probit rr `Reports' `AuthorVars' `Fields' `FixedEffects' neditor_rr_loo2 if norefresp==2, vce(cluster neditorid2)
predict p1 if norefresp==2
gen rho1 =  -(1-rr) * (normalden(invnormal(p1))/(1-p1)  )  +  rr * (normalden(invnormal(p1))/p1) 
quietly regress asinh_cites rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects' if norefresp==2, vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model' rho1 rr) nocons ///
replace ctitle(Asinh(Citations),2 Reports) label noaster excel ///
addtext(Indicators for Number of Authors, Yes, Controls for Field of Paper, Yes, Indicators for Year, Yes) ///
title("R&R Stage Regressions for Papers with Different Numbers of Reports (standard errors clustered by editor)")

drop p1 rho1

probit rr `Reports' `AuthorVars' `Fields' `FixedEffects' neditor_rr_loo2 if norefresp==3, vce(cluster neditorid2)
predict p1 if norefresp==3
gen rho1 =  -(1-rr) * (normalden(invnormal(p1))/(1-p1)  )  +  rr * (normalden(invnormal(p1))/p1) 
quietly regress asinh_cites rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects' if norefresp==3, vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model' rho1 rr) nocons ///
append ctitle(Asinh(Citations),3 Reports) label noaster excel ///
addtext(Indicators for Number of Authors, Yes, Controls for Field of Paper, Yes, Indicators for Year, Yes)

drop p1 rho1

probit rr `Reports' `AuthorVars' `Fields' `FixedEffects' neditor_rr_loo2 if norefresp>=4, vce(cluster neditorid2)
predict p1 if norefresp>=4
gen rho1 =  -(1-rr) * (normalden(invnormal(p1))/(1-p1)  )  +  rr * (normalden(invnormal(p1))/p1) 
quietly regress asinh_cites rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects' if norefresp>=4, vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model' rho1 rr) nocons ///
append ctitle(Asinh(Citations),4+ Reports) label noaster excel ///
addtext(Indicators for Number of Authors, Yes, Controls for Field of Paper, Yes, Indicators for Year, Yes) ///

*** R&R Probit Regressions
quietly probit rr neditor_rr_loo2 `Reports' `AuthorVars' `Fields' `FixedEffects' if norefresp==2, vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(`Model' neditor_rr_loo2) nocons ///
append ctitle(R&R,2 Reports) label noaster excel addstat(Pseudo-R^2, e(r2_p)) ///
addtext(Indicators for Number of Authors, Yes, Controls for Field of Paper, Yes, Indicators for Year, Yes) ///

quietly probit rr neditor_rr_loo2 `Reports' `AuthorVars' `Fields' `FixedEffects' if norefresp==3, vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(`Model' neditor_rr_loo2) nocons ///
append ctitle(R&R,3 Reports) label noaster excel addstat(Pseudo-R^2, e(r2_p)) ///
addtext(Indicators for Number of Authors, Yes, Controls for Field of Paper, Yes, Indicators for Year, Yes) ///

quietly probit rr neditor_rr_loo2 `Reports' `AuthorVars' `Fields' `FixedEffects' if norefresp>=4, vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(`Model' neditor_rr_loo2) nocons ///
append ctitle(R&R,4+ Reports) label noaster excel addstat(Pseudo-R^2, e(r2_p)) ///
addtext(Indicators for Number of Authors, Yes, Controls for Field of Paper, Yes, Indicators for Year, Yes) ///
sortvar(`Reports' `AuthorVars' `Fields' `FixedEffects' rr rho1 neditor_rr_loo2)
