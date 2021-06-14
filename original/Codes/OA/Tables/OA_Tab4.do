set more off

*** Please set path ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/OA/Tables
* cd `folderpath'

* Output
local filename ../../../Output/OA/Tables/OA_Tab4.xls

*** Read in data and drop desk-rejected papers as well as those with only 1 referee assigned
use "../../../Data/Main/Pooled_cleaned.dta", clear
keep if notdeskrej
drop if noref==1

*** Keep variables
local Model asinh_cites rr rr_early rr_qje rr_restat rr_restud rho1 rho1_early rho1_qje rho1_restat rho1_restud frRR pub6

*** Store as local macros sets of covariates for the various regressions
local Reports frReject-frAccept
local FixedEffects i.journal_factor i.year_factor i.journal_factor#i.year_factor
local AuthorVars pub1 pub2 pub3 pub45 pub6 auth2 auth3 auth4
local Fields fr_lab fr_labor fr_healthurblaw fr_dev fr_hist fr_pub fr_io fr_fin fr_macro missingfield fr_micro fr_other fr_theory fr_metrics

* Define generalized residual
probit rr `Reports' `AuthorVars' `Fields' `FixedEffects' neditor_rr_loo2, vce(cluster neditorid2)
predict p1
gen rho1 =  -(1-rr) * (normalden(invnormal(p1))/(1-p1)  )  +  rr * (normalden(invnormal(p1))/p1)
gen early = yearsubmit<=2010

gen rr_early = rr*early
gen rr_linear = rr*(2015-yearsubmit)

gen rr_qje = rr*qje
gen rr_restud = rr*restud
gen rr_restat = rr*restat

gen rr_linear_qje = rr_linear*qje
gen rr_linear_restud = rr_linear*restud
gen rr_linear_restat = rr_linear*restat

gen rho1_early = rho1*early
gen rho1_qje = rho1*qje
gen rho1_restud = rho1*restud
gen rho1_restat = rho1*restat

gen rho1_linear = rho1*(2015-yearsubmit)


*** Regressions using various measures of citations as the dependent variable (using papers submitted in all years)
quietly regress asinh_cites rr rho1 `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model') nocons ///
replace ctitle(Asinh(Citations)) label noaster excel ///
addtext(Author Prominence and No. of, Yes, Controls for Field of Paper, Yes, Indicators for Journal-Year, Yes) ///
title("Citation Models: Publication Bias vs. Editor Signal (standard errors clustered by editor)")

quietly regress asinh_cites rr rr_early rr_qje rr_restat rr_restud rho1 `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model') nocons ///
addtext(Author Prominence and No. of, Yes, Controls for Field of Paper, Yes, Indicators for Journal-Year, Yes) ///
append ctitle(Asinh(Citations)) label noaster excel

quietly regress asinh_cites rr rr_early rr_qje rr_restat rr_restud rho1 rho1_early rho1_qje rho1_restat rho1_restud `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model') nocons ///
addtext(Author Prominence and No. of, Yes, Controls for Field of Paper, Yes, Indicators for Journal-Year, Yes) ///
append ctitle(Asinh(Citations)) label noaster excel ///
sortvar(`Model' frRR pub6)
