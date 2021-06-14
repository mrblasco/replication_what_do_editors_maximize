set more off

*** Please set path ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/OA/Tables
* cd `folderpath'

* Output
local filename ../../../Output/OA/Tables/OA_Tab9.xls

*** Read in data and drop desk-rejected papers as well as those with only 1 referee assigned
use "../../../Data/Main/Pooled_cleaned.dta", clear
keep if notdeskrej
drop if noref==1

*** Store as local macros sets of covariates for the various regressions
local Reports frReject-frAccept
local FixedEffects i.journal_factor i.year_factor i.journal_factor#i.year_factor
local AuthorVars pub1 pub2 pub3 pub45 pub6 auth2 auth3 auth4
local Fields fr_lab fr_labor fr_healthurblaw fr_dev fr_hist fr_pub fr_io fr_fin fr_macro missingfield fr_micro fr_other fr_theory fr_metrics

local Top5Vars top5_1 top5_2 top5_3
local OldPubs pub610_13 pub610_4
local InstRanking ellison110 ellison1120 qs14europe110 qs14row15

*** Output variables
local ModelOne frRR pub1 pub2 pub3 pub45 pub6
local ModelTwo frRR pub1 pub2 pub3 pub45 pub6 top5_1 top5_2 top5_3 pub610_13 pub610_4 
local ModelThree frRR pub1 pub2 pub3 pub45 pub6 top5_1 top5_2 top5_3 pub610_13 pub610_4 ellison110 ellison1120 qs14europe110 qs14row15

*** Asinh(Citations) OLS Regressions
probit rr `Reports' `AuthorVars' `Fields' `FixedEffects' neditor_rr_loo2, vce(cluster neditorid2)
predict p1
gen rho1 =  -(1-rr) * (normalden(invnormal(p1))/(1-p1)  )  +  rr * (normalden(invnormal(p1))/p1) 
quietly regress asinh_cites rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`ModelOne' rr rho1) nocons ///
replace ctitle(Asinh(Citations), Full Sample) label noaster excel ///
title("Different Measures of Author Prominence (standard errors clustered by editor)")
drop p1 rho1

probit rr `Reports' `AuthorVars' `Fields' `FixedEffects' `Top5Vars' `OldPubs' neditor_rr_loo2, vce(cluster neditorid2)
predict p1
gen rho1 =  -(1-rr) * (normalden(invnormal(p1))/(1-p1)  )  +  rr * (normalden(invnormal(p1))/p1) 
quietly regress asinh_cites rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects' `Top5Vars' `OldPubs', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`ModelTwo' rr rho1) nocons ///
append ctitle(Asinh(Citations),Full Sample) label noaster excel
drop p1 rho1

probit rr `Reports' `AuthorVars' `Fields' `FixedEffects' `Top5Vars' `OldPubs' `InstRanking' neditor_rr_loo2 if journal=="REStud" | journal=="JEEA", vce(cluster neditorid2)
predict p1 if journal=="REStud" | journal=="JEEA"
gen rho1 =  -(1-rr) * (normalden(invnormal(p1))/(1-p1)  )  +  rr * (normalden(invnormal(p1))/p1) 
quietly regress asinh_cites rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects' `Top5Vars' `OldPubs' `InstRanking' if journal=="REStud" | journal=="JEEA", vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`ModelThree' rr rho1) nocons ///
append ctitle(Asinh(Citations),JEEA and REStud) label noaster excel
drop p1 rho1

*** R&R Probit Regressions
quietly probit rr neditor_rr_loo2 `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(`ModelOne' neditor_rr_loo2) nocons ///
append ctitle(R&R, Full Sample) label noaster excel addstat(Pseudo-R^2, e(r2_p))

quietly probit rr neditor_rr_loo2 `Reports' `AuthorVars' `Fields' `FixedEffects' `Top5Vars' `OldPubs', vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(`ModelTwo' neditor_rr_loo2) nocons ///
append ctitle(R&R, Full Sample) label noaster excel addstat(Pseudo-R^2, e(r2_p))

quietly probit rr neditor_rr_loo2 `Reports' `AuthorVars' `Fields' `FixedEffects' `Top5Vars' `OldPubs' `InstRanking' if journal=="REStud" | journal=="JEEA", vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(`ModelThree' neditor_rr_loo2) nocons ///
append ctitle(R&R,JEEA and REStud) label noaster excel addstat(Pseudo-R^2, e(r2_p)) ///
sortvar(frRR pub1 pub2 pub3 pub45 pub6 top5_1 top5_2 top5_3 `OldPubs' ellison110 ellison1120 qs14europe110 qs14row15 rr rho1 neditor_rr_loo2)
