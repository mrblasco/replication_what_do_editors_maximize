set more off

*** Please set path ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/OA/Tables
* cd `folderpath'

* Output
local filename ../../../Data/Secondary/OA_Tab5_fig.xls


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
* Define generalized residual
probit rr `Reports' `AuthorVars' `Fields' `FixedEffects' neditor_rr_loo2 if journal=="QJE", vce(cluster neditorid2)
predict p1 if journal=="QJE"
gen rho1 =  -(1-rr) * (normalden(invnormal(p1))/(1-p1)  )  +  rr * (normalden(invnormal(p1))/p1) 
quietly regress asinh_cites rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects' if journal=="QJE", vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model' `Fields' rr rho1) nocons ///
replace ctitle(Asinh(Citations),QJE) label noaster excel ///
addtext(Authors, Yes, Controls for Field of Paper, Yes, Indicators fors Year, Yes) ///
title("R&R Stage Regressions by Journal (standard errors clustered by editor)")
drop p1 rho1

probit rr `Reports' `AuthorVars' `Fields' `FixedEffects' neditor_rr_loo2 if journal=="REStud", vce(cluster neditorid2)
predict p1 if journal=="REStud"
gen rho1 =  -(1-rr) * (normalden(invnormal(p1))/(1-p1)  )  +  rr * (normalden(invnormal(p1))/p1) 
quietly regress asinh_cites rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects' if journal=="REStud", vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model' `Fields' rr rho1) nocons ///
addtext(Authors, Yes, Controls for Field of Paper, Yes, Indicators fors Year, Yes) ///
append ctitle(Asinh(Citations),REStud) label noaster excel
drop p1 rho1

probit rr `Reports' `AuthorVars' `Fields' `FixedEffects' neditor_rr_loo2 if journal=="REStat", vce(cluster neditorid2)
predict p1 if journal=="REStat"
gen rho1 =  -(1-rr) * (normalden(invnormal(p1))/(1-p1)  )  +  rr * (normalden(invnormal(p1))/p1) 
quietly regress asinh_cites rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects' if journal=="REStat", vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model' `Fields' rr rho1) nocons ///
addtext(Authors, Yes, Controls for Field of Paper, Yes, Indicators fors Year, Yes) ///
append ctitle(Asinh(Citations),REStat) label noaster excel
drop p1 rho1

probit rr `Reports' `AuthorVars' `Fields' `FixedEffects' neditor_rr_loo2 if journal=="JEEA", vce(cluster neditorid2)
predict p1 if journal=="JEEA"
gen rho1 =  -(1-rr) * (normalden(invnormal(p1))/(1-p1)  )  +  rr * (normalden(invnormal(p1))/p1) 
quietly regress asinh_cites rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects' if journal=="JEEA", vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model' `Fields' rr rho1) nocons ///
addtext(Authors, Yes, Controls for Field of Paper, Yes, Indicators fors Year, Yes) ///
append ctitle(Asinh(Citations),JEEA) label noaster excel
drop p1 rho1

*** R&R Probit Regressions
quietly probit rr neditor_rr_loo2 `Reports' `AuthorVars' `Fields' `FixedEffects' if journal=="QJE", vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(`Model' `Fields' neditor_rr_loo2) nocons ///
addtext(Authors, Yes, Controls for Field of Paper, Yes, Indicators fors Year, Yes) ///
append ctitle(R&R,QJE) label noaster excel addstat(Pseudo-R^2, e(r2_p))

quietly probit rr neditor_rr_loo2 `Reports' `AuthorVars' `Fields' `FixedEffects' if journal=="REStud", vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(`Model' `Fields' neditor_rr_loo2) nocons ///
addtext(Authors, Yes, Controls for Field of Paper, Yes, Indicators fors Year, Yes) ///
append ctitle(R&R,REStud) label noaster excel addstat(Pseudo-R^2, e(r2_p))

quietly probit rr neditor_rr_loo2 `Reports' `AuthorVars' `Fields' `FixedEffects' if journal=="REStat", vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(`Model' `Fields' neditor_rr_loo2) nocons ///
addtext(Authors, Yes, Controls for Field of Paper, Yes, Indicators fors Year, Yes) ///
append ctitle(R&R,REStat) label noaster excel addstat(Pseudo-R^2, e(r2_p))

quietly probit rr neditor_rr_loo2 `Reports' `AuthorVars' `Fields' `FixedEffects' if journal=="JEEA", vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(neditor_rr_loo2 `Model' `Fields') nocons ///
addtext(Authors, Yes, Controls for Field of Paper, Yes, Indicators fors Year, Yes) ///
append ctitle(R&R,JEEA) label noaster excel addstat(Pseudo-R^2, e(r2_p)) ///
sortvar(rr rho1 neditor_rr_loo2 `Model' `Fields')
