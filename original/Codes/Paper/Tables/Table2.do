* Set the working directory where the cleaned input data is located, and also the output filename.
set more off

*** Please set path ***
*local folderpath /Users/username/Desktop/ReplicationFiles/Codes/Paper/Tables
*cd `folderpath'

* Output
local filename ../../../Output/Paper/Tables/Table2.xls

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
local Model frReject-frAccept pub1 pub2 pub3 pub45 pub6 auth2 auth3 auth4

* Define generalized residual
probit rr `Reports' `AuthorVars' `Fields' `FixedEffects' neditor_rr_loo2, vce(cluster neditorid2)
predict p1
gen rho1 =  -(1-rr) * (normalden(invnormal(p1))/(1-p1)  )  +  rr * (normalden(invnormal(p1))/p1) 

* Define auxiliary variables
* Create indicator for top 2% of all submissions 
gen top2 = GScitePct>=98
label var top2 "Dummy equal to 1 when the paper is in the top-2 percentile of most-cited papers, 0 otherwise"
* For the cnreg command, I first have to create a <markervar> that tells whether an observation is uncensored (<markvar>==0),
* and, if censored, whether it is left- (-1) or right- (1) censored 
* Recall: restud is topcoded when GScites>200, the other three journals whenever GScites>500

generate 	topcoded=0	if qje==1 		& GScites<500
replace 	topcoded=0 	if restat==1 	& GScites<500
replace 	topcoded=0	if jeea==1		& GScites<500
replace 	topcoded=0 	if restud==1	& GScites<200

replace 	topcoded=1	if qje==1		& GScites>=500
replace 	topcoded=1 	if restat==1 	& GScites>=500
replace 	topcoded=1	if jeea==1		& GScites>=500
replace 	topcoded=1 	if restud==1	& GScites>=200 	

label var topcoded "Dummy equal to 1 when the paper citations are top-coded, 0 otherwise"

* Note cnreg does not support interactions, so I have to create year*journal fixed effects manually
tabulate 	year_factor, gen(year_dummy)
tabulate 	journal_factor, gen(journal_dummy)

foreach v of numlist 1/4 	{
foreach z of numlist 1/11 	{
g yearjournal_`v'_`z'=journal_dummy`v'*year_dummy`z'
	}
	}

***************************************
*** OLS regressions [cols. (1)-(5)] ***
***************************************

*** Asinh(Citations) OLS Regressions

quietly regress asinh_cites `Reports' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model') nocons ///
replace ctitle(Asinh(Citations),Pooled) noaster label excel ///
addtext(Controls for Field of Paper, No, Indicator for Journal Year, Yes) ///
title("R&R Stage Regressions (standard errors clustered by editor)")
*sortvar(`Reports' `AuthorVars' `Fields'  rr rho1 neditor_rr_loo2 )

quietly regress asinh_cites `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model') nocons ///
append ctitle(Asinh(Citations),Pooled) noaster label excel ///
addtext(Controls for Field of Paper, Yes, Indicator for Journal Year, Yes)

quietly regress asinh_cites `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2)  keep(`Model') nocons ///
append ctitle(Asinh(Citations),Pooled) noaster label excel ///
addtext(Controls for Field of Paper, Yes, Indicator for Journal Year, Yes)

quietly regress asinh_cites rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2)  keep(`Model' rr rho1) nocons ///
append ctitle(Asinh(Citations),Pooled) noaster label excel ///
addtext(Controls for Field of Paper, Yes, Indicator for Journal Year, Yes)

quietly regress asinh_cites rr `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model' rr rho1) nocons ///
append ctitle(Asinh(Citations),Pooled) noaster label excel ///
addtext(Controls for Field of Paper, Yes, Indicator for Journal Year, Yes)


******************************************
*** Tobit with censonred obs., col.(6) ***
******************************************

* Censored and uncensored observations (Tobit using cnreg)
* Note: I am using the command cnreg, although from Stata 15 onward there is a new command called intreg

quietly xi: cnreg asinh_cites rho1 rr `Reports' `AuthorVars' `Fields' i.year_factor i.journal_factor yearjournal_2_1-yearjournal_4_11, censored(topcoded) vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model' rr rho1) nocons ///
append ctitle(Tobit Asinh(Citations),Pooled) label  excel noaster addstat(Pseudo-R^2, e(r2_p)) ///
addtext(Controls for Field of Paper, Yes, Indicator for Journal Year, Yes)

*********************************************
*** Probit of being top 2% cites, col.(7) ***
*********************************************

quietly probit top2 rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(`Model' rr rho1) nocons ///
append ctitle(Probit Top 2%, Pooled) label  excel noaster addstat(Pseudo-R^2, e(r2_p)) ///
addtext(Controls for Field of Paper, Yes, Indicator for Journal Year, Yes)


******************************************
*** Probit regressions, cols. (8)-(10) ***
******************************************

*** R&R Probit Regressions

quietly probit rr `Reports' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(`Model') nocons ///
append ctitle(R&R,Pooled) label noaster excel addstat(Pseudo-R^2, e(r2_p)) ///
addtext(Controls for Field of Paper, No, Indicator for Journal Year, Yes)

quietly probit rr `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(`Model') nocons ///
append ctitle(R&R,Pooled) label noaster excel addstat(Pseudo-R^2, e(r2_p)) ///
addtext(Controls for Field of Paper, Yes, Indicator for Journal Year, Yes)

quietly probit rr neditor_rr_loo2 `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) adec(2) keep(`Model' neditor_rr_loo2) nocons ///
append ctitle(R&R,Pooled) label noaster excel addstat(Pseudo-R^2, e(r2_p)) ///
addtext(Controls for Field of Paper, Yes, Indicator for Journal Year, Yes) ///
sortvar(`Model' rr rho1 neditor_rr_loo2)
