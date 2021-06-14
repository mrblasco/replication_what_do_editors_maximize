set more off

*** Please set path ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/OA/Tables
* cd `folderpath'

* Output
local filename ../../../Output/OA/Tables/OA_Tab8.xls

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
local Model frReject-frAccept pub1 pub2 pub3 pub45 pub6 rr rho1


* Define generalized residual
probit rr `Reports' `AuthorVars' `Fields' `FixedEffects' neditor_rr_loo2, vce(cluster neditorid2)
predict p1
gen rho1 =  -(1-rr) * (normalden(invnormal(p1))/(1-p1)  )  +  rr * (normalden(invnormal(p1))/p1) 

* Define auxiliary variables
* Create indicator for top 2% of all submissions 
gen top5 = GScitePct>=95
label var top5 "Dummy equal to 1 when the paper is in the top-5 percentile of most-cited papers, 0 otherwise"

* Preparation for censoring
*** Note: I am using the command cnreg, although from Stata 15 onward there is a new command called intreg
*For the cnreg command, I first have to create a <markervar> that tells whether an observation is uncensored (<markvar>==0),
* and, if censored, whether it is left- (-1) or right- (1) censored
generate 	leftcensored=-1 if WOScites==0
replace 	leftcensored=0 	if leftcensored==.
label var 	leftcensored "Dummy equal to 1 when the paper has 0 cites on WoS, 0 otherwise"

* Note cnreg does not support interactions, so I have to create year*journal fixed effects manually
tabulate 	year_factor, gen(year_dummy)
tabulate 	journal_factor, gen(journal_dummy)

foreach v of numlist 1/4 	{
foreach z of numlist 1/5 	{
g yearjournal_`v'_`z'=journal_dummy`v'*year_dummy`z'
	}
}

*** Regressions using various measures of citations as the dependent variable (using papers submitted in all years)
quietly regress asinh_cites rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model') nocons ///
replace ctitle(Asinh(Citations),All Years) label noaster excel  ///
title("Alternative Dependent Variables (standard errors clustered by editor)")

quietly regress GScitePct rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model') nocons ///
append ctitle(GScitePct,All Years) label noaster excel

quietly probit topcited rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model') adec(2) nocons ///
append ctitle(Top-Cited,All Years) label noaster excel addstat(Pseudo-R^2, e(r2_p))

quietly probit top5 rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model') adec(2) nocons ///
append ctitle(OLS Top 5% asinh,Pooled) label noaster excel addstat(Pseudo-R^2, e(r2_p))

quietly regress logplusone_cites rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model') nocons ///
append ctitle(Ln(Citations+1),All Years) label noaster excel

quietly xi: cnreg  asinh_wos rho1 rr `Reports' `AuthorVars' `Fields' yearjournal_2_1-yearjournal_4_5	if yearsubmit>=2006 & yearsubmit<=2010, censored(leftcensored) vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model') nocons ///
append ctitle(Tobit Asinh(WOScites),2006-2010) label  excel noaster addstat(Pseudo-R^2, e(r2_p))

quietly regress asinh_cites rho1 rr `Reports' `AuthorVars' `Fields' `FixedEffects' if scraper_missed!=1, vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model') nocons ///
append ctitle(Asinh(Citations),All Years) label noaster excel ///
title("Alternative Dependent Variables (standard errors clustered by editor)") ///
sortvar(`Model')

