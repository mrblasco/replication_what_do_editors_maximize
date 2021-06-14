set more off

*** Please set path ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/OA/Tables
* cd `folderpath'

* Output
local filename ../../../Output/OA/Tables/OA_Tab10.xls

*** Store as local macros sets of covariates for the various regressions
local Reports frReject-frAccept
local FixedEffects i.journal_factor i.year_factor i.journal_factor#i.year_factor
local Pubs pub1 pub2 pub3 pub45 pub6
local NumAuthors auth2 auth3 auth4
local Fields fr_lab fr_labor fr_healthurblaw fr_dev fr_hist fr_pub fr_io fr_fin fr_macro missingfield fr_micro fr_other fr_theory fr_metrics

*** Table output for publication
local Model pub1 pub2 pub3 pub45 pub6 

******************* R&R stage papers ******************
*** Read in data and drop desk-rejected papers as well as those with only 1 referee assigned
use "../../../Data/Main/Pooled_cleaned.dta", clear
keep if notdeskrej
drop if noref==1

*** Asinh(Citations) OLS Regressions
quietly regress asinh_cites `Pubs' `NumAuthors' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model') nocons ///
replace ctitle(Asinh(Citations),EE,NDR=1) label noaster excel ///
addtext(Fractions of Referee Recs., No, Indicators for Number of Authors, Yes, Controls for Field of Paper, Yes, Indicators for Journal-Year, Yes) ///
title("Discount")

quietly regress asinh_cites `Reports' `Pubs' `NumAuthors' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model') nocons ///
append ctitle(Asinh(Citations),EE,NDR=1) label noaster excel ///
addtext(Fractions of Referee Recs., Yes, Indicators for Number of Authors, Yes, Controls for Field of Paper, Yes, Indicators for Journal-Year, Yes) ///

******************* Papers receiving R&R ******************
keep if rr==1

quietly regress asinh_cites `Pubs' `NumAuthors' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model') nocons ///
append ctitle(Asinh(Citations),EE,RR=1) label noaster excel ///
addtext(Fractions of Referee Recs., No, Indicators for Number of Authors, Yes, Controls for Field of Paper, Yes, Indicators for Journal-Year, Yes) ///

quietly regress asinh_cites `Reports' `Pubs' `NumAuthors' `Fields' `FixedEffects', vce(cluster neditorid2)
outreg2 using `filename', dec(2) keep(`Model') nocons ///
append ctitle(Asinh(Citations),EE,RR=1) label noaster excel ///
addtext(Fractions of Referee Recs., Yes, Indicators for Number of Authors, Yes, Controls for Field of Paper, Yes, Indicators for Journal-Year, Yes) ///

******************* Published Papers ******************

*** Read in data on published papers for our 4 journals
use "../../../Data/Secondary/Published_EE_Data5.dta", clear
gen pub6=pub67+pub8
label var pub6 "Publications: 6+"

quietly regress asinh_cites `Pubs' `NumAuthors' `Fields' `FixedEffects', vce(robust)
outreg2 using `filename', dec(2) keep(`Model') nocons ///
append ctitle(Asinh(Citations),EE (2008-2015),Published) label noaster excel ///
addtext(Fractions of Referee Recs., No, Indicators for Number of Authors, Yes, Controls for Field of Paper, Yes, Indicators for Journal-Year, Yes) ///

*** Read in data on top 5 journals (1991-2010)
use "../../../Data/Secondary/Top5Papers.dta", clear
gen pub6=pub67+pub8
label var pub6 "Publications: 6+"

quietly regress asinh_cites `Pubs' `NumAuthors' `Fields' `FixedEffects' if yearsubmit>=1995 & yearsubmit<=2010, vce(robust)
outreg2 using `filename', dec(2) keep(`Model') nocons ///
append ctitle(Asinh(Citations),Top 5 (1997-2012),Published) label noaster excel ///
addtext(Fractions of Referee Recs., No, Indicators for Number of Authors, Yes, Controls for Field of Paper, Yes, Indicators for Journal-Year, Yes) ///
sortvar(`Pubs' `Reports' `NumAuthors' `Fields' `FixedEffects')
