set more off

*** Please set path ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/OA/Tables
* cd `folderpath'

* Output
local filename1 ../../../Output/OA/Tables/OA_Tab13_PanelA.xls
local filename2 ../../../Output/OA/Tables/OA_Tab13_PanelB.xls

*** Read in data of the survey results
use "../../../Data/Raw/survey_results.dta", clear

* Weight observations such that each paper received equal weight.
gen wt = 1/responses

label var log_ratio "Log of Actual Citation Ratio"

*** Panel A ***
quietly regress est_log_ratio log_ratio, cluster(title_id)
outreg2 using `filename1', dec(2) ///
replace ctitle(All Responses,OLS) label noaster excel ///
title("Survey Regressions: Panel A")

quietly regress est_log_ratio log_ratio [aweight = wt], cluster(title_id)
outreg2 using `filename1', dec(2) ///
append ctitle(All Responses,WLS) label noaster excel

quietly regress est_log_ratio log_ratio if abs(log_ratio)<=0.5, cluster(title_id)
outreg2 using `filename1', dec(2) ///
append ctitle(Paper Pairs with Balanced Citations,OLS) label noaster excel

quietly regress est_log_ratio log_ratio if subject_pub<=1, cluster(title_id)
outreg2 using `filename1', dec(2) ///
append ctitle(PhD Students & Non-Prolific Faculty,OLS) label noaster excel

quietly regress est_log_ratio log_ratio if subject_pub>=4, cluster(title_id)
outreg2 using `filename1', dec(2) ///
append ctitle(Prolific Faculty,OLS) label noaster excel


*** Panel B ***
quietly regress Expo log_ratio, cluster(title_id)
outreg2 using `filename2', dec(2) ///
replace ctitle(Exposition,OLS) label noaster excel ///
title("Survey Regressions: Panel B")

quietly regress Imp log_ratio, cluster(title_id)
outreg2 using `filename2', dec(2) ///
append ctitle(Importance,OLS) label noaster excel

quietly regress Rig log_ratio, cluster(title_id)
outreg2 using `filename2', dec(2) ///
append ctitle(Rigor,OLS) label noaster excel

quietly regress Nov log_ratio, cluster(title_id)
outreg2 using `filename2', dec(2) ///
append ctitle(Novelty,OLS) label noaster excel
