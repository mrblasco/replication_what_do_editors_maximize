clear
set more off

*** Please set path ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/OA/Tables
* cd `folderpath'

*** Read in data and drop desk-rejected papers as well as those with only 1 referee assigned
use "../../../Data/Main/Pooled_cleaned.dta", clear

keep if notdeskrej
drop if noref==1

*** Store as local macros sets of covariates for the various regressions
local Reports frReject-frAccept
local FixedEffects i.journal_factor i.year_factor i.journal_factor#i.year_factor
local AuthorVars pub1 pub2 pub3 pub45 pub6 auth2 auth3 auth4
local Fields fr_labor fr_healthurblaw fr_dev fr_hist fr_pub fr_io fr_fin fr_macro missingfield fr_micro fr_other fr_theory fr_metrics fr_lab 

* Define generalized residual
qui probit rr `Reports' `AuthorVars' `Fields' `FixedEffects' neditor_rr_loo2, vce(cluster neditorid2)
matrix b0_rr=e(b)


* Create a program that computes the estimates of interests, with across-regression non-linear restrictions on the coefficient
capture program drop myML_nlc
program myML_nlc , eclass
version 13
args todo b lnfj 

tempvar  rr_coeff mu_cite mu_rr mu_rr1 lnsigma_phi sigma_phi g_resid extrac_1 extrac_2 extrac_3 extrac_45 extrac_6 extrac_auth2 extrac_auth3 extrac_auth4 constr_auth_coeff constr_rep_coeff lj pr_rr  ctrl_fct

tempname pi_rep1 pi_rep2 pi_rep3 pi_rep4 pi_rep5 pi_rep6 pi_pub1 pi_pub2 pi_pub3 pi_pub45 pi_pub6 pi_auth2 pi_auth3 pi_auth4 theta1 theta2 theta3 theta45 theta6 theta_auth2 theta_auth3 theta_auth4 sigma_v lambda_rep1 lambda_rep2 lambda_rep3 lambda_rep4 lambda_rep5 lambda_rep6 mlambda_rep1 mlambda_rep2 mlambda_rep3 mlambda_rep4 mlambda_rep5 mlambda_rep6 lambda_r

* Linear combination of x and beta's for rr and cite equations
mleval `mu_cite'=`b', equation(1) 
mleval `mu_rr'=`b', equation(2)

* Variance of cite model
mleval	`lnsigma_phi'= `b', equation(3) scalar
generate double `sigma_phi'=exp(`lnsigma_phi')


* Evaluating authors' extra-cite parameters
mleval `theta1'= `b', 		equation(4) scalar
mleval `theta2'= `b', 		equation(5) scalar
mleval `theta3'= `b', 		equation(6) scalar
mleval `theta45'= `b', 		equation(7) scalar
mleval `theta6'= `b', 		equation(8) scalar
mleval `theta_auth2'= `b', 	equation(9) scalar
mleval `theta_auth3'= `b', 	equation(10) scalar
mleval `theta_auth4'= `b', 	equation(11) scalar

* Evaluating other parameters: sigma_v and lambda_r (coefficient of control function)
mleval `sigma_v'=`b', 		equation(12) scalar

qui{
* Generalized residuals
mleval `lambda_r'=`b', 		equation(13) scalar

mleval `pi_rep1'=`b', equation(14) scalar /* Note!! I have to put referees' reports variables as a first thing in the rr equation */
mleval `pi_rep2'=`b', equation(15) scalar
mleval `pi_rep3'=`b', equation(16) scalar
mleval `pi_rep4'=`b', equation(17) scalar
mleval `pi_rep5'=`b', equation(18) scalar
mleval `pi_rep6'=`b', equation(19) scalar
mleval `pi_pub1'=`b', equation(20) scalar /* Note!! I have to put authors' variables as second thing in the rr equation */
mleval `pi_pub2'=`b', equation(21) scalar
mleval `pi_pub3'=`b', equation(22) scalar
mleval `pi_pub45'=`b', equation(23) scalar
mleval `pi_pub6'=`b', equation(24) scalar
mleval `pi_auth2'=`b', equation(25) scalar
mleval `pi_auth3'=`b', equation(26) scalar
mleval `pi_auth4'=`b', equation(27) scalar

generate double `mu_rr1'=`mu_rr'+`pi_rep1'*frReject+`pi_rep2'*frNoRec+`pi_rep3'*frWeakRR+`pi_rep4'*frRR+`pi_rep5'*frStrongRR+`pi_rep6'*frAccept+`pi_pub1'*pub1+`pi_pub2'*pub2+`pi_pub3'*pub3+`pi_pub45'*pub45+`pi_pub6'*pub6+`pi_auth2'*auth2+`pi_auth3'*auth3+`pi_auth4'*auth4
generate double `g_resid'= (normalden(`mu_rr1'))/normal(`mu_rr1')  if $ML_y2 ==1
replace `g_resid'= - ( normalden(`mu_rr1')/(normal(-`mu_rr1') ) ) if $ML_y2 ==0


* For easiness of notation: I create the constrained coefficients on referees variables in the cite model
scalar `lambda_rep1'=`sigma_v'*`pi_rep1'
scalar `lambda_rep2'=`sigma_v'*`pi_rep2'
scalar `lambda_rep3'=`sigma_v'*`pi_rep3'
scalar `lambda_rep4'=`sigma_v'*`pi_rep4'
scalar `lambda_rep5'=`sigma_v'*`pi_rep5'
scalar `lambda_rep6'=`sigma_v'*`pi_rep6'

* For easiness of notation: I define the constrained coefficients for authors' variables and their respective variable
generate double `extrac_1'=(`pi_pub1'*`sigma_v'+`theta1')*pub1
generate double `extrac_2'=(`pi_pub2'*`sigma_v'+`theta2')*pub2
generate double `extrac_3'=(`pi_pub3'*`sigma_v'+`theta3')*pub3
generate double `extrac_45'=(`pi_pub45'*`sigma_v'+`theta45')*pub45
generate double `extrac_6'=(`pi_pub6'*`sigma_v'+`theta6')*pub6
generate double `extrac_auth2'=(`pi_auth2'*`sigma_v'+`theta_auth2')*auth2
generate double `extrac_auth3'=(`pi_auth3'*`sigma_v'+`theta_auth3')*auth3
generate double `extrac_auth4'=(`pi_auth4'*`sigma_v'+`theta_auth4')*auth4

* Elements that enter the log-likeliood
generate double `constr_auth_coeff'=`extrac_1'+`extrac_2'+`extrac_3'+`extrac_45'+`extrac_6'+`extrac_auth2'+`extrac_auth3'+`extrac_auth4'
generate double `constr_rep_coeff' =`lambda_rep1'*frReject+`lambda_rep2'*frNoRec+`lambda_rep3'*frWeakRR+`lambda_rep4'* frRR+`lambda_rep5'*frStrongRR+`lambda_rep6'*frAccept
generate double `ctrl_fct'=`lambda_r'*`g_resid'

generate double `pr_rr'=ln(normal(`mu_rr1')) if $ML_y2 ==1
replace `pr_rr'=ln(normal(-`mu_rr1')) if $ML_y2 ==0
*replace `lnfj'=`pr_rr'+ln(normalden($ML_y1 ,`mu_cite'  + `lambda_r'*`g_resid', `sigma_phi'))
replace `lnfj'=`pr_rr'+ln(normalden($ML_y1 , `mu_cite'+`constr_auth_coeff'+`constr_rep_coeff'+`ctrl_fct', `sigma_phi'))
if (`todo'==0) exit

}
end

ml model lf0 myML_nlc (asinh_cites= rr `Fields' `FixedEffects') (rr= `Fields' `FixedEffects' neditor_rr_loo2) /lnsigma_phi /theta1 /theta2 /theta3 /theta45 /theta6 /theta_auth2 /theta_auth3 /theta_auth4 /sigma_v /lambda_r /pi_rep1 /pi_rep2 /pi_rep3 /pi_rep4 /pi_rep5 /pi_rep6 /pi_pub1 /pi_pub2 /pi_pub3 /pi_pub45 /pi_pub6 /pi_auth2 /pi_auth3 /pi_auth4 , vce(cluster neditorid2)
ml check
ml search 
ml maximize, difficult
outreg2 using "../../../Output/OA/Tables/OA_Tab11.xls", replace excel long nonotes noobs nodepvar dec(2) noaster

estimates store structural_est


** Store estiamtes and standard errors in a matrix
matrix define RR_model=[_b[pi_rep1:_cons] \ [pi_rep1]_se[_cons] \  _b[pi_rep2:_cons] \ [pi_rep2]_se[_cons] \ ///
_b[pi_rep3:_cons] \ [pi_rep3]_se[_cons]  \ _b[pi_rep4:_cons] \ [pi_rep4]_se[_cons] \ _b[pi_rep5:_cons] \ [pi_rep5]_se[_cons] \ /// 
_b[pi_rep6:_cons] \ [pi_rep6]_se[_cons] \ ///
_b[pi_pub1:_cons] \ [pi_pub1]_se[_cons] \ _b[pi_pub2:_cons] \ [pi_pub2]_se[_cons] \ _b[pi_pub3:_cons] \ [pi_pub3]_se[_cons] \ ///
_b[pi_pub45:_cons] \ [pi_pub45]_se[_cons] \ _b[pi_pub6:_cons] \ [pi_pub6]_se[_cons] \ ///
_b[pi_auth2:_cons] \ [pi_auth2]_se[_cons] \ _b[pi_auth3:_cons] \ [pi_auth3]_se[_cons] \ ///
_b[pi_auth4:_cons] \ [pi_auth4]_se[_cons] \ _b[eq2:neditor_rr_loo2] \ [eq2]_se[neditor_rr_loo2] \ . \ . ]
matrix rownames RR_model= Reject SE NoRec SE WeakRR SE RR SE StrongRR SE Accept SE Pub1 SE Pub2 SE Pub3 SE Pub45 SE Pub6 SE Auth2 SE Auth3 SE Auth4plus SE EditorLOO SE EditorSignalStdDev SE
matrix colnames RR_model= RRmodel

matrix define Aux_coeff=[.\.\.\.\.\.\.\.\.\.\.\.\ ///
_b[theta1:_cons] \ [theta1]_se[_cons] \ _b[theta2:_cons] \ [theta2]_se[_cons] \ _b[theta3:_cons] \ [theta3]_se[_cons] \ ///
_b[theta45:_cons] \ [theta45]_se[_cons] \ _b[theta6:_cons] \ [theta6]_se[_cons] \ _b[theta_auth2:_cons] \ [theta_auth2]_se[_cons] \ ///
_b[theta_auth3:_cons] \ [theta_auth3]_se[_cons]  \  _b[theta_auth4:_cons] \ [theta_auth4]_se[_cons] \ . \ . \ _b[sigma_v:_cons] \ [sigma_v]_se[_cons] ]
matrix rownames RR_model= Reject SE NoRec SE WeakRR SE RR SE StrongRR SE Accept SE Pub1 SE Pub2 SE Pub3 SE Pub45 SE Pub6 SE Auth2 SE Auth3 SE Auth4plus SE EditorLOO SE EditorSignalStdDev SE
matrix colnames RR_model= AuxiliaryCoeffs


matrix define OATab11_cols34= [RR_model,Aux_coeff]
matrix colnames OATab11_cols34= RR_model Aux_coeff

* Lambda_1 coefficients (cite model - authors' variables)
*estimates restore structural_est
nlcom (lambda_r1: _b[sigma_v:_cons]*_b[pi_rep1:_cons] ) ///
(lambda_r2: _b[sigma_v:_cons]*_b[pi_rep2:_cons] ) ///
(lambda_r3: _b[sigma_v:_cons]*_b[pi_rep3:_cons] ) ///
(lambda_r4: _b[sigma_v:_cons]*_b[pi_rep4:_cons] ) ///
(lambda_r5: _b[sigma_v:_cons]*_b[pi_rep5:_cons] ) ///
(lambda_r6: _b[sigma_v:_cons]*_b[pi_rep6:_cons] ) ///
(lambda_pub1: _b[sigma_v:_cons]*_b[pi_pub1:_cons]+_b[theta1:_cons]) ///
(lambda_pub2:_b[sigma_v:_cons]*_b[pi_pub2:_cons]+_b[theta2:_cons]) ///
(lambda_pub3: _b[sigma_v:_cons]*_b[pi_pub3:_cons]+_b[theta3:_cons]) ///
(lambda_pub45: _b[sigma_v:_cons]*_b[pi_pub45:_cons]+_b[theta45:_cons]) ///
(lambda_pub6: _b[sigma_v:_cons]*_b[pi_pub6:_cons]+_b[theta6:_cons]) ///
(lambda_auth2: _b[sigma_v:_cons]*_b[pi_auth2:_cons]+_b[theta_auth2:_cons]) ///
(lambda_auth3: _b[sigma_v:_cons]*_b[pi_auth3:_cons]+_b[theta_auth3:_cons]) ///
(lambda_auth4: _b[sigma_v:_cons]*_b[pi_auth4:_cons]+_b[theta_auth4:_cons]) ///
(RRindicator: _b[eq1: rr]) ///
(CtrlFct: _b[lambda_r:_cons]), post
outreg2 using "../../../Output/OA/Tables/OA_Tab11.xls", append excel nonotes noobs nodepvar dec(2) noaster







