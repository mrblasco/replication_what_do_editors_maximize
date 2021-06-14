clear all
set more off

*** Please set path ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/Paper/Figures
* cd `folderpath'


***************************************
* Wrangle data from estimation outputs
***************************************

* Dummy for black-white graph (bw=1: black and white)
local bw = 1

* Open data; keep wanted obs.
import excel "../../../Data/Secondary/Outcomes_NLML.xlsx", firstrow clear

keep v1 Cites_Pub_J_F RR_Pub_J_F index n
drop if _n<=2

* reshape SEs
replace v1 = v1[_n-1] if index==2
reshape wide Cites_Pub_J_F RR_Pub_J_F n, i(v1) j(index)
sort n1

rename Cites*1 Coeff_Cites
rename RR*1 Coeff_RR
rename RR*2 SE_RR
drop Cites*2

* Destring
foreach v of varlist SE_* {
	replace `v' = subinstr(`v', "(", "", .)
	replace `v' = subinstr(`v', ")", "", .)
}
destring Coeff* SE*,  replace

********************
* Prepare graph data
********************

* Keep wanted coefficients
local AuthPubs AuthPubs_1 AuthPubs_2 AuthPubs_3 AuthPubs_4 AuthPubs_6
local Reviews Rej NoRec WeakRR RR StrongRR Accept
local RefPubs Slope_Ref_Prom3 Int_Ref_Prom3

local all 
foreach s in `AuthPubs' {
	local all1 `" `all1', "`s'" "'
}
foreach s in `Reviews' {
	local all2 `" `all2', "`s'" "'
}
foreach s in `RefPubs' {
	local all3 `" `all3', "`s'" "'
}
disp `all'
keep if inlist(v1 `all1') | inlist(v1 `all2') | inlist(v1 `all3')

* Create scaled coefficients for prominent referees
foreach v in Cites RR {
	local scale = Coeff_`v'[2]
	local scale = exp(`scale')
	local intcpt = Coeff_`v'[1]
	gen Coeff_`v'_Hi = `scale'*Coeff_`v'  if _n>=3 & _n<=8
}
local intcptse = SE_RR[2]
gen SE_RR_Hi = sqrt( (`scale'*SE_RR)^2  ) if _n>=3 & _n<=8

rename (Coeff_Cites Coeff_RR SE_RR) (Coeff_Cites_Lo Coeff_RR_Lo SE_RR_Lo)

reshape long Coeff_Cites Coeff_RR SE_RR, i(v1) j(scale) string
sort scale n1
drop if missing(Coeff_Cites)

* Create 2 SE CIs
gen Low_RR = Coeff_RR - 2*SE_RR
gen High_RR = Coeff_RR + 2*SE_RR

* Create weight
gen weight = 1/SE_RR

* Rename coeffs for labels
replace v1 = subinstr(v1, "AuthPubs_", "", .)
replace v1 = "4-5" if v1=="4"
replace v1 = "6+" if v1=="6"

replace v1 = "3+ pub. referee intercepts" if v1=="Int_Ref_Prom3"

********************
* Graph
********************

* Graph options
local XOpts " xlabel(-0.5(0.5)3) xscale(range(-0.5 3)) ylabel(-1(1)7) yscale(range(-1 7)) "

* Colors
if `bw' == 1 local Colors "gs8 gs2 gs12 gs8"
if `bw' != 1 local Colors "emidblue blue red emidblue"
local LPatterns " l - - "

local i = 1
* "startn, endn, label pos, start lfit"
foreach range in "1 6 9 0" "9 14 9 -0.45" "15 19 7 -0.45" "7 7 3 0" {

	* Graph locals
	local start: word 1 of `range'
	local end: word 2 of `range'
	local pos: word 3 of `range'
	local lfitstart: word 4 of `range'
	
	local col: word `i' of `Colors'
	local lp: word `i' of `LPatterns'
	

	local G1	(scatter Coeff_RR Coeff_Cites if _n>=`start' & _n<=`end', mlabel(v1) mlabc(`col') mlabpos(`pos') color(`col') `XOpts' )
	local G2	(rcap High_RR Low_RR Coeff_Cites if _n>=`start' & _n<=`end', color(`col') `XOpts' ) 
	local G3	(lfit Coeff_RR Coeff_Cites [aweight=weight] if _n>=`start' & _n<=`end' , estopts(nocons) range(`lfitstart' 3) color(`col') lpattern(`lp') `XOpts' )
	
	if `start'-`end'==0 {
		local G3
	}
	
	local Gall_`i' `G1' `G2' `G3'
	
	if `i'==1 {
		local Gall_`i' `G1' `G3'
	}
	
	local ++i

}
macro li
twoway `Gall_1' `Gall_2' `Gall_3' `Gall_4'	///
		, legend(order(1 "3+ pub. referee reports" 3 "0-2 pub. referee reports" 6 "Author publications") col(1) pos(11) ring(0)) 	///
		xtitle("Least-squares coefficients on asinh(cites)") ytitle("Maximum likelihood coefficients on R&R")	///
		scheme(s1color)
if `bw' == 1 graph export "../../../Output/Paper/Figures/Fig3b_bw.pdf", replace
if `bw' != 1 graph export "../../../Output/Paper/Figures/Fig3b.pdf", replace
