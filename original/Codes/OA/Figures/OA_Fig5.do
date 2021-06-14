clear all
set more off

********************
* Wrangle data from estimation outputs
********************

*** Please set path ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/OA/Figures
* cd `folderpath'

* Open data; keep wanted obs.
import excel "../../../Data/Secondary/OA_Tab5_fig.xls", clear cellrange(:I62)

* Rename variables to models
local i = 1
foreach v in B C D E F G H I {
	local j = `v'[4]
	
	if `i'<=4 {
		rename `v' Cites_`j'
	}
	if `i'>=5 {
		rename `v' RR_`j'
	}
	local ++i
}


drop if _n<=5

* reshape SEs
gen type = "Coeff" if !missing(A) 
replace type = "SE" if missing(A)
replace A = A[_n-1] if missing(A)

gen n1 = _n
reshape wide Cites_* RR_* n1, i(A) j(type) string
sort n1*
drop n1*

rename *Coeff Coeff_*
rename *SE SE_*

* Destring
foreach v of varlist SE_* {
	replace `v' = subinstr(`v', "(", "", .)
	replace `v' = subinstr(`v', ")", "", .)
}
destring Coeff* SE*,  replace


********************
* Prepare graph data
********************

* Rename coeffs for labels
replace A = subinstr(A, "Publications: ", "", .)
replace A = subinstr(A, " authors", "", .)
replace A = subinstr(A, "Field Fraction: ", "", .)

foreach s in 	`"Lab/Experiments Exp"' `"Labor Lab"' `" "Health, Urban, Law" HeaUrbLaw"' `"Development Dev"' ///
				`"History Hist"' `"Public Pub"' `" "Industrial Organization" IO"' `"Finance Fin"'	///
				`"Macro Macro"' `" "Field Missing" Missing"' `"Micro Micro"' `"Unclassified Other"' `"Theory Theory"' `"Econometrics Metrics"'	 {	///
				
	local original: word 1 of `s'
	local new: word 2 of `s'
	replace A = "`new'" if A=="`original'"
}


program define GraphCoeffs
program drop GraphCoeffs

local j "`1'"
local specs "`2'"

preserve

* Keep this journal
keep A *_`j'
rename *_`j' *

* Create 2 SE CIs
gen Low_RR = Coeff_RR - 2*SE_RR
gen High_RR = Coeff_RR + 2*SE_RR

* Create weight
gen weight = 1/SE_RR



********************
* Graph
********************

* Graph options
local XOpts " xlabel(-1(0.5)3) xscale(range(-1 3)) ylabel(-2(1)9) yscale(range(-2 9)) "

* Colors
local Colors "blue red maroon olive_teal"
local LPatterns " - - _ l"
local SymPat

local i = 1
* startn, endn, label pos, start lfit endlfit
foreach range in `specs'  {

	* Graph locals
	local start: word 1 of `range'
	local end: word 2 of `range'
	local pos: word 3 of `range'
	local lfitstart: word 4 of `range'
	local lfitend: word 5 of `range'
	
	local col: word `i' of `Colors'
	local lp: word `i' of `LPatterns'


	local G1	(scatter Coeff_RR Coeff_Cites if _n>=`start' & _n<=`end', mlabel(A) mlabc(`col') mlabpos(`pos') color(`col') `XOpts' )
	local G2	(rcap High_RR Low_RR Coeff_Cites if _n>=`start' & _n<=`end', color(`col') `XOpts' ) 
	local G3	(lfit Coeff_RR Coeff_Cites [aweight=weight] if _n>=`start' & _n<=`end' , estopts(nocons) range(`lfitstart' `lfitend') color(`col') lpattern(`lp') `XOpts' )
	
	if `start'-`end'==0 {
		local G3
	}
	
	local Gall_`i' `G1' `G2' `G3'
	
	local ++i

}
macro li
twoway `Gall_1' `Gall_2' `Gall_3' `Gall_4' 	///
		, legend(order(1 "Referee reports" 4 "Author pubs") col(1) pos(11) ring(0)) 	///
		xtitle("OLS coefficients on asinh(cites)") ytitle("Probit coefficients on R&R")	///
		scheme(s1color)

graph export "../../../Output/OA/Figures/OA_Fig5_`j'.pdf", replace

restore

end
* referee reports, author pubs, n authors, fields
* startn, endn, label pos, start lfit endlfit
GraphCoeffs JEEA 	`" "4 9 9 -.5 2.1"	"10 14 7 -1 3" "'
GraphCoeffs REStat 	`" "4 9 9 -.7 3"	"10 14 7 -1 3" "'
GraphCoeffs REStud 	`" "4 9 9 -.9 3"	"10 14 7 -1 3" "'
GraphCoeffs QJE 	`" "4 9 9 -.9 3"	"10 14 7 -1 3" "'


