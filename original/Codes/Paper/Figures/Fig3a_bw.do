clear all
set more off

*** Please set path ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/Paper/Figures
* cd `folderpath'

* Open data; keep wanted obs.
use "../../../Data/Secondary/Pooled2_PaperReferee.dta", clear
keep if RefNum==1
keep if Decision_NDR==1
drop if NRef==1

* Print-version black and white (bw=1: black and white)
local bw = 1


* Locals
local evals Evalfr_Reject Evalfr_NoRec Evalfr_WeakRR Evalfr_RR Evalfr_StrongRR Evalfr_Accept
local authpubs i.NPubs35_AuthMax
local nauth i.NAuthors
local fields JEL_*_fr
local journalyear JournalN##Year

* Estimations - store the coefficients
reg GScites_asinh `evals' `authpubs' `nauth' `fields' `journalyear', vce(cluster Editor_NID2)

mat cites = e(b)'
svmat2 cites, name(cites) rn(names)

probit Decision_RR `evals' `authpubs' `nauth' `fields' `journalyear' Editor_RR_LOO2, vce(cluster Editor_NID2)

mat rr = e(b)'
svmat rr, name(rr)

mat var = vecdiag(e(V))'
svmat var, name(var)


* Rename
replace names = subinstr(names, "Evalfr_", "fr", .)
replace names = subinstr(names, ".NPubs35_AuthMax", " pubs", .)

*replace names = "1 pub" if names=="1 pubs"
replace names = "4-5 pubs" if names=="4 pubs"
replace names = "6+ pubs" if names=="6 pubs"
replace names = subinstr(names, " pubs", "", .)


* Create wanted vars for graphs
gen weight = 1/var
replace var = sqrt(var)

* Create CIs with probit coeffs.
gen rr1_low = rr1 - 2*var
gen rr1_high = rr1 + 2*var


if `bw' ==1 local color1 gs4
if `bw' ==1 local color2 gs12

if `bw' !=1 local color1 red
if `bw' !=1 local color2 blue


* Graph
local XOpts " xlabel(-0.5(0.5)3) xscale(range(-0.5 3)) ylabel(-1(1)7) yscale(range(-1 7)) "

twoway	(scatter rr1 cites if _n<=6, mlabel(names) mlabc(`color1') mlabpos(9) color(`color1') `XOpts' )	///
		(rcap rr1_high rr1_low cites if _n<=6, color(`color1') `XOpts' )	///
		(lfit rr1 cites [aweight=weight] if _n<=6 , estopts(nocon) range(-0.45 2.95) color(`color1') lpattern(-) `XOpts' )	///
		(scatter rr1 cites if _n>=8 & _n<=12 , mlabel(names) mlabc(`color2') mlabpos(1) mlabgap(*2) color(`color2') `XOpts' )	///
		(rcap rr1_high rr1_low cites if _n>=8 & _n<=12 , color(`color2') `XOpts')	///
		(lfit rr1 cites [aweight=weight] if  _n>=8 & _n<=12 ,  estopts(nocon) range(-0.45 2.95) color(`color2') lpattern(-) `XOpts' )	///
		, legend(order(1 "Referee reports" 4 "Author publications") col(1) pos(11) ring(0)) 	///
		xtitle("OLS coefficients on asinh(cites)") ytitle("Probit coefficients on R&R")	///
		scheme(s1color)


if `bw' == 1 graph export "../../../Output/Paper/Figures/Fig3a_bw.pdf", replace
if `bw' != 1 graph export "../../../Output/Paper/Figures/Fig3a.pdf", replace

