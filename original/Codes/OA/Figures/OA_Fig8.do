clear all
set more off

*** Please set path ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/OA/Figures
* cd `folderpath'

*********************************************

cap program drop OutcomesByEval
program define OutcomesByEval

local SplitVar "`1'"
local Restriction "`2'"
local TitleTag "`3'"
local FileTag "`4'"

local startx "`5'"
local endx "`6'"
local starty "`7'"
local endy "`8'"
local label "`9'"

preserve

`restriction'

******************
* Collapse to graph
******************

* Keep labels
levelsof `SplitVar'
local SplitVarLevels `r(levels)'
local NSplitVarLevels = wordcount("`SplitVarLevels'")
foreach s in `SplitVarLevels' {
	local VarLab`s' : label `SplitVar' `s'
}

* Collapse data to means and clustered SEs
foreach v of varlist GScites_asinh Decision_RR {

	tempfile temp`v'
	
	statsby `v'=_b[_cons] sd_`v'=_se[_cons] N=e(N) , ///
		by(Eval `SplitVar', missing) verb saving(`temp`v'', replace): ///
		reg `v' [aweight=Weight], vce(cluster PIDJournal)
		
}

use `tempGScites_asinh', clear
merge 1:1 Eval `SplitVar' using `tempDecision_RR', nogen 


* Create upper and lower bounds
foreach v in GScites_asinh Decision_RR {
	replace sd_`v' = 0 if missing(sd_`v')
	gen `v'_ub = `v' + 2*sd_`v'
	gen `v'_lb = `v' - 2*sd_`v'
}
drop sd_*

			
* Reshape wide prom/nonprom
reshape wide GScites_asinh Decision_RR *_ub *_lb N, i(Eval) j(`SplitVar')

* Replace N=0 for missing divisions
foreach v of varlist N? {
	replace `v' = 0 if missing(`v')
}

* Label split variables
foreach s in `SplitVarLevels' {
	disp "`s'"
	foreach v of varlist *`s' {
		label var `v' "`VarLab`s''"
	}
}


******************
* Graph it!
******************

* Color order
local ColorOrder blue red midblue orange_red
local Colors
forval i = 1/`NSplitVarLevels' {
	local thisColor : word `i' of `ColorOrder'
	local Colors `Colors' `thisColor'
	local LegendOrder `LegendOrder' `i'
}
* Coordinates for arrows
set obs 8
foreach s in startx endx starty endy {
	gen `s' = .
}
gen label = ""
local round = 0

foreach s in startx endx starty endy {
	local i = 1
	foreach m in ``s'' {
		replace `s' = `m' in `i'
		local ++i
	}
}
foreach s in label {
	local i = 1
	foreach m in ``s'' {
		replace `s' = "`m'" in `i'
		local ++i
	}
}
	
* Graph cites and RR
local iter=1
foreach v in GScites_asinh {

* Set y axis
if "`v'"=="GScites_asinh" {
	local max = 4.5
	local min = 1.5
	local inc = 0.5
	local title "cites"
	local ytitle "asinh(cites)"
}

* rcap bars
local rcapBars
foreach i in `SplitVarLevels' {
	local j = `i'+1
	local thisColor : word `j' of `ColorOrder'
	local rcapBars "`rcapBars' (rcap `v'_lb`i' `v'_ub`i' Eval, color(`thisColor'))"
}
disp "`rcapBars'"

* Which arrows
local length = wordcount("`startx'")/2
disp `length'
local start = `length'*(`iter'-1) + 1
local end = `length'*`iter'


twoway	(	scatter `v'? Eval,		///
			connect( direct direct direct direct ) lpattern( l l - - ) color( `Colors' )	///
			ylabel(`min'(`inc')`max', format(%9.1f))	///
			xlabel(1(1)7, valuelabel)	///
			xtitle("Referee recommendation")	///
			ytitle("`ytitle'")	///
			title("")	///
			legend(off)	///
		)	///
		`rcapBars'	///
		( pcarrow starty startx endy endx if _n>=`start' & _n<=`end' , color(black) mlab(label) mlabc(black) )	///
		, scheme(s1color) note("") name(g`v', replace)
	
graph export "../../../Output/OA/Figures/OA_Fig8_`FileTag'_`title'.pdf", replace

local ++iter

}



window manage close graph _all

restore

end

******************
* Prepare data
******************
use "../../../Data/Secondary/Pooled3_PaperReferee.dta", clear

* Drop missing evaluations
drop if missing(Eval)
drop if NRefAssign<2
drop if Decision_NDR==0
drop if Year>2013

* Keep needed vars
keep PID Journal PIDJournal Eval Decision_RR GScites_asinh NPubs35*

* Create weight
bysort PIDJournal: egen Weight = count(PIDJournal)
replace Weight = 1/Weight


*********************************************
				
OutcomesByEval "NPubs35_RefXAuthMax_High" 	"" "Split by auth. and ref. prominence" 	"AuthRefPubs"	///
				"5.5 2.5 4.5 2.5 0 0 0 0"	"5.5 2.5 4.5 2.5 0 0 0 0"	///
				"2.6 2.9 3.4 3.9 0 0 0 0"	"2.9 2.5 3.5 3.45 0 0 0 0 "  		///
				`" "Auth 0-2, Ref 0-2" "Auth 0-2, Ref 3+" "Auth 3+, Ref 0-2" "Auth 3+, Ref 3+"  "'	
				
