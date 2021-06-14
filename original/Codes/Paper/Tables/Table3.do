clear all
set more off

*** Please set path ***
*local folderpath /Users/username/Desktop/ReplicationFiles/Codes/Paper/Tables
*cd `folderpath'

* List of all files that were produced from OA_Tab6_Tab7
local taglist Pub_J Pub_J_F Pub_J_F_Re

foreach tag in Base `taglist' {

	* Import the output AFTER running a_OA_Tab_6_7_NLLS_data	
	import delimited using "../../../Output/OA/Tables/OA_Tab6_Tab7_OutcomesNLMLE_`tag'.csv",  delim(",") clear

	* Remove excel formatting =""
	foreach v of varlist _all {
		replace `v' = subinstr(`v', char(34), "", .)
		replace `v' = subinstr(`v', "=", "", .)
	}

	* Keep wanted specifications
	keep v1 v2 v3

	* Remove _cons from NL output
	replace v1 = subinstr(v1, "_cons", "", .)

	* Remove coefficient prefixes from NL specifications
	foreach s in j_ na_ ap_ jr_ ffr_ {
		replace v1 = subinstr(v1, "`s'", "", .)
	}

	* Carry variable name forward
	gen v1_2 = v1
	replace v1 = v1_2[_n-1] if v1=="" & _n>=4
	replace v1 = v1_2[_n-2] if v1=="" & _n>=4
	drop v1_2

	drop if missing(v2) & missing(v3)

	* Create sort order
	gen group = 1 if missing(v3) & !missing(v2)
	replace group = 2 if missing(v2) & !missing(v3)

	gen n = _n
	bysort group v1 (n) : gen index = _n
	sort n

	* Re-merge
	tempfile  temp1
	preserve
	keep v1 v3 index n
	drop if missing(v3)
	save `temp1'
	restore

	drop v3
	drop if missing(v2)

	merge 1:1 v1 index using `temp1', nogen

	sort n
	drop group
	order v1 v2 v3 index

	* Save
	rename v2 Cites_`tag'
	rename v3 RR_`tag'
	
	tempfile temp_`tag'
	save `temp_`tag''

}


*************************
* Merge all
*************************

use `temp_Base', clear
foreach tag in `taglist' {
	merge 1:1 v1 index using `temp_`tag'', nogen update replace
}
sort n

order n index, last
replace v1 = "" if index==2

* Export the table for producing Fig3b_bw ( we need the full set for that )
export excel "../../../Data/Secondary/Outcomes_NLML.xlsx", replace firstrow(var)

* trim the table such that only relevant columns and rows are shown
keep if inlist(n, 5, 6, 7, 8, 9, 10, 11, 12, 53, 54, 67, 68, 175, 176, 177, 178, 295, 296)
* tidy up
drop n 
drop index

* Path
export excel "../../../Output/Paper/Tables/Table3.xlsx", replace firstrow(var)


