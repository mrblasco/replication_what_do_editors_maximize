set more off
set matsize 800

*** Please set path ***
*local folderpath /Users/username/Desktop/ReplicationFiles/Codes/Paper/Tables
*cd `folderpath'

* Output
local filename ../../../Output/Paper/Tables/Table1.xls

************************** Summary Statistics for all papers (including desk-rejected ones) *******************************
*** Read in data
use "../../../Data/Main/Pooled_cleaned.dta", clear

*** Define the variables for which we want summary statistics for
local depVars GScitePct asinh_cites notdeskrej rr
local controlVars1 pub0 pub1 pub2 pub3 pub45 pub6 ///
qje restat jeea restud
local controlVars2 auth1 auth2 auth3 auth4 fr_lab fr_labor fr_internat fr_healthurblaw fr_dev fr_hist fr_pub ///
fr_io fr_fin fr_macro missingfield fr_micro fr_other fr_theory fr_metrics

*** table output publication
local Model depVars asinh_cites notdeskrej rr pub1 pub2 pub3 pub45 pub6 qje restat jeea restud

keep `depVars' `controlVars1' `controlVars2'

outreg2 using `filename', dec(2) drop(GScitePct qje restat jeea restud) replace label excel ///
ctitle(All Papers, Pooled, Mean) sum(detail) eqkeep(mean sd N)


* Save, by journal
foreach j in QJE REStat JEEA REStud {

	use "../../../Data/Main/Pooled_cleaned.dta", clear
	keep if journal == "`j'"

	keep `depVars' `controlVars1' `controlVars2'

	outreg2 using `filename', dec(2) drop(GScitePct qje restat jeea restud) append label excel ///
	ctitle(All Papers, `j', Mean) sum(detail) eqkeep(mean sd N)

}


************************** Summary Statistics for NDR papers *******************************

*** Read in data and drop desk-rejected papers as well as those with only 1 referee assigned
use "../../../Data/Main/Pooled_cleaned.dta", clear
drop if noref == 1
keep if notdeskrej

* Create referee share above 3 pubs
forval i = 1/10 {
	gen refpub5_`i'_3plus = (refpub5_`i'>=3 & !missing(refpub5_`i'))
}
egen refpub5_count = rownonmiss(refpub5_? refpub5_??)

egen refpub5_3plus = rowtotal(refpub5_*_3plus)
replace refpub5_3plus = refpub5_3plus/refpub5_count
assert refpub5_3plus>=0 & refpub5_3plus<=1
label var refpub5_3plus "Share of referees with at least 3 recent publications"

local controlVars4 frDefReject-frAccept
local controlVars5 refpub5_3plus

tempfile temp1
save `temp1'

keep `depVars' `controlVars1' `controlVars2' `controlVars3' `controlVars4' `controlVars5'

outreg2 using `filename', dec(2) drop(GScitePct qje restat jeea restud) append label excel ///
ctitle(Non-Desk-Rejected Papers, Pooled, Mean) sum(detail) eqkeep(mean sd)

* Same, by journal
foreach j in QJE REStat JEEA REStud {

	*** All Non-Desk-Rejected REStud Papers
	use `temp1', clear
	drop if noref == 1
	keep if journal == "`j'"
	keep if notdeskrej
	 
	keep `depVars' `controlVars1' `controlVars2' `controlVars3' `controlVars4' `controlVars5'

	outreg2 using `filename', dec(2) drop(GScitePct qje restat jeea restud) append label excel ///
	ctitle(Non-Desk-Rejected Papers, `j', Mean) sum(detail) eqkeep(mean sd N) ///
	sortvar(asinh_cites notdeskrej rr ///
	pub0 pub1 pub2 pub3 pub45 pub6 ///
	auth1 auth2 auth3 auth4 ///
	fr_dev fr_metrics fr_fin fr_healthurblaw fr_hist fr_internat fr_io fr_lab fr_labor fr_macro fr_micro fr_pub fr_theory fr_other missingfield ///
	frDefReject-frAccept ///
	qje restat jeea restud) ///

}
