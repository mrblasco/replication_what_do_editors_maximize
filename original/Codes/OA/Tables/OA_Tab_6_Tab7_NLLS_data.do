clear all
set more off

use "../../../Data/Secondary/Pooled3_PaperReferee.dta", clear

* Drop papers: Desk rejected, one ref, and missing evals
keep if Decision_NDR == 1
drop if NRef == 1
drop if missing(Eval)

* Randomly drop referees after 5
set seed 12345
gen random=uniform()
bysort Journal PID: egen rank = rank(random)
drop if rank>5
replace NRefRespond = 5 if NRefRespond>5
drop rank random

drop RefNum
bysort Journal PID (Eval): gen RefNum = _n

gen Ref_NPastRepEd_asinh = asinh(Ref_NPastRepEd)
gen Ref_NPastRepEd_ln1 = ln(Ref_NPastRepEd+1)

* for testing purposes
gen Ref_NPastRepJ_asinh = asinh(Ref_NPastRepJ)
gen Ref_NPastRepJ_ln1= ln(Ref_NPastRepJ)

* Create flags: evals
forval i = 1/7 {
	local lab : label Eval `i'
	gen Eval_`lab' = (Eval==`i')
}

* Create flags: JournalYear
foreach j in JEEA REStat REStud QJE {
	qui summa Year if Journal=="`j'"
	local start = r(min)+1
	forval y = `start'/2013 {
		gen JournalYear_`j'`y' = (Journal=="`j'" & Year==`y')
	}
}
* create flags: year
forval i = 2003/2013 {
	gen Year_`i' = (Year==`i')
}

* Journals
foreach j in REStat REStud QJE {
	gen Journal_`j' = (Journal=="`j'")
}

* NAuthors
forval i = 2/4 {
	gen NAuthors_`i' = (NAuthors==`i')
}

* Author prominence
foreach i in 1 2 3 4 6 {
	gen AuthPubs_`i' = (NPubs35_AuthMax==`i')
}

* Create flags: buckets for ref. prom.
gen NPubs35_Ref_b1 = (NPubs35_Ref<=1)
gen NPubs35_Ref_b2 = (NPubs35_Ref>=2 & NPubs35_Ref<=3)
gen NPubs35_Ref_b3 = (NPubs35_Ref>=4 & !missing(NPubs35_Ref))

* Rename for readability
rename NPubs35_Ref_High Ref_Prom3
rename NPubs35_AuthMax_High Auth_Prom3
drop NPubs35_RefXAuthMax_High

* Create fraction of referee prominent indicators
bysort PIDJournal: egen frRef_Prom3 = total(Ref_Prom3)
replace frRef_Prom3 = frRef_Prom3/NRefRespond
assert frRef_Prom3>=0 & frRef_Prom3<=1

* Reshape wide
drop Ref_NPastRepEd 
*testing 
drop Ref_NPastRepJ
local PastRep Ref_NPastRepEd_asinh Ref_NPastRepEd_ln1 Ref_NPastRepJ_asinh Ref_NPastRepJ_ln1

local Evals Eval_* Eval 
local RefProm NPubs35_Ref Ref_Prom3 NPubs35_Ref_b1 NPubs35_Ref_b2 NPubs35_Ref_b3

reshape wide  `PastRep' `Evals' `RefProm' , i(PIDJournal) j(RefNum)

* Assert nonmissing is up front
assert !missing(Eval4) if !missing(Eval5)
assert !missing(Eval3) if !missing(Eval4)
assert !missing(Eval2) if !missing(Eval3)
assert !missing(Eval1) if !missing(Eval2)

* Assert nrefrespond == number of evals
egen NEvals = anycount(Eval?), values(1 2 3 4 5 6 7)
assert NEvals==NRefRespond
drop NEvals

* Rename ref vars & replace missing with 0

foreach v in `PastRep' `Evals' `RefProm' `RefTime' {
	forval i = 1/5 {
		rename `v'`i' `v'_`i'
	}
}

foreach v of varlist *_1 *_2 *_3 *_4 *_5 {
	replace `v' = 0 if missing(`v')
}

keep	PID Journal JournalYear* Year* Journal* NAuthors* AuthPubs* NRef* Decision* Editor_NID* *_LOO* JEL_*_fr JEL_Missing GScites*	///
	Eval* *_1 *_2 *_3 *_4 *_5	///

drop *NDR* Ref_NPastRepEd_ln1_* 
