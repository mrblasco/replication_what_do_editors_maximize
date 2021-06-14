set more off

*** Please set path ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Data/Main
* cd `folderpath'

************** Read in and Combine Datasets from Different Journals ******************
use ../Raw/REStud.dta, clear
append using ../Raw/QJE.dta
append using ../Raw/JEEA.dta
append using ../Raw/REStat.dta

*** We consider only papers up to 2013 to give them enough time to accrue citations.
*** Information in the EE files for papers at REStud before 2005 were not very reliable
drop if year > 2013 | (year<2005 & journal == "REStud")
drop if year == 2002 & journal == "JEEA"
drop if missing == 1

* Dummy for our scraper being unable to find the paper on Google Scholar
gen scraper_missed = 0
replace scraper_missed = 1 if GScites == .
replace GScites = 0 if GScites == .


********************* Prepare Wide Data File, where the Unit of Observation is a Paper *****************************


******************************************************************************************************
*************************************** Outcome Variables ********************************************

********************** Code Editor Decisions ********************
drop if decision == ""
*** Replace "DeskRej" decisions with "Rejected" if number of referees who respond is positive
replace decision = "Rejected" if decision == "DeskRej" & norefresp > 0

*** Dummy for papers not being desk-rejected
gen notdeskrej = (decision=="RR" | decision=="Rejected")
replace notdeskrej = 0 if norefresp == 0
label var notdeskrej "Not Desk Rejected"

***Make rr a dummy variable for R&R
gen rr = 0
replace rr = 1 if decision=="RR"
label var rr "R&R"


*** Regenerate variable for number of referee reports (norefresp) to ensure consistency with evaluation variables (eval1 to eval10)
drop norefresp
gen norefresp = (eval1!="") + (eval2!="") + (eval3!="") + (eval4!="") + (eval5!="") + (eval6!="") + (eval7!="") + (eval8!="") + (eval9!="") + (eval10!="")

*** Drop papers with no referee reports but were coded as R&R (may be papers for symposiums, etc.)
drop if decision == "RR" & norefresp == 0


**************** Generate Citation Percentiles **************
*** Google Scholar Citations
egen GScitePct_raw = xtile(GScites), by(journal year) nq(1000)
replace GScitePct_raw = GScitePct_raw/10
set seed 1000
gen jitter = (runiform()-0.5)/10
gen GScites_jitter = GScites + jitter
egen GScitePct = xtile(GScites_jitter), by(journal year) nq(1000) 
replace GScitePct = GScitePct/10
drop jitter
*** Web-Of-Science Citations
replace WOScites = 0 if WOScites == .
set seed 1000
gen jitter = (runiform()-0.5)/10
gen WOScites_jitter = WOScites + jitter
egen WOScitePct = xtile(WOScites_jitter), by(journal year) nq(1000) 
replace WOScitePct = WOScitePct/10


*** Replace top-coded REStud Citations with Means at Other Journals ***
gen GScites_nonrestud200 = GScites
replace GScites_nonrestud200 = . if (journal == "REStud") | GScites<200
bysort yearsubmit: egen nonrestud_top_mean = mean(GScites_nonrestud200)

forvalues i = 2005/2013 {
	replace GScites = nonrestud_top_mean if journal == "REStud" & GScites==200 & yearsubmit == `i'
}

gen WOScites_nonrestud50 = WOScites
replace WOScites_nonrestud50 = . if (journal == "REStud") | WOScites<50
bysort yearsubmit: egen nonrestud_top_meanWOS = mean(WOScites_nonrestud50)

forvalues i = 2005/2013 {
	replace WOScites = nonrestud_top_meanWOS if journal == "REStud" & WOScites==50 & yearsubmit == `i' & nonrestud_top_meanWOS!=.
}

*** Define other citation measures used in the paper.
gen asinh_cites = asinh(GScites)
gen asinh_wos = asinh(WOScites)
gen logplusone_cites = log(GScites+1)

* There are a small number of cases (6 in total) where publication information for referees who gave a report is missing;
* In such cases, we code the publications as zero instead of missing (similar to our treatment of missing author publications).
replace refpub5_1=0 if refpub5_1==. & eval1!=""
replace refpub5_2=0 if refpub5_2==. & eval2!=""
replace refpub5_3=0 if refpub5_3==. & eval3!=""
replace refpub5_4=0 if refpub5_4==. & eval4!=""
replace refpub5_5=0 if refpub5_5==. & eval5!=""
replace refpub5_6=0 if refpub5_6==. & eval6!=""
replace refpub5_7=0 if refpub5_7==. & eval7!=""
replace refpub5_8=0 if refpub5_8==. & eval8!=""
replace refpub5_9=0 if refpub5_9==. & eval9!=""
replace refpub5_10=0 if refpub5_10==. & eval10!=""
* Number of Referees assigned who do not decline *
gen noref = (refpub5_1!=.) + (refpub5_2!=.) + (refpub5_3!=.) + (refpub5_4!=.) + (refpub5_5!=.) + (refpub5_6!=.) + (refpub5_7!=.) + (refpub5_8!=.) + (refpub5_9!=.) + (refpub5_10!=.)
label var noref "Number of Referees who did not decline"

*** Generate dummies for being in the top p% within a given journal and year, where p is defined by the 
*** percentage of papers avoiding receiving R&R during that year.
*** Denote this dummy variable "topcited".
* There are still a small of ties in GScitePct, so jitter GScitePct a little more to break these for purposes of defining "topcited"
drop 	jitter
set 	seed 1000
gen 	jitter = (runiform()-0.5)/1000
gen 	GScitePct_jitter = GScitePct + jitter
* Generate top-cited variable
gen 	topcited = 0
egen 	rr_number = sum(rr), by (yearsubmit journal)
egen 	citation_rank = rank(-GScitePct_jitter), by (yearsubmit journal)
replace topcited = 1 if citation_rank<=rr_number
drop 	citation_rank rr_number GScitePct_jitter

***** Generate Referee Recommendation Fractions ******
gen 	frDefReject = ((eval1=="DefReject")+(eval2=="DefReject")+(eval3=="DefReject")+(eval4=="DefReject")+ ///
                       (eval5=="DefReject")+(eval6=="DefReject")+(eval7=="DefReject")+ ///
					   (eval8=="DefReject")+(eval9=="DefReject")+(eval10=="DefReject"))/(norefresp)

gen 	frReject 	= ((eval1=="Reject")+(eval2=="Reject")+(eval3=="Reject")+(eval4=="Reject")+ ///
                     (eval5=="Reject")+(eval6=="Reject")+(eval7=="Reject")+ ///
                     (eval8=="Reject")+(eval9=="Reject")+(eval10=="Reject"))/(norefresp)			 
					 
gen 	frNoRec 	= ((eval1=="NoRec")+(eval2=="NoRec")+(eval3=="NoRec")+(eval4=="NoRec")+ ///
                     (eval5=="NoRec")+(eval6=="NoRec")+(eval7=="NoRec")+ ///
                     (eval8=="NoRec")+(eval9=="NoRec")+(eval10=="NoRec"))/(norefresp)

gen 	frWeakRR 	= ((eval1=="WeakRR")+(eval2=="WeakRR")+(eval3=="WeakRR")+(eval4=="WeakRR")+ ///
                     (eval5=="WeakRR")+(eval6=="WeakRR")+(eval7=="WeakRR")+ ///
                     (eval8=="WeakRR")+(eval9=="WeakRR")+(eval10=="WeakRR"))/(norefresp)

gen 	frRR 		= ((eval1=="RR")+(eval2=="RR")+(eval3=="RR")+(eval4=="RR")+ ///
                     (eval5=="RR")+(eval6=="RR")+(eval7=="RR")+ ///
                     (eval8=="RR")+(eval9=="RR")+(eval10=="RR"))/(norefresp)

gen 	frStrongRR 	= ((eval1=="StrongRR")+(eval2=="StrongRR")+(eval3=="StrongRR")+(eval4=="StrongRR")+ ///
                     (eval5=="StrongRR")+(eval6=="StrongRR")+(eval7=="StrongRR")+ ///
                     (eval8=="StrongRR")+(eval9=="StrongRR")+(eval10=="StrongRR"))/(norefresp)

gen 	frAccept 	= ((eval1=="Accept")+(eval2=="Accept")+(eval3=="Accept")+(eval4=="Accept")+ ///
                     (eval5=="Accept")+(eval6=="Accept")+(eval7=="Accept")+ ///
                     (eval8=="Accept")+(eval9=="Accept")+(eval10=="Accept"))/(norefresp)


***** Generate Variables for Author Publication *****

*** Publications in the last 5 years
gen pub0 = authpub5 == 0
gen pub1 = authpub5 == 1
gen pub2 = authpub5 == 2
gen pub3 = authpub5 == 3
gen pub45 = (authpub5 >= 4 & authpub5 <=5)
gen pub6 = authpub5 >= 6

label var pub0 "Publications: 0"
label var pub1 "Publications: 1"
label var pub2 "Publications: 2"
label var pub3 "Publications: 3"
label var pub45 "Publications: 4-5"
label var pub6 "Publications: 6+"

*** Indicator that equals 1 for authors with publication at least 4 publications, 0 for 1 or less publications, and missing otherwise
gen pub4 = .
replace pub4 = 1 if authpub5>=4
replace pub4 = 0 if authpub5<=1

label var pub4 "1 if Pub>=4, =0 if Pub<=1, and missing otherwise"

***** Variables for Number of Authors *****
gen auth1 = (auth_count == 1)
gen auth2 = (auth_count == 2)
gen auth3 = (auth_count == 3)
gen auth4 = (auth_count >= 4)

label var auth1 "1 author"
label var auth2 "2 authors"
label var auth3 "3 authors"
label var auth4 "4+ authors"


**************** Generate Field Variables ******************

*** Generate Field Dummies ***
replace micro 		= 0 if micro == .
replace theory 		= 0 if theory == .
replace metrics 	= 0 if metrics == .
replace macro		= 0 if macro == .
replace internat 	= 0 if internat == .
replace fin 		= 0 if fin == .
replace pub 		= 0 if pub == .
replace labor 		= 0 if labor == .
replace healthurblaw = 0 if healthurblaw == .
replace hist 		= 0 if hist == .
replace io 			= 0 if io == .
replace dev 		= 0 if dev == .
replace lab 		= 0 if lab == .
replace other		= 0 if other == .

label var micro "Indicator for being in the field: Micro"
label var theory "Indicator for being in the field: Theory"
label var metrics "Indicator for being in the field: Econometrics"
label var macro "Indicator for being in the field: Macro"
label var internat "Indicator for being in the field: International"
label var fin "Indicator for being in the field: Finance"
label var pub "Indicator for being in the field: Public"
label var labor "Indicator for being in the field: Labor"
label var healthurblaw "Indicator for being in the field: Health, Urban, Law"
label var hist "Indicator for being in the field: History"
label var io "Indicator for being in the field: Industrial Organization"
label var dev "Indicator for being in the field: Development"
label var lab "Indicator for being in the field: Lab/Experiments"
label var other "Field Unclassified"


*** Generate Field fractions ***
gen 	missingfield = 0
egen 	no_fields = rowtotal(micro-other)
replace missingfield = 1 if no_fields==0
order 	missingfield, after(other)

label var missingfield "Field Missing"
label var no_fields "Number of Fields"

* Temporarily change no_fields to 1 when missingfield=1 to avoid generating missing values when dividing by 0
replace no_fields=1 if missingfield==1

gen fr_micro 	= micro/no_fields
gen fr_theory 	= theory/no_fields
gen fr_metrics 	= metrics/no_fields
gen fr_macro 	= macro/no_fields
gen fr_internat = internat/no_fields
gen fr_fin 		= fin/no_fields
gen fr_pub 		= pub/no_fields
gen fr_labor 	= labor/no_fields
gen fr_healthurblaw = healthurblaw/no_fields
gen fr_hist 	= hist/no_fields
gen fr_io 		= io/no_fields
gen fr_dev 		= dev/no_fields
gen fr_lab 		= lab/no_fields
gen fr_other 	= other/no_fields

label var fr_micro "Field Fraction: Micro"
label var fr_theory "Field Fraction: Theory"
label var fr_metrics "Field Fraction: Econometrics"
label var fr_macro "Field Fraction: Macro"
label var fr_internat "Field Fraction: International"
label var fr_fin "Field Fraction: Finance"
label var fr_pub "Field Fraction: Public"
label var fr_labor "Field Fraction: Labor"
label var fr_healthurblaw "Field Fraction: Health, Urban, Law"
label var fr_hist "Field Fraction: History"
label var fr_io "Field Fraction: Industrial Organization"
label var fr_dev "Field Fraction: Development"
label var fr_lab "Field Fraction: Lab/Experiments"
label var fr_other "Field Fraction: Unclassified"

* Change no_fields back to its original definition
replace no_fields = 0 if missingfield==1


************** Generate Journal Dummies ****************
gen qje 	= journal == "QJE"
gen restat 	= journal == "REStat"
gen jeea 	= journal == "JEEA"
gen restud 	= journal == "REStud"

label var qje "Indicator for QJE Paper"
label var restat "Indicator for REStat Paper"
label var jeea "Indicator for JEEA Paper"
label var restud "Indicator for REStud Paper"

************** Generate Year Dummies ****************
tab yearsubmit, gen(year)

gen year2003 = year1
gen year2004 = year2
gen year2005 = year3
gen year2006 = year4
gen year2007 = year5
gen year2008 = year6
gen year2009 = year7
gen year2010 = year8
gen year2011 = year9
gen year2012 = year10
gen year2013 = year11

label var year2003 "Year Indicator for 2003"
label var year2004 "Year Indicator for 2004"
label var year2005 "Year Indicator for 2005"
label var year2006 "Year Indicator for 2006"
label var year2007 "Year Indicator for 2007"
label var year2008 "Year Indicator for 2008"
label var year2009 "Year Indicator for 2009"
label var year2010 "Year Indicator for 2010"
label var year2011 "Year Indicator for 2011"
label var year2012 "Year Indicator for 2012"
label var year2013 "Year Indicator for 2013"

drop year1-year11

*** Generate factor variables for year and journal (for regressions in Stata)
egen journal_factor = group(journal), label
egen year_factor = group(yearsubmit), label

* Although the PID (paper id) in the datasets for our various journals are scrambled,
* they still uniquely identify papers, and the following variable is helpful for clustering
* when running regressions at the referee report level in R.
gen new_pid = "Journal=" + journal + ", PID=" + string(pid)

* Generate editor dummies (making sure ids are different for different journals)
replace editorid = editorid+100 if journal=="JEEA"
replace editorid = editorid+200 if journal=="REStud"

tab editorid, gen(journal_editor)

forvalues i = 1/6 {
    label variable journal_editor`i' "Editor Dummy (QJE)"
}
forvalues i = 7/20 {
    label variable journal_editor`i' "Editor Dummy (JEEA)"
}
forvalues i = 21/38 {
    label variable journal_editor`i' "Editor Dummy (REStud)"
}


*** Remove some extra variables
drop missing jitter WOScites_jitter GScites_nonrestud200 nonrestud_top_mean 


gen year=yearsubmit
gen njournal=1*qje+2*restud+3*restat+4*jeea
tab year journal

gen ndr=notdeskrej
gen dr=1-ndr

*** code to analyze aldens 38 editors plus restat
forvalues i = 1/38 {
 gen editor`i'=0
 replace editor`i'=1 if journal_editor`i'==1
}

gen editor39=0
replace editor39=1 if restat==1


sum editor1-editor39

forvalues i = 1/39 {
    sum editor`i' njournal year rr if editor`i'==1
}

** now re-set editor dummies  LK=35, restat=34, pooled 2013 only editors (3 at jeea, 1 at restud) =33 

forvalues i = 1/35 {
 gen neditor`i'=0
}

replace neditor1=1 if editor1==1
replace neditor2=1 if editor2==1
replace neditor3=1 if editor3==1
replace neditor4=1 if editor5==1
replace neditor5=1 if editor6==1
replace neditor6=1 if editor7==1
replace neditor7=1 if editor8==1
replace neditor8=1 if editor9==1
replace neditor9=1 if editor10==1
replace neditor10=1 if editor11==1
replace neditor11=1 if editor12==1
replace neditor12=1 if editor13==1
replace neditor13=1 if editor15==1
replace neditor14=1 if editor18==1
replace neditor15=1 if editor19==1
replace neditor16=1 if editor20==1
replace neditor17=1 if editor21==1
replace neditor18=1 if editor22==1
replace neditor19=1 if editor23==1
replace neditor20=1 if editor24==1
replace neditor21=1 if editor25==1
replace neditor22=1 if editor26==1
replace neditor23=1 if editor28==1
replace neditor24=1 if editor29==1
replace neditor25=1 if editor30==1
replace neditor26=1 if editor31==1
replace neditor27=1 if editor32==1
replace neditor28=1 if editor33==1
replace neditor29=1 if editor34==1
replace neditor30=1 if editor36==1
replace neditor31=1 if editor37==1
replace neditor32=1 if editor38==1
replace neditor33=1 if editor14==1 | editor16==1 | editor17==1 | editor27==1
replace neditor34=1 if editor39==1
replace neditor35=1 if editor4==1

gen neditorid=0

forvalues i = 1/35 {
    replace neditorid=`i' if neditor`i'==1
}

tab neditorid

*classify 0s to group 33
replace neditor33=1 if neditorid==0
replace neditorid=33 if neditorid==0

tab neditorid ndr, row

* Create alternative editor ID with REStat year#tenure
egen editor_restat = group(year edtenure) if journal=="REStat"

gen neditorid2 = neditorid
replace neditorid2 = 100*editor_restat if journal=="REStat"


forvalues i = 1/35 {
    sum neditor`i' njournal year ndr if neditor`i'==1
}

** Now get mean ndr rate by neditor, and leave one out mean of editor assigned to each paper
foreach v in "" 2 {

	gen c=1
	egen neditor_ndr_avg`v' = mean(ndr), by (neditorid`v')
	egen neditor_papers = sum(c), by (neditorid`v')

	gen neditor_ndr_loo`v'=(neditor_ndr_avg`v'- (ndr/neditor_papers))*(neditor_papers/(neditor_papers-1))

	drop c neditor_papers


	** Now get mean RR rate by neditor, and leave one out mean of editor assigned to each paper

	gen c=1
	egen neditor_rr_avg`v' = mean(rr) if (notdeskrej==1 & noref!=1), by(neditorid`v')
	egen neditor_papers = sum(c) if (notdeskrej==1 & noref!=1), by(neditorid`v')

	gen neditor_rr_loo`v'=(neditor_rr_avg`v'- (rr/neditor_papers))*(neditor_papers/(neditor_papers-1))

	drop c neditor_papers

}

assert neditor_rr_loo==neditor_rr_loo2 if journal!="REStat"
assert neditor_rr_loo!=neditor_rr_loo2 if journal=="REStat" & !missing(neditor_rr_loo)

saveold ../Main/Pooled_cleaned.dta, replace


********************* Prepare Long Data File, where the Unit of Observation is a Referee Report *****************************

***************
* Rename
***************

* Paper
rename pid PID
rename journal Journal
rename yearsubmit Year
rename auth_count NAuthors

drop journal_factor year_factor njournal year???? year qje restat jeea restud auth?

* Author
rename authpub5 NPubs35_AuthMax

drop pub0 pub1 pub2 pub3 pub45 pub6 pub4

* Decisions
*rename dectime Time_Decision
rename decision Decision
rename rr Decision_RR
rename notdeskrej Decision_NDR

drop dr ndr

* Cites
rename GScites GScites
rename GScitePct GScites_pct
rename asinh_cites GScites_asinh 
rename topcited Gscites_TopCoded

drop WOS* GScitePct_raw GScites_jitter logplusone_cites nonrestud_top_meanWOS

* Editor
rename editorid Editor_ID
rename neditorid Editor_NID
rename neditorid2 Editor_NID2
rename edtenure Editor_Tenure
rename neditor_ndr_loo Editor_NDR_LOO
rename neditor_ndr_loo2 Editor_NDR_LOO2
rename neditor_rr_loo Editor_RR_LOO
rename neditor_rr_loo2 Editor_RR_LOO2

drop neditor* editor* journal_editor*

* Referees
rename noref NRef
rename norefresp NRefRespond
rename norefassign NRefAssign
rename eval*  Eval*
rename refpub5_* NPubs35_Ref_*
rename pastreped* Ref_NPastRepEd_*
*rename pastrep* Ref_NPastRepJ_*

* Evals
foreach s in DefReject Reject NoRec WeakRR RR StrongRR Accept {
	rename fr`s' Evalfr_`s'
}

* Fields
rename *micro JEL_Micro*
rename *theory JEL_Theory*
rename *macro JEL_Macro*
rename *internat JEL_Intl*
rename *pub JEL_Pub*
rename *labor JEL_Labor*
rename *healthurblaw JEL_HealthUrbLaw*
rename *io JEL_IO*
rename *hist JEL_Hist*
rename *fin JEL_Fin*
rename *metrics JEL_Metrics*
rename *dev JEL_Dev*
rename *lab JEL_Exp*
rename *other JEL_Other*
rename JEL_*fr_ JEL_*_fr
rename no_fields JEL_NFields
rename missingfield JEL_Missing


***************
* Reshape referees to long
***************

* Reshape
reshape long Eval NPubs35_Ref_ Ref_NPastRepEd_ Ref_NPastRepJ_, i(PID Journal) j(RefNum)
rename *_ *

* Drop missing referees
drop RefNum
bysort Journal PID  (Eval) : gen RefNum = _n

drop if missing(Eval) & RefNum!=10

drop RefNum
bysort Journal PID  (Eval) : gen RefNum = _n


***************
* Publications
***************

* Top code author publications; pool 4-5
replace NPubs35_AuthMax = 6 if NPubs35_AuthMax>6 & !missing(NPubs35_AuthMax)
replace NPubs35_AuthMax = 4 if NPubs35_AuthMax==5

label define NPubs35 4 "4-5" 6 "6+"
label val NPubs35_AuthMax NPubs35

* Top code ref publications; pool 4-5
replace NPubs35_Ref = 6 if NPubs35_Ref>6 & !missing(NPubs35_Ref)
replace NPubs35_Ref = 4 if NPubs35_Ref==5

label val NPubs35_Ref NPubs35

* Create 0-2 and 3+ flags
gen NPubs35_AuthMax_High = (NPubs35_AuthMax>=3 & !missing(NPubs35_AuthMax))
label define NPubs35_AuthMax_High 0 "Auth 0-2" 1 "Auth 3+"
label val NPubs35_AuthMax_High NPubs35_AuthMax_High

gen NPubs35_Ref_High = (NPubs35_Ref>=3 & !missing(NPubs35_Ref))
label define NPubs35_Ref_High 0 "Ref 0-2" 1 "Ref 3+"
label val NPubs35_Ref_High NPubs35_Ref_High

gen NPubs35_RefXAuthMax_High = 2*NPubs35_AuthMax_High + 1*NPubs35_Ref_High
label define NPubs35_RefXAuthMax_High 0 "Auth 0-2, Ref 0-2" 1 "Auth 0-2, Ref 3+" 2 "Auth 3+, Ref 0-2"  3 "Auth 3+, Ref 3+"
label val NPubs35_RefXAuthMax_High NPubs35_RefXAuthMax_High

* label Refpubs
label val NPubs35_Ref NPubs35


***************
* Evaluations
***************

* Encode
local i = 1
foreach s in DefReject Reject NoRec WeakRR RR StrongRR Accept {
	replace Eval = "`i'" if Eval=="`s'"
	local ++i
	}
destring Eval, replace

label define Eval 1 "DefReject" 2 "Reject" 3 "NoRec" 4 "WeakRR" 5 "RR" 6 "StrongRR" 7 "Accept"
label val Eval Eval


***************
* Misc
***************

* Assert sum to 1: Eval
egen EvalSum = rowtotal(Evalfr_*)
assert inlist(round(EvalSum,0.01), 0, 1)

* Assert sum to 1: Field
egen FieldSum = rowtotal(JEL_*_fr JEL_Missing)
assert round(FieldSum,0.01)==1

drop FieldSum EvalSum

* Create Journal-PID
gen PIDJournal1 = Journal + string(PID, "%05.0f") + ": " + Journal + ", " + string(PID)
encode PIDJournal, gen(PIDJournal)
drop PIDJournal1

* Encode journal
encode Journal, gen(JournalN)

* Create Journal-Year
gen JournalYear1 = Journal + string(Year) + ": " + Journal + ", " + string(Year)
encode JournalYear1, gen(JournalYear)
drop JournalYear1

* Label Nauthors
label define NAuthors 4 "4+"
label val NAuthors NAuthors

***************
* Evaluations
***************
* Label variables:

* Cites
label var GScites_asinh "asinh(Cites)"
label var GScites_pct "percentile(Cites)"

* Number of referees
label var NRefRespond "Referees responded"

* Publications
label var NPubs35_AuthMax "Max auth pubs"
label var NPubs35_Ref "Ref pubs"

* Fraction evaluations
foreach s in DefReject Reject NoRec WeakRR RR StrongRR Accept {
	label var Evalfr_`s' "Ref recommendation: fr(`s')"
}


compress
saveold "../Secondary/Pooled3_PaperReferee.dta", replace
* Evaluation dummies
*** Create dummies for evaluation categories
gen DefReject = 0
gen Reject = 0
gen NoRec = 0
gen WeakRR = 0
gen RR = 0
gen StrongRR = 0
gen Accept = 0

replace DefReject = 1 if Eval == 1
replace Reject = 1 if Eval == 2
replace NoRec = 1 if Eval == 3
replace WeakRR = 1 if Eval == 4
replace RR = 1 if Eval == 5
replace StrongRR = 1 if Eval == 6
replace Accept = 1 if Eval == 7

drop if Eval==.

gen Eval1="DefReject" if Eval == 1
replace Eval1="DefReject" if Eval == 1
replace Eval1="Reject"  if Eval == 2
replace Eval1="NoRec"  if Eval == 3
replace Eval1="WeakRR" if Eval == 4
replace Eval1="RR" if Eval == 5
replace Eval1="StrongRR" if Eval == 6
replace Eval1="Accept" if Eval == 7

drop Eval
rename Eval1 Eval
* Save
sort PIDJournal
compress
saveold "../Secondary/Pooled2_PaperReferee.dta", replace
