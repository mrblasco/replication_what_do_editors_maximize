set more off

clear all
set more off

*** Please set path ***
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/OA/Figures
* cd `folderpath'

use "../../../Data/Main/Pooled_cleaned.dta", clear

tempfile original
save `original'

keep if notdeskrej
drop if noref==1


*******************************************
** Artificial Histogram (from bar chart) **
*******************************************

use "../../../Data/Main/Pooled_cleaned.dta", clear

rename asinh_cites GScites_asinh

gen increments = GScites_asinh - mod(GScites_asinh, 0.5)

collapse (count) count_cites = GScites_asinh ///
		(min) min_cites = GScites_asinh ///
		(max) max_cites = GScites_asinh, ///
	by(increments decision journal)

collapse (sum) count_cites ///
		(min) min_cites ///
		(max) max_cites, ///
	by(increments decision)

bysort decision: egen cites_by_decision = sum(count_cites)
gen frac_cites = count_cites/cites_by_decision

assert increments == 0 if min_cites == 0 | max_cites == 0
assert min_cites == 0 & max_cites == 0 if increments == 0

drop count_cites min_cites max_cites cites_by_decision

reshape wide frac_cites, i(increments) j(decision, string)
order increments frac_citesDeskRej frac_citesRejected frac_citesRR

gen inc_lower = increments - 0.5
tostring inc_lower, replace
tostring increments, gen(inc_upper)
gen increments_graph = "[" + inc_lower + ", " + inc_upper + ")"
replace increments_graph = "[6, infinity)" if increments_graph == "[6, 6.5)"
drop inc_lower inc_upper
encode increments_graph, gen(increments_graph1)

	
graph bar frac_citesDeskRej frac_citesRejected frac_citesRR, ///
	over(increments, ///
			label(labsize(vsmall)) ///
			relabel(1 "[0, .5)" 2 "[.5, 1)" 3 "[1, 1.5)" 4 "[1.5, 2)" 5 "[2, 2.5)" 6 "[2.5, 3)" 7 "[3, 3.5)" 8 "[3.5, 4)" 9 "[4, 4.5)" 10 "[4.5, 5)" 11 "[5, 5.5)" 12 "[5.5, 6)" 13 "[6, infinity)")) ///
	legend(ring(0) pos(2) label(1 "Desk Reject") label(2 "Rejected") label(3 "R&R") ) ///
	title("Distribution of Citations, All Journals") ///
	ytitle("fraction of submissions within decision category") ///
	b1title("asinh(Google Cites)") /// 
	bar(1, lcolor(blue) fcolor(blue)) bar(2, lcolor(red) fcolor(red)) bar(3, lcolor(green) fcolor(green)) ///
	graphregion(color(white))
graph export "../../../Output/OA/Figures/OA_Fig1b.pdf", replace

graph bar frac_citesDeskRej frac_citesRejected frac_citesRR, ///
	over(increments, ///
			label(labsize(vsmall)) ///
			relabel(1 "[0, .5)" 2 "[.5, 1)" 3 "[1, 1.5)" 4 "[1.5, 2)" 5 "[2, 2.5)" 6 "[2.5, 3)" 7 "[3, 3.5)" 8 "[3.5, 4)" 9 "[4, 4.5)" 10 "[4.5, 5)" 11 "[5, 5.5)" 12 "[5.5, 6)" 13 "[6, infinity)")) ///
	legend(ring(0) pos(2) label(1 "Desk Reject") label(2 "Rejected") label(3 "R&R") ) ///
	title("Distribution of Citations, All Journals") ///
	ytitle("fraction of submissions within decision category") ///
	b1title("asinh(Google Cites)") ///
	bar(1, lcolor(black) fcolor(white)) bar(2, lcolor(black) fcolor(gray)) bar(3, lcolor(black) fcolor(black)) ///
	graphregion(color(white))
graph export "../../../Output/OA/Figures/OA_Fig1b_bw.pdf", replace

exit


