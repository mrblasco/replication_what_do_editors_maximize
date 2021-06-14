********************
* FUNCTION FOR ESTIMATIONS
********************

cap program drop NonParamEstimate
program define NonParamEstimate

local SlopeVars	"`1'"
local NonRefSlopeVars "`2'"
local IntVars	"`3'"

local controls "`4'"

local restriction "`5'"

local filetag "`6'"

preserve

********************
* Create parameters
********************

* Local for each referee recommendations slope and intercept
forval i = 1/5 {
	
	* Attach slope and intercept terms for each specified variable
	local part1
	local part2
	
	* Part 1 - intercepts
	local IntNVar = wordcount("`IntVars'")
	forval v = 1/`IntNVar' {
		local thisVar : word `v' of `IntVars'
		local part1 " `part1' + {Int_`thisVar'}*`thisVar'_`i' "
	}
	
	* part 2 - slopes
	local SlopeNVar = wordcount("`SlopeVars'")
	forval v = 1/`SlopeNVar' {
		local thisVar : word `v' of `SlopeVars'
		local part2 " `part2' + {Slope_`thisVar'}*`thisVar'_`i' "
	}
	local NonRefSlopeVarsNVar = wordcount("`NonRefSlopeVars'")
	forval v = 1/`NonRefSlopeVarsNVar' {
		local thisVar : word `v' of `NonRefSlopeVars'
		local part2 "`part2' + {Slope_`thisVar'}*`thisVar'   "
	}

	* part 3 - the referee recommendations - same for every one
	local part3 " ( {Rej}*(Eval_Reject_`i') + {NoRec}*(Eval_NoRec_`i') + {WeakRR}*(Eval_WeakRR_`i') + {RR}*(Eval_RR_`i') + {StrongRR}*(Eval_StrongRR_`i') + {Accept}*(Eval_Accept_`i') ) "
	
	local Ref`i' " 0 `part1' + exp(0 `part2')*`part3' "
	
}
disp "`Ref1'"
disp "`Ref2'"

* Editor RR rate
local EditorRRRate "{Editor_MeanRR_loo}*Editor_RR_LOO2"

* Fields (Missing omitted)
local allFields JEL_Intl_fr JEL_Exp_fr JEL_Labor_fr JEL_HealthUrbLaw_fr JEL_Dev_fr JEL_Hist_fr JEL_Pub_fr JEL_IO_fr JEL_Fin_fr JEL_Macro_fr JEL_Theory_fr JEL_Micro_fr JEL_Other_fr JEL_Metrics_fr

* Controls
local AllControls1 "{ap: AuthPubs_*} + {na: NAuthors_*} + {j: Journal_*} + {ffr: `allFields'} + {jr: JournalYear_*}"
local AllControls2 "{ap: } + {na: } + {j: } + {ffr: } + {jr: }"

* Controls for journal regressions
if strpos("`restriction'", "Journal")>=1 {
	* Set years with omitted cat.
	qui summa Year if `restriction'
	local min = r(min) + 1
	local max = r(max)
	local AllYears
	forval i = `min'/`max' {
		local AllYears "`AllYears' Year_`i'"
	}
	
	local AllControls1 "{ap: AuthPubs_*} + {na: NAuthors_*} + {ffr: `allFields'} + {yr: `AllYears'}"
	local AllControls2 "{ap: } + {na: } + {ffr: } + {yr: }"
}

* Controls for year regressions
if strpos("`restriction'", "Year")>=1 {
	local AllYears
	foreach j in JEEA QJE REStat REStud {
		qui summa Year if `restriction' & Journal=="`j'"
		local min = r(min) + 1
		local max = r(max)
		forval i = `min'/`max' {
			local AllYears "`AllYears' JournalYear_`j'`i'"
		}
	}
	local AllControls1 "{ap: AuthPubs_*} + {na: NAuthors_*} + {j: Journal_*} + {ffr: `allFields'} + {yr: `AllYears'}"
	local AllControls2 "{ap: } + {na: } + {j: } + {ffr: } + {yr: }"
}




********************
* MLE probabilistic
********************

mlexp	(Decision_RR* 		lnnormal({alpha} + (`Ref1' + `Ref2' + `Ref3' + `Ref4' + `Ref5')/NRefRespond + `AllControls1' + `EditorRRRate') +	///
		((1-Decision_RR)*	lnnormal( - ( {alpha} + (`Ref1' + `Ref2' + `Ref3' + `Ref4' + `Ref5')/NRefRespond + `AllControls2' + `EditorRRRate') ) ))	///
		if `restriction', vce(cluster Editor_NID2) iter(75)

		
eststo RR_`filetag', title("RR")

disp "`e(params)'"

* Create control function --> we do not need the control function excel output for the replication (if it does not works out outcommend them)
predict phat*, scores

rename phat1 rho1_`filetag'
export excel PID Journal rho1_`filetag' using "../../../Output/OA/Tables/OA_Tab6_Tab7_ControlFunction.xls", sheet("`filetag'") sheetreplace firstrow(var)


********************
* NL Estimations with control functions & mechanical bias
********************

* RR
local RR " + Decision_RR*{Control_RR} "

* Control function
local Ctrl " + rho1_`filetag'*{Control_fn} "

* Nonlinear slope estimation - RR and control function
cap noi	nl	(GScites_asinh = {alpha} + ///
			(`Ref1' + `Ref2' + `Ref3' + `Ref4' + `Ref5')/NRefRespond	+ `AllControls1' `RR' `Ctrl')	///
			if `restriction', vce(cluster Editor_NID2) iter(200) initial(`InitValCites')

		
eststo cites_rrctrl_`filetag', title("asinh(Cites) (both)")

********************
* output results
********************

*local filetag "Pub_J_F"
* Filename locals
local filename "../../../Output/OA/Tables/OA_Tab6_Tab7_OutcomesNLMLE_`filetag'.csv"


* esttab
esttab   cites_rrctrl_`filetag' RR_`filetag' using `filename', replace	///
	title("RR-stage regressions: paper outcomes by referee using: `SlopeVars' (robust SEs)")	///
	mtitles("asinh(cites) both" "RR")	///
	csv noeqlines se(%9.3f) nostar b(%9.3f) se stats(N r2 ic converged converge ll)


eststo clear


restore

end


