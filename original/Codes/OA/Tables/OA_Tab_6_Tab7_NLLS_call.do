clear all
set more off
eststo clear

* please adjust path here
* local folderpath /Users/username/Desktop/ReplicationFiles/Codes/OA/Tables
* cd `folderpath'

run OA_Tab_6_Tab7_NLLS_data.do
run OA_Tab_6_Tab7_NLLS_fn.do

************************
* Function calls 
************************

local Journals Journal_QJE Journal_REStat Journal_REStud
* Fields: Missing omitted
local Fields JEL_Intl_fr JEL_Exp_fr JEL_Labor_fr JEL_HealthUrbLaw_fr JEL_Dev_fr JEL_Hist_fr JEL_Pub_fr JEL_IO_fr JEL_Fin_fr JEL_Macro_fr JEL_Theory_fr JEL_Micro_fr JEL_Other_fr JEL_Metrics_fr
local AuthProm Auth_Prom3
local RefPromIndicators Ref_nTop35_1 Ref_nTop35_2 Ref_nTop35_3 Ref_nTop35_4 Ref_nTop35_5 Ref_nTop35_6


* Basic specifications

NonParamEstimate	""	///
					""	///
					"" ///
					"" "1" "Base"			

NonParamEstimate	"Ref_Prom3"	///
					"`Journals'"	///
					"Ref_Prom3" ///
					"" "1" "Pub_J"	

NonParamEstimate	"Ref_Prom3"	///
					"`Journals' `Fields'"	///
					"Ref_Prom3" ///
					"" "1" "Pub_J_F"	

NonParamEstimate	"Ref_Prom3 Ref_NPastRepEd_asinh"	///
					"`Journals' `Fields'"	///
					"Ref_Prom3 Ref_NPastRepEd_asinh" ///
					"" "1" "Pub_J_F_Re"	
		
*Loop by journal
foreach j in JEEA QJE REStat REStud {
					
NonParamEstimate	"Ref_Prom3"	///
					"`Fields'"	///
					"Ref_Prom3" ///
					"" `"Journal=="`j'""' "Base`j'"

}
					

					
			
