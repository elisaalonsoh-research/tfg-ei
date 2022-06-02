/***********************************************************************
T2 TRIPLE DIFFERENCE 
AUTHOR: Elisa Alonso Herrero 
DATE: 15/05/2022
DESCRIPTION: This script takes the CPS Supplements raw, and saves them as dta
				files. For the unbalanced, it
OUTPUT: 
	
***********************************************************************/
	
	clear
	set more off
	cap log close


	use "${final}/final_voters.dta", clear
	
	gen post_hisp = post_activation*hispanic
	gen post_black = post_activation*black
	
	reg voted post_hisp ${controls_indv} i.survey_date i.fips, vce(cluster fips)
	
	
	/* EXAMPLE
	esttab foodinsecure_`group' runoutfood_`group' phq4_score_`group' i_depressed_somep_`group' alcohol_days_perwk_`group' smoke_now_`group' using 	"${tables}/implementation/fpuc600/implementation_fpuc600_`group'_${date}.tex", ///
		keep(postFPUC600 _cons) ///
		se ///
		b(3) ///
		ar2 ///
		nogaps ///
		star(* 0.10 ** 0.05 *** 0.01) ///
		title("Effects of the Start of FPUC600 on Food Security and Well-being (`group')") ///
		mtitles("Food insecure" "Ran out of food" "PHQ4 Score" "Depressed" "Alcohol Days per Week" "Smoke") 		/// 
		label ///
		compress ///
		booktabs ///
		replace
		restore
		
		*/