/***********************************************************************
T2 SUMMARY STATS (breakdown)
AUTHOR: Elisa Alonso Herrero 
DATE: 30/06/2022
DESCRIPTION: This script recreates Table 1 from the paper, which tabs 
			descriptive statistics (weighted).
OUTPUT: 
	
***********************************************************************/


*** T2 SUMMARY STATISTICS OF ELIGIBLE VOTERS (Using voters CPS)
	use "${final}/final_voters_cvap.dta", clear
	
	local sum_vars registered voted voted_reg $controls_indv ///
					$controls_county $controls_state
	
	eststo nonhisp: quietly estpost summarize `sum_vars' if hispanic == 0 [w=wr]
	eststo hisp: quietly estpost summarize `sum_vars' if hispanic == 1 [w=wr]
	
	esttab nonhisp hisp using "${tables}/T2_summarystats_breakdown.tex", ///
		title("Summary statistics of eligible voters (by ethnicity)") ///
		cells("mean(fmt(3)) sd") ///
		mtitle("Non-hispanic" "Hispanic") ///
		label ///		
		addnotes ("Note: Sample contains U.S. citizens aged 18 or older" ///
		"Source: 2004-2014 Current Population (CPS) November Voting and Registration Supplements" "The sample used for the voting statistic is restricted to individuals who have registered to vote") ///
		compress ///
		booktab ///
		replace 
		
