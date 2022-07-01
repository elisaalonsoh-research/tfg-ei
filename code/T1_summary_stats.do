/***********************************************************************
T1 SUMMARY STATS
AUTHOR: Elisa Alonso Herrero 
DATE: 15/05/2022
DESCRIPTION: This script recreates Table 1 from the paper, which tabs 
			descriptive statistics (weighted).
OUTPUT: 
	
***********************************************************************/


*** T1 SUMMARY STATISTICS OF ELIGIBLE VOTERS (Using voters CPS)
	use "${final}/final_voters_cvap.dta", clear
	
	
	// Summary (weighted)
	local sum_vars registered voted voted_reg hispanic $controls_indv ///
					$controls_county $controls_state
						
	estpost summarize `sum_vars' [w=wr] 
	eststo summstats
	
	
	
	// Create table
	esttab summstats using "${tables}/T1_summarystats.tex", ///
		cell("mean(fmt(3)) sd(fmt(3)) min(fmt(1)) max(fmt(1))") ///
		mtitle("All eligible voters") ///
		title("Summary statistics of eligible voters") ///
		nonumbers ///
		label ///
		addnotes ("Note: Sample contains U.S. citizens aged 18 or older" ///
	"Source: 2004-2014 Current Population (CPS) November Voting and Registration Supplements" "The sample used for the voting statistic is restricted to individuals who have registered to vote") ///
		compress ///
		booktab ///
		replace 
	

