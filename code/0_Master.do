clear all
macro drop _all
set more off, perm
cap log close

****************************************************************
*To run this dofile, change the file paths in these "globals":



** Set up directory path and stata version
	if c(os)=="Windows" {
		version 16 // If I need to use MP --> 14
		global dir "C:/Users/elisa/Documents/5. Research/TFG EI"
	}
	
	if c(os) == "Unix" {
		version 17
		global dir "/home/users/ealonsoh.AD3/TFG EI"
	}

****************************************************************

* Filepaths:
	global do 			"${dir}/code"
	global log			"${dir}/log"
	
	global data	        "${dir}/data"
	global raw          "${data}/raw"
	global temp 		"${data}/temp"
	global final		"${data}/final"


	global results	 	"${dir}/results"
	global figs	 	    "${results}/figures"
	global tables		"${results}/tables"
	
*/

* Date string to add to log files
	global dt= subinstr("${S_DATE}", " ","",.)

****************************************************************
*** Structure of dofiles:
****************************************************************
  
*-------- CONSTRUCTING DATASET ------------------------------------------*

  *do "${do}/1a_Rawdata_to_dta.do" // Done
  
  *do "${do}/1b_Data_cleaning.do" // Done
  
  *do "${do}/1c_Final_dataset.do" //Done


*-------- TABLES  -------------------------------------------------------*

	use "${final}/final_voters.dta"

	global controls_indv likely_mixed naturalized mexican age female single below50k lowed 
	global controls_county county_unauthorized county_MS county_inc
	global controls_state republican presidential

** TABLE 1: Summary statistics

	do "${do}/T1_summary_stats.do"


** TABLE 2: Triple differences for voting and registration


	

	*do "${do}/T2_Triple_differences_voters.do"

