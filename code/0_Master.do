clear all
macro drop _all
set more off, perm
cap log close

****************************************************************
*To run this dofile, change the file paths in these "globals":

	* New PC
	*global dir "C:/Users/elisa/Documents/5. Research/TFG EI"
	
	* Old PC
	global dir "C:/Users/Windows/Documents/5. Research/TFG EI"


****************************************************************
* Dependencies
*ssc install shp2dta
*ssc install spmap
*ssc inst unique

****************************************************************

* Folder paths:
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
  
*-------- DATASET CONSTRUCTION ------------------------------------------*

	*do "${do}/1_Rawdata_to_dta.do" // Done
  
	do "${do}/2a_Data_cleaning_cps.do" // 
	*do "${do}/2b_Data_cleaning_dates.do" //
	*do "${do}/2c_Data_cleaning_other.do" //
   
	global controls_indv mixedstatus likely_mixed naturalized mexican age female married below50k lowed 
    global controls_county county_MS county_LMS county_inc county_hisp
    global controls_state republican presidential
	
	      
	do "${do}/3_Final_dataset.do" //Done

*-------- FIGURES -------------------------------------------------------*
	// County-level timing of implementation map
	*do "${do}/map_implementation.do"
	
*-------- TABLES  -------------------------------------------------------*

	
** TABLE 1: Summary statistics

	*do "${do}/T1_summary_stats.do"
	*do "${do}/T2_summary_stats_breakdown.do"


** TABLE 2: Triple differences for voting and registration

	*do "${do}/T2_Triple_differences_voters.do"

