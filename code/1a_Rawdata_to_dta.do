/***********************************************************************
1a. TRANSFORM RAW DATA INTO DTA FORMAT
AUTHOR: Elisa Alonso Herrero 
DATE: 15/05/2022
DESCRIPTION: This script takes the CPS Supplements raw, and saves them as dta
				files. 
OUTPUT: 
	
***********************************************************************/

clear
set more off
cap log close

// Set Globals: commented-out because they are already specified in 0_Master.do

/* Set up directory path and stata version
	if c(os)=="Windows" {
		version 16 // If I need to use MP --> 14
		global db "C:/Users/elisa/Documents/5. Research/TFG EI"
		global data "C:/Users/elisa/Documents/5. Research/TFG EI/data"
	}
	
	if c(os) == "Unix" {
		version 17
		global db "/home/users/ealonsoh.AD3/TFG EI"
	}

****************************************************************

* Filepaths:
global do 			"${db}/code"
global log			"${db}/log"
global temp 		"${data}/temp"


* Date string to add to log files
  global dt= subinstr("${S_DATE}", " ","",.)
*/


* Log file:
	log using "${log}/1a_Rawdata_to_dta_${dt}.txt", text replace


***********************************
**** CPS DATA
***********************************
// CPS supplements
local supplement voters civic volunteer

foreach s of local supplement {
	insheet using "${raw}/cps_`s'.csv", delim(",") clear 
	compress
	save "${raw}/cps_`s'_raw.dta", replace  // Save CPS rawdata in dta
	}
	


// CPS Annual Economic and Social supplement
insheet using "${raw}/cps_asec.csv", delim(",") clear 
compress
save "${raw}/cps_asec_raw.dta", replace  // Save CPS rawdata in dta


/*
***********************************
**** PER CAP INCOME BY COUNTY: BEA
***********************************

	insheet using "${data}/raw/CAINC1__ALL_AREAS_1969_2020.csv", delim(",") clear 
	
	compress
	save "${raw}/percapincome_county_raw.dta", replace 
*/



***********************************
**** CSPP DATA: The Correlates of State Policy
***********************************

	insheet using "${raw}/cspp_data_2022-05-24.csv", delim(",") clear 
	
	* Keep only relevant variables
	
	// The following variables are not available for the whole period: 
	// poptotal, pctlatinx, foreign_born, immig_laws_restrict, hincomemed
	keep year state_fips poptotal pctlatinx foreign_born ///
			immig_laws_restrict ///
			vep 
	rename year survey_year 
	rename state_fips statefip
	
	* Convert to numeric variables and 
	destring *, replace ignore("NA") 

	compress
	save "${raw}/cspp_data_raw.dta", replace  
	