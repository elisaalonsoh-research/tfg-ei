/***********************************************************************
1c FINAL DATASET
AUTHOR: Elisa Alonso Herrero 
DATE: 15/05/2022
DESCRIPTION: This script takes merges each of the CPS Supplements data
				with the activation dates per county, and the institutinal
				data for our control variables
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
	log using "${log}/3_Final_dataset_${dt}.txt", text replace


	
**********************************************
**** MERGE STATE-LEVEL CONTROLS
**********************************************
	
	use "${temp}/party_state.dta", clear
	merge 1:1 statefip survey_year using "${raw}/cspp_data_raw.dta"
	drop _merge
	
	save "${temp}/controls_state.dta", replace

	
**********************************************
**** MERGE CPS DATA W/ ACTIVATION DATES AND CONTROLS
**********************************************

local supplement voters civic volunteer

foreach s of local supplement {
	* Load CPS data:
	use "${temp}/cps_`s'.dta" , clear
	
	tempfile cps_`s'
	save `cps_`s''
	
	* Load activation dates
	use "${temp}/activation_dates_clean.dta"
	
		
	*** MERGE CPS AND ACTIVATION DATES
	
	// Is the statefip necessary?
	merge 1:m fips statefip using `cps_`s''
	keep if _merge==3
	drop _merge
	
		
	* Create Post Activation Dummy
	gen post_activation = 0
	replace post_activation = 1 if (survey_date > activation_date)
	replace post_activation = (survey_month - activation_month)/12 if (survey_year == activation_year & survey_date > activation_date)
	
	tempfile main_vars_`s'
	save `main_vars_`s''
	
	*** MERGE COUNTY LEVEL CONTROLS
	
	use "${temp}/county_income.dta", clear
	merge 1:m fips survey_year using `main_vars_`s''
	drop if _merge==1
	drop _merge
	
	tempfile main_vars_`s'2
	save `main_vars_`s'2'
	
	*** MERGE STATE LEVEL CONTROLS
	use "${temp}/controls_state.dta", clear
	merge 1:m statefip survey_year using `main_vars_`s'2'
	drop if _merge==1
	drop _merge
	
	save "${temp}/merged_data_`s'.dta", replace

	} 

	
**********************************************
**** CLEAN FINAL DATASET
**********************************************
local supplement voters civic volunteer

foreach s of local supplement {
	use "${temp}/merged_data_`s'.dta", clear
	
	order state statecode statefip county fips activation_year activation_month ///
		survey_year survey_month post_activation serial cpsid cpsidp ///
		${controls_indv} ${controls_county} ${controls_state}
		
	// Technical variables
	drop pernum serial // To identify households:CPSID, for persons: CPSIDP
	
	// Geographical variables
	destring fips, replace
	
	*** LABELLING:
	* Particular to one dataset
	cap label variable registered "Registered" // CPS voters
	cap label variable voted "Voted"           // CPS voters
	
	* Present in all
	label variable hispanic "Hispanic"
	label variable black "Black"
	label variable likely_mixedstatus "Mixed-Status Household"
	label variable naturalized "Naturalized"
	label variable mexican "Mexican"
	label variable age "Age"
	label variable female "Gender (female)"	
	label variable single "Single"
	label variable lowed "Completed High School"
	label variable highed "Completed more than High School"
	label variable below50k "Income below 50k"
	label variable county_MS "Share of Mixed-Status Households by County"
	label variable county_unauthorized "Share of Likely Unauthorized by County"
	label variable county_inc "Family Income by County"
	label variable republican "State had a Republican Governor"
	label variable presidential "Presidential Election Year"
	
	
	*** SAMPLE RESTRICTION: universe of possible voters (citizens and 18>=)
	drop if noncitizen==1
	drop if age<18
	
	save "${final}/final_`s'.dta", replace
	
	
	}