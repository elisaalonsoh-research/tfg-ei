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
	log using "${log}/1c_Final_dataset_${dt}.txt", text replace


	
**********************************************
**** MERGE STATE-LEVEL CONTROLS
**********************************************
	use "${temp}/party_state.dta", clear
	merge 1:1 statefip survey_year using "${raw}/cspp_data_raw.dta"
	drop _merge
	merge 1:1 statefip survey_year using "${temp}/state_income.dta"
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
	use "${data}/raw/activation_dates.dta"
	
	gen statefip=.
	replace statefip=01 if ST=="AL"
	replace statefip=02 if ST=="AK"
	replace statefip=04 if ST=="AZ"
	replace statefip=05 if ST=="AR"
	replace statefip=06 if ST=="CA"
	replace statefip=08 if ST=="CO"
	replace statefip=09 if ST=="CT"
	replace statefip=10 if ST=="DE"
	replace statefip=11 if ST=="DC"
	replace statefip=12 if ST=="FL"
	replace statefip=13 if ST=="GA"
	replace statefip=15 if ST=="HI"
	replace statefip=16 if ST=="ID"
	replace statefip=17 if ST=="IL"
	replace statefip=18 if ST=="IN"
	replace statefip=19 if ST=="IA"
	replace statefip=20 if ST=="KS"
	replace statefip=21 if ST=="KY"
	replace statefip=22 if ST=="LA"
	replace statefip=23 if ST=="ME"
	replace statefip=24 if ST=="MD"
	replace statefip=25 if ST=="MA"
	replace statefip=26 if ST=="MI"
	replace statefip=27 if ST=="MN"
	replace statefip=28 if ST=="MS"
	replace statefip=29 if ST=="MO"
	replace statefip=30 if ST=="MT"
	replace statefip=31 if ST=="NE"
	replace statefip=32 if ST=="NV"
	replace statefip=33 if ST=="NH"
	replace statefip=34 if ST=="NJ"
	replace statefip=35 if ST=="NM"
	replace statefip=36 if ST=="NY"
	replace statefip=37 if ST=="NC"
	replace statefip=38 if ST=="ND"
	replace statefip=39 if ST=="OH"
	replace statefip=40 if ST=="OK"
	replace statefip=41 if ST=="OR"
	replace statefip=42 if ST=="PA"
	replace statefip=44 if ST=="RI"
	replace statefip=45 if ST=="SC"
	replace statefip=46 if ST=="SD"
	replace statefip=47 if ST=="TN"
	replace statefip=48 if ST=="TX"
	replace statefip=49 if ST=="UT"
	replace statefip=50 if ST=="VT"
	replace statefip=51 if ST=="VA"
	replace statefip=53 if ST=="WA"
	replace statefip=54 if ST=="WV"
	replace statefip=55 if ST=="WI"
	replace statefip=56 if ST=="WY"

	drop if statefip ==.
	rename ST statecode
	
	*** MERGE CPS AND ACTIVATION DATES
	
	// Is the statefip necessary?
	merge 1:m fips statefip using `cps_`s''
	keep if _merge==3
	drop _merge
	
	* Create dates (activation and survey)
	gen activation_date = ym(activation_year, activation_month)
	gen survey_date = ym(survey_year, survey_month)
	
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
		survey_year survey_month post_activation serial cpsid cpsidp
	
	// Geographical variables
	destring fips, replace
	
	*** LABELLING:
	* Particular to one dataset
	cap label variable registered "Registered"
	cap label variable voted "Voted"
	
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