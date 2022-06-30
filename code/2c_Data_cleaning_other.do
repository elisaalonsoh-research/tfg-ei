/***********************************************************************
2c Data cleaning: other datasets
AUTHOR: Elisa Alonso Herrero 
DATE: 15/05/2022
DESCRIPTION: This script takes the CPS Supplements raw, and saves them as dta
				files. For the unbalanced, it
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
	log using "${log}/2c_Data_cleaning__other_${dt}.txt", text replace
	
	
***********************************
**** US Governors 1775-2020: https://www.openicpsr.org/openicpsr/project/102000/version/V3/view?path=/openicpsr/102000/fcr:versions/V3/united_states_governors_1775_2020.dta&type=file
***********************************
	use "${raw}/united_states_governors_1775_2020.dta"
	keep year state party

	gen statefip=.
	replace statefip=01 if state=="Alabama"
	replace statefip=02 if state=="Alaska"
	replace statefip=04 if state=="Arizona"
	replace statefip=05 if state=="Arkansas"
	replace statefip=06 if state=="California"
	replace statefip=08 if state=="Colorado"
	replace statefip=09 if state=="Connecticut"
	replace statefip=10 if state=="Delaware"
	replace statefip=11 if state=="District of Columbia"
	replace statefip=12 if state=="Florida"
	replace statefip=13 if state=="Georgia"
	replace statefip=15 if state=="Hawaii"
	replace statefip=16 if state=="Idaho"
	replace statefip=17 if state=="Illinois"
	replace statefip=18 if state=="Indiana"
	replace statefip=19 if state=="Iowa"
	replace statefip=20 if state=="Kansas"
	replace statefip=21 if state=="Kentucky"
	replace statefip=22 if state=="Louisiana"
	replace statefip=23 if state=="Maine"
	replace statefip=24 if state=="Maryland"
	replace statefip=25 if state=="Massachusetts"
	replace statefip=26 if state=="Michigan"
	replace statefip=27 if state=="Minnesota"
	replace statefip=28 if state=="Mississippi"
	replace statefip=29 if state=="Missouri"
	replace statefip=30 if state=="Montana"
	replace statefip=31 if state=="Nebraska"
	replace statefip=32 if state=="Nevada"
	replace statefip=33 if state=="New Hampshire"
	replace statefip=34 if state=="New Jersey"
	replace statefip=35 if state=="New Mexico"
	replace statefip=36 if state=="New York"
	replace statefip=37 if state=="North Carolina"
	replace statefip=38 if state=="North Dakota"
	replace statefip=39 if state=="Ohio"
	replace statefip=40 if state=="Oklahoma"
	replace statefip=41 if state=="Oregon"
	replace statefip=42 if state=="Pennsylvania"
	replace statefip=44 if state=="Rhode Island"
	replace statefip=45 if state=="South Carolina"
	replace statefip=46 if state=="South Dakota"
	replace statefip=47 if state=="Tennessee"
	replace statefip=48 if state=="Texas"
	replace statefip=49 if state=="Utah"
	replace statefip=50 if state=="Vermont"
	replace statefip=51 if state=="Virginia"
	replace statefip=53 if state=="Washington"
	replace statefip=54 if state=="West Virginia"
	replace statefip=55 if state=="Wisconsin"
	replace statefip=56 if state=="Wyoming"
	
	
	// Drop Puerto Rico
	drop if statefip==.
	
	// Dummy for Republican gov at the state-level
	gen republican = 0
	replace republican = 1 if party == "Republican"
	drop party
	
	// Restrict year
	drop if year>2016 | year <2004
	rename year survey_year
	
	// Eliminate duplicates
	duplicates drop statefip survey_year, force	
	
	save "${temp}/party_state.dta", replace

	/*

***********************************
**** PER CAP INCOME BY COUNTY: BEA
***********************************

	use "${raw}/percapincome_county_raw.dta", clear
	
	// Keep only per capital personal income
	drop if description == "Population (persons) 1/" | ///
			description == "Personal income (thousands of dollars)"
			
	// Drop unwanted variables
	drop geoname region tablename linecode industryclassification description unit
	keep geofips v44-v57
	
	// Reshape dataset to ong format
	foreach v of varlist v44-v57 {
		local x : variable label `v'
		rename `v' income_pc_state`x'
		}
	reshape long income_pc_state, i(geofips) j(year)
	
	* Transform to common format as in CPS
	destring(income_pc_state), replace ignore("(NA)")
	rename geofips fips
	drop if fips == "00000"
	rename year survey_year
	
	save "${temp}/percapincome_county.dta", replace 
