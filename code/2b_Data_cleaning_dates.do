/***********************************************************************
2b DATA CLEANING DATES
AUTHOR: Elisa Alonso Herrero 
DATE: 30/06/2022
DESCRIPTION: This script takes raw activation dates, puts common names to the
				variables and merges it with the map database
OUTPUT: "${temp}/activation_dates_clean.dta"
	
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
	log using "${log}/2b_Data_cleaning_dates_${dt}.txt", text replace
	
	* Load activation dates
	use "${raw}/activation_dates.dta", clear
	
	// Create activation date:
	gen activation_date = ym(activation_year, activation_month)
	
	// Common format for statefip
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
	
	
	save "${temp}/activation_dates_clean.dta", replace
	
* ------------- MAP CREATION ---------------------------------------------*
	// Translate map shapefiles into stata format
	shp2dta using "${raw}/shapefiles/cb_2013_us_county_20m.shp", replace ///
			database("${temp}/usdb.dta") coordinates("${temp}/uscoord.dta") genid(id)
	
	// Rename county fips
	
	use "${temp}/usdb.dta", clear	
	rename GEOID fips
	keep fips id
	
 	// Merge with activation dates
	merge 1:1 fips using "${temp}/activation_dates_clean.dta"
	drop if _merge==1
	drop _merge
	
	save "${temp}/activation_dates_clean.dta", replace

	
	
