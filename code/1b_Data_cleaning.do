/***********************************************************************
1b DATA CLEANING
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
	log using "${log}/1b_Data_cleaning_${dt}.txt", text replace

	/*

/***************************************************************************
                                 CPS DATA
****************************************************************************/

* ----- ASEC data ---------------------------------------------------------*
	use "${raw}/cps_asec_raw.dta", clear 
	
	*** TECHNICAL VARIABLES
	rename year survey_year
	
	*** GEOGRAPHICAL VARIABLES
	* Delete observations that do not have an identified county
	drop if county==0
	
	* Create variable with consistent 5-digit string FIPS code
	gen fips = string(county,"%05.0f") // Previously, some states missed front 0
	drop county
	
	* Recode missing and negative values (might be lost investments)
	replace ftotval = . if ftotval == 9999999999 | ftotval<0
	
	* Generate round weight
	gen wr = .
	replace wr = round(asecwt)
	
	
	// County-level
	preserve
		collapse (mean) county_inc= ftotval /// 
		[w=wr], by(fips survey_year)
		
		save "${temp}/county_income.dta", replace
	restore
	
	// State-level
	 collapse (mean) state_inc= ftotval /// 
		[w=wr], by(statefip survey_year)
		
	save "${temp}/state_income.dta", replace



*------ Common data cleaning for all CPS supplements -----------------------*

local supplements voters civic volunteer

foreach s of local supplements {
	
	use "${raw}/cps_`s'_raw.dta", clear 
	
	*** TECHNICAL VARIABLES
	rename year survey_year
	rename month survey_month
	
	*** GEOGRAPHICAL VARIABLES
	* Delete observations that do not have an identified county
	drop if county==0
	
	* Create variable with consistent 5-digit string FIPS code
	gen fips = string(county,"%05.0f") // Previously, some states missed front 0
	drop county
		
	*** SOCIO-ECONOMIC AND DEMOGRAPHIC VARIABLES
	
	* Recode family income and dummy for low income individuals
	replace faminc = . if inrange(faminc,995,998)
	
	gen below50k = 0 //Low-income
	replace below50k = 1 if faminc < 800
	
	* Recode sex
	gen female= sex-1
	drop sex
	
	* Dummy for single
	gen single = 0
	replace single = 1 if marst==6
	replace single = . if marst==9
	
	* Dummy for education: high school or less, more than high school
	gen lowed = 0
	replace lowed = 1 if (educ <= 73)
	replace lowed = . if (educ==000|educ==001|educ==999)
	
	gen highed = 0
	replace highed = 1 if educ >73
	replace highed = . if (educ==000|educ==001|educ==999)
	
	
	// Race and ethnicity
	
	* Dummy for White
	gen white = 0
	replace white = 1 if race==100
	
	* Dummy for Black
	gen black = 0
	replace black = 1 if race==200
	
	* Dummy for Hispanic
	gen hispanic = 1
	replace hispanic = 0 if (hispan==000)
	
	* Dummy for non-Hispanic white
	gen nonhispwhite = 0
	replace nonhispwhite = 1 if (hispanic==0 & white==1)
	
	* Dummy for non-Hispanic black
	gen nonhispblack = 0
	replace nonhispblack = 1 if (hispanic==0 & black==1)
	
	* Dummy for Mexican
	gen mexican = 0
	replace mexican = 1 if hispan==100
	
	// Immigration status
	
	* Dummy for non-citizens
	gen noncitizen = 0 
	replace noncitizen = 1 if citizen==5
		
	* Dummy for naturalized citizens
	gen naturalized = 0
	replace naturalized = 1 if citizen==4
	
	* Dummy for immigrants
	gen immigrant = 0
	replace immigrant = 1 if (citizen==4 | citizen==5)
	
	* Dummy for natives
	gen native = 0
	replace native = 1 if inrange(citizen,1,3)
	
	* Years in the US: NOT REALLY TRUST WORTHY DUE TO CODING OF CPS
	gen yearsus = survey_year-yrimmig if native==0
	replace yearsus=. if yrimmig==0000
	replace yearsus= age if native==1
	
	

		
	* Mixed-status Household
	
	/* Defined as different citizenship status (see Bitler and Hoynes, 2011)
	 egen citizens_household = total(noncitizen==0), by(cpsid survey_year)
	 egen totindv_household = total(cpsid != .), by(cpsid survey_year)
	 egen mixedstatus = count(totindv_household > citizens_household), by(cpsid survey_year)*/
	
	// Defined as at least one likely undocumented migrant (see Amuedo-Dorantes and Lopez) --> this is a proxy (less reliable)
	
	* Dummy for likely unauthorized migrant
	gen likely_unauthorized = 0
	replace likely_unauthorized = 1 if (noncitizen==1 & hispanic==1 & lowed==1 & yearsus>5 )
	
	egen likely_mixedstatus = total(likely_unauthorized==1), by(cpsid survey_year)
	replace likely_mixedstatus = 1 if likely_mixedstatus > 1 // 1.69%
	
	*** CONTROLS:
	
	// Round weight and rename s.t. it is the same in every supplement
	gen wr = .
	
	if "`s'" == "voters" {
		replace wr = round(vosuppwt)
	}
	if "`s'" == "civic" {
		replace wr = round(cesuppwt)
	}
	if "`s'" == "volunteer"{
		replace wr = round(vlsuppwt)
	}

		
	* Dummy for presidential election
	gen presidential = 0
	replace presidential = 1 if (survey_year==2004|survey_year==2008|survey_year==2012|survey_year==2016)
	
	 
	// County-level controls: likely share of MS, median household income
	preserve
		collapse (mean) county_MS=likely_mixedstatus county_unauthorized = likely_unauthorized [w=wr], by(fips survey_year)
		
		tempfile countychars_cps
		save `countychars_cps'
	restore
	merge m:1 fips survey_year using `countychars_cps'
	drop _merge
	
	
	// State-level controls: likely share of MS, median household income
	preserve
		collapse (mean) state_MS=likely_mixedstatus  /// 
		[w=wr], by(statefip survey_year)
		
		tempfile statechars_cps
		save `statechars_cps'
	restore
	merge m:1 statefip survey_year using `statechars_cps'
	drop _merge
	

	// Reduce dataset
	compress	
	save "${temp}/cps_`s'_temp.dta", replace
}

*/

*------- Cleaning for CPS Voters and Registered ---------------------------*
	use "${temp}/cps_voters_temp.dta", clear 
		
	
	* Dummy for registering to vote:
	* The universe for VOREG compromises only those persons who did not vote !!
	* Not in universe: code 99 (I will later restrict to citizens 18+)
	* My dummy variable = individuals who registered (vote and no vote)
	
	gen registered = 0
	replace registered = 1 if voreg==02 | voreg == 99
	replace registered = . if inrange(voreg,96,98)
	drop voreg
	
	* Dummy for voting
	* The universe for VOTE_CAT (voted in CPS) is eligible voters
	rename voted vote_cat
	gen voted = 0
	replace voted = 0 if vote_cat==02
	replace voted = . if inrange(vote_cat,96,99)
	drop vote_cat
	
	
	save "${temp}/cps_voters.dta", replace



*------- Cleaning for CPS Civic Engagement --------------------------------*
	use "${temp}/cps_civic_temp.dta", clear 
	
	
	save "${temp}/cps_civic.dta", replace
	
*------- Cleaning for CPS Volunteers --------------------------------------*
	use "${temp}/cps_volunteer_temp.dta", clear 
	save "${temp}/cps_volunteer.dta", replace
	


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
