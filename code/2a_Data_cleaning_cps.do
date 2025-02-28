/***********************************************************************
1b DATA CLEANING
AUTHOR: Elisa Alonso Herrero 
DATE: 15/05/2022
DESCRIPTION: This script cleans the CPS dataset to create our dependent 
				variables, individual and county-level controls, and
				standardize names.
OUTPUT: "${temp}/cps_`s'.dta
	
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
	log using "${log}/2a_Data_cleaning_cps_${dt}.txt", text replace

	

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
	
	/*
	// State-level
	 collapse (mean) state_inc= ftotval /// 
		[w=wr], by(statefip survey_year)
		
	save "${temp}/state_income.dta", replace
	*/


*------ Common data cleaning for all CPS supplements -----------------------*

local supplements voters civic volunteer

foreach s of local supplements {
	
	use "${raw}/cps_`s'_raw.dta", clear 
	
	*** TECHNICAL VARIABLES
	rename year survey_year
	rename month survey_month
	
	
	gen survey_date = ym(survey_year, survey_month)

	
	*** GEOGRAPHICAL VARIABLES
	* Delete observations that do not have an identified county
	drop if county==0
	
	* Create variable with consistent 5-digit string FIPS code
	gen fips = string(county,"%05.0f") // Previously, some states missed front 0
	drop county
		
	*** SOCIO-ECONOMIC AND DEMOGRAPHIC VARIABLES
	
	* Recode family income and dummy for low income individuals
	replace faminc = . if inrange(faminc,995,998)
	
	gen below50k = 0 
	replace below50k = 1 if faminc < 800 // Low income
	
	* Recode sex
	gen female= sex-1
	drop sex
	
	* Dummy for married
	gen married = 0
	replace married = 1 if marst==1 | marst==2
	replace married = . if marst==9
	
	* Dummy for education: high school or less, more than high school
	gen lowed = 0
	replace lowed = 1 if (educ <= 073)
	replace lowed = . if (educ==001|educ==999)
	
	gen highed = 0
	replace highed = 1 if educ > 073
	replace highed = . if (educ==001|educ==999)
	
	
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
	
	* Citizens a la brown
	gen citizen_brown = 0
	replace citizen_brown = 1 if native == 1 | (naturalized==1& yearsus>10)
			
	* Mixed-status Household
	
	// Defined as different citizenship status (see Bitler and Hoynes, 2011)
	 egen citizens_household = total(noncitizen==0), by(cpsid survey_year)
	 egen noncitizens_household = total(noncitizen==1), by(cpsid survey_year)
	 egen totindv_household = total(cpsid != .), by(cpsid survey_year)
	 gen mixedstatus = 0
	 replace mixedstatus = 1 if (totindv_household > citizens_household)
	 replace mixedstatus = 0 if (totindv_household == noncitizens_household)
	
	
	// Defined as at least one likely undocumented migrant (see Amuedo-Dorantes and Lopez) --> this is a proxy (less reliable)
	
	* Dummy for likely unauthorized migrant
	gen likely_unauthorized = 0
	replace likely_unauthorized = 1 if (noncitizen==1 & hispanic==1 & lowed==1 & yearsus>5 )
	
	egen likely_mixedstatus = total(likely_unauthorized==1), by(cpsid survey_year)
	replace likely_mixedstatus = 1 if likely_mixedstatus > 1 
	
	
	
	*** COUNTY AND STATE CONTROLS:
	
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
	
	 
	// County-level controls: mixedstatus (real and likely) 
	preserve
		collapse (mean) county_LMS=likely_mixedstatus county_unauthorized = likely_unauthorized county_MS=mixedstatus county_hisp=hispanic [w=wr], by(fips survey_year)
		
		tempfile countychars_cps
		save `countychars_cps'
	restore
	merge m:1 fips survey_year using `countychars_cps'
	drop _merge
	
	// State-level controls: hispanic (to compare with real data)
		preserve
		collapse (mean) state_hisp=hispanic [w=wr], by(statefip survey_year)
		
		tempfile statechars_cps
		save `statechars_cps'
	restore
	merge m:1 statefip survey_year using `statechars_cps'
	drop _merge
	
	
	// Reduce dataset
	compress	
	save "${temp}/cps_`s'_temp.dta", replace
	

}



*------- Cleaning for CPS Voters and Registered ---------------------------*
	use "${temp}/cps_voters_temp.dta", clear 
		
	* Dummy for voting
	* The universe for VOTE_CAT (voted in CPS) is eligible voters (+18, citizens)
	rename voted vote_cat
	gen voted = 0
	replace voted = 1 if vote_cat==02
	replace voted = . if inrange(vote_cat,96,99)
	drop vote_cat
	
	* Dummy for registering to vote:
	* The universe for VOREG compromises only those persons who did not vote !!
	* Not in universe: code 99 (I will later restrict to citizens 18+)
	* My dummy variable = individuals who registered (vote and no vote)
	
	
	gen registered = 0
	replace registered = . if inrange(voreg,96,99)
	replace registered = 1 if voreg==02 | voted==1	
	drop voreg
	
	* Dummy for voted if registered:
	gen voted_reg = .
	replace voted_reg = 0 if registered==1 & voted == 0
	replace voted_reg = 1 if registered==1 & voted ==1
	
		
	
	save "${temp}/cps_voters.dta", replace



*------- Cleaning for CPS Civic Engagement --------------------------------*
	use "${temp}/cps_civic_temp.dta", clear 
	
	
	save "${temp}/cps_civic.dta", replace
	
*------- Cleaning for CPS Volunteers --------------------------------------*
	use "${temp}/cps_volunteer_temp.dta", clear 
	save "${temp}/cps_volunteer.dta", replace
	

