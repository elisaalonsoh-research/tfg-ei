/* This file creates creates 6 figures (2008-2013) showing the implementation
 of the SC program at county-level */

clear all

** Install dependencies
*ssc install spmap    // For map creation
*ssc install shp2dta  // For translating shapefile into stata

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
 	
	// Create already implemented
	gen implemented = 0

	// Maps 2008-2012
	forv y=2008/2012 {
		replace implemented = 1 if  activation_year <= `y'	
	
		spmap implemented using "${temp}/uscoord.dta", id(id) clm(u) ///
				fcolor(gs15 eltblue) ocolor(gs13 ltblue) osize(vthin) legenda(off) ///
				title("{bf:`y'}", size(11))
				
		graph export "${figs}/map_implementation_`y'.png", replace
	}
	
	// Map 2013 (all implemented)
	replace implemented = 1 if  activation_year <= 2013
	spmap implemented using "${temp}/uscoord.dta", id(id) clm(u) ///
				fcolor(eltblue) ocolor(ltblue) osize(vthin) legenda(off) ///
				title("{bf:2013}", size(BBB11)) 
	graph export "${figs}/map_implementation_2013.png", replace
	
