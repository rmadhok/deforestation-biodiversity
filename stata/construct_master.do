*********************************************************************************
*																			
* PROJECT: 	Deforestation and Biodiversity										
*																			
* PURPOSE: Construct Master Dataset		    
*																			
* START DATE: Dec 30, 2018       				 							
*																			
* AUTHOR:  Raahil Madhok, madhokr@mail.ubc.ca											
*																				
********************************************************************************
*===============================================================================
*SET ENVIRONMENT
*===============================================================================
// Settings
clear all
pause on
cap log close
set more off
set maxvar 10000
set matsize 10000

//Set Directory Paths
gl ROOT 	"/Users/rmadhok/Dropbox (Personal)/def_biodiv"
gl DATA 	"${ROOT}/data"
gl DO		"${ROOT}/scripts/stata"

// Modules
local biodiv_unbalanced		1
local district_forest		1
local district_campa		0

*===============================================================================
* BIODIVERSITY INDICATORS
*===============================================================================
if `biodiv_unbalanced' == 1 {
	
	*------ UNBALANCED DISTRICT-MONTH PANEL
	
	//1. Read
	import delimited using "${DATA}/csv/ebird_all.csv", clear

	* ----------
	* 597 districts with bird sightings
	*------------
	
	** Replace Missing
	foreach var of varlist si* duration* distance* {
		
		replace `var' = "" if `var' == "NA"
		destring `var', replace
		
	}
	
	** Format date
	gen year_month = ym(year(date(yearmonth, "20YM")), month(date(yearmonth, "20YM")))
	format year_month %tmCCYY-NN
	sort c_code_2011 year_month
			 
	tempfile biodiversity
	save "`biodiversity'"
	
	//2. Merge with Monthly Datasets
	
	** Monthly Spatial Coverage and Weather
	
	foreach file in "coverage_ym_grid_8km" "india_precipitation" "india_temperature" {
	
		** Read
		import delimited "${DATA}/csv/`file'", clear
		
		** Formate Date
		gen year_month = ym(year(date(yearmonth, "20YM")), month(date(yearmonth, "20YM")))
		format year_month %tmCCYY-NN
		drop yearmonth
		
		** Merge
		merge 1:1 c_code_2011 year_month ///
			using "`biodiversity'", keep (2 3) nogen
		
		save "`biodiversity'", replace
	}

	//3. Merge with District Data
	
	** a. Hotspots
	import delimited "${DATA}/csv/hotspots_dist_codes", clear
	drop if c_code_2011 == "NA"
	
	bys c_code_2011: gen n_hotspots = _N
	bys c_code_2011: keep if _n == 1
	keep c_code_2011 n_hotspots

	tempfile hotspots
	save "`hotspots'"
	
	** b. Spatial Coverage, census names
	import delimited "${DATA}/csv/coverage_dist_grid_8km", clear
	merge 1:1 c_code_2011 using "`hotspots'", keep(1 3) nogen
	merge 1:1 c_code_2011 using "${DATA}/dta/2011_india_dist", ///
		keepus(state district tot_pop tot_area) nogen
	merge 1:m c_code_2011 using "`biodiversity'", keep(3) nogen
	
	//4. Prep Variables
	
	** Log/IHS
	foreach var of varlist s_richness* sh_index* si_index* ///
		coverage temperature_mean precipitation_mean {
			
			gen `var'_ihs = asinh(`var')
	}
	
	** Label
	gen pop_density = tot_pop / tot_area
	la var tot_area "District Area (\(km^{2}\))"
	la var pop_density "Population Density (per \(km^{2}\))"
	la var coverage_all "Spatial Coverage"
	la var n_hotspots "Number of Birding Hotspots"
	la var duration_mean "Duration (min)"
	la var distance_mean "Distance (km)"
	la var n_birders "Number of Birders"
	la var n_trips "Number of Trips"
	la var s_richness_mean_ihs "IHS Species Richenss (per user)"
	la var s_richness_mean "Species Richness (per user)"
	la var s_richness_d "Species Richness (all users)"
	la var s_richness_d_ihs "IHS Species Richness (all users)"
	la var sh_index_mean "Shannon Index (per user)"
	la var sh_index_mean_ihs "IHS Shannon Index (per user) "
	la var sh_index_d "Shannon Index (all users)"
	la var sh_index_d_ihs "IHS Shannon Index (all users)"
	la var si_index_mean "Simpson Index (per user)"
	la var si_index_mean_ihs "IHS Simpson Index (per user)"
	la var si_index_d "Simpson Index (all users)"
	la var si_index_d_ihs "IHS Simpson Index (all users)"
	la var coverage "Spatial Coverage"
	la var coverage_ihs "Spatial Coverage"
	la var temperature_mean "Temperature (C)"
	la var temperature_mean_ihs "Temperature (C)"
	la var precipitation_mean "Precipitation (mm)"
	la var precipitation_mean_ihs "Precipitation (mm)"
	
	//5. Write
	order c_code_2011* state district year_month ///
		s_richness* sh* si* coverage* n_* 
	sort c_code_2011 year_month
	save "${DATA}/dta/ebird_dist_biodiv_unbalanced", replace
	
	* ------------ TRIP-LEVEL DATA
	
	* Read
	import delimited using "${DATA}/csv/ebird_triplevel.csv", clear 
	
	* Clean
	drop effortareaha n_birders
	ren (samplingeventidentifier durationminutes effortdistancekm) ///
		(trip_id duration distance)
	destring duration distance, replace force
	
	* Format Date
	gen year_month = ym(year(date(yearmonth, "20YM")), month(date(yearmonth, "20YM")))
	format year_month %tmCCYY-NN
	drop yearmonth
	
	* Save
	order trip_id c_code_2011 year_month protocoltype
	sort c_code_2011 year_month
	save "${DATA}/dta/ebird_triplevel", replace

}

*===============================================================================
* PREPARE FOREST CLEARANCE
*===============================================================================
if `district_forest' == 1 {
	
	//1. Organize project-level
	
	** Read 
	use "${DATA}/dta/fc_clean.dta", clear
	drop village*
	
	/*------------
	19,495 projects
	--------------*/
	
	** Construction Started
	*keep if regexm(prop_status, "approved") | ///
	*		regexm(prop_status, "pending") | ///
	*		regexm(prop_status, "in-principle") | ///
	*		regexm(prop_status, "recommended") 
	// 11,887 projects remaining
	
	keep if prop_status == "approved" // 2,362 projects
	
		
	** Format Date
	gen year = year(date_rec)
	gen month = month(date_rec)
	gen year_month = ym(year, month)
	format year_month %tmCCYY-NN
	keep if year > 2014 & !mi(year_month)
	
	//2. Project-District Level
	** Reshape
	reshape long district_ district_forest_ district_nonforest_, ///
		i(prop_no) j(dist_id)
	
	** Clean
	ren (district_ district_forest_ district_nonforest_) ///
		(district district_forest district_nonforest)
	drop if district == ""
	
	//3. Merge Census Codes

	* Replace names
	do "${DO}/dist_name_sync"
	
	** Tempfile
	tempfile fc_district
	save "`fc_district'"

	** Merge with Census
	use "${DATA}/dta/2011_india_dist", clear
	keep state district c_code_2011
	replace state = lower(state)
	replace district = lower(district)
	merge 1:m state district using "`fc_district'", keep(3) nogen
	
	//4. Aggregate to District
	
	** Sector Indicator
	tab proj_cat, gen(proj_cat_)
	tab proj_shape, gen(proj_shape_)
	
	** Land Diversion by Project Type
	local projlist `" "Electricity" "Forest Village Relocation" "Industry" "Irrigation" "Mining" "Other" "Transport" "'
	local i = 1
	
	foreach j in elec fvr ind irr mine o tran {
	
		* Forest Area
		gen forest_`j' = district_forest if proj_cat_`i' == 1
		gen nonforest_`j' = district_nonforest if proj_cat_`i' == 1
		bys c_code_2011 year_month: egen district_forest_`j' = total(forest_`j')
		bys c_code_2011 year_month: egen district_nonforest_`j' = total(nonforest_`j')
		drop forest_`j' nonforest_`j'
		
		* Label
		local type : word `i' of `projlist'
		la var district_forest_`j' "Deforestation (`type')" 
		la var district_nonforest_`j' "Non-forest Land Diversion (`type')" 
		
		local ++i
	
	}
		
	** Land Diversion by Project Shape
	local shapelist "Hybrid Linear Non-Linear"
	local i = 1
	
	foreach j in hyb lin nl {
	
		gen forest_`j' = district_forest if proj_shape_`i' == 1
		gen nonforest_`j' = district_nonforest if proj_shape_`i' == 1
		bys c_code_2011 year_month: egen district_forest_`j' = total(forest_`j')
		bys c_code_2011 year_month: egen district_nonforest_`j' = total(nonforest_`j')
		drop forest_`j' nonforest_`j'
		
		* Label
		local type : word `i' of `shapelist'
		la var district_forest_`j' "Deforestation (`type')" 
		la var district_nonforest_`j' "Non-forest Land Diversion (`type')" 
		
		local ++i
		
	}
	
	** Land Diversion in Protected Area
	gen forest_pa = district_forest if proj_in_pa_esz_num == 1
	gen nonforest_pa = district_nonforest if proj_in_pa_esz_num == 1
	bys c_code_2011 year_month: egen district_forest_pa = total(forest_pa)
	la var district_forest_pa "Deforestation (Protected Area)"
	bys c_code_2011 year_month: egen district_nonforest_pa = total(nonforest_pa)
	la var district_nonforest_pa "Non-forest Land Diversion (Protected Area)"
	drop forest_pa nonforest_pa

	** Save Labels
	foreach v of varlist district_forest_* district_nonforest_* {
		local lab_`v' : var lab `v'
	}

	** Aggregate
	collapse (sum)  district_forest district_nonforest ///
			 (first) district_forest_* district_nonforest_*, ///
			 by(c_code_2011 year_month)
	
	** Label
	foreach v of varlist district_forest_* district_nonforest_* {
		la var `v' "`lab_`v''"
	}
	la var district_forest "Deforestation"
	la var district_nonforest "Non-forest Land Diversion"
	
	//5. Balance Panel
	
	** Merge with Census
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", keepus(c_code_2011)

	** Balance Panel
	encode c_code_2011, gen(c_code_2011_num)
	tsset c_code_2011_num year_month
	sort year_month 
	gen ym = year_month[1]
	replace year_month = ym if _m == 2
	tsfill, full
	
	** Clean
	drop c_code_2011 _merge ym
	decode c_code_2011_num, gen(c_code_2011)
	
	** Zeros
	foreach var of varlist district_forest-district_nonforest_pa {
		replace `var' = 0 if `var' == .
	}
	
	//6. Generate Variables
	foreach var of varlist district_forest-district_nonforest_pa {
		
		local vlab : var lab `var'
	
		** Cumulative
		sort c_code_2011 year_month
		by c_code_2011: gen `var'_cum = sum(`var')
		
		** km2
		gen `var'_cum_km2 = `var'_cum / 100
		la var `var'_cum_km2 "Cum. `vlab'"
			
		** Inverse Hyperbolic Sine
		gen `var'_cum_ihs = asinh(`var'_cum_km2)
		la var `var'_cum_ihs "Cum. `vlab'"
		
	}

	** State/district strings
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", ///
		keepus(state district) nogen
	
	** Year, Month
	gen date = dofm(year_month)
	gen year = year(date)
	gen month = month(date)
	drop date
	
	** State Codes
	gen state_code_2011 = substr(c_code_2011, 1, 3)
	encode state_code_2011, gen(state_code_2011_num)
	
	* Lags
	foreach var of varlist *forest_cum_ihs {
	
		foreach i of numlist 1/12 {
		
			bys c_code_2011: gen `var'_l`i' = `var'[_n - `i']
			la var `var'_l`i' "`i' Month"
		
		}
	}
	
	//7. Save
	
	* Forest Clearance
	save "${DATA}/dta/fc_dist_ym", replace

	* Merge with district-level ebird
	merge 1:1 c_code_2011 year_month using "${DATA}/dta/ebird_dist_biodiv_unbalanced", keep(3) nogen
	order c_code_2011* state_code_2011* state district year_month district*
	save "${DATA}/dta/fc_ebd", replace
	
	* Merge with trip-level ebird
	use "${DATA}/dta/fc_dist_ym", clear
	merge 1:m c_code_2011 year_month using "${DATA}/dta/ebird_triplevel", keep(3) nogen
	sort c_code_2011 year_month
	order *_code_2011 state district trip_id year_month duration distance
	save "${DATA}/dta/fc_ebd_trip", replace
	
}

if `district_campa' == 1 {

	//1. Project-Level
	
	** Read
	use "${DATA}/dta/fc_clean.dta", clear
	
	drop village_* district_* displacement_num employment_num ///
		ca_nfl_rfl* ca_area_less_div_num date_submit ///
		proj_area_forest2-proj_category proj_cost ///
		proj_scheduledarea_num-perm_emp temp_emp ua_legal_status

	** Construction Started
	
	*keep if regexm(prop_status, "approved") | ///
	*		regexm(prop_status, "pending") | ///
	*		regexm(prop_status, "in-principle") | ///
	*		regexm(prop_status, "recommended") 
	// 11,887 projects remaining
	
	keep if prop_status == "approved" // 2,362 projects remaining
	
	** Format Date
	gen year = year(date_rec)
	gen month = month(date_rec)
	gen year_month = ym(year, month)
	format year_month %tmCCYY-NN
	keep if year > 2014 & !mi(year_month)
	
	//2. Campa District Project Level
	
	** Reshape
	reshape long ca_district_ ca_district_nfl_ , i(prop_no) j(dist_id)
	
	** Clean
	ren (ca_district_ ca_district_nfl_) (district ca_district_nfl)
	drop if district == ""
	foreach val of numlist 137 140 189 191 237 239 {	
		replace district = subinstr(district, "`=char(`val')'", "", .)
	}
	replace district = trim(itrim(lower(district)))
	
	//3. Merge Census Codes
	
	* Sync District Names
	do "${DO}/dist_name_sync"

	* Merge with Census
	tempfile temp
	save "`temp'"
	
	use "${DATA}/dta/2011_india_dist", clear
	keep state district c_code_2011
	replace state = lower(state)
	replace district = lower(district)
	merge 1:m state district using "`temp'", keep(3) nogen
		
	//4. Aggregate to District
	
	** Sector Indicator
	tab proj_cat, gen(proj_cat_)
	tab proj_shape, gen(proj_shape_)
	
	** Land Diversion by Project Type
	local projlist `" "Electricity" "Industry" "Irrigation" "Mining" "Other" "Transport" "'
	local i = 1
	
	foreach j in elec ind irr mine o tran {
		
		* Forest Area
		gen ca_nfl_`j' = ca_district_nfl if proj_cat_`i' == 1
		bys c_code_2011 year_month: egen ca_district_nfl_`j' = total(ca_nfl_`j')
		drop ca_nfl_`j'
		
		* Label
		local type : word `i' of `projlist'
		la var ca_district_nfl_`j' "Comp. Afforestation (`type')" 
		
		local ++i
	
	}
		
	** Land Diversion by Project Shape
	local shapelist "Hybrid Linear Non-Linear"
	local i = 1
	
	foreach j in hyb lin nl {
	
		gen ca_nfl_`j' = ca_district_nfl if proj_shape_`i' == 1
		bys c_code_2011 year_month: egen ca_district_nfl_`j' = total(ca_nfl_`j')
		drop ca_nfl_`j'
		
		* Label
		local type : word `i' of `shapelist'
		la var ca_district_nfl_`j' "Comp. Afforestation (`type')" 
		
		local ++i
		
	}
	
	** Land Diversion in Protected Area
	gen ca_nfl_pa = ca_district_nfl if proj_in_pa_esz_num == 1
	bys c_code_2011 year_month: egen ca_district_nfl_pa = total(ca_nfl_pa)
	la var ca_district_nfl_pa "Comp. Afforestation (Protected Area)"
	drop ca_nfl_pa
	
	** Save Labels
	foreach v of varlist ca_district_nfl* {
		local lab_`v' : var lab `v'
	}

	** Aggregate
	collapse (sum)  ca_district_nfl ///
			 (first) ca_district_nfl_*, ///
			 by(c_code_2011 year_month)
	
	** Label
	foreach v of varlist ca_district_nfl_* {
		la var `v' "`lab_`v''"
	}
	la var ca_district_nfl "Comp. Afforestation"
	
	//5. Balance Panel
	
	** Merge with Census
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", keepus(c_code_2011)

	** Balance Panel
	encode c_code_2011, gen(c_code_2011_num)
	tsset c_code_2011_num year_month
	sort year_month 
	gen ym = year_month[1]
	replace year_month = ym if _m == 2
	tsfill, full
	
	** Clean
	drop c_code_2011 _merge ym
	decode c_code_2011_num, gen(c_code_2011)
	
	** Zeros
	foreach var of varlist ca_district_* {
		replace `var' = 0 if `var' == .
	}
	
	//6. Cumulate
	foreach var of varlist ca_* {
		
		local vlab : var lab `var'
	
		** Cumulative
		sort c_code_2011 year_month
		by c_code_2011: gen `var'_cum = sum(`var')
		
		** km2
		gen `var'_cum_km2 = `var'_cum / 100
		la var `var'_cum_km2 "Cum. `vlab'"
			
		** Inverse Hyperbolic Sine
		gen `var'_cum_ihs = asinh(`var'_cum_km2)
		la var `var'_cum_ihs "Cum. `vlab'"
		
	}
	
	** State/district strings
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", ///
		keepus(state district) nogen
	
	** Year, Month
	gen date = dofm(year_month)
	gen year = year(date)
	gen month = month(date)
	drop date
	
	** State Codes
	gen state_code_2011 = substr(c_code_2011, 1, 3)
	encode state_code_2011, gen(state_code_2011_num)

	//7. Merge
	
	* Deforestation // CHECK THIS
	merge 1:1 c_code_2011 year_month using "${DATA}/dta/fc_dist_ym"
	sort c_code_2011 year_month
	
	foreach var of varlist ca_* {
		
		replace `var' = `var'[_n-1] if _m == 2
		
		}

	
	drop _merge
	
	* Lags
	foreach var of varlist *forest_cum_ihs {
	
		foreach i of numlist 1/12 {
		
			bys c_code_2011: gen `var'_l`i' = `var'[_n - `i']
			la var `var'_l`i' "`i' Month"
		
		}
	}

	* Nightlights
	*merge 1:1 c_code_2011 year_month using "${DATA}/dta/india_nightlights", keep(3) nogen
	
	* ebird
	merge 1:1 c_code_2011 year_month using "${DATA}/dta/ebird_dist_biodiv_unbalanced", keep(3) nogen
	
	order c_code_2011* state_code_2011* state district year_month district* ca_district*
	save "${DATA}/dta/fc_ebd", replace
	
}


