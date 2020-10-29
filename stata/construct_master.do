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

// Module
local ebird					0
local district_forest		1
local slx					1
local growth				0

*===============================================================================
* BIODIVERSITY
*===============================================================================
/*---------------------------------------
For dataset restricted to trips with 
all species reported use:
file: ebird_user_allreported.csv
coverage: coverage_ym_grid_5km_allreported
No other changes.
-----------------------------------------*/

if `ebird' == 1 {
	
	//1. Clean
	
	* Read
	import delimited using "${DATA}/csv/ebird_user.csv", clear
	keep if year < 2019
	* Clean
	destring distance si_index group_size, replace force
	
	* Format Date
	g year_month = ym(year, month(date(yearmonth, "20YM")))
	format year_month %tmCCYY-NN
	drop yearmonth
	
	tempfile ebd_user
	save "`ebd_user'"
	
	//2. Merge temporal data
	
	foreach file in "coverage_ym_grid_5km" "india_rainfall_gpm" "india_temperature" {
	
		import delimited "${DATA}/csv/`file'", clear
		g year_month = ym(year(date(yearmonth, "20YM")), month(date(yearmonth, "20YM")))
		format year_month %tmCCYY-NN
		drop yearmonth
		
		* Merge
		merge 1:m c_code_2011 year_month using "`ebd_user'", keep (2 3) nogen
		
		save "`ebd_user'", replace
	}
	
	//3. Merge with District Data
	
	* Coverage
	import delimited "${DATA}/csv/coverage_dist_grid_5km", clear
	
	* Census Vars
	merge 1:1 c_code_2011 using "${DATA}/dta/2011_india_dist", ///
		keepus(state district tot_pop tot_area) nogen
	
	* BirdLife
	merge 1:1 c_code_2011 using "${DATA}/dta/bl_district", nogen

	/*
	import delimited using "${DATA}/csv/bl_district.csv", clear
	ren c_code_11 c_code_2011
	gen sr_bl = 1
	collapse (count) sr_bl, by(c_code_2011)
	save "${DATA}/dta/bl_district", replace
	*/
	
	merge 1:m c_code_2011 using "`ebd_user'", keep(3) nogen
	
	//4. Merge with Annual Survey of Industries
	/*
	* Merge with ASI
	gen state_cd = substr(c_code_2011, 2, 2)
	destring state_cd, replace
	merge m:1 state_cd year using "${DATA}/dta/asi_wage_st_panel", keep(1 3) nogen
	gen wage_hr = wage_rate_d / 8	// Assume 8-hr workday
	gen wage_min = wage_hr / 60
	*/
	
	//5. Clean Variables
		
	replace coverage = coverage * 100
	replace coverage_all = coverage_all * 100
	
	* Generate vars
	g state_code_2011 = substr(c_code_2011, 1, 3)
	bys observerid: egen n_st_user = nvals(state_code_2011)
	bys observerid: egen n_dist_user = nvals(c_code_2011)
	bys observerid: egen n_ym_user = nvals(year_month)
	bys observerid: egen n_trips_user = total(n_trips)
	bys c_code_2011: egen n_users_dist = nvals(observerid)
	bys c_code_2011: egen n_trips_dist = total(n_trips)
	g pop_density = tot_pop / tot_area
	ren (temperature_mean rainfall_mm) (temp rain)
	*gen time_cost = wage_min * duration
	encode observerid, gen(user_id)
	drop state_code_2011
	
	* Label
	la var tot_area "District Area (\(km^{2}\))"
	la var pop_density "Population Density (per \(km^{2}\))"
	la var n_ym_user "Num. Year-months"
	la var n_dist_user "Num. Districts"
	la var n_st_user "Num. States"
	la var n_users_dist "Num. Users"
	la var n_trips_dist "Num. Trips"
	la var n_trips "Num. Trips"
	la var n_trips_user "Num. Trips"
	la var sr "Species Richness"
	la var sr_udym "Species richness across all trips in district-month"
	la var sr_uym "Species richness across all trips in year-month"
	la var sr_uyr "Species richness across all trips in year"
	la var sr_ym "Species richness across all users in year-month"
	la var sr_yr "Species richness across all users in year"
	la var group_size "Group Size"
	la var sh_index "Shannon Index"
	la var si_index "Simpson Index"
	la var coverage "Spatial Coverage (\%)"
	la var duration "Duration (min)"
	la var distance "Distance (km)"
	la var rain "Rainfall (mm)"
	la var temp "Temperature ($\degree$ C)"
	la var all_species "\% Full Reporting"
	la var n_mon_yr "Birding Rate (Months/Year)"
	*la var wage_min "Wage per Min."
	*la var time_cost "Time Cost [Duration $\times$ Wage]"
	
	* Save
	order user_id c_code_2011 year_month sr* *_index
	sort user_id c_code_2011 year_month
	save "${DATA}/dta/ebird_user", replace

}

*===============================================================================
* FOREST CLEARANCE
*===============================================================================
/*--------------------------------------------------
Run this on 1) project sample w/o the 3 mega projects
2) winsorize project area at 99% 3) full sample.
On each run, save final panel with corresponding 
prefix
---------------------------------------------------*/
*---------------------------------------------------
* PROGRAM TO CONSTRUCT BALANCED DEFORESTATION PANEL

* arguments: stage
*	Approval stage at which to include projects
* 	Select 1 or 2
*---------------------------------------------------
capture program drop construct_deforest
program define construct_deforest

	syntax, stage(integer)
	
	//1. Select Projects
	
	* Read 
	use "${DATA}/dta/fc_clean", clear // 19,495 total projects
	drop prop_name proj_category ca_*

	* Select Stage
	if `stage' == 2 {
		keep if prop_status == "approved" // 2,773
	}
	
	if `stage' == 1 {
		keep if prop_status == "approved by rohq" | regexm(prop_status, "principle") == 1 | regexm(prop_status, "pending at ho") == 1 | prop_status == "pending at ro for stage-ii" // 3,858 (4068) projects
	}
	
	* Format Date
	g year_month = ym(year(date_rec), month(date_rec))
	format year_month %tmCCYY-NN
	drop if year_month == .

	* Alternative Distribution
	/*---------------------------------------
	* 2 mega project in TG - laying of canals, 
	  tunnels, power lines for lift irrigation
	* 1 coal PP - 4400MW and 2400MW stations
	-----------------------------------------*/
	
	* Drop 3 megaprojects
	drop if proj_area_forest2 > 1000 // drops 3 megaprojects
	
	* Truncate Distribution (save with trunc99 prefix)
	*winsor2 proj_area_forest2, cut(0 99) replace

	//2. Reshape Project-District Level
	
	* Re-categorize
	/*------------------------------------------------------------
	there are 42 ind. projects which cause miniscule def'n. 
	296 underground projects (pipelines, OFC) which cause almost no 
	def'n. SE's are massive in own category. I lump into "other"
	-------------------------------------------------------------*/
	replace proj_cat = "other" if inlist(proj_cat, "industry", "underground")
	
	* Reshape
	reshape long district_ dist_f_ dist_nf_, i(prop_no) j(dist_id)
	
	ren (district_ dist_f_ dist_nf_) (district dist_f dist_nf)
	drop if district == ""
	
	//3. Merge Census Codes
	
	* Sync Names
	do "${DO}/dist_name_sync"
	tempfile fc_dist
	save "`fc_dist'"

	* Merge Census Code
	use "${DATA}/dta/2011_india_dist", clear
	keep state district c_code_2011
	replace state = lower(state)
	replace district = lower(district)
	merge 1:m state district using "`fc_dist'", keep(3) nogen	
	/*---------------------------------------
	391 unmatched from master: no FC
	 12 unmatched from FC: complex dist split (3 unique districts)
	---------------------------------------*/
	
	g year = year(date_rec) 
	bys year: egen n_dist_clear_yr = nvals(c_code_2011) // Number of districts with projects
	bys year_month: egen n_dist_clear_ym = nvals(c_code_2011) 
	drop year

	* Sector Indicator
	tab proj_cat, gen(proj_cat_)
	
	* Land Diversion by Project Type
	local projlist `" "Electricity" "Irrigation" "Mining" "Other" "Resettlement" "Transportation" "'
	local i = 1
	
	foreach j in elec irr mine o res tran {
	
		* Forest Area
		g dist_f_`j' = dist_f if proj_cat_`i' == 1
		g dist_nf_`j' = dist_nf if proj_cat_`i' == 1
		
		* Label
		local type : word `i' of `projlist'
		la var dist_f_`j' "`type'" 
		la var dist_nf_`j' "Non-forest Diversion (`type')" 
		local ++i
	}
	
	* Save Labels
	foreach v of varlist dist_f_* dist_nf_* {
		local lab_`v' : var lab `v'
	}
	
	* Aggregate
	g count = 1
	collapse (sum)  dist_f* dist_nf* n_patches=count ///
	         (first) n_dist_clear_*, ///
			 by(c_code_2011 year_month)
	
	* Label
	foreach v of varlist dist_f_* dist_nf_* {
		la var `v' "`lab_`v''"
	}
	la var dist_f "Deforestation"
	la var dist_nf "Non-forest Diversion"
	la var n_patches "Num. Patches"

	//5. Balance Panel
	
	* Add census codes for control group
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", keepus(c_code_2011)

	* Balance Panel
	encode c_code_2011, gen(c_code_2011_num)
	tsset c_code_2011_num year_month
	sort year_month 
	g ym = year_month[1]
	replace year_month = ym if _m == 2
	tsfill, full
	drop c_code_2011 _merge ym
	decode c_code_2011_num, gen(c_code_2011)

	//6. Generate Variables
	foreach var of varlist dist_f-n_patches {
		
		* No deforestation
		replace `var' = 0 if `var' == .
		local vlab : var lab `var'
	
		* Cumulative
		bys c_code_2011 (year_month): g `var'_cum = sum(`var')
		la var `var'_cum "`vlab'"
		
		* km2
		g `var'_cum_km2 = `var'_cum / 100
		la var `var'_cum_km2 "`vlab'"
		g `var'_km2 = `var'/100
			
		* Inverse Hyperbolic Sine
		g `var'_cum_ihs = asinh(`var'_cum_km2)
		la var `var'_cum_ihs "`vlab'"
		
	}
	drop n_patches_cum_km2 n_patches_km2 n_patches_cum_ihs

	* Front/Backfill
	g year = year(dofm(year_month))
	bys year: egen temp1 = min(n_dist_clear_yr)
	bys year_month: egen temp2 = min(n_dist_clear_ym)
	drop n_dist_clear_* year
	ren (temp1 temp2) (n_dist_clear_yr n_dist_clear_ym)
	
	* State/Dist Strings
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", ///
		keepus(state district) nogen
	g state_code_2011 = substr(c_code_2011, 1, 3)
	encode state_code_2011, gen(state_code_2011_num)
	
	* Dates
	g year = year(dofm(year_month))
	g month = month(dofm(year_month))
	keep if inrange(year, 2015,2018)
	
	* Add Forest Cover
	tempfile temp
	save "`temp'"
	import delimited "${DATA}/csv/forest_cover.csv", clear
	drop tree_cover_ihs tree_cover_log
	merge 1:m c_code_2011 year using "`temp'", keep(2 3) nogen
	
	ren tree_cover_mean tree_cover
	la var tree_cover "Tree Cover (\%)"
	
	* Finish
	order c_code_2011 year_month dist_f*
	sort c_code_2011 year_month

end

if `district_forest' == 1 {

	* Construct Stage I and II Panel
	foreach i of numlist 2 1 {
		
		construct_deforest, stage(`i')
		save "${DATA}/dta/fc_dist_ym_stage`i'", replace
		export delimited "${DATA}/csv/fc_dist_ym_stage`i'.csv", replace
	}
}

*===============================================================================
* SPATIAL LAGS
*===============================================================================
if `slx' == 1 {
	
	local dataset "" // either "" (main dataset), "full", or "trunc99"
	*--------------------------------
	* PREP DATA
	*--------------------------------
	
	if "`dataset'" == "" {
	
		* Prepare SLX Data
		foreach i of numlist 1 2 {
		
			* Read SLX
			import delimited "${DATA}/csv/slx_stage`i'.csv", clear
	
			* Clean
			ren year_month ym
			g year_month = ym(year(date(ym, "20YM")), month(date(ym, "20YM")))
			format year_month %tmCCYY-NN
			drop ym
		
			foreach v of varlist dist_f* {
				la var `v' "SLX Deforestation (Stage `i')"
			}
			
			if `i' == 1 {
				
				keep c_code_2011 year_month dist_f_km2_slx_bc dist_nf_km2_slx_bc
				ren (dist_f_km2_slx_bc dist_nf_km2_slx_bc) ///
					(dist_f_km2_slx_s1 dist_nf_km2_slx_s1)
			}
			
			tempfile stage`i'_slx
			save "`stage`i'_slx'"
		}
		
		* Prepare Stage I Deforestation
		use "${DATA}/dta/fc_dist_ym_stage1", clear
		keep c_code_2011 year_month dist_f_km2 dist_nf_km2
		ren (dist_f_km2 dist_nf_km2) (dist_f_km2_s1 dist_nf_km2_s1)
		la var dist_f_km2_s1 "Deforestation (Stage 1)"
		tempfile stage1
		save "`stage1'"
		
		*----------------------------
		* CONSTRUCT FINAL DATASET
		*----------------------------
		
		* Read Stage II Deforestation (main, full, or truncated)
		use "${DATA}/dta/fc_dist_ym_stage2", clear
		
		* Merge Stage I Deforestation
		merge 1:1 c_code_2011 year_month using "`stage1'", nogen // 2 variables
		
		* Merge Stage II SLX
		merge 1:1 c_code_2011 year_month using "`stage2_slx'", nogen
		
		* Merge Stage I SLX
		merge 1:1 c_code_2011 year_month using "`stage1_slx'", nogen
	
		* Merge to ebird
		/* For sample with trips where all species 
		reported merge to ebird_user_allreported.dta */
		merge 1:m c_code_2011 year_month using "${DATA}/dta/ebird_user", keep(3) nogen
		
		* User Time Trend
		bys user_id (year_month): g u_lin = sum(year_month != year_month[_n-1]) // Linear 
		la var u_lin "Linear Trend"
		g u_cubic = u_lin^3
		la var u_cubic "Cubic Trend"
		
		* Save
		sort user_id year_month
		order user_id *_code_2011 year_month state district sr *_index
		save "${DATA}/dta/fc_ebd_user", replace
	}
	
	else {
	
		* Read
		use "${DATA}/dta/fc_dist_ym_stage2_`dataset'", clear
		
		* Merge to Ebird
		merge 1:m c_code_2011 year_month using "${DATA}/dta/ebird_user", keep(3) nogen
		
		* User Time Trend
		bys user_id (year_month): g u_lin = sum(year_month != year_month[_n-1]) // Linear 
		la var u_lin "Linear Trend"
		g u_cubic = u_lin^3
		la var u_cubic "Cubic Trend"
		
		* Save
		sort user_id year_month
		order user_id *_code_2011 year_month state district sr *_index
		save "${DATA}/dta/fc_ebd_user_`dataset'", replace
	}

}

*===============================================================================
* ECON BENEFITS
*===============================================================================

if `growth' == 1 {

	* Read nightlights
	import delimited using "${DATA}/csv/india_nightlights.csv", clear
	
	* Dates
	gen year_month = ym(year(date(yearmonth,"20YM")), month(date(yearmonth, "20YM")))
	format year_month %tmCCYY-NN
	
	* Merge with deforestation
	merge 1:1 c_code_2011 year_month using "${DATA}/dta/fc_dist_ym_main", ///
		keep(2 3) nogen
		
	tempfile temp
	save "`temp'"
	use "${DATA}/dta/fc_dist_ym_robust", clear
	keep c_code_2011 year_month dist_f_cum_km2
	ren dist_f_cum_km2 dist_f_cum_km2_s1
	merge 1:1 c_code_2011 year_month using "`temp'", nogen
		
	* Weather
	tempfile nl
	save "`nl'"
	
	foreach file in "india_rainfall_gpm" "india_temperature" {
		
		import delimited "${DATA}/csv/`file'", clear
		gen year_month = ym(year(date(yearmonth, "20YM")), month(date(yearmonth, "20YM")))
		format year_month %tmCCYY-NN
		drop yearmonth
		
		* Merge
		merge 1:1 c_code_2011 year_month using "`nl'", keep (2 3) nogen
		save "`nl'", replace
	}
	la var temperature_mean "Temperature"
	la var rainfall_mm "Rainfall"
	
	* Save
	order c_code_2011 year_month year month state district dist_f_cum* mean_lights
	save "${DATA}/dta/def_lights_dist", replace
	

}
