*********************************************************************************
*																			
* PROJECT: 	Deforestation and Biodiversity										
*																			
* PURPOSE: Construct Master Dataset		    																	      				 							
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

// Set Directory Paths
gl ROOT 	"/Users/rmadhok/Dropbox/def_biodiv"
gl DATA 	"${ROOT}/data"
gl DO		"${ROOT}/scripts/stata"

// Module
local ebird					0
local district_forest		0
local merge					1
*===============================================================================
* BIODIVERSITY
*===============================================================================

if `ebird' == 1 {
	
	local level "udt" // udt = user-district-yearmonth, uct = user-cell-yearmonth
	
	**# Read
	import delimited using "${DATA}/csv/ebird_`level'.csv", clear
	
	* Format Date
	g year_month = ym(year, month(date(yearmonth, "20YM")))
	format year_month %tmCCYY-NN
	drop yearmonth
	
	tempfile ebd_user
	save "`ebd_user'"
	
	**# Merge to external data
	
	* Weather
	foreach file in "india_rainfall_gpm" "india_temperature_era" {
	
		import delimited "${DATA}/csv/`file'", varn(1) clear
		g year_month = ym(year(date(yearmonth, "20YM")), month(date(yearmonth, "20YM")))
		format year_month %tmCCYY-NN
		drop yearmonth
		
		* Merge
		merge 1:m c_code_2011 year_month using "`ebd_user'", keep (2 3) nogen
		save "`ebd_user'", replace
	}
	
	* Census Vars
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", ///
		keepus(state district tot_pop tot_area) keep(1 3) nogen
	save "`ebd_user'", replace
	
	* BirdLife
	import excel "${DATA}/csv/bl_district.xlsx", first clear
	ren c_code_11 c_code_2011
	drop if c_code_2011 == ""
	keep if PRESENC == 1 | PRESENC == 3
	bys c_code_2011: egen sr_bl = nvals(SISID)
	collapse (first) sr_bl, by(c_code_2011)
	merge 1:m c_code_2011 using "`ebd_user'", keep(2 3) nogen // all match
	save "`ebd_user'", replace
	
	* Biome Clusters
	import delimited "${DATA}/csv/biome_cluster", clear
	merge 1:m c_code_2011 using "`ebd_user'", keep(2 3) nogen
	replace biome = 888 if biome == . // note: only 1 district w missing biome (Diu)
	
	**# Clean Variables
	
	g state_code_2011 = substr(c_code_2011, 1, 3)
	bys user_id: egen n_st_user = nvals(state_code_2011) // states per user
	bys user_id: egen n_dist_user = nvals(c_code_2011) // districts per user
	bys user_id: egen n_ym_user = nvals(year_month) // time periods per user
	bys user_id: egen n_trips_user = total(n_trips) // total trips per user
	bys c_code_2011: egen n_users_dist = nvals(user_id)
	bys c_code_2011: egen n_trips_dist = total(n_trips)
	bys c_code_2011 year_month: egen n_users_dym = nvals(user_id)
	
	* Transform
	g pop_density = tot_pop / tot_area
	g ln_exp_idx = ln(exp_idx)
	g ln_duration = ln(duration)
	g ln_distance = asinh(distance)
	g ln_traveling = asinh(traveling)
	g ln_group_size = ln(group_size)
	
	* Coverage fraction
	if "`level'" == "udt" {
		
		foreach v of varlist coverage* {
			replace `v' = (`v' / n_cells_dist)*100 // fraction of district covered
			replace `v' = 100 if `v' > 100 // possible bc num cells/district weighted by overlap fraction
			g ln_`v' = ln(`v')
		}
		la var coverage_d "Coverage (\%)"
		la var coverage_dym "Coverage (\%)"
		la var coverage_udym "Coverage (\%)"
		la var ln_coverage_dym "Coverage (\%)"
		la var ln_coverage_udym "Coverage (\%)"
	}
	
	* Clean
	destring si_index*, replace force
	ren (temp_era rain_gpm) (temp rain)
	encode user_id, gen(uid)
	drop state_code_2011
	la var tot_area "District Area (\(km^{2}\))"
	la var pop_density "Population Density (per \(km^{2}\))"
	la var n_ym_user "Num. Year-months"
	la var n_dist_user "Num. Districts"
	la var n_st_user "Num. States"
	la var n_users_dist "Num. Users"
	la var n_trips_dist "Num. Trips"
	la var n_trips "Num. Trips"
	la var n_trips_user "Num. Trips"
	la var n_users_dym "Num. Users per District-Yearmonth"
	la var sr "Species Richness"
	la var sr_udym "Species richness across all trips in district-month"
	la var sr_uym "Species richness across all trips in year-month"
	la var sr_uyr "Species richness across all trips in year"
	la var sr_ym "Species richness across all users in year-month"
	la var sr_yr "Species richness across all users in year"
	la var group_size "Group Size"
	la var sh_index "Shannon Index"
	la var si_index "Simpson Index"
	la var duration "Duration (min)"
	la var distance "Distance (km)"
	la var rain "Rainfall (mm)"
	la var temp "Temperature ($\degree$ C)"
	la var n_mon_yr "Birding Rate (Months/Year)"
	la var exp_idx "Experience" 
	la var ln_exp_idx "Experience" 
	la var ln_duration "Duration (min)"
	la var ln_distance "Distance (km)"
	la var traveling "\% Travelling Trips"
	
	* Save
	drop sr_uyr sr_hr sr_uym sr_yr n_cells_dist tot_area tot_pop
	order user_id c_code_2011 year_month sr* *_index
	sort user_id c_code_2011 year_month
	save "${DATA}/dta/ebird_`level'", replace

}

*===============================================================================
* FOREST CLEARANCE
*===============================================================================
*---------------------------------------------------
* PROGRAM TO CONSTRUCT BALANCED DEFORESTATION PANEL
* arguments: stage
*	Approval stage at which to include projects
* 	Select 1 or 2
* arguement: restrict
*	Sample restriction
*	drop - drops megaprojects > 10km2
*	trunc - truncates full sample at 99th percentile
*---------------------------------------------------
capture program drop construct_deforest
program define construct_deforest

	syntax, stage(integer) [restrict(string)] [drop_projects(string)]
	
	*------------------------
	* SELECT APPROVAL STAGE
	*------------------------
	
	* Read post-2014 permits
	use "${DATA}/dta/fc_clean_v02", clear

	* Select Stage
	if `stage' == 2 {
	
		keep if prop_status == "approved"
		
		* add pre-2014 submissions (approved post 2014)
		append using "${DATA}/dta/fc_pre2014_clean_v02"
		replace pre2014 = 0 if pre2014 == .
	}
	if `stage' == 1 {
		keep if prop_status == "approved by rohq" | regexm(prop_status, "principle") == 1 | regexm(prop_status, "pending at ho") == 1 | prop_status == "pending at ro for stage-ii" // 3,858 (4068) projects
	}
	
	* Date
	g year_month = ym(year(date_rec), month(date_rec))
	format year_month %tmCCYY-NN
	drop if year_month == .
	
	* Alternative Distribution (UPDATE)
	/*---------------------------------------
	* 2 megaprojects in TG - laying of canals, 
	  tunnels, power lines for lift irrigation
	* 1 coal PP - 4400MW and 2400MW stations
	-----------------------------------------*/
	* Drop 3 megaprojects
	if "`restrict'" == "drop" {
		drop if proj_area_forest2 > 2000 // drops 4 megaprojects > 20 km2
	}
	* Keep all projects - truncate size
	if "`restrict'" == "trunc" {
		winsor2 proj_area_forest2, cut(0 99) replace
	}
	
	*-----------------------------------
	* RESHAPE TO PROJECT-DISTRICT LEVEL
	*-----------------------------------
	/*-------------------------------------------------
	- industry projects w/ tiny def'n. 
	- SE's huge in own category. add to "other"
	---------------------------------------------------*/
	replace proj_cat = "underground" if proj_cat == "mining" & mining_type == "underground"
	replace proj_cat = "other" if inlist(proj_cat, "underground", "industry")
	
	* Drop projects in provided list (REMOVE?)
	if "`drop_projects'" != "" {

		foreach v of local drop_projects {
			drop if proj_cat == "`v'"
		}
	}
	
	* Reshape project-district level
	reshape long district_ dist_f_ dist_nf_, i(prop_no) j(dist_id)
	ren (district_ dist_f_ dist_nf_) (district dist_f dist_nf)
	drop if district == ""

	* Sync district names w census
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
	non-matched:
		v01: 220 w/ no FC; 9 w complex splits
		v02: 141 w/ no FC; 8 w complex splits
	---------------------------------------*/
	
	* Save project-level
	if `stage' == 2 {
		save "${DATA}/dta/fc_pdym_s2_v02", replace
	}
	
	*------------------------------
	* AGGREGATE TO DISTRICT-MONTH
	*------------------------------

	* Land Diversion by project category
	levelsof proj_cat, local(proj_cat)
	foreach cat of local proj_cat {
		
		local abbrev_`cat' = substr("`cat'", 1, 3) // project abbrevation
		g dist_f_`abbrev_`cat'' = dist_f if proj_cat == "`cat'"
		g n_`abbrev_`cat'' = (proj_cat == "`cat'") // number of projects of type
		g dist_nf_`abbrev_`cat'' = dist_nf if proj_cat == "`cat'"
	}	
	/*
	* Land diversion by project type 
	levelsof proj_type, local(proj_type)
	foreach type of local proj_type {
		
		local abbrev_`type' = substr("`type'", 1, 3) // project abbrevation
		g dist_f_`abbrev_`type'' = dist_f if proj_type == "`type'"
		g n_`abbrev_`type'' = (proj_type == "`type'") // number of projects of type
		g dist_nf_`abbrev_`type'' = dist_nf if proj_type == "`type'"
	}
	*/
	* Aggregate
	collapse (sum) dist_f* dist_nf* n_* ///
		     (count) n_proj = dist_id, ///
			 by(c_code_2011 year_month)

	* Label
	la var dist_f "Deforestation"
	la var dist_nf "Non-forest Diversion"
	foreach cat of local proj_cat {
		
		local lab = proper("`cat'")
		la var dist_f_`abbrev_`cat'' "`lab'"
		la var n_`abbrev_`cat'' "`lab'"
		la var dist_nf_`abbrev_`cat'' "`lab'"
	}
	/*
	foreach type of local proj_type {
		
		local lab = proper("`type'")
		la var dist_f_`abbrev_`type'' "`lab'"
		la var n_`abbrev_`type'' "`lab'"
		la var dist_nf_`abbrev_`type'' "`lab'"
	}
	*/
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
	
	* Dates
	g year = year(dofm(year_month))
	g month = month(dofm(year_month))
	keep if inrange(year, 2014, 2020)

	*------------------------------
	* CONSTRUCT VARIABLES
	*------------------------------
	
	* Add Forest Area
	tempfile temp
	save "`temp'"
	import delimited "${DATA}/csv/forest_cover.csv", clear
	merge 1:m c_code_2011 year using "`temp'", keep(2 3) nogen
	
	* Baseline tree cover
	bys c_code_2011 (year_month): g tree_cover_base = tree_cover_km2 if _n == 1
	bys c_code_2011: carryforward tree_cover_base, replace

	* Encroachment Area
	foreach var of varlist dist_f-dist_nf_tra {
		
		* No deforestation
		replace `var' = 0 if `var' == .
		local vlab : var lab `var'
	
		* Cumulative km2
		bys c_code_2011 (year_month): g `var'_cum_km2 = sum(`var')/100 // cumulative
		la var `var'_cum_km2 "`vlab'"
		drop `var'
	}
	
	* Number of Projects
	foreach var of varlist n_ele-n_proj {
		
		* No deforestation
		replace `var' = 0 if `var' == .
		local vlab : var lab `var'
		
		* Cumulative
		bys c_code_2011 (year_month): g `var'_cum = sum(`var')
		la var `var'_cum "`vlab'"
		drop `var'
	}
	
	* Share of total projects
	foreach var of varlist n_ele_cum - n_tra_cum {
		
		local vlab : var lab `var'
		g `var'_s = 0 
		la var `var'_s "`vlab' (Pct. of total)"
		replace `var'_s = (`var' / n_proj_cum)*100 if n_proj_cum > 0
		drop `var'
	}
	
	* State/Dist Strings
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", ///
		keepus(state district) nogen
	g state_code_2011 = substr(c_code_2011, 1, 3)
	encode state_code_2011, gen(state_code_2011_num)
	
	* Finish
	order c_code_2011 year_month dist_f*
	sort c_code_2011 year_month

end

if `district_forest' == 1 {

	* Stage 2 (drop 3 mega projects)
	construct_deforest, stage(2) restrict("drop")
	save "${DATA}/dta/fc_dym_s2_v02", replace
	export delimited "${DATA}/csv/fc_dym_s2_v02.csv", replace
	
	* Stage 1 (drop 3 mega projects)
	construct_deforest, stage(1) restrict("drop")
	save "${DATA}/dta/fc_dym_s1_v02", replace

	// Stage 2 - Truncate at 99th pctile
	construct_deforest, stage(2) restrict("trunc")
	save "${DATA}/dta/fc_dym_s2_trunc_v02", replace
	
	// Full
	construct_deforest, stage(2)
	save "${DATA}/dta/fc_dym_s2_full_v02", replace
}

*===============================================================================
* MERGE DATASETS
*===============================================================================
if `merge' == 1 {

	local fc_data "trunc" // either "" (main dataset), "full", or "trunc" 
	local level "uct" // udt (user-dist-time) or uct (user-cell-time)
	
	if "`fc_data'" == "" {
		
		* Prepare Stage I Deforestation
		use "${DATA}/dta/fc_dym_s1_v02", clear
		keep c_code_2011 year_month dist_f_cum_km2 dist_nf_cum_km2
		ren (dist_f_cum_km2 dist_nf_cum_km2) (dist_f_cum_km2_s1 dist_nf_cum_km2_s1)
		tempfile stage1
		save "`stage1'"
		
		* Spatial Spillovers
		import delimited "${DATA}/csv/slx_v02.csv", clear
		keep c_code_2011 year_month dist_f_cum_km2_slx_*
	
		ren year_month ym
		g year_month = ym(year(date(ym, "20YM")), month(date(ym, "20YM")))
		format year_month %tmCCYY-NN
		drop ym
		
		tempfile slx
		save "`slx'"

		*----------------------------
		* CONSTRUCT FINAL DATASET
		*----------------------------
		
		* Read Stage II Deforestation
		use "${DATA}/dta/fc_dym_s2_v02", clear
		merge 1:1 c_code_2011 year_month using "`slx'", nogen // merge slx
		merge 1:1 c_code_2011 year_month using "`stage1'", nogen // merge stage 1
		merge 1:m c_code_2011 year_month using "${DATA}/dta/ebird_`level'", keep(3) nogen // merge ebird
		
		* Save
		sort user_id c_code_2011 year_month
		order user_id *_code_2011 year_month state district sr *_index
		save "${DATA}/dta/fc_ebd_`level'_v02", replace
	}
	
	else {
	
		* Read
		use "${DATA}/dta/fc_dym_s2_`fc_data'_v02", clear
		
		* Merge to ebird
		merge 1:m c_code_2011 year_month using "${DATA}/dta/ebird_`level'", keep(3) nogen
		
		* Save
		sort user_id year_month
		order user_id *_code_2011 year_month state district sr *_index
		save "${DATA}/dta/fc_ebd_`level'_`fc_data'_v02", replace
	}

}
