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

//Set Directory Paths
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
/*---------------------------------------
For dataset restricted to trips with 
all species reported, set local `data'
to "_allreported", otherwise leave `data'
empty
-----------------------------------------*/
if `ebird' == 1 {
	
	local data "" // set as "" (main) or "_allreported"
	
	//1. Clean
	
	* Read
	import delimited using "${DATA}/csv/ebird_user`data'_v02.csv", clear
	
	* Clean
	destring distance si_index group_size, replace force
	
	* Format Date
	g year_month = ym(year, month(date(yearmonth, "20YM")))
	format year_month %tmCCYY-NN
	drop yearmonth
	
	tempfile ebd_user
	save "`ebd_user'"
	
	//2. Merge temporal data
	
	foreach file in "coverage_ym_grid_5km`data'" "india_rainfall_gpm" "india_temperature_era" {
	
		import delimited "${DATA}/csv/`file'", varn(1) clear
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
	merge 1:m c_code_2011 using "`ebd_user'", keep(3) nogen
	save "`ebd_user'", replace
	
	//4. Biome Clusters
	import delimited "${DATA}/csv/biome_cluster", clear
	merge 1:m c_code_2011 using "`ebd_user'", keep(2 3) nogen
	replace biome = 888 if biome == . // note: only 1 district w missing biome (Diu)
	
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
	bys c_code_2011 year_month: egen n_users_dym = nvals(observerid)
	g pop_density = tot_pop / tot_area
	ren (temp_era rain_gpm) (temp rain)
	destring temp, force replace
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
	la var coverage "Spatial Coverage (\%)"
	la var duration "Duration (min)"
	la var distance "Distance (km)"
	la var rain "Rainfall (mm)"
	la var temp "Temperature ($\degree$ C)"
	la var all_species "\% Full Reporting"
	la var n_mon_yr "Birding Rate (Months/Year)"
	la var exp_idx "Experience" 
	
	* Save
	order user_id c_code_2011 year_month sr* *_index
	sort user_id c_code_2011 year_month
	save "${DATA}/dta/ebird_user`data'", replace

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
* agurment: drop_projects
*	project type to exclude
*	select from "electricity irrigation
*   transportation mining resettlement other"
*---------------------------------------------------
capture program drop construct_deforest
program define construct_deforest

	syntax, stage(integer) [restrict(string)] [drop_projects(string)]
	
	*------------------------
	* SELECT APPROVAL STAGE
	*------------------------
	
	* Read 
	use "${DATA}/dta/fc_clean_v02", clear

	* Select Stage
	if `stage' == 2 {
		keep if prop_status == "approved"
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
	// Drop 3 megaprojects
	if "`restrict'" == "drop" {
		drop if proj_area_forest2 > 1000 // drops 3 megaprojects > 10 km2
	}
	// Keep all projects - truncate size
	if "`restrict'" == "trunc" {
		winsor2 proj_area_forest2, cut(0 99) replace
	}
	
	*-----------------------------------
	* RESHAPE TO PROJECT-DISTRICT LEVEL
	*-----------------------------------
	
	* Re-categorize projects
	/*------------------------------------------------------------
	there are 42 ind. projects which cause miniscule def'n. 
	296 underground projects (pipelines, OFC) which cause almost no 
	def'n. SE's are massive in own category. I lump into "other"
	-------------------------------------------------------------*/
	replace proj_cat = "other" if inlist(proj_cat, "industry", "underground")
	
	* Drop projects in provided list
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
		v02: 215 w/ no FC; 8 w complex splits
	---------------------------------------*/
	
	*------------------------------
	* AGGREGATE TO DISTRICT-MONTH
	*------------------------------
	
	* Land Diversion by project type
	levelsof proj_cat, local(proj_cat)
	foreach cat of local proj_cat {
		
		local abbrev_`cat' = substr("`cat'", 1, 3) // project abbrevation
		g dist_f_`abbrev_`cat'' = dist_f if proj_cat == "`cat'"
		g n_`abbrev_`cat'' = (proj_cat == "`cat'") // number of projects of type
		g dist_nf_`abbrev_`cat'' = dist_nf if proj_cat == "`cat'"
	}	
	
	* Aggregate
	collapse (sum) dist_f* dist_nf* n_* ///
		     (count) n_patches = dist_id, ///
			 by(c_code_2011 year_month)

	* Label
	la var dist_f "Deforestation"
	la var dist_nf "Non-forest Diversion"
	la var n_patches "Number of Patches"
	foreach cat of local proj_cat {
		
		local lab = proper("`cat'")
		la var dist_f_`abbrev_`cat'' "`lab'"
		la var n_`abbrev_`cat'' "`lab'"
		la var dist_nf_`abbrev_`cat'' "`lab'"
	}
	
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
	keep if inrange(year, 2015, 2020)
	kk
	*------------------------------
	* CONSTRUCT VARIABLES
	*------------------------------
	
	* Add district total forest area
	tempfile temp
	save "`temp'"
	import delimited "${DATA}/csv/forest_cover.csv", clear
	merge 1:m c_code_2011 year using "`temp'", keep(2 3) nogen
	
	* Baseline tree cover
	bys c_code_2011 (year_month): g tree_cover_base = tree_cover_km2 if _n == 1
	bys c_code_2011 (year_month): g tree_cover_pt_base = tree_cover_pct if _n == 1
	bys c_code_2011: carryforward tree_cover*base, replace

	* Encroachment Area
	foreach var of varlist dist_f-dist_nf_tra {
		
		* No deforestation
		replace `var' = 0 if `var' == .
		local vlab : var lab `var'
	
		* Cumulative
		bys c_code_2011 (year_month): g `var'_cum = sum(`var')
		la var `var'_cum "`vlab'"
		
		* km2
		g `var'_km2 = `var' / 100
		la var `var'_km2 "`vlab'"
		g `var'_cum_km2 = `var'_cum / 100
		la var `var'_cum_km2 "`vlab'"
		
		* Share of forest area
		g `var'_cum_km2_s = (`var'_cum_km2 / tree_cover_base)*100
		la var `var'_cum_km2_s "`vlab'"
			
		* Inverse Hyperbolic Sine
		g `var'_cum_ihs = asinh(`var'_cum_km2)
		la var `var'_cum_ihs "`vlab'"
	}
	
	* Number of Projects
	foreach var of varlist n_ele-n_patches {
		
		* No deforestation
		replace `var' = 0 if `var' == .
		local vlab : var lab `var'
		
		* Cumulative
		bys c_code_2011 (year_month): g `var'_cum = sum(`var')
		la var `var'_cum "`vlab'"
		
		* Projects per 10 km2 of forest
		g `var'_cum_s = (`var'_cum / tree_cover_base)*10
		la var `var'_cum_s "`vlab'"
	}

	* Share of total projects
	foreach var of varlist n_ele_cum - n_tra_cum {
		local vlab : var lab `var'
		g `var'_proj_s = 0 
		la var `var'_proj_s "`vlab' (share of total projects)"
		replace `var'_proj_s = `var' / n_patches_cum if n_patches_cum > 0
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
	
	// Main (drop 3 mega, drop mines)
	*construct_deforest, stage(2) restrict("drop") drop_projects("mining")
	*save "${DATA}/dta/fc_dist_ym_stage2", replace
	*export delimited "${DATA}/csv/fc_dist_ym_stage2.csv", replace
	
	* Stage 1 (drop 3 mega projects)
	construct_deforest, stage(1) restrict("drop")
	save "${DATA}/dta/fc_dym_s1_v02", replace
	
	* Stage 2
	// Main (drop 3 mega projects)
	construct_deforest, stage(2) restrict("drop")
	save "${DATA}/dta/fc_dym_s2_v02", replace
	export delimited "${DATA}/csv/fc_dym_s2_v02.csv", replace

	// Truncate at 99th pctile
	construct_deforest, stage(2) restrict("trunc")
	save "${DATA}/dta/fc_dym_s2_trunc99_v01", replace
	
	// Full
	construct_deforest, stage(2)
	save "${DATA}/dta/fc_dym_s2_full_v01", replace
}

*===============================================================================
* MERGE DATASETS
*===============================================================================
if `merge' == 1 {
	
	local fc_data "" // either "" (main dataset), "full", or "trunc99"
	local ebd_restrict "" // either "" (main) or "_allreported" 
	local version 2 
	/*---------------------------------------
	For complete lists, set `restrict' to 
	"_allreported"
	----------------------------------------*/
	
	if "`fc_data'" == "" {
		
		* Prepare Stage I Deforestation
		use "${DATA}/dta/fc_dym_s1_v0`version'", clear
		keep c_code_2011 year_month dist_f_km2 dist_nf_km2
		ren (dist_f_km2 dist_nf_km2) (dist_f_km2_s1 dist_nf_km2_s1)
		la var dist_f_km2_s1 "Deforestation (Stage 1)"
		tempfile stage1
		save "`stage1'"
		
		* Spatial Spillovers
		import delimited "${DATA}/csv/slx_v0`version'.csv", clear
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
		
		* Read Stage II Deforestation (main)
		use "${DATA}/dta/fc_dym_s2_v0`version'", clear
		
		* Merge SLX
		merge 1:1 c_code_2011 year_month using "`slx'", nogen
		
		* Merge Stage I Deforestation
		merge 1:1 c_code_2011 year_month using "`stage1'", nogen // 2 variables
	
		* Merge to ebird
		merge 1:m c_code_2011 year_month using "${DATA}/dta/ebird_user`ebd_restrict'", keep(3) nogen
		
		* User Time Trend
		bys user_id (year_month): g u_lin = sum(year_month != year_month[_n-1]) // Linear 
		la var u_lin "Linear Trend"
		g u_cubic = u_lin^3
		la var u_cubic "Cubic Trend"
		
		* Save
		sort user_id year_month
		order user_id *_code_2011 year_month state district sr *_index
		save "${DATA}/dta/fc_ebd_user`ebd_restrict'_v0`version'", replace
	}
	
	else {
	
		* Read
		use "${DATA}/dta/fc_dist_ym_stage2_`fc_data'", clear
		
		* Merge to ebird
		merge 1:m c_code_2011 year_month using "${DATA}/dta/ebird_user", keep(3) nogen
		
		* User Time Trend
		bys user_id (year_month): g u_lin = sum(year_month != year_month[_n-1]) // Linear 
		la var u_lin "Linear Trend"
		g u_cubic = u_lin^3
		la var u_cubic "Cubic Trend"
		
		* Save
		sort user_id year_month
		order user_id *_code_2011 year_month state district sr *_index
		save "${DATA}/dta/fc_ebd_user_`fc_data'", replace
	}

}
