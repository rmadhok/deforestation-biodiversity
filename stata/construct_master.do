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
local district_forest		0
local slx					1

*===============================================================================
* BIODIVERSITY
*===============================================================================
if `ebird' == 1 {
	
	//1. Clean
	
	* Read
	import delimited using "${DATA}/csv/ebird_user.csv", clear
	
	* Clean
	destring duration distance si_index, replace force
	drop if c_code_2011 == "NA"
	
	* Format Date
	gen year_month = ym(year, month(date(yearmonth, "20YM")))
	format year_month %tmCCYY-NN
	drop yearmonth
	
	tempfile biodiv_user
	save "`biodiv_user'"
	
	//2. Merge temporal data
	foreach file in "coverage_ym_grid_5km" "india_precipitation" "india_temperature" {
	
		import delimited "${DATA}/csv/`file'", clear
		gen year_month = ym(year(date(yearmonth, "20YM")), month(date(yearmonth, "20YM")))
		format year_month %tmCCYY-NN
		drop yearmonth
		
		** Merge
		merge 1:m c_code_2011 year_month ///
			using "`biodiv_user'", keep (2 3) nogen
		
		save "`biodiv_user'", replace
	}
	
	//3. Merge with District Data
	import delimited "${DATA}/csv/coverage_dist_grid_5km", clear
	merge 1:1 c_code_2011 using "${DATA}/dta/2011_india_dist", ///
		keepus(state district tot_pop tot_area) nogen
	merge 1:m c_code_2011 using "`biodiv_user'", keep(3) nogen

	* Transform
	foreach var of varlist s_richness *_index {
		gen `var'_ihs = asinh(`var')
	}
	
	* Generate vars
	gen state_code_2011 = substr(c_code_2011, 1, 3)
	bys observerid: egen n_states_user = nvals(state_code_2011)
	bys observerid: egen n_dist_user = nvals(c_code_2011)
	bys observerid: egen n_ym_user = nvals(year_month)
	bys observerid: egen n_trips_user = total(n_trips)
	bys c_code_2011: egen n_users_dist = nvals(observerid)
	gen pop_density = tot_pop / tot_area
	gen veteran = (n_mon_yr >= 6)
	encode observerid, gen(user_id)
	drop state_code_2011
	
	* Label
	la var tot_area "District Area (\(km^{2}\))"
	la var pop_density "Population Density (per \(km^{2}\))"
	la var n_ym_user "Num. Year-months per User"
	la var n_dist_user "Num. Districts per User"
	la var n_states_user "Num. States per User"
	la var n_users_dist "Num. Users"
	la var n_trips "Num. Trips per User"
	la var n_trips_user "Num. Trips per User"
	la var s_richness "Species Richness"
	la var s_richness_ihs "Species Richness"
	la var sh_index "Shannon Index"
	la var sh_index_ihs "Shannon Index"
	la var si_index "Simpson Index"
	la var si_index_ihs "Simpson Index"
	la var coverage "Spatial Coverage"
	la var coverage_all "Spatial Coverage"
	la var duration "Duration (min)"
	la var distance "Distance (km)"
	la var veteran "Veteran (=1)"
	la var all_species_prop "\% Full Reporting"
	la var n_mon_yr "Months/Year of Birdwatching"
	
	* Save
	order user_id c_code_2011 year_month
	sort user_id c_code_2011 year_month
	save "${DATA}/dta/ebird_user", replace

}

*===============================================================================
* FOREST CLEARANCE
*===============================================================================
if `district_forest' == 1 {
	
	//1. Organize project-level
	
	* Read 
	use "${DATA}/dta/fc_clean", clear //19,495 total projects
	
	* Stage I and II approvals
	gen stage2 = 1 if prop_status == "approved" // 2,773 projects
	replace stage2 = 0 if prop_status == "approved by rohq" | regexm(prop_status, "principle") == 1 | regexm(prop_status, "pending at ho") == 1 | prop_status == "pending at ro for stage-ii" // 3,858 projects
	*keep if stage2 == 1
	keep if stage2 == 1 | stage2 == 0 // 6,631 projects
	
	* Format Date
	gen year_month = ym(year(date_rec), month(date_rec))
	format year_month %tmCCYY-NN
	drop if year_month == .
	
	//2. Project-District Level
	
	* Reshape
	reshape long district_ district_forest_ district_nonforest_, ///
		i(prop_no) j(dist_id)
	
	* Clean
	ren (district_ district_forest_ district_nonforest_) ///
		(district dist_f dist_nf)
	drop if district == ""
	
	//3. Merge Census Codes

	* Replace names
	do "${DO}/dist_name_sync"
	
	tempfile fc_district
	save "`fc_district'"

	* Merge with Census
	use "${DATA}/dta/2011_india_dist", clear
	keep state district c_code_2011
	replace state = lower(state)
	replace district = lower(district)
	merge 1:m state district using "`fc_district'", keep(3) nogen
	
	//4. Aggregate to District

	* Sector Indicator
	tab proj_cat, gen(proj_cat_)
	tab proj_shape, gen(proj_shape_)
	
	* Land Diversion by Project Type
	local projlist `" "Electricity" "Forest Village Relocation" "Industry" "Irrigation" "Mining" "Other" "Transport" "'
	local i = 1
	
	foreach j in elec fvr ind irr mine o tran {
	
		* Forest Area
		gen forest_`j' = dist_f if proj_cat_`i' == 1
		gen nonforest_`j' = dist_nf if proj_cat_`i' == 1
		bys c_code_2011 year_month: egen dist_f_`j' = total(forest_`j')
		bys c_code_2011 year_month: egen dist_nf_`j' = total(nonforest_`j')
		drop forest_`j' nonforest_`j'
		
		* Label
		local type : word `i' of `projlist'
		la var dist_f_`j' "`type'" 
		la var dist_nf_`j' "Non-forest Diversion (`type')" 
		
		local ++i
	}
		
	* Land Diversion by Project Shape
	local shapelist "Hybrid Linear Non-Linear"
	local i = 1
	
	foreach j in hyb lin nl {
	
		gen forest_`j' = dist_f if proj_shape_`i' == 1
		gen nonforest_`j' = dist_nf if proj_shape_`i' == 1
		bys c_code_2011 year_month: egen dist_f_`j' = total(forest_`j')
		bys c_code_2011 year_month: egen dist_nf_`j' = total(nonforest_`j')
		drop forest_`j' nonforest_`j'
		
		* Label
		local type : word `i' of `shapelist'
		la var dist_f_`j' "`type'" 
		la var dist_nf_`j' "Non-forest Land Diversion (`type')" 
		
		local ++i
		
	}
	
	** Land Diversion in Protected Area
	gen forest_pa = dist_f if proj_in_pa_esz_num == 1
	gen nonforest_pa = dist_nf if proj_in_pa_esz_num == 1
	bys c_code_2011 year_month: egen dist_f_pa = total(forest_pa)
	la var dist_f_pa "Near Protected Area"
	bys c_code_2011 year_month: egen dist_nf_pa = total(nonforest_pa)
	la var dist_nf_pa "Non-forest Land Diversion (Near Protected Area)"
	drop forest_pa nonforest_pa

	* Save Labels
	foreach v of varlist dist_f_* dist_nf_* {
		local lab_`v' : var lab `v'
	}

	* Aggregate
	collapse (sum)  dist_f dist_nf ///
			 (first) dist_f_* dist_nf_* ///
			 (mean) stage2, ///
			 by(c_code_2011 year_month)
	
	* Label
	foreach v of varlist dist_f_* dist_nf_* {
		la var `v' "`lab_`v''"
	}
	la var dist_f "Deforestation"
	la var dist_nf "Non-forest Diversion"
	
	//5. Balance Panel
	
	* Merge with Census
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", keepus(c_code_2011)

	* Balance Panel
	encode c_code_2011, gen(c_code_2011_num)
	tsset c_code_2011_num year_month
	sort year_month 
	gen ym = year_month[1]
	replace year_month = ym if _m == 2
	tsfill, full
	drop c_code_2011 _merge ym
	decode c_code_2011_num, gen(c_code_2011)
	
	//6. Generate Variables
	foreach var of varlist dist_f-dist_nf_pa {
		
		* No deforestation
		replace `var' = 0 if `var' == .
		local vlab : var lab `var'
	
		* Cumulative
		sort c_code_2011 year_month
		by c_code_2011: gen `var'_cum = sum(`var')
		la var `var'_cum "`vlab'"
		
		* km2
		gen `var'_cum_km2 = `var'_cum / 100
		la var `var'_cum_km2 "`vlab'"
			
		* Inverse Hyperbolic Sine
		gen `var'_cum_ihs = asinh(`var'_cum_km2)
		la var `var'_cum_ihs "`vlab'"
		
	}
	bys c_code_2011: replace stage2 = stage2[_n - 1] if dist_f == 0
	replace stage2 = 0 if stage2 == .
	gen stage2_ihs = asinh(stage2)
	la var stage2 "\% Stage II Projects"
	la var stage2_ihs "\% Stage II Projects"
	
	* State/Dist Strings
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", ///
		keepus(state district) nogen
	gen state_code_2011 = substr(c_code_2011, 1, 3)
	encode state_code_2011, gen(state_code_2011_num)
	
	** Dates
	gen date = dofm(year_month)
	gen year = year(date)
	gen month = month(date)
	keep if year > 2014
	drop date
	
	* Add Forest Cover
	tempfile temp
	save "`temp'"
	import delimited "${DATA}/csv/forest_cover.csv", clear
	merge 1:m c_code_2011 year using "`temp'", keep(2 3) nogen
	
	gen tree_cover_mean_ihs = asinh(tree_cover_mean)
	la var tree_cover_mean_ihs "Tree Cover"
	la var tree_cover_mean "Tree Cover (\%)"
	
	* Lags
	sort c_code_2011 year_month
	foreach var of varlist *f_cum_ihs {
	
		foreach i of numlist 1/12 {
		
			bys c_code_2011: gen `var'_l`i' = `var'[_n - `i']
			la var `var'_l`i' "`i' Month"
		
		}
	}
	
	* Save
	order c_code_2011 year_month dist_f*
	*save "${DATA}/dta/fc_dist_ym_main", replace
	save "${DATA}/dta/fc_dist_ym_robust", replace
	*export delimited "${DATA}/csv/fc_dist_ym_main.csv", replace
	export delimited "${DATA}/csv/fc_dist_ym_robust.csv", replace
	
}

if `slx' == 1 {
	
	// Setting (main or robust)
	local section "main"
	
	* Read SLX
	import delimited "${DATA}/csv/slx_`section'.csv", clear

	* Clean
	destring *mean*, replace force
	ren year_month yearmonth
	gen year_month = ym(year(date(yearmonth, "20YM")), month(date(yearmonth, "20YM")))
	format year_month %tmCCYY-NN
	drop yearmonth
	
	la var dist_f_cum_ihs_slx_bc "Sp. Lag Deforestation"
	la var dist_nf_cum_ihs_slx_bc "Sp. Lag Non-forest Diversion"
	la var tree_cover_mean_ihs_slx_bc "Sp. Lag Tree Cover"
	la var dist_f_cum_ihs_slx_i "Sp. Lag Deforestation"
	la var dist_nf_cum_ihs_slx_i "Sp. Lag Non-forest Diversion"
	la var tree_cover_mean_ihs_slx_i "Sp. Lag Tree Cover"
	la var dist_f_cum_ihs_slx_i2 "Sp. Lag Deforestation"
	la var dist_nf_cum_ihs_slx_i2 "Sp. Lag Non-forest Diversion"
	la var tree_cover_mean_ihs_slx_i2 "Sp. Lag Tree Cover"
	if "`section'" == "robust" {
		la var stage2_ihs_slx_bc "Sp. Lag \% Stage II Projects"
		la var stage2_ihs_slx_i "Sp. Lag \% Stage II Projects"
		la var stage2_ihs_slx_i2 "Sp. Lag \% Stage II Projects"
	}
	
	* Merge to forest clearance
	merge 1:1 c_code_2011 year_month using "${DATA}/dta/fc_dist_ym_`section'", nogen

	* Merge to ebird
	merge 1:m c_code_2011 year_month using "${DATA}/dta/ebird_user", keep(3) nogen
	
	* User Time Trend
	bys user_id (year_month): gen user_trend = sum(year_month != year_month[_n-1]) 
	la var user_trend "User Time Trend"
	
	* Save
	sort user_id year_month
	order user_id *_code_2011 year_month state district s_richness *_index
	save "${DATA}/dta/fc_ebd_user_`section'", replace
		
}


