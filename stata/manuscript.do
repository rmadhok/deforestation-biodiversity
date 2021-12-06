*********************************************************************************
*																			
* PROJECT: 	Deforestation and Biodiversity										
*																			
* PURPOSE: 	MANUSCRIPT (TEX)   
*																			
* START DATE: January 27, 2020   				 							
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
gl TABLE	"${ROOT}/docs/jmp/tex_doc/v3"
cd "${TABLE}"

// Modules
local main_analysis		0
local mechanism			1
local dynamics			0
local simulation		0
local robustness		0
	local patch			0
	local others		0

set scheme modern
*===============================================================================
* PROGRAMS
*===============================================================================
// Main Spec
capture program drop reg_sat
program define reg_sat
	
	syntax varlist
	set more off
	
	* Parse 
	local depvar : word 1 of `varlist'
	local indepvar : word 2 of `varlist'
	local ctrls = substr("`varlist'", length("`depvar'") + ///
		length("`indepvar'") + 3, length("`varlist'"))

	reghdfe `depvar' `indepvar' `ctrls', ///
		a(user_id#year c_code_2011_num state_code_2011_num#month, savefe) ///
		vce(cl biome) resid
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		sum `depvar' if e(sample)==1
		estadd scalar ymean = `r(mean)'
end

// Drop Outliers
capture program drop drop_outliers
	program define drop_outliers

	egen n_trips_pc99 = pctile(n_trips), p(99)
	gen outlier = (n_trips > n_trips_pc99)
	drop if outlier == 1

end

*===============================================================================
* MAIN ANALYSIS
*===============================================================================
if `main_analysis' == 1 {
	
	* Read
	use "${DATA}/dta/fc_ebd_user_v02", clear
	
	* Prep
	*g tree_cover_p = tree_cover_km2 / tot_area
	local ctrls coverage temp rain tree_cover_km2 ln_duration ln_exp_idx
	*drop_outliers
	
	la var dist_f_cum_km2 "Forest Infrastructure (\(km^{2}\))"
	la var dist_nf_cum_km2 "Non-Forest Diversion (\(km^{2}\))"
	
	*--------------------
	* 1. MAIN RESULTS
	*--------------------
	* NOTE: REMOVE TRIP WEIGHTS (SEE REFEREE COMMENT)
	/*
	eststo clear
	eststo m1: reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls', ///
		a(user_id c_code_2011_num state_code_2011_num#month year) vce(cl biome)
	
		sum sr if e(sample)==1
		estadd scalar ymean = `r(mean)'
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"

	eststo m2: reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls', ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) vce(cl biome)

		sum sr if e(sample)==1
		estadd scalar ymean = `r(mean)'
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"

	* Coefficient Plot
	coefplot (m1, rename(dist_f_cum_km2 = `" "(1) Learning" "Bias" "') \ m2, ///
		rename(dist_f_cum_km2 = `" "(2) No Learning" "Bias" "')), ///
		keep(dist_f*) xline(0, lcolor(maroon) lpattern(solid)) ///
		coeflabels(, labsize(medium)) mlabsize(medium) mlabcolor(black) ///
		ciopts(recast(rcap) lpattern(shortdash) lcolor(gs5)) msize(medium) ///
		mfcolor(white) msymbol(D) mlcolor(black) mlabel format(%9.2g) ///
		mlabposition(1) mlabgap(*2) xlabel(-0.5(.5)0.5, labsize(medium)) ///
		ylabel(, labsize(medium)) title("A", size(large)) ///
		saving("${TABLE}/fig/main.gph", replace)
	graph export "${TABLE}/fig/main.png", replace
	
	* Table
	esttab using "${TABLE}/tables/main_results.tex", keep(dist_*_cum_km2) replace ///
		stats(ymean user_fe user_y_fe dist_fe st_m_fe ///
		st_y_fe year_fe N r2, labels(`"Outcome Mean"' ///
		`"User FEs"' `"User x Year FEs"' `"District FEs"' ///
		`"State x Month FEs"' `"State x Year FEs"' `"Year FEs"' ///
		`"N"' `"\(R^{2}\)"') fmt(3 0 0 0 0 0 0 0 3)) ///
		mgroups("With Learning Bias" "Without Learning Bias", ///
		pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span ///
		erepeat(\cmidrule(lr){@span})) indicate("Experience Index=exp_idx") ///
		wrap nocons nonotes booktabs nomtitles star(* .1 ** .05 *** .01) ///
		label se b(%5.3f) width(\hsize)
	eststo clear
	
	*--------------------------
	* 2. DECOMPOSED ESTIMATES
	*--------------------------

	* Fig 2 - category-Wise Impacts
	eststo clear
	eststo: reg_sat sr dist_f_ele_cum_km2 dist_f_irr_cum_km2 ///
		dist_f_min_cum_km2 dist_f_oth_cum_km2 dist_f_res_cum_km2 ///
		dist_f_tra_cum_km2 dist_nf_ele_cum_km2 dist_nf_irr_cum_km2 ///
		dist_nf_min_cum_km2 dist_nf_oth_cum_km2 dist_nf_res_cum_km2 ///
		dist_nf_tra_cum_km2 `ctrls'

	coefplot, keep(dist_f*) sort xline(0, lcolor(maroon) lpattern(solid)) ///
		msize(small) mfcolor(white) msymbol(D) ///
		ciopts(recast(rcap) lpattern(shortdash) lcolor(gs5)) ///
		mlabel format(%9.2g) mlabposition(1) mlabcolor(black) /// 
		coeflabels(, labsize(medium)) mlabsize(medium) mcolor(black) ///
		xlabel(, labsize(medium)) ylabel(, labsize(medium)) mlabgap(*2) ///
		title("B", size(large)) saving("${TABLE}/fig/projwise.gph", replace)
	graph export "${TABLE}/fig/projwise.png", replace
	
	*--------------------------
	* 3. FRAGMENTATION-WISE
	*--------------------------
	
	**# Linear model
	*g tree_cover_base_p = (tree_cover_base/tot_area)*100
	g dist_f_tree_base = dist_f_cum_km2 * tree_cover_base_p
	g dist_nf_tree_base = dist_nf_cum_km2 * tree_cover_base_p
	
	eststo clear
	eststo: reghdfe sr dist_f_tree_base dist_nf_tree_base ///
		dist_f_cum_km2 dist_nf_cum_km2 `ctrls', ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
	*/
	
	**# Quantiles
	
	// Quantiles of Forest Cover
	g tree_cover_base_p = (tree_cover_base/tot_area)*100
	xtile tcover_base_q = tree_cover_base_p, nquantiles(2)
	tab tcover_base_q, gen(tcover_q_)
	foreach v of varlist tcover_q_* { // interactions
		g dist_f_`v' = dist_f_cum_km2 * `v'
		g dist_nf_`v' = dist_nf_cum_km2 * `v'
	}

	la var dist_f_tcover_q_1 "1st"
	la var dist_f_tcover_q_2 "2nd" 
	*la var dist_f_tcover_q_3 "3rd"
	*la var dist_f_tcover_q_4 "4th"
	*la var dist_f_tcover_q_5 "5th"

	eststo clear
	eststo: reghdfe sr dist_f_tcover_q_1-dist_nf_tcover_q_1 ///
		dist_f_cum_km2 dist_nf_cum_km2 `ctrls', ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) vce(cl biome)

	coefplot, keep(dist_f_tcover_*) xline(0, lcolor(maroon) lpattern(solid))  ///
		msize(small) mfcolor(white) msymbol(D) mcolor(black) ///
		ciopts(recast(rcap) lpattern(shortdash) lcolor(gs5)) ///
		mlabel ytitle("Forest Cover (Quintiles)", size(large)) format(%9.2g) ///
		mlabposition(1) coeflabels(, labsize(medium)) mlabsize(medium) ///
		mlabcolor(black) xlabel(-2(2)2, labsize(medium)) ylabel(, labsize(medium)) ///
		mlabgap(*2) title("C", size(large)) ///
		saving("${TABLE}/fig/tcoverwise.gph", replace)
	graph export "${TABLE}/fig/tcoverwise.png", replace
	
	* MAIN RESULTS
	graph combine "${TABLE}/fig/main.gph" "${TABLE}/fig/projwise.gph" ///
		"${TABLE}/fig/tcoverwise.gph", holes(4)
	graph export "${TABLE}/fig/main_results.png", width(1000) replace

}

*===============================================================================
* MECHANISM
*===============================================================================
if `mechanism' == 1 {
	
	*---------------------
	* SEARCH ENDOGENEITY
	*---------------------
	capture program drop
	program define lab_vars
		
		la var dist_f_cum_km2 "Infrastructure (km2)"
		la var dist_nf_cum_km2 "Non-Forest Diversion (km2)"
		la var coverage "Access (\%)"
		la var temp "Temperature ($\degree$ C)"
		la var rain "Rain (mm)"
		la var tree_cover_km2 "Forest Cover (km2)"
		la var ln_duration "Duration"
		*la var ln_exp_idx "Experience"
	
	end
	
	* Read
	use "${DATA}/dta/fc_ebd_user_v02", clear
	local ctrls coverage temp rain tree_cover_km2 ln_duration ln_exp_idx
	*drop_outliers

	**# 2. Endogeneity: Do users move to other districts?
	*-------------------------------------------------------
	/*
	// USER LEVEL -- USE SLX or n_trips_sj???
	eststo: reghdfe n_trips dist_f_cum_km2 dist_nf_cum_km2 ///
		dist_f_cum_km2_slx_i_500 temp rain tree_cover_km2 ln_duration ln_exp_idx, ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
	
	* User-Level: Does a project displace users to other districts in same month 
	rangestat (sum) n_trips, interval(year_month 0 0) by(observerid state_code_2011) excludeself
	ren n_trips_sum n_trips_sj
	rangestat (sum) n_trips, interval(year_month 0 0) by(observerid) excludeself
	ren n_trips_sum n_trips_nj

	eststo: reghdfe n_trips_sj dist_f_cum_km2 dist_nf_cum_km2 ///
		temp rain tree_cover_km2 ln_duration ln_exp_idx, ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)

		estadd local agg "User-District-Time" 
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
		
	eststo: reghdfe n_trips_nj dist_f_cum_km2 dist_nf_cum_km2 ///
		temp rain tree_cover_km2 ln_duration ln_exp_idx, ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
		
		estadd local agg "User-District-Time" 
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"	
	// Note: No. But, there is little variation across districts w/n same month

	// DISTRICT LEVEL
	
	* Aggregate To District
	collapse (sum) n_trips (mean) exp_idx duration distance ///
			 (firstnm) dist_*_cum_km2_slx* dist_*_cum_km2 coverage ///
					   tree_cover_* temp tot_area rain biome ///
					   sr_bl *_code_2011_num month year, ///
		     by(c_code_2011 year_month)
	
	* District: Does a project cause users to increase trips to other districts?
	g ln_duration = log(duration)
	g ln_exp_idx = log(exp_idx)
	lab_vars
	
	
	*eststo: reghdfe n_trips dist_f_cum_km2 dist_f_cum_km2_slx_i_100 dist_nf_cum_km2 ///
	*	coverage temp rain tree_cover_km2 ln_duration ln_exp_idx, ///
	*	a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
	
	rangestat (sum) n_trips, interval(year_month 0 0) by(state_code_2011) excludeself
	ren n_trips_sum n_trips_sj
	rangestat (sum) n_trips, interval(year_month 0 0) excludeself
	ren n_trips_sum n_trips_nj
	
	eststo: reghdfe duration dist_f_cum_km2 dist_nf_cum_km2 ///
		temp rain tree_cover_km2 ln_exp_idx, ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
	
		estadd local agg "District-Time" 
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
		
	eststo: reghdfe n_trips_nj dist_f_cum_km2 dist_nf_cum_km2 ///
		temp rain tree_cover_km2 ln_duration ln_exp_idx, ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
		
		estadd local agg "District-Time" 
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
	// Note: project DECREASES trips in other districts... interpret
	
	* District: Does project displace users to prettier districts?
	xtile pretty = sr_bl, nquantiles(3)
	
	// Ugly districts w/n state
	clonevar n_trips_1 = n_trips
	replace n_trips_1 = 0 if pretty == 2 | pretty == 3
	rangestat (sum) n_trips_1, interval(year_month 0 0) by(state_code_2011_num) excludeself
	ren n_trips_1_sum n_trips_sj_1
	
	// Medium pretty districts w/n state
	clonevar n_trips_2 = n_trips
	replace n_trips_2 = 0 if pretty == 1 | pretty == 3
	rangestat (sum) n_trips_2, interval(year_month 0 0) by(state_code_2011_num) excludeself
	ren n_trips_2_sum n_trips_sj_2
	
	clonevar n_trips_3 = n_trips
	replace n_trips_3 = 0 if pretty == 1 | pretty == 2
	rangestat (sum) n_trips_3, interval(year_month 0 0) by(state_code_2011_num) excludeself
	ren n_trips_3_sum n_trips_sj_3
	
	foreach v of varlist n_trips_sj_1 n_trips_sj_2 n_trips_sj_3 {
		
		eststo: reghdfe `v' dist_f_cum_km2 dist_nf_cum_km2 ///
			temp rain tree_cover_km2 ln_duration ln_exp_idx, ///
			a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
		
			estadd local agg "District-Time" 
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"	
	}
	// Note: fragmentation increases number of trips in medium biodiverse districts j in the state
	
	// CELL LEVEL: Do users find other locations w/n the district?
	
	* User-cell-month
	use "${DATA}/dta/fc_ebd_user_cell_v01", clear
	
	* Aggregate to user-district-month
	bys c_code_2011_num: egen n_cell_dist = nvals(cell)
	collapse (count) n_cells = cell (mean) exp_idx duration distance ///
			 (firstnm) dist_*_cum_km2 coverage tree_cover_* temp n_cell_dist ///
					   tot_area rain biome sr_bl *_code_2011_num month year, ///
		     by(user_id c_code_2011 year_month)
			 
	g cell_area = (n_cells / n_cell_dist)*100
	g ln_duration = log(duration)
	g ln_exp_idx = log(exp_idx)
	lab_vars
	
	eststo: reghdfe cell_area dist_f_cum_km2 dist_nf_cum_km2 ///
		temp rain tree_cover_km2 ln_duration ln_exp_idx, ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
		
			estadd local agg "User-District-Time" 
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"	
	// note: fragmentation increases the number of within-district locations covered
	
	* Tabulate
	esttab using "${TABLE}/tables/mech_search.tex", replace ///
		stats(agg dist_fe st_m_fe year_fe N r2, labels(`"Data Aggregation"' ///
		`"District FEs"' `"State x Month FEs"' `"Year FEs"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 3)) mgroups("Cross-District" "Cross-District (By Prettiness)" ///
		"Within-District", pattern(1 0 0 0 1 0 0 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span ///
		erepeat(\cmidrule(lr){@span})) mlabels("In State" "National" ///
		"In State" "National" "Low" "Medium" "High" "Coverage (\%)") ///
		wrap nocons nonotes booktabs nomtitles star(* .1 ** .05 *** .01) ///
		label se b(%5.3f) width(\hsize)
	eststo clear
	*/
	**# 3. Fixing Location
	*-------------------------------------------------------
	use "${DATA}/dta/fc_ebd_user_v02", clear
	
	* Main (For Reference)
	eststo: reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls', ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
		
		estadd local agg "User-District-Month"
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		
	* Users who stay in one district
	eststo: reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' if n_dist_user == 1, ///
		a(user_id#year state_code_2011_num#month) vce(cl biome)
	
	
	// Note: Negative effect. But, may be driven by users being driven to new 
	// locations following construction. 
	
	* District FE
	eststo: reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls', ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
		
		estadd local agg "User-District-Month"
		estadd local year_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
	//Note: partly removes search effect, but pools together heterogeneous users
	// and increases sd. Effect may be driven by new locations w/n district
	
	* Cell Fixed Effect
	use "${DATA}/dta/fc_ebd_user_cell_v01", clear
	eststo: reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls', ///
		a(cell state_code_2011_num#month year) vce(cl biome)
		
		estadd local agg "User-Cell-Month"
		estadd local cell_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
	//Note: Nearly the same as the district FE. Pools user
	
	* Is location-FE result driven by increasing variance of user types?
	use "${DATA}/dta/fc_ebd_user_v01", clear
	
	* Aggregate to District
	collapse (sd) exp_idx_sd=exp_idx (mean) duration distance ///
			 (firstnm) dist_*_cum_km2 coverage tree_cover_* temp ///
					   tot_area rain biome sr_bl *_code_2011_num month year, ///
		     by(c_code_2011 year_month)
	egen exp_idx_var = std(exp_idx_sd ^ 2)
	g ln_duration = log(duration)
	lab_vars
	
	eststo: reghdfe exp_idx_var dist_f_cum_km2 dist_nf_cum_km2 ///
		coverage temp rain tree_cover_km2 ln_duration, ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
		
		estadd local agg "District-Month"
		estadd local dist_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
	// Note: Fragmentation in district changes variance of user types in district
	// Explains high std error in main regression w location FE. This is why we need user FE
	
	* Tabulate
	esttab using "${TABLE}/tables/mech_search_main.tex", replace ///
		stats(agg user_y_fe cell_fe dist_fe st_m_fe year_fe N r2, ///
		labels(`"Data Aggregation"' `"User x Year FE"' `"Cell FEs"' ///
		`"District FEs"' `"State x Month FEs"' `"Year FEs"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 0 3)) mlabels("Species Richness" "Species Richness" ///
		"Species Richness" "Var(Experience)") wrap nocons nonotes booktabs ///
		nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) width(\hsize)
	eststo clear
	
	**# Heterogeneity
	use "${DATA}/dta/fc_ebd_user_v01", clear
	foreach v of varlist sr sh_index {
	
	* User-Year
	use "${DATA}/dta/fc_ebd_user_v01", clear
	eststo: reghdfe `v' dist_f_*_cum_km2 dist_nf_*_cum_km2 `ctrls', ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
		
		estadd local agg "User-Dist-Month"
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
	
	* District FE
	eststo: reghdfe `v' dist_f_*_cum_km2 dist_nf_*_cum_km2 `ctrls', ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
		
		estadd local agg "User-Dist-Month"
		estadd local year_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
	
	* Cell Fixed Effect
	use "${DATA}/dta/fc_ebd_user_cell_v01", clear
	eststo: reghdfe `v' dist_f_*_cum_km2 dist_nf_*_cum_km2 `ctrls', ///
		a(cell state_code_2011_num#month year) vce(cl biome)
		
		estadd local agg "User-Cell-Month"
		estadd local cell_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
	}
	esttab using "${TABLE}/tables/mech_het_main.tex", replace keep(dist_f_*) ///
		stats(agg user_y_fe cell_fe dist_fe st_m_fe year_fe N r2, ///
		labels(`"Data Aggregation"' `"User x Year FEs"' `"Cell FEs"' ///
		`"District FEs"' `"State x Month FEs"'`"Year FEs"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 0 3))  mgroups("Species Richness" "Shannon Index", ///
		pattern(1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span ///
		erepeat(\cmidrule(lr){@span})) wrap nocons nonotes booktabs nomtitles ///
		star(* .1 ** .05 *** .01) label se b(%5.3f) width(\hsize)
	eststo clear
	*/
	*---------------------
	* BEHAVIOUR
	*---------------------
	
	**# 1. Do Users spend more/less time birding after project?
	use "${DATA}/dta/fc_ebd_user_v01", clear
	
	// User FE
	eststo: reghdfe duration dist_f_cum_km2 dist_nf_cum_km2 ///
		temp rain tree_cover_km2 ln_exp_idx, ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
		
		estadd local agg "User-Dist-Month"
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		
	eststo: reghdfe duration dist_f_cum_km2 dist_nf_cum_km2 ///
		coverage temp rain tree_cover_km2 ln_exp_idx, ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
		
		estadd local agg "User-Dist-Month"
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
	// note: no change in duration across differentially fragmented districts
	
	* District FE (do with and without access to test for access effect)
	eststo: reghdfe duration dist_f_cum_km2 dist_nf_cum_km2 ///
		temp rain tree_cover_km2 ln_exp_idx, ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
		
		estadd local agg "User-Dist-Month"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
		
	eststo: reghdfe duration dist_f_cum_km2 dist_nf_cum_km2 ///
		coverage temp rain tree_cover_km2 ln_exp_idx, ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
	
		estadd local agg "User-District-Month"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
	// note: duration decreases in fragmented districts
	
	// District Level
	
	* Aggregate To District
	collapse (mean) exp_idx duration distance ///
			 (firstnm) dist_*_cum_km2 coverage tree_cover_* temp ///
					   tot_area rain biome sr_bl *_code_2011_num month year, ///
		     by(c_code_2011 year_month)
	g ln_duration = log(duration)
	g ln_exp_idx = log(exp_idx)
	lab_vars
	
	* Does fragmentation increase mean duration in district?
	eststo: reghdfe duration dist_f_cum_km2 dist_nf_cum_km2 ///
		temp rain tree_cover_km2 ln_exp_idx, ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
		
		estadd local agg "Dist-Month"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
	
	eststo: reghdfe duration dist_f_cum_km2 dist_nf_cum_km2 ///
		coverage temp rain tree_cover_km2 ln_exp_idx, ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
		
		estadd local agg "Dist-Month"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
	// note: within-district fragmentation decreases trip duration
	// effect remains after controlling for access -- then plausibly driven by species decline
	
	// Cell-level
	* Perhaps district result is bc users relocate to prettier areas and spend less time
	use "${DATA}/dta/fc_ebd_user_cell_v01", clear
	*drop_outliers
	
	eststo: reghdfe duration dist_f_cum_km2 dist_nf_cum_km2 ///
		temp rain tree_cover_km2 ln_exp_idx, ///
		a(cell state_code_2011_num#month year) vce(cl biome)
		
		estadd local agg "User-Cell-Month"
		estadd local cell_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
		
	eststo: reghdfe duration dist_f_cum_km2 dist_nf_cum_km2 ///
		coverage temp rain tree_cover_km2 ln_exp_idx, ///
		a(cell state_code_2011_num#month year) vce(cl biome)
		
		estadd local agg "User-Cell-Month"
		estadd local cell_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
	// note: controlling for search issue, duration (weakly?) declines even controlling for access
	// possibly driven by decline in species diversity
	
	esttab using "${TABLE}/tables/mech_behaviour.tex", replace ///
		stats(agg user_y_fe cell_fe dist_fe st_m_fe year_fe N r2, ///
		labels(`"Data Aggregation"' `"User x Year FEs"' `"Cell FEs"' ///
		`"District FEs"' `"State x Month FEs"' `"Year FEs"' `"N"' ///
		`"\(R^{2}\)"') fmt(0 0 0 0 0 0 0 3)) wrap nocons nonotes ///
		booktabs nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f)
	
	
	iiiii
	

	* Prep
	local ctrls temp rain tree_cover_km2 duration exp_idx
	drop_outliers
	
	**# 1. Projects per 10km2 forest area
	
	* Main
	*reg_sat sr n_patches_cum `ctrls'
	// positive effect
	
	* Main (shannon)
	*reg_sat sh_index n_patches_cum `ctrls' 
	// negative, null
	*reg_sat sh_index n_patches_cum_s `ctrls'
	// positive, significant

	* Category-wise
	*reg_sat sr n_ele_cum n_irr_cum n_min_cum n_oth_cum n_res_cum n_tra_cum `ctrls'
	// valid results -- driven by resettlement 
	
	* Category-wise (Shannon)
	*reg_sat sh_index n_ele_cum n_irr_cum n_min_cum n_oth_cum n_res_cum n_tra_cum `ctrls'
	// doesn't work

	**# 2. Drop never treated
	*bys c_code_2011: egen any_def = max(dist_f > 0 & dist_f!=.)
	*drop if any_def == 0
	
	* Main
	*reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
	
	// same effect
	*reg_sat sr dist_f_*_cum_km2 dist_nf_*_cum_km2 `ctrls'
	// driven by resettlement + other. irrigation positive

	// positive...
	* Category-wise, controlling for forest area
	*reg_sat sr n_ele_cum n_irr_cum n_min_cum n_oth_cum n_res_cum n_tra_cum `ctrls'
	// driven by irrigation + resettlement (***). transport positive
	
	**# 3. Shannon Index
	
	* Main
	*reg_sat sh_index dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
	// null
	*reg_sat sh_index dist_f_*_cum_km2 dist_nf_*_cum_km2 `ctrls'
	// too many positive (drop)
	*reg_sat sh_index dist_f_*_cum_km2_s dist_nf_*_cum_km2_s `ctrls'
	// too many positive (drop)
	
	* District-level
	collapse (sum) n_trips (mean) exp_idx ///
			 (firstnm) dist_*_cum_km2 coverage tree_cover_km2 ///
					   temp rain duration distance biome ///
					   state_code_2011* c_code_2011_num month year, ///
		     by(c_code_2011 year_month)
	
	* TWFE
	// number of trips (positive)
	reghdfe coverage dist_f_cum_km2 dist_nf_cum_km2 `ctrls', ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome) 
	jj
	// experience (null)	
	*reghdfe exp_idx dist_f_cum_km2 dist_nf_cum_km2 `ctrls', ///
	*	a(c_code_2011_num state_code_2011_num#month year) vce(cl biome) 
	// duration (negative)
	reghdfe duration dist_f_cum_km2 dist_nf_cum_km2 ///
		coverage temp rain all_species tree_cover_km2 exp_idx, ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome) 	
	
	* by projects
	reghdfe duration dist_f_*_cum_km2 dist_nf_*_cum_km2 ///
		coverage temp rain all_species tree_cover_km2 , ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome) 
	
	* TO DO
	* 1. twfe with n_trips or other measure of activity
	* 2. twfe by project, add average experience. std dev?
	* 3. remove duration from main spec- bad control?
	* 4. use distance as outcome, by project
	* 5. drop mine and other from forest data. see effect of remaining
	
}

*===============================================================================
* DYNAMICS
*===============================================================================
if `dynamics' == 1 {
	
	* Read
	use "${DATA}/dta/fc_ebd_user_update2020", clear
	drop_outliers

	* Controls
	local ctrls coverage tree_cover temp rain all_species duration
	
	* Lags/Leads
	foreach var of varlist dist_f_cum_km2 dist_nf_cum_km2 {
		foreach i of numlist 1/6 {
		
			bys c_code_2011 (year_month): gen `var'_lag`i' = `var'[_n - `i']
			la var `var'_lag`i' "Lag `i'"
			
			bys c_code_2011 (year_month): gen `var'_lead`i' = `var'[_n + `i']
			la var `var'_lag`i' "Lead `i'"
			
		}
	}
	/
	bys c_code_2011: egen any_def = max(dist_f > 0 & dist_f!=.)
	
	* Show Serial Correlation
	/* ----------------------------------------
	Since deforestation is seriall correlation
	individual lags are no well identified.
	Sum of immediate + first K lags is better
	identified so I use a cumulative lag model
	------------------------------------------*/
	preserve
		bys c_code_2011_num year_month: keep if _n == 1
		xtset c_code_2011_num year_month
		reg dist_f_cum_km2 year_month
		predict r, resid
		corr r L.r
		corr dist_f_cum_km2 L5.dist_f_cum_km2
	restore

	*--------------------
	* 1. Lagged Analysis - ONLY CUMULATIVE LAG?
	*--------------------
	eststo clear
	eststo: reghdfe sr dist_f_cum_km2 dist_f_cum_km2_lag* dist_f_cum_km2_lead* dist_nf_cum_km2  `ctrls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) ///
		vce(cl biome)
	
	*----------------------------
	* 2. Cumulative Dynamic Lag
	*----------------------------
	
	// No Lag
	la var dist_f_cum_km2 "No Lag"
	reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' 
	eststo nolag: lincomest dist_f_cum_km2
	
	// 1 month
	reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 ///
		dist_f_cum_km2_lag1 dist_nf_cum_km2_lag1 `ctrls'
	eststo lag_1: lincomest dist_f_cum_km2_lag1 + dist_f_cum_km2 
	
	// 3 month
	reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 *_lag1 *_lag2 *_lag3 `ctrls'
	eststo lag_3: lincomest dist_f_cum_km2_lag3 + dist_f_cum_km2_lag2 + dist_f_cum_km2_lag1 + dist_f_cum_km2 
	
	// 5 month
	reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 *_lag1 *_lag2 *_lag3 *_lag4 *_lag5 `ctrls'
	eststo lag_5: lincomest dist_f_cum_km2_lag5 + dist_f_cum_km2_lag4 + dist_f_cum_km2_lag3 + dist_f_cum_km2_lag2 + dist_f_cum_km2_lag1 + dist_f_cum_km2
	
	coefplot (nolag, rename((1) = "Baseline") \ lag_1, rename((1) = "Sum L0-L1") \ ///
		lag_3, rename((1) = "Sum L0-L3") \ lag_5, rename((1) = "Sum L0-L5")), ///
		yline(0, lcolor(maroon) lpattern(solid)) msymbol(D) msize(small) ///
		mfcolor(white) ciopts(recast(rcap) lcolor(gs5) lpattern(shortdash)) ///
		coeflabels(, labsize(large)) mlabel format(%9.2g) mlcolor(black) ///
		mlabposition(3) mlabgap(*2) mlabsize(medium) mlabcolor(black) ///
		xlabel(, labsize(medium)) vertical
	graph export "${TABLE}/fig/temp_spillovers.png", width(1000) replace
	eststo clear
		
}
	

*===============================================================================
* SIMULATION
*===============================================================================
if `simulation' == 1 {

	*----------------------------------------------------
	* BACKLOG BETWEEN 1975-2018
	*----------------------------------------------------
	/*
	* Read post-2014 records
	import delimited "${DATA}/csv/fc_records.csv", clear
	keep proposal_status category area_applied state_name
	replace proposal_status = lower(proposal_status)
	keep if !inlist(proposal_status, "approved", "closed", "rejected", "rejected by rec", "withdrawn")
	drop if area_applied > 1000 // 22 mega projects	
	sum area_applied, d
	local total_pre = r(sum)
	
	* Read pre-2014 records
	import delimited "${DATA}/csv/fc_records_pre2014.csv", clear
	keep proposal_status category area_applied state_name
	keep if proposal_status == "APPROVED"
	*keep if !inlist(proposal_status, "APPROVED", "REJECTED", "RETURNED", "REVOKED", "CLOSED", "WITHDRAWN")
	drop if area_applied > 1000 // 82 mega projects
	sum area_applied, d
	local total_post = r(sum)
	di `total_post'
	local area_total = (`total_post' + `total_pre')/100 // 3817 km2 under review from 1975-2018
	di `area_total'
	*/
	
	*----------------------------------------------------
	* DECOMPOSED EFFECTS BY PROJECT NUMBER
	*----------------------------------------------------
	
	* Read
	use "${DATA}/dta/fc_ebd_user", clear

	* Prep
	local ctrls coverage tree_cover temp rain all_species duration
	drop_outliers
	
	* Fragmentation Wise
	g bin_0 = (dist_f_cum_km2 == 0)
	g bin_2 = (dist_f_cum_km2 > 0 & dist_f_cum_km2 <=2)
	g bin_4 = (dist_f_cum_km2 > 2 & dist_f_cum_km2 <=4)
	g bin_6 = (dist_f_cum_km2 > 4 & dist_f_cum_km2 <=6)
	g bin_8 = (dist_f_cum_km2 > 6 & dist_f_cum_km2 <=8)
	g bin_max = (dist_f_cum_km2 > 8)
	
	local labels `" "0" "(0-2]" "(2-4]" "(4-6]" "(6-8]" "8+" "'
	local i=1
	foreach x in 0 2 4 6 8 max {
		local lab: word `i' of `labels'
		la var bin_`x' "`lab'"
		local ++i
	}
	
	eststo clear
	eststo: reghdfe sr bin_2-bin_max dist_nf_km2 dist_f_*_s `ctrls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
		
	coefplot, keep(bin_*) yline(0, lcolor(maroon) lpattern(solid)) ///
		msize(small) mfcolor(white) msymbol(D) mlabcolor(black) ///
		ciopts(recast(rcap) lpattern(shortdash) lcolor(gs5)) ///
		mlabel xtitle("Cuml. Patch Area (km{sup:2})", size(medium)) ///
		ytitle("Species Richness") format(%9.2g) mlabposition(4) ///
		coeflabels(, labsize(medium)) mlabsize(medium) mcolor(black) ///
		ylabel(-12(3)3, labsize(medium)) xlabel(, labsize(medium)) ///
		mlabgap(*2) vertical
	graph export "${TABLE}/fig/fragwise_sim.png", replace
	
}

*===============================================================================
* ROBUSTNESS CHECKS
*===============================================================================
if `robustness' == 1 {
		
	if `patch' == 1 {
		
		* Read
		use "${DATA}/dta/fc_ebd_user", clear
	
		* Prep
		local ctrls coverage tree_cover temp rain all_species duration
		drop_outliers
	
		* Baseline Num Patches (2016)
		preserve
			keep if year == 2016
			sort c_code_2011 year_month
			collapse (lastnm) n_patch_base = n_patches_cum, by(c_code_2011)
			tempfile temp
			save "`temp'"
		restore
		merge m:1 c_code_2011 using "`temp'", nogen
		
		// Above/Below Median
		xtile n_patch_base_m = n_patch_base, nquantiles(3)
		tab n_patch_base_m, gen(n_patch_m_)
		foreach v of varlist n_patch_m_* { // interactions
			g dist_f_`v' = dist_f_cum_km2 * `v'
		}
		la var dist_f_n_patch_m_1 "Below Median" 
		la var dist_f_n_patch_m_2 "Above Median"  

		eststo clear
		eststo: reghdfe sr dist_f_n_patch_m_1 dist_f_n_patch_m_2  ///
			dist_f_cum_km2 dist_nf_cum_km2 `ctrls' if year > 2016 [aweight=n_trips], ///
			a(user_id#year c_code_2011_num state_code_2011_num#month) ///
			vce(cl biome) nocons

		coefplot, keep(dist_f_n_patch_*) xline(0, lcolor(maroon) lpattern(solid))  ///
			msize(small) mfcolor(white) msymbol(D) mcolor(black) ///
			ciopts(recast(rcap) lpattern(shortdash) lcolor(gs5)) ///
			mlabel ytitle("Baseline Num. Patches", size(large)) format(%9.2g) ///
			mlabposition(1) coeflabels(, labsize(medium)) mlabcolor(black) ///
			mlabsize(medium)  ylabel(, labsize(medium)) mlabgap(*2) 
		graph export "${TABLE}/fig/patchwise_het.png", replace
	
	}
	
	if `others' == 1 {
		
		* Read
		use "${DATA}/dta/fc_ebd_user", clear
		drop_outliers
	
		* Controls
		local ctrls coverage tree_cover temp rain all_species duration
		la var dist_f_cum_km2 "Infrastructure (\(km^{2}\))"
		
		* 1. Stage-I projects
		la var dist_f_km2_s1 "Stage-I Approvals (\(km^{2}\))"
		eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' *_km2_s1
			estadd local u_trend "None"
			estadd local add_cont "No"
	
		* 2. User x Month FE 
		eststo: reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' n_mon_yr u_lin, ///
			a(user_id#month c_code_2011_num state_code_2011_num#year) vce(cl biome)
				estadd local user_m_fe "$\checkmark$"
				estadd local dist_fe "$\checkmark$"
				estadd local st_y_fe "$\checkmark$"
				estadd local u_trend "Linear"
				estadd local add_cont "No"
				sum `depvar' if e(sample)==1
				estadd scalar ymean = `r(mean)'
				
		* 3. Cubic Trend
		eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' u_cubic
			estadd local u_trend "Cubic"
			estadd local add_cont "No"
		
		* 4. Observer Effort
		eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' ///
			group_size distance
			estadd local u_trend "None"
			estadd local add_cont "Yes"
		
		* 5. Winsorize all projects
		preserve
			
			use "${DATA}/dta/fc_ebd_user_trunc99", clear
			drop_outliers
			
			eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
				estadd local u_trend "None"
				estadd local add_cont "No"
	
		restore
		
		* 6. IHS all projects
		preserve
			
			use "${DATA}/dta/fc_ebd_user_full", clear
			drop_outliers
			drop dist_f_cum_km2 dist_nf_cum_km2
			ren dist_f_cum_ihs dist_f_cum_km2
			ren dist_nf_cum_ihs dist_nf_cum_km2
			la var dist_f_cum_km2 "Infrastructure \(km^{2}\"
			eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
				estadd local u_trend "None"
				estadd local add_cont "No"
				
		restore
		
		* TABLE: ROBUSTNESS
		esttab using "${TABLE}/tables/robustness1.tex", replace ///
			keep(dist_f*) stats(user_m_fe user_y_fe dist_fe st_y_fe ///
			st_m_fe u_trend add_cont N r2, labels(`"User x Month FEs"' ///
			`"User x Year FEs"' `"District FEs"' ///
			`"State x Year FEs"' `"State x Month FEs"' `"User Trend"' ///
			`"Additional Controls"' `"N"' `"\(R^{2}\)"') ///
			fmt(0 0 0 0 0 0 0 0 3)) mlabel("Stage-I" "User-Month" "Cubic" ///
			"Effort" "Winsorize" "IHS") wrap nocons nonotes booktabs ///
			nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) width(\hsize)
		eststo clear
		
		* 7/8. Alternative Outcomes
		eststo: reg_sat sh_index dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
			estadd local samp "None"
			estadd local clust "Biome"
		eststo: reg_sat si_index dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
			 estadd local samp "None"
			 estadd local clust "Biome"
			 
		* 9. Only trips where all species reported
		preserve
			
			use "${DATA}/dta/fc_ebd_user_allreported", clear
			drop_outliers
			
			eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
				estadd local samp "Complete"
				estadd local clust "Biome"
				
		restore
		
		* 10. Alt Sample: Drop non-forest districts
		preserve
			bys c_code_2011: egen any_def = max(dist_f > 0 & dist_f!=.)
			drop if any_def == 0
			eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
				estadd local samp "Treated"
				estadd local clust "Biome"
		restore
		
		* 11. Alt Sample: Drop sparse eBird districts
		preserve
			egen pc25 = pctile(n_trips_dist), p(10)
			drop if n_trips_dist < pc25
			eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
				estadd local samp "High-Activity"
				estadd local clust "Biome"
		restore
		
		* 12. Clustering
		eststo: reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' [aweight=n_trips], ///
			a(user_id#year c_code_2011_num state_code_2011_num#month) vce(cl c_code_2011_num)
				sum sr if e(sample)==1
				estadd scalar ymean = `r(mean)'
				estadd local user_y_fe "$\checkmark$"
				estadd local dist_fe "$\checkmark$"
				estadd local st_m_fe "$\checkmark$"
				estadd local samp "None"
				estadd local clust "District"
		
		
		* 13. Wild Bootstrap
		/*
		// Partial out FEs
		reghdfe sr dist_nf_cum_km2 `ctrls' [aweight=n_trips], a(user_id#year c_code_2011_num state_code_2011_num#month) vce(cl biome) residuals(r_sr)
		reghdfe dist_f_cum_km2 dist_nf_cum_km2 `ctrls' [aweight=n_trips], a(user_id#year c_code_2011_num state_code_2011_num#month) vce(cl biome) residuals(r_treat)
		
		* Model
		local reg_wild = "reg_wild"
		*eststo `reg_wild': reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' i.c_code_2011_num i.state_code_2011_num#i.month [aweight=n_trips], ///
		*	a(user_id#year) vce(cl biome)
		eststo `reg_wild': reg r_sr r_treat [aweight=n_trips], vce(cl biome)
		
		* Run boottest to get bootstrapped clustered pvals and CIs
		local indep_vars r_treat _cons
		local n_vars 2
		local indep_hyp
		foreach var of local indep_vars {
			local indep_hyp = "`indep_hyp' {`var'}"
		}
		boottest `indep_hyp', seed(1234) boottype(wild) cluster(biome) nograph // boottest each coeff
		
		* Store bootttest output in e() matrices
		matrix boot_pval = J(1, `n_vars',.)
		matrix boot_ci_lb = J(1, `n_vars',.)
		matrix boot_ci_ub = J(1, `n_vars', .)
		forval k = 1/`n_vars' { // loop through predictors, aka matrix cols
			matrix boot_pval[1,`k'] = r(p_`k')
			matrix CI_temp = r(CI_`k')
			matrix boot_ci_lb[1,`k'] = CI_temp[1,1]
			matrix boot_ci_ub[1,`k'] = CI_temp[1,2]
		}
		foreach mat in "boot_pval" "boot_ci_lb" "boot_ci_ub" {
			matrix colnames `mat' = `indep_vars'
			estadd matrix `mat' = `mat': `reg_wild'
		}
		esttab `reg_wild' using "${TABLE}/tables/wild.tex", replace ///
			keep(r_treat) cells(b boot_pval(fmt(2) par) ///
			boot_ci_lb(fmt(a3) par(`"["' `","')) ///
			& boot_ci_ub(fmt(a3) par(`""' `"]"')))
		*/
		
		* TABLE: ROBUSTNESS (Continued)
		esttab using "${TABLE}/tables/robustness2.tex", replace ///
			keep(dist_f*) stats(ymean user_y_fe dist_fe st_m_fe samp clust N r2, ///
			labels(`"Outcome Mean"' `"User x Year FEs"' `"District FEs"' ///
			`"State x Month FEs"' `"Sample Restriction"' `"Clustering"' `"N"' ///
			`"\(R^{2}\)"') fmt(3 0 0 0 0 0 0 3)) mlabel("Shannon" "Simpson" ///
			"Complete" "Treated" "Active" "Cluster") wrap nocons nonotes ///
			booktabs nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) ///
			width(\hsize)
		eststo clear

	}
	kk
	*/
	* Spatial Spillovers
	la var dist_f_cum_km2_slx_bc "Other-district Infrastructure" 
	eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' *_slx_bc
		estadd local cutoff "N/A"
	drop dist_f_cum_km2_slx_bc
	ren dist_f_cum_km2_slx_i_0 dist_f_cum_km2_slx_bc
	la var dist_f_cum_km2_slx_bc "Other-district Infrastructure"
	eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' *_slx_bc
		estadd local cutoff "None"
	drop dist_f_cum_km2_slx_bc
	ren dist_f_cum_km2_slx_i_100 dist_f_cum_km2_slx_bc
	la var dist_f_cum_km2_slx_bc "Other-district Infrastructure"
	eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' *_slx_bc
		estadd local cutoff "100km"
	drop dist_f_cum_km2_slx_bc
	ren dist_f_cum_km2_slx_i_200 dist_f_cum_km2_slx_bc
	la var dist_f_cum_km2_slx_bc "Other-district Infrastructure"
	eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' *_slx_bc
		estadd local cutoff "200km"
	drop dist_f_cum_km2_slx_bc
	ren dist_f_cum_km2_slx_i_500 dist_f_cum_km2_slx_bc
	la var dist_f_cum_km2_slx_bc "Other-district Infrastructure"
	eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' *_slx_bc
		estadd local cutoff "500km"
	esttab using "${TABLE}/tables/spatial_spillovers.tex", replace ///
		keep(dist_f_cum*) stats(cutoff user_y_fe dist_fe st_m_fe N r2, ///
		labels(`"Distance Cutoff"' `"User x Year FEs"' `"District FEs"' ///
		`"State x Month FEs"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 3)) ///
		mgroups("Neighbours" "Inverse Distance", pattern(1 1 0 0 0) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span ///
		erepeat(\cmidrule(lr){@span})) wrap nocons nonotes booktabs ///
		nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) width(\hsize)
	eststo clear
	
}



