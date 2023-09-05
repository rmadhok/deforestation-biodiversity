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
cap log close
set more off
set scheme modern

//Set Directory Paths
gl ROOT 	"/Users/rmadhok/Dropbox/def_biodiv"
gl DATA 	"${ROOT}/data"
gl TABLE	"${ROOT}/docs/jmp/tex_doc/v4"
cd "${TABLE}"

// Modules
local main_analysis		0
local dynamics			0
local robustness		0
	local mobility		0
	local sensitivity	0
	local slx			0

*===============================================================================
* DATA PREPARATION
*===============================================================================

* Regression Program
capture program drop reg_sat
program define reg_sat
	
	syntax varlist(fv) [if]
	set more off
	
	local depvar : word 1 of `varlist'
	local indepvar : word 2 of `varlist'
	local controls = substr("`varlist'", length("`depvar'") + ///
		length("`indepvar'") + 3, length("`varlist'"))

	reghdfe `depvar' `indepvar' `controls' `if', ///
		a(uid#year c_code_2011_num state_code_2011_num#month, savefe) ///
		vce(cl biome) resid
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		sum `depvar' if e(sample)==1
		estadd scalar ymean = `r(mean)'
	
end

* Drop Outliers
capture program drop drop_outliers
	program define drop_outliers

	egen n_trips_pc99 = pctile(n_trips), p(99)
	gen outlier = (n_trips > n_trips_pc99)
	drop if outlier == 1

end

* Load default data
use "${DATA}/dta/fc_ebd_udt_v02", clear
drop if year == 2014
drop_outliers

* Prep
local ctrls temp rain ln_duration ln_distance ln_exp_idx ///
	ln_coverage_udym ln_group_size traveling ln_hour ln_rad_sum
	
g dist_f_pub_cum_km2 = dist_f_cen_cum_km2 + dist_f_sta_cum_km2
replace dist_f_nei_cum_km2 = dist_f_nei_cum_km2 + dist_f_joi_cum_km2
replace dist_f_non_cum_km2 = dist_f_non_cum_km2 + dist_f_hyb_cum_km2
drop dist_f_hyb* dist_f_sta* dist_f_cen* dist_f_joi*
la var dist_f_pub_cum_km2 "Public"
	
foreach v of local ctrls { // missing flags
	
		g `v'_flag = (`v' == .)
		qui sum `v', d
		replace `v' = r(p50) if `v'_flag
}
*===============================================================================
* MAIN ANALYSIS
*===============================================================================
if `main_analysis' == 1 {

	*-----------------------------
	* 1. MAIN RESULTS
	*-----------------------------
	eststo clear
	eststo m1: reghdfe sr dist_f_cum_km2 `ctrls' *_flag, ///
		a(uid c_code_2011_num state_code_2011_num#month year) vce(cl biome)
	
		sum sr if e(sample)==1
		estadd scalar ymean = `r(mean)'
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"

	eststo m2: reghdfe sr dist_f_cum_km2 `ctrls' *_flag, ///
		a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl biome)

		sum sr if e(sample)==1
		estadd scalar ymean = `r(mean)'
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
	
	la var dist_f_cum_km2_slx_ib "Infrastructure (district j $\neq$ d)"
	eststo m3: reghdfe sr dist_f_cum_km2 dist_f_cum_km2_slx_ib `ctrls' *_flag, ///
		a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl biome)

		sum sr if e(sample)==1
		estadd scalar ymean = `r(mean)'
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"

	* Plot
	coefplot (m1, rename(dist_f_cum_km2 = `" "(1) User FE""') \ m2, ///
		rename(dist_f_cum_km2 = `" "(2) User-Year FE" "') \ m3, ///
		rename(dist_f_cum_km2 = `" "(3) SLX" "')), ///
		keep(dist_f_cum_km2) xline(0, lcolor(black*0.8) lpattern(dash)) ///
		coeflabels(, labsize(large)) mlabsize(med) mlabcolor(black) ///
		levels(95 90) ciopts(recast(rcap) lwidth(*1 *3) ///
		color(dkgreen dkgreen) lcolor(*.4 *.7)) ///
		legend(order(1 "95" 2 "90") size(med)) msize(med) ///
		mfcolor(white) msymbol(D) mlcolor(black) mlabel format(%9.2g) ///
		mlabposition(12) mlabgap(*2) xlabel(-0.3(.1)0.1, labsize(large)) ///
		ylabel(, labsize(large)) title("A", size(large)) xsize(4.6)
	graph export "${TABLE}/fig/main_results.png", replace
	
	*-----------------------------
	* 2. MAIN RESULTS (SENSITIVITY)
	*-----------------------------
	
	**# User-Year Fixed Effects
	
	* No controls
	eststo: reg_sat sr dist_f_cum_km2 temp rain temp_flag rain_flag
	
	* Behaviour controls
	eststo: reg_sat sr dist_f_cum_km2 temp rain ln_duration ln_distance ///
	    ln_exp_idx ln_coverage_udym ln_group_size traveling ln_hour ///
		temp_flag rain_flag ln_duration_flag ln_distance_flag ln_exp_idx_flag ln_coverage_udym_flag ln_group_size_flag traveling_flag ln_hour_flag // missing flags
	
	* Non-forest land diversion
	la var dist_nf_cum_km2 "Non-forest Land Diversion (\(km^{2}\))"
	eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' *_flag
	
	* Table (appendix)
	esttab using "${TABLE}/tables/main_results.tex", keep(dist_*_cum_km2*) replace ///
		stats(ymean user_fe user_y_fe dist_fe st_m_fe year_fe N r2, ///
		labels(`"Outcome Mean"' `"User FEs"' `"User $\times$ Year FEs"' ///
		`"District FEs"'  `"State $\times$ Month FEs"' `"Year FEs"' `"N"' ///
		`"\(R^{2}\)"') fmt(3 0 0 0 0 0 0 3)) mgroups("Main Estimates" ///
		"Sensitivity", pattern(1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) ///
		suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		indicate("Weather Controls=temp" "Behaviour Controls=ln_duration" ///
		"General Economic Trends=ln_rad_sum") wrap nocons nonotes booktabs nomtitles ///
	star(* .1 ** .05 *** .01) label se b(%5.3f) width(1\hsize) varwidth(40)
	
	**# User Fixed Effects
	
	* No controls
	eststo clear
	eststo: reghdfe sr dist_f_cum_km2 temp rain temp_flag rain_flag, ///
		a(uid c_code_2011_num state_code_2011_num#month year) vce(cl biome)
	
		sum sr if e(sample)==1
		estadd scalar ymean = `r(mean)'
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"

	* Behaviour controls
	eststo: reghdfe sr dist_f_cum_km2 temp rain ln_duration ln_distance ///
	    ln_exp_idx ln_coverage_udym ln_group_size traveling ln_hour ///
		temp_flag rain_flag ln_duration_flag ln_distance_flag ln_exp_idx_flag ln_coverage_udym_flag ln_group_size_flag traveling_flag ln_hour_flag, ///
		a(uid c_code_2011_num state_code_2011_num#month year) vce(cl biome)
	
		sum sr if e(sample)==1
		estadd scalar ymean = `r(mean)'
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
	
	* Economic Trends
	eststo: reghdfe sr dist_f_cum_km2 `ctrls' *_flag, ///
		a(uid c_code_2011_num state_code_2011_num#month year) vce(cl biome)
	
		sum sr if e(sample)==1
		estadd scalar ymean = `r(mean)'
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
		
	* Non-forest land diversion
	la var dist_nf_cum_km2 "Non-forest Land Diversion (\(km^{2}\))"
	eststo: reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' *_flag, ///
		a(uid c_code_2011_num state_code_2011_num#month year) vce(cl biome)
	
		sum sr if e(sample)==1
		estadd scalar ymean = `r(mean)'
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
	
	* Spatial lag
	la var dist_f_cum_km2_slx_ib "Infrastructure (district j $\neq$ d)"
	eststo: reghdfe sr dist_f_cum_km2 dist_f_cum_km2_slx_ib `ctrls' *_flag, ///
		a(uid c_code_2011_num state_code_2011_num#month year) vce(cl biome)
	
		sum sr if e(sample)==1
		estadd scalar ymean = `r(mean)'
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
	
	* Table
	esttab using "${TABLE}/tables/main_results_ufe.tex", keep(dist_*_cum_km2*) replace ///
		stats(ymean user_fe dist_fe st_m_fe year_fe N r2, ///
		labels(`"Outcome Mean"' `"User FEs"' `"District FEs"' ///
		`"State $\times$ Month FEs"' `"Year FEs"' `"N"' `"\(R^{2}\)"') ///
		fmt(3 0 0 0 0 0 3)) indicate("Weather Controls=temp" "Behaviour Controls=ln_duration" ///
		"General Economic Trends=ln_rad_sum") wrap nocons nonotes booktabs nomtitles ///
		star(* .1 ** .05 *** .01) label se b(%5.3f) width(1\hsize) varwidth(40)
	
	*--------------------------
	* 3. DECOMPOSED ESTIMATES
	*--------------------------
	
	**# Decomposed by Project Category
	
	* Only Weather Controls
	eststo clear
	eststo m1: reg_sat sr dist_f_ele_cum_km2-dist_f_tra_cum_km2 temp rain temp_flag rain_flag
	estadd local data "Full"
	* Weather+Behaviour Controls
	eststo m2: reg_sat sr dist_f_ele_cum_km2-dist_f_tra_cum_km2 temp rain ln_duration ln_distance ln_exp_idx ln_coverage_udym ln_group_size traveling ln_hour temp_flag rain_flag ln_duration_flag ln_distance_flag ln_exp_idx_flag ln_coverage_udym_flag ln_group_size_flag traveling_flag ln_hour_flag // missing flags
	estadd local data "Full"
	* All Controls
	eststo m3: reg_sat sr dist_f_ele_cum_km2-dist_f_tra_cum_km2 `ctrls' *_flag
	estadd local data "Full" 
	
	* Plot main specification
	coefplot m3, keep(dist_f_*_cum_km2) sort xline(0, lcolor(black*0.8) lpattern(dash)) ///
		msize(med) mfcolor(white) msymbol(D) levels(95 90) ///
		ciopts(recast(rcap) lwidth(*1 *3) color(dkgreen dkgreen) ///
		lcolor(*.4 *.7)) legend(order(1 "95" 2 "90") size(med)) ///
		mlabel format(%9.2g) mlabposition(1) mlabcolor(black) /// 
		coeflabels(, labsize(large)) mlabsize(med) mcolor(black) ///
		xlabel(, labsize(large)) ylabel(, labsize(large)) mlabgap(*2) ///
		title("B", size(large)) xsize(4.6)
	graph export "${TABLE}/fig/decomposed_category.png", replace

	* Decomposed: controlling for eBird sparsity
	bys c_code_2011: egen mine = max(dist_f_min_cum_km2 > 0 & dist_f_min_cum_km2!=.)
	egen tag_d = tag(c_code_2011)
	tab mine if tag_d // N=76 districts with mines
	egen tag_u = tag(uid)
	sum n_trips_user if tag_u, d
	g n_trips_user_med = r(p50) // median number of trips/user
	sum n_users_dist if tag_d, d 
	g n_users_dist_med = r(p50) // median number of users/district
	g subsample = ( (n_users_dist > n_users_dist_med) & (n_trips_user > n_trips_user_med) ) // high-activity districts and users
	tab mine if tag_d == 1 & subsample == 1
	
	eststo m4: reg_sat sr dist_f_ele_cum_km2-dist_f_tra_cum_km2 `ctrls' *_flag if subsample == 1
	estadd local data "High-Activity"
	
	* Plot decomposed (High activity) - for presentation
	coefplot m4, keep(dist_f_*_cum_km2) sort xline(0, lcolor(black*0.8) lpattern(dash)) ///
		msize(med) mfcolor(white) msymbol(D) levels(95 90) ///
		ciopts(recast(rcap) lwidth(*1 *3) color(dkgreen dkgreen) ///
		lcolor(*.4 *.7)) legend(order(1 "95" 2 "90") size(med)) ///
		mlabel format(%9.2g) mlabposition(1) mlabcolor(black) /// 
		coeflabels(, labsize(large)) mlabsize(med) mcolor(black) ///
		xlabel(, labsize(large)) ylabel(, labsize(large)) mlabgap(*2) ///
		title("B", size(large)) xsize(4.6)
	graph export "${TABLE}/fig/decomposed_cat_highactivity.png", replace
	
	* Full Table
	esttab using "${TABLE}/tables/decomposed_cat_tab.tex", keep(dist_*_cum_km2*) replace ///
		stats(ymean data user_y_fe dist_fe st_m_fe N r2, ///
		labels(`"Outcome Mean"' `"Sample"' `"User x Year FEs"' `"District FEs"' ///
		`"State $\times$ Month FEs"' `"N"' `"\(R^{2}\)"') ///
		fmt(3 0 0 0 0 0 3)) indicate("Weather Controls=temp" "Behaviour Controls=ln_duration" ///
		"General Economic Trends=ln_rad_sum") wrap nocons nonotes booktabs nomtitles ///
		star(* .1 ** .05 *** .01) label se b(%5.3f) width(1\hsize) varwidth(40)
	
	/*-----------------------------------------------------------------
	mining impact doubles in high-activity districts but others unchanged
	i.e. mining coeff driven by low-activity users but others are not.
	------------------------------------------------------------------*/
	
	**# Decomposed by Public/Private/Etc
	eststo clear
	eststo: reg_sat sr dist_f_pub_cum_km2 dist_f_pri_cum_km2 dist_f_nei_cum_km2 temp rain temp_flag rain_flag
	eststo: reg_sat sr dist_f_pub_cum_km2 dist_f_pri_cum_km2 dist_f_nei_cum_km2 temp rain ln_duration ln_distance ln_exp_idx ln_coverage_udym ln_group_size traveling ln_hour temp_flag rain_flag ln_duration_flag ln_distance_flag ln_exp_idx_flag ln_coverage_udym_flag ln_group_size_flag traveling_flag ln_hour_flag // missing flags
	eststo: reg_sat sr dist_f_pub_cum_km2 dist_f_pri_cum_km2 dist_f_nei_cum_km2 `ctrls' *_flag
	
	esttab using "${TABLE}/tables/decomposed_type_shape.tex", replace f ///
		keep(dist_*_cum_km2*) refcat(dist_f_pub_cum_km2 ///
		"\underline{\emph{Panel A: Ownership}}", nolabel) wrap nocons nonotes ///
		booktabs noobs nomtitles star(* .1 ** .05 *** .01) label se  ///
		b(%5.3f) width(1\hsize) varwidth(40)
	
	**# Decomposed by shape
	eststo clear
	eststo: reg_sat sr dist_f_lin_cum_km2 dist_f_non_cum_km2 temp rain temp_flag rain_flag
	eststo: reg_sat sr dist_f_lin_cum_km2 dist_f_non_cum_km2 temp rain ln_duration ln_distance ln_exp_idx ln_coverage_udym ln_group_size traveling ln_hour temp_flag rain_flag ln_duration_flag ln_distance_flag ln_exp_idx_flag ln_coverage_udym_flag ln_group_size_flag traveling_flag ln_hour_flag // missing flags
	eststo: reg_sat sr dist_f_lin_cum_km2 dist_f_non_cum_km2 `ctrls' *_flag
	
	esttab using "${TABLE}/tables/decomposed_type_shape.tex", append f ///
		keep(dist_*_cum_km2*) stats(ymean user_y_fe dist_fe st_m_fe N r2, ///
		labels(`"Outcome Mean"' `"User x Year FEs"' `"District FEs"' ///
		`"State $\times$ Month FEs"' `"N"' `"\(R^{2}\)"') fmt(3 0 0 0 0 3)) ///
		indicate("Weather Controls=temp" "Behaviour Controls=ln_duration" ///
		"General Economic Trends=ln_rad_sum") refcat(dist_f_lin_cum_km2 ///
		"\underline{\emph{Panel B: Shape}}", nolabel) wrap nocons nonotes ///
		booktabs nonumbers nomtitles noobs star(* .1 ** .05 *** .01) label se ///
		b(%5.3f) width(1\hsize) varwidth(40)

	*--------------------------
	* 4. HETEROGENEITY
	*--------------------------

	* Baseline biodiversity/treecover
	g tree_cover_base_p = (tree_cover_base/tot_area)*100 // forest cover %
	egen sr_bl_std = std(sr_bl) // standardize for comparison
	egen tree_cover_base_p_std = std(tree_cover_base_p) // standardize for comparison
	la var sr_bl_std "Baseline Species Richness"
	la var tree_cover_base_p_std "Baseline Forest Cover"

	eststo clear
	eststo: reg_sat sr c.dist_f_cum_km2##c.tree_cover_base_p_std `ctrls'
	eststo: reg_sat sr c.dist_f_cum_km2##c.tree_cover_base_p_std n_*_cum_s `ctrls'
	eststo: reg_sat sr c.dist_f_cum_km2##c.sr_bl_std `ctrls'
	eststo: reg_sat sr c.dist_f_cum_km2##c.sr_bl_std n_*_cum_s `ctrls'	

	esttab using "${TABLE}/tables/het_biodiv.tex", replace ///
		keep(dist_f_cum_km2 c.*) stats(user_y_fe dist_fe st_m_fe N r2, ///
		labels(`"User x Year FEs"' `"District FEs"' `"State $\times$ Month FEs"' ///
		`"N"' `"\(R^{2}\)"') fmt(0 0 0 0 3)) interaction(" $\times$ ") ///
		indicate("Controls=`ctrls'" "Category Shares=n_*_cum_s") wrap nocons ///
		nonotes booktabs nomtitles star(* .1 ** .05 *** .01) label se ///
		b(%5.3f) width(1\hsize) varwidth(40)
	eststo clear

}

*===============================================================================
* DYNAMICS
*===============================================================================
if `dynamics' == 1 {
	
	*----------------------------
	* Cumulative Dynamic Lag
	*----------------------------
	
	// No Lag
	la var dist_f_cum_km2 "No Lag"
	reg_sat sr dist_f_cum_km2 `ctrls' 
	eststo nolag: lincomest dist_f_cum_km2
	
	local lag_set "dist_f_cum_km2"
	forvalues i = 1/12 {

		* Lag specification
		local lag_set "dist_f_cum_km2_lag`i' `lag_set'"
		reg_sat sr `lag_set' `ctrls'
		
		* Cumulative Lag
		local lincom_eqn = subinstr("`lag_set'", " ", "+", .)
		eststo lag_`i': lincomest `lincom_eqn'
		
	}
	
	coefplot (nolag, rename((1) = "Baseline") \ lag_1, rename((1) = "Sum L0-L1") \ ///
		lag_3, rename((1) = "Sum L0-L3") \ lag_6, rename((1) = "Sum L0-L6") \ ///
		lag_12, rename((1) = "Sum L0-L12")), ///
		vertical ytitle("Cumulative Effect of Development" "on Species Richness", ///
		size(large)) yline(0, lcolor(black*0.8) lpattern(dash)) msymbol(D) msize(small) ///
		mfcolor(white) levels(95 90) ciopts(recast(rcap) lwidth(*1 *3) /// 
		color(dkgreen dkgreen) lcolor(*.4 *.7)) ylabel(, labsize(large)) ///
		legend(order(1 "95" 2 "90") size(medlarge)) coeflabels(, labsize(large)) ///
		mlabel format(%9.2g) mlcolor(black) mlabposition(1) mlabgap(*2) ///
		mlabsize(large) mlabcolor(black) xlabel(, labsize(large)) ysize(2.5)
	graph export "${TABLE}/fig/cumulative_lag.png", replace
	
}

*===============================================================================
* ROBUSTNESS CHECKS
*===============================================================================
if `robustness' == 1 {
	
	if `mobility' == 1 {

	* District level
	collapse (mean) exp_idx duration distance group_size ///
					traveling coverage_udym hour ///
			 (firstnm) dist_f_cum_* coverage_dym tree_cover_s ///
					   temp rain biome rad_* n_trips_dym n_users_dym ///
					   month year *_code_2011_num, ///
					   by(c_code_2011 year_month)
	
	* Controls
	foreach v of varlist duration exp_idx group_size n_*_dym coverage_* rad_* distance hour {
		g ln_`v' = ln(1+`v')
	}
	* Standardize
	foreach v of varlist dist_f_cum_km2 dist_f*slx* {
			egen `v'_std = std(`v')
	}
	la var dist_f_cum_km2_std "Forest Infrastructure (district $\emph{d}$)"
	
	* Extensive margin: Do projects reshuffle users to other districts?
	eststo: reghdfe ln_n_users_dym dist_f_cum_km2_std *_slx_i_100_std `ctrls', ///
			a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
			
			estadd local agg "District"
			estadd local cutoff "100km"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	
	drop dist_f_cum_km2_slx_i_100_std
	ren dist_f_cum_km2_slx_i_200_std dist_f_cum_km2_slx_i_100_std
	eststo: reghdfe ln_n_users_dym dist_f_cum_km2_std *_slx_i_100_std `ctrls', ///
			a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
			
			estadd local agg "District"
			estadd local cutoff "200km"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	
	drop dist_f_cum_km2_slx_i_100_std
	ren dist_f_cum_km2_slx_i_500_std dist_f_cum_km2_slx_i_100_std
	eststo: reghdfe ln_n_users_dym dist_f_cum_km2_std *_slx_i_100_std `ctrls', ///
			a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
			
			estadd local agg "District"
			estadd local cutoff "500km"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	
	* Tabulate
	la var dist_f_cum_km2_slx_i_100_std "Forest Infrastructure (district $\emph{j}$ $\neq$ $\emph{d}$)"
	esttab using "${TABLE}/tables/displacement.tex", replace ///
		keep(dist_f*) stats(agg cutoff dist_fe st_m_fe year_fe N r2, ///
		labels(`"Data Aggregation"' `"Distance Cutoff"' `"District FEs"' ///
		`"State $\times$ Month FEs"' `"Year FEs"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) mlabels("Users" "Users" "Users") ///
		indicate("Controls = `ctrls'") wrap nocons nonotes booktabs nomtitles ///
		star(* .1 ** .05 *** .01) label se b(%5.3f) width(1\hsize) varwidth(30)
	eststo clear
	}
	
	
	if `sensitivity' == 1 {
		
		/*------------------------------------------
		for category-wise robustness replace 
		dist_f_cum_km2 with dist_f_*_cum_km2
		-------------------------------------------*/
		
		**# 1. User x Month FE
		use "${DATA}/dta/fc_ebd_udt_v02", clear
		drop if year == 2014
		drop_outliers
		la var dist_f_cum_km2 "Forest Infrastructure"
		
		eststo: reghdfe sr dist_f_cum_km2 `ctrls' n_mon_yr, ///
			a(uid#month c_code_2011_num state_code_2011_num#year) vce(cl biome)
				
				estadd local unit "\(km^{2}\)"
				estadd local wt "None"
				estadd local samp "None"
				estadd local clust "Biome"
				estadd local user_m_fe "$\checkmark$"
				estadd local dist_fe "$\checkmark$"
				estadd local st_y_fe "$\checkmark$"
		
		**# 2. Weighted
		eststo: reghdfe sr dist_f_cum_km2 `ctrls' [aw=n_trips], ///
			a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
				
				estadd local unit "\(km^{2}\)"
				estadd local wt "Num. Trips"
				estadd local samp "None"
				estadd local clust "Biome"
				estadd local user_y_fe "$\checkmark$"
				estadd local dist_fe "$\checkmark$"
				estadd local st_m_fe "$\checkmark$"
		
		**# 3. Truncate - drop 3 largest projects
		preserve
			
			use "${DATA}/dta/fc_ebd_udt_trunc_v02", clear
			drop if year == 2014
			drop_outliers
			
			eststo: reg_sat sr dist_f_cum_km2 `ctrls'
				
				estadd local unit "\(km^{2}\)"
				estadd local wt "None"
				estadd local samp "Truncated"
				estadd local clust "Biome"
	
		restore
		
		**# 4. IHS
		preserve
			
			foreach v of varlist dist_f*cum_km2 {
				g `v'_ihs = asinh(`v')
			}
			
			drop dist_f*cum_km2
			ren *_ihs* **
	
			eststo: reg_sat sr dist_f_cum_km2 `ctrls'
				
			estadd local unit "IHS"
			estadd local wt "None"
			estadd local samp "None"
			estadd local clust "Biome"
			
		restore
		
		**# 5/6. Alternative Diversity Metrics
		preserve
			
			foreach v of varlist dist_f*cum_km2 {
				replace `v' = `v' / 100
			}
		
			eststo: reg_sat sh_index dist_f_cum_km2 `ctrls'
				
				sum sh_index if e(sample)==1
				estadd local samp "None"
				estadd local clust "Biome"
				estadd local wt "None"
				estadd local unit "\(km^{2}\)"
			
			eststo: reg_sat si_index dist_f_cum_km2 `ctrls'
				
				sum si_index if e(sample)==1
				estadd local samp "None"
				estadd local clust "Biome"
				estadd local wt "None"
				estadd local unit "\(km^{2}\)"
		
		restore
		
		**# 7. Share of Baseline Forest Cover
		preserve
		
			foreach v of varlist dist_f*cum_km2 {
				g `v'_new = (`v' / tree_cover_base)*100
			}
			drop dist_f*cum_km2
			ren *_new* **
			
			eststo: reg_sat sr dist_f_cum_km2 `ctrls'
				
				estadd local samp "None"
				estadd local clust "Biome"
				estadd local wt "None"
				estadd local unit "\% Forest"
		
		restore
		
		**# 8. Alt Sample: Drop sparse eBird districts
		eststo: reg_sat sr dist_f_cum_km2 `ctrls' if n_dist_user == 1

			estadd local samp "One District"
			estadd local clust "Biome"
			estadd local wt "None"
			estadd local unit "\(km^{2}\)"
			
		**# 9. Above median trips per user and users per district
		egen tag_d = tag(c_code_2011) 
		egen tag_u = tag(uid)
		sum n_trips_user if tag_u, d
		g n_trips_user_med = r(p50)
		sum n_users_dist if tag_d, d
		g n_users_dist_med = r(p50)
		g high_activity = ( (n_users_dist > n_users_dist_med) & (n_trips_user > n_trips_user_med) )
			
		eststo: reg_sat sr dist_f_cum_km2 `ctrls' if high_activity
				
			estadd local samp "High-Activity"
			estadd local clust "Biome"
			estadd local wt "None"
			estadd local unit "\(km^{2}\)"
		
		**# 10. Drop 2020
		eststo: reg_sat sr dist_f_cum_km2 `ctrls' if inrange(year, 2015, 2019)
				
			estadd local samp "2015-2019"
			estadd local clust "Biome"
			estadd local wt "None"
			estadd local unit "\(km^{2}\)"
		
		
		**# 11. Clustering
		eststo: reghdfe sr dist_f_cum_km2 `ctrls', ///
			a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl c_code_2011_num)
				
				estadd local user_y_fe "$\checkmark$"
				estadd local dist_fe "$\checkmark$"
				estadd local st_m_fe "$\checkmark$"
				estadd local samp "None"
				estadd local clust "District"
				estadd local wt "None"
				estadd local unit "\(km^{2}\)"
		
		**# 12. Cell FEs
		preserve
			
			use "${DATA}/dta/fc_ebd_uct_v02", clear
			drop if year == 2014
			drop_outliers
			
			* Cell Fixed Effect
			eststo: reghdfe sr dist_f_cum_km2 temp rain ln_duration ///
				ln_distance ln_hour ln_rad_sum ln_exp_idx ln_group_size traveling, ///
				a(cell state_code_2011_num#month year) vce(cl biome)
		
				estadd local cell_fe "$\checkmark$"
				estadd local year_fe "$\checkmark$"
				estadd local st_m_fe "$\checkmark$"
				estadd local samp "None"
				estadd local clust "Biome"
				estadd local wt "None"
				estadd local unit "\(km^{2}\)"
		
		restore
		
		* TABLE: ROBUSTNESS
		esttab using "${TABLE}/tables/robustness.tex", replace ///
			keep(dist_f*) stats(unit samp user_y_fe user_m_fe dist_fe cell_fe ///
			st_m_fe st_y_fe year_fe wt clust N r2, labels(`"Infrastructure Unit"' ///
			`"Sample Restriction"' `"User x Year FEs"' `"User $\times$ Month FEs"' ///
			`"District FEs"' `"Cell FEs"' `"State x Month FEs"' `"State $\times$ Year FEs"' ///
			`"Year FEs"' `"Weights"' `"Clustering"' `"N"' `"\(R^{2}\)"') ///
			fmt(0 0 0 0 0 0 0 0 0 0 0 0 3)) mlabel("SR" "SR" "SR" "SR" "Shannon" ///
			"Simpson" "SR" "SR" "SR" "SR" "SR" "SR") indicate("Controls=temp") ///
			wrap nocons nonotes booktabs nomtitles ///
			star(* .1 ** .05 *** .01) label se b(%5.3f) varwidth(30)
		eststo clear
	}

	if `slx' == 1 {
		
		* Standardize for ease of interpretation
		foreach v of varlist dist_f_cum_km2 dist_f*slx* {
			egen `v'_std = std(`v')
		}
		
		* Spatial Spillovers
		la var dist_f_cum_km2_std "Forest Infrastructure (district d)"
		la var dist_f_cum_km2_slx_i_100_std "Forest Infrastructure (district j $\neq$ d)" 
		eststo: reg_sat sr dist_f_cum_km2_std dist_f_cum_km2_slx_i_100_std `ctrls'
			estadd local cutoff "100km"
		
		drop dist_f_cum_km2_slx_i_100_std
		ren dist_f_cum_km2_slx_i_200_std dist_f_cum_km2_slx_i_100_std
		la var dist_f_cum_km2_slx_i_100_std "Forest Infrastructure (district j $\neq$ d)"
		eststo: reg_sat sr dist_f_cum_km2_std dist_f_cum_km2_slx_i_100_std `ctrls'
			estadd local cutoff "200km"
		
		drop dist_f_cum_km2_slx_i_100_std
		ren dist_f_cum_km2_slx_i_500_std dist_f_cum_km2_slx_i_100_std
		la var dist_f_cum_km2_slx_i_100_std "Forest Infrastructure (district j $\neq$ d)"
		eststo: reg_sat sr dist_f_cum_km2_std dist_f_cum_km2_slx_i_100_std `ctrls'
			estadd local cutoff "500km"
		
		drop dist_f_cum_km2_slx_i_100_std
		ren dist_f_cum_km2_slx_i_0_std dist_f_cum_km2_slx_i_100_std
		la var dist_f_cum_km2_slx_i_100_std "Forest Infrastructure (district j $\neq$ d)"
		eststo: reg_sat sr dist_f_cum_km2_std dist_f_cum_km2_slx_i_100_std `ctrls'
			estadd local cutoff "None"
		
		esttab using "${TABLE}/tables/spatial_spillovers.tex", replace ///
			keep(dist_f_cum_km2_std dist_f_cum_km2_slx_i_100_std) ///
			stats(cutoff user_y_fe dist_fe st_m_fe N r2, ///
			labels(`"Distance Cutoff"' `"User x Year FEs"' `"District FEs"' ///
			`"State x Month FEs"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 3)) ///
			wrap nocons nonotes booktabs nomtitles star(* .1 ** .05 *** .01) ///
			label se b(%5.3f) width(\hsize) varwidth(30)
		eststo clear
	}
	
}



