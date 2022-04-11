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
gl TABLE	"${ROOT}/docs/jmp/tex_doc/v3"
cd "${TABLE}"

// Modules
local main_analysis		0
local dynamics			0
local robustness		1
	local mobility		0
	local sensitivity	1
	local slx			0

*===============================================================================
* PROGRAMS
*===============================================================================

* Regression Program
capture program drop reg_sat
program define reg_sat
	
	syntax varlist(fv) [if]
	set more off
	
	local depvar : word 1 of `varlist'
	local indepvar : word 2 of `varlist'
	local ctrls = substr("`varlist'", length("`depvar'") + ///
		length("`indepvar'") + 3, length("`varlist'"))

	reghdfe `depvar' `indepvar' `ctrls' `if', ///
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
local ctrls temp rain tree_cover_s ln_duration ln_distance ln_rad_sum ///
	        ln_exp_idx ln_coverage_udym ln_group_size traveling
*===============================================================================
* MAIN ANALYSIS
*===============================================================================
if `main_analysis' == 1 {
	/*
	*--------------------
	* 1. MAIN RESULTS
	*--------------------
	eststo clear
	eststo m1: reghdfe sr dist_f_cum_km2 `ctrls', ///
		a(uid c_code_2011_num state_code_2011_num#month year) vce(cl biome)
	
		sum sr if e(sample)==1
		estadd scalar ymean = `r(mean)'
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"

	eststo m2: reghdfe sr dist_f_cum_km2 `ctrls', ///
		a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl biome)

		sum sr if e(sample)==1
		estadd scalar ymean = `r(mean)'
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"

	* Plot
	coefplot (m1, rename(dist_f_cum_km2 = `" "(1) Learning""') \ m2, ///
		rename(dist_f_cum_km2 = `" "(2) No Learning" "')), ///
		keep(dist_f*) xline(0, lcolor(black*0.8) lpattern(dash)) ///
		coeflabels(, labsize(medium)) mlabsize(medium) mlabcolor(black) ///
		levels(99 95 90) ciopts(recast(rcap) lwidth(*1 *3 *4) ///
		color(dkgreen dkgreen dkgreen) lcolor(*.3 *.5 *.8)) ///
		legend(order(1 "99" 2 "95" 3 "90")) msize(medium) mfcolor(white) ///
		msymbol(D) mlcolor(black) mlabel format(%9.2g) mlabposition(1) ///
		mlabgap(*2) xlabel(-0.3(.1)0.1, labsize(medium)) ylabel(, labsize(medium)) ///
		title("A", size(large)) saving("${TABLE}/fig/main.gph", replace) xsize(4.6)
	graph export "${TABLE}/fig/main.png", replace

	* Table
	esttab using "${TABLE}/tables/main_results.tex", keep(dist_f_cum_km2) replace ///
		stats(ymean user_fe user_y_fe dist_fe st_m_fe year_fe N r2, ///
		labels(`"Outcome Mean"' `"User FEs"' `"User x Year FEs"' ///
		`"District FEs"' `"State x Month FEs"' `"Year FEs"' `"N"' `"\(R^{2}\)"') ///
		fmt(3 0 0 0 0 0 0 3)) mgroups("With Learning Bias" ///
		"Without Learning Bias", pattern(1 1) prefix(\multicolumn{@span}{c}{) ///
		suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		indicate("Experience Index=ln_exp_idx") wrap nocons nonotes booktabs ///
		nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) width(\hsize)
	eststo clear

	*--------------------------
	* 2. DECOMPOSED ESTIMATES
	*--------------------------
	
	* Category-Wise Impacts
	eststo clear
	eststo: reg_sat sr dist_f_*_cum_km2 `ctrls'

	coefplot, keep(dist_f*) sort xline(0, lcolor(black*0.8) lpattern(dash)) ///
		msize(small) mfcolor(white) msymbol(D) levels(99 95 90) ///
		ciopts(recast(rcap) lwidth(*1 *3 *4) color(dkgreen dkgreen dkgreen) ///
		lcolor(*.3 *.5 *.8)) legend(order(1 "99" 2 "95" 3 "90")) ///
		mlabel format(%9.2g) mlabposition(1) mlabcolor(black) /// 
		coeflabels(, labsize(medium)) mlabsize(medium) mcolor(black) ///
		xlabel(, labsize(medium)) ylabel(, labsize(medium)) mlabgap(*2) ///
		title("B", size(large)) xsize(4.6) ///
		saving("${TABLE}/fig/projwise.gph", replace)
	graph export "${TABLE}/fig/projwise.png", replace
	
	* Category-wise impacts: controlling for eBird sparsity
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
	
	eststo clear
	eststo: reg_sat sr dist_f_*_cum_km2 `ctrls' if subsample == 1
	
	coefplot, keep(dist_f*) sort xline(0, lcolor(black*0.8) lpattern(dash)) ///
		msize(small) mfcolor(white) msymbol(D) levels(99 95 90) ///
		ciopts(recast(rcap) lwidth(*1 *3 *4) color(dkgreen dkgreen dkgreen) ///
		lcolor(*.3 *.5 *.8)) legend(order(1 "99" 2 "95" 3 "90")) ///
		mlabel format(%9.2g) mlabposition(1) mlabcolor(black) /// 
		coeflabels(, labsize(medium)) mlabsize(medium) mcolor(black) ///
		xlabel(, labsize(medium)) ylabel(, labsize(medium)) mlabgap(*2) xsize(4.6)
	graph export "${TABLE}/fig/projwise_highactivity.png", replace
	/*-----------------------------------------------------------------
	mining impact doubles in high-activity districts but others unchanged
	i.e. mining coeff driven by low-activity users but others are not.
	------------------------------------------------------------------*/
	*/
	*--------------------------
	* 3. HETEROGENEITY
	*--------------------------
	
	* Baseline biodiversity/tree_cover
	g tree_cover_base_p = (tree_cover_base/tot_area)*100 // forest cover %
	egen sr_bl_std = std(sr_bl) // standardize for comparison
	egen tree_cover_base_p_std = std(tree_cover_base_p) // standardize for comparison
	la var sr_bl_std "Baseline Species Diversity"
	la var tree_cover_base_p_std "Baseline Forest Cover"
	
	eststo clear
	eststo: reg_sat sr c.dist_f_cum_km2##c.sr_bl_std n_*_cum_s `ctrls'
	eststo: reg_sat sr c.dist_f_cum_km2##c.tree_cover_base_p_std n_*_cum_s `ctrls'	
	ss
	esttab using "${TABLE}/tables/het_biodiv.tex", replace ///
		keep(dist_f_cum_km2 c.*) stats(ymean user_y_fe dist_fe st_m_fe N r2, ///
		labels(`"Outcome Mean"' `"User x Year FEs"' `"District FEs"' ///
		`"State x Month FEs"' `"N"' `"\(R^{2}\)"') fmt(3 0 0 0 0 3)) ///
		interaction(" $\times$ ") wrap nocons nonotes booktabs nomtitles ///
		star(* .1 ** .05 *** .01) label se b(%5.3f) width(0.6\hsize) varwidth(40)
	eststo clear
	
	* MAIN RESULTS
	graph combine "${TABLE}/fig/main.gph" "${TABLE}/fig/projwise.gph"
	graph export "${TABLE}/fig/main_results.png", width(1000) replace

}

*===============================================================================
* DYNAMICS
*===============================================================================
if `dynamics' == 1 {
	
	*--------------------
	* Lagged Analysis - ONLY CUMULATIVE LAG?
	*--------------------
	*eststo clear
	*eststo: reg_sat sr dist_f_cum_km2 dist_f_cum_km2_lag*  `ctrls'
	
	*----------------------------
	* Cumulative Dynamic Lag
	*----------------------------
	
	// No Lag
	la var dist_f_cum_km2 "No Lag"
	reg_sat sr dist_f_cum_km2 `ctrls' 
	eststo nolag: lincomest dist_f_cum_km2
	
	// 1 month
	reg_sat sr dist_f_cum_km2 dist_f_cum_km2_lag1 `ctrls'
	eststo lag_1: lincomest dist_f_cum_km2_lag1 + dist_f_cum_km2 
	
	// 2 month
	reg_sat sr dist_f_cum_km2 *_lag1 *_lag2 `ctrls'
	eststo lag_2: lincomest dist_f_cum_km2_lag2 + dist_f_cum_km2_lag1 + dist_f_cum_km2 
	
	// 3 month
	reg_sat sr dist_f_cum_km2 *_lag1 *_lag2 *_lag3 `ctrls'
	eststo lag_3: lincomest dist_f_cum_km2_lag3 + dist_f_cum_km2_lag2 + dist_f_cum_km2_lag1 + dist_f_cum_km2 
	
	// 4 month
	reg_sat sr dist_f_cum_km2 *_lag1 *_lag2 *_lag3 *_lag4 `ctrls'
	eststo lag_4: lincomest dist_f_cum_km2_lag4 + dist_f_cum_km2_lag3 + dist_f_cum_km2_lag2 + dist_f_cum_km2_lag1 + dist_f_cum_km2 
	
	// 5 month
	reg_sat sr dist_f_cum_km2 *_lag1 *_lag2 *_lag3 *_lag4 *_lag5 `ctrls'
	eststo lag_5: lincomest dist_f_cum_km2_lag5 + dist_f_cum_km2_lag4 + dist_f_cum_km2_lag3 + dist_f_cum_km2_lag2 + dist_f_cum_km2_lag1 + dist_f_cum_km2
	
	// 6 month
	reg_sat sr dist_f_cum_km2 *_lag1 *_lag2 *_lag3 *_lag4 *_lag5 *_lag6 `ctrls'
	eststo lag_6: lincomest dist_f_cum_km2_lag6 + dist_f_cum_km2_lag5 + dist_f_cum_km2_lag4 + dist_f_cum_km2_lag3 + dist_f_cum_km2_lag2 + dist_f_cum_km2_lag1 + dist_f_cum_km2
	
	coefplot (nolag, rename((1) = "Baseline") \ lag_1, rename((1) = "Sum L0-L1") \ ///
		lag_3, rename((1) = "Sum L0-L3") \ lag_5, rename((1) = "Sum L0-L5")), ///
		xline(0, lcolor(black*0.8) lpattern(dash)) msymbol(D) msize(small) ///
		mfcolor(white) levels(99 95 90) ciopts(recast(rcap) lwidth(*1 *3 *4) /// 
		color(dkgreen dkgreen dkgreen) lcolor(*.3 *.5 *.8)) ///
		legend(order(1 "99" 2 "95" 3 "90")) coeflabels(, labsize(large)) ///
		mlabel format(%9.2g) mlcolor(black) mlabposition(1) mlabgap(*2) ///
		mlabsize(medium) mlabcolor(black) xlabel(, labsize(medium)) xsize(4.6)
	graph export "${TABLE}/fig/cumulative_lag.png", width(1000) replace
	
}

*===============================================================================
* ROBUSTNESS CHECKS
*===============================================================================
if `robustness' == 1 {
	
	if `mobility' == 1 {

	* District level
	*preserve
	collapse (mean) exp_idx duration distance group_size ///
					traveling coverage_udym ///
			 (firstnm) dist_f_cum_* coverage_dym tree_cover_s ///
					   temp rain biome rad_* n_trips_dym n_users_dym ///
					   sr_dym month year *_code_2011_num, ///
					   by(c_code_2011 year_month)
	
	* Controls
	foreach v of varlist duration exp_idx group_size n_*_dym coverage_* rad_* {
		g ln_`v' = ln(`v')
	}
	g ln_distance = asinh(distance)
	
	* Spatial lag window (500km)
	replace dist_f_cum_km2_slx_i_500 = dist_f_cum_km2_slx_i_500/10000
	replace dist_f_cum_km2 = dist_f_cum_km2 / 100
	la var dist_f_cum_km2 "Infrastructure (district $\emph{d}$)"
	la var dist_f_cum_km2_slx_i_500 "Infrastructure (district j $\neq$ d)"
	
	* Extensive margin: Do projects reshuffle users to other districts (w/n 500 km)?
	foreach v of varlist n_users_dym n_trips_dym {
	
		eststo: reghdfe ln_`v' dist_f_cum_km2 *_slx_i_500 `ctrls', ///
			a(c_code_2011_num state_code_2011_num#month year) vce(cl biome)
			
			estadd local agg "District"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	}
	*restore
	/*
	* Intensive margin: Does fragmentation displace users within district?
	g ln_n_trips = ln(n_trips)
	la var dist_f_cum_km2 "Forest Infrastructure (district $\emph{d}$)"
	la var dist_f_cum_km2_slx_i_500 "Forest Infrastructure (district j $\neq$ d)"
	local ctrls temp rain tree_cover_s ln_duration ln_distance ///
		        ln_exp_idx ln_group_size traveling 
	
	eststo: reghdfe ln_coverage_udym dist_f_cum_km2 `ctrls', ///
			a(c_code_2011_num state_code_2011_num#month year) vce(cl c_code_2011_num)
			
			estadd local agg "User"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	*/
	* Tabulate
	esttab using "${TABLE}/tables/displacement.tex", replace ///
		stats(agg dist_fe st_m_fe year_fe N, labels(`"Data Aggregation"' ///
		`"District FEs"' `"State x Month FEs"' `"Year FEs"' `"N"') ///
		fmt(0 0 0 0 0 3)) mlabels("Num. Users" "Num. Trips") ///
		indicate("Controls = `ctrls'") wrap nocons nonotes booktabs ///
		nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) ///
		width(1\hsize)
	eststo clear
	}
	
	if `sensitivity' == 1 {
		
		**# Control for general economic growth (without nightlights)
		local ctrls_alt temp rain tree_cover_s ln_duration ln_distance ln_exp_idx ln_coverage_udym ln_group_size traveling
		
		* State-Year FE
		eststo: reghdfe sr dist_f_cum_km2 `ctrls_alt', ///
			a(uid c_code_2011_num state_code_2011_num#year month) vce(cl biome)
			estadd local user_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_y_fe "$\checkmark$"
			estadd local month_fe "$\checkmark$"
			
		* State-Time Trend FE
		eststo: reg_sat sr dist_f_cum_km2 `ctrls_alt' i.state_code_2011_num#c.year_month
			
		* Nightlights
		eststo: reg_sat sr dist_f_cum_km2 `ctrls_alt' ln_rad_sum
			
		esttab using "${TABLE}/tables/robust_growth.tex", replace ///
			stats(user_fe user_y_fe dist_fe st_y_fe st_m_fe month_fe N r2, ///
			labels(`"User FEs"' `"User x Year FEs"' `"District FEs"' ///
			`"State x Year FEs"' `"State x Month FEs"' `"Month FEs"' `"N"' ///
			`"\(R^{2}\)"') fmt(0 0 0 0 0 0 0 3)) indicate("Main Controls=`ctrls_alt'" /// 
			"State $\times$ Time Trend=*c.year_month" "Nightlights=ln_rad_sum") ///
			wrap nocons nonotes booktabs nomtitles star(* .1 ** .05 *** .01) ///
			label se b(%5.3f) width(0.8\hsize) varwidth(40)
		eststo clear
		
		**# 1. Stage-I projects
		use "${DATA}/dta/fc_ebd_udt_post_v02", clear
		drop if year == 2014
		drop_outliers
		la var dist_f_cum_km2_s1 "Stage-I Approvals (\(km^{2}\))"

		eststo: reg_sat sr dist_f_cum_km2 *_s1 `ctrls'
			estadd local data "Post-2014"
			estadd local wt "No"
			estadd local samp "None"

		**# 2. User x Month FE
		use "${DATA}/dta/fc_ebd_udt_v02", clear
		drop if year == 2014
		drop_outliers
		
		eststo: reghdfe sr dist_f_cum_km2 `ctrls' n_mon_yr, ///
			a(uid#month c_code_2011_num state_code_2011_num#year) vce(cl biome)
				estadd local data "All"
				estadd local wt "No"
				estadd local samp "None"
				estadd local user_m_fe "$\checkmark$"
				estadd local dist_fe "$\checkmark$"
				estadd local st_y_fe "$\checkmark$"
		
		**# 3. Weighted
		eststo: reghdfe sr dist_f_cum_km2 `ctrls' [aw=n_trips], ///
			a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
				
				sum sr if e(sample)==1
				estadd scalar ymean = `r(mean)'
				estadd local user_y_fe "$\checkmark$"
				estadd local dist_fe "$\checkmark$"
				estadd local st_m_fe "$\checkmark$"
				estadd local data "All"
				estadd local wt "Yes"
				estadd local samp "None"
		
		**# 4. Truncate - drop 3 largest projects
		preserve
			
			use "${DATA}/dta/fc_ebd_udt_trunc_v02", clear
			drop if year == 2014
			drop_outliers
			
			eststo: reg_sat sr dist_f_cum_km2 `ctrls'
				estadd local data "All"
				estadd local wt "No"
				estadd local samp "Truncated"
	
		restore
		
		**# 5. IHS
		preserve
			
			use "${DATA}/dta/fc_ebd_udt_v02", clear
			drop if year == 2014
			drop_outliers
			g dist_f_cum_ihs = asinh(dist_f_cum_km2)
			la var dist_f_cum_ihs "IHS(Forest Infrastructure)"
			
			eststo: reg_sat sr dist_f_cum_ihs `ctrls'
				estadd local data "All"
				estadd local wt "No"
				estadd local samp "None"
				
		restore
		
		* TABLE: ROBUSTNESS
		esttab using "${TABLE}/tables/robustness1.tex", replace ///
			keep(dist_f*) stats(data samp wt user_m_fe user_y_fe dist_fe ///
			st_y_fe st_m_fe N r2, labels(`"Data"' `"Sample Restriction"' ///
			`"Weighted"' `"User x Month FEs"' `"User x Year FEs"' ///
			`"District FEs"' `"State x Year FEs"' `"State x Month FEs"' ///
			`"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 0 0 0 0 3)) wrap nocons nonotes ///
			booktabs nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) ///
			width(\hsize) varwidth(40)
		eststo clear
	
		**# 6. Simpson/Shannon Index
		preserve
			
			replace dist_f_cum_km2 = dist_f_cum_km2 / 100
			eststo: reg_sat sh_index dist_f_cum_km2 `ctrls'
				estadd local data "All"
				estadd local samp "None"
				estadd local clust "Biome"
			
			*eststo: reg_sat si_index dist_f_cum_km2 `ctrls'
			*	estadd local data "All"
			*	estadd local samp "None"
			*	estadd local clust "Biome"	 
		restore
		
		**# 7. Share of Baseline Forest Cover
		g dist_f_cum_km2_s = (dist_f_cum_km2 / tree_cover_base)*100
		la var dist_f_cum_km2_s "Forest Infrastructure (\%)"
		eststo: reg_sat sr dist_f_cum_km2_s `ctrls'
				estadd local data "All"
				estadd local samp "None"
				estadd local clust "Biome"	
		
		**# 8. Alt Sample: Drop sparse eBird districts
		preserve
			
			* Above median trips per user and users per district
			egen tag_d = tag(c_code_2011) 
			egen tag_u = tag(uid)
			sum n_trips_user if tag_u, d
			g n_trips_user_med = r(p50)
			sum n_users_dist if tag_d, d
			g n_users_dist_med = r(p50)
			g high_activity = ( (n_users_dist > n_users_dist_med) & (n_trips_user > n_trips_user_med) )
			
			eststo: reg_sat sr dist_f_cum_km2 `ctrls' if high_activity
				estadd local data "All"
				estadd local samp "High-Activity"
				estadd local clust "Biome"
		restore
		
		**# 9. Clustering
		eststo: reghdfe sr dist_f_cum_km2 `ctrls', ///
			a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl c_code_2011_num)
				estadd local user_y_fe "$\checkmark$"
				estadd local dist_fe "$\checkmark$"
				estadd local st_m_fe "$\checkmark$"
				estadd local data "All"
				estadd local samp "None"
				estadd local clust "District"
		
		**# 10. Cell FEs
		preserve
			
			use "${DATA}/dta/fc_ebd_uct_v02", clear
			drop if year == 2014
			drop_outliers
			
			* Cell Fixed Effect (remove coverage control)
			eststo: reghdfe sr dist_f_cum_km2 temp rain tree_cover_s ln_duration ///
				ln_distance ln_exp_idx ln_group_size ln_rad_sum traveling, ///
				a(cell state_code_2011_num#month year) vce(cl biome)
		
				estadd local data "All"
				estadd local samp "None"
				estadd local clust "Biome"
				estadd local cell_fe "$\checkmark$"
				estadd local year_fe "$\checkmark$"
				estadd local st_m_fe "$\checkmark$"
		
		restore
		
		* 13. Wild Bootstrap
		/*
		// Partial out FEs
		reghdfe sr `ctrls', a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl biome) residuals(r_sr)
		reghdfe dist_f_cum_km2 `ctrls', a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl biome) residuals(r_treat)
		
		* Model
		local reg_wild = "reg_wild"
		eststo `reg_wild': reg r_sr r_treat, vce(cl biome)
		
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
			keep(dist_f*) stats(data samp user_y_fe dist_fe cell_fe st_m_fe ///
			year_fe clust N r2, labels(`"Data"' `"Sample Restriction"' ///
			`"User x Year FEs"' `"District FEs"' `"Cell FEs"' ///
			`"State x Month FEs"' `"Year FEs"' `"Clustering"' `"N"' ///
			`"\(R^{2}\)"') fmt(0 0 0 0 0 0 0 0 0 3)) ///
			mlabel("Shannon" "SR" "SR" "SR" "SR") ///
			wrap nocons nonotes booktabs nomtitles star(* .1 ** .05 *** .01) ///
			label se b(%5.3f) width(\hsize) varwidth(40)
		eststo clear
	}

	if `slx' == 1 {
		
		* Standardize for ease of interpretation
		foreach v of varlist dist_f_cum_km2 dist_f*slx* {
			egen `v'_std = std(`v')
		}
		
		* Spatial Spillovers
		la var dist_f_cum_km2_std "Forest Infrastructure (district d)"
		la var dist_f_cum_km2_slx_bc_std "Forest Infrastructure (district j $\neq$ d)" 
		eststo: reg_sat sr dist_f_cum_km2_std `ctrls' *_slx_bc_std
			estadd local cutoff "N/A"
		drop dist_f_cum_km2_slx_bc_std
		ren dist_f_cum_km2_slx_i_0_std dist_f_cum_km2_slx_bc_std
		la var dist_f_cum_km2_slx_bc_std "Forest Infrastructure (district j $\neq$ d)"
		eststo: reg_sat sr dist_f_cum_km2_std `ctrls' *_slx_bc_std
			estadd local cutoff "None"
		drop dist_f_cum_km2_slx_bc_std
		ren dist_f_cum_km2_slx_i_100_std dist_f_cum_km2_slx_bc_std
		la var dist_f_cum_km2_slx_bc_std "Forest Infrastructure (district j $\neq$ d)"
		eststo: reg_sat sr dist_f_cum_km2_std `ctrls' *_slx_bc_std
			estadd local cutoff "100km"
		drop dist_f_cum_km2_slx_bc_std
		ren dist_f_cum_km2_slx_i_200_std dist_f_cum_km2_slx_bc_std
		la var dist_f_cum_km2_slx_bc_std "Forest Infrastructure (district j $\neq$ d)"
		eststo: reg_sat sr dist_f_cum_km2_std `ctrls' *_slx_bc_std
			estadd local cutoff "200km"
		drop dist_f_cum_km2_slx_bc_std
		ren dist_f_cum_km2_slx_i_500_std dist_f_cum_km2_slx_bc_std
		la var dist_f_cum_km2_slx_bc_std "Forest Infrastructure (district j $\neq$ d)"
		eststo: reg_sat sr dist_f_cum_km2_std `ctrls' *_slx_bc_std
			estadd local cutoff "500km"
		
		esttab using "${TABLE}/tables/spatial_spillovers.tex", replace ///
			keep(dist_f_cum*) stats(cutoff user_y_fe dist_fe st_m_fe N r2, ///
			labels(`"Distance Cutoff"' `"User x Year FEs"' `"District FEs"' ///
			`"State x Month FEs"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 3)) ///
			mgroups("Neighbours" "Inverse Distance", pattern(1 1 0 0 0) ///
			prefix(\multicolumn{@span}{c}{) suffix(}) span ///
			erepeat(\cmidrule(lr){@span})) wrap nocons nonotes booktabs ///
			nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) ///
			width(\hsize) varwidth(30)
		eststo clear
	}
	
}



