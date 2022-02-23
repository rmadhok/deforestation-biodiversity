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
local scratch			1
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
	
	syntax varlist(fv)
	set more off
	
	* Parse 
	local depvar : word 1 of `varlist'
	local indepvar : word 2 of `varlist'
	local ctrls = substr("`varlist'", length("`depvar'") + ///
		length("`indepvar'") + 3, length("`varlist'"))

	reghdfe `depvar' `indepvar' `ctrls', ///
		a(uid#year c_code_2011_num state_code_2011_num#month, savefe) ///
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
	use "${DATA}/dta/fc_ebd_udt_v02", clear
	
	* Prep
	drop if year == 2014
	drop_outliers
	local ctrls temp rain tree_cover_s ln_duration ln_distance ///
		        ln_exp_idx ln_coverage_udym ln_group_size traveling
	la var dist_f_cum_km2 "Forest Infrastructure (\(km^{2}\))"

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

	* Coefficient Plot
	coefplot (m1, rename(dist_f_cum_km2 = `" "(1) Learning""') \ m2, ///
		rename(dist_f_cum_km2 = `" "(2) No Learning" "')), ///
		keep(dist_f*) xline(0, lcolor(black*0.8) lpattern(dash)) ///
		coeflabels(, labsize(medium)) mlabsize(medium) mlabcolor(black) ///
		levels(99 95 90) ciopts(recast(rcap) lwidth(*1 *3 *4) ///
		color(dkgreen dkgreen dkgreen) lcolor(*.3 *.5 *.8)) ///
		legend(order(1 "99" 2 "95" 3 "90")) ///
		msize(medium) mfcolor(white) msymbol(D) mlcolor(black) mlabel ///
		format(%9.2g) mlabposition(1) mlabgap(*2) xlabel(-0.3(.1)0.1, ///
		labsize(medium)) ylabel(, labsize(medium)) title("A", size(large)) ///
		saving("${TABLE}/fig/main.gph", replace) xsize(4.6)
	graph export "${TABLE}/fig/main.png", replace
	
	* Table
	esttab using "${TABLE}/tables/main_results.tex", keep(dist_f_cum_km2) replace ///
		stats(ymean user_fe user_y_fe dist_fe st_m_fe ///
		st_y_fe year_fe N r2, labels(`"Outcome Mean"' ///
		`"User FEs"' `"User x Year FEs"' `"District FEs"' ///
		`"State x Month FEs"' `"State x Year FEs"' `"Year FEs"' ///
		`"N"' `"\(R^{2}\)"') fmt(3 0 0 0 0 0 0 0 3)) ///
		mgroups("With Learning Bias" "Without Learning Bias", ///
		pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span ///
		erepeat(\cmidrule(lr){@span})) indicate("Experience Index=ln_exp_idx") ///
		wrap nocons nonotes booktabs nomtitles star(* .1 ** .05 *** .01) ///
		label se b(%5.3f) width(\hsize)
	eststo clear

	*--------------------------
	* 2. DECOMPOSED ESTIMATES
	*--------------------------

	* Fig 2 - category-Wise Impacts
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

	*--------------------------
	* 3. FRAGMENTATION-WISE
	*--------------------------
	
	// Quantiles of Forest Cover -- works with truncated
	* Note : monotonic decline until q5. This is mainly remote NE where there is little eBird activity and estimates are noisy/selected on those who go there
	*g tree_cover_base_p = (tree_cover_base/tot_area)*100
	*xtile tcover_base = tree_cover_base_p, nquantiles(5)
	xtile tcover_base = tree_cover_base, nquantiles(5)
	tab tcover_base, gen(tcover_q)

	eststo clear
	eststo: reg_sat sr c.dist_f_cum_km2##c.(tcover_q2-tcover_q5) n_*_cum_s `ctrls'

	coefplot, keep(c.dist_f_cum_km2#c.*) xline(0, lcolor(black*0.8) lpattern(dash)) ///
		coeflabels(c.dist_f_cum_km2#c.tcover_q2 = "2nd" ///
		c.dist_f_cum_km2#c.tcover_q3 = "3rd" ///
		c.dist_f_cum_km2#c.tcover_q4 = "4th" ///
		c.dist_f_cum_km2#c.tcover_q5 = "5th", labsize(medium)) ///
		msize(small) mfcolor(white) msymbol(D) mcolor(black) ///
		levels(99 95 90) ciopts(recast(rcap) lwidth(*1 *3 *4) ///
		color(dkgreen dkgreen dkgreen) lcolor(*.3 *.5 *.8)) ///
		legend(order(1 "99" 2 "95" 3 "90")) ///
		mlabel ytitle("Forest Cover" "(Quintiles)", size(large) margin(l=8)) ///
		format(%9.2g) mlabposition(1) mlabsize(medium) mlabcolor(black) ///
		xlabel(-2(1)1, labsize(medium)) ylabel(, labsize(medium)) ///
		mlabgap(*2) title("C", size(large)) yscale(titlegap(3)) ///
		saving("${TABLE}/fig/tcoverwise.gph", replace) xsize(4.6)
	graph export "${TABLE}/fig/tcoverwise.png", replace

	* MAIN RESULTS
	graph combine "${TABLE}/fig/main.gph" "${TABLE}/fig/projwise.gph" ///
		"${TABLE}/fig/tcoverwise.gph", holes(4) 
	graph export "${TABLE}/fig/main_results.png", width(1000) replace
		
}

*===============================================================================
* MECHANISM
*===============================================================================
if `scratch' == 1 {

	* Read
	use "${DATA}/dta/fc_ebd_udt_v02", clear
	drop if year == 2014
	drop_outliers
	
	**# Extensive margin: Does fragmentation displace users to other districts
	
	* District level
	
	collapse (mean) exp_idx duration distance group_size ///
					traveling coverage_udym ///
			 (firstnm) dist_f_cum_* coverage_dym tree_cover_s ///
					   temp rain biome n_trips_dym n_users_dym ///
					   sr_dym month year *_code_2011_num, ///
					   by(c_code_2011 year_month)
	
	foreach v of varlist duration exp_idx group_size n_*_dym coverage_* {
		g ln_`v' = ln(`v')
	}
	g ln_distance = asinh(distance)
	
	replace dist_f_cum_km2_slx_i_500 = dist_f_cum_km2_slx_i_500/10000
	local ctrls temp rain tree_cover_s ln_duration ln_distance ///
		        ln_exp_idx ln_group_size traveling
	la var dist_f_cum_km2 "Infrastructure (district $\emph{i}$)"
	la var dist_f_cum_km2_slx_i_500 "Infrastructure (district d $\neq$ i)"
	
	* Does deforestation increase activity in *other* districts (w/n 200 km)?
	foreach v of varlist n_users_dym n_trips_dym {
	
		eststo: reghdfe ln_`v' dist_f_cum_km2 *_slx_i_500 `ctrls', ///
			a(c_code_2011_num state_code_2011_num#month year) vce(cl c_code_2011_num)
			
			estadd local agg "District"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	}
	
	**# Intensive margin: Does fragmentation displace users within district?
	g ln_n_trips = ln(n_trips)
	la var dist_f_cum_km2 "Infrastructure (district $\emph{i}$)"
	la var dist_f_cum_km2_slx_i_500 "Infrastructure (district d $\neq$ i)"
	local ctrls temp rain tree_cover_s ln_duration ln_distance ///
		        ln_exp_idx ln_group_size traveling 
	
	foreach v of varlist n_trips coverage_udym {
		
		eststo: reghdfe ln_`v' dist_f_cum_km2 `ctrls', ///
			a(c_code_2011_num state_code_2011_num#month year) vce(cl c_code_2011_num)
			
			estadd local agg "User"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	}
	
	* Tabulate
	esttab using "${TABLE}/tables/displacement.tex", replace ///
		stats(agg dist_fe st_m_fe year_fe N, labels(`"Data Aggregation"' ///
		`"District FEs"' `"State x Month FEs"' `"Year FEs"' `"N"') ///
		fmt(0 0 0 0 0 3)) mlabels("Num. Users" "Num. Trips") indicate("Controls = `ctrls'") wrap nocons ///
		nonotes booktabs nomtitles star(* .1 ** .05 *** .01) label se ///
		b(%5.3f) width(\hsize)
	eststo clear
	
	
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
	
	
	
	* TO DO
	* 1. twfe with n_trips or other measure of activity
	* 2. twfe by project, add average experience. std dev?
	* 3. remove duration from main spec- bad control?
	* 4. use distance as outcome, by project
	
}

*===============================================================================
* DYNAMICS
*===============================================================================
if `dynamics' == 1 {
	
	* Read
	use "${DATA}/dta/fc_ebd_udt_v02", clear
	
	* Prep
	drop if year == 2014
	drop_outliers
	local ctrls temp rain tree_cover_s ln_duration ln_distance ///
		ln_exp_idx ln_coverage_udym ln_group_size traveling
	
	* Lags/Leads
	foreach var of varlist dist_f_cum_km2 {
		foreach i of numlist 1/6 {
		
			bys c_code_2011 (year_month): gen `var'_lag`i' = `var'[_n - `i']
			la var `var'_lag`i' "Lag `i'"
			
			bys c_code_2011 (year_month): gen `var'_lead`i' = `var'[_n + `i']
			la var `var'_lead`i' "Lead `i'"
			
		}
	}
	
	* Show Serial Correlation -- see olken paper
	/* ----------------------------------------
	Since deforestation is serially correlated
	individual lags are not well identified.
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
	*eststo clear
	*eststo: reg_sat sr dist_f_cum_km2 dist_f_cum_km2_l*  `ctrls'
	
	*----------------------------
	* 2. Cumulative Dynamic Lag
	*----------------------------
	
	// No Lag
	la var dist_f_cum_km2 "No Lag"
	reg_sat sr dist_f_cum_km2 `ctrls' 
	eststo nolag: lincomest dist_f_cum_km2
	
	// 1 month
	reg_sat sr dist_f_cum_km2 dist_f_cum_km2_lag1 `ctrls'
	eststo lag_1: lincomest dist_f_cum_km2_lag1 + dist_f_cum_km2 
	
	// 3 month
	reg_sat sr dist_f_cum_km2 *_lag1 *_lag2 *_lag3 `ctrls'
	eststo lag_3: lincomest dist_f_cum_km2_lag3 + dist_f_cum_km2_lag2 + dist_f_cum_km2_lag1 + dist_f_cum_km2 
	
	// 5 month
	reg_sat sr dist_f_cum_km2 *_lag1 *_lag2 *_lag3 *_lag4 *_lag5 `ctrls'
	eststo lag_5: lincomest dist_f_cum_km2_lag5 + dist_f_cum_km2_lag4 + dist_f_cum_km2_lag3 + dist_f_cum_km2_lag2 + dist_f_cum_km2_lag1 + dist_f_cum_km2
	
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
	
	* ADD: district FE, cell FE, users who stay in one district?, shannon/simpson, weighted, drop outliers, IHS, dist_f_cum_km2 share
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
		
		* 1. Stage-I projects -- COMPUTE ADDED KM2 (lag)
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
		
		* 4. Observer Effort ---- ADD LOG GROUP SIZE AND % TRAVELLING
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



