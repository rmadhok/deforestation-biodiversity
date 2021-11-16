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
gl TABLE	"${ROOT}/docs/jmp/tex_doc/v2"
cd "${TABLE}"

// Modules
local main_analysis		1
local mechanism			0
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

	reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips], ///
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
	local ctrls coverage tree_cover_pct temp rain all_species duration
	drop_outliers
	
	la var dist_f_cum_km2 "Forest Infrastructure (\(km^{2}\))"
	la var dist_nf_cum_km2 "Non-Forest Diversion (\(km^{2}\))"
	
	*--------------------
	* 1. MAIN RESULTS - USE EXPERIENCE INDEX INSTEAD OF LINEAR USER TREND
	*--------------------
	eststo clear
	eststo m1: reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' u_lin [aweight=n_trips], ///
		a(user_id c_code_2011_num state_code_2011_num#month year) vce(cl biome)
	
		sum sr if e(sample)==1
		estadd scalar ymean = `r(mean)'
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"

	eststo m2: reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' u_lin [aweight=n_trips], ///
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
		mlabposition(1) mlabgap(*2) xlabel(-1(.5)0.5, labsize(medium)) ///
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
		erepeat(\cmidrule(lr){@span})) indicate("Experience Index=u_lin") ///
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
		dist_nf_tra_cum_km2 `ctrls' exp_idx
	
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

	* Baseline Forest Cover
	// USE 2014 DISTRICT BASELINE ALREADY CALCULATED?
	bys c_code_2011 (year_month): g tree_base = tree_cover_km2 if _n == 1
	bys c_code_2011: carryforward tree_base, replace
	
	// Quintiles of Forest Cover
	*g tree_cover_base = (tree_cover_base/tot_area)*100
	xtile tcover_base_q = tree_base, nquantiles(5)
	tab tcover_base_q, gen(tcover_q_)
	foreach v of varlist tcover_q_* { // interactions
		g dist_f_`v' = dist_f_cum_km2 * `v'
	}

	la var dist_f_tcover_q_1 "1st"
	la var dist_f_tcover_q_2 "2nd" 
	la var dist_f_tcover_q_3 "3rd"
	la var dist_f_tcover_q_4 "4th"
	la var dist_f_tcover_q_5 "5th"

	eststo clear
	eststo: reghdfe sr dist_f_tcover_q_2-dist_f_tcover_q_5 ///
		dist_f_cum_km2 dist_nf_cum_km2 `ctrls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) vce(cl biome)

	coefplot, keep(dist_f_tcover_*) xline(0, lcolor(maroon) lpattern(solid))  ///
		msize(small) mfcolor(white) msymbol(D) mcolor(black) ///
		ciopts(recast(rcap) lpattern(shortdash) lcolor(gs5)) ///
		mlabel ytitle("Forest Cover (Quintiles)", size(large)) format(%9.2g) ///
		mlabposition(1) coeflabels(, labsize(medium)) mlabsize(medium) ///
		mlabcolor(black) xlabel(-8(2)2, labsize(medium)) ylabel(, labsize(medium)) ///
		mlabgap(*2) transform(* = min(max(@,-8.3),2)) title("C", size(large)) ///
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

	* Read
	use "${DATA}/dta/fc_ebd_user_update2020", clear

	* Prep
	local ctrls coverage temp rain all_species duration
	drop_outliers
	
	// ALL PROJECTS
	
	* Number of projects per 10km2 forest (null)
	*reg_sat sr n_patches_cum_s `ctrls' tree_cover_km2 exp_idx
	
	* Category-wise, controlling for forest area
	*reg_sat sr n_ele_cum n_irr_cum n_min_cum n_oth_cum n_res_cum n_tra_cum tree_cover_km2 `ctrls'
	// resettlement and mining have large opposing effects. Everything else null. Edge effect?
	
	* Category-wise num projects per 10km2 forest
	*reg_sat sr n_ele_cum_s n_irr_cum_s n_min_cum_s n_oth_cum_s n_res_cum_s n_tra_cum_s `ctrls'
	
	// mining is null. Irrigation has largest negative effect
	// resettlement effect large and POSITIVE...
	
	// DROP NON-PROJECT DISTRICTS
	bys c_code_2011: egen any_def = max(dist_f > 0 & dist_f!=.)
	drop if any_def == 0
	
	* main
	*reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' tree_cover_km2 exp_idx
	
	* Number of projects, controlling for forest area (null)
	*reg_sat sr n_patches_cum `ctrls' tree_cover_km2 exp_idx
	
	* Category-wise, controlling for forest area
	*reg_sat sr n_ele_cum n_irr_cum n_min_cum n_oth_cum n_res_cum n_tra_cum tree_cover_km2 exp_idx `ctrls'
	// mining effect goes away. Everything driven by resettlement
	
	* category-wise, num projects per 10km2 forest area
	*reg_sat sr n_ele_cum_s n_irr_cum_s n_min_cum_s n_oth_cum_s n_res_cum_s n_tra_cum_s tree_cover_pct exp_idx `ctrls'
	// irrigation drives all sr loss (everything else null)

	*reg_sat sr dist_f_*_cum_km2 dist_nf_*_cum_km2 tree_cover_km2 exp_idx `ctrls'
	// other and resettlement still drive results. Mine is null
	
	* AREA share
	*reg_sat sr dist_f_*_cum_km2_s dist_nf_*_cum_km2_s `ctrls' exp_idx
	// similar to absolute area
	
	// electricity increases SR, mining null (big magnitude, big sd), other and resettlement decrease SR. This is more intuitive
	
	* shannon index? perhaps SR results driven by higher abundance of common species.
	*reg_sat sh_index dist_f_cum_km2 dist_nf_cum_km2 `ctrls' tree_cover_pct exp_idx
	*reg_sat sh_index dist_f_*_cum_km2 dist_nf_*_cum_km2 tree_cover_pct exp_idx `ctrls'
	*reg_sat si_index dist_f_*_cum_km2_s dist_nf_*_cum_km2_s tree_cover_pct exp_idx `ctrls'
	
	// almost everything negative (mines have largest effect)
	
	* drop mine altogether
	*reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' tree_cover_km2 exp_idx
	*reg_sat sr dist_f_*_cum_km2_s dist_nf_*_cum_km2_s `ctrls' tree_cover_pct exp_idx
	

	// TRIP LEVEL (none of this works)
	/*
	use "/Volumes/Backup Plus/research/data/def_biodiv/ebird/fc_ebird_trip", clear
	local ctrls coverage temp rain duration tree_cover_km2 allspeciesreported
	bys observerid c_code_2011 year_month: egen n_trips = nvals(samplingeventidentifier)
	drop_outliers
	reghdfe sr dist_f_cum_km2_s dist_nf_cum_km2 `ctrls' exp_idx, ///
		a(user_id#c_code_2011_num state_code_2011_num#month year) vce(cl c_code_2011_num#year_month) 
	reghdfe sh_index dist_f_*_cum_km2 dist_nf_*_cum_km2 `ctrls', ///
		a(user_id#c_code_2011_num state_code_2011_num#month) vce(cl c_code_2011_num#year_month)   
	*/
	
	// Edge effect
	/*
	*xtile edge = tree_cover_pt_base, nquantiles(3)
	g forest_density = 1 if tree_cover_pt_base < 10
	replace forest_density = 2 if tree_cover_pt_base >=10
	*replace forest_density = 2 if inrange(tree_cover_pt_base, 10, 40)
	*replace forest_density = 3 if inrange(tree_cover_pt_base, 40, 70)
	tab forest_density, gen(edge_)
	g dist_f_others_cum_km2 = dist_f_cum_km2 - dist_f_min_cum_km2
	foreach v in ele irr min oth res tra { // interactions
		g dist_f_`v'_edge_1 = dist_f_`v'_cum_km2_s * edge_1
		g dist_f_`v'_edge_2 = dist_f_`v'_cum_km2_s * edge_2	
	}
	
	eststo: reghdfe sr dist_f_min_cum_km2_s dist_f_min_edge_1 tree_cover_km2 exp_idx `ctrls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) vce(cl biome)

	
	* District-level
	g exp_log = log(exp_idx)
	collapse (sum) n_trips (mean) u_lin exp_log ///
			 (firstnm) dist_*_cum_km2 coverage tree_cover ///
					   temp rain all_species duration distance biome ///
					   state_code_2011* c_code_2011_num month year, ///
		     by(c_code_2011 year_month)
	
	* TWFE
	reghdfe n_trips dist_f_cum_km2 dist_nf_cum_km2 `ctrls', ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome) 
		
	reghdfe exp_log dist_f_cum_km2 dist_nf_cum_km2 `ctrls', ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome) 
	
	* by project
	reghdfe duration dist_f_ele_cum_km2 dist_f_irr_cum_km2 ///
		dist_f_min_cum_km2 dist_f_oth_cum_km2 dist_f_res_cum_km2 ///
		dist_f_tra_cum_km2 dist_nf_ele_cum_km2 dist_nf_irr_cum_km2 ///
		dist_nf_min_cum_km2 dist_nf_oth_cum_km2 dist_nf_res_cum_km2 ///
		dist_nf_tra_cum_km2 coverage tree_cover temp rain all_species, ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl biome) 
	*/
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



