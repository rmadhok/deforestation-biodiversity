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
gl ROOT 	"/Users/rmadhok/Dropbox (Personal)/def_biodiv"
gl DATA 	"${ROOT}/data"
gl TABLE	"${ROOT}/docs/manuscript/"

// Module
local sumstats			0
local event_study		0
local main_analysis		0
local simulation		1
local robustness		1
	local spillovers	1
	local others		1
	
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

	qui reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month, savefe) ///
		vce(cl state_code_2011_num#year) resid
		estadd local user_y_fe "x"
		estadd local dist_fe "x"
		estadd local st_m_fe "x" 
end

// Drop Outliers
capture program drop drop_outliers
	program define drop_outliers

	egen n_trips_pc99 = pctile(n_trips), p(99)
	gen outlier = (n_trips > n_trips_pc99)
	drop if outlier == 1

end
*===============================================================================
* SUMMARY STATISTICS
*===============================================================================
if `sumstats' == 1 {
	
	* Read
	use "${DATA}/dta/fc_ebd_user", clear
	drop_outliers
	
	* Tag district
	bys c_code_2011: egen any_def = max(dist_f > 0 & dist_f!=.)
	egen tag_d = tag(c_code_2011) //district-level
	
	* Indent for pretty latex formatting
	foreach v of varlist dist_f_cum dist_f_*_cum* tot_area tree* ///
		s_richness pop_density coverage n_* duration distance temp rain { 
	
			label variable `v' `"    `: variable label `v''"'	
		}

	** Collect
	local dist_vars tot_area pop_density n_users_dist n_trips_dist
	local trip s_richness
	local covariates tree_cover coverage rain temp
	
	*---------------------------------------
	* OUTCOMES AND COVARIATES
	*---------------------------------------

	** District Variables
	eststo clear
	eststo A: estpost tabstat `dist_vars' if tag_d & any_def, s(n mean sd) c(s)
	eststo B: estpost tabstat `dist_vars' if tag_d & !any_def, s(n mean sd) c(s)
	esttab A B using "${TABLE}/tables/sumstats_biodiv.rtf", replace main(mean) ///
		aux(sd) cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		mgroups("Deforestation Districts" "Non-deforestation Districts", pattern(1 1)) ///
		collabel("Mean" "Std. Dev." "N") refcat(tot_area "District Variables", ///
		nolabel) nomtitle noobs label unstack nonumber
	
	** Birdwatching Details
	eststo clear
	eststo A: estpost tabstat `trip' if any_def, s(n mean med sd) c(s)
	eststo B: estpost tabstat `trip' if !any_def, s(n mean med sd) c(s)
	esttab A B using "${TABLE}/tables/sumstats_biodiv.rtf", append main(mean) aux(sd) ///
		cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		refcat(s_richness "Outcome" , nolabel) nomtitle collabel(none) ///
		noobs label unstack nonumber plain
	
	** Covariates
	eststo clear
	eststo A: estpost tabstat `covariates' if any_def, s(n mean med sd) c(s)
	eststo B: estpost tabstat `covariates' if !any_def, s(n mean med sd) c(s)
	esttab A B using "${TABLE}/tables/sumstats_biodiv.rtf", append main(mean) ///
		aux(sd) cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		refcat(tree_cover "Covariates" , nolabel) nomtitle collabel(none) ///
		noobs label unstack nonumber plain ///
		addnotes("Note: Spatial coverage is the fraction of grid cells in a district" ///
		"containing at least one bird observation over the study period using a" ///
		"5km x 5km grid")

	*-----------------------------------
	* Forest Clearance
	*-----------------------------------
	
	* Per-District Month
	eststo clear
	eststo B: estpost tabstat dist_f_elec-dist_f_pa if any_def, s(n mean sd) c(s)

	* Project-wise area totals and shares
	use "${DATA}/dta/fc_clean", clear
	keep if prop_status == "approved"
	replace proj_cat = "other" if proj_cat == "industry" | proj_cat == "underground"
	
	levelsof proj_cat, local(category)
	levelsof proj_shape, local(shape)
	foreach cat of local category {
		
		gen `cat' = proj_area_forest2 if proj_cat == "`cat'"
		egen `cat'_sum = total(`cat')
		drop `cat'
		ren `cat'_sum `cat'
		replace `cat' = . if proj_cat != "`cat'"
	}

	foreach sh of local shape {
		gen `sh' = proj_area_forest2 if proj_shape == "`sh'"
		egen `sh'_sum = total(`sh')
		drop `sh'
		ren `sh'_sum `sh'
		replace `sh' = . if proj_shape != "`sh'"
	}
	
	gen dist_f_pa = proj_area_forest2 if proj_in_pa_esz_num
	egen dist_f_pa_sum = total(dist_f_pa)
	drop dist_f_pa
	ren dist_f_pa_sum dist_f_pa
	replace dist_f_pa = . if proj_in_pa_esz_num == 0
	la var dist_f_pa "Near Protected Area"
	
	* Label
	foreach v of varlist electricity-nonlinear {
		la var `v' "`v'"
		local x : variable label `v'
		local x = proper("`x'")
		la var `v' "`x'"
	}
	ren (electricity irrigation mining other resettlement transportation hybrid linear nonlinear) ///
		(dist_f_elec dist_f_irr dist_f_mine dist_f_o dist_f_res dist_f_tran dist_f_hyb dist_f_lin dist_f_nl)
	
	* Totals
	eststo A: estpost tabstat dist_f_elec-dist_f_pa, s(n mean sd) c(s)
	
	esttab A B using "${TABLE}/tables/sumstats_deforest2.rtf", replace ///
		label cells("count(fmt(0)) mean(fmt(2)) sd(fmt(2))") ///
		mgroups("Totals" "Per District-Yearmonth", pattern(1 1)) ///
		collabel("N" "Mean" "Std. Dev.") ///
		refcat(dist_f_elec "Project Type" dist_f_hyb "Project Group", nolabel) ///
		nomtitle unstack nonumber
	eststo clear
	
	** Spatial Correlation
	egen num_districts = rownonmiss(district_0-district_9), s
	la var num_districts "Districts"
	drop if num_districts == 0
	
	eststo: estpost tabulate num_districts, nototal
	esttab using "${TABLE}/tables/spatial_dep.rtf", replace ///	
		cells("count(fmt(0)) pct(fmt(2)) cumpct(fmt(2))") ///
		nonumbers nomtitle collabels("" "%" "Cum.") ///
		noobs gap label 
	eststo clear
}

*===============================================================================
* EVENT STUDY
*===============================================================================
if `event_study' == 1 {
	
	* Read
	use "${DATA}/dta/fc_ebd_user", clear
	
	* Prep
	drop_outliers
	keep user_id *code* year_month n_trips ///
		 s_richness dist_f_cum_km2 coverage ///
		 tree_cover temp rain all_species month year
	local controls coverage tree_cover temp rain all_species
	
	* Construct Event Dummies
	
	* Event Date
	bys c_code_2011 (year_month): gen e_date = year_month if ///
		dist_f_cum_km2[_n] != dist_f_cum_km2[_n-1]
	
	* Fill in
	bys c_code_2011 (year_month): replace e_date = . if _n == 1 
	bys c_code_2011 (year_month): carryforward e_date, gen(event_date)
	gen nym = -year_month
	bysort c_code_2011 (nym): carryforward event_date, replace
	sort c_code_2011 year_month
	drop nym
	format event_date %tmCCYY-NN
	
	* Time to Construction
	gen dif = year_month-event_date
	keep if inrange(dif, -6, 6) // 6 month window
	
	* Event dummies
	tostring dif, replace
	tab dif, gen(e_)
	foreach v of varlist e_* {
		local x : variable label `v'
		local y = subinstr("`x'", "dif==", "",.)
		la var `v' "`y'"
	}
	
	* Regression
	reghdfe s_richness e_6 e_5 e_4 e_3 e_2 e_1 e_8-e_13 ///
		`controls' [aweight=n_trips], ///
		a(user_id c_code_2011_num state_code_2011_num#month)
	
	parmest, label list(parm label estimate min* max* p) saving(results, replace)
	use results, clear
	
	replace parm = label
	destring parm, replace force
	drop if parm == .
	
	twoway (rarea min95 max95 parm, sort color(gs13)) ///
		   (connected  estimate parm, sort mcolor(black) ///
		   lpattern(solid) lcolor(black) msymbol(o)), ///
		   scheme(s2mono) yline(0) xline(0, lpattern(dash)) ///
		   graphregion(color(white)) bgcolor(white) ///
		   legend(on order(1 "95% CI" 2 "Coefficient")) ///
		   xtitle("Time Periods Since Construction", size(medium)) ///
		   ytitle("Species Richness Relative" "To Time of Construction (t=0)", ///
		   size(medium)) yscale(titlegap(6)) xlabel(-6(1)6) 

	graph export "${TABLE}/fig/event_study.png", replace
	
}

*===============================================================================
* MAIN ANALYSIS
*===============================================================================
if `main_analysis' == 1 {
	
	* Read
	use "${DATA}/dta/fc_ebd_user", clear
	
	* Prep
	local ctrls coverage tree_cover temp rain all_species
	drop_outliers
	la var dist_f_cum_km2 "Deforestation (\(km^{2}\))"
	
	* Fig 1 - Plot
	eststo clear
	eststo m1: reghdfe s_richness dist_f_cum_km2 ///
		dist_nf_cum_km2 `ctrls' [aweight=n_trips], ///
		a(user_id c_code_2011_num state_code_2011_num#month year) ///
		vce(cl state_code_2011_num#year)
	eststo m2: reghdfe s_richness dist_f_cum_km2 ///
		dist_nf_cum_km2 `ctrls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) ///
		vce(cl state_code_2011_num#year)
		sum s_richness if e(sample)
	
	coefplot (m1, rename(dist_f_cum_km2 = "Learning Curve") \ m2, ///
		rename(dist_f_cum_km2 = "No Learning Curve")), ///
		keep(dist_f*) xline(0, lcolor(gs5) lpattern(dash)) ///
		coeflabels(, labsize(large)) mlabsize(medium) ///
		ciopts(recast(rcap)) msize(small) mfcolor(white) msymbol(D) ///
		graphregion(color(white)) bgcolor(white) grid(glcolor(gs15)) ///
		mlabel format(%9.2g) mlabposition(12) mlabgap(*2) ///
		title("A") saving(main, replace)
	graph export "${TABLE}/fig/main.png", replace

	* Fig 2 - Project-Wise Impacts
	eststo clear
	eststo: reg_sat s_richness dist_f_elec_cum_km2 dist_f_irr_cum_km2 ///
		dist_f_mine_cum_km2 dist_f_o_cum_km2 dist_f_res_cum_km2 ///
		dist_f_tran_cum_km2 dist_nf_elec_cum_km2 dist_nf_irr_cum_km2 ///
		dist_nf_mine_cum_km2 dist_nf_o_cum_km2 dist_nf_res_cum_km2 ///
		dist_nf_tran_cum_km2 `ctrls'
	
	coefplot, keep(dist_f*) sort xline(0, lcolor(gs5) lpattern(dash)) ///
		msize(small) mfcolor(white) msymbol(D) ciopts(recast(rcap)) ///
		graphregion(color(white)) bgcolor(white) grid(glcolor(gs15))  ///
		transform(* = min(max(@,-2),6)) mlabel format(%9.2g) mlabposition(12) /// 
		coeflabels(, labsize(large)) mlabsize(medium) ///
		mlabgap(*2) title("B") saving(projwise, replace)
	graph export "${TABLE}/fig/projwise.png", replace
	
	graph combine main.gph projwise.gph
	graph export "${TABLE}/fig/main_results.png", replace

}

*===============================================================================
* SIMULATION
*===============================================================================
if `simulation' == 1 {
	
	*-----------------------------------
	* TOTAL PROJECT DISTRIBUTION
	*-----------------------------------
	
	* Read
	use "${DATA}/dta/fc_ebd_user", clear
	
	drop s_richness_ihs
	drop_outliers
	local ctrls coverage tree_cover temp rain all_species
	
	* Baseline
	reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2  `ctrls'
	
	* Store
	gen b_cons = _b[_cons]
	foreach v of varlist dist_f_cum_km2 dist_nf_cum_km2 `ctrls' {
		
		gen b_`v' = _b[`v']
	}
	
	forvalues i = 95(-5)50 {
		
		di "Simulating Diversity Under `i' % Deforestation..."
		
		* Simulate
		foreach v in dist_f dist_nf {
			
			gen `v'_sim_`i' = 0 if `v'_cum_km2 != .
			replace `v'_sim_`i' = `v'_cum_km2 * (`i'/100) if `v'_cum_km2 > 0 & `v'_cum_km2 < .
		}
	
		* Predicted
		gen s_richness_`i' = b_cons + (b_dist_f_cum_km2*dist_f_sim_`i') + ///
			(b_dist_nf_cum_km2*dist_nf_sim_`i') + (b_coverage*coverage) + ///
			(b_tree_cover*tree_cover) + (b_temp*temp) + (b_rain*rain) + ///
			(b_all_species*all_species) + __hdfe1__ + __hdfe2__ + ///
			__hdfe3__ + _reghdfe_resid
		
		local lab = 100 - `i'
		la var s_richness_`i' "-`lab'"
		eststo s`i': qui mean(s_richness_`i')	
	
	}

	sum s_richness
	local ymean = round(r(mean), 0.01)
	di "`ymean'"
	
	coefplot (s95 \ s90 \ s85 \ s80 \ s75 \ s70 \ s65 \ s60 \ s65 \ s60 \ s55 \ s50), ///
		title("A", pos(1)) vertical mcolor(black) msize(small) ///
		ciopts(recast(rcap) lcolor(gs8)) ytitle("Predicted Species Richness", ///
		size(medium)) yscale(titlegap(6)) ylabel(18.5(0.1)19,labsize(medium) angle(hor)) ///
		yline(`ymean', lpattern(dash)) xlabel(, labsize(medium)) ///
		xscale(titlegap(6)) bgcolor(white) grid(glcolor(gs15)) ///
		graphregion(color(white)) saving(sim1, replace)
	
	* % change rel. to baseline
	preserve

		collapse (mean) s_richness*
		gen id = _n
		reshape long s_richness_, i(id) j(pct)
		gen decline = 100 - pct
		gen change = ((s_richness_ - s_richness) / s_richness) * 100
	
		* Plot
		twoway connected change decline, lcolor(black) mcolor(black) ///
			title("C", pos(1)) xlabels(5 "-5" 10 "-10" 15 "-15" 20 "-20" ///
			25 "-25" 30 "-30" 35 "-35" 40 "-40" 45 "-45" 50 "-50") ///
			xtitle("% Change in Land Diversion", size(medium)) ///
			xlabel(, labsize(medium)) xscale(titlegap(6)) ///
			ytitle("% Change in Species Richness" "Relative to BAU", size(medium)) ///
			yscale(titlegap(6)) ylabel(,labsize(medium) angle(hor)) ///
			bgcolor(white) graphregion(color(white)) saving(sim3, replace)
	
	restore
	
	*-----------------------------------
	* SHIFT TO NON-FOREST AREA
	*-----------------------------------
	drop *_sim* s_richness_*
	
	forvalues i = 95(-5)50 {
		
		di "Simulating Diversity Under `i' % Deforestation..."
		
		* Reduce Forest Diversion by x%
		gen dist_f_sim_`i' = 0 if dist_f_cum_km2 != .
		replace dist_f_sim_`i' = dist_f_cum_km2 * (`i'/100) if dist_f_cum_km2 > 0 & dist_f_cum_km2 < .
		
		* Shift forest to non-forest diversion
		gen dist_nf_sim_`i' = dist_nf_cum_km2 + dist_f_cum_km2*((100-`i')/100)
		
		* Predicted
		gen s_richness_`i' = b_cons + (b_dist_f_cum_km2*dist_f_sim_`i') + ///
			(b_dist_nf_cum_km2*dist_nf_sim_`i') + (b_coverage*coverage) + ///
			(b_tree_cover*tree_cover) + (b_temp*temp) + (b_rain*rain) + ///
			(b_all_species*all_species) + __hdfe1__ + __hdfe2__ + ///
			__hdfe3__ + _reghdfe_resid
		
		local lab = 100 - `i'
		la var s_richness_`i' "-`lab'"
		eststo s`i': mean(s_richness_`i')	
	
	}
	
	coefplot (s95 \ s90 \ s85 \ s80 \ s75 \ s70 \ s65 \ s60 \ s65 \ s60 \ s55 \ s50), ///
		title("B", pos(1)) vertical mcolor(black) msize(small) ///
		ciopts(recast(rcap) lcolor(gs8)) yscale(titlegap(6)) ///
		ylabel(18.5(0.1)19,labsize(medium) angle(hor)) ///
		yline(`ymean', lpattern(dash)) note("BAU Mean = `ymean'", ///
		ring(0) pos(5) box) xlabel(, labsize(medium)) xscale(titlegap(6)) ///
		bgcolor(white) grid(glcolor(gs15)) graphregion(color(white)) ///
		saving(sim2, replace)
	
	* % change rel. to baseline
	preserve

		collapse (mean) s_richness*
		gen id = _n
		reshape long s_richness_, i(id) j(pct)
		gen decline = 100 - pct
		gen change = ((s_richness_ - s_richness) / s_richness)*100
	
		* Plot
		twoway connected change decline, lcolor(black) mcolor(black) ///
			title("D", pos(1)) xlabels(5 "-5" 10 "-10" 15 "-15" 20 "-20" ///
			25 "-25" 30 "-30" 35 "-35" 40 "-40" 45 "-45" 50 "-50") ///
			xtitle("% of Forest Diversion Relocated", size(medium)) ///
			xlabel(, labsize(medium)) xscale(titlegap(6)) yscale(titlegap(6)) ///
			ytitle("") ylabel(, labsize(medium) angle(hor)) ///
			bgcolor(white) graphregion(color(white)) saving(sim4, replace)
	
	restore
	
	graph combine sim1.gph sim2.gph sim3.gph sim4.gph
	graph export "${TABLE}/fig/simulation.png", replace
}

*===============================================================================
* ROBUSTNESS CHECKS
*===============================================================================
if `robustness' == 1 {
	
	if `spillovers' == 1 {
	
		* Read
		use "${DATA}/dta/fc_ebd_user", clear
		drop_outliers
	
		* Controls
		local ctrls coverage tree_cover temp rain all_species
		local slx_b dist_f_cum_km2_slx_bc dist_nf_cum_km2_slx_bc
		
		*------------------------
		* SPATIAL SPILLOVERS
		*------------------------
		
		* Project-Wise
		la var dist_f_cum_km2 "All"
		eststo all: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
		eststo all_w: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' `slx_b'
		
		// Baseline
		eststo proj: reg_sat s_richness dist_f_elec_cum_km2 dist_f_irr_cum_km2 ///
			dist_f_mine_cum_km2 dist_f_o_cum_km2 dist_f_res_cum_km2 ///
			dist_f_tran_cum_km2 dist_nf_elec_cum_km2 dist_nf_irr_cum_km2 ///
			dist_nf_mine_cum_km2 dist_nf_o_cum_km2 dist_nf_res_cum_km2 ///
			dist_nf_tran_cum_km2 `ctrls'
		// SLX
		eststo proj_w: reg_sat s_richness ///
			dist_f_elec_cum_km2 dist_nf_elec_cum_km2 dist_f_elec_cum_km2_slx_bc dist_nf_elec_cum_km2_slx_bc ///
			dist_f_irr_cum_km2 dist_nf_irr_cum_km2 dist_f_irr_cum_km2_slx_bc dist_nf_irr_cum_km2_slx_bc ///
			dist_f_mine_cum_km2 dist_nf_mine_cum_km2 dist_f_mine_cum_km2_slx_bc dist_nf_mine_cum_km2_slx_bc ///
			dist_f_o_cum_km2 dist_nf_o_cum_km2 dist_f_o_cum_km2_slx_bc dist_nf_o_cum_km2_slx_bc ///
			dist_f_res_cum_km2 dist_nf_res_cum_km2 dist_f_res_cum_km2_slx_bc dist_nf_res_cum_km2_slx_bc ///
			dist_f_tran_cum_km2 dist_nf_tran_cum_km2 dist_f_tran_cum_km2_slx_bc dist_nf_tran_cum_km2_slx_bc ///
			`ctrls'
		
		coefplot (all proj, label("Baseline")) ///
				 (all_w proj_w, label("SLX")), ///
				 keep(dist_f*km2) xline(0, lcolor(gs5) lpattern(dash)) ///
				 msymbol(D) mfcolor(white) msize(small) ///
				 graphregion(color(white)) bgcolor(white) grid(glcolor(gs15)) ///
				 transform(* = min(max(@,-5),5)) ///
				 order(dist_f_cum_km2 dist_f_res_cum_km2 dist_f_o_cum_km2 ///
				 dist_f_tran_cum_km2 dist_f_elec_cum_km2 dist_f_irr_cum_km2 ///
				 dist_f_mine_cum_km2)
		graph export "${TABLE}/fig/projwise_slx.png", replace
		
		* All Projects
		
		// PANEL A: SPATIAL SPILLOVERS
		la var dist_f_cum_km2 "Direct Effect"
		la var dist_f_cum_km2_slx_bc "Spillover Effect"
		eststo: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' `slx_b' // binary cont.
		
		coefplot, keep(dist_f*) xline(0, lcolor(gs5) lpattern(dash)) ///
			ciopts(recast(rcap)) msize(small) mfcolor(white) msymbol(D) ///
			coeflabels(, labsize(large)) graphregion(color(white)) ///
			bgcolor(white) grid(glcolor(gs15)) mlabel format(%9.2g) ///
			mlabposition(12) mlabgap(*2) mlabsize(medium) ///
			title("Panel A: Spatial Lag") saving(slx, replace)
		eststo clear
		
		// PANEL B: TEMPORAL SPILLOVERS
		
		// Construct Lags
		foreach var of varlist dist_f_cum_km2 dist_nf_cum_km2 {
			foreach i of numlist 1/6 {
			
				bys c_code_2011 (year_month): gen `var'_l`i' = `var'[_n - `i']
				la var `var'_l`i' "`i' Month"
			}
		}
		
		// No Lag
		la var dist_f_cum_km2 "No Lag"
		reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' 
		eststo nolag: lincomest dist_f_cum_km2
		
		// 1 month
		reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 ///
			dist_f_cum_km2_l1 dist_nf_cum_km2_l1 `ctrls'
		eststo lag_1: lincomest dist_f_cum_km2_l1 + dist_f_cum_km2 
		
		// 3 month
		reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 *_l1 *_l2 *_l3 `ctrls'
		eststo lag_3: lincomest dist_f_cum_km2_l3 + dist_f_cum_km2_l2 + dist_f_cum_km2_l1 + dist_f_cum_km2 
		
		// 5 month
		reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 *_l1 *_l2 *_l3 *_l4 *_l5 `ctrls'
		eststo lag_5: lincomest dist_f_cum_km2_l5 + dist_f_cum_km2_l4 + dist_f_cum_km2_l3 + dist_f_cum_km2_l2 + dist_f_cum_km2_l1 + dist_f_cum_km2
		
		coefplot (nolag, rename((1) = "Baseline") \ lag_1, rename((1) = "Sum L0-L1") \ ///
			lag_3, rename((1) = "Sum L0-L3") \ lag_5, rename((1) = "Sum L0-L5")), ///
			xline(0, lcolor(gs5) lpattern(dash)) msymbol(D) msize(small) ///
			mfcolor(white) ciopts(recast(rcap)) coeflabels(, labsize(large)) ///
			graphregion(color(white)) bgcolor(white) grid(glcolor(gs15)) ///
			mlabel format(%9.2g) mlabposition(12) mlabgap(*2) mlabsize(medium) ///
			xlabel(-1.5(0.5)0) title("Panel B: Temporal Lag") saving(dlag, replace)
		
		graph combine slx.gph dlag.gph
		graph export "${TABLE}/fig/spillovers.png", replace
	}
	
	if `others' == 1 {
		
		* Read
		use "${DATA}/dta/fc_ebd_user", clear
		drop_outliers
	
		* Controls
		local ctrls coverage tree_cover temp rain all_species
		local slx_b dist_f_cum_km2_slx_bc dist_nf_cum_km2_slx_bc
		la var dist_f_cum_km2 "Deforestation (Stage 2)"
		la var dist_f_cum_km2_slx_bc "SLX Deforestation (Stage 2)"
		
		// Seasonality
		
		* 1. User x Month FE 
		eststo: qui reghdfe s_richness dist_f_cum_km2 dist_nf_cum_km2 ///
			`ctrls' n_mon_yr u_lin, ///
			a(user_id#month c_code_2011_num state_code_2011_num#year) ///
			vce(cl state_code_2011_num#year)
				estadd local user_m_fe "x"
				estadd local dist_fe "x"
				estadd local st_y_fe "x"
				estadd local u_trend "Linear"
		
		* 2. User x Year, Cubic Trend
		eststo: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' u_cubic
			estadd local u_trend "Cubic"
		
		// SLX Weight Matrix
		
		* 3. W = 1/d
		preserve
			drop *_slx_bc
			ren *_slx_i *_slx_bc
			eststo: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' `slx_b'
				estadd local u_trend "None"
			drop *_slx_bc
		
		* 4. W = 1/d^2
			ren *_slx_i2 *_slx_bc
			eststo: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' `slx_b'
				estadd local u_trend "None" 
		restore
		
		// Illegal Deforestation
		
		* 5. Stage 1
		eststo: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' *_km2_s1
			estadd local u_trend "None"
		
		* 6. Stage 1 SLX
		eststo: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' *_s1
			estadd local u_trend "None"
		
		// Alternative Outcomes
		
		* 7. IHS
		eststo: reg_sat s_richness_ihs dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
			estadd local u_trend "None"
			
		* 8. Species Richness (across all trips in dist-ym)
		eststo: reg_sat s_richness_all dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
			estadd local u_trend "None"
		
		* 9. Only trips where all species reported
		preserve
			
			use "${DATA}/dta/fc_ebd_user_allreported", clear
			drop_outliers
			
			eststo: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
				estadd local u_trend "None"
		
		restore
			
		esttab using "${TABLE}/tables/robustness.rtf", replace ///
			keep(dist_f* n_mon_yr) stats(user_m_fe user_y_fe dist_fe st_y_fe ///
			st_m_fe u_trend N r2, labels(`"User x Month FEs"' `"User x Year FEs"' ///
			`"District FEs"' `"State x Year FEs"' `"State x Month FEs"' ///
			`"User Trend"' `"N"' `"R^2"') fmt(0 0 0 0 0 0 0 3)) nomtitles ///
			mlabel("User-Month" "User-Year" "W=1/d" "W=1/d^2" "Stage 1" "SLX Stage 1" ///
			"IHS" "All Trips" "All Species Reported") star(* .1 ** .05 *** .01) label ///
			nonotes se b(%5.3f) se(%5.3f)
		eststo clear
	}
	
}



