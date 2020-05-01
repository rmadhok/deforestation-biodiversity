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
gl TABLE	"${ROOT}/docs/science_pub/"

// Module
local sumstats			0
local event_study		0
local main_analysis		0
local robustness		0
local sim				0
local appendix			1

*===============================================================================
* PROGRAMS
*===============================================================================

// Main Analysis
capture program drop reg_user
	program define reg_user

	syntax varlist
	set more off
	
	** Parse
	local depvar : word 1 of `varlist'
	local indepvar : word 2 of `varlist'
	local ctrls = substr("`varlist'", length("`depvar'") + length("`indepvar'") + 3, length("`varlist'"))

	eststo: reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips], ///
		a(user_id c_code_2011_num) vce(r)
		estadd ysumm
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
	eststo: reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips], ///
		a(user_id c_code_2011_num state_code_2011_num#month year) vce(r)
		estadd ysumm
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
	eststo: reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) vce(r)
		estadd ysumm
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
end

// Preferred Spec
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
		vce(cl state_code_2011_num#year) resid	
			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$" 
end

// Drop Outliers
capture program drop drop_outliers
	program define drop_outliers

	egen n_trips_pc99 = pctile(n_trips), p(99)
	gen outlier = (n_trips > n_trips_pc99)
	drop if outlier == 1

end

// Format labels for LaTeX
capture program drop la_var_tex
program define la_var_tex

	syntax varlist
	set more off

	foreach v of varlist `varlist' { 

		label variable `v' `"\hspace{0.2cm} `: variable label `v''"'	
	}

end
*===============================================================================
* SUMMARY STATISTICS
*===============================================================================
if `sumstats' == 1 {

	* Read
	use "${DATA}/dta/fc_ebd_user", clear
	
	* Tag 
	bys c_code_2011: egen any_def = max(dist_f > 0 & dist_f!=.)
	egen tag_d = tag(c_code_2011) //district-level
	
	* Indent for pretty latex formatting
	foreach v of varlist dist_f_cum dist_f_*_cum* tot_area tree* ///
		s_richness pop_density coverage n_* duration distance temp rain { 
	
			label variable `v' `"\hspace{0.2cm} `: variable label `v''"'	
		}

	** Collect
	local dist_vars tot_area pop_density n_users_dist n_trips_dist
	local trip s_richness
	local covariates tree_cover_mean coverage rainfall_mm temperature_mean
	
	** District level
	eststo clear
	*eststo A: estpost tabstat `dist_vars' if tag_d, s(n mean sd) c(s)
	eststo B: estpost tabstat `dist_vars' if tag_d & any_def, s(n mean med sd) c(s)
	eststo C: estpost tabstat `dist_vars' if tag_d & !any_def, s(n mean med sd) c(s)
	esttab B C using "${TABLE}/tables/sumstats_biodiv.tex", replace f main(mean) aux(sd) ///
		cells("mean(star fmt(2)) sd(par fmt(2))  count(fmt(0))") ///
		width(\hsize) mgroups("Deforestation Districts" ///
		"\shortstack{Non-deforestation \\ Districts}", pattern(1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		collabel("\specialcell{Mean}" "Std. Dev." "N", prefix({) suffix(})) ///
		refcat(tot_area "\emph{District Variables}" , nolabel) ///
		nomtitle booktabs noobs label unstack nonumber
		
	** Birdwatching Details
	eststo clear
	*eststo A: estpost tabstat `biodiversity', s(n mean sd) c(s)
	eststo B: estpost tabstat `trip' if any_def, s(n mean med sd) c(s)
	eststo C: estpost tabstat `trip' if !any_def, s(n mean med sd) c(s)
	
	esttab B C using "${TABLE}/tables/sumstats_biodiv.tex", append f main(mean) aux(sd) ///
		cells("mean(star fmt(2)) sd(par fmt(2)) count(fmt(0))") ///
		width(\hsize) refcat(s_richness "\emph{Birdwatching}" , nolabel) ///
		nomtitle collabel(none) booktabs noobs label unstack nonumber plain
	
	** Covariates
	eststo clear
	*eststo A: estpost tabstat `covariates', s(n mean sd) c(s)
	eststo B: estpost tabstat `covariates' if any_def, s(n mean med sd) c(s)
	eststo C: estpost tabstat `covariates' if !any_def, s(n mean med sd) c(s)
	
	esttab B C using "${TABLE}/tables/sumstats_biodiv.tex", append f main(mean) aux(sd) ///
		cells("mean(star fmt(2)) sd(par fmt(2)) count(fmt(0))") ///
		width(\hsize) refcat(tree_cover_mean "\emph{Covariates}" , nolabel) ///
		nomtitle collabel(none) booktabs noobs label unstack nonumber plain ///
		addnotes("Note: Spatial coverage is the fraction of grid cells in a district" ///
		"containing at least one bird observation over the study period using an" ///
		"5km $\times$ 5km grid")

	** Forest Clearance

	* Deforestation per dist-month
	ren dist_f Total
	eststo clear
	eststo C: estpost tabstat Total dist_f_elec-dist_f_pa if any_def, s(n mean sd) c(s)

	use "${DATA}/dta/fc_clean", clear
	drop ca_*
	keep if prop_status == "approved"
	replace proj_cat = "other" if proj_cat == "industry" | proj_cat == "underground"
	
	* Proj-wise Area
	levelsof proj_cat, local(category)
	levelsof proj_shape, local(shape)
	
	foreach cat of local category {
		
		gen `cat' = proj_area_forest2 if proj_cat == "`cat'"
	}
	
	foreach sh of local shape {
		gen `sh' = proj_area_forest2 if proj_shape == "`sh'"
	}
	gen dist_f_pa = proj_area_forest2 if proj_in_pa_esz_num
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

	eststo A: estpost tabstat dist_f_elec-dist_f_pa, s(sum) c(s)
	
	foreach v of varlist dist_f_elec-dist_f_pa {
		
		replace `v' = 1 if `v' != .
		replace `v' = 0 if `v' == .
	}
	
	eststo B: estpost tabstat dist_f_elec-dist_f_pa, s(mean) c(s)

	** FIX
	esttab A B C using "${TABLE}/tables/sumstats_deforest2.tex", replace ///
		label cells("mean(star fmt(2)) sd(par fmt(2)) count(fmt(0))") ///
		width(\hsize) mgroups("Project-Wise Break-Up" ///
		"Project-Wise Deforestation", pattern(1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		collabel("\specialcell{Mean}" "Std. Dev" "N", prefix({) suffix(})) ///
		refcat(dist_f_elec "\emph{\underline{Project Type}}" ///
		dist_f_hyb "\emph{\underline{Project Group}}", nolabel) ///
		nomtitle booktabs noobs unstack nonumber
	eststo clear
	
	** Spatial Correlation
	use "${DATA}/dta/fc_clean", clear
	keep if prop_status == "approved"
	
	egen num_districts = rownonmiss(district_0-district_9), s
	la var num_districts "Districts"
	drop if num_districts == 0
	
	eststo: estpost tabulate num_districts, nototal
	
	*local titles "& Districts & {\%} & {Cum. \%} \\ \midrule"
	esttab using "${TABLE}/tables/spatial_dep.tex", replace ///	
		cells("count(fmt(0)) pct(fmt(2)) cumpct(fmt(2))") ///
		nonumbers nomtitle collabels("" "{\%}" "Cum.") ///
		booktabs noobs gap label 
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
		 tree_cover_mean temperature_mean ///
		 rainfall_mm all_species month year
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
	
	coefplot, keep(e*) title("Event Study", margin(b=5)) ///
		vertical yline(0, lcolor(gs5) lpattern(dash)) ///
		xline(6.5, lcolor(gs5) lpattern(dash)) ///
		msize(small) mcolor(black) ciopts(lcolor(black)) ///
		graphregion(color(white)) bgcolor(white) grid(glcolor(gs15)) ///
		xtitle("Time-Periods Since Construction Event", size(medium)) ///
		xscale(titlegap(6)) ytitle("Species Richness Relative" ///
		"To Time of Construction (t=0)", size(medium)) yscale(titlegap(6))
	
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

	/*
	* Table 1 - Impact of Deforestation on Species Richness
	eststo clear
	reg_user s_richness dist_f_cum_km2 dist_nf_cum_km2  `ctrls'
	esttab using "${TABLE}/tables/s_richness_science.tex", replace ///
		keep(dist_f_cum*) stats(ymean user_fe user_y_fe dist_fe ///
		st_m_fe year_fe N r2, labels("Mean" `"User FEs"' `"User $\times$ Year FEs"' ///
		`"District FEs"' `"State $\times$ Month FEs"' `"Year FE"' `"N"' ///
		`"\(R^{2}\)"') fmt(2 0 0 0 0 0 0 3)) nocons nomtitles ///
		indicate("Controls=temperature_mean") star(* .1 ** .05 *** .01) ///
		label nonotes booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	*/
	
	* Table 1 - Plot
	eststo clear
	eststo m1: reghdfe s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' [aweight=n_trips], ///
		a(user_id c_code_2011_num state_code_2011_num#month year) vce(cl state_code_2011_num#year)
	eststo m2: reghdfe s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) vce(cl state_code_2011_num#year)
		sum s_richness if e(sample)
	
	coefplot (m1, rename(dist_f_cum_km2 = "Learning Curve") \ m2, ///
		rename(dist_f_cum_km2 = "No Learning Curve")), ///
		keep(dist_f*) xline(0, lcolor(gs5) lpattern(dash)) ///
		coeflabels(, labsize(large)) mlabsize(medium) ///
		ciopts(recast(rcap)) msize(small) mfcolor(white) msymbol(D) ///
		graphregion(color(white)) bgcolor(white) grid(glcolor(gs15)) ///
		mlabel format(%9.2g) mlabposition(12) mlabgap(*2) ///
		title("A") saving(main, replace)
	graph export "${TABLE}/fig/science_main.png", replace

	* Table 2 - Project-Wise Impacts
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
	graph export "${TABLE}/fig/projwise_science_all.png", replace
	
	graph combine main.gph projwise.gph
	graph export "${TABLE}/fig/main_results_science.png", replace
	
}

*===============================================================================
* ROBUSTNESS CHECKS
*===============================================================================
if `robustness' == 1 {

	* Read
	use "${DATA}/dta/fc_ebd_user", clear
	drop_outliers
	
	* Controls
	local ctrls coverage tree_cover temp rain all_species
	local slx_b dist_f_cum_km2_slx_bc dist_nf_cum_km2_slx_bc
	
	* PANEL A: SPATIAL LAG
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
	
	* PANEL B: DYNAMIC LAG
	
	* Construct lags
	foreach var of varlist dist_f_cum_km2 dist_nf_cum_km2 {
		foreach i of numlist 1/12 {
		
			bys c_code_2011 (year_month): gen `var'_l`i' = `var'[_n - `i']
			la var `var'_l`i' "`i' Month"
		}
	}
	
	* No Lag
	la var dist_f_cum_km2 "No Lag"
	reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' 
	eststo nolag: lincomest dist_f_cum_km2
	
	* 1 month
	reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 ///
		dist_f_cum_km2_l1 dist_nf_cum_km2_l1 `ctrls'
	eststo lag_1: lincomest dist_f_cum_km2_l1 + dist_f_cum_km2 
	
	* 3 month
	reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 *_l1 *_l2 *_l3 `ctrls'
	eststo lag_3: lincomest dist_f_cum_km2_l3 + dist_f_cum_km2_l2 + dist_f_cum_km2_l1 + dist_f_cum_km2 
	
	* 5 month
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
	graph export "${TABLE}/fig/lag_science.png", replace
}

*===============================================================================
* SIMULATION
*===============================================================================
if `sim' == 1 {
	
	*-----------------------------------
	* TOTAL PROJECT DISTRIBUTION
	*-----------------------------------
	
	* Read
	use "${DATA}/dta/fc_ebd_user", clear
	
	drop s_richness_ihs
	drop_outliers
	local ctrls coverage tree_cover_mean temperature_mean rainfall_mm all_species_prop
	
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
			(b_tree_cover_mean*tree_cover_mean) + (b_temperature_mean*temperature_mean) + ///
			(b_rainfall_mm*rainfall_mm) + (b_all_species_prop*all_species_prop) ///
			+ __hdfe1__ + __hdfe2__ + __hdfe3__ + _reghdfe_resid
		
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
			(b_tree_cover_mean*tree_cover_mean) + (b_temperature_mean*temperature_mean) + ///
			(b_rainfall_mm*rainfall_mm) + (b_all_species_prop*all_species_prop) ///
			+ __hdfe1__ + __hdfe2__ + __hdfe3__ + _reghdfe_resid
		
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
	graph export "${TABLE}/fig/simulation_science.png", replace
}

*===============================================================================
* APPENDIX
*===============================================================================
if `appendix' == 1 {

	* Read
	use "${DATA}/dta/fc_ebd_user", clear
	drop_outliers

	* Controls
	local ctrls coverage tree_cover temp rain all_species
	
	
	
		* Read
	use "${DATA}/dta/fc_ebd_user", clear
	drop_outliers
	
	* Controls
	local ctrls coverage tree_cover temp rain all_species
	local slx_b dist_f_cum_km2_slx_bc dist_nf_cum_km2_slx_bc

	// Project-Wise SLX Plot
	* All
	la var dist_f_cum_km2 "All"
	eststo all: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
	eststo all_w: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' `slx_b'
	
	* Project-wise
	eststo proj: reg_sat s_richness dist_f_elec_cum_km2 dist_f_irr_cum_km2 ///
		dist_f_mine_cum_km2 dist_f_o_cum_km2 dist_f_res_cum_km2 ///
		dist_f_tran_cum_km2 dist_nf_elec_cum_km2 dist_nf_irr_cum_km2 ///
		dist_nf_mine_cum_km2 dist_nf_o_cum_km2 dist_nf_res_cum_km2 ///
		dist_nf_tran_cum_km2 `ctrls'
		
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
	graph export "${TABLE}/fig/projwise_slx_science_all.png", replace
	
		/*
	// Table 3 - Robustness Checks

	* Additional Controls
	eststo: qui reghdfe s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' n_mon_yr user_lin_trend, ///
		a(user_id#month c_code_2011_num state_code_2011_num#year) ///
		vce(cl state_code_2011_num#year)
			estadd local user_m_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_y_fe "$\checkmark$"
			estadd local u_trend "Linear"
	eststo: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' user_cubic_trend
		estadd local u_trend "Cubic"
	* Spatial
	eststo: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' `slx_b' // binary cont.
		estadd local u_trend "None"
	preserve
		drop *_slx_bc
		ren *_slx_i *_slx_bc
		eststo: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' `slx_b' // 1/D
			estadd local u_trend "None"
		drop *_slx_bc
		ren *_slx_i2 *_slx_bc
		eststo: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' `slx_b' // 1/D sq.
			estadd local u_trend "None" 
	restore
	esttab using "${TABLE}/tables/robustness_science.tex", replace ///
		keep(dist_f* n_mon_yr) stats(user_m_fe user_y_fe dist_fe st_y_fe st_m_fe ///
		u_trend N r2, labels(`"User $\times$ Month FEs"' ///
		`"User $\times$ Year FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"User Trend"' `"N"' ///
		`"\(R^{2}\)"') fmt(0 0 0 0 0 0 0 3)) mgroups("Additional Controls" ///
		"Spatial Lag", pattern(1 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) ///
		span erepeat(\cmidrule(lr){@span})) mlabels("" "" "W=Contiguity" ///
		"W=$\frac{1}{d}$" "W=$\frac{1}{d^2}$") nomtitles ///
		star(* .1 ** .05 *** .01) label nonotes booktabs se b(%5.3f) ///
		se(%5.3f) width(\hsize)
	eststo clear
*/

	
	// 1. Dynamic Lag Model

	** Figure 3 - Alternative Diversity Measures
	la var dist_f_cum_km2 "All"
	foreach y in s_richness sh_index si_index {
		egen `y'_std = std(`y')
		eststo `y'_all: reg_sat `y'_std dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
		
		foreach x in elec irr mine o res tran pa hyb lin nl {
	
			eststo `y'_`x': reg_sat `y'_std dist_f_`x'_cum_km2 dist_nf_`x'_cum_km2 `ctrls'
		}
	}
	set scheme s2color
	coefplot (s_richness_all \ s_richness_elec \ s_richness_irr \ s_richness_mine \ s_richness_o \ s_richness_res \ s_richness_tran \ s_richness_pa \ s_richness_lin \ s_richness_nl), ///
				keep(dist_f*) bylabel("Species Richness") || ///
			(sh_index_all \ sh_index_elec \ sh_index_irr \ sh_index_mine \ sh_index_o \ sh_index_res \ sh_index_tran \ sh_index_pa \ sh_index_lin \ sh_index_nl), ///
				keep(dist_f*) bylabel("Shannon Index") || ///
			(si_index_all \ si_index_elec \ si_index_irr \ si_index_mine \ si_index_o \ si_index_res \ si_index_tran \ si_index_pa \ si_index_lin \ si_index_nl), ///
				keep(dist_f*) bylabel("Simpson Index") || ///
		, xline(0, lcolor(gs10)) msymbol(D) msize(small) ///
		mcolor(white) levels(99 95 90) ciopts(lwidth(3 ..) lcolor(*.2 *.6 *1)) ///
		graphregion(color(white)) bgcolor(white) transform(* = min(max(@,-.8),.5))	
	graph export "${TABLE}/fig/projwise_coef_alt.png", replace
	eststo clear
	
}


