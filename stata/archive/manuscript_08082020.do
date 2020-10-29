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
local sumstats			1
local learning			0
local event_study		0
local main_analysis		0
local simulation		0
local robustness		0
	local spillovers	0
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
		sr pop_density coverage n_* duration distance temp rain all* { 
	
			label variable `v' `"    `: variable label `v''"'	
		}

	** Collect
	local dist_vars tot_area pop_density n_users_dist n_trips_dist
	local trip sr
	local covariates tree_cover coverage rain temp duration all_species
	
	*---------------------------------------
	* 1. TABLE: OUTCOMES AND COVARIATES
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
		refcat(sr "Outcome" , nolabel) nomtitle collabel(none) ///
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
	* 3. Table: CDF OF N_TRIPS
	*-----------------------------------
	cumul n_trips, gen(cum)
	sort cum
	line cum n_trips, ytitle("Cumulative Density (%)") xlabel(0(5)50)
	graph export "${TABLE}/fig/cdf_ntrips.png", replace
	
	*-------------------------------------
	* 4. VARIATION in USER CHARACTERISTICS
	*-------------------------------------
	preserve
		
		collapse (first) n_*_user, by(user_id)
		local lab `" "A. Num. States Per User" "B. Num. Districts Per User" "C. Num. Time-Periods Per User" "'
		local i = 1
		foreach v of varlist n_st_user n_dist_user n_ym_user {
		
			local title: word `i' of `lab'
			hist `v', frac bcolor(dknavy) ///
				title("`title'") ytitle("% of Users") xtitle("") ///
				saving(`v'.gph, replace)
			graph export "${TABLE}/fig/hist_`v'.png", replace
			local ++i
		}
		graph combine n_st_user.gph n_dist_user.gph n_ym_user.gph
		graph export "${TABLE}/fig/user_variation.png", replace
	
	restore	
	*-----------------------------------
	* 4. Table: Forest Clearance
	*-----------------------------------

	* Per-District Month
	preserve
		collapse (first) dist_f_elec-dist_f_tran any_def, by(c_code_2011 year_month)
		eststo clear
		eststo B: estpost tabstat dist_f_elec-dist_f_tran if any_def, s(n mean p50 sd) c(s)
	restore
	
	* Project-wise area totals and shares
	use "${DATA}/dta/fc_clean", clear
	keep if prop_status == "approved"
	replace proj_cat = "other" if proj_cat == "industry" | proj_cat == "underground"
	
	preserve
	
		drop if proj_area_forest2 > 1000 // drop 3 megaprojects
		levelsof proj_cat, local(category)
		
		foreach cat of local category {
			
			gen `cat' = proj_area_forest2 if proj_cat == "`cat'"
			egen `cat'_sum = total(`cat')
			drop `cat'
			ren `cat'_sum `cat'
			replace `cat' = . if proj_cat != "`cat'"
		}
		
		* Label
		foreach v of varlist electricity-transportation {
			la var `v' "`v'"
			local x : variable label `v'
			local x = proper("`x'")
			la var `v' "`x'"
		}
		ren (electricity irrigation mining other resettlement transportation) ///
			(dist_f_elec dist_f_irr dist_f_mine dist_f_o dist_f_res dist_f_tran)
		
		* Totals
		eststo A: estpost tabstat dist_f_elec-dist_f_tran, s(n mean sd) c(s)
		
		esttab A B using "${TABLE}/tables/sumstats_deforest2.rtf", replace ///
			label cells("count(fmt(0)) mean(fmt(2)) sd(fmt(2))") ///
			mgroups("Totals" "Per District-Yearmonth", pattern(1 1)) ///
			collabel("N" "Mean" "Std. Dev.") ///
			refcat(dist_f_elec "Project Type" dist_f_hyb "Project Group", nolabel) ///
			nomtitle unstack nonumber
		eststo clear
	
		* Project Size
		eststo: estpost tabstat proj_area_forest2, s(n mean p50 sd min max) by(proj_cat)
		esttab using "${TABLE}/tables/sumstats_projsize.rtf", replace ///
			label cells("count(fmt(0) label(N)) mean(fmt(2) label(Mean)) p50(fmt(2) label(Median)) sd(fmt(2) label(Std. Dev.)) min(fmt(3) label(Min.)) max(fmt(3) label(Max))") ///
			nomtitle unstack nonumber noobs
		eststo clear
			
		*-----------------------------------
		* 3. Table: SPATIAL SPAN OF PROJECT
		*-----------------------------------
		egen num_districts = rownonmiss(district_0-district_9), s
		la var num_districts "Districts"
		drop if num_districts == 0
	
		eststo: estpost tabulate num_districts, nototal
		esttab using "${TABLE}/tables/spatial_dep.rtf", replace ///	
			cells("count(fmt(0)) pct(fmt(2)) cumpct(fmt(2))") ///
			nonumbers nomtitle collabels("" "%" "Cum.") ///
			noobs gap label 
		eststo clear
	
	restore
	
	* Project Size in Full Sample
	eststo: estpost tabstat proj_area_forest2, s(n mean p50 sd min max) by(proj_cat)
	esttab using "${TABLE}/tables/sumstats_projsize_full.rtf", replace ///
		label cells("count(fmt(0) label(N)) mean(fmt(2) label(Mean)) p50(fmt(2) label(Median)) sd(fmt(2) label(Std. Dev.)) min(fmt(3) label(Min.)) max(fmt(3) label(Max))") ///
		nomtitle unstack nonumber noobs
	eststo clear
	
}

*===============================================================================
* LEARNING CURVE
*===============================================================================
if `learning' == 1 {

	* Read
	use "${DATA}/dta/fc_ebd_user", clear
	drop_outliers

	// 1. Seasonality
	
	* Total
	preserve
		
		* Total species richness by ym across all users/trips
		collapse (first) sr_ym (sum) n_trips, by(year_month)
		replace n_trips = n_trips/1000
		twoway bar n_trips year_month, yscale(alt) bcolor(gs10) ///
			ytitle("Num. Trips (thousands)", size(medium) axis(alt)) ///
			ylabel(, angle(hor) labsize(medium)) || connected sr_ym year_month, ///
			yaxis(2) yscale(alt axis(2) titlegap(3)) ///
			ylabel(, axis(2) angle(hor) labsize(medium)) ///
			ytitle("Species Richness", size(medium) axis(2)) ///
			mcolor(gs3) msize(small) lcolor(black) ||, ///
			graphregion(color(white)) bgcolor(white) xtitle("") ///
			xlabel(660 "Jan-15" 666 "Jul-15" 672 "Jan-16" 678 "Jul-16" ///
			684 "Jan-17" 690 "Jul-17" 696 "Jan-18" 702 "Jul-18", ///
			labsize(medium)angle(45)) legend(order(2 "Species" "Richness" ///
			1 "Num." "Trips") size(medsmall)) fysize(37.5) ///
			title("A", pos(1)) saving(season.gph, replace) 
	restore

	/*
	// 2. Per User
	preserve 
		
		collapse (first) sr_uym, by(user_id year_month)
		statsby, by(year_month) clear: ci means sr_uym
		format year_month %tmCCYY-NN
		*ciplot s_richness_uym, by(year_month)
		
		twoway (rcap lb ub year_month, color(gs10)) ///
		   (connected mean year_month, mcolor(black) ///
		   lpattern(solid) lcolor(black) msymbol(o)), ///
		   yline(0) xline(0, lpattern(dash)) ///
		   graphregion(color(white)) bgcolor(white) ///
		   legend(on order(1 "95% CI" 2 "Mean")) ///
		   xtitle("Time", size(medium)) ytitle("Species Richness" "(Per User)", ///
		   size(medium)) yscale(titlegap(6)) xscale(titlegap(4)) ///
		   xlabel(660 "Jan-15" 666 "Jul-15" 672 "Jan-16" 678 "Jul-16" ///
		   684 "Jan-17" 690 "Jul-17" 696 "Jan-18" 702 "Jul-18", ///
		   angle(45) labsize(medsmall)) saving(season_u.gph, replace)
	
	restore
	*/
	
	// 3. Learning and No Learning Overlay
	
	* No learning
	preserve	
		
		* Residualize on User FE
		reghdfe sr, a(user_id c_code_2011 state_code_2011_num#month year) resid
	
		* Average residual per user-yearmonth
		collapse (mean) resid = _reghdfe_resid, by(user_id year)
		statsby, by(year) clear: ci means resid
		
		tempfile temp
		save "`temp'"
	
	restore
	
	* Linear experience
	preserve	
		
		* Residualize on User FE, linear trend
		reghdfe sr u_lin, a(user_id c_code_2011 state_code_2011_num#month year) resid
	
		* Average residual per user-yearmonth
		collapse (mean) resid = _reghdfe_resid, by(user_id year)
		statsby, by(year) clear: ci means resid
		ren * *_nex
		ren year_nex year
		tempfile temp2
		save "`temp2'"
	
	restore
	
	preserve
		
		* Residualize on User-Year FE
		reghdfe sr u_lin, a(user_id#year c_code_2011 state_code_2011_num#month) resid
		
		* Average residual per user-yearmonth
		collapse (mean) resid = _reghdfe_resid, by(user_id year)
		statsby, by(year) clear: ci means resid
		ren * *_nl
		ren year_nl year
		merge 1:1 year using "`temp'", nogen
		merge 1:1 year using "`temp2'", nogen
		
		twoway scatter mean year, mfcolor(white) mcolor(maroon) ///
			msize(small) || lfit mean year, lcolor(maroon) || ///
			scatter mean_nex year, mfcolor(white) mcolor(navy) ///
			msize(small) msymbol(triangle) || ///
			lfit mean_nex year, lcolor(navy) || ///
			scatter mean_nl year, mfcolor(white) mcolor(black) ///
			msize(small) msymbol(square) || ///
			lfit mean_nl year, lcolor(black) ||, ///
			graphregion(color(white)) bgcolor(white) ylabel(, angle(hor)) ///
			ytitle("Mean User Residual", size(medium)) xtitle("") ///
			yscale(titlegap(*-30)) ylabel(, labsize(medsmall)) ///
			legend(order(1 "Learning Bias" 3 "Minus Participation" ///
			5 "No Learning Bias") size(medsmall)) ///
			xlabel(, labsize(medsmall)) title("C", size(medium) pos(1)) ///
			fxsize(75) fysize(90) saving(learn_nl.gph, replace)

	restore

	*xlabel(660 "Jan-15" 666 "Jul-15" 672 "Jan-16" 678 "Jul-16" 684 "Jan-17" 690 "Jul-17" 696 "Jan-18" 702 "Jul-18", angle(45))

	// 4. BirdLife
	preserve
	
		* Quantiles of species richness
		xtile sr_bl_q3 = sr_bl, nquantiles(3)
		statsby, by(sr_bl_q3) clear noisily: ci means n_trips 
		
		twoway bar mean sr_bl_q3, barwidth(0.7) color(gs8) ///
			|| rcap ub lb sr_bl_q3, lcolor(black) ||, ///
			xlabel(1 "Low" 2 "Medium"  3 "High", labsize(medium)) ///
			ylabel(3(0.5)5, angle(hor) labsize(medsmall)) ///
			ytitle("Trips Per User-Month" "(eBird)", size(medium)) ///
			xtitle("True District Species Diversity" "(BirdLife)", size(medium)) ///
			yscale(titlegap(3)) xscale(titlegap(3)) ///
			graphregion(color(white)) bgcolor(white) ///
			legend(order(1 "Mean" 2 "95% CI") size(medium)) ///
			title("B", pos(1)) fysize(37.5) saving(dist.gph, replace)
	restore
	
	* combine
	graph combine season.gph dist.gph, rows(2) saving(comb.gph, replace)
	graph combine comb.gph learn_nl.gph, imargin(0 0 1 0)
	graph export "${TABLE}/fig/bias_plot.png", replace
	
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
		 sr dist_f_cum_km2 coverage ///
		 tree_cover temp rain all_species ///
		 group_size month year duration
	local controls coverage tree_cover temp rain all_species duration
	
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
	
	* Regression (try 12)
	reghdfe sr e_6 e_5 e_4 e_3 e_2 e_1 e_8-e_13 ///
		`controls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month)
	
	parmest, label list(parm label estimate min* max* p) saving(results, replace)
	use results, clear
	
	replace parm = label
	destring parm, replace force
	drop if parm == .
	
	twoway (rarea min95 max95 parm, sort color(gs13)) ///
		   (connected  estimate parm, sort mcolor(black) ///
		   lpattern(solid) lcolor(black) msymbol(o)), ///
		   yline(0) xline(0, lpattern(dash)) ///
		   graphregion(color(white)) bgcolor(white) ///
		   legend(on order(1 "95% CI" 2 "Coefficient")) ///
		   xtitle("Months Since Construction", size(medium)) ///
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
	local ctrls coverage tree_cover temp rain all_species duration
	drop_outliers
	
	la var dist_f_cum_km2 "Deforestation (\(km^{2}\))"
	
	* Fig 1 - Plot
	eststo clear
	eststo m1: reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' [aweight=n_trips], ///
		a(user_id c_code_2011_num state_code_2011_num#month year) ///
		vce(cl state_code_2011_num#year)
	eststo m2: reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) ///
		vce(cl state_code_2011_num#year)
		sum sr if e(sample)
	
	coefplot (m1, rename(dist_f_cum_km2 = "Learning Bias") \ m2, ///
		rename(dist_f_cum_km2 = "No Learning Bias")), ///
		keep(dist_f*) xline(0, lcolor(gs5) lpattern(dash)) ///
		coeflabels(, labsize(medium)) mlabsize(medium) ///
		ciopts(recast(rcap)) msize(small) mfcolor(white) msymbol(D) ///
		graphregion(color(white)) bgcolor(white) grid(glcolor(gs15)) ///
		mlabel format(%9.2g) mlabposition(12) mlabgap(*2) ///
		xlabel(, labsize(medium)) ylabel(, labsize(medium)) ///
		title("A") saving(main, replace)
	graph export "${TABLE}/fig/main.png", replace

	* Fig 2 - Project-Wise Impacts
	eststo clear
	eststo: reg_sat sr dist_f_elec_cum_km2 dist_f_irr_cum_km2 ///
		dist_f_mine_cum_km2 dist_f_o_cum_km2 dist_f_res_cum_km2 ///
		dist_f_tran_cum_km2 dist_nf_elec_cum_km2 dist_nf_irr_cum_km2 ///
		dist_nf_mine_cum_km2 dist_nf_o_cum_km2 dist_nf_res_cum_km2 ///
		dist_nf_tran_cum_km2 `ctrls'
	
	coefplot, keep(dist_f*) sort xline(0, lcolor(gs5) lpattern(dash)) ///
		msize(small) mfcolor(white) msymbol(D) ciopts(recast(rcap)) ///
		graphregion(color(white)) bgcolor(white) grid(glcolor(gs15))  ///
		mlabel format(%9.2g) mlabposition(12) /// 
		coeflabels(, labsize(medium)) mlabsize(medium) ///
		transform(* = min(max(@,-2),6)) xlabel(, labsize(medium)) ///
		ylabel(, labsize(medium)) mlabgap(*2) title("B") ///
		saving(projwise, replace)
	graph export "${TABLE}/fig/projwise.png", replace
	
	graph combine main.gph projwise.gph
	graph export "${TABLE}/fig/main_results.png", replace

}

*===============================================================================
* SIMULATION
*===============================================================================
if `simulation' == 1 {
	
	* Read 
	use "${DATA}/dta/fc_ebd_user", clear
	
	*-----------------------------------
	* State-wise constraint
	*-----------------------------------
	
	// State-wise mean species per dist
	preserve
		collapse (first) sr_bl state_code_2011, by(c_code_2011)
		collapse (mean) sr_bl_st = sr_bl, by(state_code_2011)
		tempfile temp 
		save "`temp'"
	restore
	merge m:1 state_code_2011 using "`temp'", nogen
	g sr_max = sr_bl_st / 12
	
	*-----------------------------------
	* REDUCTION POLICY
	*-----------------------------------
	
	* Read
	drop_outliers
	local ctrls coverage tree_cover temp rain all_species duration
	
	* Baseline
	reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls'

	* Store
	gen b_cons = _b[_cons]
	foreach v of varlist dist_f_cum_km2 dist_nf_cum_km2 `ctrls' {
		
		gen b_`v' = _b[`v']
	}
	
	foreach v of varlist `ctrls' {
		
		egen `v'_mean = mean(`v')
	}
	
	forvalues i = 95(-5)5 {
		
		di "Simulating Diversity Under `i' % Deforestation..."
		
		* Simulate
		foreach v in dist_f dist_nf {
			
			gen `v'_red_`i' = 0 if `v'_cum_km2 != .
			replace `v'_red_`i' = `v'_cum_km2 * (`i'/100) if `v'_cum_km2 > 0 & `v'_cum_km2 < .
		}

		* Predicted
		gen sr_red_`i' = b_cons + (b_dist_f_cum_km2*dist_f_red_`i') + ///
			(b_dist_nf_cum_km2*dist_nf_red_`i') + (b_coverage*coverage_mean) + ///
			(b_tree_cover*tree_cover_mean) + (b_temp*temp_mean) + (b_rain*rain_mean) + ///
			(b_all_species*all_species_mean) + (b_duration*duration_mean) + ///
			__hdfe1__ + __hdfe2__ + __hdfe3__ + _reghdfe_resid
		
	}
	
	* State-wise counterfactuals 
	preserve
		
		collapse (first) sr_max (mean) sr_red_*, by(state) 
		drop if sr_red_30 == .
		drop if sr_red_95 == sr_red_90
		reshape long sr_red_, i(state) j(decline)
		replace decline = 100 - decline
	
		// Export for better graphics in R
		export delimited using "${DATA}/csv/sim_reduction.csv", replace
	
	restore
	
	*-----------------------------------
	* RELOCATION POLICY
	*-----------------------------------
	drop *_red_*
	
	forvalues i = 95(-5)5 {
		
		di "Simulating Diversity Under `i' % Deforestation..."
		
		* Reduce Forest Diversion by x%
		gen dist_f_rel_`i' = 0 if dist_f_cum_km2 != .
		replace dist_f_rel_`i' = dist_f_cum_km2 * (`i'/100) if dist_f_cum_km2 > 0 & dist_f_cum_km2 < .
		
		* Shift forest to non-forest diversion
		gen dist_nf_rel_`i' = dist_nf_cum_km2 + dist_f_cum_km2*((100-`i')/100)
		
		* Predicted
		gen sr_rel_`i' = b_cons + (b_dist_f_cum_km2*dist_f_rel_`i') + ///
			(b_dist_nf_cum_km2*dist_nf_rel_`i') + (b_coverage*coverage_mean) + ///
			(b_tree_cover*tree_cover_mean) + (b_temp*temp_mean) + (b_rain*rain_mean) + ///
			(b_all_species*all_species_mean) + (b_duration*duration_mean) + ///
			__hdfe1__ + __hdfe2__ + __hdfe3__ + _reghdfe_resid	
	
	}
	
	collapse (first) sr_max (mean) sr_rel_*, by(state) 
	drop if sr_rel_30 == .
	drop if sr_rel_95 == sr_rel_90
	reshape long sr_rel_, i(state) j(decline)
	replace decline = 100 - decline
	
	// Export for better graphics in R
	export delimited using "${DATA}/csv/sim_relocation.csv", replace

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
		local ctrls coverage tree_cover temp rain all_species duration
		local slx_b dist_f_cum_km2_slx_bc dist_nf_cum_km2_slx_bc
		
		*------------------------
		* SPATIAL SPILLOVERS
		*------------------------
		
		* Project-Wise
		la var dist_f_cum_km2 "All"
		eststo all: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
		eststo all_w: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' `slx_b'
		
		// Baseline
		eststo proj: reg_sat sr dist_f_elec_cum_km2 dist_f_irr_cum_km2 ///
			dist_f_mine_cum_km2 dist_f_o_cum_km2 dist_f_res_cum_km2 ///
			dist_f_tran_cum_km2 dist_nf_elec_cum_km2 dist_nf_irr_cum_km2 ///
			dist_nf_mine_cum_km2 dist_nf_o_cum_km2 dist_nf_res_cum_km2 ///
			dist_nf_tran_cum_km2 `ctrls'
		// SLX
		eststo proj_w: reg_sat sr ///
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
				 dist_f_mine_cum_km2) xlabel(, labsize(medsmall))
		graph export "${TABLE}/fig/projwise_slx.png", replace
		
		* All Projects
		
		// PANEL A: SPATIAL SPILLOVERS
		la var dist_f_cum_km2 "Direct Effect"
		la var dist_f_cum_km2_slx_bc "Spillover Effect"
		eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' `slx_b' // binary cont.
		
		coefplot, keep(dist_f*) xline(0, lcolor(gs5) lpattern(dash)) ///
			ciopts(recast(rcap)) msize(small) mfcolor(white) msymbol(D) ///
			coeflabels(, labsize(large)) graphregion(color(white)) ///
			bgcolor(white) grid(glcolor(gs15)) mlabel format(%9.2g) ///
			mlabposition(12) mlabgap(*2) mlabsize(medium) xlabel(, labsize(medsmall)) ///
			title("Panel A: Spatial Lag", size(medium)) saving(slx, replace)
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
		reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' 
		eststo nolag: lincomest dist_f_cum_km2
		
		// 1 month
		reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 ///
			dist_f_cum_km2_l1 dist_nf_cum_km2_l1 `ctrls'
		eststo lag_1: lincomest dist_f_cum_km2_l1 + dist_f_cum_km2 
		
		// 3 month
		reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 *_l1 *_l2 *_l3 `ctrls'
		eststo lag_3: lincomest dist_f_cum_km2_l3 + dist_f_cum_km2_l2 + dist_f_cum_km2_l1 + dist_f_cum_km2 
		
		// 5 month
		reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 *_l1 *_l2 *_l3 *_l4 *_l5 `ctrls'
		eststo lag_5: lincomest dist_f_cum_km2_l5 + dist_f_cum_km2_l4 + dist_f_cum_km2_l3 + dist_f_cum_km2_l2 + dist_f_cum_km2_l1 + dist_f_cum_km2
		
		coefplot (nolag, rename((1) = "Baseline") \ lag_1, rename((1) = "Sum L0-L1") \ ///
			lag_3, rename((1) = "Sum L0-L3") \ lag_5, rename((1) = "Sum L0-L5")), ///
			xline(0, lcolor(gs5) lpattern(dash)) msymbol(D) msize(small) ///
			mfcolor(white) ciopts(recast(rcap)) coeflabels(, labsize(large)) ///
			graphregion(color(white)) bgcolor(white) grid(glcolor(gs15)) ///
			mlabel format(%9.2g) mlabposition(12) mlabgap(*2) mlabsize(medium) ///
			xlabel(-0.5(0.25)0, labsize(medsmall)) title("Panel B: Temporal Lag", size(medium)) saving(dlag, replace)
		eststo clear
		graph combine slx.gph dlag.gph
		graph export "${TABLE}/fig/spillovers.png", replace
	}
	
	if `others' == 1 {
		
		* Read
		use "${DATA}/dta/fc_ebd_user", clear
		drop_outliers
	
		* Controls
		local ctrls coverage tree_cover temp rain all_species duration
		local slx_b dist_f_cum_km2_slx_bc dist_nf_cum_km2_slx_bc
		la var dist_f_cum_km2 "Deforestation (Stage 2)"
		la var dist_f_cum_km2_slx_bc "SLX Deforestation (Stage 2)"
		
		// Additional Controls/FEs
		
		* 1. Additional Controls
		eststo clear
		eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' ///
			group_size distance
		
		* 2. User x Month FE 
		eststo: reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 ///
			`ctrls' n_mon_yr u_lin, ///
			a(user_id#month c_code_2011_num state_code_2011_num#year) ///
			vce(cl state_code_2011_num#year)
				estadd local user_m_fe "x"
				estadd local dist_fe "x"
				estadd local st_y_fe "x"
				estadd local u_trend "Linear"
		
		* 3. User x Year, Cubic Trend
		eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' u_cubic
			estadd local u_trend "Cubic"
		
		// SLX Weight Matrix
		
		* 4. W = 1/d
		preserve
			drop *_slx_bc
			ren *_slx_i *_slx_bc
			eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' `slx_b'
				estadd local u_trend "None"
			drop *_slx_bc
		
		* 5. W = 1/d^2
			ren *_slx_i2 *_slx_bc
			eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' `slx_b'
				estadd local u_trend "None" 
		restore
		
		// Illegal Deforestation
		
		* 6. Stage 1
		eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' *_km2_s1
			estadd local u_trend "None"
		
		* 7. Stage 1 SLX
		eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' *_s1
			estadd local u_trend "None"
		
		// Alternative Outcomes
		
		* 8. Semi-IHS
		*eststo: reg_sat sr dist_f_cum_ihs dist_nf_cum_ihs `ctrls'
		*	estadd local u_trend "None"
			
		* 9. Species Richness (across all trips in dist-ym)
		eststo: reg_sat sr_udym dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
			estadd local u_trend "None"
			sum sr_udym if e(sample)
	
		* 10. Only trips where all species reported
		preserve
			
			use "${DATA}/dta/fc_ebd_user_allreported", clear
			drop_outliers
			
			eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
				estadd local u_trend "None"
				
		restore

		* 11 Windsorize at 99%
		preserve
			
			use "${DATA}/dta/fc_ebd_user_trunc99", clear
			drop_outliers
			
			eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
				estadd local u_trend "None"
				
		restore
		
		* 12 Full Data
		preserve
			
			use "${DATA}/dta/fc_ebd_user_full", clear
			drop_outliers
			drop dist_f_cum_km2 dist_nf_cum_km2
			ren dist_f_cum_ihs dist_f_cum_km2
			ren dist_nf_cum_ihs dist_nf_cum_km2
			la var dist_f_cum_km2 "Deforestation (Stage 2)"
			eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
				estadd local u_trend "None"
				
		restore
				
		esttab using "${TABLE}/tables/robustness.rtf", replace ///
			keep(dist_f*) stats(user_m_fe user_y_fe dist_fe st_y_fe ///
			st_m_fe u_trend N r2, labels(`"User x Month FEs"' `"User x Year FEs"' ///
			`"District FEs"' `"State x Year FEs"' `"State x Month FEs"' ///
			`"User Trend"' `"N"' `"R2"') fmt(0 0 0 0 0 0 0 3)) ///
			mlabel("Add. Controls" "User-Month" "User-Year" "W=1/d" "W=1/d^2" "Stage 1" "SLX Stage 1" ///
			"All Trips" "All Species Reported" "Truncate" "Full (IHS)") ///
			star(* .1 ** .05 *** .01) label nomtitles nonotes se b(%5.3f) ///
			se(%5.3f)
		eststo clear
	}
	
}



