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
cd "${TABLE}"

// Modules
local sumstats			0
local learning			0
local event_study		0
local main_analysis		0
local simulation		0
local robustness		1
	local spillovers	1
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
	/*
	esttab A B using "${TABLE}/tables/sumstats_biodiv.rtf", replace main(mean) ///
		aux(sd) cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		mgroups("Deforestation Districts" "Non-deforestation Districts", pattern(1 1)) ///
		collabel("Mean" "Std. Dev." "N") refcat(tot_area "District Variables", ///
		nolabel) nomtitle noobs label unstack nonumber
	*/
	esttab A B using "/Users/rmadhok/Dropbox (Personal)/dissertation/proposal/tables/sumstats_ebd.tex", ///
		replace f main(mean) aux(sd) cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		mgroups("Deforestation Districts" ///
		"Non-deforestation Districts", pattern(1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		collabel("Mean" "Std. Dev." "N", prefix({) suffix(})) ///
		refcat(tot_area "\underline{\emph{District Variables}}" , nolabel) ///
		nomtitle booktabs noobs label unstack nonumber
		
	** Birdwatching Details
	eststo clear
	eststo A: estpost tabstat `trip' if any_def, s(n mean med sd) c(s)
	eststo B: estpost tabstat `trip' if !any_def, s(n mean med sd) c(s)
	
	/*
	esttab A B using "${TABLE}/tables/sumstats_biodiv.rtf", append main(mean) aux(sd) ///
		cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		refcat(sr "Outcome" , nolabel) nomtitle collabel(none) ///
		noobs label unstack nonumber plain
	*/	
	
	esttab A B using "/Users/rmadhok/Dropbox (Personal)/dissertation/proposal/tables/sumstats_ebd.tex", ///
		append f main(mean) aux(sd) cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		refcat(sr "\underline{\emph{Outcome}}" , nolabel) nomtitle collabel(none) ///
		width(\hsize) nomtitle booktabs noobs label ///
		unstack nonumber plain
	
	** Covariates
	eststo clear
	eststo A: estpost tabstat `covariates' if any_def, s(n mean med sd) c(s)
	eststo B: estpost tabstat `covariates' if !any_def, s(n mean med sd) c(s)
	
	/*
	esttab A B using "${TABLE}/tables/sumstats_biodiv.rtf", append main(mean) ///
		aux(sd) cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		refcat(tree_cover "Covariates" , nolabel) nomtitle collabel(none) ///
		noobs label unstack nonumber plain ///
		addnotes("Note: Spatial coverage is the fraction of grid cells in a district" ///
		"containing at least one bird observation over the study period using a" ///
		"5km x 5km grid")
	*/
	
	esttab A B using "/Users/rmadhok/Dropbox (Personal)/dissertation/proposal/tables/sumstats_ebd.tex", ///
		append f main(mean) aux(sd) cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		width(\hsize) refcat(tree_cover "\underline{\emph{Covariates}}" , nolabel) ///
		nomtitle collabel(none) booktabs noobs label unstack nonumber plain ///
		addnotes("Note:") 
	
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
				saving("${TABLE}/fig/`v'.gph", replace)
			graph export "${TABLE}/fig/hist_`v'.png", replace
			local ++i
		}
		graph combine "${TABLE}/fig/n_st_user.gph" "${TABLE}/fig/n_dist_user.gph" "${TABLE}/fig/n_ym_user.gph", iscale(1.1)
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
			title("A", pos(1) size(large)) saving(season.gph, replace) 
	restore
	
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
			legend(order(1 "Full Learning Bias" 3 "Participation Covariate" ///
			5 "No Learning Bias") size(medsmall) position(6)) ///
			xlabel(, labsize(medsmall)) title("C", size(medium) pos(1)) ///
			fxsize(75) fysize(90) saving(learn_nl.gph, replace)

	restore

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
			title("B", pos(1) size(large)) fysize(37.5) saving(dist.gph, replace)
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
	
	//1. EVENT STUDY OF FIRST PROJECT OPENING
	
	* Prep
	drop_outliers
	keep user_id *code* year_month n_trips ///
		 sr dist_f_cum_km2 coverage ///
		 tree_cover temp rain all_species ///
		 group_size month year duration u_lin
	local controls coverage tree_cover temp rain all_species duration
	
	* Event Date
	bys c_code_2011 (year_month): gen e_date = year_month if ///
		dist_f_cum_km2[_n] != dist_f_cum_km2[_n-1]
	bys c_code_2011 (year_month): replace e_date = . if _n == 1 
	
	* Fill
	bys c_code_2011 (year_month): carryforward e_date, gen(event_date)
	gen nym = -year_month
	bysort c_code_2011 (nym): carryforward event_date, replace
	sort c_code_2011 year_month
	drop nym
	
	* Keep First Project
	bys c_code_2011 (year_month): egen first = min(event_date)
	keep if event_date == first
	drop if event_date == . & first == .
	format event_date %tmCCYY-NN
	
	* Time to Construction
	gen dif = year_month - event_date
	*gen dif = (year_month-event_date) + 1 // relative to month before construction
	keep if inrange(dif, -6, 10) // 6 month window
	
	* Event dummies
	tostring dif, replace
	tab dif, gen(e_)
	foreach v of varlist e_* {
		local x : variable label `v'
		local y = subinstr("`x'", "dif==", "",.)
		la var `v' "`y'"
	}
	
	* Regression (try 12)
	reghdfe sr e_6 e_5 e_4 e_3 e_2 e_1 e_8 e_10-e_17 e_9 ///
		`controls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month)
	
	parmest, label list(parm label estimate min* max* p) saving(results, replace)
	use results, clear
	
	replace parm = label
	destring parm, replace force
	drop if parm == . | p == .
	
	twoway (rcap min95 max95 parm, sort color(gs5) lpattern(shortdash)) ///
		   (connected  estimate parm, sort mcolor(black) mfcolor(white) ///
		   lpattern(solid) lcolor(black) msymbol(D)), ///
		   yline(0, lcolor(maroon) lpattern(solid)) ///
		   xline(0, lcolor(maroon) lpattern(solid)) ///
		   legend(on order(1 "95% CI" 2 "Coefficient")) ///
		   xtitle("Months Since Construction", size(medium)) ///
		   ytitle("Species Richness Relative" "To Month of Construction", ///
		   size(medium)) yscale(titlegap(6)) xlabel(-6(2)10) ///
		   saving("${TABLE}/fig/event_study.gph", replace)
	graph export "${TABLE}/fig/event_study.png", width(1000) replace
	
	/*
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
	*/
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
	
	* Main Results - Table
	eststo clear
	eststo m1: reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' [aweight=n_trips], ///
		a(user_id c_code_2011_num state_code_2011_num#month year) ///
		vce(cl state_code_2011_num#year)
		
		estadd local user_fe "x"
		estadd local dist_fe "x"
		estadd local st_m_fe "x"	
		
	eststo m2: reghdfe sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) ///
		vce(cl state_code_2011_num#year)
		
		estadd local user_y_fe "x"
		estadd local dist_fe "x"
		estadd local st_m_fe "x"
		sum sr if e(sample)

	* Main Results - Coefficient Plot
	coefplot (m1, rename(dist_f_cum_km2 = `" "(1) Learning" "Bias" "') \ m2, ///
		rename(dist_f_cum_km2 = `" "(2) No Learning" "Bias" "')), ///
		keep(dist_f*) xline(0, lcolor(maroon) lpattern(solid)) ///
		coeflabels(, labsize(medium)) mlabsize(medium) mlabcolor(black) ///
		ciopts(recast(rcap) lpattern(shortdash) lcolor(gs5)) msize(medium) ///
		mfcolor(white) msymbol(D) mlcolor(black) mlabel format(%9.2g) ///
		mlabposition(1) mlabgap(*2) xlabel(, labsize(medium)) ///
		ylabel(, labsize(medium)) title("A", size(large)) ///
		saving("${TABLE}/fig/main.gph", replace)
	graph export "${TABLE}/fig/main.png", replace

	* Table
	esttab using "${TABLE}/tables/main.rtf", replace ///
		stats(user_fe user_y_fe dist_fe st_m_fe N r2, ///
		labels(`"User FEs"' `"User x Year FEs"' ///
		`"District FEs"' `"State x Month FEs"'  `"N"' `"R2"') fmt(0 0 0 0 0 3)) ///
		mlabel("Learning Bias" "No Learning Bias") star(* .1 ** .05 *** .01) ///
		label nomtitles nonotes se b(%5.3f) se(%5.3f)
	eststo clear

	* Fig 2 - Project-Wise Impacts
	eststo clear
	eststo: reg_sat sr dist_f_elec_cum_km2 dist_f_irr_cum_km2 ///
		dist_f_mine_cum_km2 dist_f_o_cum_km2 dist_f_res_cum_km2 ///
		dist_f_tran_cum_km2 dist_nf_elec_cum_km2 dist_nf_irr_cum_km2 ///
		dist_nf_mine_cum_km2 dist_nf_o_cum_km2 dist_nf_res_cum_km2 ///
		dist_nf_tran_cum_km2 `ctrls'
	
	coefplot, keep(dist_f*) sort xline(0, lcolor(maroon) lpattern(solid)) ///
		msize(small) mfcolor(white) msymbol(D) ///
		ciopts(recast(rcap) lpattern(shortdash) lcolor(gs5)) ///
		mlabel format(%9.2g) mlabposition(1) mlabcolor(black) /// 
		coeflabels(, labsize(medium)) mlabsize(medium) mcolor(black) ///
		transform(* = min(max(@,-2),6)) xlabel(, labsize(medium)) ///
		ylabel(, labsize(medium)) mlabgap(*2) title("B", size(large)) ///
		saving("${TABLE}/fig/projwise.gph", replace)
	graph export "${TABLE}/fig/projwise.png", replace

	* Fig 3 - Fragmentation Wise
	g bin_0 = (dist_f_cum_km2 == 0)
	g bin_2 = (dist_f_cum_km2 > 0 & dist_f_cum_km2 <=2)
	g bin_4 = (dist_f_cum_km2 > 2 & dist_f_cum_km2 <=4)
	g bin_6 = (dist_f_cum_km2 > 4 & dist_f_cum_km2 <=6)
	g bin_8 = (dist_f_cum_km2 > 6 & dist_f_cum_km2 <=8)
	g bin_max = (dist_f_cum_km2 > 8)
	
	local labels `" "0" "(0-2]" "(2-4]" "(4-6]" "(6-8]" "> 8" "'
	local i=1
	foreach x in 0 2 4 6 8 max {
		local lab: word `i' of `labels'
		la var bin_`x' "`lab'"
		local ++i
	}
	
	eststo clear
	eststo: qui reghdfe sr bin_2-bin_max dist_nf_cum_km2 `ctrls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) ///
		vce(cl state_code_2011_num#year)

	coefplot, keep(bin_*) xline(0, lcolor(maroon) lpattern(solid)) ///
		msize(small) mfcolor(white) msymbol(D) mlabcolor(black) ///
		ciopts(recast(rcap) lpattern(shortdash) lcolor(gs5)) ///
		mlabel ytitle("Cuml. Patch Area (km{sup:2})", size(large)) format(%9.2g) mlabposition(1) /// 
		coeflabels(, labsize(medium)) mlabsize(medium) mcolor(black) ///
		xlabel(-20(5)10, labsize(medium)) ylabel(, labsize(medium)) ///
		mlabgap(*2) title("C", size(large)) ///
		saving("${TABLE}/fig/fragwise.gph", replace)
	graph export "${TABLE}/fig/fragwise.png", replace

	* Patch-wise
	egen patch_cat = cut(n_patches_cum), at(0, 10, 20, 30, 40, 50, 60, 70)
	table patch_cat, contents(min n_patches_cum max n_patches_cum)
	tab patch_cat, gen(patch_cat_)
	foreach v of varlist patch_cat_* { // interactions
		g dist_f_`v' = dist_f_cum_km2 * `v'
	}

	local labels `" "[0-9]" "[10-19]" "[20-29]" "[30-39]" "[40-49]" "[50-59]" "> 60" "'
	local i=1
	forvalues x=1/7 {
		local lab: word `i' of `labels'
		la var dist_f_patch_cat_`x' "`lab'"
		local ++i
	}
	eststo clear
	eststo: qui reghdfe sr dist_f_patch_cat_2-dist_f_patch_cat_7 ///
		dist_f_cum_km2 patch_cat* dist_nf_cum_km2 `ctrls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) ///
		vce(cl state_code_2011_num#year)

	coefplot, keep(dist_f_patch_*) xline(0, lcolor(maroon) lpattern(solid))  ///
		msize(small) mfcolor(white) msymbol(D) mcolor(black) ///
		ciopts(recast(rcap) lpattern(shortdash) lcolor(gs5)) ///
		mlabel ytitle("Number of Patches", size(large)) format(%9.2g) ///
		mlabposition(1) coeflabels(, labsize(medium)) mlabcolor(black) ///
		mlabsize(medium) xlabel(-30(5)10, labsize(medium)) ///
		ylabel(, labsize(medium)) mlabgap(*2) title("A", size(large)) ///
		saving("${TABLE}/fig/patchwise.gph", replace)
	graph export "${TABLE}/fig/patchwise.png", replace
	
	* Tree-cover wise
	egen tcover_cat = cut(tree_cover), at(0, 10, 20, 30, 40, 50, 60, 80)
	table tcover_cat, contents(min tree_cover max tree_cover)
	tab tcover_cat, gen(tcover_cat_)
	foreach v of varlist tcover_cat_* { // interactions
		g dist_f_`v' = dist_f_cum_km2 * `v'
	}

	local labels `" "[0-9]" "[10-19]" "[20-29]" "[30-39]" "[40-49]" "[50-59]" "> 60" "'
	local i=1
	forvalues x=1/7 {
		local lab: word `i' of `labels'
		la var dist_f_tcover_cat_`x' "`lab'"
		local ++i
	}
	
	eststo clear
	eststo: reghdfe sr dist_f_tcover_cat_1-dist_f_tcover_cat_6 ///
		dist_f_cum_km2 tcover_cat_* dist_nf_cum_km2 `ctrls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) ///
		vce(cl state_code_2011_num#year)
	
	coefplot, keep(dist_f_tcover_*) xline(0, lcolor(maroon) lpattern(solid))  ///
		msize(small) mfcolor(white) msymbol(D) mcolor(black) ///
		ciopts(recast(rcap) lpattern(shortdash) lcolor(gs5)) ///
		mlabel ytitle("Forest Cover (%)", size(large)) format(%9.2g) ///
		mlabposition(1) coeflabels(, labsize(medium)) mlabsize(medium) ///
		mlabcolor(black) xlabel(, labsize(medium)) ylabel(, labsize(medium)) ///
		mlabgap(*2) transform(* = min(max(@,-5),5)) title("B", size(large)) ///
		saving("${TABLE}/fig/tcoverwise.gph", replace)
	graph export "${TABLE}/fig/tcoverwise.png", replace

	* MAIN RESULTS
	graph combine "${TABLE}/fig/main.gph" "${TABLE}/fig/projwise.gph" ///
		"${TABLE}/fig/fragwise.gph", holes(4)
	graph export "${TABLE}/fig/main_results.png", width(1000) replace
	
	* PATCH GRAPH
	graph combine "${TABLE}/fig/patchwise.gph" "${TABLE}/fig/tcoverwise.gph"
	graph export "${TABLE}/fig/patch_analysis.png", width(1000) replace
		

}

*===============================================================================
* SIMULATION
*===============================================================================
if `simulation' == 1 {
	
	* Read 
	use "${DATA}/dta/fc_ebd_user", clear
	
	* Read
	drop_outliers
	local ctrls coverage tree_cover temp rain all_species duration
	
	* Save Main coefficient
	reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
	local beta = _b[dist_f_cum_km2]
	
	*----------------------------------------------------
	* BACKLOG BETWEEN 1975-2018
	*----------------------------------------------------
	
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
	keep if !inlist(proposal_status, "APPROVED", "REJECTED", "RETURNED", "REVOKED", "CLOSED", "WITHDRAWN")
	drop if area_applied > 1000 // 82 mega projects
	sum area_applied, d
	local total_post = r(sum)
	
	local area_total = (`total_post' + `total_pre')/100 // 3817 km2 under review from 1975-2018
	
	*----------------------------------------------------
	* APPROVAL OF BACKLOG
	*----------------------------------------------------
	
	//1. Policy 1: Spread over time
	
	clear
	
	* All Projects Approved
	input num_meetings 
	1
	3
	6
	9
	12
	15
	18
	21
	24
	27
	30
	end
	
	* Simulation vars
	g years = _n-1
	g backlog = `area_total' // total backlog since 1975
	g dist_f_ym = backlog / 180 / num_meetings // backlog per dist-time period
	g sr_decline = dist_f_ym * `beta' 
	g sr_frac = (sr_decline / 19.5)*100 // sr decline relative to mean
	
	* Plot
	line sr_frac years, ///
		ytitle("Species Richness Relative" "to Baseline Mean (%)") ///
		xtitle("Number of Years") xlabel(0 "Tmw." 1 "1" 2 "2" 3 "3" ///
		4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10") ///
		title("A") saving(backlog1.gph, replace)
	
	
	//2. Fix time, change approval rate
	
	clear

	* All Projects Approved
	input approval_rate 
	100
	90
	80
	70
	60
	50
	40
	30
	20
	10
	end
	
	* Simulation vars
	g backlog = `area_total'
	g dist_f_ym = backlog * (approval_rate / 100) / 180 / 12
	g sr_decline = dist_f_ym * `beta'
	g sr_frac = (sr_decline / 19.5)*100
	
	* Plot
	g order = -approval_rate
	labmask order, values(approval_rate) 
	line sr_frac order, ///
		ytitle("Species Richness Relative" "to Baseline Mean (%)") ///
		xtitle("Approval Rate") xlabel(, valuelabel) ///
		title("B") saving(backlog2.gph, replace)
	
	graph combine backlog1.gph backlog2.gph, iscale(1.2) ysize(3)
	graph export "${TABLE}/fig/simulations.png", replace

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
		/*
		*------------------------
		* SPATIAL SPILLOVERS
		*------------------------
		
		* Project-Wise
		la var dist_f_cum_km2 "All"
		eststo all: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
		eststo all_w: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' `slx_b'
		
		// Baseline
		eststo proj: qui reg_sat sr dist_f_elec_cum_km2 dist_f_irr_cum_km2 ///
			dist_f_mine_cum_km2 dist_f_o_cum_km2 dist_f_res_cum_km2 ///
			dist_f_tran_cum_km2 dist_nf_elec_cum_km2 dist_nf_irr_cum_km2 ///
			dist_nf_mine_cum_km2 dist_nf_o_cum_km2 dist_nf_res_cum_km2 ///
			dist_nf_tran_cum_km2 `ctrls'
		// SLX
		eststo proj_w: qui reg_sat sr ///
			dist_f_elec_cum_km2 dist_nf_elec_cum_km2 dist_f_elec_cum_km2_slx_bc dist_nf_elec_cum_km2_slx_bc ///
			dist_f_irr_cum_km2 dist_nf_irr_cum_km2 dist_f_irr_cum_km2_slx_bc dist_nf_irr_cum_km2_slx_bc ///
			dist_f_mine_cum_km2 dist_nf_mine_cum_km2 dist_f_mine_cum_km2_slx_bc dist_nf_mine_cum_km2_slx_bc ///
			dist_f_o_cum_km2 dist_nf_o_cum_km2 dist_f_o_cum_km2_slx_bc dist_nf_o_cum_km2_slx_bc ///
			dist_f_res_cum_km2 dist_nf_res_cum_km2 dist_f_res_cum_km2_slx_bc dist_nf_res_cum_km2_slx_bc ///
			dist_f_tran_cum_km2 dist_nf_tran_cum_km2 dist_f_tran_cum_km2_slx_bc dist_nf_tran_cum_km2_slx_bc ///
			`ctrls'
		
		coefplot (all proj, label("Baseline")) ///
				 (all_w proj_w, label("SLX")), ///
				 keep(dist_f*km2) xline(0, lcolor(maroon) lpattern(solid)) ///
				 ciopts(recast(rcap) lpattern(shortdash)) ///
				 msymbol(D) mfcolor(white) msize(small) ///
				 xlabel(, labsize(large)) ylabel(, labsize(large)) ///
				 transform(* = min(max(@,-5),5)) ///
				 order(dist_f_cum_km2 dist_f_res_cum_km2 dist_f_o_cum_km2 ///
				 dist_f_tran_cum_km2 dist_f_elec_cum_km2 dist_f_irr_cum_km2 ///
				 dist_f_mine_cum_km2)
		graph export "${TABLE}/fig/projwise_slx.png", replace
		*/
		* All Projects
		
		// PANEL A: SPATIAL SPILLOVERS
		la var dist_f_cum_km2 "Direct Effect"
		la var dist_f_cum_km2_slx_bc "Spillover Effect"
		eststo: reg_sat sr dist_f_cum_km2 dist_nf_cum_km2 `ctrls' `slx_b' // binary cont.
		
		coefplot, keep(dist_f*) xline(0, lcolor(maroon) lpattern(solid)) ///
			ciopts(recast(rcap) lpattern(shortdash) lcolor(gs5)) ///
			msize(small) mfcolor(white) msymbol(D) mlcolor(black) ///
			coeflabels(, labsize(large)) mlabel format(%9.2g) ///
			mlabposition(12) mlabgap(*2) mlabsize(medium) mlabcolor(black) ///
			xlabel(, labsize(large)) title("Panel A: Spatial Lag", size(large)) ///
			saving("${TABLE}/fig/spatial.gph", replace)
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
			xline(0, lcolor(maroon) lpattern(solid)) msymbol(D) msize(small) ///
			mfcolor(white) ciopts(recast(rcap) lcolor(gs5) lpattern(shortdash)) ///
			coeflabels(, labsize(large)) mlabel format(%9.2g) mlcolor(black) ///
			mlabposition(12) mlabgap(*2) mlabsize(medium) mlabcolor(black) ///
			xlabel(-1.5(0.5)0, labsize(large)) title("Panel B: Temporal Lag", ///
			size(large)) saving("${TABLE}/fig/temporal.gph", replace) 
		eststo clear
		graph combine "${TABLE}/fig/spatial.gph" "${TABLE}/fig/temporal.gph"
		graph export "${TABLE}/fig/spillovers.png", width(1000) replace
		
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



