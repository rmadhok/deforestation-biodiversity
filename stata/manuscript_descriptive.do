*********************************************************************************
*																			
* PROJECT: 	Deforestation and Biodiversity										
*																			
* PURPOSE: 	MANUSCRIPT - DESCRIPTIVE WORK (TEX)   
*																			 				 							
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
local sumstats			0
local learning			1
local event_study		0

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
	foreach v of varlist dist_f_cum dist_f_*_cum* tree* pop* ///
		sr coverage n_* duration distance temp rain all* { 
	
			label variable `v' `"\hspace{0.2cm} `: variable label `v''"'	
		}

	** Collect
	local dist_vars n_users_dist n_trips_dist pop_density
	local trip sr
	local covariates tree_cover_pct coverage rain temp duration all_species
	
	*---------------------------------------
	* 1. TABLE: OUTCOMES AND COVARIATES
	*---------------------------------------

	** District Variables
	eststo clear
	eststo A: estpost tabstat `dist_vars' if tag_d & any_def, s(n mean sd) c(s)
	eststo B: estpost tabstat `dist_vars' if tag_d & !any_def, s(n mean sd) c(s)

	esttab A B using "${TABLE}/tables/sumstats_ebd.tex", replace f main(mean) aux(sd) ///
		cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		mgroups("Deforestation Districts" "Non-deforestation Districts", pattern(1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		collabel("Mean" "Std. Dev." "N", prefix({) suffix(})) ///
		refcat(n_users_dist "\underline{\emph{District Variables}}" , nolabel) ///
		nomtitle booktabs noobs label unstack nonumber

	** Birdwatching Details
	eststo clear
	eststo A: estpost tabstat `trip' if any_def, s(n mean med sd) c(s)
	eststo B: estpost tabstat `trip' if !any_def, s(n mean med sd) c(s)

	esttab A B using "${TABLE}/tables/sumstats_ebd.tex", append f main(mean) aux(sd) ///
		cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		refcat(sr "\underline{\emph{Outcome}}" , nolabel) nomtitle collabel(none) ///
		width(\hsize) nomtitle booktabs noobs label unstack nonumber plain
	
	** Covariates
	eststo clear
	eststo A: estpost tabstat `covariates' if any_def, s(n mean med sd) c(s)
	eststo B: estpost tabstat `covariates' if !any_def, s(n mean med sd) c(s)
	
	esttab A B using "${TABLE}/tables/sumstats_ebd.tex", append f main(mean) aux(sd) ///
		cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		width(\hsize) refcat(tree_cover "\underline{\emph{Covariates}}" , nolabel) ///
		nomtitle collabel(none) booktabs noobs label unstack nonumber plain
	
	*-----------------------------------
	* 3. Table: CDF OF N_TRIPS
	*-----------------------------------
	cumul n_trips, gen(cum)
	sort cum
	la var n_trips "Num. Trips"
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
		collapse (first) dist_f_ele-dist_f_tra any_def, by(c_code_2011 year_month)
		eststo clear
		eststo B: estpost tabstat dist_f_ele-dist_f_tra if any_def, s(n mean p50 sd) c(s)
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
			(dist_f_ele dist_f_irr dist_f_min dist_f_oth dist_f_res dist_f_tra)
		
		* Totals
		eststo clear
		eststo A: estpost tabstat dist_f_ele-dist_f_tra, s(n mean sd) c(s)
		
		/*
		esttab A B using "${TABLE}/tables/sumstats_deforest2.rtf", replace ///
			label cells("count(fmt(0)) mean(fmt(2)) sd(fmt(2))") ///
			mgroups("Totals" "Per District-Yearmonth", pattern(1 1)) ///
			collabel("N" "Mean" "Std. Dev.") ///
			refcat(dist_f_elec "Project Type" dist_f_hyb "Project Group", nolabel) ///
			nomtitle unstack nonumber
		*/
		* Project Size
		eststo clear
		eststo: estpost tabstat proj_area_forest2, s(n mean p50 sd min max) by(proj_cat)
		/*
		esttab using "${TABLE}/tables/sumstats_projsize.rtf", replace ///
			label cells("count(fmt(0) label(N)) mean(fmt(2) label(Mean)) p50(fmt(2) label(Median)) sd(fmt(2) label(Std. Dev.)) min(fmt(3) label(Min.)) max(fmt(3) label(Max))") ///
			nomtitle unstack nonumber noobs
		eststo clear
		*/
		*-----------------------------------
		* 3. Table: SPATIAL SPAN OF PROJECT
		*-----------------------------------
		egen num_districts = rownonmiss(district_0-district_9), s
		la var num_districts "Districts"
		drop if num_districts == 0
		
		eststo clear
		eststo: estpost tabulate num_districts, nototal
		esttab using "${TABLE}/tables/spatial_dep.tex", replace ///	
			cells("count(fmt(0)) pct(fmt(2)) cumpct(fmt(2))") ///
			nonumbers nomtitle collabels("" "Pct." "Cum.") ///
			noobs gap label 
	
	restore
	
	* Project Size in Full Sample
	eststo: estpost tabstat proj_area_forest2, s(n mean sd min max) by(proj_cat)
	esttab using "${TABLE}/tables/sumstats_projsize_full.tex", replace ///
		label cells("count(fmt(0) label(N)) mean(fmt(2) label(Mean (ha.))) sd(fmt(2) label(Std. Dev.)) min(fmt(3) label(Min.)) max(fmt(3) label(Max))") ///
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
			labsize(medium) angle(45)) legend(order(2 "Species" "Richness" ///
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
	use "${DATA}/dta/fc_ebd_user_update2020", clear
	
	//1. EVENT STUDY OF FIRST PROJECT OPENING
	
	* Prep
	drop_outliers
	keep user_id *code* year_month n_trips ///
		 sr dist_f_cum_km2 coverage ///
		 tree_cover_* temp rain all_species ///
		 group_size month year duration u_lin biome exp_idx
	local controls coverage tree_cover_km2 temp rain all_species duration
	
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
	keep if inrange(dif, -6, 13) // 6 month window
	
	* Event dummies
	tostring dif, replace
	tab dif, gen(e_)
	foreach v of varlist e_* {
		local x : variable label `v'
		local y = subinstr("`x'", "dif==", "",.)
		la var `v' "`y'"
	}
	
	* Regression
	reghdfe sr e_6 e_5 e_4 e_3 e_2 e_7 e_8 e_13-e_20 e_9-e_11 ///
		`controls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
	
	/*
	reghdfe sr e_6 e_5 e_4 e_3 e_2 e_1 e_8-e_13 ///
		`controls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
	*/
	parmest, label list(parm label estimate min* max* p) saving(results, replace)
	use results, clear
	
	replace parm = label
	destring parm, replace force
	drop if parm == . | p == .
	
	twoway (rcap min95 max95 parm, sort color(gs5) lpattern(shortdash)) ///
		   (connected  estimate parm, sort mcolor(black) mfcolor(white) ///
		   lpattern(solid) lcolor(black) msymbol(D)), ///
		   yline(0, lcolor(maroon) lpattern(solid)) ///
		   xline(-1, lcolor(maroon) lpattern(solid)) ///
		   legend(on order(1 "95% CI" 2 "Coefficient")) ///
		   xtitle("Months Since Approval", size(medium)) ///
		   ytitle("Species Richness Relative" "To Approval Month", ///
		   size(medium)) yscale(titlegap(6)) xlabel(-6(2)12) ///
		   saving("${TABLE}/fig/event_study.gph", replace)
	graph export "${TABLE}/fig/event_study.png", width(1000) replace
	
}
