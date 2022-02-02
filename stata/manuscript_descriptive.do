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
gl TABLE	"${ROOT}/docs/jmp/tex_doc/v3"
cd "${TABLE}"

// Modules
local sumstats			0
local learning			0
local estudy_f			0
local estudy			0
local valuation			1

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
		refcat(n_users_dist "\underline{\emph{District Variables}}", nolabel) ///
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
	use "${DATA}/dta/fc_ebd_udt_v02", clear
	drop_outliers
	
	**# 1. Seasonality

	* Total species richness by ym across all users/trips
	preserve
	
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
			684 "Jan-17" 690 "Jul-17" 696 "Jan-18" 702 "Jul-18" 708 "Jan-19" ///
			714 "Jul-19" 720 "Jan-20" 726 "Jul-20", labsize(medium) angle(45)) ///
			legend(order(2 "Species" "Richness" 1 "Num." "Trips") ///
			size(medsmall) pos(6) rows(1)) fysize(37.5) title("A", pos(1) size(large)) ///
			saving("./fig/season.gph", replace) 
	restore

	**# 2. Learning Bias
	
	* No learning
	preserve	
		
		* Residualize on User FE
		reghdfe sr, a(uid c_code_2011 state_code_2011_num#month year) resid

		* Average residual per user-yearmonth
		collapse (mean) resid = _reghdfe_resid, by(user_id year)
		statsby, by(year) clear: ci means resid
		tempfile temp
		save "`temp'"

	restore
	
	* Partial out learning curve
	preserve	
		
		* Residualize on User FE, linear trend
		reghdfe sr exp_idx, a(uid c_code_2011 state_code_2011_num#month year) resid
	
		* Average residual per user-yearmonth
		collapse (mean) resid = _reghdfe_resid, by(user_id year)
		statsby, by(year) clear: ci means resid
		ren * *_nex
		ren year_nex year
		tempfile temp2
		save "`temp2'"
	
	restore

	* Residualize on user-year FE
	preserve
	
		* Residualize
		reghdfe sr ln_exp_idx, a(uid#year c_code_2011 state_code_2011_num#month) resid
		
		* Average residual per user-yearmonth
		collapse (mean) resid = _reghdfe_resid, by(user_id year)
		statsby, by(year) clear: ci means resid
		ren * *_nl
		ren year_nl year
		
		merge 1:1 year using "`temp'", nogen
		merge 1:1 year using "`temp2'", nogen
		
		* Scatter
		twoway scatter mean year, mfcolor(white) mcolor(maroon) ///
			msize(small) || lfit mean year, lcolor(maroon) lwidth(medthick) || ///
			scatter mean_nex year, mfcolor(white) mcolor(navy) ///
			msize(small) msymbol(triangle) || ///
			lfit mean_nex year, lcolor(navy) lwidth(medthick) || ///
			scatter mean_nl year, mfcolor(white) mcolor(black) ///
			msize(small) msymbol(square) || ///
			lfit mean_nl year, lcolor(black) lwidth(medthick) ||, ///
			ylabel(, angle(hor)) ytitle("Mean User Residual", size(medium)) ///
			xtitle("") yscale(titlegap(*-30)) ylabel(, labsize(medsmall)) ///
			legend(order(1 "Learning Bias" 3 "Experience Index" ///
			5 "No Learning Bias") size(medsmall) position(6) rows(2)) ///
			xlabel(, labsize(medsmall)) title("C", size(medium) pos(1)) ///
			fxsize(75) fysize(90) saving("./fig/learn_nl.gph", replace)
	
	restore

	**# Site Choice Bias
	preserve
	
		* Quantiles of species richness
		xtile sr_bl_q3 = sr_bl, nquantiles(3)
		statsby, by(sr_bl_q3) clear noisily: ci means n_trips 
		
		twoway bar mean sr_bl_q3, barwidth(0.7) color(gs8) ///
			|| rcap ub lb sr_bl_q3, lcolor(black) ||, ///
			xlabel(1 "Low" 2 "Medium"  3 "High", labsize(medium)) ///
			ylabel(3(1)6, angle(hor) labsize(medsmall)) ///
			ytitle("Monthly eBird Trips/User", size(medium)) ///
			xtitle("True District Species Richness" "(BirdLife)", size(medium)) ///
			yscale(titlegap(3)) xscale(titlegap(3)) ///
			graphregion(color(white)) bgcolor(white) ///
			legend(order(1 "Mean" 2 "95% CI") size(medium)) ///
			title("B", pos(1) size(large)) fysize(45) ///
			saving("./fig/bl_dist.gph", replace)
	restore
	
	* combine
	graph combine "./fig/season.gph" "./fig/bl_dist.gph", rows(2) saving("./fig/comb.gph", replace)
	graph combine "./fig/comb.gph" "./fig/learn_nl.gph", imargin(0 0 1 0)
	graph export "${TABLE}/fig/bias_plot.png", replace
	
}
*===============================================================================
* DID - DEFORESTATION
*===============================================================================
if `estudy_f' == 1 {
	
	* Read
	use "${DATA}/dta/fc_dym_s2_v02", clear
	keep *_code_2011_num year_month year month dist_f_cum_km2 tree* n_proj_cum
	order *_code_2011_num, first
	
	/*
	**# Event-Study -- New DID Lit
	
	* Event date
	bys c_code_2011 (year_month): gen event = year_month if ///
		dist_f_cum_km2[_n] != dist_f_cum_km2[_n-1]
	bys c_code_2011 (year_month): replace event = . if _n == 1 
	
	* DID vars
	bys c_code_2011_num: egen cohort = min(event) // first event (0 if never treated)
	drop event
	recode cohort (.=0)
	g running = year_month - cohort // time to treatment
	replace running = -99 if cohort == 0
	g treated = (cohort != 0)
	g treatment = (running > 0 & treated == 1)
	
	* Sample
	preserve
		bys c_code_2011: keep if _n == 1
		keep c_code_2011
		sample 20
		tempfile samp
		save "`samp'"
	restore
	merge m:1 c_code_2011 using "`samp'", keep(3) nogen
	
	csdid tree_cover_pct, ivar(c_code_2011_num) time(year_month) gvar(cohort) notyet

	estat event, window(-20 20) estore(cs) 
	event_plot cs, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") ///
	title("csdid") xlabel(-20(1)20)) stub_lag(Tp#) stub_lead(Tm#) together	 
	*/
	
	**# TWFE
	
	* Transform (see https://www.statalist.org/forums/forum/general-stata-discussion/general/1522076-how-do-i-interpret-a-log-level-and-log-log-model-when-my-independent-variable-is-already-a-percentage)
	g ln_dist_f_cum_km2 = asinh(dist_f_cum_km2)
	g ln_tree_cover_pct = ln(tree_cover_pct)
	g ln_n_proj_cum = asinh(n_proj_cum)
	la var n_proj_cum "Num. Projects"
	
	foreach v of varlist dist_f_cum_km2 n_proj_cum {
		
		* lin-lin
		eststo: reghdfe tree_cover_pct `v', ///
			a(c_code_2011_num state_code_2011_num#year month) vce(cl c_code_2011_num)
	
			estadd local dist_fe "$\checkmark$"
			estadd local st_y_fe "$\checkmark$"
			estadd local month_fe "$\checkmark$"
		
		* log-lin
		eststo: reghdfe ln_tree_cover_pct `v', ///
			a(c_code_2011_num state_code_2011_num#year month) vce(cl c_code_2011_num)
			
			estadd local dist_fe "$\checkmark$"
			estadd local st_y_fe "$\checkmark$"
			estadd local month_fe "$\checkmark$"
			
		* lin-log
		preserve
		drop `v'
		ren ln_`v' `v'
		eststo: reghdfe tree_cover_pct `v', ///
			a(c_code_2011_num state_code_2011_num#year month) vce(cl c_code_2011_num)
			
			estadd local dist_fe "$\checkmark$"
			estadd local st_y_fe "$\checkmark$"
			estadd local month_fe "$\checkmark$"
		
		* log-log
		eststo: reghdfe ln_tree_cover_pct `v', ///
			a(c_code_2011_num state_code_2011_num#year month) vce(cl c_code_2011_num)
			
			estadd local dist_fe "$\checkmark$"
			estadd local st_y_fe "$\checkmark$"
			estadd local month_fe "$\checkmark$"
		restore
	}
	esttab using "${TABLE}/tables/f_verify.tex", replace ///
		stats(dist_fe st_y_fe month_fe N r2, labels(`"District FEs"' ///
		`"State x Year FEs"' `"Month FEs"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 3)) ///
		mlabels("lin-lin" "log-lin" "lin-log" "log-log" ///
		"lin-lin" "log-lin" "lin-log" "log-log") wrap nocons nonotes ///
		booktabs nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) ///
		width(\hsize)
	eststo clear
	
}


*===============================================================================
* EVENT STUDY - SPECIES DIVERSION
*===============================================================================
if `estudy' == 1 {
	
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

*===============================================================================
* VALUATION
*===============================================================================
if `valuation' == 1 {
	
	*----------------------
	* Load Datasets
	*----------------------
	/*
	* Read deforestation
	use "${DATA}/dta/fc_dym_s2_v02", clear

	* Reduce
	keep c_code_2011 year_month year month state dist_f*cum_km2 tree_cover_km2
	keep if year == 2020 & month == 12 // total forest occupied by infrastructure at end of study
	tempfile temp
	save "`temp'"
	
	* Read valuation
	import delimited "${DATA}/csv/dist_forest_value.csv", clear
	
	* Merge
	merge 1:1 c_code_2011 using "`temp'", keep(3) nogen
	order c_code_2011 year_month, first

	*----------------------
	* Net Present Value
	*----------------------

	* Convert from ha to km2
	foreach v of varlist nwfp-npv {
		replace `v' = `v' / 100
	}
	
	* NPV of each benefit flow
	/*--------------------------------
	Data = year flow of benefits. 
	Convert to NPV w/ annuity
	formula w/ 4% discount rate and
	60 year rotation period
	Note: carbon storage is a stock
	---------------------------------*/
	foreach v of varlist nwfp-cseq seed-tev { // Rs./km2
	
		g `v'_npv = (((1-(1/(1.04)^60))/0.04) * `v')
	}
	
	* Project Eco-Valuation (NPV Rs.)
	foreach v of varlist *_npv csto npv {
		g dist_f_`v'= dist_f_cum_km2 * `v'
	}

	*----------------------
	* Tabulate
	*----------------------
	replace state = subinstr(state, "&", "and", .)

	eststo clear
	eststo: estpost tabstat dist_f_nwfp_npv, by(state) c(s) s(sum)
	eststo: estpost tabstat dist_f_fodder_npv, by(state) c(s) s(sum)
	eststo: estpost tabstat dist_f_fuelwood_npv, by(state) c(s) s(sum)
	eststo: estpost tabstat dist_f_cseq_npv, by(state) c(s) s(sum)
	eststo: estpost tabstat dist_f_csto, by(state) c(s) s(sum)
	eststo: estpost tabstat dist_f_soil_npv, by(state) c(s) s(sum)
	eststo: estpost tabstat dist_f_water_npv, by(state) c(s) s(sum)
	eststo: estpost tabstat dist_f_seed_npv, by(state) c(s) s(sum)
	
	esttab using "${TABLE}/tables/valuation_boe.tex", replace ///
		cells("sum(fmt(%12.0fc) label())") mgroups("Livelihoods" "Carbon" ///
		"Watershed Services" "", pattern(1 0 0 1 0 1 0 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span ///
		erepeat(\cmidrule(lr){@span})) mlabels("NWFP" "Fodder" "Wood" "Flow" ///
		"Stock" "Soil" "Water" "Seed") collabel(none) booktabs noobs ///
		label unstack

	*----------------------
	* Value of a bird
	*----------------------
	* Assume birds contribute
	* 50% of seed dispersal service
	
	* Pollination/Seed dispersal NPV (Rs./km2)
	g bird_npv = seed_npv*0.5
	
	* species/district (from BirdLife)
	save "`temp'", replace
	import delimited "${DATA}/csv/bl_district.csv", clear
	keep if presenc == 1 & seasona == 1 // currently extant and resident
	ren c_code_11 c_code_2011
	g n_species = 1
	collapse (sum) n_species, by(c_code_2011) // num species/district
	merge 1:1 c_code_2011 using "`temp'", keep(3) nogen
	
	* species per forest km2
	g species_km2 = n_species/tree_cover_km2
	
	* Species value (mean = Rs. 3220 per species) 
	g species_value = bird_npv / species_km2 // (=Rs./species)
	
	* Histogram -- variation comes from species spatial distribution
	su species_value, d
	local mean = round(r(mean),.01)
	twoway hist species_value, frac bcolor(navy8) ///
		xtitle("Species Value (Rs.)", size(medium)) ///
		ytitle("Proportion of Districts", size(medium)) ///
		xlabel(,labsize(medium)) ylabel(,labsize(medium)) ///
		xline(`mean', lcolor(red)) ///
		text(0.2 4000 "Mean = `mean' Rs.", place(e) color(red))
	graph export "${TABLE}/fig/hist_species_value.png", replace
	
	*----------------------
	* Livelihood Loss
	*----------------------
	/*--------------------------------------
	NWFP is in NPV Rs/Km2. Spread this across
	tribal families per km2 --> NWFP per ST. 
	scale by number of STs displaced 
	---------------------------------------*/
	
	* Merge ST from census
	save "`temp'", replace
	*/
	import delimited "${DATA}/csv/2011_india_dist.csv", clear
	keep c_code_11 tot_nm_hh tot_pop tot_st
	kk
	ren c_code_11 c_code_2011
	merge 1:1 c_code_2011 using "`temp'", keep(3) nogen
	
	* ST families per km2 forest
	g n_st_hh = tot_st / (tot_pop / tot_nm_hh) // assume mean hh size
	g n_st_hh_km2 = n_st_hh / tree_cover_km2
	
	* NWFP benefit per ST hh (Rs./hh)
	g nwfp_st_value = nwfp_npv / n_st_hh_km2 // mean = Rs. 425
	
	* Mean ST income from IHDS (for reference)
	preserve
		
		* Read
		use "/Volumes/Backup Plus/dropbox/Dropbox (CID)/IndiaPowerPlant/data/ihds/wave2/36151-0002-Data" , clear
		
		* Main income source
		tab ID14 if ID13==5 // dominant income source = cultivation
		sum INCOME if ID13 == 5 [aw=WT] // mean = 75,216 Rs.
		di r(mean)
		sum INCCROP if ID13 == 5 [aw=WT] // mean = 20,283 Rs.
		di r(mean)
	restore
	
	
	
	
		
}
