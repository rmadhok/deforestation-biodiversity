*********************************************************************************
*																			
* PROJECT: 	Deforestation and Biodiversity										
*																			
* PURPOSE: 	MANUSCRIPT - DESCRIPTIVE WORK (TEX)   																 				 							
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
local learning			1
local estudy_f			0
local estudy			0
local valuation			0

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
* SUMMARY STATISTICS
*===============================================================================
if `sumstats' == 1 {
	
	* Read
	use "${DATA}/dta/fc_ebd_udt_v02", clear
	drop if year == 2014
	drop_outliers
	
	* Tag district
	bys c_code_2011: egen treat = max(dist_f_cum_km2 > 0 & dist_f_cum_km2!=.)
	egen tag_d = tag(c_code_2011) //district-level
	egen tag_dym = tag(c_code_2011 year_month)
	
	* Indent for pretty latex formatting
	foreach v of varlist dist_f*_cum_km2 tree* pop* ///
		sr coverage* n_* duration distance temp rain exp_idx group_size { 
	
			label variable `v' `"\hspace{0.2cm} `: variable label `v''"'	
	}

	* Collect
	local dist_vars n_users_dist n_trips_dist pop_density
	local trip sr coverage_udym duration distance group_size
	local covariates tree_cover_s rain temp rad_mean
	
	*---------------------------------------
	* 1. TABLE: OUTCOMES AND COVARIATES
	*---------------------------------------
	
	**# Split by Treatment / Control
	
	* District Variables
	eststo clear
	eststo A: estpost tabstat `dist_vars' if tag_d & treat, s(n mean sd) c(s)
	eststo B: estpost tabstat `dist_vars' if tag_d & !treat, s(n mean sd) c(s)

	esttab A B using "${TABLE}/tables/sumstats_ebd.tex", replace f main(mean) aux(sd) ///
		cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		mgroups("Deforestation Districts" "Non-deforestation Districts", pattern(1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		collabel("Mean" "Std. Dev." "N", prefix({) suffix(})) ///
		refcat(n_users_dist "\underline{\emph{District}}", nolabel) ///
		nomtitle booktabs noobs label unstack nonumber

	* Birdwatching Details
	eststo clear
	eststo A: estpost tabstat `trip' if treat, s(n mean med sd) c(s)
	eststo B: estpost tabstat `trip' if !treat, s(n mean med sd) c(s)

	esttab A B using "${TABLE}/tables/sumstats_ebd.tex", append f main(mean) aux(sd) ///
		cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		refcat(sr "\underline{\emph{User-District-Time}}", nolabel) nomtitle collabel(none) ///
		width(\hsize) nomtitle booktabs noobs label unstack nonumber plain
	
	* Covariates
	eststo clear
	eststo A: estpost tabstat `covariates' if tag_dym & treat, s(n mean med sd) c(s)
	eststo B: estpost tabstat `covariates' if tag_dym & !treat, s(n mean med sd) c(s)
	
	esttab A B using "${TABLE}/tables/sumstats_ebd.tex", append f main(mean) aux(sd) ///
		cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") refcat(tree_cover_s ///
		"\underline{\emph{District-Time}}" , nolabel) nomtitle collabel(none) ///
		booktabs noobs label unstack nonumber plain
	
	**# Full Summary Stats
	
	eststo clear
	eststo: estpost tabstat `dist_vars' if tag_d, s(n mean sd) c(s) // District

	esttab using "${TABLE}/tables/sumstats_ebd_full.tex", replace f main(mean) aux(sd) ///
		cells("mean(fmt(2) label(Mean)) sd(fmt(2) label(SD)) count(fmt(0) label(N))") ///
		refcat(n_users_dist "\underline{\emph{District}}", nolabel) ///
		nomtitle booktabs noobs label unstack nonumber

	eststo clear
	eststo: estpost tabstat `trip', s(n mean med sd) c(s) // Birdwatching

	esttab using "${TABLE}/tables/sumstats_ebd_full.tex", append f main(mean) aux(sd) ///
		cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		refcat(sr "\underline{\emph{User-District-Time}}", nolabel) nomtitle collabel(none) ///
		width(\hsize) nomtitle booktabs noobs label unstack nonumber plain
	
	eststo clear
	eststo: estpost tabstat `covariates' if tag_dym, s(n mean med sd) c(s) // Covariates
	
	esttab using "${TABLE}/tables/sumstats_ebd_full.tex", append f main(mean) aux(sd) ///
		cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") refcat(tree_cover_s ///
		"\underline{\emph{District-Time}}" , nolabel) nomtitle collabel(none) ///
		booktabs noobs label unstack nonumber plain
	
	*-----------------------------------
	* 2. Table: CDF OF N_TRIPS
	*-----------------------------------
	cumul n_trips, gen(cum)
	sort cum
	la var n_trips "Num. Trips"
	line cum n_trips, ytitle("Cumulative Density (%)") xlabel(0(5)60)
	graph export "${TABLE}/fig/cdf_ntrips.png", replace
	
	*-------------------------------------
	* 3. VARIATION in USER CHARACTERISTICS
	*-------------------------------------
	// Add figures separately in latex instead of graph combine (see https://twitter.com/michaelstepner/status/1201621569736445957)
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

	* Project-level
	use "${DATA}/dta/fc_clean_v02", clear // post-2014 approvals
	keep if prop_status == "approved"
	append using "${DATA}/dta/fc_pre2014_clean_v02" // pre-2014 approvals
	replace proj_cat = "other" if inlist(proj_cat, "underground", "industry")
	
	* Tabulate
	*drop if proj_area_forest2 > 2000 // drop 3 megaprojects
	replace proj_cat = proper(proj_cat)
	eststo clear
	eststo: estpost tabstat proj_area_forest2, by(proj_cat) s(n mean sd sum) c(s)
	esttab using "${TABLE}/tables/sumstats_fc.tex", replace ///
		label cells("count(fmt(%12.0fc) label(Num. Projects)) mean(fmt(%12.2fc) label(Mean Size (ha.))) sd(fmt(%12.2fc) label(SD (ha.))) sum(fmt(%12.2fc) label(Total Area (ha.)))") ///
		nomtitle unstack nonumber noobs booktabs
	
	*-----------------------------------
	* 5. Table: SPATIAL SPAN OF PROJECT
	*-----------------------------------
	egen num_districts = rownonmiss(district_*), s
	la var num_districts "Districts"
	
	eststo clear
	eststo: estpost tabulate num_districts, nototal
	esttab using "${TABLE}/tables/spatial_dep.tex", replace ///	
		cells("count(fmt(0)) pct(fmt(2)) cumpct(fmt(2))") ///
		nonumbers nomtitle collabels("" "Pct." "Cum.") ///
		noobs label booktabs
}

*===============================================================================
* LEARNING CURVE
*===============================================================================
if `learning' == 1 {

	* Read
	use "${DATA}/dta/fc_ebd_udt_v02", clear
	drop if year == 2014
	drop_outliers
	
	*---------------------
	* BIAS PLOTS
	*---------------------
	
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
			size(medsmall) pos(12) rows(1)) xsize(4.6) title("A: Seasonality", ///
			pos(11) size(large) margin(b=3)) saving("./fig/season.gph", replace) 
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
		reghdfe sr exp_idx, a(uid#year c_code_2011 state_code_2011_num#month) resid
		
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
			xtitle("Year") yscale(titlegap(*-30)) ylabel(, labsize(medsmall)) ///
			text(-.4 2016.5 "Learning Bias" "(User FE)", place(e) color(maroon) size(medsmall)) ///
			text(-.17 2015.2 "User FE +" "Experience Index", place(e) color(navy) size(medsmall)) ///
			text(0.07 2016 "No Learning Bias" "(User x Year FE)", place(c) color(black) size(medsmall)) ///
			legend(off) xlabel(, labsize(medsmall)) title("C: Learning", ///
			size(medium) pos(11) margin(b=3)) fxsize(75) fysize(90) ///
			saving("./fig/learn_nl.gph", replace)
	
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
			ytitle("Monthly Trips/User (eBird)", size(medium)) ///
			xtitle("True District Species Richness (BirdLife)", size(medium)) ///
			yscale(titlegap(3)) xscale(titlegap(3)) ///
			graphregion(color(white)) bgcolor(white) ///
			legend(order(1 "Mean" 2 "95% CI") size(medium)) ///
			title("B: Site Choice", pos(11) size(large) margin(b=3)) ///
			xsize(4.6) saving("./fig/bl_dist.gph", replace)
	restore
	
	* combine
	graph combine "./fig/season.gph" "./fig/bl_dist.gph", rows(2) saving("./fig/comb.gph", replace)
	graph combine "./fig/comb.gph" "./fig/learn_nl.gph", imargin(0 0 1 0)
	graph export "${TABLE}/fig/bias_plot.png", replace

	*-----------------------------------------
	* IDENTIFYING VARIATION ACROSS FE SPECS
	*-----------------------------------------
	* Hacky table but it works
	
	* District
	reghdfe sr, a(c_code_2011_num) resid(r_dfe)
	g r2_dfe = e(r2)
	egen r_dfe_sd = sd(r_dfe)
	la var r2_dfe "District FE"
	la var r_dfe_sd "District FE"

	* District + State-Month
	reghdfe sr, a(c_code_2011_num state_code_2011_num#month year) resid(r_dsmfe)
	g r2_dsmfe = e(r2)
	egen r_dsmfe_sd = sd(r_dsmfe)
	la var r2_dsmfe "District + State-Month + Year FE"
	la var r_dsmfe_sd "District + State-Month + Year FE"
	
	* User + district + State-Month
	reghdfe sr, a(uid c_code_2011_num state_code_2011_num#month year) resid(r_udsmfe)
	g r2_udsmfe = e(r2)
	egen r_udsmfe_sd = sd(r_udsmfe)
	la var r2_udsmfe "User + District + State-Month + Year FE"
	la var r_udsmfe_sd "User + District + State-Month + Year FE"
	
	* User-year + district + State-Month
	reghdfe sr, a(uid#year c_code_2011_num state_code_2011_num#month) resid(r_uydsmfe)
	g r2_uydsmfe = e(r2)
	egen r_uydsmfe_sd = sd(r_uydsmfe)
	la var r2_uydsmfe "\textbf{User-Year + District + State-Month FE}"
	la var r_uydsmfe_sd "\textbf{User-Year + District + State-Month FE}"

	* Tabulate
	eststo A: estpost tabstat r2*, s(mean) c(s) // R2 
	
	drop r2_* r_*fe
	ren r_*_sd r2_*
	
	eststo B: estpost tabstat r2*, s(mean) c(s) // SD of residual
	
	esttab using "${TABLE}/tables/id_variation.tex", replace ///
		mgroups(`"\$R^{2}\$"' `"\$\sigma_{\epsilon}\$"', pattern(1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		cells("mean(fmt(3))") nomtitles collabel(none) label noobs ///
		booktabs sub(\_ _)
	
}
*===============================================================================
* DID - DEFORESTATION
*===============================================================================
if `estudy_f' == 1 {
	
	** TO DO: AGGREGATE FC DATA TO ANNUAL AND DO EVENT STUDY + TWFE
	
	* Read
	*use "${DATA}/dta/fc_dym_s2_v02", clear
	use "${DATA}/dta/fc_dym_s2_post_v02", clear
	merge 1:1 c_code_2011 year_month using "${DATA}/dta/fc_dym_s1_post_v02", keep(3) nogen // merge stage 1
	*keep *_code_2011 year_month year month dist_*_cum_km2 tree* n_proj_cum
	keep *_code_2011 year_month year month dist_f_cum_km2* tree* n_proj_cum
	order *_code_2011, first
	*drop if year == 2014
	
	* Add Weather
	tempfile temp
	save "`temp'"
	foreach file in "india_rainfall_gpm" "india_temperature_era" {
		import delimited "${DATA}/csv/`file'", varn(1) clear
		g year_month = ym(year(date(yearmonth, "20YM")), month(date(yearmonth, "20YM")))
		format year_month %tmCCYY-NN
		drop yearmonth
		merge 1:1 c_code_2011 year_month using "`temp'", keep(2 3) nogen
		save "`temp'", replace
	}
	
	* Add area
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", keepusing(tot_area) nogen
	encode c_code_2011, gen(c_code_2011_num)
	encode state_code_2011, gen(state_code_2011_num)
	g tree_cover_s = (tree_cover_km2 / tot_area)*100
	
	**# TWFE
	* Transform (see https://www.statalist.org/forums/forum/general-stata-discussion/general/1522076-how-do-i-interpret-a-log-level-and-log-log-model-when-my-independent-variable-is-already-a-percentage)
	g ln_dist_f_cum_km2 = asinh(dist_f_cum_km2)
	la var ln_dist_f_cum_km2 "log(Infrastructure)"
	g ln_tree_cover_pct = ln(tree_cover_pct)
	g ln_tree_cover_km2 = ln(tree_cover_km2)
	g ln_n_proj_cum = asinh(n_proj_cum)
	g ln_tree_cover_s = ln(tree_cover_s)
	la var n_proj_cum "Num. Projects"

	foreach v of varlist dist_f_cum_km2 n_proj_cum {
		
		* lin-lin
		eststo `v'_linlin: reghdfe tree_cover_pct `v', ///
			a(c_code_2011_num state_code_2011_num#year) vce(cl c_code_2011_num)
	
			estadd local dist_fe "$\checkmark$"
			estadd local st_y_fe "$\checkmark$"
		
		* log-lin
		eststo `v'_loglin: reghdfe ln_tree_cover_pct `v', ///
			a(c_code_2011_num state_code_2011_num#year) vce(cl c_code_2011_num)
			
			estadd local dist_fe "$\checkmark$"
			estadd local st_y_fe "$\checkmark$"
			
		* lin-log
		eststo `v'_linlog: reghdfe tree_cover_pct ln_`v', ///
			a(c_code_2011_num state_code_2011_num#year) vce(cl c_code_2011_num)
			
			estadd local dist_fe "$\checkmark$"
			estadd local st_y_fe "$\checkmark$"
		
		* log-log
		eststo `v'_loglog: reghdfe ln_tree_cover_pct ln_`v', ///
			a(c_code_2011_num state_code_2011_num#year) vce(cl c_code_2011_num)
			
			estadd local dist_fe "$\checkmark$"
			estadd local st_y_fe "$\checkmark$"
	
		esttab `v'_linlin `v'_loglog using "${TABLE}/tables/f_verify_`v'.tex", ///
			replace stats(dist_fe st_y_fe N r2, labels(`"District FEs"' ///
			`"State x Year FEs"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 3)) ///
			mlabels("Tree Cover (\%)" "log(Tree Cover)") ///
			wrap nocons nonotes booktabs nomtitles star(* .1 ** .05 *** .01) ///
			label se b(%5.3f) width(0.8\hsize)
		eststo clear
	}
	ll
	/*
	**# Event-Study -- New DID Lit
	
	* Event date
	bys c_code_2011 (year_month): gen event = year_month if ///
		dist_f_cum_km2[_n] != dist_f_cum_km2[_n-1]
	bys c_code_2011 (year_month): replace event = . if _n == 1 
	
	* DID vars
	bys c_code_2011: egen cohort = min(event) // first event (0 if never treated)
	drop event
	recode cohort (.=0)
	g running = year_month - cohort // time to treatment
	replace running = -99 if cohort == 0
	g treated = (cohort != 0)
	g treatment = (running > 0 & treated == 1)
	
	* Naive TWFE
	*keep if inrange(running, -36, 36)
	*tab running, gen(t_)
	*reghdfe tree_cover_pct t_1-t_35 t_37-t_73 rain_gpm temp_era, a(c_code_2011_num state_code_2011_num#year month) vce(cl c_code_2011_num)
	*drop t_*
	
	* New DID
	
	* Sample
	preserve
		bys c_code_2011: keep if _n == 1
		keep c_code_2011
		sample 5
		tempfile samp
		save "`samp'"
	restore
	merge m:1 c_code_2011 using "`samp'", keep(3) nogen
	
	csdid tree_cover_pct, ivar(c_code_2011_num) time(year_month) gvar(cohort) notyet

	estat event, window(-24 24) estore(cs) 
	event_plot cs, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") ///
	title("csdid") xlabel(-24(6)24)) stub_lag(Tp#) stub_lead(Tm#) together	 
	*/
	
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
	*eststo: estpost tabstat dist_f_fodder_npv, by(state) c(s) s(sum)
	eststo: estpost tabstat dist_f_fuelwood_npv, by(state) c(s) s(sum)
	eststo: estpost tabstat dist_f_cseq_npv, by(state) c(s) s(sum)
	eststo: estpost tabstat dist_f_csto, by(state) c(s) s(sum)
	*eststo: estpost tabstat dist_f_soil_npv, by(state) c(s) s(sum)
	*eststo: estpost tabstat dist_f_water_npv, by(state) c(s) s(sum)
	eststo: estpost tabstat dist_f_seed_npv, by(state) c(s) s(sum)
	eststo: estpost tabstat dist_f_tev_npv, by(state) c(s) s(sum)
	
	esttab using "${TABLE}/tables/valuation_boe.tex", replace ///
		cells("sum(fmt(%12.0fc) label())") mlabels("NWFP" "Wood" "C-Flow" ///
		"C-Stock" "Seed" "Total") collabel(none) booktabs noobs ///
		label unstack

	*----------------------
	* Tabulate Top 5
	*----------------------
	bys state: egen state_npv = total(dist_f_npv)
	gsort -state_npv
	egen rank = group(state_npv)
	drop if state_npv == 0
	labmask rank, values(state)
	
	eststo clear
	eststo: estpost tabstat dist_f_nwfp_npv if inrange(rank, 29,33), by(rank) c(s) s(sum) nototal
	*eststo: estpost tabstat dist_f_fodder_npv if inrange(rank, 29,33), by(rank) c(s) s(sum) nototal
	eststo: estpost tabstat dist_f_fuelwood_npv if inrange(rank, 29,33), by(rank) c(s) s(sum) nototal
	eststo: estpost tabstat dist_f_cseq_npv if inrange(rank, 29,33), by(rank) c(s) s(sum) nototal
	eststo: estpost tabstat dist_f_csto if inrange(rank, 29,33), by(rank) c(s) s(sum) nototal
	*eststo: estpost tabstat dist_f_soil_npv if inrange(rank, 29,33), by(rank) c(s) s(sum) nototal
	*eststo: estpost tabstat dist_f_water_npv if inrange(rank, 29,33), by(rank) c(s) s(sum) nototal
	eststo: estpost tabstat dist_f_seed_npv if inrange(rank, 29,33), by(rank) c(s) s(sum) nototal
	eststo: estpost tabstat dist_f_tev_npv if inrange(rank, 29,33), by(rank) c(s) s(sum) nototal
	
	esttab using "${TABLE}/tables/valuation_boe_top5.tex", replace ///
		cells("sum(fmt(%12.0fc) label())") mlabels("NWFP" "Wood" "C-Flow" ///
		"C-Stock" "Seed" "Total") collabel(none) booktabs noobs ///
		label unstack
	
	*----------------------
	* Value of a bird
	*----------------------
	* Assume birds contribute
	* 50% of seed dispersal service
	
	* Pollination/Seed dispersal NPV (Rs./km2)
	*g bird_npv = seed_npv*0.5
	g bird_npv = seed_npv
	
	* species/district (from BirdLife)
	save "`temp'", replace
	import excel "${DATA}/csv/bl_district.xlsx", first clear
	keep if PRESENC == 1 & SEASONA == 1 // currently extant and resident
	ren c_code_11 c_code_2011
	g n_species = 1
	collapse (sum) n_species, by(c_code_2011) // num species/district
	merge 1:1 c_code_2011 using "`temp'", keep(3) nogen
	
	* species per forest km2
	g species_km2 = n_species/tree_cover_km2
	
	* Species value (mean = Rs. 3182 per species) 
	g species_value = bird_npv / species_km2 // (=Rs./species)
	
	* Histogram -- variation comes from species spatial distribution
	su species_value, d
	local mean = round(r(mean),.01)
	twoway hist species_value, frac bcolor(navy8) ///
		xtitle("Species Value (Rs.)", size(medium)) ///
		ytitle("Proportion of Districts", size(medium)) ///
		xlabel(,labsize(medium)) ylabel(,labsize(medium)) ///
		xline(`mean', lcolor(red)) ///
		text(0.2 6500 "Mean = `mean' Rs.", place(n) color(red)) 
	graph export "${TABLE}/fig/hist_species_value.png", replace
	nn
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
	import delimited "${DATA}/csv/2011_india_dist.csv", clear
	keep c_code_11 tot_nm_hh tot_pop tot_st
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
