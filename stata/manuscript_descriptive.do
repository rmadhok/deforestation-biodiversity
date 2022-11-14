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
gl TABLE	"${ROOT}/docs/jmp/tex_doc/v4"
cd "${TABLE}"

// Modules
local sumstats			0
local learning			0
local verify			0
local event_study		1
local valuation			0
local fra				0
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
	
	* Tag
	bys c_code_2011: egen treat = max(dist_f_cum_km2 > 0 & dist_f_cum_km2!=.)
	egen tag_d = tag(c_code_2011) //district-level
	egen tag_dym = tag(c_code_2011 year_month)
	egen tag_u = tag(uid)
	
	* Indent for pretty latex formatting
	foreach v of varlist dist_f*_cum_km2 tree* pop* rad* ///
		sr coverage* n_* duration distance temp rain exp_idx group_size hour { 
	
			label variable `v' `"\hspace{0.2cm} `: variable label `v''"'	
	}

	* Collect
	local dist_vars n_users_dist n_trips_dist
	local user_vars n_dist_user n_st_user n_ym_user
	local trip sr coverage_udym duration distance hour
	local covariates tree_cover_s rain temp rad_mean
	
	*---------------------------------------
	* 1. TABLE: OUTCOMES AND COVARIATES
	*---------------------------------------
	
	**# Split by Treatment / Control
	
	* District Variables
	eststo clear
	eststo A: estpost tabstat `dist_vars' if tag_d & treat, s(n mean sd) c(s)
	eststo B: estpost tabstat `dist_vars' if tag_d & !treat, s(n mean sd) c(s)

	esttab A B using "${TABLE}/tables/sumstats_ebd_split.tex", replace f main(mean) aux(sd) ///
		cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		mgroups("Deforestation Districts" "Non-deforestation Districts", pattern(1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		collabel("Mean" "Std. Dev." "Obs.", prefix({) suffix(})) ///
		refcat(n_users_dist "\underline{\emph{District}}", nolabel) ///
		nomtitle booktabs noobs label unstack nonumber
	
	* User Details
	eststo clear
	eststo A: estpost tabstat `user_vars' if tag_u & treat, s(n mean med sd) c(s)
	eststo B: estpost tabstat `user_vars' if tag_u & !treat, s(n mean med sd) c(s)

	esttab A B using "${TABLE}/tables/sumstats_ebd_split.tex", append f main(mean) aux(sd) ///
		cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		refcat(n_dist_user "\underline{\emph{User}}", nolabel) nomtitle collabel(none) ///
		width(\hsize) nomtitle booktabs noobs label unstack nonumber plain
	
	* Birdwatching Details
	eststo clear
	eststo A: estpost tabstat `trip' if treat, s(n mean med sd) c(s)
	eststo B: estpost tabstat `trip' if !treat, s(n mean med sd) c(s)

	esttab A B using "${TABLE}/tables/sumstats_ebd_split.tex", append f main(mean) aux(sd) ///
		cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		refcat(sr "\underline{\emph{User-District-Time}}", nolabel) nomtitle collabel(none) ///
		width(\hsize) nomtitle booktabs noobs label unstack nonumber plain
	
	* Covariates
	eststo clear
	eststo A: estpost tabstat `covariates' if tag_dym & treat, s(n mean med sd) c(s)
	eststo B: estpost tabstat `covariates' if tag_dym & !treat, s(n mean med sd) c(s)
	
	esttab A B using "${TABLE}/tables/sumstats_ebd_split.tex", append f main(mean) aux(sd) ///
		cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") refcat(tree_cover_s ///
		"\underline{\emph{District-Time}}" , nolabel) nomtitle collabel(none) ///
		booktabs noobs label unstack nonumber plain
	
	**# Full Summary Stats
	
	eststo clear
	eststo: estpost tabstat `dist_vars' if tag_d, s(n mean sd) c(s) // District
	esttab using "${TABLE}/tables/sumstats_ebd_full.tex", replace f main(mean) aux(sd) ///
		cells("mean(fmt(2) label(Mean)) sd(fmt(2) label(Std. Dev.)) count(fmt(0) label(Obs.))") ///
		refcat(n_users_dist "\underline{\emph{District}}", nolabel) ///
		nomtitle booktabs noobs label unstack nonumber

	eststo clear
	eststo: estpost tabstat `user_vars' if tag_u, s(n mean med sd) c(s) // User
	esttab using "${TABLE}/tables/sumstats_ebd_full.tex", append f main(mean) aux(sd) ///
		cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		refcat(n_dist_user "\underline{\emph{User}}", nolabel) nomtitle collabel(none) ///
		width(\hsize) nomtitle booktabs noobs label unstack nonumber plain
	
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
	line cum n_trips, ///
		ytitle("Cumulative Density", size(large)) ///
		ylabel(, labsize(large)) xtitle(, size(large)) ///
		xlabel(0(5)60, labsize(large))
	graph export "${TABLE}/fig/cdf_ntrips.png", replace

	*-------------------------------------
	* 3. VARIATION in USER CHARACTERISTICS
	*-------------------------------------
	// Add figures separately in latex instead of graph combine (see https://twitter.com/michaelstepner/status/1201621569736445957)
	preserve
		
		collapse (first) n_*_user, by(user_id)
		local lab `" "A. Num. States Per User" "B. Num. Districts Per User" "C. Num. Time Periods Per User" "'
		local i = 1
		foreach v of varlist n_st_user n_dist_user n_ym_user {
		
			local title: word `i' of `lab'
			hist `v', frac bcolor(dknavy) ///
				title("`title'", size(large)) ///
				ytitle("Share of Users", size(large)) ///
				xtitle("") xlabel(, labsize(large)) ///
				ylabel(, labsize(large)) name(`v', replace)
			local ++i
		}
		graph combine n_st_user n_dist_user n_ym_user
		graph export "${TABLE}/fig/user_variation.png", replace
	
	restore
	
	*-----------------------------------
	* 4. Table: Forest Clearance
	*-----------------------------------
	
	* Read project-level data
	use "${DATA}/dta/fc_clean_v02", clear // post-2014 approvals
	keep if prop_status == "approved"
	append using "${DATA}/dta/fc_pre2014_clean_v02" // pre-2014 approvals
	replace proj_cat = "other" if inlist(proj_cat, "underground", "industry")
	
	**# Category-wise (electricity, transport, etc) stats
	replace proj_cat = proper(proj_cat)
	eststo clear
	eststo: estpost tabstat proj_area_forest2, by(proj_cat) s(n mean sd sum) c(s)
	esttab using "${TABLE}/tables/sumstats_fc.tex", replace ///
		label cells("count(fmt(%12.0fc) label(Num. Projects)) mean(fmt(%12.2fc) label(Mean Size (ha.))) sd(fmt(%12.2fc) label(SD (ha.))) sum(fmt(%12.2fc) label(Total Area (ha.)))") ///
		nomtitle unstack nonumber noobs booktabs width(1\hsize)
	
	**# Type and Shape stats
	
	replace proj_type = "public" if inlist(proj_type, "state", "central")
	replace proj_type = "neither" if proj_type == "joint"
	replace proj_type = proper(proj_type)
	label define order_type  1 "Public"   2 "Private"  3 "Neither"
	encode proj_type, gen(proj_type2) label(order_type)
	
	* Type (pub/private)
	eststo clear
	eststo: estpost tabstat proj_area_forest2, by(proj_type2) s(n mean sd sum) c(s) nototal
	esttab using "${TABLE}/tables/sumstats_fc_decomp.tex", replace f ///
		label cells("count(fmt(%12.0fc) label(Num. Projects)) mean(fmt(%12.2fc) label(Mean Size (ha.))) sd(fmt(%12.2fc) label(SD (ha.))) sum(fmt(%12.2fc) label(Total Area (ha.)))") ///
		nomtitle unstack nonumber noobs booktabs width(1\hsize) ///
		posthead("\midrule \underline{\emph{Panel A: Ownership}} \\")
		
	* Shape (linear/nonlinear)
	replace proj_shape = "nonlinear" if proj_shape == "hybrid"
	replace proj_shape = proper(proj_shape)
	eststo clear
	eststo: estpost tabstat proj_area_forest2, by(proj_shape) s(n mean sd sum) c(s) nototal
	
	esttab using "${TABLE}/tables/sumstats_fc_decomp.tex", append f ///
		label cells("count(fmt(%12.0fc)) mean(fmt(%12.2fc)) sd(fmt(%12.2fc)) sum(fmt(%12.2fc))") ///
		nomtitle unstack nonumber noobs booktabs width(1\hsize) collabel(none) ///
		posthead("\\ \underline{\emph{Panel B: Shape}} \\ ")
	
	* Type and Shape by Category
	eststo clear
	eststo: estpost tabulate proj_cat proj_type2, nototal
	eststo: estpost tabulate proj_cat proj_shape, nototal
	
	local num "& (1) & (2) & (3) & (4) & (5) \\ \midrule"
	esttab using "${TABLE}/tables/sumstats_fc_twoway.tex", replace ///
		label cell(rowpct(fmt(2))) mgroups("Ownership (\%)" "Shape (\%)", pattern(1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		nomtitle collabels(none) unstack nonumber noobs booktabs width(1\hsize) ///
		posthead(`num')

	*-----------------------------------
	* 5. Table: SPATIAL SPAN OF PROJECT
	*-----------------------------------
	egen num_districts = rownonmiss(district_*), s
	la var num_districts "Districts"
	
	eststo clear
	eststo: estpost tabulate num_districts, nototal
	esttab using "${TABLE}/tables/spatial_span.tex", replace ///	
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
	
	**# 1. Seasonality
	
	* Total species richness by ym across all users/trips
	preserve
	
		collapse (first) sr_ym (sum) n_trips, by(year_month)
		replace n_trips = n_trips/1000
		twoway bar n_trips year_month, yscale(alt) bcolor(dkgreen*0.5) ///
			ytitle("Num. Trips (thousands)", size(large) axis(alt)) ///
			ylabel(, angle(hor) labsize(large)) || connected sr_ym year_month, ///
			yaxis(2) yscale(alt axis(2) titlegap(3)) ///
			ylabel(, axis(2) angle(hor) labsize(large)) ///
			ytitle("Species Richness", size(large) axis(2)) ///
			mcolor(gs3) msize(small) lcolor(black) ||, xtitle("") ///
			xlabel(660 "Jan-15" 666 "Jul-15" 672 "Jan-16" 678 "Jul-16" ///
			684 "Jan-17" 690 "Jul-17" 696 "Jan-18" 702 "Jul-18" 708 "Jan-19" ///
			714 "Jul-19" 720 "Jan-20" 726 "Jul-20", labsize(medlarge) angle(45)) ///
			legend(order(2 "Species" "Richness" 1 "Num." "Trips") ///
			size(med) pos(12) rows(1)) xsize(4.6) title("A: Seasonality", ///
			pos(11) size(large) margin(b=3)) name(season, replace) 
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
			ylabel(, angle(hor) labsize(medsmall)) ytitle("Mean User Residual", ///
			size(medlarge)) xtitle("Year", size(medlarge)) yscale(titlegap(*-30)) ///
			ylabel(, labsize(med)) text(-.4 2016.5 "Learning Bias" "(User FE)", ///
			place(e) color(maroon) size(medsmall)) text(-.17 2015.2 ///
			"User FE +" "Experience Index", place(e) color(navy) size(medsmall)) ///
			text(0.07 2016 "No Learning Bias" "(User x Year FE)", ///
			place(c) color(black) size(medsmall)) legend(off) ///
			xlabel(, labsize(medsmall)) title("C: Learning", size(medlarge) ///
			pos(11) margin(b=3)) fxsize(75) fysize(90) name(learn, replace)
	
	restore

	**# Site Choice Bias
	preserve
	
		* Quantiles of species richness
		xtile sr_bl_q3 = sr_bl, nquantiles(3)
		statsby, by(sr_bl_q3) clear noisily: ci means n_trips 
		
		twoway bar mean sr_bl_q3, barwidth(0.7) color(dkgreen*0.5) ///
			|| rcap ub lb sr_bl_q3, lcolor(black) ||, ///
			xlabel(1 "Low" 2 "Medium"  3 "High", labsize(large)) ///
			ylabel(3(1)6, angle(hor) labsize(large)) ///
			ytitle("Monthly Trips/User (eBird)", size(large)) ///
			xtitle("True District Species Richness (BirdLife)", size(large)) ///
			yscale(titlegap(3)) xscale(titlegap(3)) ///
			legend(order(1 "Mean" 2 "95% CI") size(medium)) ///
			title("B: Site Choice", pos(11) size(large) margin(small)) ///
			xsize(4.6) name(site_choice, replace)
	restore
	
	* combine
	graph combine season site_choice, rows(2) name(comb, replace)
	graph combine comb learn, col(2) imargin(0 0 1)
	graph export "${TABLE}/fig/bias_plot.png", replace
	
	*-----------------------------------------
	* IDENTIFYING VARIATION ACROSS FE SPECS
	*-----------------------------------------
	* Hacky table but it works
	
	* District
	reghdfe sr, a(c_code_2011_num) resid(r_dfe)
	g r2_dfe = 1-e(r2)
	egen r_dfe_sd = sd(r_dfe)
	la var r2_dfe "District FE"
	la var r_dfe_sd "District FE"

	* District + State-Month
	reghdfe sr, a(c_code_2011_num state_code_2011_num#month year) resid(r_dsmfe)
	g r2_dsmfe = 1-e(r2)
	egen r_dsmfe_sd = sd(r_dsmfe)
	la var r2_dsmfe "District + State-Month + Year FE"
	la var r_dsmfe_sd "District + State-Month + Year FE"
	
	* User + district + State-Month
	reghdfe sr, a(uid c_code_2011_num state_code_2011_num#month year) resid(r_udsmfe)
	g r2_udsmfe = 1-e(r2)
	egen r_udsmfe_sd = sd(r_udsmfe)
	la var r2_udsmfe "User + District + State-Month + Year FE"
	la var r_udsmfe_sd "User + District + State-Month + Year FE"
	
	* User-year + district + State-Month
	reghdfe sr, a(uid#year c_code_2011_num state_code_2011_num#month) resid(r_uydsmfe)
	g r2_uydsmfe = 1-e(r2)
	egen r_uydsmfe_sd = sd(r_uydsmfe)
	la var r2_uydsmfe "User-Year + District + State-Month FE"
	la var r_uydsmfe_sd "User-Year + District + State-Month FE"

	* Tabulate
	eststo A: estpost tabstat r2*, s(mean) c(s) // R2 
	drop r2_* r_*fe
	ren r_*_sd r2_*
	eststo B: estpost tabstat r2*, s(mean) c(s) // SD of residual
	esttab using "${TABLE}/tables/id_variation.tex", replace ///
		mgroups(`"\$1-R^{2}\$"' `"\$\sigma_{\epsilon}\$"', pattern(1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		cells("mean(fmt(3))") nomtitles collabel(none) label noobs ///
		booktabs sub(\_ _) width(\hsize)
	
}
*===============================================================================
* VERIFY ACTUAL DEFORESTATION FROM PROJECTS
*===============================================================================
if `verify' == 1 {
	
	foreach file in "" "post_" {
	
		* Read
		use "${DATA}/dta/fc_dym_s2_`file'v02", clear

		* Post-2014 only (for stage 1 placebo)
		if "`file'" == "post_" {
			
			ren dist_nf_cum_km2 dist_f_cum_km2_nf
		}
		keep *_code_2011 year_month year month dist_f*cum_km2* tree*
		drop *_lag* *_lead*
	
		* Add Controls
		tempfile temp
		save "`temp'"
		foreach file in "india_rainfall_gpm" "india_temperature_era" "india_nightlights" {
			import delimited "${DATA}/csv/`file'", varn(1) clear
			g year_month = ym(year(date(yearmonth, "20YM")), month(date(yearmonth, "20YM")))
			format year_month %tmCCYY-NN
			drop yearmonth
			merge 1:1 c_code_2011 year_month using "`temp'", keep(2 3) nogen
			save "`temp'", replace
		}
	
		* Collapse to Annual
		sort c_code_2011 year_month
		collapse (lastnm) dist_f*cum_km2* tree* state_code_2011 ///
			     (mean) rain_gpm temp_era rad_*, by(c_code_2011 year)
		g rad_mean_ln = ln(1+rad_mean)
		encode c_code_2011, gen(c_code_2011_num)
		encode state_code_2011, gen(state_code_2011_num)
		
		**# TWFE
		* Transform (see https://www.statalist.org/forums/forum/general-stata-discussion/general/1522076-how-do-i-interpret-a-log-level-and-log-log-model-when-my-independent-variable-is-already-a-percentage)
		foreach v of varlist dist_f_cum_km2* {
			g ln_`v' = log(1+`v')
		}
		la var ln_dist_f_cum_km2 "Forest Infrastructure (Stage-II)"
		g ln_tree_cover_pct = ln(tree_cover_pct)
		g ln_tree_cover_km2 = ln(tree_cover_km2)
		local controls temp_era rain_gpm rad_mean
		
		
		**** DOUBLE CHECK THIS. SHOW THAT PUBLIC CLEARS MORE THAN APPROVED??
		g dist_f_pub_cum_km2 = dist_f_cen_cum_km2 + dist_f_sta_cum_km2
		replace dist_f_nei_cum_km2 = dist_f_nei_cum_km2 + dist_f_joi_cum_km2
		g ln_dist_f_pub_cum_km2 = log(1+dist_f_pub_cum_km2)
		g ln_dist_f_pri_cum_km2 = log(1+dist_f_pri_cum_km2)
		g ln_dist_f_nei_cum_km2 = log(1+dist_f_nei_cum_km2)
		eststo: reghdfe tree_cover_pct dist_f_pub_cum_km2 dist_f_pri_cum_km2 dist_f_nei_cum_km2 `controls', ///
			a(c_code_2011_num state_code_2011_num#year) vce(cl c_code_2011_num)
			
			estadd local dist_fe "$\checkmark$"
			estadd local st_y_fe "$\checkmark$"
	
		
		* lin-lin
		eststo: reghdfe tree_cover_pct dist_f_cum_km2* `controls', ///
			a(c_code_2011_num state_code_2011_num#year) vce(cl c_code_2011_num)
			
			estadd local dist_fe "$\checkmark$"
			estadd local st_y_fe "$\checkmark$"

		* log-log
		drop dist_f_cum_km2* tree_cover*
		ren *ln_* **
		eststo: reghdfe tree_cover_pct dist_f_cum_km2* `controls', ///
			a(c_code_2011_num state_code_2011_num#year) vce(cl c_code_2011_num) 
			
			estadd local dist_fe "$\checkmark$"
			estadd local st_y_fe "$\checkmark$"
		
	}
	esttab using "${TABLE}/tables/deforestation_verify.tex", ///
		replace stats(dist_fe st_y_fe N, labels(`"District FEs"' ///
		`"State x Year FEs"' `"N"') fmt(0 0 0)) mgroups("Data: Full Sample" ///
		"Data: Digital Subsample", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) ///
		suffix(}) span erepeat(\cmidrule(lr){@span})) mlabels("Linear" "Log" ///
		"Linear" "Log") wrap nocons nonotes indicate("Controls=`controls'") ///
		booktabs nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) ///
		width(1\hsize) varwidth(40)
	eststo clear
}

*===============================================================================
* EVENT STUDY - SPECIES DIVERSION
*===============================================================================
if `event_study' == 1 {
	
	**# First Event (persistent) -- only works w weighted OLS...
	
	* Read
	use "${DATA}/dta/fc_dym_s2_v02", clear
	keep *_code_2011* year month year_month dist_f_cum_km2 tree*
	
	* First Event
	bys c_code_2011 (year_month): gen event = year_month if ///
		dist_f_cum_km2[_n] != dist_f_cum_km2[_n-1]
	bys c_code_2011 (year_month): replace event = . if _n == 1 
	bys c_code_2011: egen first_event = min(event) // first event (0 if never treated)
	drop event
	recode first_event (.=0)
	g running = year_month - first_event // time to treatment
	replace running = -99 if first_event == 0
	keep if inrange(running, -6, 12)
	tab running, gen(t_)
	
	* Merge to ebird
	merge 1:m c_code_2011 year_month using "${DATA}/dta/ebird_udt", keep(3) nogen // merge ebird
	g tree_cover_s = (tree_cover_km2 / tot_area)*100
	
	* Prep
	keep if inrange(year, 2015,2020)
	drop_outliers
	local ctrls temp rain tree_cover_s ln_duration ln_distance ln_rad_sum ///
	        ln_exp_idx ln_coverage_udym ln_group_size traveling ln_hour
	
	* Regression
	reghdfe sr t_1-t_5 t_7-t_19 `ctrls' [aw=n_trips], ///
		a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
	
	* Figure
	parmest, label list(parm label estimate min* max* p) norestore
	replace parm = subinstr(label, "running==", "",.)
	destring parm, replace force
	drop if parm == . | p == .
	expand 2 if parm == 0, gen(omit) // omitted group
	replace parm = -1 if omit
	foreach v of varlist estimate min95 max95 {
		replace `v' = 0 if omit
	}
	twoway (rarea min95 max95 parm, sort color(dkgreen*0.5) lpattern(shortdash)) ///
		   (connected  estimate parm, sort mcolor(black) mfcolor(white) ///
		   lpattern(solid) lcolor(black) msymbol(D)), yline(0, lcolor(maroon) ///
		   lpattern(solid)) legend(on order(1 "95% CI" 2 "Coefficient") size(med)) ///
		   xtitle("Months Since Approval", size(large)) ///
		   ytitle("Species Richness", size(large)) xlabel(-6(2)12, labsize(large)) ///
		   ylabel(, labsize(large)) ysize(3.5)
	graph export "${TABLE}/fig/event_study_persistent.png", replace
	kkk
	
	**# Callaway Sant'anna
	
	* Read deforestation
	use "${DATA}/dta/fc_dym_s2_v02", clear
	keep *_code_2011* year month year_month dist_f_cum_km2 tree* 
	
	* Prep
	bys c_code_2011 (year_month): gen event = year_month if ///
		dist_f_cum_km2[_n] != dist_f_cum_km2[_n-1]
	bys c_code_2011 (year_month): replace event = . if _n == 1 
	bys c_code_2011: egen first_event = min(event) // first event (0 if never treated)
	drop event
	recode first_event (.=0)
	
	* Merge to ebird
	merge 1:m c_code_2011 year_month using "${DATA}/dta/ebird_udt", keep(3) nogen // merge ebird
	g tree_cover_s = (tree_cover_km2 / tot_area)*100

	* Residualize
	egen panelid = group(uid c_code_2011 year_month)
	
	
	
	csdid sr, time(year_month) gvar(first_event) method(dripw) notyet
	kk
	
	
	
	
	
	
	**# First event only (OLS) -- doesnt work...
	
	* Read Project Panel
	use "${DATA}/dta/fc_dym_s2_v02", clear
	keep *_code_2011* year month year_month dist_f_cum_km2 tree*
	
	* Event
	bys c_code_2011 (year_month): gen event = year_month if ///
		dist_f_cum_km2[_n] != dist_f_cum_km2[_n-1]
	bys c_code_2011 (year_month): replace event = . if _n == 1  
	
	* Time to event (first event only)
	bys c_code_2011: egen first_event = min(event) // first event (0 if never treated)
	bys c_code_2011: replace event = . if event == first_event
	bys c_code_2011: egen second_event = min(event)
	drop event
	recode first_event (.=0)
	recode second_event (.=0)
	keep if year_month < second_event
	g running = year_month - first_event // time to treatment
	replace running = -99 if first_event == 0
	keep if inrange(running, -12, 24)
	tab running, gen(t_)
	
	* Merge to ebird
	merge 1:m c_code_2011 year_month using "${DATA}/dta/ebird_udt", keep(3) nogen // merge ebird
	g tree_cover_s = (tree_cover_km2 / tot_area)*100
	
	* Prep
	keep if inrange(year, 2015, 2020)
	drop_outliers
	local ctrls temp rain tree_cover_s ln_duration ln_distance ln_rad_sum ///
	        ln_exp_idx ln_coverage_udym ln_group_size traveling ln_hour
	
	* Regression
	reghdfe sr t_1-t_11 t_13-t_37 `ctrls', ///
		a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
	
	* Figure 
	parmest, label list(parm label estimate min* max* p) norestore
	replace parm = subinstr(label, "running==", "",.)
	destring parm, replace force
	drop if parm == . | p == .
	expand 2 if parm == 0, gen(omit) // omitted group
	replace parm = -1 if omit
	foreach v of varlist estimate min95 max95 {
		replace `v' = 0 if omit
	}
	twoway (rarea min95 max95 parm, sort color(dkgreen*0.5) lpattern(shortdash)) ///
		   (connected  estimate parm, sort mcolor(black) mfcolor(white) ///
		   lpattern(solid) lcolor(black) msymbol(D)), yline(0, lcolor(maroon) ///
		   lpattern(solid)) xline(-1, lcolor(maroon) lpattern(solid)) ///
		   legend(on order(1 "95% CI" 2 "Coefficient")) ///
		   xtitle("Months Since Approval", size(medium)) ytitle("Species Richness", ///
		   size(medium)) xlabel(-6(2)12, labsize(med)) ///
		   ylabel(, labsize(med)) xsize(4.6)
	graph export "${TABLE}/fig/event_study_first.png", width(1000) replace
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

*===============================================================================
* FOREST RIGHTS ACT
*===============================================================================
if `fra' == 1 {
	
	**# Distribution of projects obtaining FRA
	
	* Read project level
	use "${DATA}/dta/fc_pdym_s2_v02", clear
	
	g n_proj = 1
	collapse (sum) fra = proj_fra_num n_proj, by(c_code_2011)
	g fra_share = fra/n_proj
	su fra_share, d
	local mean = round(r(mean),.01)
	hist fra_share, frac bcolor(dkgreen*0.5) ///
		xtitle("Share of District Projects Obtaining Gram Sabha Consent", size(large)) ///
		ytitle("Share of Districts", size(large)) ///
		xlabel(,labsize(medium)) ylabel(,labsize(large)) ///
		xline(`mean', lcolor(red)) ///
		text(0.205 .6 "Mean = `mean'", place(n) size(large)) ysize(3)
	graph export "${TABLE}/fig/hist_fra.png", replace
	
}

