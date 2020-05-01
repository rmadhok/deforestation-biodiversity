*********************************************************************************
*																			
* PROJECT: 	Deforestation and Biodiversity										
*																			
* PURPOSE: 	Summary Tables	    
*																			
* START DATE: January 7, 2019    				 							
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
gl TABLE	"${ROOT}/docs/jmp/tex_doc/"

// Module
local sumstats			1
local main_analysis		0
local robustness		0
local appendix			0

*===============================================================================
* SUMMARY STATISTICS
*===============================================================================
if `sumstats' == 1 {
	
	/*
	* Read
	use "${DATA}/dta/fc_ebd_user_main", clear
	keep if year >= 2015 & year <=2018
	
	* Tag 
	bys c_code_2011: egen any_def = max(dist_f > 0 & dist_f!=.)
	egen tag_d = tag(c_code_2011) //district-level
	*egen tag_u = tag(observerid)  //user
	
	* Indent for pretty latex formatting
	foreach v of varlist dist_f_cum dist_f_*_cum* tot_area tree* ///
		s_richness pop_density coverage n_* duration distance temp* rain* { 
	
			label variable `v' `"\hspace{0.2cm} `: variable label `v''"'	
		}

	** Collect
	local dist_vars tot_area pop_density n_users_dist n_trips_dist
	*local user_details n_trips_user n_states_user n_dist_user n_ym_user 
	local trip s_richness duration distance
	local covariates tree_cover_mean coverage rainfall_mm temperature_mean
	
	** District level
	eststo clear
	*eststo A: estpost tabstat `dist_vars' if tag_d, s(n mean sd) c(s)
	eststo B: estpost tabstat `dist_vars' if tag_d & any_def, s(n mean med sd) c(s)
	eststo C: estpost tabstat `dist_vars' if tag_d & !any_def, s(n mean med sd) c(s)
	esttab B C using "${TABLE}/tables/sumstats_biodiv.tex", replace f main(mean) aux(sd) ///
		cells("mean(star fmt(2)) p50(star fmt(2)) count(fmt(0))" "sd(par fmt(2))") ///
		width(\hsize) mgroups("Deforestation Districts" ///
		"\shortstack{Non-deforestation \\ Districts}", pattern(1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		collabel("\specialcell{Mean}" "Median" "N", prefix({) suffix(})) ///
		refcat(tot_area "\emph{District Variables}" , nolabel) ///
		nomtitle booktabs noobs label unstack nonumber
	
	/*
	** User Details
	eststo clear
	*eststo A: estpost tabstat `user_details', s(n mean sd) c(s)
	eststo B: estpost tabstat `user_details' if tag_u & any_def, s(n mean med sd) c(s)
	eststo C: estpost tabstat `user_details' if tag_u & !any_def, s(n mean med sd) c(s)
	
	esttab B C using "${TABLE}/tables/sumstats_biodiv.tex", append f main(mean) aux(sd) ///
		cells("mean(star fmt(2)) p50(star fmt(2)) count(fmt(0))" "sd(par fmt(2))") ///
		width(\hsize) refcat(n_trips_user "\emph{User Characteristics}" , nolabel) ///
		nomtitle collabel(none) booktabs noobs label unstack nonumber plain
	*/
	
	** Birdwatching Details
	eststo clear
	*eststo A: estpost tabstat `biodiversity', s(n mean sd) c(s)
	eststo B: estpost tabstat `trip' if any_def, s(n mean med sd) c(s)
	eststo C: estpost tabstat `trip' if !any_def, s(n mean med sd) c(s)
	
	esttab B C using "${TABLE}/tables/sumstats_biodiv.tex", append f main(mean) aux(sd) ///
		cells("mean(star fmt(2)) p50(star fmt(2)) count(fmt(0))" "sd(par fmt(2))") ///
		width(\hsize) refcat(s_richness "\emph{Birdwatching}" , nolabel) ///
		nomtitle collabel(none) booktabs noobs label unstack nonumber plain
	
	** Covariates
	eststo clear
	*eststo A: estpost tabstat `covariates', s(n mean sd) c(s)
	eststo B: estpost tabstat `covariates' if any_def, s(n mean med sd) c(s)
	eststo C: estpost tabstat `covariates' if !any_def, s(n mean med sd) c(s)
	
	esttab B C using "${TABLE}/tables/sumstats_biodiv.tex", append f main(mean) aux(sd) ///
		cells("mean(star fmt(2)) p50(star fmt(2)) count(fmt(0))" "sd(par fmt(2))") ///
		width(\hsize) refcat(tree_cover_mean "\emph{Covariates}" , nolabel) ///
		nomtitle collabel(none) booktabs noobs label unstack nonumber plain ///
		addnotes("Note: Spatial coverage is the fraction of grid cells in a district" ///
		"containing at least one bird observation over the study period using an" ///
		"5km $\times$ 5km grid")

	** Forest Clearance
	// Deforestation
	ren dist_f Total
	eststo clear
	eststo A: estpost tabstat Total dist_f_elec-dist_f_pa if any_def, s(n mean sd) c(s)
	
	// Project Breakup
	use "${DATA}/dta/fc_clean", clear
	keep if prop_status == "approved"
	replace proj_cat = "other" if proj_cat == "industry" | proj_cat == "underground"
	tab proj_cat, gen(proj_cat_)
	tab proj_shape, gen(proj_shape_)
	ren (proj_cat_1 proj_cat_2 proj_cat_3 proj_cat_4 proj_cat_5 proj_cat_6) ///
		(dist_f_elec dist_f_irr dist_f_mine dist_f_o dist_f_res dist_f_tran)
	ren (proj_shape_1 proj_shape_2 proj_shape_3 proj_in_pa_esz_num) (dist_f_hyb dist_f_lin dist_f_nl dist_f_pa)
	foreach v of varlist dist_f_elec-dist_f_nl {
		local x : variable label `v'
		local y = subinstr("`x'", " ", "", .)
		local y = subinstr("`y'", "proj_cat==", "", .)
		local y = subinstr("`y'", "proj_shape==", "", .)
		local y = proper("`y'")
		la var `v' `y'
	}
	la var dist_f_nl "Non-Linear"
	la var dist_f_pa "Near Protected Area"
	
	eststo B: estpost tabstat dist_f_elec-dist_f_nl dist_f_pa, s(n mean sd) c(s)

	esttab B A using "${TABLE}/tables/sumstats_deforest.tex", replace ///
		label cells("mean(star fmt(2)) count(fmt(0))" "sd(par fmt(2))") ///
		width(\hsize) mgroups("Project-Wise Break-Up" ///
		"Project-Wise Deforestation", pattern(1 1) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		collabel("\specialcell{Mean}" "N", prefix({) suffix(})) ///
		refcat(dist_f_elec "\emph{By Project Category}" ///
		dist_f_hyb "\emph{By Project Type}", nolabel) ///
		nomtitle booktabs noobs unstack nonumber gap	
	*/
	
}

*===============================================================================
* MAIN ANALYSIS
*===============================================================================
// Regression Program
capture program drop reg_user
program define reg_user

	syntax varlist
	set more off
	
	** Parse
	local depvar : word 1 of `varlist'
	di "`depvar'"
	local indepvar : word 2 of `varlist'
	di "`indepvar'"
	local ctrls = substr("`varlist'", length("`depvar'") + length("`indepvar'") + 3, length("`varlist'"))
	di "`ctrls'"

	eststo: qui reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips], ///
		a(user_id) vce(r)
		estadd local user_fe "$\checkmark$"
	eststo: qui reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips], ///
		a(user_id c_code_2011_num) vce(r)
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
	eststo: qui reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips], ///
		a(user_id c_code_2011_num state_code_2011_num#year) vce(r)
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_y_fe "$\checkmark$"
	eststo: qui reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips], ///
		a(user_id c_code_2011_num state_code_2011_num#month year) vce(r)
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
	eststo: qui reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) vce(r)
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
end

// Saturated
capture program drop reg_sat
program define reg_sat
	
	syntax varlist
	set more off
	
	* Parse 
	local depvar : word 1 of `varlist'
	di "`depvar'"
	local indepvar : word 2 of `varlist'
	di "`indepvar'"
	local ctrls = substr("`varlist'", length("`depvar'") + length("`indepvar'") + 3, length("`varlist'"))
	di "`ctrls'"

	qui reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) vce(r)
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$" 
end

* Econ Growth Regs
capture program drop reg_iv
program define reg_iv
	syntax varlist
	set more off

	** Parse
	local depvar : word 1 of `varlist'
	local indepvar : word 2 of `varlist'
	local ctrls = substr("`varlist'", length("`depvar'") + length("`indepvar'") + 3, length("`varlist'"))
	
	eststo: qui reghdfe `depvar' `indepvar' `ctrls', a(c_code_2011_num) vce(cl state_code_2011_num#year)
		estadd local dist_fe "$\checkmark$"
	eststo: qui reghdfe `depvar' `indepvar' `ctrls', ///
		a(c_code_2011_num state_code_2011_num#year) vce(cl state_code_2011_num#year)
		estadd local dist_fe "$\checkmark$"
		estadd local st_y_fe "$\checkmark$"
	eststo: qui reghdfe `depvar' `indepvar' `ctrls', ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl state_code_2011_num#year)
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
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
	

if `main_analysis' == 1 {

	* Read
	use "${DATA}/dta/fc_ebd_user_main", clear
	local ctrls coverage tree_cover_mean_ihs temperature_mean rainfall_mm all_species_prop
	drop_outliers
kk
	//1. Species Richness on Deforestation
	reg_user s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls'
	esttab using "${TABLE}/tables/s_richness.tex", replace ///
		keep(dist* coverage* tree*) stats(user_fe user_y_fe dist_fe st_y_fe ///
		st_m_fe year_fe N r2, labels(`"User FEs"' `"User $\times$ Year FEs"' ///
		`"District FEs"' `"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
		`"Year FE"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 0 0 3)) nocons nomtitles ///
		star(* .1 ** .05 *** .01) label nonotes booktabs se b(%5.3f) ///
		se(%5.3f) width(\hsize)
	eststo clear
	
	//2a. Time Cost on Deforestation
	reg_user time_cost dist_f_cum_km2 dist_nf_cum_km2 `ctrls' 
	esttab using "${TABLE}/tables/time_cost.tex", replace ///
		keep(dist* coverage* tree*) stats(user_fe user_y_fe dist_fe st_y_fe ///
		st_m_fe year_fe N r2, labels(`"User FEs"' `"User $\times$ Year FEs"' ///
		`"District FEs"' `"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
		`"Year FE"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 0 0 3)) nocons nomtitles ///
		star(* .1 ** .05 *** .01) label nonotes booktabs se b(%5.3f) ///
		se(%5.3f) width(\hsize)
	eststo clear
	
	//2b. Species Diversity on Birding Activity
	preserve
	la_var_tex dist_f_cum_km2 dist_nf_cum_km2 `ctrls'
	
	* Panel A: Duration
	reg_user duration dist_f_cum_km2 dist_nf_cum_km2 `ctrls' 
	esttab using "${TABLE}/tables/birding_activity.tex", replace f ///
		keep(dist* coverage* tree*) stats(user_fe user_y_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"User $\times$ Year FEs"' ///
		`"District FEs"' `"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
		`"Year FE"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 0 0 3)) nocons ///
		refcat(dist_f_cum_km2 "\emph{Panel A: Duration (min)}", nolabel) ///
		nomtitles star(* .1 ** .05 *** .01) label nonotes booktabs se ///
		b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear	
	// Panel B: Distance
	reg_user distance dist_f_cum_km2 dist_nf_cum_km2 `ctrls' 
	esttab using "${TABLE}/tables/birding_activity.tex", append f ///
		keep(dist* coverage* tree*) stats(user_fe user_y_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"User $\times$ Year FEs"' ///
		`"District FEs"' `"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
		`"Year FE"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 0 0 3)) nocons ///
		refcat(dist_f_cum_km2 "\emph{Panel B: Distance (km)}", nolabel) ///
		nomtitles star(* .1 ** .05 *** .01) label nonotes booktabs ///
		mlabels(none) nonumbers se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
	
	//3. Species Richness on Project-Wise Deforestation
	unab projvars: dist_f_*_cum_ihs
	la_var_tex `projvars'
		
	* Electricity
	reg_user s_richness_ihs dist_f_elec_cum_ihs dist_nf_elec_cum_ihs `ctrls'
	esttab using "${TABLE}/tables/projwise_s_richness.tex", f replace ///
		keep(dist_f_elec_cum_ihs) refcat(dist_f_elec_cum_ihs ///
		"\emph{Project Category}", nolabel) nocons nomtitles label noobs nonotes ///
		booktabs star(* .1 ** .05 *** .01) se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
		
	* Other Projects
	foreach i in irr mine o res tran pa hyb lin {
		
		reg_user s_richness_ihs dist_f_`i'_cum_ihs dist_nf_`i'_cum_ihs `ctrls'
		esttab using "${TABLE}/tables/projwise_s_richness.tex", append f ///
			keep(dist_f_`i'_cum_ihs) refcat(dist_f_hyb_cum_ihs ///
			"\emph{Project Type}", nolabel) nocons nolines nomtitles label ///
			nonotes mlabels(none) nonumbers booktabs star(* .1 ** .05 *** .01) ///
			noobs se b(%5.3f) se(%5.3f) width(\hsize)
		eststo clear
		}
		
	* Non-linear projects
	reg_user s_richness_ihs dist_f_nl_cum_ihs dist_nf_nl_cum_ihs `ctrls'
	esttab using "${TABLE}/tables/projwise_s_richness.tex", append f ///
		keep(dist_f_nl_cum_ihs) stats(user_fe user_y_fe dist_fe st_y_fe ///
		st_m_fe year_fe N, labels(`"User FEs"' `"User $\times$ Year FEs"' ///
		`"District FEs"' `"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
		`"Year FE"' `"N"') fmt(0 0 0 0 0 0 0)) nocons nomtitles label nolines ///
		nonotes nonumbers booktabs star(* .1 ** .05 *** .01) se b(%5.3f) ///
		se(%5.3f) width(\hsize) prefoot(\hline)
	eststo clear
	restore
	
	// Plot Project Coeffs
	foreach proj in elec irr mine o res tran pa hyb lin nl {
		
		eststo `proj': qui reghdfe s_richness_ihs dist_f_`proj'_cum_ihs ///
			dist_nf_`proj'_cum_ihs `ctrls' [aweight=n_trips], ///
			a(user_id#year c_code_2011_num state_code_2011_num#month) vce(r)
		}
		
	set scheme s2color
	coefplot (elec \ irr \ mine \ o \ res \ tran \ pa \ lin \ nl), ///
		keep(dist_f*) xline(0, lcolor(gs10)) msymbol(D) msize(small) ///
		mcolor(gs4) ciopts(color(gs2)) graphregion(color(white)) bgcolor(white)
	graph export "${TABLE}/fig/projwise_coef.png", replace
	eststo clear
*/
	
	// 4. Deforestation on nightlights
	
	use "${DATA}/dta/def_lights_dist", clear
	local controls tree_cover_mean_ihs temperature_mean rainfall_mm
	
	egen sy = group(state_code_2011_num year)
	* OLS
	reg_iv mean_lights dist_f_cum_km2 `controls'
	* 2SLS
	eststo: qui ivreghdfe mean_lights (dist_f_cum_km2=dist_f_cum_km2_s1) `controls', ///
		a(c_code_2011_num) cluster(sy)
		estadd local dist_fe "$\checkmark$"
		estadd scalar fsF = `e(widstat)' 
	eststo: qui ivreghdfe mean_lights (dist_f_cum_km2=dist_f_cum_km2_s1) `controls', ///
		a(c_code_2011_num state_code_2011_num#year) cluster(sy)
		estadd local dist_fe "$\checkmark$"
		estadd local st_y_fe "$\checkmark$"
		estadd scalar fsF = `e(widstat)' 
	eststo: qui ivreghdfe mean_lights (dist_f_cum_km2=dist_f_cum_km2_s1) `controls', ///
		a(c_code_2011_num state_code_2011_num#month year) cluster(sy)
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
		estadd scalar fsF = `e(widstat)' 

	esttab using "${TABLE}/tables/def_nl_2sls.tex", replace ///
		stats(dist_fe st_y_fe st_m_fe year_fe N r2 fsF, ///
		labels(`"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"' ///
		`"\(F\)-stat on Instrument"') fmt(0 0 0 0 0 3 3)) ///
		mgroups("OLS" "2SLS", pattern(1 0 0 1 0 0) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span ///
		erepeat(\cmidrule(lr){@span}))  nocons nomtitles ///
		star(* .1 ** .05 *** .01) label nonotes booktabs se b(%5.3f) ///
		se(%5.3f) width(\hsize)
	eststo clear

	* First Stage
	la var dist_f_cum_km2_s1 "Stage I Area (ha.)"
	reg_iv dist_f_cum_km2 dist_f_cum_km2_s1 `controls'
	reg_iv mean_lights dist_f_cum_km2_s1 `controls'
	local group "&\multicolumn{3}{c}{First Stage}&\multicolumn{3}{c}{Reduced Form}       \\\cmidrule(lr){2-4}\cmidrule(lr){5-7}"
	local titles "&\multicolumn{3}{c}{Dependent Variable: Deforestation (ha.)}&\multicolumn{3}{c}{Dependent Variable: Night Lights}       \\\cmidrule(lr){2-4}\cmidrule(lr){5-7}"
	local numbers "& (1) & (2) & (3) & (4) & (5) & (6) \\ \hline \\ \vspace{.5mm}"
	esttab using "${TABLE}/tables/def_nl_fs.tex", replace ///
		stats(dist_fe st_y_fe st_m_fe year_fe N r2, ///
		labels(`"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 3)) mgroups("First Stage" "Reduced Form", pattern(1 0 0 1 0 0) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span ///
		erepeat(\cmidrule(lr){@span})) posthead("`group2'" "`titles'" "`numbers'") ///
		nonumbers nocons nomtitles star(* .1 ** .05 *** .01) label nonotes ///
		booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear

}

*===============================================================================
* ROBUSTNESS CHECKS
*===============================================================================
if `robustness' == 1 {
	
	* Read
	use "${DATA}/dta/fc_ebd_user_main", clear
	drop_outliers

	* Controls
	local ctrls coverage tree_cover_mean_ihs temperature_mean rainfall_mm all_species_prop
	local ctrls_a `ctrls' all_species_prop user_trend
	local slx_b dist_f_cum_ihs_slx_bc dist_nf_cum_ihs_slx_bc

	//1. Additional Controls and FE
	eststo: reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls' // Main
	eststo: reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls' all_species_prop // veteran
	eststo: reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls' all_species_prop user_lin_trend // time-trend
	eststo: reghdfe s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls_a' n_mon_yr, ///
		a(user_id#month c_code_2011_num state_code_2011_num#year) vce(r)
			estadd local user_m_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_y_fe "$\checkmark$"

	esttab using "${TABLE}/tables/rchecks_controls.tex", replace ///
		drop(temp* rain* _cons) stats(user_y_fe dist_fe st_m_fe st_y_fe  ///
		user_m_fe N r2, labels(`"User $\times$ Year FEs"' `"District FEs"' ///
		`"State $\times$ Month FEs"' `"State $\times$ Year FEs"' ///
		`"User $\times$ Month FEs"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) mgroups("Additional Controls" "Additional FEs", ///
		pattern(1 0 0 1) prefix(\multicolumn{@span}{c}{) suffix(}) span ///
		erepeat(\cmidrule(lr){@span})) nomtitles star(* .1 ** .05 *** .01) ///
		label nonotes booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear

	//2. SLX Estimates - Phase 1 and 2
	foreach df in main robust{
		
		use "${DATA}/dta/fc_ebd_user_`df'", clear
		drop_outliers
			
		eststo: reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls_a' // No SLX
		eststo: reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls_a' `slx_b' // binary cont.
		preserve
		drop *_slx_bc
		ren *_slx_i *_slx_bc
		eststo: reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls_a' `slx_b' // 1/D
		drop *_slx_bc
		ren *_slx_i2 *_slx_bc
		eststo: reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls_a' `slx_b' // 1/D sq.
		restore
	}
	esttab using "${TABLE}/tables/rchecks_slx.tex", replace ///
		drop(temp* rain* cov* _cons) stats(user_y_fe dist_fe st_m_fe N r2, ///
		labels(`"User $\times$ Year FEs"' `"District FEs"' `"State $\times$ Month FEs"' ///
		`"N"' `"\(R^{2}\)"') fmt(0 0 0 0 3)) mgroups("Phase II Approvals" ///
		"Phase I and II Approvals", pattern(1 0 0 0 1 0 0 0) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		mlabels("Main" "W=Queen" "W=$\frac{1}{d}$" "W=$\frac{1}{d^2}$" ///
		"Main" "W=Queen" "W=$\frac{1}{d}$" "W=$\frac{1}{d^2}$") ///
		indicate("Veteran Controls=all_species_prop" "User Time-Trend=user_trend") ///
		nomtitles star(* .1 ** .05 *** .01) label nonotes booktabs se b(%5.3f) ///
		se(%5.3f) width(\hsize)
	eststo clear

	//Project-Wise SLX Plot
	foreach df in main robust {
		
		use "${DATA}/dta/fc_ebd_user_`df'", clear
		drop_outliers
		
		foreach proj in elec irr mine o res tran pa hyb lin nl {

			eststo `proj'_`df': reg_sat s_richness_ihs dist_f_`proj'_cum_ihs ///
				dist_nf_`proj'_cum_ihs `ctrls_a'
			eststo `proj'_w_`df': reg_sat s_richness_ihs dist_f_`proj'_cum_ihs ///
				dist_nf_`proj'_cum_ihs dist_f_`proj'_cum_ihs_slx_i2 ///
				dist_nf_`proj'_cum_ihs_slx_i2 `ctrls_a' 
		}
	}
	set scheme s1mono
	coefplot (elec_main \ irr_main \ mine_main \ o_main \ res_main \ tran_main \ pa_main \ lin_main \ nl_main \, ///
				label("No SLX") mcolor(gs3) ciopts(color(gs3))) ///
			 (elec_w_main irr_w_main mine_w_main o_w_main res_w_main tran_w_main pa_w_main lin_w_main nl_w_main, ///
				label("W=1/d{superscript:2}") mcolor(gs7) ciopts(color(gs7))), keep(dist_f*ihs) bylabel("Phase II Approvals")  || ///
			 (elec_robust \ irr_robust \ mine_robust \ o_robust \ res_robust \ tran_robust \ pa_robust \ lin_robust \ nl_robust \, ///
				label("No SLX") mcolor(gs3) ciopts(color(gs3))) ///
			 (elec_w_robust irr_w_robust mine_w_robust o_w_robust res_w_robust tran_w_robust pa_w_robust lin_w_robust nl_w_robust, ///
				label("W=1/d{superscript:2}") mcolor(gs7) ciopts(color(gs7))), keep(dist_f*ihs) bylabel("Phase I and II Approvals") || ///
		 , xline(0, lcolor(gs10)) msymbol(D) msize(vsmall) graphregion(color(white)) bgcolor(white)	
	graph export "${TABLE}/fig/projwise_coef_slx_i2.pdf", replace
	eststo clear

	//5. Birding Activity	
	foreach var of varlist duration distance{
		foreach df in main robust {

			use "${DATA}/dta/fc_ebd_user_`df'", clear
			drop_outliers
			la_var_tex dist_f_cum_km2 dist_nf_cum_km2 `ctrls'

			eststo `var'_`df'_1 : reg_sat `var' dist_f_cum_km2 dist_nf_cum_km2 `ctrls' // Main
			eststo `var'_`df'_2 : reg_sat `var' dist_f_cum_km2 dist_nf_cum_km2 `ctrls_a' // Additional controls
		}
	}
	esttab duration_main_1 duration_main_2 duration_robust_1 duration_robust_2 ///
		using "${TABLE}/tables/rchecks_birding_activity.tex", ///
		replace f drop(temp* rain* _cons) stats(user_y_fe dist_fe st_m_fe N r2, ///
		labels(`"User $\times$ Year"' `"District FEs"' `"State $\times$ Month FEs"' ///
		`"N"' `"\(R^{2}\)"') fmt(0 0 0 0 3)) mgroups("Phase II Approvals" ///
		"Phase I and II Approvals", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) ///
		suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		mlabels("Main" "Ad. Controls" "Main" "Ad. Controls") ///
		indicate("Veteran Controls=all_species_prop" "User Time-Trend=user_trend") ///
		refcat(dist_f_cum_km2 "\emph{Panel A: Duration (min)}", nolabel) ///
		nomtitles star(* .1 ** .05 *** .01) label nonotes booktabs se ///
		b(%5.3f) se(%5.3f) width(\hsize)
		
	esttab distance_main_1 distance_main_2 distance_robust_1 distance_robust_2 ///
		using "${TABLE}/tables/rchecks_birding_activity.tex", ///
		append f drop(temp* rain* _cons) stats(user_y_fe dist_fe st_m_fe ///
		N r2, labels(`"User $\times$ Year"' `"District FEs"' ///
		`"State $\times$ Month FEs"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 3)) ///
		indicate("Veteran Controls=all_species_prop" "User Time-Trend=user_trend") ///
		refcat(dist_f_cum_km2 "\emph{Panel B: Distance (km)}", nolabel) ///
		nonumbers nomtitles star(* .1 ** .05 *** .01) label nonotes booktabs ///
		se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear


	// By Forest Cover
	use "${DATA}/dta/fc_ebd_user_robust", clear
	drop_outliers
	xtile tree_cover_q = tree_cover_mean, nq(15)
	tab tree_cover_q, gen(tc_q)
	foreach i of numlist 1/15 {
		gen dist_f_cum_ihs_q`i' = dist_f_cum_ihs * tc_q`i'
		la var dist_f_cum_ihs_q`i' "`i'"
		gen dist_nf_cum_ihs_q`i' = dist_nf_cum_ihs * tc_q`i'
	}
	reghdfe s_richness_ihs dist_f_cum_ihs_q2-dist_f_cum_ihs_q15 ///
		dist_f_cum_ihs dist_nf_cum_ihs tc_q* `ctrls_a' tree_cover_mean_ihs [aweight=n_trips], ///
			a(user_id#year c_code_2011_num state_code_2011_num#month) vce(r)
	coefplot, keep(dist_f_cum_ihs_q*) vertical yline(0) xtitle("Quantile of % Tree Cover")
	graph export "${TABLE}/fig/coef_tc_q.pdf", replace	
	
}

*===============================================================================
* APPENDIX
*===============================================================================
if `appendix' == 1 {	

	* Read
	use "${DATA}/dta/fc_ebd_user_main", clear
	
	* Controls
	local ctrls coverage tree_cover_mean_ihs temperature_mean rainfall_mm
	local ctrls_a `ctrls' all_species_prop user_trend
	local slx_b dist_f_cum_ihs_slx_bc dist_nf_cum_ihs_slx_bc

	//1. Project-Wise SLX Plot
	foreach df in main robust {
		
		use "${DATA}/dta/fc_ebd_user_`df'", clear
		drop_outliers
		
		foreach proj in elec irr mine o res tran pa hyb lin nl {

			eststo `proj'_`df': reg_sat s_richness_ihs dist_f_`proj'_cum_ihs ///
				dist_nf_`proj'_cum_ihs `ctrls_a'
			eststo `proj'_w1_`df': reg_sat s_richness_ihs dist_f_`proj'_cum_ihs ///
				dist_nf_`proj'_cum_ihs dist_f_`proj'_cum_ihs_slx_bc ///
				dist_nf_`proj'_cum_ihs_slx_bc `ctrls_a'
			eststo `proj'_w2_`df': reg_sat s_richness_ihs dist_f_`proj'_cum_ihs ///
				dist_nf_`proj'_cum_ihs dist_f_`proj'_cum_ihs_slx_i ///
				dist_nf_`proj'_cum_ihs_slx_i `ctrls_a'
			eststo `proj'_w3_`df': reg_sat s_richness_ihs dist_f_`proj'_cum_ihs ///
				dist_nf_`proj'_cum_ihs dist_f_`proj'_cum_ihs_slx_i2 ///
				dist_nf_`proj'_cum_ihs_slx_i2 `ctrls_a'
		}
	}
	set scheme s1mono
	coefplot (elec_main \ irr_main \ mine_main \ o_main \ res_main \ tran_main \ pa_main \ lin_main \ nl_main \, ///
				label("No SLX") mcolor(red) ciopts(color(red))) ///
			 (elec_w1_main irr_w1_main mine_w1_main o_w1_main res_w1_main tran_w1_main pa_w1_main lin_w1_main nl_w1_main, ///
				label("Queen") mcolor(orange) ciopts(color(orange))) ///
			 (elec_w2_main irr_w2_main mine_w2_main o_w2_main res_w2_main tran_w2_main pa_w2_main lin_w2_main nl_w2_main, ///
				label("W=1/d") mcolor(purple) ciopts(color(purple))) ///
			 (elec_w3_main irr_w3_main mine_w3_main o_w3_main res_w3_main tran_w3_main pa_w3_main lin_w3_main nl_w3_main, ///
				label("W=1/d{superscript:2}") mcolor(green) ciopts(color(green))), keep(dist_f*ihs) bylabel("Phase II Approvals")  || ///
			 (elec_robust \ irr_robust \ mine_robust \ o_robust \ res_robust \ tran_robust \ pa_robust \ lin_robust \ nl_robust \, ///
				label("No SLX") mcolor(red) ciopts(color(red))) ///
			 (elec_w1_robust irr_w1_robust mine_w1_robust o_w1_robust res_w1_robust tran_w1_robust pa_w1_robust lin_w1_robust nl_w1_robust, ///
				label("Queen") mcolor(orange) ciopts(color(orange))) ///
			 (elec_w2_robust irr_w2_robust mine_w2_robust o_w2_robust res_w2_robust tran_w2_robust pa_w2_robust lin_w2_robust nl_w2_robust, ///
				label("W=1/d") mcolor(purple) ciopts(color(purple))) ///
			 (elec_w3_robust irr_w3_robust mine_w3_robust o_w3_robust res_w3_robust tran_w3_robust pa_w3_robust lin_w3_robust nl_w3_robust, ///
				label("W=1/d{superscript:2}") mcolor(green) ciopts(color(green))), keep(dist_f*ihs) bylabel("Phase I and II Approvals") || ///
		 , xline(0, lcolor(gs10)) msymbol(D) msize(vsmall) graphregion(color(white)) bgcolor(white)	
	graph export "${TABLE}/fig/projwise_coef_slx.pdf", replace
	eststo clear

	//2. Alternative Diversity Measures
	
	use "${DATA}/dta/fc_ebd_user_main.dta", clear
	la_var_tex dist_f_cum_ihs dist_nf_cum_ihs `ctrls'
	
	* Panel A: Shannon Index
	reg_user sh_index_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls'
	esttab using "${TABLE}/tables/alt_diversity.tex", replace f ///
		keep(dist* coverage* tree*) stats(user_fe user_y_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"User $\times$ Year FEs"' ///
		`"District FEs"' `"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
		`"Year FE"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 0 0 3)) ///
		refcat(dist_f_cum_ihs "\emph{Panel A: Shannon Index}", nolabel) nocons ///
		nomtitles star(* .1 ** .05 *** .01) label nonotes booktabs se b(%5.3f) ///
		se(%5.3f) width(\hsize)
	eststo clear
	* Panel B: Simpson Index
	reg_user si_index_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls'
	esttab using "${TABLE}/tables/alt_diversity.tex", append f ///
		keep(dist* coverage* tree*) stats(user_fe user_y_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"User $\times$ Year FEs"' ///
		`"District FEs"' `"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
		`"Year FE"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 0 0 3)) ///
		nocons refcat(dist_f_cum_ihs "\emph{Panel B: Simpson Index}", nolabel) ///
		nomtitles star(* .1 ** .05 *** .01) label nonotes booktabs mlabels(none) ///
		nonumbers se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear

	//2. Project-Wise Alternative Diversity Measures
	preserve
	unab projvars: dist_f_*_cum_ihs
	la_var_tex `projvars'

	foreach y in sh_index si_index {
		
		* Electricity
		reg_user `y'_ihs dist_f_elec_cum_ihs dist_nf_elec_cum_ihs `ctrls'
		esttab using "${TABLE}/tables/projwise_`y'.tex", replace f ///
			keep(dist_f_elec_cum_ihs) refcat(dist_f_elec_cum_ihs ///
			"\emph{Project Category}", nolabel) nocons nomtitles ///
			label noobs nonotes booktabs star(* .1 ** .05 *** .01) se b(%5.3f) ///
			se(%5.3f) width(\hsize)
		eststo clear
		
		* Other Projects
		foreach i in irr mine o res tran pa hyb lin {
			
			reg_user `y'_ihs dist_f_`i'_cum_ihs dist_nf_`i'_cum_ihs `ctrls'
			esttab using "${TABLE}/tables/projwise_`y'.tex", append f ///
				keep(dist_f_`i'_cum_ihs) refcat(dist_f_hyb_cum_ihs ///
				"\emph{Project Type}", nolabel) nocons nolines nomtitles label ///
				nonotes mlabels(none) nonumbers booktabs star(* .1 ** .05 *** .01) ///
				noobs se b(%5.3f) se(%5.3f) width(\hsize)
			eststo clear
			}
		
		* Non-linear projects
		reg_user `y'_ihs dist_f_nl_cum_ihs dist_nf_nl_cum_ihs `ctrls'
		esttab using "${TABLE}/tables/projwise_`y'.tex", append f ///
			keep(dist_f_nl_cum_ihs) stats(user_y_fe dist_fe st_m_fe ///
			N r2, labels(`"User $\times$ Year"' `"District FEs"' ///
			`"State $\times$ Month FEs"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 3)) ///
			nocons nomtitles label nolines nonotes booktabs nonumbers ///
			prefoot(\hline) star(* .1 ** .05 *** .01) se b(%5.3f) se(%5.3f) width(\hsize)
		eststo clear
		
	}
	restore
	
	// Project-Wise Coefficient Plot
	foreach y in s_richness sh_index si_index {
		foreach x in elec irr mine o res tran pa hyb lin nl {
	
			eststo `y'_`x': reg_sat `y'_ihs dist_f_`x'_cum_ihs dist_nf_`x'_cum_ihs `ctrls'
		}
	}
	set scheme s1mono
	coefplot (s_richness_elec \ s_richness_irr \ s_richness_mine \ s_richness_o \ s_richness_res \ s_richness_tran \ s_richness_pa \ s_richness_lin \ s_richness_nl), ///
				keep(dist_f*) bylabel("Species Richness") || ///
			(sh_index_elec \ sh_index_irr \ sh_index_mine \ sh_index_o \ sh_index_res \ sh_index_tran \ sh_index_pa \ sh_index_lin \ sh_index_nl), ///
				keep(dist_f*) bylabel("Shannon Index") || ///
			(si_index_elec \ si_index_irr \ si_index_mine \ si_index_o \ si_index_res \ si_index_tran \ si_index_pa \ si_index_lin \ si_index_nl), ///
				keep(dist_f*) bylabel("Simpson Index") || ///
		, xline(0, lcolor(gs10)) msymbol(D) msize(small) ///
		mcolor(gs4) ciopts(color(gs2)) graphregion(color(white)) bgcolor(white)
	graph export "${TABLE}/fig/projwise_coef_alt.png", replace
	eststo clear

	//4. Species Richness on Lagged Deforestation
	
	* No Lag
	la var dist_f_cum_ihs "No lag"	
	foreach var in s_richness sh_index si_index {
		
		reg_user `var'_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls'
	
		* Store Beta, CI
		gen `var'_df_b_l0 = _b[dist_f_cum_ihs]
		gen `var'_df_ciu_l0 = _b[dist_f_cum_ihs] - invttail(e(df_r),0.025)*_se[dist_f_cum_ihs]
		gen `var'_df_cil_l0 = _b[dist_f_cum_ihs] + invttail(e(df_r),0.025)*_se[dist_f_cum_ihs]

		esttab using "${TABLE}/tables/`var'_lag.tex", replace f wrap ///
			keep(dist_f_cum_ihs) stats(N, labels("N") fmt(0)) label ///
			star(* .1 ** .05 *** .01) nocons nomtitles nonotes booktabs  ///
			width(\hsize) nolines posthead(\hline) b(%5.3f) se(%5.3f)
		eststo clear
	
		* Lags
		foreach i of numlist 1/12 {
		
			* Model
			reg_user `var'_ihs dist_f_cum_ihs_l`i' dist_nf_cum_ihs_l`i' `ctrls'
				
			* Store Coeff
			gen `var'_df_b_l`i' = _b[dist_f_cum_ihs_l`i']
	
			* Store CI
			gen `var'_df_ciu_l`i' = _b[dist_f_cum_ihs_l`i'] - invttail(e(df_r),0.025)*_se[dist_f_cum_ihs_l`i']
			gen `var'_df_cil_l`i' = _b[dist_f_cum_ihs_l`i'] + invttail(e(df_r),0.025)*_se[dist_f_cum_ihs_l`i']
		
			esttab using "${TABLE}/tables/`var'_lag.tex", append f wrap ///
				keep(dist_f_cum_ihs_l`i') stats(N, labels("N") fmt(0)) label ///
				star(* .1 ** .05 *** .01) nocons nonumbers nomtitles ///
				nonotes booktabs width(\hsize) nolines posthead(\hline) ///
				b(%5.3f) se(%5.3f)
			eststo clear
		}
	}
	la var dist_f_cum_ihs "Deforestation"

	** Coefficient Plot
	preserve
	
		* Prep
		keep *_df_*
		keep if _n == 1
		gen id = _n
	
		reshape long s_richness_df_b_l s_richness_df_ciu_l s_richness_df_cil_l ///
			sh_index_df_b_l sh_index_df_ciu_l sh_index_df_cil_l ///
			si_index_df_b_l si_index_df_ciu_l si_index_df_cil_l, ///
			i(id) j(lag)
			
		ren *_l *
		
		* Plot
		set scheme s2color
		foreach var in s_richness sh_index si_index {
		
			twoway rarea `var'_df_ciu `var'_df_cil lag, sort color(gs14) || ///
				scatter `var'_df_b lag || ///
				function y = 0, range(lag) color(gs8) ///
					xtitle("Lag (months)") xlabel(0(1)12) ///
					graphregion(color(white)) bgcolor(white) ///
					legend(order(1 "95% CI" 2 "Coefficient"))
			
			graph export "${TABLE}/fig/`var'_df_lag.png", replace
		}
	
	restore
}



