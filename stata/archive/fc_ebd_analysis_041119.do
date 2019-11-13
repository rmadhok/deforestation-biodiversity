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
gl TABLE	"${ROOT}/docs/tex_doc/"

// Modules
local sumstats			0
local main_analysis		0
local robustness		1
local appendix			0

*===============================================================================
* SUMMARY STATISTICS
*===============================================================================
if `sumstats' == 1 {
	
	* Read
	use "${DATA}/dta/fc_ebd_user_main", clear
	
	* Tag 
	bys c_code_2011: egen any_def = max(dist_f > 0 & dist_f!=.)
	egen tag_d = tag(c_code_2011) //deforestation districts
	egen tag_u = tag(observerid)  //user
	
	* Indent for pretty latex formatting
	la var dist_f_cum_km2 "All Projects"
	la var dist_f_cum "All Projects"
	foreach v of varlist dist_f_cum dist_f_*_cum* tot_area tree* ///
		s_* sh_* si_* pop_density coverage* n_* duration distance { 
	
			label variable `v' `"\hspace{0.2cm} `: variable label `v''"'	
		}
	
	tempfile temp
	save "`temp'"

	** Collect
	local dist_vars tot_area pop_density coverage_all n_users_dist
	local user_details n_trips_user n_states_user n_dist_user n_ym_user 
	local trip s_richness sh_index si_index duration distance
	
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
		nomtitle booktabs noobs label unstack nonumber gap
		
	** User Details
	eststo clear
	*eststo A: estpost tabstat `trip_details', s(n mean sd) c(s)
	eststo B: estpost tabstat `user_details' if tag_u & any_def, s(n mean med sd) c(s)
	eststo C: estpost tabstat `user_details' if tag_u & !any_def, s(n mean med sd) c(s)
	
	esttab B C using "${TABLE}/tables/sumstats_biodiv.tex", append f main(mean) aux(sd) ///
		cells("mean(star fmt(2)) p50(star fmt(2)) count(fmt(0))" "sd(par fmt(2))") ///
		width(\hsize) refcat(n_trips_user "\emph{User Characteristics}" , nolabel) ///
		nomtitle collabel(none) booktabs noobs label unstack nonumber plain gap
	
	** Trip Details
	eststo clear
	*eststo A: estpost tabstat `biodiversity', s(n mean sd) c(s)
	eststo B: estpost tabstat `trip' if any_def, s(n mean med sd) c(s)
	eststo C: estpost tabstat `trip' if !any_def, s(n mean med sd) c(s)
	
	esttab B C using "${TABLE}/tables/sumstats_biodiv.tex", append f main(mean) aux(sd) ///
		cells("mean(star fmt(2)) p50(star fmt(2)) count(fmt(0))" "sd(par fmt(2))") ///
		width(\hsize) refcat(s_richness "\emph{Birdwatching}" , nolabel) ///
		nomtitle collabel(none) booktabs noobs label unstack nonumber plain gap ///
		addnotes("Note: Spatial coverage is the fraction of grid cells in a district" ///
		"containing at least one bird observation over the study period using an" ///
		"8km $\times$ 8km grid")	
	
	** Deforestation
	use "`temp'", clear
	eststo clear
	eststo: estpost tabstat dist_f*cum if any_def, s(n mean sd p50 min max) c(s)
	
	local numbers "& (1) & (2) & (3) & (4) & (5) & (6) \\ \midrule"
	local titles "& {N} & {Mean} & {SD} & {p50} & {Min} & {Max} \\"
	esttab using "${TABLE}/tables/sumstats_deforest.tex", replace ///	
		cell((count mean(fmt(%9.2f)) sd(fmt(%9.2f)) p50(fmt(%9.0f)) min(fmt(%9.0f)) ///
		max(fmt(%9.0f)))) width(\hsize) ///
		refcat(dist_f_elec_cum "\emph{By Project Type}" ///
		dist_f_hyb_cum_km2 "\emph{By Project Shape}", nolabel) ///
		posthead("`titles'" "`numbers'") nonumbers nomtitles collabels(none) ///
		mlabels(none) booktabs noobs gap label substitute("$" "\$")
	eststo clear	
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

	eststo: qui reghdfe `depvar' `indepvar' `ctrls', ///
		a(user_id) vce(r)
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe ""
		estadd local st_y_fe ""
		estadd local st_m_fe ""
		estadd local year_fe ""
	eststo: qui reghdfe `depvar' `indepvar' `ctrls', ///
		a(user_id c_code_2011_num) vce(r)
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_y_fe ""
		estadd local st_m_fe ""
		estadd local year_fe ""
	eststo: qui reghdfe `depvar' `indepvar' `ctrls', ///
		a(user_id c_code_2011_num state_code_2011_num#year) vce(r)
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_y_fe "$\checkmark$"
		estadd local st_m_fe ""
		estadd local year_fe ""
	eststo: qui reghdfe `depvar' `indepvar' `ctrls', ///
		a(user_id c_code_2011_num state_code_2011_num#month year) vce(r)
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_y_fe ""
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
end

// Saturated
capture program drop reg_sat
program define reg_sat

	syntax varlist, weighted(string)
	set more off
	
	** Parse
	local depvar : word 1 of `varlist'
	di "`depvar'"
	local indepvar : word 2 of `varlist'
	di "`indepvar'"
	local ctrls = substr("`varlist'", length("`depvar'") + length("`indepvar'") + 3, length("`varlist'"))
	di "`ctrls'"

	if "`weighted'" == "yes" {
		eststo: qui reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips], ///
			a(user_id c_code_2011_num state_code_2011_num#month year) vce(r)
			estadd local user_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	}
	if "`weighted'" == "no" {
		eststo: qui reghdfe `depvar' `indepvar' `ctrls', ///
			a(user_id c_code_2011_num state_code_2011_num#month year) vce(r)
			estadd local user_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	} 
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
	local ctrls coverage_ihs tree_cover_mean_ihs temperature_mean_ihs precipitation_mean_ihs

	* Outliers
	drop_outliers	
	
	//1. Species Richness on Deforestation
	reg_user s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls'
	esttab using "${TABLE}/tables/s_richness.tex", replace ///
		keep(dist* coverage* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) nocons nomtitles star(* .1 ** .05 *** .01) label ///
		nonotes booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
	
	//2. Species Diversity on Birding Activity
	local ba_ctrls dist_nf_cum_km2 coverage tree_cover_mean temperature_mean precipitation_mean
	la_var_tex dist_f_cum_km2 `ba_ctrls'
	
	* Panel A: Duration
	reg_user duration dist_f_cum_km2 `ba_ctrls' 
	esttab using "${TABLE}/tables/birding_activity.tex", replace f ///
		keep(dist* coverage* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) nocons refcat(dist_f_cum_km2 ///
		"\emph{Panel A: Duration (min)}", nolabel) nomtitles ///
		star(* .1 ** .05 *** .01) label nonotes booktabs se b(%5.3f) ///
		se(%5.3f) width(\hsize)
	eststo clear	
	// Panel B: Distance
	reg_user distance dist_f_cum_km2 `ba_ctrls' 
	esttab using "${TABLE}/tables/birding_activity.tex", append f ///
		keep(dist* coverage* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) nocons refcat(dist_f_cum_km2 ///
		"\emph{Panel B: Distance (km)}", nolabel) nomtitles ///
		star(* .1 ** .05 *** .01) label nonotes booktabs mlabels(none) ///
		nonumbers se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear

	//3. Species Richness on Project-Wise Deforestation
	
	preserve
	la var dist_f_cum_ihs "All Projects"
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
	foreach i in ind irr mine o tran fvr pa hyb lin {
		
		reg_user s_richness_ihs dist_f_`i'_cum_ihs dist_nf_`i'_cum_ihs `ctrls'
		esttab using "${TABLE}/tables/projwise_s_richness.tex", append f ///
			keep(dist_f_`i'_cum_ihs) refcat(dist_f_hyb_cum_ihs ///
			"\emph{Project Shape}", nolabel) nocons nolines nomtitles label ///
			nonotes mlabels(none) nonumbers booktabs star(* .1 ** .05 *** .01) ///
			noobs se b(%5.3f) se(%5.3f) width(\hsize)
		eststo clear
		}
		
	* Non-linear projects
	reg_user s_richness_ihs dist_f_nl_cum_ihs dist_nf_nl_cum_ihs `ctrls'
	esttab using "${TABLE}/tables/projwise_s_richness.tex", append f ///
		keep(dist_f_nl_cum_ihs) stats(user_fe dist_fe st_y_fe ///
		st_m_fe year_fe N, labels(`"User FEs"' `"District FEs"' ///
		`"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
		`"Year FE"' `"N"') fmt(0 0 0 0 0 0 0)) nocons nomtitles ///
		label nolines nonotes booktabs star(* .1 ** .05 *** .01) se b(%5.3f) ///
		se(%5.3f) width(\hsize) prefoot(\hline)
	eststo clear
	restore
	
	// Plot Project Coeffs
	foreach proj in elec irr mine o tran fvr pa lin nl {
		
		eststo `proj': qui reghdfe s_richness_ihs dist_f_`proj'_cum_ihs ///
			dist_nf_`proj'_cum_ihs `u_ctrls', ///
			a(user_id c_code_2011_num state_code_2011_num#month year) vce(r)

		}
	set scheme s2color
	coefplot (elec \ irr \ mine \ o \ tran \ fvr \ pa \ lin \ nl), ///
		keep(dist_f*) xline(0, lcolor(gs10)) msymbol(D) msize(small) ///
		mcolor(gs4) ciopts(color(gs2)) graphregion(color(white)) bgcolor(white)
	graph export "${TABLE}/fig/projwise_coef.png", replace
	eststo clear
}

*===============================================================================
* ROBUSTNESS CHECKS
*===============================================================================
if `robustness' == 1 {

	* Read
	use "${DATA}/dta/fc_ebd_user_main", clear
	drop_outliers

	* Collect controls
	local ctrls coverage_ihs tree_cover_mean_ihs temperature_mean_ihs precipitation_mean_ihs
	local ctrls_a coverage_ihs tree_cover_mean_ihs temperature_mean_ihs precipitation_mean_ihs all_species_prop_ihs n_mon_yr_ihs user_trend
	local slx_b dist_f_cum_ihs_slx_bc dist_nf_cum_ihs_slx_bc tree_cover_mean_ihs_slx_bc 
	la_var_tex dist_f_cum_ihs dist_nf_cum_ihs `ctrls' `slx_b'
	
	//1. Additional Controls
	la var user_trend "Time Trend"
	reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls', weighted(no) // Main
	reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls' all_species_prop_ihs n_mon_yr_ihs, weighted(no) //Veterans
	reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls' all_species_prop_ihs n_mon_yr_ihs user_trend, weighted(no) //Time-trend
	
	esttab using "${TABLE}/tables/rchecks_controls.tex", replace ///
		drop(temp* precip* _cons) stats(user_fe dist_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 3)) mgroups("Main" "Veteran" "{Time-Trend}", ///
		pattern(1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span ///
		erepeat(\cmidrule(lr){@span})) nomtitles star(* .1 ** .05 *** .01) ///
		label nonotes booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
	
	//2. SLX Estimates
	* Main
	reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls_a', weighted(no)
	* BC
	reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls_a' `slx_b', weighted(no)
	* 1/D
	preserve
	drop *_slx_bc
	ren *_slx_i *_slx_bc
	reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls_a' `slx_b', weighted(no)
	* (1/D)^2
	drop *_slx_bc
	ren *_slx_i2 *_slx_bc
	reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `ctrls_a' `slx_b', weighted(no)
	
	esttab using "${TABLE}/tables/rchecks_slx.tex", replace ///
		drop(temp* precip* _cons) stats(user_fe dist_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 3)) mgroups("Main" "W=Queen" "W=$\frac{1}{d}$" ///
		"W=$\frac{1}{d^2}$", pattern(1 1 1 1) prefix(\multicolumn{@span}{c}{) ///
		suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		indicate("Veteran Controls=n_mon_yr_ihs all_species_prop_ihs" ///
		"User Time-Trend=user_trend") nomtitles star(* .1 ** .05 *** .01) ///
		label nonotes booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
	restore
	
	//1. SLX
	* Binary Contiguity
	reg_user s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls' `slx_b'
	esttab using "${TABLE}/tables/s_richness_slx.tex", replace f ///
		keep(dist* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) refcat(dist_f_cum_ihs "\emph{Panel A: Binary Contiguity}", ///
		nolabel) nocons nomtitles star(* .1 ** .05 *** .01) label nonotes ///
		booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
	* Inverse Distance
	reg_user s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls' `slx_i'
	esttab using "${TABLE}/tables/s_richness_slx.tex", append f ///
		keep(dist* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) refcat(dist_f_cum_ihs "\emph{Panel B: Inverse Distance}", ///
		nolabel) nocons nomtitles star(* .1 ** .05 *** .01) label ///
		nonotes nonumbers booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
	
	//2. Coefficient Plot for Project-Wise SLX
	foreach proj in elec ind irr mine o tran fvr pa hyb lin nl {

		eststo `proj': qui reghdfe s_richness_ihs dist_f_`proj'_cum_ihs ///
			dist_nf_`proj'_cum_ihs `ctrls_a', ///
			a(user_id c_code_2011_num state_code_2011_num#month year) vce(r)
		eststo `proj'_slx: qui reghdfe s_richness_ihs dist_f_`proj'_cum_ihs ///
			dist_nf_`proj'_cum_ihs `ctrls_a' dist_f_`proj'_cum_ihs_slx_bc ///
			dist_nf_`proj'_cum_ihs_slx_bc tree_cover_mean_ihs_slx_bc, ///
			a(user_id c_code_2011_num state_code_2011_num#month year) vce(r)

		}
	coefplot (elec \ irr \ mine \ o \ tran \ fvr \ pa \ lin \ nl, ///
			label("No SLX") mcolor(gs4) ciopts(color(gs4))) ///
		(elec_slx irr_slx mine_slx o_slx tran_slx fvr_slx pa_slx lin_slx nl_slx, ///
			label("SLX") mcolor(gs11) ciopts(color(gs11))), ///
		keep(dist_f*ihs) xline(0, lcolor(gs10)) msymbol(D) msize(small) ///
		graphregion(color(white)) bgcolor(white)
	graph export "${TABLE}/fig/projwise_coef_slx.png", replace
	eststo clear
	
	
	//3. Control for Veteran
	reg_user s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs all_species_prop n_mon_yr `u_ctrls' 
	esttab using "${TABLE}/tables/s_richness_vet.tex", replace ///
		drop(temp* precip*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) nomtitles star(* .1 ** .05 *** .01) label ///
		nocons nonotes booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
	
	//4. Individual Time Trend
	la_var_tex user_trend
	reg_user s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs user_trend `u_ctrls' 
	esttab using "${TABLE}/tables/s_richness_utrend.tex", replace ///
		keep(dist* coverage* tree* user_trend) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) nocons nomtitles star(* .1 ** .05 *** .01) label ///
		nonotes booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
	
	//5. Phase I proposals
	
	* Read
	use "${DATA}/dta/fc_ebd_user_robust", clear
	drop_outliers
	la_var_tex dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls' `slx_b' `slx_i'
	
	* i). Species Richness
	reg_user s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls'
	esttab using "${TABLE}/tables/s_richness_phase1.tex", replace ///
		keep(dist* coverage* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) nocons nomtitles star(* .1 ** .05 *** .01) label ///
		nonotes booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
	
	* ii) SLX
	* Binary Contiguity
	reg_user s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls' `slx_b'
	esttab using "${TABLE}/tables/s_richness_slx_phase1.tex", replace f ///
		keep(dist* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) refcat(dist_f_cum_ihs "\emph{Panel A: Binary Contiguity}", ///
		nolabel) nocons nomtitles star(* .1 ** .05 *** .01) label nonotes ///
		booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
	* Inverse Distance
	reg_user s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls' `slx_i'
	esttab using "${TABLE}/tables/s_richness_slx_phase1.tex", append f ///
		keep(dist* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) refcat(dist_f_cum_ihs "\emph{Panel B: Inverse Distance}", ///
		nolabel) nocons nomtitles star(* .1 ** .05 *** .01) label ///
		nonotes nonumbers booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear

	//6. All Checks
	foreach df in main robust {
	
		* Main
		use "${DATA}/dta/fc_ebd_user_`df'", clear
		drop_outliers
		reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls'
	
		* Veteran
		reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls' n_mon_yr all_species_prop
	
		* Individual Time Trend
		reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls' n_mon_yr all_species_prop user_trend
	
		* Spatial Lag
		reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls' n_mon_yr all_species_prop user_trend `slx_b'
		preserve
		drop *_slx_bc
		ren *_slx_i *_slx_bc
		reg_sat s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls' n_mon_yr all_species_prop user_trend `slx_b'
		
		esttab using "${TABLE}/tables/s_richness_rchecks_`df'.tex", replace ///
			drop(temp* precip* _cons) stats(user_fe dist_fe st_y_fe st_m_fe ///
			year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
			`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
			fmt(0 0 0 0 0 0 3)) mgroups("Main" "Veteran" "{Time-Trend}" ///
			"\shortstack{SLX \\ (B.C.)}"  "\shortstack{SLX \\ (Inv. Dist.)}", ///
			pattern(1 1 1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span ///
			erepeat(\cmidrule(lr){@span})) indicate("Veteran Controls=n_mon_yr all_species_prop" ///
			"User Time-Trend=user_trend") nomtitles ///
			star(* .1 ** .05 *** .01) label nonotes booktabs se b(%5.3f) se(%5.3f) width(\hsize)
		eststo clear
		restore
	}
	
	
}

*===============================================================================
* APPENDIX
*===============================================================================
if `appendix' == 1 {	

	* Read
	use "${DATA}/dta/fc_ebd_user_main.dta", clear
	local u_ctrls coverage_ihs tree_cover_mean_ihs temperature_mean_ihs precipitation_mean_ihs
	la_var_tex dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls'
	
	//1. Alternative Diversity Measures
	
	* Panel A: Shannon Index
	reg_user sh_index_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls'
	esttab using "${TABLE}/tables/alt_diversity.tex", replace f ///
		keep(dist* coverage* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) refcat(dist_f_cum_ihs ///
		"\emph{Panel A: Shannon Index}", nolabel) nocons nomtitles ///
		star(* .1 ** .05 *** .01) label nonotes booktabs se b(%5.3f) ///
		se(%5.3f) width(\hsize)
	eststo clear
	* Panel B: Simpson Index
	reg_user si_index_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls'
	esttab using "${TABLE}/tables/alt_diversity.tex", append f ///
		keep(dist* coverage* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) nocons refcat(dist_f_cum_ihs ///
		"\emph{Panel B: Simpson Index}", nolabel) nomtitles ///
		star(* .1 ** .05 *** .01) label nonotes booktabs mlabels(none) ///
		nonumbers se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear

	//2. Project-Wise Alternative Diversity Measures
	preserve
	la var dist_f_cum_ihs "All Projects"
	unab projvars: dist_f_*_cum_ihs
	la_var_tex `projvars'

	foreach y in sh_index si_index {
		
		* Electricity
		reg_user `y'_ihs dist_f_elec_cum_ihs dist_nf_elec_cum_ihs `u_ctrls'
		esttab using "${TABLE}/tables/projwise_`y'.tex", replace f ///
			keep(dist_f_elec_cum_ihs) refcat(dist_f_elec_cum_ihs ///
			"\emph{Project Category}", nolabel) nocons nomtitles ///
			label noobs nonotes booktabs star(* .1 ** .05 *** .01) se b(%5.3f) ///
			se(%5.3f) width(\hsize)
		eststo clear
		
		* Other Projects
		foreach i in ind irr mine o tran fvr pa hyb lin {
			
			reg_user `y'_ihs dist_f_`i'_cum_ihs dist_nf_`i'_cum_ihs `u_ctrls'
			esttab using "${TABLE}/tables/projwise_`y'.tex", append f ///
				keep(dist_f_`i'_cum_ihs) refcat(dist_f_hyb_cum_ihs ///
				"\emph{Project Shape}", nolabel) nocons nolines nomtitles label ///
				nonotes mlabels(none) nonumbers booktabs star(* .1 ** .05 *** .01) ///
				noobs se b(%5.3f) se(%5.3f) width(\hsize)
			eststo clear
			}
		
		* Non-linear projects
		reg_user `y'_ihs dist_f_nl_cum_ihs dist_nf_nl_cum_ihs `u_ctrls'
		esttab using "${TABLE}/tables/projwise_`y'.tex", append f ///
			keep(dist_f_nl_cum_ihs) stats(user_fe dist_fe st_y_fe st_m_fe ///
			year_fe N, labels(`"User FEs"' `"District FEs"' ///
			`"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
			`"Year FE"' `"N"') fmt(0 0 0 0 0 0)) nocons ///
			nomtitles label nolines nonotes booktabs nonumbers prefoot(\hline) ///
			star(* .1 ** .05 *** .01) se b(%5.3f) se(%5.3f) width(\hsize)
		eststo clear
		
	}
	restore
	
	// Project-Wise Coefficient Plot
	foreach y in s_richness sh_index si_index {
		foreach x in elec irr mine o tran fvr pa lin nl {
	
			eststo `y'_`x': qui reghdfe s_richness_ihs dist_f_`x'_cum_ihs ///
				dist_nf_`x'_cum_ihs `u_ctrls', ///
				a(user_id c_code_2011_num state_code_2011_num#month year) vce(r)
		}
	}
	set scheme s1mono
	coefplot (s_richness_elec \ s_richness_irr \ s_richness_mine \ s_richness_o \ s_richness_tran \ s_richness_fvr \ s_richness_pa \ s_richness_lin \ s_richness_nl), ///
				keep(dist_f*) bylabel("Species Richness") || ///
			(sh_index_elec \ sh_index_irr \ sh_index_mine \ sh_index_o \ sh_index_tran \ sh_index_fvr \ sh_index_pa \ sh_index_lin \ sh_index_nl), ///
				keep(dist_f*) bylabel("Shannon Index") || ///
			(si_index_elec \ si_index_irr \ si_index_mine \ si_index_o \ si_index_tran \ si_index_fvr \ si_index_pa \ si_index_lin \ si_index_nl), ///
				keep(dist_f*) bylabel("Simpson Index") || ///
		, xline(0, lcolor(gs10)) msymbol(D) msize(small) ///
		mcolor(gs4) ciopts(color(gs2)) graphregion(color(white)) bgcolor(white)
	graph export "${TABLE}/fig/projwise_coef_alt.png", replace
	eststo clear
	
	//4. Species Richness on Lagged Deforestation
	
	* No Lag
	la var dist_f_cum_ihs "No lag"	
	foreach var in s_richness sh_index si_index {
		
		reg_user `var'_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls'
	
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
			reg_user `var'_ihs dist_f_cum_ihs_l`i' dist_nf_cum_ihs_l`i' `u_ctrls'
				
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



