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
local main_analysis		1
local robustness		0
local appendix			0

*===============================================================================
* SUMMARY STATISTICS
*===============================================================================
if `sumstats' == 1 {
	
	* Read
	use "${DATA}/dta/fc_ebd_dist.dta", clear
	
	* Tag deforestation districts
	bys c_code_2011: egen any_def = max(dist_f > 0 & dist_f!=.)
	egen tag_d = tag(c_code_2011)
	
	* Indent for pretty latex formatting
	la var dist_f_cum_km2 "All Projects"
	foreach v of varlist dist_f_*_cum_km2 {
		local vlab : var lab `v'
		local vlab = subinstr("`vlab'", "Deforestation (", "",.)
		local vlab = subinstr("`vlab'", ")", "",.)
		la var `v' "`vlab'"
	}
	foreach v of varlist dist_f_cum dist_f_*_cum_km2 tot_area ///
		s_* sh_* si_* pop_density coverage* n_birders n_trips { 
	
			label variable `v' `"\hspace{0.2cm} `: variable label `v''"'	
		}
	
	tempfile temp
	save "`temp'"
	
	** Collect
	local dist_vars tot_area pop_density coverage_all
	local trip_details n_birders n_trips
	local biodiversity s_richness_mean s_richness_d sh_index_mean sh_index_d si_index_mean si_index_d
	
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
		
	** Trip Details
	eststo clear
	*eststo A: estpost tabstat `trip_details', s(n mean sd) c(s)
	eststo B: estpost tabstat `trip_details' if any_def, s(n mean med sd) c(s)
	eststo C: estpost tabstat `trip_details' if !any_def, s(n mean med sd) c(s)
	
	esttab B C using "${TABLE}/tables/sumstats_biodiv.tex", append f main(mean) aux(sd) ///
		cells("mean(star fmt(2)) p50(star fmt(2)) count(fmt(0))" "sd(par fmt(2))") ///
		width(\hsize) refcat(n_birders "\emph{Trip Details (Monthly)}" , nolabel) ///
		nomtitle collabel(none) booktabs noobs label unstack nonumber plain gap
	
	** Biodiversity
	eststo clear
	*eststo A: estpost tabstat `biodiversity', s(n mean sd) c(s)
	eststo B: estpost tabstat `biodiversity' if any_def, s(n mean med sd) c(s)
	eststo C: estpost tabstat `biodiversity' if !any_def, s(n mean med sd) c(s)
	
	esttab B C using "${TABLE}/tables/sumstats_biodiv.tex", append f main(mean) aux(sd) ///
		cells("mean(star fmt(2)) p50(star fmt(2)) count(fmt(0))" "sd(par fmt(2))") ///
		width(\hsize) refcat(s_richness_mean "\emph{Biodiversity}" , nolabel) ///
		nomtitle collabel(none) booktabs noobs label unstack nonumber plain gap
	
	** Trip-level Data
	use "${DATA}/dta/fc_ebd_trip", clear
	bys c_code_2011: egen any_def = max(dist_f > 0 & dist_f!=.)
	foreach v of varlist duration distance n_*_user {
		label variable `v' `"\hspace{0.2cm} `: variable label `v''"'
		}
		
	eststo clear 
	*eststo A: estpost tabstat n_*_user duration distance, s(n mean sd) c(s)
	eststo B: estpost tabstat n_*_user duration distance if any_def, s(n mean med sd) c(s)
	eststo C: estpost tabstat n_*_user duration distance if !any_def, s(n mean med sd) c(s)
	
	esttab B C using "${TABLE}/tables/sumstats_biodiv.tex", append f main(mean) aux(sd) ///
		cells("mean(star fmt(2)) p50(star fmt(2)) count(fmt(0))" "sd(par fmt(2))") ///
		width(\hsize) refcat(n_states_user "\emph{Trip-level Data}" , nolabel) ///
		nomtitle collabel(none) booktabs noobs label unstack nonumber plain gap ///
		addnotes("Note: Spatial coverage is the fraction of grid cells in a district" ///
		"containing at least one bird observation over the study period using an" ///
		"8km $\times$ 8km grid")	
	
	** Deforestation
	use "`temp'", clear
	eststo clear
	eststo: estpost tabstat dist_f*km2 if any_def, s(n mean sd p50 min max) c(s)
	
	local numbers "& (1) & (2) & (3) & (4) & (5) & (6) \\ \midrule"
	local titles "& {N} & {Mean} & {SD} & {p50} & {Min} & {Max} \\"
	esttab using "${TABLE}/tables/sumstats_deforest.tex", replace ///	
		cell((count mean(fmt(%9.2f)) sd(fmt(%9.2f)) p50(fmt(%9.0f)) min(fmt(%9.0f)) ///
		max(fmt(%9.0f)))) width(\hsize) ///
		refcat(dist_f_elec_cum_km2 "\emph{By Project Type}" ///
		dist_f_hyb_cum_km2 "\emph{By Project Shape}", nolabel) ///
		posthead("`titles'" "`numbers'") nonumbers nomtitles collabels(none) ///
		mlabels(none) booktabs noobs gap label substitute("$" "\$")
	eststo clear	
}

*===============================================================================
* MAIN ANALYSIS
*===============================================================================

** REGRESSION PROGRAMS **

*--------------------------------------------------------
* TRIP LEVEL
*--------------------------------------------------------
// With User FEs
capture program drop reg_trip
program define reg_trip

	syntax varlist
	set more off
	
	** Parse
	local depvar : word 1 of `varlist'
	di "`depvar'"
	local indepvar : word 2 of `varlist'
	di "`indepvar'"
	local ctrls = substr("`varlist'", length("`depvar'") + length("`indepvar'") + 3, length("`varlist'"))
	di "`ctrls'"

	eststo: qui reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips_dym], ///
		a(user_id) vce(r)
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe ""
		estadd local st_y_fe ""
		estadd local st_m_fe ""
		estadd local year_fe ""
	eststo: qui reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips_dym], ///
		a(user_id c_code_2011_num) vce(r)
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_y_fe ""
		estadd local st_m_fe ""
		estadd local year_fe ""
	eststo: qui reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips_dym], ///
		a(user_id c_code_2011_num state_code_2011_num#year) vce(r)
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_y_fe "$\checkmark$"
		estadd local st_m_fe ""
		estadd local year_fe ""
	eststo: qui reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips_dym], ///
		a(user_id c_code_2011_num state_code_2011_num#month year) vce(r)
		estadd local user_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_y_fe ""
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
end

*--------------------------------------------------------
* DISTRICT-MONTHLY (ROBUSTNESS)
*--------------------------------------------------------
//OLS and WLS with FE
capture program drop reg_dm
program define reg_dm

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
		a(c_code_2011_num) vce(cl state_code_2011_num#year)
		estadd local dist_fe "$\checkmark$"
		estadd local st_y_fe ""
		estadd local st_m_fe ""
		estadd local year_fe ""
	eststo: qui reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips], ///
		a(c_code_2011_num) vce(cl state_code_2011_num#year)
		estadd local dist_fe "$\checkmark$"
		estadd local st_y_fe ""
		estadd local st_m_fe ""
		estadd local year_fe ""
	eststo: qui reghdfe `depvar' `indepvar' `ctrls', ///
		a(c_code_2011_num state_code_2011_num#year) vce(cl state_code_2011_num#year)
		estadd local dist_fe "$\checkmark$"
		estadd local st_y_fe "$\checkmark$"
		estadd local st_m_fe ""
		estadd local year_fe ""
	eststo: qui reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips], ///
		a(c_code_2011_num state_code_2011_num#year) vce(cl state_code_2011_num#year)
		estadd local dist_fe "$\checkmark$"
		estadd local st_y_fe "$\checkmark$"
		estadd local st_m_fe ""
		estadd local year_fe ""
	eststo: qui reghdfe `depvar' `indepvar' `ctrls', ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl state_code_2011_num#year)
		estadd local dist_fe "$\checkmark$"
		estadd local st_y_fe ""
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
	eststo: qui reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips], ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl state_code_2011_num#year)
		estadd local dist_fe "$\checkmark$"
		estadd local st_y_fe ""
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
end

// WLS vs. Dropping N<10
capture program drop reg_wls_drop
program define reg_wls_drop
	
	syntax varlist
	set more off
	
	** Parse
	local depvar : word 1 of `varlist'
	di "`depvar'"
	local indepvar : word 2 of `varlist'
	di "`indepvar'"
	local ctrls = substr("`varlist'", length("`depvar'") + length("`indepvar'") + 3, length("`varlist'"))
	di "`ctrls'"
	
	* WLS
	eststo: qui reghdfe `depvar' `indepvar' `ctrls' ///
		[aweight = n_trips], a(c_code_2011_num state_code_2011_num#month year) ///
		vce(cl state_code_2011_num#year)
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
		
	* OLS (Drop)
	eststo: qui reghdfe `depvar' `indepvar' `ctrls' if n_trips>10, ///
		a(c_code_2011_num state_code_2011_num#month year) ///
		vce(cl state_code_2011_num#year)
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
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
	use "${DATA}/dta/fc_ebd_trip.dta", clear
	local u_ctrls coverage_ihs tree_cover_mean_ihs temperature_mean_ihs precipitation_mean_ihs
	la_var_tex dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls'

	//1. Species Richness on Deforestation

	reg_trip s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls'
	esttab using "${TABLE}/tables/s_richness_trip.tex", replace ///
		keep(dist* coverage* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) nocons nomtitles star(* .1 ** .05 *** .01) label ///
		nonotes booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear

	//2. SLX
	
	local slx_b dist_f_cum_ihs_slx_bc dist_nf_cum_ihs_slx_bc tree_cover_mean_ihs_slx_bc 
	local slx_i dist_f_cum_ihs_slx_i dist_nf_cum_ihs_slx_i tree_cover_mean_ihs_slx_i
	la_var_tex `slx_b' `slx_i' 
	
	* Binary Contiguity
	reg_trip s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls' `slx_b'
	esttab using "${TABLE}/tables/s_richness_trip_slx.tex", replace f ///
		keep(dist* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) refcat(dist_f_cum_ihs "\emph{Panel A: Binary Contiguity}", ///
		nolabel) nocons nomtitles star(* .1 ** .05 *** .01) label nonotes ///
		booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
	* Inverse Distance
	reg_trip s_richness_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls' `slx_i'
	esttab using "${TABLE}/tables/s_richness_trip_slx.tex", append f ///
		keep(dist* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) refcat(dist_f_cum_ihs "\emph{Panel B: Inverse Distance}", ///
		nolabel) nocons nomtitles star(* .1 ** .05 *** .01) label ///
		nonotes nonumbers booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear

	//3. Species Diversity on Birding Activity
	local u_ba_ctrls dist_nf_cum_km2 coverage tree_cover_mean temperature_mean precipitation_mean
	la_var_tex dist_f_cum_km2 `u_ba_ctrls'
	
	* Panel A: Duration
	reg_trip duration dist_f_cum_km2 `u_ba_ctrls'
	esttab using "${TABLE}/tables/birding_activity_trip.tex", replace f ///
		keep(dist* coverage* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) nocons refcat(dist_f_cum_km2 ///
		"\emph{Panel A: Duration (min)}", nolabel) nomtitles ///
		star(* .1 ** .05 *** .01) label nonotes booktabs se b(%5.3f) ///
		se(%5.3f) width(\hsize)
	eststo clear
		
	// Panel B: Distance
	reg_trip distance dist_f_cum_km2 `u_ba_ctrls'
	esttab using "${TABLE}/tables/birding_activity_trip.tex", append f ///
		keep(dist* coverage* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) nocons refcat(dist_f_cum_km2 ///
		"\emph{Panel B: Distance (km)}", nolabel) nomtitles ///
		star(* .1 ** .05 *** .01) label nonotes booktabs mlabels(none) ///
		nonumbers se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear

	//2. Species Richness on Project-Wise Deforestation
	
	la var dist_f_cum_ihs "All Projects"
	foreach v of varlist dist_f_*_cum_ihs {
		local vlab : var lab `v'
		local vlab = subinstr("`vlab'", "Deforestation (", "",.)
		local vlab = subinstr("`vlab'", ")", "",.)
		la var `v' "`vlab'"
	}
	unab projvars: dist_f_*_cum_ihs
	la_var_tex `projvars'
		
	* Electricity
	*reg_trip s_richness_ihs dist_*_elec_cum_ihs dist_*_elec_cum_ihs_slx_bc tree_cover_mean_ihs_slx_bc `u_ctrls'
	reg_trip s_richness_ihs dist_f_elec_cum_ihs dist_nf_elec_cum_ihs `u_ctrls'
	esttab using "${TABLE}/tables/projwise_s_richness_trip.tex", f replace ///
		keep(dist_f_elec_cum_ihs) refcat(dist_f_elec_cum_ihs ///
		"\emph{Project Category}", nolabel) nocons nomtitles label noobs nonotes ///
		booktabs star(* .1 ** .05 *** .01) se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
		
	* Other Projects
	foreach i in ind irr mine o tran fvr pa hyb lin {
		
		*reg_trip s_richness_ihs dist_*_`i'_cum_ihs dist_*_`i'_cum_ihs_slx_bc tree_cover_mean_ihs_slx_bc `u_ctrls'
		reg_trip s_richness_ihs dist_f_`i'_cum_ihs dist_nf_`i'_cum_ihs `u_ctrls'
		esttab using "${TABLE}/tables/projwise_s_richness_trip.tex", append f ///
			keep(dist_f_`i'_cum_ihs) refcat(dist_f_hyb_cum_ihs ///
			"\emph{Project Shape}", nolabel) nocons nolines nomtitles label ///
			nonotes mlabels(none) nonumbers booktabs star(* .1 ** .05 *** .01) ///
			noobs se b(%5.3f) se(%5.3f) width(\hsize)
		eststo clear
		}
		
	* Non-linear projects
	
	*reg_trip s_richness_ihs dist_*_nl_cum_ihs dist_*_nl_cum_ihs_slx_bc tree_cover_mean_ihs_slx_bc `u_ctrls'
	reg_trip s_richness_ihs dist_f_nl_cum_ihs dist_nf_nl_cum_ihs `u_ctrls'
	esttab using "${TABLE}/tables/projwise_s_richness_trip.tex", append f ///
		keep(dist_f_nl_cum_ihs) stats(user_fe dist_fe st_y_fe ///
		st_m_fe year_fe N r2, labels(`"User FEs"' `"District FEs"' ///
		`"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
		`"Year FE"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 0 3)) nocons nomtitles ///
		label nolines nonotes booktabs star(* .1 ** .05 *** .01) se b(%5.3f) ///
		se(%5.3f) width(\hsize) prefoot(\hline)
	eststo clear
	
}

*===============================================================================
* ROBUSTNESS CHECKS
*===============================================================================
if `robustness' == 1 {

	* Read
	use "${DATA}/dta/fc_ebd_dist.dta", clear
	local controls coverage_ihs tree_cover_mean_ihs temperature_mean_ihs precipitation_mean_ihs
	la_var_tex dist_f_cum_ihs dist_nf_cum_ihs `controls'
	
	//1. Species Richness on Deforestation
	reg_dm s_richness_mean_ihs dist_f_cum_ihs dist_nf_cum_ihs `controls'
	esttab using "${TABLE}/tables/s_richness_dist.tex", replace ///
		keep(dist* coverage* tree*) stats(dist_fe st_y_fe smonth_fe year_fe N r2, ///
		labels(`"District FEs"' `"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
		`"Year FE"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 3)) nocons nomtitles ///
		mlabels("OLS" "WLS" "OLS" "WLS" "OLS" "WLS") star(* .1 ** .05 *** .01) ///
		label nonotes booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
	
	//2. SLX

	local slx_b dist_f_cum_ihs_slx_bc dist_nf_cum_ihs_slx_bc tree_cover_mean_ihs_slx_bc 
	local slx_i dist_f_cum_ihs_slx_i dist_nf_cum_ihs_slx_i tree_cover_mean_ihs_slx_i
	la_var_tex `slx_b' `slx_i'
	
	* Binary Contiguity
	reg_dm s_richness_mean_ihs dist_f_cum_ihs dist_nf_cum_ihs `controls' `slx_b'
	esttab using "${TABLE}/tables/s_richness_dist_slx.tex", replace f ///
		keep(dist* tree*) stats(dist_fe st_y_fe st_m_fe year_fe N r2, ///
		labels(`"District FEs"' `"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
		`"Year FE"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 3)) nocons nomtitles ///
		mlabels("OLS" "WLS" "OLS" "WLS" "OLS" "WLS") nonotes nonumbers label ///
		booktabs nocons refcat(district_forest_cum_ihs ///
		"\emph{Panel A: Binary Contiguity}", nolabel) star(* .1 ** .05 *** .01) ///
		se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
	* Inverse Distance
	reg_dm s_richness_mean_ihs dist_f_cum_ihs dist_nf_cum_ihs `controls' `slx_i'
	esttab using "${TABLE}/tables/s_richness_dist_slx.tex", append f ///
		keep(dist* tree*) stats(dist_fe st_y_fe st_m_fe year_fe N r2, ///
		labels(`"District FEs"' `"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
		`"Year FE"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 3)) nocons nomtitles ///
		nonotes nonumbers label booktabs nocons ///
		refcat(district_forest_cum_ihs "\emph{Panel B: Inverse Distance}", ///
		nolabel) star(* .1 ** .05 *** .01) se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
	
	//3. Birding Activity on Deforestation
	local ba_controls dist_nf_cum_km2 tree_cover_mean coverage temperature_mean precipitation_mean
	la_var_tex dist_f_cum_km2 `ba_controls'

	** Panel A: Duration
	eststo clear
	reg_dm duration_mean dist_f_cum_km2 `ba_controls'
	esttab using "${TABLE}/tables/birding_activity_dist.tex", replace f ///
		keep(dist* coverage* tree*) stats(dist_fe st_y_fe smonth_fe year_fe N r2, ///
		labels(`"District FEs"' `"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
		`"Year FE"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 3)) nocons ///
		mlabels("OLS" "WLS" "OLS" "WLS" "OLS" "WLS") refcat(dist_f_cum_km2 ///
		"\emph{Panel A: Duration (min)}", nolabel) nomtitles ///
		star(* .1 ** .05 *** .01) label nonotes booktabs se b(%5.3f) ///
		se(%5.3f) width(\hsize)
	eststo clear
	
	* Panel B: Distance
	reg_dm distance_mean dist_f_cum_km2 `ba_controls'
	esttab using "${TABLE}/tables/birding_activity_dist.tex", append f ///
		keep(dist* coverage* tree*) nocons mlabels(none) nomtitles ///
		nonumbers stats(dist_fe st_y_fe smonth_fe year_fe N r2, ///
		labels(`"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 3)) refcat(dist_f_cum_km2 "\emph{Panel B: Distance (km)}", ///
		nolabel) star(* .1 ** .05 *** .01) label nonotes booktabs se b(%5.3f) ///
		se(%5.3f) width(\hsize)
	eststo clear
	
	//4. Species Richness on Deforestation by Project Type
	
	* Simplify labels
	la var dist_f_cum_ihs "All Projects"
	foreach v of varlist dist_f_*_cum_ihs {
		local vlab : var lab `v'
		local vlab = subinstr("`vlab'", "Deforestation (", "",.)
		local vlab = subinstr("`vlab'", ")", "",.)
		la var `v' "`vlab'"
	}
	unab projvars: dist_f_*_cum_ihs
	la_var_tex `projvars'
	
	* Electricity
	reg_dm s_richness_mean_ihs dist_f_elec_cum_ihs dist_nf_elec_cum_ihs `controls'
	esttab using "${TABLE}/tables/projwise_s_richness_dist.tex", replace ///
		f keep(dist_f_elec_cum_ihs) mlabels("OLS" "WLS" "OLS" "WLS" "OLS" "WLS") ///
		refcat(dist_f_elec_cum_ihs "\emph{Project Category}", nolabel) ///
		label noobs booktabs nonotes nocons nomtitles b(%5.3f) se ///
		star(* .1 ** .05 *** .01) se(%5.3f) 
	eststo clear

	* Other Projects
	foreach i in ind irr mine o tran fvr pa hyb lin {
		reg_dm s_richness_mean_ihs dist_f_`i'_cum_ihs dist_nf_`i'_cum_ihs `controls'	
		esttab using "${TABLE}/tables/projwise_s_richness_dist.tex", ///
			append f keep(dist_f_`i'_cum_ihs) nolines noobs ///
			label booktabs nomtitles nonotes nocons mlabels(none) ///
			nonumbers refcat(dist_f_hyb_cum_ihs "\emph{Project Shape}", ///
			nolabel)se b(%5.3f) se(%5.3f) star(* .1 ** .05 *** .01)
		eststo clear
		}
	
	* Non-Linear
	reg_dm s_richness_mean_ihs dist_f_nl_cum_ihs dist_nf_nl_cum_ihs `controls'	
	esttab using "${TABLE}/tables/projwise_s_richness_dist.tex", ///
		append f keep(dist_f_nl_cum_ihs) stats(dist_fe st_y_fe ///
		smonth_fe year_fe N r2, labels(`"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 3)) label booktabs nonotes nocons nomtitles nonumbers ///
		nolines mlabels(none) b(%5.3f) se star(* .1 ** .05 *** .01) ///
		se(%5.3f) prefoot(\hline)
	eststo clear	
	
}

*===============================================================================
* APPENDIX
*===============================================================================
if `appendix' == 1 {

	*----------------------------------------------
	* TRIP-LEVEL
	*----------------------------------------------	

	* Read
	use "${DATA}/dta/fc_ebd_trip.dta", clear
	local u_ctrls coverage_ihs tree_cover_mean_ihs temperature_mean_ihs precipitation_mean_ihs
	la_var_tex dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls'
	
	//1. Alternative Diversity Measures
	
	* Panel A: Shannon Index
	reg_trip sh_index_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls'
	esttab using "${TABLE}/tables/alt_diversity_trip.tex", replace f ///
		keep(dist* coverage* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) refcat(dist_f_cum_ihs ///
		"\emph{Panel A: Shannon Index}", nolabel) nocons nomtitles ///
		star(* .1 ** .05 *** .01) label nonotes booktabs se b(%5.3f) ///
		se(%5.3f) width(\hsize)
	eststo clear
	
	* Panel B: Simpson Index
	reg_trip si_index_ihs dist_f_cum_ihs dist_nf_cum_ihs `u_ctrls'
	esttab using "${TABLE}/tables/alt_diversity_trip.tex", append f ///
		keep(dist* coverage* tree*) stats(user_fe dist_fe st_y_fe st_m_fe ///
		year_fe N r2, labels(`"User FEs"' `"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 3)) nocons refcat(dist_f_cum_ihs ///
		"\emph{Panel B: Simpson Index}", nolabel) nomtitles ///
		star(* .1 ** .05 *** .01) label nonotes booktabs mlabels(none) ///
		nonumbers se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear

	//2. Project-Wise Alternative Diversity Measures
	
	la var dist_f_cum_ihs "All Projects"
	foreach v of varlist dist_f_*_cum_ihs {
		local vlab : var lab `v'
		local vlab = subinstr("`vlab'", "Deforestation (", "",.)
		local vlab = subinstr("`vlab'", ")", "",.)
		la var `v' "`vlab'"
	}
	unab projvars: dist_f_*_cum_ihs
	la_var_tex `projvars'

	foreach y in sh_index si_index {
		
		* Electricity
		reg_trip `y'_ihs dist_f_elec_cum_ihs dist_nf_elec_cum_ihs `u_ctrls'
		esttab using "${TABLE}/tables/projwise_`y'_trip.tex", replace f ///
			keep(dist_f_elec_cum_ihs) refcat(dist_f_elec_cum_ihs ///
			"\emph{Project Category}", nolabel) nocons nomtitles ///
			label noobs nonotes booktabs star(* .1 ** .05 *** .01) se b(%5.3f) ///
			se(%5.3f) width(\hsize)
		eststo clear
		
		* Other Projects
		foreach i in ind irr mine o tran fvr pa hyb lin {
			
			reg_trip `y'_ihs dist_f_`i'_cum_ihs dist_nf_`i'_cum_ihs `u_ctrls'
			esttab using "${TABLE}/tables/projwise_`y'_trip.tex", append f ///
				keep(dist_f_`i'_cum_ihs) refcat(dist_f_hyb_cum_ihs ///
				"\emph{Project Shape}", nolabel) nocons nolines nomtitles label ///
				nonotes mlabels(none) nonumbers booktabs star(* .1 ** .05 *** .01) ///
				noobs se b(%5.3f) se(%5.3f) width(\hsize)
			eststo clear
			}
		
		* Non-linear projects
		reg_trip `y'_ihs dist_f_nl_cum_ihs dist_nf_nl_cum_ihs `u_ctrls'
		esttab using "${TABLE}/tables/projwise_`y'_trip.tex", append f ///
			keep(dist_f_nl_cum_ihs) stats(user_fe dist_fe st_y_fe st_m_fe ///
			year_fe N r2, labels(`"User FEs"' `"District FEs"' ///
			`"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
			`"Year FE"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 0 3)) nocons ///
			nomtitles label nolines nonotes booktabs star(* .1 ** .05 *** .01) ///
			se b(%5.3f) se(%5.3f) width(\hsize) prefoot(\hline)
		eststo clear
		
	}

	*----------------------------------------------
	* DISTRICT-MONTHLY
	*----------------------------------------------	
	
	* Read
	use "${DATA}/dta/fc_ebd_dist.dta", clear
	local controls coverage_ihs tree_cover_mean_ihs temperature_mean_ihs precipitation_mean_ihs
	la_var_tex dist_f_cum_ihs dist_nf_cum_ihs `controls'
	
	//1. Alternative Species Diversity on Deforestation
	
	foreach j in mean md d {
	
		* Panel A: Species Richness
		reg_dm s_richness_`j'_ihs dist_f_cum_ihs dist_nf_cum_ihs `controls'
		esttab using "${TABLE}/tables/alt_diversity_`j'_dist.tex", replace f ///
			keep(dist* coverage* tree*) stats(dist_fe st_y_fe st_m_fe year_fe N r2, ///
			labels(`"District FEs"' `"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
			`"Year FE"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 3)) ///
			mlabels("OLS" "WLS" "OLS" "WLS" "OLS" "WLS") nonotes nonumbers label ///
			booktabs nomtitles nocons refcat(dist_f_cum_ihs ///
			"\emph{Panel A: Species Richness}", nolabel) star(* .1 ** .05 *** .01) ///
			se b(%5.3f) se(%5.3f) width(\hsize)
		eststo clear
		
		* Panel B: Shannon Index
		reg_dm sh_index_`j'_ihs dist_f_cum_ihs dist_nf_cum_ihs `controls'
		esttab using "${TABLE}/tables/alt_diversity_`j'_dist.tex", append f ///
			keep(dist* coverage* tree*) stats(dist_fe st_y_fe st_m_fe year_fe N r2, ///
			labels(`"District FEs"' `"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
			`"Year FE"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 3)) nomtitles nocons ///
			nonotes nonumbers label booktabs refcat(dist_f_cum_ihs ///
			"\emph{Panel B: Shannon Index}", nolabel) ///
			star(* .1 ** .05 *** .01) se b(%5.3f) se(%5.3f) width(\hsize)
		eststo clear
	
		* Panel C: Simpson Index
		reg_dm si_index_`j'_ihs dist_f_cum_ihs dist_nf_cum_ihs `controls'
		esttab using "${TABLE}/tables/alt_diversity_`j'_dist.tex", append f ///
			keep(dist* coverage* tree*) nocons mlabels(none) nomtitles nonumbers ///
			stats(dist_fe st_y_fe st_m_fe year_fe N r2, labels(`"District FEs"' ///
			`"State $\times$ Year FEs"' `"State $\times$ Month FEs"' `"Year FE"' ///
			`"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 3)) refcat(dist_f_cum_ihs ///
			"\emph{Panel C: Simpson Index}", nolabel) ///
			star(* .1 ** .05 *** .01) label nonotes booktabs se b(%5.3f) ///
			se(%5.3f) width(\hsize)
		eststo clear

	}
	
	//2. Species Diversity on Deforestation (WLS vs Drop n<10)
	
	* Panel A: Mean
	foreach var in s_richness_mean sh_index_mean si_index_mean {	
		reg_wls_drop `var'_ihs dist_f_cum_ihs dist_nf_cum_ihs `controls'
	}
	esttab using "${TABLE}/tables/diversity_drop10.tex", replace f ///
		keep(dist* coverage* tree*) nocons stats(dist_fe st_m_fe year_fe N r2, ///
		labels(`"District FEs"' `"State $\times$ Month FEs"' `"Year FE"' `"N"' ///
		`"\(R^{2}\)"') fmt(0 0 0 0 3)) mgroups("Species Richness" ///
		"Shannon Index" "Simpson Index", pattern(1 0 1 0 1 0) ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
		mlabels("WLS" "Drop" "WLS" "Drop" "WLS" "Drop") refcat(dist_f_cum_ihs ///
		"\emph{Panel A: Mean Per Trip}", nolabel) nomtitles ///
		star(* .1 ** .05 *** .01) label nonumbers nonotes booktabs se b(%5.3f) ///
		se(%5.3f) width(\hsize)
	eststo clear
	
	* Panel B: Median
	foreach var in s_richness_md sh_index_md si_index_md {	
		reg_wls_drop `var'_ihs dist_f_cum_ihs dist_nf_cum_ihs `controls'
	}
	esttab using "${TABLE}/tables/diversity_drop10.tex", append f ///
		keep(dist* coverage* tree*) nocons stats(dist_fe st_m_fe year_fe N r2, ///
		labels(`"District FEs"' `"State $\times$ Month FEs"' `"Year FE"' `"N"' ///
		`"\(R^{2}\)"') fmt(0 0 0 0 3)) refcat(dist_f_cum_ihs ///
		"\emph{Panel B: Median Per Trip}", nolabel) ///
		star(* .1 ** .05 *** .01) label nonotes mlabels(none) ///
		nomtitles nonumbers booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
	
	* Panel C: All
	foreach var in s_richness_d sh_index_d si_index_d {	
		reg_wls_drop `var'_ihs dist_f_cum_ihs dist_nf_cum_ihs `controls'
	}
	esttab using "${TABLE}/tables/diversity_drop10.tex", append f ///
		keep(dist* coverage* tree*) nocons stats(dist_fe st_m_fe year_fe N r2, ///
		labels(`"District FEs"' `"State $\times$ Month FEs"' `"Year FE"' `"N"' ///
		`"\(R^{2}\)"') fmt(0 0 0 0 3)) refcat(dist_f_cum_ihs ///
		"\emph{Panel C: All Trips}", nolabel) star(* .1 ** .05 *** .01) ///
		label nonotes mlabels(none) nomtitles nonumbers booktabs se ///
		b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear

	//3. Species Diversity on Deforestation by Project Type
	
	la var dist_f_cum_ihs "All Projects"
	foreach v of varlist dist_f_*_cum_ihs {
		local vlab : var lab `v'
		local vlab = subinstr("`vlab'", "Deforestation (", "",.)
		local vlab = subinstr("`vlab'", ")", "",.)
		la var `v' "`vlab'"
	}
	unab projvars: dist_f_*_cum_ihs
	la_var_tex `projvars'
	
	foreach j in mean md d {
	
		* Project Category
		foreach var in s_richness sh_index si_index {

			eststo: qui reghdfe `var'_`j'_ihs dist_f_elec_cum_ihs ///
				dist_nf_elec_cum_ihs `controls', ///
				a(c_code_2011_num state_code_2011_num#month year) ///
				vce(cl state_code_2011_num#year)
			eststo: qui reghdfe `var'_`j'_ihs dist_f_elec_cum_ihs ///
				dist_nf_elec_cum_ihs `controls' [aweight=n_trips], ///
				a(c_code_2011_num state_code_2011_num#month year) ///
				vce(cl state_code_2011_num#year)
			}
			esttab using "${TABLE}/tables/projwise_alt_`j'_dist.tex", replace ///
				f keep(dist_f_elec_cum_ihs) mgroups("Species Richness" ///
				"Shannon Index" "Simpson Index", pattern(1 0 1 0 1 0) ///
				prefix(\multicolumn{@span}{c}{) suffix(}) span ///
				erepeat(\cmidrule(lr){@span})) mlabels("OLS" "WLS" "OLS" "WLS" "OLS" "WLS") ///
				refcat(dist_f_elec_cum_ihs "\emph{Project Category}", nolabel) ///
				label noobs booktabs nonotes nocons nomtitles b(%5.3f) se ///
				star(* .1 ** .05 *** .01) se(%5.3f) 
			eststo clear

		foreach i in ind irr mine o tran fvr pa hyb lin {
		
			foreach var in s_richness sh_index si_index {
			
				eststo: qui reghdfe `var'_`j'_ihs dist_f_`i'_cum_ihs ///
					dist_nf_`i'_cum_ihs `controls', ///
					a(c_code_2011_num state_code_2011_num#month year) ///
					vce(cl state_code_2011_num#year)
				eststo: qui reghdfe `var'_`j'_ihs dist_f_`i'_cum_ihs ///
					dist_nf_`i'_cum_ihs `controls' [aweight=n_trips], ///
					a(c_code_2011_num state_code_2011_num#month year) ///
					vce(cl state_code_2011_num#year)
					}	
			esttab using "${TABLE}/tables/projwise_alt_`j'_dist.tex", ///
				append f keep(dist_f_`i'_cum_ihs) nolines noobs ///
				label booktabs nomtitles nonotes nocons mlabels(none) ///
				nonumbers refcat(district_forest_hyb_cum_ihs "\emph{Project Shape}", ///
				nolabel)se b(%5.3f) se(%5.3f) star(* .1 ** .05 *** .01)
			eststo clear
			}
		
		foreach var in s_richness sh_index si_index {
	
			eststo: qui reghdfe `var'_`j'_ihs dist_f_nl_cum_ihs ///
				dist_nf_nl_cum_ihs `controls', ///
				a(c_code_2011_num state_code_2011_num#month year) ///
				vce(cl state_code_2011_num#year)
			eststo: qui reghdfe `var'_`j'_ihs dist_f_nl_cum_ihs ///
				dist_nf_nl_cum_ihs `controls' [aweight=n_trips], ///
				a(c_code_2011_num state_code_2011_num#month year) ///
				vce(cl state_code_2011_num#year)
			}
			esttab using "${TABLE}/tables/projwise_alt_`j'_dist.tex", ///
				append f keep(dist_f_nl_cum_ihs) label booktabs ///
				nonotes nocons nomtitles nonumbers nolines mlabels(none) ///
				b(%5.3f) se star(* .1 ** .05 *** .01) se(%5.3f) prefoot(\hline)
			eststo clear
	}

	//4. Species Diversity on Lagged Deforestation
	
	la var dist_f_cum_ihs "No lag"
	foreach j in mean d {
	
		if "`j'" == "mean" {
			local weight "[aweight=n_trips]"
		}
		else {
			local weight ""
		}
		
		foreach var in s_richness sh_index si_index {
			
			* Model
			eststo: qui reghdfe `var'_`j'_ihs dist_f_cum_ihs ///
				dist_nf_cum_ihs `controls' `weight', ///
				a(c_code_2011_num state_code_2011_num#month year) ///
				vce(cl state_code_2011_num#year)
			
			* Store Beta, CI
			gen `var'_df_`j'_b_l0 = _b[dist_f_cum_ihs]
			gen `var'_df_`j'_ciu_l0 = _b[dist_f_cum_ihs] - invttail(e(df_r),0.025)*_se[dist_f_cum_ihs]
			gen `var'_df_`j'_cil_l0 = _b[dist_f_cum_ihs] + invttail(e(df_r),0.025)*_se[dist_f_cum_ihs]
			
			}
		}
		esttab using "${TABLE}/tables/bio_def_lag.tex", replace f wrap ///
			keep(dist_f_cum_ihs) stats(N, labels("N") fmt(0)) ///
			mgroups("Per Trip" "All Trips", pattern(1 0 0 1 0 0) ///
			prefix(\multicolumn{@span}{c}{) suffix(}) span ///
			erepeat(\cmidrule(lr){@span})) mlabels("{Sp. Richness}" ///
			"{Sh. Index}" "{Si. Index}" "{Sp. Richness}" ///
			"{Sh. Index}" "{Si. Index}") label booktabs nonotes nocons ///
			nomtitles nolines posthead(\hline) b(%5.3f) se ///
			star(* .1 ** .05 *** .01) se(%5.3f) 
		eststo clear
	
	foreach i of numlist 1/12 {
		
		foreach j in mean d {
			
			if "`j'" == "mean" {
				local weight "[aweight=n_trips]"
			}
			else {
				local weight ""
			}
			
			foreach var in s_richness sh_index si_index {
		
				* Model
				eststo: qui reghdfe `var'_`j'_ihs dist_f_cum_ihs_l`i' ///
					dist_nf_cum_ihs_l`i' `controls' [aweight=n_trips], ///
					a(c_code_2011_num state_code_2011_num#month year) ///
					vce(cl state_code_2011_num#year)
					
				* Store Coeff
				gen `var'_df_`j'_b_l`i' = _b[dist_f_cum_ihs_l`i']
			
				* Store CI
				gen `var'_df_`j'_ciu_l`i' = _b[dist_f_cum_ihs_l`i'] - invttail(e(df_r),0.025)*_se[dist_f_cum_ihs_l`i']
				gen `var'_df_`j'_cil_l`i' = _b[dist_f_cum_ihs_l`i'] + invttail(e(df_r),0.025)*_se[dist_f_cum_ihs_l`i']
	
				}
			}
		
		esttab using "${TABLE}/tables/bio_def_lag.tex", append f wrap ///
			keep(dist_f_cum_ihs_l`i') label booktabs /// 
			stats(N, labels("N") fmt(0))  nolines posthead(\hline) ///
			nonotes nocons mlabels(none) nonumbers se b(%5.3f) ///
			se(%5.3f) star(* .1 ** .05 *** .01)
		eststo clear
	}
	la var dist_f_cum_ihs "Cum. Deforestation"

	** Coefficient Plot
	preserve
		
		* Prep
		keep *_df*l*
		keep if _n == 1
		gen id = _n
		
		reshape long s_richness_df_mean_b_l s_richness_df_d_b_l ///
			s_richness_df_mean_ciu_l s_richness_df_d_ciu_l ///
			s_richness_df_mean_cil_l s_richness_df_d_cil_l ///
			sh_index_df_mean_b_l sh_index_df_d_b_l ///
			sh_index_df_mean_ciu_l sh_index_df_d_ciu_l ///
			sh_index_df_mean_cil_l sh_index_df_d_cil_l ///
			si_index_df_mean_b_l si_index_df_d_b_l ///
			si_index_df_mean_ciu_l si_index_df_d_ciu_l ///
			si_index_df_mean_cil_l si_index_df_d_cil_l, ///
			i(id) j(lag)
			
		ren *_l *
		
		* Plot
		foreach var in s_richness si_index {
			
			foreach j in mean d {
		
				twoway rarea `var'_df_`j'_ciu `var'_df_`j'_cil lag, ///
						sort color(gs14) || ///
					scatter `var'_df_`j'_b lag || ///
					function y = 0, range(lag) color(gs8) ///
						xtitle("Lag (months)") xlabel(0(1)12) ///
						graphregion(color(white)) bgcolor(white) ///
						legend(order(1 "95% CI" 2 "Coefficient"))
			
				graph export "${TABLE}/fig/`var'_`j'_df_lag.png", replace
			
			}
		}
	
	restore
	
	//5. Birding Activity on Deforestation
	local ba_controls dist_nf_cum_km2 tree_cover_mean coverage temperature_mean precipitation_mean
	la_var_tex dist_f_cum_km2 `ba_controls'

	** Panel A: Duration
	eststo clear
	reg_dm duration_md dist_f_cum_km2 `ba_controls'
	esttab using "${TABLE}/tables/birding_activity_md_dist.tex", replace f ///
		keep(dist* coverage* tree*) stats(dist_fe st_y_fe st_m_fe year_fe N r2, ///
		labels(`"District FEs"' `"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
		`"Year FE"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 0 3)) nocons ///
		mlabels("OLS" "WLS" "OLS" "WLS" "OLS" "WLS") refcat(dist_f_cum_km2 ///
		"\emph{Panel A: Duration (min)}", nolabel) nomtitles ///
		star(* .1 ** .05 *** .01) label nonotes booktabs se b(%5.3f) ///
		se(%5.3f) width(\hsize)
	eststo clear
	
	* Panel B: Distance
	reg_dm distance_md dist_f_cum_km2 `ba_controls'
	esttab using "${TABLE}/tables/birding_activity_md_dist.tex", append f ///
		keep(dist* coverage* tree*) nocons mlabels(none) nomtitles ///
		nonumbers stats(dist_fe st_y_fe st_m_fe year_fe N r2, ///
		labels(`"District FEs"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 3)) refcat(dist_f_cum_km2 ///
		"\emph{Panel B: Distance (km)}", nolabel) ///
		star(* .1 ** .05 *** .01) label nonotes booktabs se b(%5.3f) ///
		se(%5.3f) width(\hsize)
	eststo clear
}



