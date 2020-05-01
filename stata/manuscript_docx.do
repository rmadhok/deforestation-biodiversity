*********************************************************************************
*																			
* PROJECT: 	Deforestation and Biodiversity										
*																			
* PURPOSE: 	ANALYSIS FOR SCIENCE MANUSCRIPT	    
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
local robustness		1
local appendix			0

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

	qui reghdfe `depvar' `indepvar' `ctrls' [aweight=n_trips], ///
		a(user_id#year c_code_2011_num state_code_2011_num#month) ///
		vce(cl state_code_2011_num#year)
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
		s_richness pop_density coverage n_* duration distance temp* rain* { 
	
			label variable `v' `"    `: variable label `v''"'	
		}

	** Collect
	local dist_vars tot_area pop_density n_users_dist n_trips_dist
	local trip s_richness
	local covariates tree_cover_mean coverage rainfall_mm temperature_mean
	
	*---------------------------------------
	* OUTCOMES AND COVARIATES
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
		refcat(s_richness "Outcome" , nolabel) nomtitle collabel(none) ///
		noobs label unstack nonumber plain
	
	** Covariates
	eststo clear
	eststo A: estpost tabstat `covariates' if any_def, s(n mean med sd) c(s)
	eststo B: estpost tabstat `covariates' if !any_def, s(n mean med sd) c(s)
	esttab A B using "${TABLE}/tables/sumstats_biodiv.rtf", append main(mean) ///
		aux(sd) cells("mean(fmt(2)) sd(fmt(2)) count(fmt(0))") ///
		refcat(tree_cover_mean "Covariates" , nolabel) nomtitle collabel(none) ///
		noobs label unstack nonumber plain ///
		addnotes("Note: Spatial coverage is the fraction of grid cells in a district" ///
		"containing at least one bird observation over the study period using a" ///
		"5km x 5km grid")

	*-----------------------------------
	* Forest Clearance
	*-----------------------------------
	
	* Per-District Month
	eststo clear
	eststo B: estpost tabstat dist_f_elec-dist_f_pa if any_def, s(n mean sd) c(s)

	* Project-wise area totals and shares
	use "${DATA}/dta/fc_clean", clear
	keep if prop_status == "approved"
	replace proj_cat = "other" if proj_cat == "industry" | proj_cat == "underground"
	
	levelsof proj_cat, local(category)
	levelsof proj_shape, local(shape)
	foreach cat of local category {
		
		gen `cat' = proj_area_forest2 if proj_cat == "`cat'"
		egen `cat'_sum = total(`cat')
		drop `cat'
		ren `cat'_sum `cat'
		replace `cat' = . if proj_cat != "`cat'"
	}

	foreach sh of local shape {
		gen `sh' = proj_area_forest2 if proj_shape == "`sh'"
		egen `sh'_sum = total(`sh')
		drop `sh'
		ren `sh'_sum `sh'
		replace `sh' = . if proj_shape != "`sh'"
	}
	
	gen dist_f_pa = proj_area_forest2 if proj_in_pa_esz_num
	egen dist_f_pa_sum = total(dist_f_pa)
	drop dist_f_pa
	ren dist_f_pa_sum dist_f_pa
	replace dist_f_pa = . if proj_in_pa_esz_num == 0
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
	
	* Totals
	eststo A: estpost tabstat dist_f_elec-dist_f_pa, s(n mean sd) c(s)
	
	esttab A B using "${TABLE}/tables/sumstats_deforest2.rtf", replace ///
		label cells("count(fmt(0)) mean(fmt(2)) sd(fmt(2))") ///
		mgroups("Totals" "Per District-Yearmonth", pattern(1 1)) ///
		collabel("N" "Mean" "Std. Dev.") ///
		refcat(dist_f_elec "Project Type" dist_f_hyb "Project Group", nolabel) ///
		nomtitle unstack nonumber
	eststo clear
	
	** Spatial Correlation
	egen num_districts = rownonmiss(district_0-district_9), s
	la var num_districts "Districts"
	drop if num_districts == 0
	
	eststo: estpost tabulate num_districts, nototal
	esttab using "${TABLE}/tables/spatial_dep.rtf", replace ///	
		cells("count(fmt(0)) pct(fmt(2)) cumpct(fmt(2))") ///
		nonumbers nomtitle collabels("" "%" "Cum.") ///
		noobs gap label 
	eststo clear
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
	
	// SEASONALITY
	
	* 1. User x Month FE 
	eststo: qui reghdfe s_richness dist_f_cum_km2 dist_nf_cum_km2 ///
		`ctrls' n_mon_yr u_lin, ///
		a(user_id#month c_code_2011_num state_code_2011_num#year) ///
		vce(cl state_code_2011_num#year)
			estadd local user_m_fe "x"
			estadd local dist_fe "x"
			estadd local st_y_fe "x"
			estadd local u_trend "x"
	
	* 2. User x Year, Cubic Trend
	eststo: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' u_cubic
		estadd local u_trend "Cubic"
	
	// SLX WEIGHT MATRICES
	
	* 3. W = 1/d
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
	
	
	
	
	eststo: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' dist_f_cum_km2_s1 dist_nf_cum_km2_s1
		estadd local u_trend "None"
	
	*2. Spatial
	eststo: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' `slx_b' // binary cont.
		estadd local u_trend "None"
	eststo: reg_sat s_richness dist_f_cum_km2 dist_nf_cum_km2 `ctrls' ///
		dist_f_cum_km2_s1 dist_nf_cum_km2_s1 `slx_b' ///
		dist_f_cum_km2_slx_s1 dist_nf_cum_km2_slx_s1 // binary cont.
		estadd local u_trend "None"
	
	esttab using "${TABLE}/tables/robustness_science.rtf", replace ///
		keep(dist_f*) stats(user_m_fe user_y_fe dist_fe st_y_fe st_m_fe ///
		u_trend N r2, labels(`"User x Month FEs"' ///
		`"User x Year FEs"' `"District FEs"' `"State x Year FEs"' ///
		`"State x Month FEs"' `"User Trend"' `"N"' ///
		`"R^2"') fmt(0 0 0 0 0 0 0 3)) nomtitles star(* .1 ** .05 *** .01) ///
		label nonotes se b(%5.3f) se(%5.3f)
	eststo clear
		
}

*===============================================================================
* APPENDIX
*===============================================================================
if `appendix' == 1 {
	
	*1. Additional Spatial Weight Matrices
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
	
	esttab using "${TABLE}/tables/apx_sp_weight.rtf", replace ///
		keep(dist_f*) stats(user_m_fe user_y_fe dist_fe st_y_fe st_m_fe ///
		u_trend N r2, labels(`"User x Month FEs"' ///
		`"User x Year FEs"' `"District FEs"' `"State x Year FEs"' ///
		`"State x Month FEs"' `"User Trend"' `"N"' ///
		`"R^2"') fmt(0 0 0 0 0 0 0 3)) nomtitles star(* .1 ** .05 *** .01) ///
		label nonotes se b(%5.3f) se(%5.3f)
	eststo clear
	

}




