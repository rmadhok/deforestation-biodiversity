*********************************************************************************
*																			
* PROJECT: Deforestation and Biodiversity										
*																		
* PURPOSE: Political economy of forest approvals  				 							
*																			
* AUTHOR:  Raahil Madhok, madhokr@mail.ubc.ca											
*																				
********************************************************************************
*-------------------------------------------------------------------------------
*SET ENVIRONMENT
*-------------------------------------------------------------------------------
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
gl TABLE	"${ROOT}/docs/jmp/tex_doc/"
cd "${TABLE}"

// Modules
local bartik	0
local hte		0
local project	1
*-------------------------------------------------------------------------------
* SHIFT-SHARE
*-------------------------------------------------------------------------------
if `bartik' == 1{
	
	* Read
	use "${DATA}/dta/polecon_v02", clear
	keep if year>=2016
	
	* Label
	la var sc_ha "SC/ha."
	la var st_ha "ST/ha."
	la var ss_st_ha "ST/ha."
	la var ss_sc_ha "SC/ha."
	la var ss_scst_ha "Dalits/ha."
	la var st_f_cum_km2_p "State Approvals" 
	la var ss_6th_sched "5th + 6th Schedule"
	
	*-----------------------------------
	* ST Presence
	*-----------------------------------
	* Density
	foreach v of varlist st_ha scst_ha {
		
		eststo: reghdfe dist_f_cum_km2 ss_`v' st_f_cum_km2_p p_st p_tcover, ///
			a(c_code_2011_num state_code_2011_num#month year) vce(cl c_code_2011_num)
		
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	}
	
	* Scheduled Areas
	eststo: reghdfe dist_f_cum_km2 ss_6th_sched st_f_cum_km2_p p_st p_tcover, ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl c_code_2011_num)
		
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
		
	esttab using "${TABLE}/v3/tables/polecon_fra.tex", keep(ss_*) replace ///
		stats(dist_fe st_m_fe year_fe N r2, labels(`"District FEs"' ///
		`"State x Month FEs"' `"Year FEs"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 3)) ///
		wrap nocons nonotes booktabs nomtitles star(* .1 ** .05 *** .01) ///
		label se b(%5.3f)
	eststo clear
	
	*-----------------------------------
	* Political Reservation
	*-----------------------------------
	
	* Double interactions
	la var ss_st_seats "ST seat share"
	la var ss_scst_seats "Dalit seat share"
	foreach i in st scst {
		
		eststo: reghdfe dist_f_cum_km2 ss_`i'_seats st_f_cum_km2_p `i'_seats p_`i' p_tcover, ///
			a(c_code_2011_num state_code_2011_num#month year) vce(cl c_code_2011_num)
		
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	}
	esttab using "${TABLE}/v3/tables/polecon_seats.tex", keep(ss_*) replace ///
		stats(dist_fe st_m_fe year_fe N r2, labels(`"District FEs"' ///
		`"State x Month FEs"' `"Year FEs"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 3)) wrap nocons nonotes booktabs nomtitles ///
		star(* .1 ** .05 *** .01) label se b(%5.3f)
	eststo clear
	
	* Triple Interactions - no effect
	la var ss_st_ha_st_seats "ST seat share $\times$ STs/ha."
	la var ss_scst_ha_scst_seats "Dalit seat share $\times$ Dalits/ha."
	foreach i in st scst {
		
		eststo: reghdfe dist_f_cum_km2 ss_`i'_ha_`i'_seats ss_`i'_ha ss_`i'_seats `i'_ha_`i'_seats ///
			st_f_cum_km2_p `i'_seats p_`i' p_tcover, a(c_code_2011_num state_code_2011_num#month year) ///
			vce(cl c_code_2011_num)
		
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	}
	esttab using "${TABLE}/v3/tables/polecon_fra_seats.tex", replace ///
		keep(ss_*_seats) stats(dist_fe st_m_fe year_fe N r2, ///
		labels(`"District FEs"' `"State x Month FEs"' `"Year FEs"' `"N"' ///
		`"\(R^{2}\)"') fmt(0 0 0 0 3)) wrap nocons nonotes booktabs nomtitles ///
		star(* .1 ** .05 *** .01) label se b(%5.3f)
	eststo clear

}

*-------------------------------------------------------------------------------
* HETEROGENEOUS EFFECTS
*-------------------------------------------------------------------------------
if `hte' == 1 {
	
	// Drop Outliers
	capture program drop drop_outliers
		program define drop_outliers

		egen n_trips_pc99 = pctile(n_trips), p(99)
		gen outlier = (n_trips > n_trips_pc99)
		drop if outlier == 1

	end
	
	*-------------------
	* Merge Data
	*-------------------
	
	* Read master
	use "${DATA}/dta/fc_ebd_udt_v02"
	merge m:1 c_code_2011 year using "${DATA}/dta/iv_scst_seats", keep(1 3) nogen // reservations; mismatched are 2019-2020 (no election data)
	tempfile temp
	save "`temp'"
	
	* Merge scheduled areas
	import delimited "${DATA}/csv/india_scheduled_areas.csv", clear
	ren (c_code_11 fifth_schedule) (c_code_2011 scheduled)
	replace scheduled = 1 if inlist(state_ut, "Assam", "Meghalaya", "Mizoram", "Tripura")
	keep c_code_2011 scheduled
	merge 1:m c_code_2011 using "`temp'", keep(3) nogen
	order scheduled, last
	
	*------------------------
	* Heterogeneous effects
	*------------------------
	
	* Prep
	*drop_outliers
	local ctrls temp rain tree_cover_km2 ln_duration ln_distance ///
		        ln_exp_idx ln_coverage_udym
	replace st_seats = st_seats*10 // scaling factor
	foreach v of varlist st_seats sc_seats scheduled {
		g dist_f_`v' = dist_f_cum_km2 * `v'
	}
	la var dist_f_st_seats "Deforestation $\times$ ST seat share"
	la var dist_f_sc_seats "Deforestation $\times$ SC seat share"
	la var dist_f_scheduled "Deforestation $\times$ Scheduled Area"
	
	* ST seats
	eststo: reghdfe sr dist_f_cum_km2 dist_f_st_seats st_seats dist_nf_cum_km2 `ctrls', ///
		a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
		
		sum sr if e(sample)==1
		estadd scalar ymean = `r(mean)'
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		
	eststo: reghdfe sr dist_f_cum_km2 dist_f_sc_seats sc_seats dist_nf_cum_km2 `ctrls', ///
		a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
		
		sum sr if e(sample)==1
		estadd scalar ymean = `r(mean)'
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		
	eststo: reghdfe sr dist_f_cum_km2 dist_f_scheduled dist_nf_cum_km2 `ctrls', ///
		a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
		
		sum sr if e(sample)==1
		estadd scalar ymean = `r(mean)'
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
	
	esttab using "${TABLE}/v3/tables/hte_polecon.tex", replace ///
		keep(dist_f_cum_km2 dist_f_*_seats dist_f_scheduled) ///
		stats(ymean user_y_fe dist_fe st_m_fe N r2, labels(`"Outcome Mean"' ///
		`"User $\times$ Year FEs"' `"District FEs"' `"State x Month FEs"' ///
		`"N"' `"\(R^{2}\)"') fmt(3 0 0 0 0 3)) wrap nocons nonotes booktabs ///
		nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) width(0.8\hsize)
	eststo clear
	
}

*-------------------------------------------------------------------------------
* MECHANISMS -- project level
*-------------------------------------------------------------------------------
if `project' == 1 {
	
	* Read project level
	use "${DATA}/dta/fc_pdym_s2_v02", clear
	
	* Prep
	keep c_code_2011 prop_no dist_id date_submit date_rec ///
	     proj_in_pa_esz_num proj_fra_num dist_f dist_nf ///
		 displacement_num *_fam_disp cba_num year_month patches
	g year = year(dofm(year_month))
	g month = month(dofm(year_month))
	g state_code_2011 = substr(c_code_2011, 1, 3)
	encode state_code_2011, gen(state_code_2011_num)
	encode c_code_2011, gen(c_code_2011_num)
	
	* Merge seats (496 districts 2014-2018)
	merge m:1 c_code_2011 year using "${DATA}/dta/iv_scst_seats", keep(1 3) nogen
	replace st_seats = st_seats*10
	
	* Get controls
	tempfile temp
	save "`temp'"
	use "${DATA}/dta/fc_ebd_udt_v02", clear
	collapse (first) temp rain tree_cover_km2 tot_area, by(c_code_2011 year_month)
	merge 1:m c_code_2011 year using "`temp'", keep(2 3) nogen
	
	* Mechanisms
	local ctrls tree_cover_km2 tot_area
	g ln_patches = ln(patches)
	reghdfe cba_num st_seats, a(state_code_2011_num#year) vce(cl c_code_2011_num)
	reghdfe proj_fra_num st_seats, a(state_code_2011_num#year) vce(cl c_code_2011_num)
	reghdfe displacement_num st_seats, a(state_code_2011_num#year) vce(cl c_code_2011_num)
	reghdfe dist_f st_seats, a(c_code_2011_num state_code_2011_num#month year) vce(cl c_code_2011_num)
	reghdfe tot_fam_disp st_seats, a(state_code_2011_num#year month) vce(cl c_code_2011_num)
	reghdfe ln_patches st_seats, a(state_code_2011_num#year) vce(cl c_code_2011_num)

}




