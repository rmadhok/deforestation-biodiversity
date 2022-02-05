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

*------------------
* PROGRAM
*------------------
// Drop Outliers
capture program drop drop_outliers
	program define drop_outliers

	egen n_trips_pc99 = pctile(n_trips), p(99)
	gen outlier = (n_trips > n_trips_pc99)
	drop if outlier == 1

end
*-------------------------------------------------------------------------------
* SHIFT-SHARE
*-------------------------------------------------------------------------------
if `bartik' == 1{
	
	* Read
	use "${DATA}/dta/polecon_v02", clear
	keep if year >= 2015 // bc 2014 is used for pre-period shares
	
	* ST Presence/autonomy
	foreach v in st scst {
		
		eststo: reghdfe dist_f_cum_km2 c.st_f_cum_km2_p##c.`v'_ha p_`v'_share p_tcover, ///
			a(c_code_2011_num state_code_2011_num#month year) vce(cl c_code_2011_num)
		
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	}
	esttab using "${TABLE}/v3/tables/polecon_fra.tex", replace keep(c.*) ///
		stats(dist_fe st_m_fe year_fe N r2, labels(`"District FEs"' ///
		`"State x Month FEs"' `"Year FEs"' `"N"' `"\(R^{2}\)"') fmt(0 0 0 0 3)) ///
		interaction(" $\times$ ") wrap nocons nonotes booktabs nomtitles ///
		star(* .1 ** .05 *** .01) label se b(%5.3f) width(0.6\hsize)
	eststo clear

	* Political Reservation
	foreach v in st scst {
		
		eststo: reghdfe dist_f_cum_km2 c.st_f_cum_km2_p##c.`v'_seats p_`v'_share p_tcover, ///
			a(c_code_2011_num state_code_2011_num#month year) vce(cl c_code_2011_num)
		
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	}
	esttab using "${TABLE}/v3/tables/polecon_seats.tex", replace keep(c.*) ///
		stats(dist_fe st_m_fe year_fe N r2, labels(`"District FEs"' ///
		`"State x Month FEs"' `"Year FEs"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 3)) interaction(" $\times$ ") wrap nocons nonotes ///
		booktabs nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f)
	eststo clear
	
	* Reservation X presence (triple interaction)
	foreach v in st scst {
		
		eststo: reghdfe dist_f_cum_km2 c.st_f_cum_km2##c.`v'_seats##c.`v'_ha p_`v'_share p_tcover, a(c_code_2011_num state_code_2011_num#month year) ///
			vce(cl c_code_2011_num)
		
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	}
	esttab using "${TABLE}/v3/tables/polecon_fra_seats.tex", replace ///
		keep(c.*#c.*_seats c*#*#*) stats(dist_fe st_m_fe year_fe N r2, ///
		labels(`"District FEs"' `"State x Month FEs"' `"Year FEs"' `"N"' ///
		`"\(R^{2}\)"') fmt(0 0 0 0 3)) wrap nocons nonotes booktabs nomtitles ///
		star(* .1 ** .05 *** .01) label se b(%5.3f)
	eststo clear
}

*-------------------------------------------------------------------------------
* HETEROGENEOUS EFFECTS
*-------------------------------------------------------------------------------
if `hte' == 1 {
	
	* Read master
	use "${DATA}/dta/fc_ebd_udt_v02"
	
	* Merge political economy
	merge m:1 c_code_2011 year_month using "${DATA}/dta/polecon_v02", keep(1 3) keepusing(*_seats schedule) nogen // mismatched are 2019-2020 (no election data)
	
	* Prep
	drop if year == 2014
	drop_outliers
	local ctrls temp rain tree_cover_s ln_duration ln_distance ///
		        ln_exp_idx ln_coverage_udym traveling ln_group_size
	replace st_seats = st_seats*10 // scaling factor
	g dist_f_cum_ihs = asinh(dist_f_cum_km2) // het table only works with IHS
	
	* HTE
	foreach v of varlist st_seats sc_seats schedule {
		
		eststo: reghdfe sr c.dist_f_cum_km2##c.`v' `ctrls', ///
			a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
			
			sum sr if e(sample)==1
			estadd scalar ymean = `r(mean)'
			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
	}
	
	esttab using "${TABLE}/v3/tables/hte_polecon.tex", replace ///
		keep(dist_f_cum_km2 c.dist_f_cum_km2#*) stats(ymean user_y_fe ///
		dist_fe st_m_fe N r2, labels(`"Outcome Mean"' `"User $\times$ Year FEs"' ///
		`"District FEs"' `"State x Month FEs"' `"N"' `"\(R^{2}\)"') ///
		fmt(3 0 0 0 0 3)) interaction(" $\times$ ") wrap nocons nonotes booktabs ///
		nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) width(0.8\hsize)
	eststo clear
}

*-------------------------------------------------------------------------------
* MECHANISMS
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
	merge m:1 c_code_2011 year_month using "${DATA}/dta/polecon_v02", keep(1 3) keepusing(*_seats *_state_share *_pop) nogen
	replace st_seats = st_seats*10
	
	* Get controls
	tempfile temp
	save "`temp'"
	use "${DATA}/dta/fc_ebd_udt_v02", clear
	collapse (first) temp rain tree* tot_area, by(c_code_2011 year_month)
	merge 1:m c_code_2011 year using "`temp'", keep(2 3) nogen
	
	* Mechanisms
	g ln_patches = ln(patches)
	foreach v of varlist cba_num proj_fra_num displacement_num tot_fam_disp dist_f ln_patches {
		
		eststo: reghdfe `v' st_seats st_state_share, a(state_code_2011_num#year month) vce(cl c_code_2011_num)
			
			sum `v' if e(sample)==1
			estadd scalar ymean = `r(mean)'
			estadd local st_y_fe "$\checkmark$"
			estadd local month "$\checkmark$"
			estadd local clust "District"
	}
	esttab using "${TABLE}/v3/tables/mech_polecon.tex", replace ///
		keep(st_seats) stats(ymean st_y_fe month clust N r2, ///
		labels(`"Outcome Mean"' `"State $\times$ Year FEs"' `"Month FEs"' ///
		`"Clustering"' `"N"' `"\(R^{2}\)"') fmt(3 0 0 0 0 3)) ///
		mlabels("CBA (=1)" "FRA (=1)" "Displ. (=1)" "Displ. (Fam.)" ///
		"Size (ha.)" "log(patches)") wrap nocons nonotes booktabs nomtitles ///
		star(* .1 ** .05 *** .01) label se b(%5.3f)
	eststo clear

}




