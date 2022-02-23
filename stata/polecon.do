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
gl TABLE	"${ROOT}/docs/jmp/tex_doc/v3"
cd "${TABLE}"

// Modules
local bartik	1
local hte		0
local project	0

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
if `bartik' == 1 {
	
	* Read
	use "${DATA}/dta/polecon_v02", clear
	keep if year > 2015 // bc 2014 is used for pre-period shares
	
	* ST Presence
	foreach v of varlist st_ha schedule {
		
		eststo `v': reghdfe dist_f_cum_km2 c.st_f_cum_km2_p##(c.`v' c.st_state_share), ///
			a(c_code_2011_num state_code_2011_num#month year) vce(cl c_code_2011_num)
		
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	}
	
	* Historical Land Tenure (163 districts at 1991 borders)
	use "${DATA}/dta/polecon_91_v02", replace
	keep if year > 2015
	
	eststo nld: reghdfe dist_f_cum_km2 c.st_f_cum_km2_p##(c.mahrai c.st_ha), ///
			a(c_code_1991_num state_code_1991_num#month year) vce(cl c_code_1991_num)
	
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"

	* Reservation (starts in 2014)
	use "${DATA}/dta/polecon_01_v02", clear
	replace st_seats = st_seats * 100
	g mahrai = 1 // for labels
	la var mahrai "Non-landlord (=1)"

	eststo res: reghdfe dist_f_cum_km2 st_seats st_curr_share st_cen_share elec tot_pop_i, ///
		a(c_code_2001_num year) vce(cl c_code_2001_num)
		
			estadd local dist_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	kk
	* Table
	esttab res nld using "${TABLE}/tables/polecon_ss.tex", replace ///
		label keep(c.*mahrai st_seats) ///
		stats(dist_fe st_m_fe year_fe N r2, labels(`"District FEs"' ///
		`"State x Month FEs"' `"Year FEs"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 3)) interaction(" $\times$ ") ///
		indicate("Reservation Controls=st_curr_share") wrap nocons nonotes ///
		booktabs nomtitles star(* .1 ** .05 *** .01) se b(%5.3f)
	
	* Plot
	coefplot (nld, keep(c.*mahrai) rename(c.st_f_cum_km2_p#c.mahrai = "Non-landlord (=1)") ///
		\ res, keep(st_seats) rename(st_seats = "ST Seat Share")), ///
		xline(0, lcolor(black*0.8) lpattern(dash)) coeflabels(, labsize(medium)) ///
		mlabsize(medium) mlabcolor(black) levels(99 95 90) ///
		ciopts(recast(rcap) lwidth(*1 *3 *4) color(dkgreen dkgreen dkgreen) ///
		lcolor(*.3 *.5 *.8)) legend(order(1 "99" 2 "95" 3 "90")) msize(medium) ///
		mfcolor(white) msymbol(D) mlcolor(black) mlabel format(%9.2g) ///
		mlabposition(11) mlabgap(*2) xlabel(-0.25(0.1)0.05, labsize(medium)) ///
		ylabel(, labsize(medium)) xsize(4.6)
	graph export "${TABLE}/fig/polecon_ss.png", replace
	eststo clear
		
}

*-------------------------------------------------------------------------------
* HETEROGENEOUS EFFECTS
*-------------------------------------------------------------------------------
if `hte' == 1 {
	
	**# ST presence
	/*
	* Read master
	use "${DATA}/dta/fc_ebd_udt_v02"
	merge m:1 c_code_2011 year_month using "${DATA}/dta/polecon_v02", ///
		keep(1 3) keepusing(*_ha *_share schedule) nogen // mismatched are 2019-2020 (no election data)

	* Prep
	drop if year == 2014
	drop_outliers
	local ctrls temp rain tree_cover_s ln_duration ln_distance ///
		        ln_exp_idx ln_coverage_udym traveling ln_group_size
	
	* HTE
	foreach v of varlist st_ha schedule {
		
		eststo `v': reghdfe sr c.dist_f_cum_km2##c.`v' `ctrls', ///
			a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
			
			sum sr if e(sample)==1
			estadd scalar ymean = `r(mean)'
			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
	}
	*/
	**# historical land tenure
	
	* Read master
	use "${DATA}/dta/fc_ebd_udt_v02", clear
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", keep(3) nogen // 40 districts untracked
	
	* Aggregate to 1991 border
	collapse (mean) temp rain tree_cover_s duration ///
					distance exp_idx coverage_udym ///
					traveling group_size sr ///
			 (sum) n_trips ///
			 (first) year month biome, by(uid c_code_1991 year_month) 
	
	* Merge to banerjee
	merge m:1 c_code_1991 year_month using "${DATA}/dta/polecon_91_v02", keep(1 3) nogen
	
	* Prep
	drop if year == 2014
	drop_outliers
	foreach v of varlist duration exp_idx coverage_udym group_size {
		g ln_`v' = ln(`v')
	}
	g ln_distance = asinh(distance)
	local ctrls temp rain tree_cover_s ln_duration ln_distance ///
		        ln_exp_idx ln_coverage_udym traveling ln_group_size
	
	* HTE
	eststo nld: reghdfe sr c.dist_f_cum_km2##(c.mahrai c.st_state_share) `ctrls', ///
			a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)	
	
			sum sr if e(sample)==1
			estadd scalar ymean = `r(mean)'
			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
	
	**# Reservations
	
	* Read master
	use "${DATA}/dta/fc_ebd_udt_v02", clear
	merge m:1 c_code_2011 using "${DATA}/dta/c_code_crosswalk", keep(3) nogen // 18 districts untracked from SHRUG
	
	* Aggregate to 2001 border
	collapse (mean) temp rain tree_cover_s duration ///
					distance exp_idx coverage_udym ///
					traveling group_size sr ///
			 (sum) n_trips ///
			 (first) year month biome, by(uid c_code_2001 year_month) 
	
	* Merge to reservations
	merge m:1 c_code_2001 year_month using "${DATA}/dta/polecon_01_v02", keep(1 3) nogen
	
	* Prep
	drop if year == 2014
	drop_outliers
	foreach v of varlist duration exp_idx coverage_udym group_size {
		g ln_`v' = ln(`v')
	}
	g ln_distance = asinh(distance)
	local ctrls temp rain tree_cover_s ln_duration ln_distance ///
		        ln_exp_idx ln_coverage_udym traveling ln_group_size
	g mahrai = 1 // for labelling
	la var mahrai "Non-landlord (=1)"
	
	* HTE 
	// Note: we use user FE only bc otherwise insufficient variation
	eststo res: reghdfe sr c.dist_f_cum_km2##(c.st_seats c.st_curr_share c.st_cen_share c.elec c.tot_pop_i) `ctrls', ///
			a(uid c_code_2001_num state_code_2001_num#month) vce(cl biome)	
	
			sum sr if e(sample)==1
			estadd scalar ymean = `r(mean)'
			estadd local user_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
	
	
	* Table
	esttab res nld using "${TABLE}/tables/hte_polecon.tex", replace ///
		keep(dist_f_cum_km2 c*mahrai c*st_seats) stats(ymean user_y_fe ///
		user_fe dist_fe st_m_fe N r2, labels(`"Outcome Mean"' ///
		`"User $\times$ Year FEs"' `"User FEs"' `"District FEs"' ///
		`"State x Month FEs"' `"N"' `"\(R^{2}\)"') fmt(3 0 0 0 0 0 3)) ///
		interaction(" $\times$ ") indicate("Reservation Controls=st_seats") ///
		wrap nocons nonotes booktabs nomtitles star(* .1 ** .05 *** .01) ///
		label se b(%5.3f)
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
	
	* Merge to crosswalk
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", ///
		keepusing(c_code_1991) keep(3) nogen
	
	* Merge landlord
	merge m:1 c_code_1991 year_month using "${DATA}/dta/polecon_91_v02", keep(1 3) nogen
	
	* Get controls
	tempfile temp
	save "`temp'"
	use "${DATA}/dta/fc_ebd_udt_v02", clear
	collapse (first) temp rain tree* tot_area, by(c_code_2011 year_month)
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", keep(3) nogen
	collapse (mean) temp rain (sum) tot_area tree_cover_base, by(c_code_1991 year_month)
	kk
	
	merge 1:m c_code_1991 year_month using "`temp'"
	kk
	
	* Mechanisms
	g ln_patches = ln(patches)
	foreach v of varlist cba_num proj_fra_num displacement_num tot_fam_disp dist_f patches {
		
		eststo: reghdfe `v' p_nland st_state_share, a(state_code_1991_num#year) vce(cl c_code_1991_num)
			
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




