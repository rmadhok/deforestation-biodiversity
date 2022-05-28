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
gl BACKUP 	"/Volumes/Backup Plus 1/research/data/def_biodiv/parivesh/"
gl ROOT 	"/Users/rmadhok/Dropbox/def_biodiv"
gl DATA 	"${ROOT}/data"
gl TABLE	"${ROOT}/docs/jmp/tex_doc/v3"
cd "${TABLE}"

// Modules
local bartik	0
local hte		1
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
	
	* Historical Land Tenure (163 districts at 1991 borders)
	use "${DATA}/dta/polecon_91_v02", replace
	keep if year > 2014 // bc 2015 used for pre-period shares
	la var mahrai "Inclusive (=1)"
	
	eststo nld: reghdfe dist_f_cum_km2 c.st_f_cum_km2_p##(c.mahrai c.st_share c.tot_area c.lat c.alt c.coastal), ///
			a(c_code_1991_num state_code_1991_num#year month) vce(cl c_code_1991_num)
	
			estadd local dist_fe "$\checkmark$"
			estadd local st_y_fe "$\checkmark$"
			estadd local month_fe "$\checkmark$"	
	
	/*
	* Reservation (starts in 2014)
	use "${DATA}/dta/polecon_01_v02", clear
	replace st_seats = st_seats * 100
	g mahrai = 1 // for labels
	la var mahrai "Inclusive (=1)"

	eststo res: reghdfe dist_f_cum_km2 st_seats st_curr_share st_cen_share elec tot_pop_i, ///
		a(c_code_2001_num year) vce(cl c_code_2001_num)
		
			estadd local dist_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
	*/
	* Table
	esttab nld using "${TABLE}/tables/polecon_ss.tex", replace ///
		label keep(c.*mahrai) ///
		stats(dist_fe st_y_fe month_fe N r2, labels(`"District FEs"' ///
		`"State x Year FEs"' `"Month FEs"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 3)) interaction(" $\times$ ") ///
		indicate("ST Share=st_state_share") wrap nocons nonotes ///
		booktabs nomtitles star(* .1 ** .05 *** .01) se b(%5.3f)
		
}

*-------------------------------------------------------------------------------
* HETEROGENEOUS EFFECTS
*-------------------------------------------------------------------------------
if `hte' == 1 {

	**# 1. historical land tenure

	* Read master
	use "${DATA}/dta/fc_ebd_udt_v02", clear
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", keep(3) nogen // 40 districts untracked
	
	* Aggregate to 1991 border
	collapse (mean) temp rain tree_cover_s duration ///
					distance exp_idx coverage_udym ///
					traveling group_size rad_sum sr ///
			 (sum) n_trips tree_cover_base tot_area ///
			 (first) year month biome, by(uid c_code_1991 year_month) 
	
	* Merge to banerjee
	merge m:1 c_code_1991 year_month using "${DATA}/dta/polecon_91_v02", keep(1 3) nogen
	
	* Prep
	drop if year == 2014
	drop_outliers
	foreach v of varlist duration exp_idx coverage_udym group_size rad_sum {
		g ln_`v' = ln(`v')
	}
	g ln_distance = asinh(distance)
	g tree_cover_base_s = tree_cover_base / tot_area
	local ctrls temp rain tree_cover_s ln_duration ln_distance ///
		        ln_exp_idx ln_coverage_udym traveling ln_group_size rad_sum

	* HTE
	eststo: reghdfe sr c.dist_f_cum_km2##c.(mahrai st_share tree_cover_base_s) `ctrls', ///
			a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)

			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
	ss
	**# FRA (2006)

	* 2015 FRA Fraction
	use "${DATA}/dta/fc_dym_s2_v02", clear
	collapse (lastnm) fra = n_fra_cum_s (first) state_code_2011, by(c_code_2011 year)
	keep if year == 2015
	egen fra_mean = mean(fra)
	g fra_d = (fra )
	la var fra_d "ST Consulted (=1)"
	drop year
	
	* Merge
	merge 1:m c_code_2011 using "${DATA}/dta/polecon_v02", keep(3) nogen // ST share, scheduled area, etc
	merge 1:m c_code_2011 year_month using "${DATA}/dta/fc_ebd_udt_v02", keep(3) nogen
	keep if year > 2015
	drop_outliers
	local ctrls temp rain tree_cover_s ln_duration ln_distance ///
		        ln_exp_idx ln_coverage_udym traveling ln_group_size rad_sum
	g mahrai = 1 // for tabulation only
	la var mahrai "Inclusive (=1)"
				
	eststo: reghdfe sr c.dist_f_cum_km2##c.(fra_d st_share) `ctrls', ///
			a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
			
			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"

	* Table
	esttab using "${TABLE}/tables/hte_polecon.tex", replace ///
		keep(dist_f_cum_km2 c*mahrai c*fra_d) stats(user_y_fe ///
		dist_fe st_m_fe N r2, labels(`"User $\times$ Year FEs"' ///
		`"District FEs"' `"State x Month FEs"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 3)) interaction(" $\times$ ") indicate("ST Share=c*st_share") ///
		wrap nocons nonotes booktabs nomtitles star(* .1 ** .05 *** .01) ///
		label se b(%5.3f) width(0.8\hsize) varwidth(40)
	eststo clear
}

*-------------------------------------------------------------------------------
* MECHANISMS
*-------------------------------------------------------------------------------
if `project' == 1 {
	
	**# 1. District-Month level project characteristics
	/*
	* Read project level
	use "${DATA}/dta/fc_pdym_s2_v02", clear
	
	* Prep
	keep c_code_2011 prop_no proj_fra_num dist_f dist_nf cba_num year_month
	g year = year(dofm(year_month))
	g month = month(dofm(year_month))
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", ///
		keepusing(c_code_1991) keep(3) nogen
	
	* Aggregate to 1991 borders
	g encroach_frac = dist_f / (dist_f + dist_nf)
	collapse (mean) cba = cba_num fra = proj_fra_num encroach_frac ///
			 (first) year month, by(c_code_1991 year_month)
	
	* Merge landlord
	merge m:1 c_code_1991 year_month using "${DATA}/dta/polecon_91_v02", keep(3) nogen
	tempfile temp
	save "`temp'"
	
	* Get controls
	use "${DATA}/dta/fc_ebd_udt_v02", clear
	collapse (first) temp rain, by(c_code_2011 year_month)
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", keep(3) nogen
	collapse (mean) temp rain, by(c_code_1991 year_month)
	merge 1:1 c_code_1991 year_month using "`temp'", keep(2 3) nogen
	
	* Mechanisms
	g tree_cover_base_s = tree_cover_base/tot_area
	foreach v of varlist cba fra encroach_frac {
		eststo: reghdfe `v' mahrai st_share tot_area tree_cover_base_s alt lat coastal, ///
			a(state_code_1991_num#year month) vce(cl state_code_1991_num)
			
			sum `v' if e(sample)==1
			estadd scalar ymean = `r(mean)'
			estadd local st_y_fe "$\checkmark$"
			estadd local month "$\checkmark$"
			estadd local clust "District"
	}
	*/
	**# 3. Project Level
	
	* Read project level
	use "${DATA}/dta/fc_pdym_s2_v02", clear

	* Prep
	keep c_code_2011 prop_no proj_fra_num dist_f dist_nf cba_num year_month proj_in_pa_esz
	g year = year(dofm(year_month))
	g month = month(dofm(year_month))
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", ///
		keepusing(c_code_1991) keep(3) nogen
	merge m:1 c_code_1991 year_month using "${DATA}/dta/polecon_91_v02", keep(3) nogen // merge landlord
	
	* Regressions
	g encroach_frac = dist_f / (dist_f + dist_nf)
	g tree_cover_base_s = tree_cover_base / tot_area
	local controls st_share tree_cover_base_s tot_area dist_f alt lat coastal
	
	foreach v of varlist cba_num proj_fra_num proj_in_pa_esz encroach_frac {
		
		eststo: reghdfe `v' mahrai `controls', ///
			a(state_code_1991_num year_month) vce(cl state_code_1991_num)
			
			sum `v' if e(sample)==1
			estadd scalar ymean = `r(mean)'
			estadd local st_fe "$\checkmark$"
			estadd local ym_fe "$\checkmark$"
		
		eststo: reghdfe `v' mahrai `controls', ///
			a(state_code_1991_num#year month) vce(cl state_code_1991_num)
			
			sum `v' if e(sample)==1
			estadd scalar ymean = `r(mean)'
			estadd local st_y_fe "$\checkmark$"
			estadd local month_fe "$\checkmark$"
		
		eststo: reghdfe `v' mahrai `controls', ///
			a(state_code_1991_num#year_month) vce(cl state_code_1991_num)
			
			sum `v' if e(sample)==1
			estadd scalar ymean = `r(mean)'
			estadd local st_ym_fe "$\checkmark$"		
	}
	
	esttab using "${TABLE}/tables/mech_polecon.tex", replace ///
		keep(mahrai) stats(ymean st_fe st_y_fe st_ym_fe ym_fe month_fe N r2, ///
		labels(`"Outcome Mean"' `"State FE"' `"State $\times$ Year FEs"' ///
		`"State $\times$ Yearmonth FEs"' `"Yearmonth FEs"' `"Month FEs"' ///
		`"N"' `"\(R^{2}\)"') fmt(3 0 0 0 0 0 0 3)) ///
		mgroups("Cost-Benefit Analysis (=1)" "Tribal Consutations (=1)" ///
		"Protected Area/ESZ (=1)" "Encroachment (\%)", ///
		pattern(1 0 0 1 0 0 1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) ///
		suffix(}) span erepeat(\cmidrule(lr){@span})) indicate("Controls=`controls'") ///
		wrap nocons nonotes booktabs nomtitles star(* .1 ** .05 *** .01) ///
		label se b(%5.3f)
	eststo clear
	
}




