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
	
	* Read ST presence
	*use "${DATA}/dta/polecon_v02", clear
	*keep if year > 2015 // bc 2014 is used for pre-period shares
	
	* Historical Land Tenure (163 districts at 1991 borders)
	use "${DATA}/dta/polecon_91_v02", replace
	keep if year > 2015
	la var mahrai "Inclusive (=1)"
	
	eststo nld: reghdfe dist_f_cum_km2 c.st_f_cum_km2_p##(c.mahrai c.st_state_share), ///
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
	kk
	esttab nld using "${TABLE}/tables/polecon_ss.tex", replace ///
		label keep(c.*mahrai) ///
		stats(dist_fe st_y_fe month_fe N r2, labels(`"District FEs"' ///
		`"State x Year FEs"' `"Month FEs"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 3)) interaction(" $\times$ ") ///
		indicate("ST Share=st_state_share") wrap nocons nonotes ///
		booktabs nomtitles star(* .1 ** .05 *** .01) se b(%5.3f) width(0.3\hsize)
	
	* Plot
	coefplot (nld, keep(c.*mahrai) rename(c.st_f_cum_km2_p#c.mahrai = "Inclusive (=1)") ///
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

	**# historical land tenure
	
	* Read master
	use "${DATA}/dta/fc_ebd_udt_v02", clear
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", keep(3) nogen // 40 districts untracked
	
	* Aggregate to 1991 border
	collapse (mean) temp rain tree_cover_s duration ///
					distance exp_idx coverage_udym ///
					traveling group_size rad_sum sr ///
			 (sum) n_trips ///
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
	local ctrls temp rain tree_cover_s ln_duration ln_distance ///
		        ln_exp_idx ln_coverage_udym traveling ln_group_size rad_sum
	
	* HTE (remove dist FE?, add if britdum==1)
	eststo nld: reghdfe sr c.dist_f_cum_km2##(c.mahrai c.st_state_share) `ctrls', ///
			a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)
	
			sum sr if e(sample)==1
			estadd scalar ymean = `r(mean)'
			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
	kk
	
	/*
	**# FRA (2006)
	
	* 2014 FRA Fraction
	use "${DATA}/dta/fc_dym_s2_v02", clear
	collapse (lastnm) fra = n_fra_cum_s, by(c_code_2011 year)
	keep if year == 2015
	sum fra, d
	g fra_d = (fra >= r(mean))
	drop year
	
	merge 1:m c_code_2011 using "${DATA}/dta/fc_ebd_udt_v02", keep(3) nogen
	keep if year > 2015
	drop_outliers
	
	reghdfe sr c.dist_f_cum_km2##c.fra_d `ctrls', ///
			a(uid#year c_code_2011_num state_code_2011_num#month) vce(cl biome)
	*/
	
	
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
	la var mahrai "Inclusive (=1)"
	
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
	esttab nld using "${TABLE}/tables/hte_polecon.tex", replace ///
		keep(dist_f_cum_km2 c*mahrai) stats(user_y_fe ///
		dist_fe st_m_fe N r2, labels(`"User $\times$ Year FEs"' ///
		`"District FEs"' `"State x Month FEs"' `"N"' ///
		`"\(R^{2}\)"') fmt(0 0 0 0 3)) interaction(" $\times$ ") ///
		indicate("ST Share=st_state_share") wrap nocons nonotes booktabs ///
		nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) width(0.5\hsize)
	eststo clear
}

*-------------------------------------------------------------------------------
* MECHANISMS
*-------------------------------------------------------------------------------
if `project' == 1 {
	
	**# 1. District-Month Panel
	/*
	* Read project level
	use "${DATA}/dta/fc_pdym_s2_v02", clear
	
	* Aggregate to dist-month
	collapse (sum) n_cba = cba_num n_disp = displacement_num n_fra = proj_fra_num ///
		     (count) n_proj = dist_id, ///
			 by(c_code_2011 year_month)
	
	* Control group
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", keepus(c_code_2011)
	
	* Balance
	encode c_code_2011, gen(c_code_2011_num)
	tsset c_code_2011_num year_month
	sort year_month 
	g ym = year_month[1]
	replace year_month = ym if _m == 2
	tsfill, full
	drop c_code_2011 _merge ym
	decode c_code_2011_num, gen(c_code_2011)
	
	* Dates
	g year = year(dofm(year_month))
	g month = month(dofm(year_month))
	keep if inrange(year, 2015, 2020)
	
	* Construct variables
	foreach v of varlist n_proj n_cba n_disp n_fra {
		
		replace `v' = 0 if `v' == . // non-treated
		bys c_code_2011 (year_month): g `v'_cum = sum(`v') // cumulative
	}
	
	* Adjust to 1991 borders
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", ///
		keepusing(c_code_1991) keep(3) nogen
	collapse (sum) n_*_cum (first) year month, by(c_code_1991 year_month)

	* Shares
	foreach var of varlist n_cba_cum-n_fra_cum {
		
		g `var'_s = 0 
		replace `var'_s = (`var' / n_proj_cum) if n_proj_cum > 0
	}
	
	* Merge landlord
	merge m:1 c_code_1991 year_month using "${DATA}/dta/polecon_91_v02", keep(1 3) nogen

	**# Regression
	g tree_cover_base_s = tree_cover_base / tot_area
	foreach v of varlist n_cba_cum-n_fra_cum {
		reghdfe `v' mahrai st_state_share tree_cover_base_s tot_area, a(state_code_1991_num#year_month) vce(cl c_code_1991_num)
	}
	
	**# 2. District-Month level project characteristics
	
	* Read project level
	use "${DATA}/dta/fc_pdym_s2_v02", clear
	
	* Prep
	keep c_code_2011 prop_no proj_fra_num dist_f dist_nf ///
		displacement_num cba_num year_month patches
	g year = year(dofm(year_month))
	g month = month(dofm(year_month))
	
	* Merge to crosswalk
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", ///
		keepusing(c_code_1991) keep(3) nogen
	
	* Aggregate
	g encroach_frac = dist_f / (dist_f + dist_nf)
	collapse (mean) cba = cba_num fra = proj_fra_num disp = displacement_num ///
				    encroach_frac patches ///
			 (first) year month, by(c_code_1991 year_month)
	
	* Merge landlord
	merge m:1 c_code_1991 year_month using "${DATA}/dta/polecon_91_v02", keep(3) nogen
	tempfile temp
	save "`temp'"
	
	* Get controls
	/*
	use "${DATA}/dta/fc_ebd_udt_v02", clear
	collapse (first) temp rain, by(c_code_2011 year_month)
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", keep(3) nogen
	collapse (mean) temp rain, by(c_code_1991 year_month)
	merge 1:1 c_code_1991 year_month using "`temp'", keep(2 3) nogen
	*/
	* Mechanisms
	foreach v of varlist cba fra disp encroach_frac patches {
		eststo: reghdfe `v' mahrai st_state_share tree_cover_base tot_area, a(state_code_1991_num#year_month) vce(cl state_code_1991_num)
			
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
	
	keep c_code_2011 prop_no proj_fra_num dist_f dist_nf ///
		displacement_num cba_num year_month patches tot_fam_disp
	g year = year(dofm(year_month))
	g month = month(dofm(year_month))
	
	* Merge to crosswalk
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", ///
		keepusing(c_code_1991) keep(3) nogen
	
	* Merge landlord
	merge m:1 c_code_1991 year_month using "${DATA}/dta/polecon_91_v02", keep(3) nogen
	
	* Regressions (state-year_month, state + year_month works)
	g encroach_frac = dist_f / (dist_f + dist_nf)
	g tree_cover_base_s = tree_cover_base / tot_area
	local controls st_state_share tree_cover_base_s tot_area tot_pop dist_f britdum 
	foreach v of varlist cba_num proj_fra_num displacement_num {
		
		eststo: reghdfe `v' mahrai `controls', a(state_code_1991_num#year_month) ///
			vce(cl state_code_1991_num)
			
			sum `v' if e(sample)==1
			estadd scalar ymean = `r(mean)'
			estadd local st_ym_fe "$\checkmark$"
			estadd local clust "State"
	}
	kk
	esttab using "${TABLE}/tables/mech_polecon.tex", replace ///
		keep(mahrai) stats(ymean st_ym_fe clust N r2, ///
		labels(`"Outcome Mean"' `"State $\times$ Time FEs"' ///
		`"Clustering"' `"N"' `"\(R^{2}\)"') fmt(3 0 0 0 3)) ///
		mlabels("CBA (=1)" "FRA (=1)" "Displacement") ///
		indicate("Controls=`controls'") wrap nocons nonotes ///
		booktabs nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) width(1\hsize)
	eststo clear
	
}




