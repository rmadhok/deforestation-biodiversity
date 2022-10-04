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

	* SEE https://stats.oarc.ucla.edu/stata/seminars/interactions-stata/#s1

	* Read master
	use "${DATA}/dta/fc_ebd_udt_v02", clear
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", keep(3) nogen // 40 districts untracked

	* Aggregate to 1991 border
	collapse (mean) temp rain duration distance exp_idx coverage_udym ///
					traveling group_size rad_sum sr tree_cover_s hour n_fra_cum_s ///
			 (sum) n_trips n_trips_user n_users_dist tot_area tree_cover_base dist_f_cum_km2_slx_ib ///
			 (first) year month biome, by(uid c_code_1991 year_month) 
			 
	* Merge to banerjee
	merge m:1 c_code_1991 year_month using "${DATA}/dta/polecon_91_v02", keep(1 3) nogen
	
	* Prep
	drop if year == 2014
	drop_outliers
	foreach v of varlist exp_idx duration group_size distance rad_* hour {
		g ln_`v' = ln(1+`v')
	}
	g ln_coverage_udym = ln(coverage_udym)
	g tree_cover_base_s = (tree_cover_base / tot_area)*100
	local ctrls temp rain ln_duration ln_distance ln_rad_sum tree_cover_s ///
	        ln_exp_idx ln_coverage_udym ln_group_size traveling ln_hour
	
	la var dist_f_cum_km2 "Infrastructure (\(km^{2}\))"
	la var st_share "Tribal Pop. Share"
	la var tree_cover_base_s "Baseline Forest Cover"
	
	**# Split Sample
/*
	eststo: reghdfe sr dist_f_cum_km2 `ctrls' if mahrai, ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)
		
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local clust "Biome"

	eststo: reghdfe sr dist_f_cum_km2 `ctrls' dist_f_cum_km2_slx_ib if mahrai, ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)
		
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local slx "$\checkmark$"
		estadd local clust "Biome"
	
	eststo: reghdfe sr dist_f_cum_km2 `ctrls' if mahrai [aw=n_trips], ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)
		
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local clust "Biome"
		estadd local wt "$\checkmark$"
	
	eststo: reghdfe sr dist_f_cum_km2 `ctrls' if mahrai, ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl c_code_1991)
		
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local clust "District"
	
	eststo: reghdfe sr dist_f_cum_km2 `ctrls' if !mahrai, ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)
		
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local clust "Biome"
	
	eststo: reghdfe sr dist_f_cum_km2 `ctrls' dist_f_cum_km2_slx_ib if !mahrai, ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)
		
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local slx "$\checkmark$"
		estadd local clust "Biome"
	
	eststo: reghdfe sr dist_f_cum_km2 `ctrls' if !mahrai [aw=n_trips], ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)
		
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local clust "Biome"
		estadd local wt "$\checkmark$"
	
	eststo: reghdfe sr dist_f_cum_km2 `ctrls' if !mahrai, ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl c_code_1991)
		
		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local clust "District"
		
	esttab using "${TABLE}/tables/polecon_split.tex", replace ///
		keep(dist_f_cum_km2) stats(user_y_fe dist_fe st_m_fe slx wt clust N r2, ///
		labels(`"User $\times$ Year FEs"' `"District FEs"' `"State x Month FEs"' ///
		`"Spillovers"' `"Weighted"' `"Clustering"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 0 3)) mgroups("Inclusive Institutions" "Extractive Institutions", ///
		pattern(1 0 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span ///
		erepeat(\cmidrule(lr){@span})) indicate("Controls=`ctrls'") nocons nonotes ///
		booktabs nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) width(1\hsize)
	eststo clear
*/
	**# Heterogeneity
	
	* 1. Control for ST share
	eststo: reghdfe sr c.dist_f_cum_km2##c.(mahrai st_share) `ctrls', ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)

		estadd local user_y_fe "$\checkmark$"
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local clust "Biome"

	* 2. Control for ST share + baseline forest cover
	eststo: reghdfe sr c.dist_f_cum_km2##c.(mahrai st_share tree_cover_base_s) `ctrls', ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)

			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local clust "Biome"
	
	* 3. Spillovers
	eststo: reghdfe sr c.dist_f_cum_km2##c.(mahrai st_share tree_cover_base_s) ///
		`ctrls' dist_f_cum_km2_slx_ib, ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)

			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local slx	 "$\checkmark$"
			estadd local clust "Biome"
	
	* 4. Weighted OLS
	eststo: reghdfe sr c.dist_f_cum_km2##c.(mahrai st_share tree_cover_base_s) ///
		`ctrls' [aw=n_trips], ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)
		
			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local wt "$\checkmark$"
			estadd local clust "Biome"
			
	* 5. High-activity Sample
	egen tag_d = tag(c_code_1991) 
	egen tag_u = tag(uid)
	sum n_trips_user if tag_u, d
	g n_trips_user_med = r(p50)
	sum n_users_dist if tag_d, d
	g n_users_dist_med = r(p50)
	g high_activity = ( (n_users_dist > n_users_dist_med) & (n_trips_user > n_trips_user_med) )
	
	eststo: reghdfe sr c.dist_f_cum_km2##c.(mahrai st_share tree_cover_base_s high_activity) `ctrls', ///
			a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)

			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local clust "Biome"
	
	* 6. Clustering
	eststo: reghdfe sr c.dist_f_cum_km2##c.(mahrai st_share tree_cover_base_s) `ctrls', ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl c_code_1991_num)
	
			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local clust "District"
	
	* Table
	esttab using "${TABLE}/tables/hte_polecon.tex", replace ///
		keep(dist_f_cum_km2 c*) stats(user_y_fe dist_fe st_m_fe slx wt clust N r2, ///
		labels(`"User $\times$ Year FEs"' `"District FEs"' `"State x Month FEs"' ///
		`"Spillovers"' `"Weighted"' `"Clustering"' `"N"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 0 3)) nocons nonotes booktabs ///
		indicate("Baseline Forest Cover and Interactions=c.dist_*#c.tree_cover*" ///
				 "High-Activity eBird District and Interactions=c.dist_*#c.high*") ///
		nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) width(1\hsize)
	eststo clear
}

*-------------------------------------------------------------------------------
* MECHANISMS
*-------------------------------------------------------------------------------
if `project' == 1 {
	
	** Mechanism: Scheduled Area (doesn't work)
	/*
	* Read
	import delimited "${DATA}/csv/india_scheduled_areas.csv", clear
	
	ren (c_code_11 fifth_schedule) (c_code_2011 schedule)
	replace schedule = 1 if inlist(state_ut, "Assam", "Meghalaya", "Mizoram", "Tripura") // 6th schedule
	keep c_code_2011 schedule
	
	* Aggregate to 1991
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", keepusing(c_code_1991) keep(3) nogen
	collapse (mean) schedule, by(c_code_1991)
	replace schedule = round(schedule)
	
	* Merge to polecon
	merge 1:m c_code_1991 using "${DATA}/dta/polecon_91_v02", keep(3) nogen
	
	* Aggregate
	collapse (first) schedule mahrai state_code_1991_num lat alt coastal s*_share so_* totrain nbcluster, by(c_code_1991)
	
	reghdfe schedule mahrai lat alt coastal st_share so_* totrain, a(nbcluster) vce(cl state_code_1991_num)
	*/
	
	**# Mechanism: Project Review
	
	* Read project level
	use "${DATA}/dta/fc_pdym_s2_v02", clear
	
	* Get controls
	merge m:1 c_code_2011 year_month using "${DATA}/dta/fc_dym_s2_v02", keepus(tree_cover_base) keep(1 3) nogen
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", keepus(tot_area) keep(3) nogen
	g tree_cover_base_s = (tree_cover_base / tot_area)*100
	
	* Prep
	keep c_code_2011 prop_no proj_fra_num dist_f dist_nf cba_num year_month ///
		 proj_in_pa_esz proj_sched* tree_cover_base_s tot_area patches
	g year = year(dofm(year_month))
	g month = month(dofm(year_month))
	g log_patches = log(patches)
	g encroach = (dist_nf/(dist_f+dist_nf))*100
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", keepusing(c_code_1991) keep(3) nogen
	merge m:1 c_code_1991 year_month using "${DATA}/dta/polecon_91_v02", keep(3) nogen

	* Mechanisms
	local controls st_share tree_cover_base_s tot_area dist_f lat alt coastal
	foreach v of varlist proj_fra_num cba_num proj_in_pa_esz { // proj_sched*

		eststo: reghdfe `v' mahrai `controls' if britdum, ///
			a(state_code_1991_num#year_month) vce(cl state_code_1991_num)
			
			sum `v' if e(sample)==1
			estadd scalar ymean = `r(mean)'
			estadd local st_ym_fe "$\checkmark$"		
	}
	
	esttab using "${TABLE}/tables/mech_polecon.tex", replace ///
		keep(mahrai) stats(ymean st_ym_fe N r2, labels(`"Outcome Mean"' ///
		`"State $\times$ Time FEs"' `"N"' `"\(R^{2}\)"') fmt(3 0 0 3)) ///
		mlabel("Informed Consent" "Cost-Benefit" "Protected Area") ///
		indicate("Controls=st_share") wrap nocons nonotes booktabs ///
		nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) width(1\hsize)
	eststo clear

	/*
	**# Mechanism: Protests
	
	* Read protest panel
	use "${DATA}/dta/protests_2011.dta", clear

	* controls
	merge 1:m c_code_2011 year using "${DATA}/dta/fc_dym_s2_v02", keepus(tree_cover_base) keep(3) nogen
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", keepus(tot_area tot_pop) nogen
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", keep(3) nogen // 40 districts untracked
	
	* 1991 borders
	collapse (sum) tree_cover_base tot_pop tot_area n_*, ///
		by(c_code_1991 year) 

	* Merge to banerjee
	tempfile temp
	save "`temp'"
	
	use "${DATA}/dta/polecon_91_v02", clear
	collapse (lastnm) dist_f_cum_km2 tot_pop *_share mahrai *_num lat alt britdum, by(c_code_1991 year)
	merge 1:1 c_code_1991 year using "`temp'", nogen
	
	la var mahrai "Inclusive (=1)"
	g tree_cover_base_s = (tree_cover_base / tot_area)*100
	foreach v of varlist n_* {
		g ln_`v' = log(1+`v')
	}
	local controls n_protest st_share tree_cover_base_s tot_area dist_f_cum_km2 lat
	
	* Mechanisms
	* district clustering works without alt
	foreach v of varlist n_peaceful n_tribal {
	eststo: reghdfe ln_`v' mahrai `controls', ///
		a(state_code_1991_num#year) vce(cl state_code_1991_num)
		
		*sum `v' if e(sample)==1
		*estadd scalar ymean = `r(mean)'
		estadd local st_ym_fe "$\checkmark$"
	}
	*/
	
}




