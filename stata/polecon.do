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
gl TABLE	"${ROOT}/docs/jmp/tex_doc/v5"
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
			 (first) year month biome user_id, by(uid c_code_1991 year_month) 
	
	* Merge to banerjee
	merge m:1 c_code_1991 year_month using "${DATA}/dta/polecon_91_v02", keep(1 3) nogen
	
	* Prep
	drop if year == 2014
	drop_outliers
	foreach v of varlist exp_idx duration group_size distance rad_* {
		g ln_`v' = ln(1+`v')
	}
	g ln_coverage_udym = ln(coverage_udym)
	g tree_cover_base_s = (tree_cover_base / tot_area)*100
	local ctrls temp rain ln_duration ln_distance ln_rad_sum tree_cover_s ///
	        ln_exp_idx ln_coverage_udym ln_group_size traveling
	
	la var dist_f_cum_km2 "Infrastructure (\(km^{2}\))"
	la var st_share "Tribal Pop. Share"
	la var tree_cover_base_s "Baseline Forest Cover"
	
	**# Heterogeneity
	/*
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
		`"Spillovers"' `"Weighted"' `"Clustering"' `"Observations"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 0 3)) nocons nonotes booktabs ///
		indicate("Baseline Forest Cover and Interactions=c.dist_*#c.tree_cover*" ///
				 "High-Activity eBird District and Interactions=c.dist_*#c.high*") ///
		nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) width(1\hsize)
	eststo clear
	*/
	
	**# Heterogeneity: Robustness
	
	*1. User-Month FEs
	la var dist_f_cum_km2 "Infrastructure"
	eststo: reghdfe sr c.dist_f_cum_km2##c.(mahrai st_share tree_cover_base_s) `ctrls', ///
		a(uid#month c_code_1991_num state_code_1991_num#year) vce(cl biome)

			estadd local user_m_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_y_fe "$\checkmark$"
			estadd local clust "Biome"
			estadd local samp "None"
			estadd local unit "\(km^{2}\)"
	
	*2. District-Month FEs
	eststo: reghdfe sr c.dist_f_cum_km2##c.(mahrai st_share tree_cover_base_s) `ctrls', ///
		a(uid#year c_code_1991_num#month) vce(cl biome)

			estadd local user_y_fe "$\checkmark$"
			estadd local dist_m_fe "$\checkmark$"
			estadd local clust "Biome"
			estadd local samp "None"
			estadd local unit "\(km^{2}\)"
	
	*3. Experience FEs
	eststo: reghdfe sr c.dist_f_cum_km2##c.(mahrai st_share tree_cover_base_s) `ctrls', ///
		a(uid#year c_code_1991_num state_code_1991_num#month exp_idx) vce(cl biome)

			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local exp_fe "$\checkmark$"
			estadd local clust "Biome"
			estadd local samp "None"
			estadd local unit "\(km^{2}\)"
	
	*4. Time of Day FEs
	gen time = "morning" if inrange(hour, 6, 12)
	replace time = "afternoon" if inrange(hour, 1, 18)
	replace time = "evening" if inrange(hour, 19, 24) 
	replace time = "night" if inrange(hour, 1, 5)
	encode time, gen(time_code)
	eststo: reghdfe sr c.dist_f_cum_km2##c.(mahrai st_share tree_cover_base_s) `ctrls', ///
		a(uid#year c_code_1991_num state_code_1991_num#month time_code) vce(cl biome)

			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local time_fe "$\checkmark$"
			estadd local clust "Biome"
			estadd local samp "None"
			estadd local unit "\(km^{2}\)"
	
	*5. Sample: Users from 2015
	preserve
			
		keep if year == 2015
		keep uid
		duplicates drop uid, force
		tempfile u2015
		save "`u2015'"
	restore

	preserve
		merge m:1 uid using "`u2015'", keep(3) nogen
		eststo: reghdfe sr c.dist_f_cum_km2##c.(mahrai st_share tree_cover_base_s) `ctrls', ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)

			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local clust "Biome"
			estadd local samp "2015 users"
			estadd local unit "\(km^{2}\)"
	restore
	
	*6. Sample: Non Covid
	eststo: reghdfe sr c.dist_f_cum_km2##c.(mahrai st_share tree_cover_base_s) `ctrls' if inrange(year, 2015, 2019), ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)

			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local clust "Biome"
			estadd local samp "Non-COVID"
			estadd local unit "\(km^{2}\)"
			
	*7. Outliers: Drop mega projects
	preserve
		
		use "${DATA}/dta/fc_ebd_udt_trunc_v02", clear
		merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", keep(3) nogen // 40 districts untracked

		* Aggregate to 1991 border
		collapse (mean) temp rain duration distance exp_idx coverage_udym ///
					traveling group_size rad_sum sr tree_cover_s hour n_fra_cum_s ///
				 (sum) n_trips n_trips_user n_users_dist tot_area tree_cover_base dist_f_cum_km2_slx_ib ///
				 (first) year month biome user_id, by(uid c_code_1991 year_month) 
		* Merge to banerjee
		merge m:1 c_code_1991 year_month using "${DATA}/dta/polecon_91_v02", keep(1 3) nogen
		* Prep
		drop if year == 2014
		drop_outliers
		foreach v of varlist exp_idx duration group_size distance rad_* {
			g ln_`v' = ln(1+`v')
		}
		g ln_coverage_udym = ln(coverage_udym)
		g tree_cover_base_s = (tree_cover_base / tot_area)*100
		
		eststo: reghdfe sr c.dist_f_cum_km2##c.(mahrai st_share tree_cover_base_s) `ctrls', ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)

			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local clust "Biome"
			estadd local samp "Truncate"
			estadd local unit "\(km^{2}\)"
	restore
	
	*8. Outliers: IHS
	preserve
		g dist_f_cum_ihs = asinh(dist_f_cum_km2)
		drop dist_f_cum_km2
		ren dist_f_cum_ihs dist_f_cum_km2
		eststo: reghdfe sr c.dist_f_cum_km2##c.(mahrai st_share tree_cover_base_s) `ctrls', ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl biome)

			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local clust "Biome"
			estadd local samp "None"
			estadd local unit "IHS"
	restore
	
	*9. Clustering: State
	eststo: reghdfe sr c.dist_f_cum_km2##c.(mahrai st_share tree_cover_base_s) `ctrls', ///
		a(uid#year c_code_1991_num state_code_1991_num#month) vce(cl state_code_1991_num)

			estadd local user_y_fe "$\checkmark$"
			estadd local dist_fe "$\checkmark$"
			estadd local st_m_fe "$\checkmark$"
			estadd local clust "State"
			estadd local samp "None"
			estadd local unit "\(km^{2}\)"
	
	esttab using "${TABLE}/tables/hte_polecon_robust.tex", replace ///
		keep(dist_f_cum_km2 c*) stats(unit samp user_m_fe user_y_fe dist_fe dist_m_fe ///
		st_m_fe st_y_fe exp_fe time_fe clust N r2, labels(`"Unit"' ///
		`"Sample Restriction"' `"User $\times$ Month FEs"' `"User $\times$ Year FEs"' ///
		`"District FEs"' `"District $\times$ Month FEs"' `"State x Month FEs"' ///
		`"State $\times$ Year Fes"' `"Experience FEs"' `"Time-of-day FEs"' ///
		`"Clustering"' `"Observations"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 0 0 0 0 0 0 3)) nocons nonotes booktabs ///
		indicate("Baseline Forest Cover + Interactions=c.dist_*#c.tree_cover*") ///
		nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) width(1\hsize) wrap
	eststo clear

}

*-------------------------------------------------------------------------------
* MECHANISMS
*-------------------------------------------------------------------------------
if `project' == 1 {
	
	* Read project level
	use "${DATA}/dta/fc_pdym_s2_v02", clear
	
	* Get controls
	merge m:1 c_code_2011 year_month using "${DATA}/dta/fc_dym_s2_v02", keepus(tree_cover_base) keep(1 3) nogen
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", keepus(tot_area) keep(3) nogen
	g tree_cover_base_s = (tree_cover_base / tot_area)*100
	
	* Prep
	keep c_code_2011 prop_no proj_fra_num proj_cat dist_f dist_nf cba_num ///
		year_month proj_in_pa_esz proj_sched* tree_cover_base_s tot_area patches
	g year = year(dofm(year_month))
	g month = month(dofm(year_month))
	merge m:1 c_code_2011 using "${DATA}/dta/crosswalk_full", keepusing(c_code_1991) keep(3) nogen
	merge m:1 c_code_1991 year_month using "${DATA}/dta/polecon_91_v02", keep(3) keepus(mahrai britdum *_1991_num st_share lat alt coastal) nogen
	local controls st_share tree_cover_base_s tot_area dist_f lat alt coastal
	
	/*
	**# Mechanisms: All Projects
	foreach v of varlist proj_fra_num cba_num proj_in_pa_esz_num {

		eststo `v': reghdfe `v' mahrai `controls' if britdum == 1, ///
			a(state_code_1991_num#year_month) vce(cl state_code_1991_num)
			
			sum `v' if e(sample)==1
			estadd scalar ymean = `r(mean)'
			estadd local st_ym_fe "$\checkmark$"		
	}
	
	* Bar Plot
	coefplot (proj_fra_num, rename(mahrai="Informed Consent") \ ///
		cba_num, rename(mahrai="Cost-Benefit") \ ///
		proj_in_pa_esz_num, rename(mahrai="Protected Area")), ///
		keep(mahrai) vertical recast(bar) bcolor(dkgreen*0.7) barwidth(0.5) ///
		fcolor(*.7) ciopts(recast(rcap) color(black) lcolor(*.7) ///
		lpattern(dash)) citop format(%9.2f) yline(0, lcolor(black*0.8) ///
		lpattern(solid)) coeflabels(, labsize(large)) ///
		addplot(scatter @b @at, ms(i) mlabel(@b) mlabpos(2) mlabcolor(black) ///
		mlabsize(large)) ylabel(, labsize(large)) ///
		ytitle("Coefficient (Share of projects" "in inclusive districts)", ///
		size(large)) ysize(2.7)
	graph export  "${TABLE}/fig/mech_polecon.png", replace
	
	* Table
	esttab using "${TABLE}/tables/mech_polecon.tex", replace ///
		keep(mahrai) stats(ymean st_ym_fe N r2, labels(`"Outcome Mean"' ///
		`"State $\times$ Time FEs"' `"N"' `"\(R^{2}\)"') fmt(3 0 0 3)) ///
		mlabel("Informed Consent" "Cost-Benefit" "Protected Area") ///
		indicate("Controls=st_share") wrap nocons nonotes booktabs ///
		nomtitles star(* .1 ** .05 *** .01) label se b(%5.3f) width(1\hsize)
	eststo clear
	*/
	
	
	**# Project Distribution by Institutuional Type
	tab proj_cat, gen(cat_)
	la var cat_1 "Electricity"
	la var cat_2 "Irrigation" 
	la var cat_3 "Mining"
	la var cat_4 "Other"
	la var cat_5 "Resettlement"
	la var cat_6 "Transportation"
	
	* Electricity (cat 1)
	la var mahrai "Electricity"
	eststo: reg cat_1 mahrai, vce(cl state_code_1991_num)
	eststo: reg cat_1 mahrai `controls', vce(cl state_code_1991_num)
	eststo: reghdfe cat_1 mahrai `controls', a(state_code_1991_num)
	eststo: reghdfe cat_1 mahrai `controls', a(state_code_1991_num year_month)
	*eststo: reghdfe cat_1 mahrai `controls', a(state_code_1991_num#year_month)
	
	esttab using "${TABLE}/tables/polecon_cat.tex", f replace ///
		keep(mahrai) mlabel("No Controls or FEs" "Controls" "State FEs" "State + Year FEs") ///
		wrap nocons nonotes booktabs nomtitles noobs star(* .1 ** .05 *** .01) ///
		label se b(%5.3f) width(1\hsize)
	eststo clear
	
	foreach v of varlist cat_2-cat_6 {
		
		local name: variable label `v'
		la var mahrai `name'
		
		eststo: reg `v' mahrai, vce(cl state_code_1991_num)
		eststo: reg `v' mahrai `controls', vce(cl state_code_1991_num)
		eststo: reghdfe `v' mahrai `controls', a(state_code_1991_num)
		eststo: reghdfe `v' mahrai `controls', a(state_code_1991_num year_month)
		*eststo: reghdfe `v' mahrai `controls', a(state_code_1991_num#year_month)
			
		esttab using "${TABLE}/tables/polecon_cat.tex", f append ///
			keep(mahrai) wrap nocons nonotes booktabs nomtitles noobs plain ///
			star(* .1 ** .05 *** .01) label nonumbers collabels(none) se b(%5.3f) width(1\hsize)
			
	eststo clear
	}
	
}
	
			
	
