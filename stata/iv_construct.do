*********************************************************************************
*																			
* PROJECT: 	DEFORESTATION-BIODIVERSITY	
*																									
* PURPOSE: 	CONSTRUCT IV	 	
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
gl READ	"/Volumes/Backup Plus/research/data/"
gl DATA "/Users/rmadhok/Dropbox/def_biodiv/data/"

// Module
local fra			0
local vil			0
local election		0
local benefits		1


*-------------------------------------------------------------------------------
* COMMUNITY CFR POTENTIAL (Lele et al. 2020)
*-------------------------------------------------------------------------------
if `fra' == 1 {

	*----------------
	* ASSEMBLE PANEL
	*----------------
	
	* Empty dataset
	clear
	tempfile cfr
	save `cfr', emptyok
	
	* Append
	foreach state in mp mh jh cg {
	
		* Read
		import excel "${DATA}/raw/`state'_cfr_potential_villages.xlsx", first all clear
		
		* Append
		append using `cfr'
		save `cfr', replace
		
	}
	
	* Clean
	ren *, lower
	ren totalgeographicalareaha area
	ren totalhouseholds n_hh
	ren totalpopulation tot_pop
	ren scpopulation sc_pop
	ren stpopulation st_pop
	ren villagetype vil_type
	ren cfrtype cfr_type
	ren forestinsiderevenueboundary f_rev
	ren potentialcfrareain2kmbuffe cfr_buf
	ren totalcfrpotentialhaforest cfr_tot
	replace cfr_tot = f_rev if state == "Chhattisgarh"
	destring f_rev cfr_buf cfr_tot st_pop sc_pop tot_pop, replace force
	
	* Rename to match census
	// CH
	replace district = "Dakshin Bastar Dantewada" if district == "Dantewada"
	replace district = "Janjgir - Champa" if district == "Janjgir-Champa"
	replace district = "Kabeerdham" if district == "Kabirdham"
	replace district = "Uttar Bastar Kanker" if district == "Kanker"
	// JH
	replace district = "Pakaur" if district == "Pakur"
	replace district = "Saraikela-Kharsawan" if district == "Saraikella-Kharsawan"
	// MP
	replace district = "East Nimar" if district == "Khandwa (East Nimar)"
	replace district = "West Nimar" if district == "Khargone (West Nimar)"
	// MH
	replace district = "Bid" if district == "Beed"
	replace district = "Gondiya" if district == "Gondia"
	replace district = "Nashik" if district == "Nasik"
	replace district = "Thane" if district == "Palghar"
	replace district = "Raigarh" if district == "Raigad"
	replace district = "Sangli" if district == "Sangali"
	
	*----------------
	* DISTRICT LEVEL
	*----------------
	
	* Census codes
	tempfile temp
	save "`temp'"
	
	import delimited "${DATA}/csv/2011_india_dist.csv", clear
	keep c_code_11 name state_ut 
	ren (name state_ut c_code_11) (district state c_code_2011)
	merge 1:m state district using "`temp'", keep(3) nogen
	
	* Aggregate (n=125 districts)
	collapse (sum) cfr_buf cfr_tot, by(c_code_2011)
	*g sc_share = sc_pop / tot_pop
	*g st_share = st_pop / tot_pop
	*drop sc_pop st_pop tot_pop
	
	* Label
	*la var f_rev "Revenue Forest Area (ha.)"
	la var cfr_buf "Potential CFR Area (ha.)"
	la var cfr_tot "Total CFR Area (ha.)"
	*la var sc_share "SC population share"
	*la var st_share "ST population share"
	
	* Save
	save "${DATA}/dta/iv_cfr", replace

}

*-------------------------------------------------------------------------------
* VILLAGE REVENUE FOREST AREA
*-------------------------------------------------------------------------------
if `vil' == 1 {
	
	* Construct village panel from raw files
	{
	/*
		set excelxlsxlargefile on
		
		* Empty dataset
		clear
		tempfile forest
		save `forest', emptyok
				
		* Add files to local
		cd "${READ}/def_biodiv/census/village_2011/"
		fs *.xlsx // list files
		
		* Iteratively append
		foreach file in `r(files)' {
			
			di "`file'"
			
			* read file
			import excel using "`file'", first case(lower) allstring clear
			
			* reduce
			keep statecode statename districtcode districtname ///
				villagecode forestarea* totalpop* totalscheduledcastesp* ///
				totalscheduledtribesp*
			
			* clean
			g c_code_2011 = "c" + statecode + districtcode
			drop districtcode
			order c_code_2011, first
			ren statecode state_code_2011
			ren villagecode village_code_2011
			ren totalpopulationofvillage tot_pop
			ren totalscheduledcastespopulatio sc_pop
			ren totalscheduledtribespopulatio st_pop
			ren forestareainhectares f_rev 
			destring *_pop f_rev, replace force
			
			la var st_pop "ST Population"
			la var sc_pop "SC population"
			la var f_rev "Revenue Forest Area (ha.)"
			
			* Append
			append using `forest'
			save `forest', replace
				 
		}

		* Save
		sort c_code_2011 village_code_2011
		save "${READ}/def_biodiv/census/vil_forest_area.dta"
	*/
	}

	* Read village panel (n=640,948 villages)
	use "${READ}/def_biodiv/census/vil_forest_area.dta", clear
	drop if village_code_2011 == ""
	
	* Construct
	g scst_pop = sc_pop + st_pop
	g f_rev_d = (f_rev > 10) // village has forest w/n revenue boundary
	replace f_rev_d = . if f_rev == .
	g f_rev_vil = f_rev * f_rev_d
	foreach v of varlist *_pop {
		g f_rev_`v' = (f_rev_d * `v')/100000 // population inside forest villages (in lakhs)
		g `v'_ha = (`v' / f_rev_vil)/1000 // population per ha. (if revenue forest)
	}

	* District level
	collapse (mean) f_vil = f_rev_d *_ha ///
			 (sum) *_pop f_rev=f_rev_vil, by(c_code_2011)

	* Construct
	foreach v of varlist st_pop sc_pop scst_pop tot_pop {
		g `v'_d_ha = `v' / f_rev // popln per ha of revenue forest
	}
	
	* Save
	save "${DATA}/dta/iv_fra", replace
	
}


*-------------------------------------------------------------------------------
* ST LEADERS
*-------------------------------------------------------------------------------
if `election' == 1 {
	
	* Read SHRUG assembly
	use "${READ}/shrug/shrug-v1.5.samosa-assembly-dta/shrug-v1.5.samosa-assembly-dta/assembly_elections_clean", clear
	
	* clean
	keep if year >= 2008
	drop if winner_type == ""
	
	* st winners
	g st_winner = (winner_type == "ST")
	replace st_winner = . if winner_type == ""
	replace st_winner = 1 if constituency_type == "ST"
	
	* sc winners
	g sc_winner = (winner_type == "SC")
	replace sc_winner = . if winner_type == ""
	replace sc_winner = 1 if constituency_type == "SC"
	
	* Reserved 
	g r_seats = st_winner | sc_winner
	replace r_seats = . if st_winner == . | sc_winner == .
	
	* District level
	drop if pc01_district_id == "" // drops 15% of obs
	g c_code_2001 = "c" + pc01_state_id + pc01_district_id
	collapse (mean) st_seats=st_winner sc_seats=sc_winner r_seats, ///
		by(c_code_2001 year)
	
	* Identify unchanging district borders
	tempfile temp
	save "`temp'"
	import delimited "${DATA}/csv/c_code_crosswalk.csv", varn(1) clear
	bys c_code_2001: g splits = _N
	keep if splits == 1 // N=523 districts
	drop splits
	merge 1:m c_code_2001 using "`temp'", keep(3) nogen // N=496 matching districts
	drop c_code_2001
	
	* Balance
	encode c_code_2011, gen(c_code_2011_num)
	drop c_code_2011
	tsset c_code_2011_num year
	tsfill, full
	bys c_code_2011_num: carryforward *_seats, replace
	decode c_code_2011_num, gen(c_code_2011)
	drop c_code_2011_num
	
	* Save
	keep if year >=2014
	order c_code_2011, first
	save "${DATA}/dta/iv_scst_seats", replace

}

*-------------------------------------------------------------------------------
* MERGE FILES
*-------------------------------------------------------------------------------
if `benefits' == 1 {

	*----------------------------
	* PREDICTED STATE INTRUSIONS
	*----------------------------
	
	* Read FC
	use "${DATA}/dta/fc_dym_s2_v02", clear

	* Reduce
	keep *_code_2011* year year_month month dist_f*cum_km2 dist_nf*cum_km2 // n_*cum
	
	* 2015 State Fractions
	preserve
	
		* Pre-period 
		keep if year == 2015
		keep c_code_2011 state_code_2011 year_month dist_f_cum_km2 // n_patches_cum
		
		* State cumulatives
		bys state_code_2011 year_month: egen st_f_cum_km2 = total(dist_f_cum_km2)
		*bys state_code_2011 year_month: egen st_patches_cum = total(n_patches_cum)
		bys state_code_2011 year_month: keep if _n == 1
		drop dist_f_cum_km2 c_code_2011 // n_patches_cum
		
		* National deforestation
		bys year_month: egen nat_f_cum_km2 = total(st_f_cum_km2)
		*bys year_month: egen nat_patches_cum = total(st_patches_cum)
		
		* Collapse
		sort state_code_2011 year_month
		collapse (lastnm) st_f* nat_f*, by(state_code_2011)
		
		* Share
		g st_f_share = st_f_cum_km2 / nat_f_cum_km2
		*g st_patches_share = st_patches_cum / nat_patches_cum
		
		* Save
		keep state_code_2011 *_share
		tempfile temp 
		save "`temp'"
	
	restore
	
	* Merge state shares
	merge m:1 state_code_2011 using "`temp'", nogen
	keep if year >= 2016
	
	* Predicted state deforestation
	bys year_month: egen nat_f_cum_km2 = total(dist_f_cum_km2)
	*bys year_month: egen nat_patches_cum = total(n_patches_cum)
	g st_f_cum_km2_p = st_f_share * nat_f_cum_km2
	*g st_patches_cum_p = st_patches_share * nat_patches_cum
	sort c_code_2011 year_month
	
	*-------------------------
	* MERGE TO VILLAGE CENSUS
	*-------------------------
	
	* Merge
	merge m:1 c_code_2011 using "${DATA}/dta/iv_fra", nogen
	sort c_code_2011 year_month
	
	* IV
	foreach v of varlist f_vil-scst_pop_ha f_rev_tot_pop-tot_pop_d_ha {
		
		// state def'n X fra
		g iv_`v' = `v' * st_f_cum_km2_p 
		*g p_`v'_iv = `v' * st_patches_cum_p
	}
	
	*---------------------------
	* MERGE TO ATREE CFR
	*---------------------------
	
	* Merge
	merge m:1 c_code_2011 using "${DATA}/dta/iv_cfr", nogen
	
	* construct
	foreach v of varlist sc_pop st_pop scst_pop {
		g `v'_cfr_ha = (`v' / cfr_tot)/1000
	}
	
	* IV
	foreach v of varlist *_cfr_ha {
		
		g iv_`v' = `v' * st_f_cum_km2_p
	}
	
	*---------------------------
	* MERGE TO ELECTION LEADERS
	*---------------------------
	
	* Merge
	merge m:1 c_code_2011 year using "${DATA}/dta/iv_scst_seats", keep(1 3) nogen
	
	* IV (double interactions)
	foreach v of varlist st_seats sc_seats r_seats {
		
		// state def'n X seats
		g iv_`v' = `v' * st_f_cum_km2_p
		*g iv_p_`v' = `v' * st_patches_cum_p
	}
	
	* IV (triple interactions)
	foreach i of varlist f_vil-scst_pop_ha f_rev_tot_pop-tot_pop_d_ha sc_pop_cfr_ha-scst_pop_cfr_ha {
		foreach j of varlist st_seats sc_seats r_seats {
			
			// fra X seats
			g `i'_`j' = `i' * `j' 
			
			// state def'n X fra X seats
			g iv_`i'_`j' = `i' * `j' * st_f_cum_km2_p
			*g `i'_`j'_iv = `i' * `j' * st_patches_cum_p
		}
	}
	
	*---------------------------
	* NIGHTLIGHTS
	*---------------------------
	
	tempfile temp2
	save "`temp2'"
	
	* Read
	import delimited "${DATA}/csv/india_nightlights.csv", clear
	
	* Date
	g year_month = ym(year(date(yearmonth, "20YM")), month(date(yearmonth, "20YM")))
	format year_month %tmCCYY-NN
	drop yearmonth
	
	* Merge
	merge 1:1 c_code_2011 year_month using "`temp2'", keep(3) nogen
	
	*---------------------------
	* FINAL
	*---------------------------
	
	* Covariates
	g scst_pop_p = (scst_pop/tot_pop) * st_f_cum_km2_p
	g st_pop_p = (st_pop/tot_pop) * st_f_cum_km2_p
	g sc_pop_p = (sc_pop/tot_pop) * st_f_cum_km2_p
	
	* Save
	save "${DATA}/dta/benefits_iv_v02", replace

}


