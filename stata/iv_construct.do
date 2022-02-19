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
local landlord		1
local shiftshare	0

*-------------------------------------------------------------------------------
* COMMUNITY CFR POTENTIAL (Lele et al. 2020)
*-------------------------------------------------------------------------------
if `fra' == 1 {

	**# Assemble Panel
	*------------------------
	
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
	ren scpopulation sc
	ren stpopulation st
	ren forestinsiderevenueboundary f_rev
	ren potentialcfrareain2kmbuffe cfr_buf
	ren totalcfrpotentialhaforest cfr_tot
	replace cfr_tot = f_rev if state == "Chhattisgarh"
	destring f_rev cfr_buf cfr_tot st sc, replace force
	
	* Population per ha
	g scst = sc + st
	foreach v of varlist sc st scst {
		g `v'_cfr_ha = (`v' / cfr_tot)/1000
	}
	
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
	
	**# District Level
	*-------------------------
	
	* Aggregate (n=125 districts)
	collapse (mean) *_cfr_ha, by(state district)
	*collapse (sum) cfr_buf cfr_tot, by(state district) // USE MEAN?
	tempfile temp
	save "`temp'"
	
	* Census codes
	import delimited "${DATA}/csv/2011_india_dist.csv", clear
	keep c_code_11 name state_ut 
	ren (name state_ut c_code_11) (district state c_code_2011)
	merge 1:1 state district using "`temp'", keep(3) nogen
	drop state district
	
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

	* Read villages (n=640,948 villages; no urban)
	use "${READ}/def_biodiv/census/vil_forest_area.dta", clear
	drop if village_code_2011 == ""
	ren (sc_pop st_pop) (sc st)

	* Construct (village level)
	g scst = sc + st // dalit population
	g f_rev_d = (f_rev > 10) // habitable forest w/n vil revenue boundary
	replace f_rev_d = . if f_rev == .
	g f_rev_vil = f_rev * f_rev_d
	foreach v of varlist sc st scst {
		g `v'_ha = 0 if `v' != .
		replace `v'_ha = (`v' / f_rev_vil)/1000 if f_rev_vil > 0 & f_rev_vil < . // population per ha. (if revenue forest)
	}

	* District level (n=631 districts; remaining have no villages)
	collapse (mean) *_ha (sum) f_rev=f_rev_vil, by(c_code_2011)

	la var st_ha "ST/ha."
	la var sc_ha "SC/ha."
	la var scst_ha "Dalits/ha."
	
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

	* reserved seats
	g st_seats = (constituency_type == "ST") // ST reservations
	replace st_seats = . if constituency_type == ""
	g sc_seats = (constituency_type == "SC") // SC reservations
	replace sc_seats = . if constituency_type == ""
	g scst_seats = st_seats | sc_seats // dalit reservations
	replace scst_seats = . if st_seats == . | sc_seats == .
	
	* District level
	drop if pc01_district_id == "" // drops 15% of obs
	g c_code_2001 = "c" + pc01_state_id + pc01_district_id
	collapse (mean) st_seats sc_seats scst_seats, by(c_code_2001 year)
	la var st_seats "ST seat share"
	la var sc_seats "SC seat share"
	la var scst_seats "Dalit seat share"
	
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
* LANDLORD DISTRICTS (Banerjee and Iyer 2005)
*-------------------------------------------------------------------------------
if `landlord' == 1 {
	
	**# Construct 91-01-11 crosswalk

	* Read from SHRUG
	foreach year in 91 01 11 {
	
		use "${READ}/shrug/shrug-v1.5.samosa-pop-econ-census-dta/shrug-v1.5.samosa-keys-dta/shrug_pc`year'_district_key", clear
		g c_code_`year' = "c" + pc`year'_state_id + pc`year'_district_id
		keep shrid c_code_`year' pc`year'_*_name
		tempfile key_`year'
		save "`key_`year''"
	}

	* Merge
	use "`key_11'", clear
	merge 1:1 shrid using "`key_01'", keep(3) nogen
	merge 1:1 shrid using "`key_91'", keep(3) nogen
	duplicates drop c_code_11, force
	keep if length(c_code_11) == 6
	drop shrid
	tempfile crosswalk
	save "`crosswalk'"
	
	**# Banerjee Data

	* Read
	use "${READ}/def_biodiv/banerjee_iyer/yld_sett_aug03", clear
	
	* Prep
	collapse (firstnm) p_nland mahrai state, by(dist_91)
	
	replace dist_91 = lower(dist_91)
	replace state = lower(state)
	replace dist_91 = "north twenty four parganas" if dist_91 == "24 parganas(north)"
	replace dist_91 = "banas kantha" if dist_91 == "banaskantha"
	replace dist_91 = "bathinda" if dist_91 == "bhatinda"
	replace dist_91 = "bid" if dist_91 == "bir"
	replace dist_91 = "chikmagalur" if dist_91 == "chikmangalur"
	replace dist_91 = "the dangs" if dist_91 == "dangs"
	replace dist_91 = "hugli" if dist_91 == "hooghly"
	replace dist_91 = "haora" if dist_91 == "howrah"
	replace dist_91 = "raigarh" if dist_91 == "kolaba"
	replace dist_91 = "maldah" if dist_91 == "malda"
	replace dist_91 = "nashik" if dist_91 == "nasik"
	replace dist_91 = "nilgiri" if dist_91 == "nilgiris"
	replace dist_91 = "north arcot ambedker" if dist_91 == "north arcot" 
	replace dist_91 = "palamu" if dist_91 == "palamau"
	replace dist_91 = "panch mahals" if dist_91 == "panchmahals"
	replace dist_91 = "pashchimi singhbhum" if dist_91 == "pashchim singhbhum"
	replace dist_91 = "phulabani" if dist_91 == "phulbani"
	replace dist_91 = "sabar kantha" if dist_91 == "sabarkantha"
	replace dist_91 = "solapur" if dist_91 == "sholapur"
	replace dist_91 = "sundargarh" if dist_91 == "sundergarh"
	replace dist_91 = "tiruchchirappalli" if dist_91 == "tiruchirapalli"
	replace dist_91 = "tirunelveli kattabomman" if dist_91 == "tirunelveli"
	replace dist_91 = "visakhapatnam" if dist_91 == "vishakhapatnam"
	ren (dist_91 state) (pc91_district_name pc91_state_name)
	
	* Merge
	merge 1:m pc91_district_name pc91_state_name using "`crosswalk'", keep(3) nogen
	
	* Save
	save "${DATA}/dta/iv_landlord", replace

}

*-------------------------------------------------------------------------------
* MERGE FILES
*-------------------------------------------------------------------------------
if `shiftshare' == 1 {

	**# Predicted State Intrusions
	*-----------------------------------------------------
	
	* Read FC
	use "${DATA}/dta/fc_dym_s2_v02", clear
	*drop if year == 2014
	
	* Reduce
	keep *_code_2011* year year_month month dist_f_cum_km2 tree_cover_base

	* 2014 State Fractions
	preserve
	
		* Pre-period 
		keep if year == 2014
		keep c_code_2011 state_code_2011 year_month dist_f_cum_km2
		
		* Cumulatives
		bys state_code_2011 year_month: egen st_f_cum_km2 = total(dist_f_cum_km2) 
		bys state_code_2011 year_month: keep if _n == 1 // state-monthly cumulative
		drop dist_f_cum_km2 c_code_2011
		bys year_month: egen nat_f_cum_km2 = total(st_f_cum_km2) // national monthly cumulative
		
		* Collapse to end-of-year
		sort state_code_2011 year_month
		collapse (lastnm) st_f* nat_f*, by(state_code_2011)
		
		* Share
		g st_f_share = st_f_cum_km2 / nat_f_cum_km2
		keep state_code_2011 *_share
		tempfile shares 
		save "`shares'"
	
	restore
	
	* Merge state shares
	merge m:1 state_code_2011 using "`shares'", nogen
	*keep if year >= 2016
	
	* Predicted state deforestation
	bys year_month: egen nat_f_cum_km2 = total(dist_f_cum_km2)
	g st_f_cum_km2_p = st_f_share * nat_f_cum_km2
	la var st_f_cum_km2_p "State Approvals"
	sort c_code_2011 year_month

	**# Merge Political Economy
	*-------------------------------------------------------
	
	* FRA
	merge m:1 c_code_2011 using "${DATA}/dta/iv_fra", nogen
	sort c_code_2011 year_month
	
	* ATREE CFR
	merge m:1 c_code_2011 using "${DATA}/dta/iv_cfr", nogen
	
	* Scheduled Areas
	tempfile temp
	save "`temp'"
	import delimited "${DATA}/csv/india_scheduled_areas.csv", clear
	ren (c_code_11 fifth_schedule) (c_code_2011 schedule)
	replace schedule = 1 if inlist(state_ut, "Assam", "Meghalaya", "Mizoram", "Tripura") // 6th schedule
	keep c_code_2011 schedule
	merge 1:m c_code_2011 using "`temp'", nogen
	order schedule, last
	la var schedule "Scheduled Area"
	
	* Election leaders/reservations
	merge m:1 c_code_2011 year using "${DATA}/dta/iv_scst_seats", keep(1 3) nogen
	
	* State Population Shares
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", keepusing(tot_st tot_sc tot_pop tot_area) nogen
	ren	(tot_st tot_sc) (st sc)
	g scst = sc + st
	foreach v of varlist sc st scst {
		bys state_code_2011: egen `v'_state = total(`v'), m
		g `v'_state_share = `v'/`v'_state
	}
	
	*---------------------------
	* NIGHTLIGHTS
	*---------------------------
	/*
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
	*/
	
	* Covariates
	g p_scst = (scst/tot_pop) * st_f_cum_km2_p
	g p_st = (st/tot_pop) * st_f_cum_km2_p
	g p_sc = (sc/tot_pop) * st_f_cum_km2_p
	g p_tcover = (tree_cover_base/tot_area) * st_f_cum_km2_p 
	g p_st_share = st_state_share * st_f_cum_km2_p
	g p_sc_share = sc_state_share * st_f_cum_km2_p
	g p_scst_share = scst_state_share * st_f_cum_km2_p

	* Save
	sort c_code_2011 year_month
	save "${DATA}/dta/polecon_v02", replace

}


