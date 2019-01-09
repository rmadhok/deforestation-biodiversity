*********************************************************************************
*																			
* PROJECT: 	Deforestation and Biodiversity										
*																			
* PURPOSE: Construct Master Dataset		    
*																			
* START DATE: Dec 30, 2018       				 							
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
gl ROOT 	"/Users/rmadhok/Documents/ubc/research"
gl DATA 	"${ROOT}/def_biodiv/data"

// Modules
local biodiversity 		1
local forest_codes		1

// Settings
* Percent random sample of eBird population (set to 5 or 10%)
local sampsize			5

*===============================================================================
* BIODIVERSITY INDICATORS
*===============================================================================
if `biodiversity' == 1 {

	//1. Organize Data
	
	** Read data
	import delimited using "${DATA}/csv/ebird_sample_`sampsize'.csv", clear
	
	** Rename
	ren (taxonomicorder category commonname scientificname ///
		 subspeciescommonname subspeciesscientificname observationcount ///
		 localitytype observationdate observerid samplingeventidentifier ///
		 protocoltype protocolcode durationminutes effortdistancekm ///
		 effortareaha numberobservers groupidentifier) ///
		(tax_order category name name_sci name_subspecies name_subspecies_sci ///
		count locality_type date_obs id id_event protocol_type protocol_code ///
		duration_min effort_distance_km effort_area_ha num_observers id_group)
	
	** Sort and Order
	sort c_code_2011 id id_event id_group
	order c_code_2011 year date_obs id id_event  ///
		  id_group tax_order count name latitude longitude

	** Missing Data
	destring count, replace force 
	foreach v of varlist category-num_observers{
		replace `v' = "" if `v' == "NA"
		destring `v', replace
		}
	
	//2. Prepare eBird (Sullivan et al, 2014)
	
	** Drop duplicates from group events
	bys id_group tax_order: keep if _n == 1
	
	** Birdwatching duration (Callaghan et al 2018)
	keep if duration_min >= 5 & duration_min <= 240
	
	** Drop incomplete checklists
	bys id_event: egen num_check = count(count)
	drop if num_check == 0
	drop num_check
	
	//3. Diversity Indices
	
	** Species Richness
	bys id_event: gen species_richness = _N
	la var species_richness "Species Richness"
	
	** Shannon Diversity Index
	bys id_event: egen sum = total(count), mi
	gen p = count / sum
	gen plnp = p*ln(p)
	bys id_event: egen sum_plnp = total(plnp)
	gen shannon_index = -sum_plnp
	drop sum p plnp sum_plnp	
	la var shannon_index "Shannon Diversity Index"
	
	
	//4. Aggregate
	
	** Format date
	gen date = date(date_obs, "20YMD")
	gen month = month(date)
	gen year_month = ym(year, month)
	format year_month %tmCCYY-NN

	** Aggregate to district
	collapse (mean) species_richness shannon_index, ///
			 by(c_code_2011 year_month)

	//5. Balance Panel
	
	** Merge with Census 
	merge m:1 c_code_2011 using "${DATA}/dta/dist_census_codes"
	
	** Balance Panel
	encode c_code_2011, gen(c_code_2011_num)
	tsset c_code_2011_num year_month
	sort year_month 
	gen ym = year_month[1]
	replace year_month = ym if _m == 2
	tsfill, full

	** Clean
	drop c_code_2011 district state _merge ym
	decode c_code_2011_num, gen(c_code_2011)
	
	** Get state/district strings
	merge m:1 c_code_2011 using "${DATA}/dta/dist_census_codes", ///
		keepus(state district) nogen
		
	** Write
	order c_code_2011* state district year_month species* shannon* 
	sort c_code_2011 year_month
	export delimited using "${DATA}/csv/ebird_dist_biodiv_`sampsize'.csv", replace
	save "${DATA}/dta/ebird_dist_biodiv_`sampsize'.dta", replace
	
}

*===============================================================================
* PREPARE FOREST CLEARANCE
*===============================================================================
if `forest_codes' == 1 {
	
	//1. Organize project-level
	
	** Read
	use "${DATA}/dta/fc_clean.dta", clear
	drop village*
	
	** Construction Started
	replace prop_status = trim(itrim(lower(prop_status)))
	keep if regexm(prop_status, "approved") | ///
			regexm(prop_status, "pending") | ///
			regexm(prop_status, "in-principle") | ///
			regexm(prop_status, "recommended") 
			
	** Format Date
	gen year = year(date_rec)
	gen month = month(date_rec)
	gen year_month = ym(year, month)
	format year_month %tmCCYY-NN
	keep if !mi(year_month)
	
	//2. Project-District Level
	
	** Reshape
	reshape long district_ district_forest_ district_nonforest_ ///
		, i(prop_no) j(dist_id)
	
	** Clean
	ren (district_ district_forest_ district_nonforest_) ///
		(district district_forest district_nonforest)
	drop if district == ""
	
	//3. Merge Census Codes
	
	** Manual Sync Names
	
	* States
	replace state = "andaman & nicobar islands" if state == "andaman and nicobar"
	replace state = "puducherry" if state == "pondicherry"
	replace state = "dadra & nagar haveli" if state == "dadar & nagar haveli"
	replace state = "daman & diu" if state == "daman and diu"
	replace state = "tamilnadu" if state == "tamil nadu"
	replace state = "odisha" if state == "orissa"
	
	* Districts
	replace district = "nicobars" if district == "nicobar"
	replace district = "north & middle andaman" if district == "north and middle andaman"
	
	replace district = "y.s.r" if (state == "andhra pradesh" & district == "kadapa") | (state == "andhra pradesh" & district == "cuddapah")
	replace district = "sri potti sriramulu nellore" if state == "andhra pradesh" & district == "nellore"
	
	replace district = "balemu east kameng" if state == "arunachal pradesh" & district == "east kameng"
	
	replace district = "tinsukia" if state == "assam" & district == "digboi"
	replace district = "karbi anglong" if state == "assam" & district == "diphu"
	replace district = "dima hasao" if state == "assam" & district == "north cachar hills"
	
	replace district = "kaimur (bhabua)" if state == "bihar" & district == "kaimur"
	replace district = "purba champaran" if state == "bihar" & district == "east champaran"
	
	replace district = "dakshin bastar dantewada" if state == "chhattisgarh" & district == "dantewada"
	replace district = "kabeerdham" if state == "chhattisgarh" & district == "kabirdham"
	replace district = "uttar bastar kanker" if state == "chhattisgarh" & district == "kanker"
	
	replace district = "dadra & nagar haveli" if district == "dadra and nagar haveli"
	
	replace district = "ahmadabad" if state == "gujarat" & district == "ahmedabad"
	replace district = "banas kantha" if state == "gujarat" & district == "banaskantha"
	replace district = "dohad" if state == "gujarat" & district == "dahod"
	replace district = "the dangs" if state == "gujarat" & district == "dang"
	replace district = "junagadh" if state == "gujarat" & district == "junagarh"
	replace district = "kachchh" if state == "gujarat" & district == "kutch"
	replace district = "panch mahals" if state == "gujarat" & district == "panchmahal"
	
	replace district = "kangra" if state == "himachal pradesh" & district == "dharmshala"
	replace district = "lahul & spiti" if district == "lahul and spiti"
	
	replace district = "purbi singhbhum" if state == "jharkhand" & district == "east singhbhum"
	replace district = "hazaribagh" if state == "jharkhand" & district == "hazaribag"
	replace district = "pashchimi singhbhum" if state == "jharkhand" & district == "west singhbhum"
	replace district = "saraikela-kharsawan" if (state == "jharkhand" & district == "seraikela") | (state == "jharkhand" & district == "seraikela kharsawan")
	
	replace district = "bangalore" if state == "karnataka" & district == "bangalore urban"
	replace district = "dakshina kannada" if state == "karnataka" & district == "mangalore"
	
	replace district = "kollam" if state == "kerala" & district == "quilon"
	
	replace district = "east nimar" if state == "madhya pradesh" & district == "khandwa"
	replace district = "west nimar" if state == "madhya pradesh" & district == "khargaone"
	replace district = "singrauli" if state == "madhya pradesh" & district == "singrouli"
	
	replace district = "ahmadnagar" if state == "maharashtra" & district == "ahmednagar"
	replace district = "bid" if state == "maharashtra" & district == "beed"
	replace district = "palhgar" if state == "maharashtra" & district == "dahanu"
	replace district = "gondiya" if state == "maharashtra" & district == "gondia"
	replace district = "pune" if state == "maharashtra" & district == "junnar"
	replace district = "mumbai suburban" if state == "maharashtra" & district == "mumbai (suburban)"
	replace district = "mumbai" if state == "maharashtra" & district == "mumbai city"
	replace district = "raigarh" if state == "maharashtra" & district == "raigad"
	
	replace district = "imphal east" if state == "manipur" & district == "manipur(east)"
	replace district = "imphal west" if state == "manipur" & district == "manipur(west)"
	
	replace district = "baleshwar" if state == "odisha" & district == "balasore"
	replace district = "baudh" if state == "odisha" & district == "boudh"
	replace district = "debagarh" if state == "odisha" & district == "devgarh"
	replace district = "jajapur" if state == "odisha" & district == "jajpur"
	replace district = "kendujhar" if state == "odisha" & district == "keonjhar"
	replace district = "nabarangapur" if state == "odisha" & district == "nabarangpur"
	replace district = "subarnapur" if state == "odisha" & district == "sonapur"
	
	replace district = "puducherry" if state == "puducherry" & district == "pondicherry"
	
	replace district = "sahibzada ajit singh nagar" if state == "punjab" & district == "sas nagar"
	replace district = "shahid bhagat singh nagar" if (state == "punjab" & district == "shahid bhagat singh nagar(nawansheher tahsil)") | (state == "punjab" & district == "nawanshahr")
	replace district = "tarn taran" if state == "punjab" & district == "tran taran"
	replace district = "rupnagar" if state == "punjab" & district == "ropar"
	replace district = "muktsar" if state == "punjab" & district == "shri muktsar sahib"
	
	replace district = "chittaurgarh" if state == "rajasthan" & district == "chittorgarh"
	replace district = "dhaulpur" if state == "rajasthan" & district == "dholpur"
	replace district = "ganganagar" if state == "rajasthan" & district == "sri ganganagar"
	replace district = "jalor" if state == "rajasthan" & district == "jalore"
	replace district = "jhunjhunun" if state == "rajasthan" & district == "jhunjhunu"
	
	replace district = "north district" if state == "sikkim" & district == "north"
	replace district = "south district" if state == "sikkim" & district == "south"
	replace district = "east district" if state == "sikkim" & district == "east"
	replace district = "west district" if state == "sikkim" & district == "west"
	
	replace district = "kancheepuram" if (state == "tamilnadu" & district == "kanchipuram") | (state == "tamilnadu" & district == "chengalpattu")
	replace district = "kanniyakumari" if state == "tamilnadu" & district == "kanyakumari"
	replace district = "thiruvallur" if state == "tamilnadu" & district == "tiruvallur"
	replace district = "tiruchirappalli" if state == "tamilnadu" & district == "trichy"
	
	replace district = "dhalai" if state == "tripura" & district == "dhalai district"
	replace district = "north tripura" if state == "tripura" & district == "district north"
	replace district = "south tripura" if state == "tripura" & district == "district south"
	
	replace district = "baghpat" if state == "uttar pradesh" & district == "bagpat"
	replace district = "kanshiram nagar" if state == "uttar pradesh" & district == "kashiram nagar"
	replace district = "kheri" if state == "uttar pradesh" & district == "lakhimpur kheri"
	replace district = "mahrajganj" if state == "uttar pradesh" & district == "maharajganj"
	replace district = "rae bareli" if state == "uttar pradesh" & district == "raebareli"
	replace district = "sant ravidas nagar (bhadohi)" if state == "uttar pradesh" & district == "sant ravidas nagar"
	replace district = "sant ravidas nagar (bhadohi)" if state == "uttar pradesh" & district == "bhadoi"
	replace district = "mahamaya nagar" if state == "uttar pradesh" & district == "hathras"
	
	replace district = "hardwar" if state == "uttarakhand" & district == "haridwar"
	replace district = "garhwal" if state == "uttarakhand" & district == "pauri garhwal"
	
	** Tempfile
	tempfile fc_district
	save "`fc_district'"
	
	** Merge with Census
	use "${DATA}/dta/dist_census_codes", clear
	replace state = lower(state)
	replace district = lower(district)
	merge 1:m state district using "`fc_district'", keep(3) nogen
	
	//4. Aggregate to District
	
	** Sector Indicator
	tab proj_category, gen(proj_cat_)
	tab proj_shape, gen(proj_shape_)

	** Handle Labels
	foreach v of varlist proj_cat_1-proj_shape_3 {
	
		** Newvar Name
		local a : var lab `v'
		local b = subinstr("`a'", "proj_category==", "", .)
		local b = subinstr("`b'", "proj_shape==", "", .)
		local c = subinstr("`b'", "/", " ", .)
		
		** Clone
		local newvar = subinstr("`c'", " ", "_", .)
		clonevar "`newvar'" = `v'
		
		** Store Label
		local lab_`v' = strproper("`b'")
		local lab_`newvar' = strproper("`b'")
	}

	** Aggregate
	collapse (sum)  district_forest district_nonforest approach_access-non_linear ///
			 (mean) proj_in_pa_num proj_in_pa_esz_num proj_scheduledarea_num ///
					proj_cat_* proj_shape_*, ///
			 by(c_code_2011 year_month)
	
	** Add labels
	foreach v of varlist proj_cat_1-proj_shape_3 approach_access-non_linear {
		label var `v' "`lab_`v''"
	}

	//5. Balance Panel
	
	** Merge with Census
	merge m:1 c_code_2011 using "${DATA}/dta/dist_census_codes"
	
	** Balance Panel
	encode c_code_2011, gen(c_code_2011_num)
	tsset c_code_2011_num year_month
	sort year_month 
	gen ym = year_month[1]
	replace year_month = ym if _m == 2
	tsfill, full
	
	** Clean
	drop c_code_2011 district state _merge ym
	decode c_code_2011_num, gen(c_code_2011)
	
	** Zeros
	foreach var of varlist district_forest-proj_shape_3 {
		replace `var' = 0 if `var' == .
	}
		
	//6. Generate Variables
	
	foreach var of varlist district_forest district_nonforest ///
		approach_access-non_linear {
	
		** Cumulative
		sort c_code_2011 year_month
		by c_code_2011: gen `var'_cum = sum(`var')
		
		** km-squared
		if "`var'" == "district_forest" | "`var'" == "district_nonforest"{
			gen `var'_km2 = `var' / 100
			gen `var'_cum_km2 = `var'_cum / 100
		}
		
	}
	
	** Label
	foreach v of varlist approach_access-non_linear {
		la var `v'_cum "`lab_`v''"
	}
	
	la var district_forest "Deforestation (ha.)"
	la var district_forest_cum "Cumulative Deforestation (ha.)"
	la var district_nonforest "Non-Forest Diversion (ha.)"
	la var district_nonforest_cum "Cumulative Non-Forest Diversion (ha.)"
	la var district_forest_km2 "Deforestation (\(km^{2}\))"
	la var district_forest_cum_km2 "Cumulative Deforestation (\(km^{2}\))"
	la var district_nonforest_km2 "Non-Forest Land Diversion (\(km^{2}\))"
	la var district_nonforest_cum_km2 "Cumulative Non-forest Land Diversion (\(km^{2}\))"
	
	
	** State/district strings
	merge m:1 c_code_2011 using "${DATA}/dta/dist_census_codes", ///
		keepus(state district) nogen
	
	** Year, Month
	gen date = dofm(year_month)
	gen year = year(date)
	gen month = month(date)
	drop date
	
	** State Codes
	gen state_code_2011 = substr(c_code_2011, 1, 3)
	encode state_code_2011, gen(state_code_2011_num)
	
	//7. Merge with eBird
	merge 1:1 c_code_2011 year_month ///
			  using "${DATA}/dta/ebird_dist_biodiv_`sampsize'.dta", ///
			  keepus(species_richness shannon_index) keep(3) nogen
			  
	** Order
	order *_2011_num state district year_month *_cum species* shannon*
		  
	** Write Master
	save "${DATA}/dta/fc_ebd_master_`sampsize'.dta", replace
	
	
}
