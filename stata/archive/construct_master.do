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
local biodiversity_all 		1
local biodiversity_vet		1
local forest_codes			1

// Settings
* Percent random sample of eBird population (set to 5 or 10%)
local sampsize			5

*===============================================================================
* BIODIVERSITY INDICATORS
*===============================================================================
if `biodiversity_all' == 1 {

	//1. Organize Data
	
	** Read data
	import delimited using "${DATA}/csv/ebird_sample_`sampsize'.csv", clear
	*keep if year > 2014

	** Rename
	ren (taxonomicorder category commonname scientificname ///
		 subspeciescommonname subspeciesscientificname observationcount ///
		 localitytype observationdate observerid samplingeventidentifier ///
		 protocoltype protocolcode durationminutes effortdistancekm ///
		 effortareaha groupidentifier numberobservers) ///
		(tax_order category name name_sci name_subspecies name_subspecies_sci ///
		count locality_type date_obs id id_event protocol_type protocol_code ///
		duration_min effort_distance_km effort_area_ha id_group num_observers)
	
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
	duplicates drop id_group tax_order, force

	** Duration (Callaghan et al 2018)
	keep if duration_min >= 5 & duration_min <= 240
	
	** Drop incomplete checklists
	bys id_event: egen num_check = count(count)
	drop if num_check == 0
	drop num_check
	
	** Protocol - Stationary, Travelling
	keep if inlist(protocol_code, "P21", "P22")
	
	//3. Veteran Birders
	preserve
	
		bys year id: egen n_months = nvals(yearmonth)
		bys year id: gen veteran = (n_months >= 6)
		bys c_code_2011 yearmonth id: keep if _n == 1
		collapse (sum) n_veterans = veteran, by(c_code_2011 yearmonth)
		
		gen year_month = ym(year(date(yearmonth, "20YM")), month(date(yearmonth, "20YM")))
		format year_month %tmCCYY-NN
		drop yearmonth
		
		tempfile veterans
		save "`veterans'"
		
	restore

	** Unique Birders
	bys c_code_2011 yearmonth: egen n_birders = nvals(id)
	bys c_code_2011 yearmonth: egen n_trips = nvals(id_event)
	
	//3. Diversity Indices
	
	** Species Richness
	bys id_event: gen species_richness = _N
	la var species_richness "Species Richness"
	
	** Shannon Diversity Index
	bys id_event: egen N = total(count), mi
	gen p = count / N
	gen plnp = p*ln(p)
	bys id_event: egen sum_plnp = total(plnp)
	gen shannon_index = -sum_plnp
	drop p plnp sum_plnp	
	la var shannon_index "Shannon Diversity Index"
	
	** Simpson Diversity Index
	gen n_n_1 = count*(count-1)
	bys id_event: egen sum_n_n_1 = total(n_n_1)
	gen N_N_1 = N*(N - 1)
	gen inv_index = sum_n_n_1 / N_N_1
	gen simpson_index = 1 - inv_index
	drop N inv_index n_n_1 N_N_1 sum_n_n_1
	la var simpson_index "Simpson Diversity Index"
	
	** Format date
	gen year_month = ym(year, month(date(date_obs, "20YMD")))
	format year_month %tmCCYY-NN

	//4. Aggregate to district
	bys id_event: keep if _n == 1
	collapse (mean) species_richness shannon_index simpson_index ///
					duration_min effort_distance_km ///
			 (first) n_birders n_trips, ///
			 by(c_code_2011 year_month)
			 
	tempfile biodiversity
	save "`biodiversity'"
	
	//5. Balance Panel
	
	** Read Census Data
	import delimited "${DATA}/csv/2011_india_dist.csv", clear
	
	** Clean
	drop id c_code11 c_code11_n code_11
	drop if c_code_11 == "NA"
	ren (c_code_11 name state_ut) (c_code_2011 district state)
	save "${DATA}/dta/2011_india_dist.dta", replace
	keep c_code_2011 district state
	
	** Merge with eBird
	merge 1:m c_code_2011 using "`biodiversity'"
	
	** Balance Panel
	encode c_code_2011, gen(c_code_2011_num)
	tsset c_code_2011_num year_month
	sort year_month 
	gen ym = year_month[1]
	replace year_month = ym if _m == 1
	tsfill, full

	** Clean
	drop c_code_2011 district state _merge ym
	decode c_code_2011_num, gen(c_code_2011)
	
	save "`biodiversity'", replace

	//6. Fill Missing with BirdLife
	
	** Read
	import delimited using "${DATA}/csv/bl_district.csv", clear
	
	** Condense
	ren c_code_11 c_code_2011
	drop if c_code_2011 == ""
	keep if presenc == 1 | presenc == 2 // Species likely to occur

	** Collapse
	gen species_richness_bl = 1
	collapse (count) species_richness_bl, by(c_code_2011)
	
	** Merge with eBird
	merge 1:m c_code_2011 using "`biodiversity'", nogen
	
	//7. Merge with Monthly Datasets
	
	** Veterans
	merge 1:1 c_code_2011 year_month using "`veterans'", nogen
	
	save "`biodiversity'", replace
	
	** Monthly Spatial Coverage and Weather
	
	foreach file in "coverage_ym_grid_10km_sample_`sampsize'" "india_weather" {
	
		** Read
		import delimited "${DATA}/csv/`file'", clear
		
		** Formate Date
		gen year_month = ym(year(date(yearmonth, "20YM")), month(date(yearmonth, "20YM")))
		format year_month %tmCCYY-NN
		drop yearmonth
		
		** Merge
		merge 1:1 c_code_2011 year_month ///
			using "`biodiversity'", keep(2 3) nogen
		
		save "`biodiversity'", replace
	}

	//8. Merge with District Data
	
	** a. Hotspots
	import delimited "${DATA}/csv/hotspots_dist_codes", clear
	drop if c_code_2011 == "NA"
	
	bys c_code_2011: gen n_hotspots = _N
	bys c_code_2011: keep if _n == 1
	keep c_code_2011 n_hotspots

	tempfile hotspots
	save "`hotspots'"
	
	** b. District Spatial Coverage
	import delimited "${DATA}/csv/coverage_dist_grid_10km_sample_`sampsize'", clear
	
	** Merge with hotspots
	merge 1:1 c_code_2011 using "`hotspots'", nogen
	
	** Merge with Census Data
	merge 1:1 c_code_2011 using "${DATA}/dta/2011_india_dist", nogen
	
	** Merge with Biodiversity Panel
	merge 1:m c_code_2011 using "`biodiversity'", nogen
	
	//9. Prep Variables
	
	** Log/IHS
	foreach var of varlist species_richness shannon_index simpson_index ///
		species_richness_bl coverage temperature_mean precipitation_mean {
		
		gen `var'_ln = log(`var')
		gen `var'_ihs = asinh(`var')
		
	}
	
	** Construct
	gen pop_density = tot_pop / tot_area
	gen bird_density_dist = n_birds / tot_area
	gen bird_density_ym = n_birds_ym / tot_area
	gen veteran_frac = n_veterans / n_birders 
	gen species_richness_ihs2 = species_richness_ihs
	replace species_richness_ihs2 = species_richness_bl_ihs if species_richness_ihs2 == .
	
	** Label
	la var species_richness_ln "Log Species Richness"
	la var species_richness_ihs "IHS Species Richenss"
	la var species_richness "Species Richness"
	la var shannon_index "Shannon Index"
	la var shannon_index_ln "Log Shannon Index"
	la var shannon_index_ihs "IHS Shannon Index"
	la var simpson_index "Simpson Index"
	la var simpson_index_ln "Log Simpson Index"
	la var simpson_index_ihs "IHS Simpson Index"
	la var coverage "Spatial Coverage"
	la var coverage_ihs "Spatial Coverage"
	la var coverage_all "Spatial Coverage (all)"
	la var n_hotspots "No. of Birding Hotspots"
	la var n_birders "No. of Birders"
	la var n_trips "No. of Birding Trips"
	la var veteran_frac "Fraction Veteran Users"
	la var bird_density_dist "Bird Density (per \(km^{2}\))"
	la var tot_area "Total Area (\(km^{2}\))"
	la var pop_density "Population Density (per \(km^{2}\))"
	la var temperature_mean "Temperature (C)"
	la var temperature_mean_ihs "Temperature (C)"
	la var precipitation_mean "Precipitation (mm)"
	la var precipitation_mean_ihs "Precipitation (mm)"
	la var duration_min "Trip Duration (min.)"
	la var effort_distance_km "Trip Distance (km)"
	
	//10. Write
	order c_code_2011* state district year_month species* shannon* simpson* ///
		coverage* veteran_frac n_* 
	sort c_code_2011 year_month
	save "${DATA}/dta/ebird_dist_biodiv_`sampsize'.dta", replace
	
}

if `biodiversity_vet' == 1 {

	//1. Organize Data
	
	** Read data
	import delimited using "${DATA}/csv/ebird_sample_`sampsize'.csv", clear
	*keep if year > 2014

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
		
	//2. Keep Veteran Birders Only
	bys year id: egen n_months = nvals(yearmonth)
	bys year id: gen veteran = (n_months >= 6)
	keep if veteran == 1

	//3. Prepare eBird (Sullivan et al, 2014)
	
	** Drop duplicates from group events
	duplicates drop id_group tax_order, force

	** Birdwatching duration (Callaghan et al 2018)
	keep if duration_min >= 5 & duration_min <= 240
	
	** Drop incomplete checklists
	bys id_event: egen num_check = count(count)
	drop if num_check == 0
	drop num_check
	
	** Protocol - Stationary, Travelling
	keep if inlist(protocol_code, "P21", "P22")

	** Unique Birders
	bys c_code_2011 yearmonth: egen n_birders = nvals(id)
	bys c_code_2011 yearmonth: egen n_trips = nvals(id_event)
	
	//4. Diversity Indices
	
	** Species Richness
	bys id_event: gen species_richness = _N
	la var species_richness "Species Richness"

	** Shannon Diversity Index
	bys id_event: egen N = total(count), mi
	gen p = count / N
	gen plnp = p*ln(p)
	bys id_event: egen sum_plnp = total(plnp)
	gen shannon_index = -sum_plnp
	drop p plnp sum_plnp	
	la var shannon_index "Shannon Diversity Index"
	
	** Simpson Diversity Index
	gen n_n_1 = count*(count-1)
	bys id_event: egen sum_n_n_1 = total(n_n_1)
	gen N_N_1 = N*(N - 1)
	gen inv_index = sum_n_n_1 / N_N_1
	gen simpson_index = 1 - inv_index
	drop N inv_index n_n_1 N_N_1 sum_n_n_1
	la var simpson_index "Simpson Diversity Index"
	
	** Format date
	gen year_month = ym(year, month(date(date_obs, "20YMD")))
	format year_month %tmCCYY-NN

	//5. Aggregate to district
	bys id_event: keep if _n == 1
	collapse (mean) species_richness shannon_index simpson_index ///
					duration_min effort_distance_km ///
			 (first) n_birders n_trips, ///
			 by(c_code_2011 year_month)
			 
	tempfile biodiversity
	save "`biodiversity'"
	
	//6. Balance Panel
	
	** Read Census Data
	import delimited "${DATA}/csv/2011_india_dist.csv", clear
	
	** Clean
	drop id c_code11 c_code11_n code_11
	drop if c_code_11 == "NA"
	ren (c_code_11 name state_ut) (c_code_2011 district state)
	save "${DATA}/dta/2011_india_dist.dta", replace
	keep c_code_2011 district state
	
	** Merge with eBird
	merge 1:m c_code_2011 using "`biodiversity'"
	
	** Balance Panel
	encode c_code_2011, gen(c_code_2011_num)
	tsset c_code_2011_num year_month
	sort year_month 
	gen ym = year_month[1]
	replace year_month = ym if _m == 1
	tsfill, full

	** Clean
	drop c_code_2011 district state _merge ym
	decode c_code_2011_num, gen(c_code_2011)
	
	save "`biodiversity'", replace
	
	//7. Fill Missing with BirdLife
	
	** Read
	import delimited using "${DATA}/csv/bl_district.csv", clear
	
	** Condense
	ren c_code_11 c_code_2011
	drop if c_code_2011 == ""
	keep if presenc == 1 | presenc == 2 // Species likely to occur

	** Collapse
	gen species_richness_bl = 1
	collapse (count) species_richness_bl, by(c_code_2011)
	
	** Merge with eBird
	merge 1:m c_code_2011 using "`biodiversity'", nogen
	
	//8. Merge with Monthly Datasets
	save "`biodiversity'", replace
	
	** Monthly Spatial Coverage and Weather
	foreach file in "coverage_ym_grid_10km_sample_`sampsize'" "india_weather" {
	
		** Read
		import delimited "${DATA}/csv/`file'", clear
		
		** Formate Date
		gen year_month = ym(year(date(yearmonth, "20YM")), month(date(yearmonth, "20YM")))
		format year_month %tmCCYY-NN
		drop yearmonth
		
		** Merge
		merge 1:1 c_code_2011 year_month ///
			using "`biodiversity'", keep(2 3) nogen
		
		save "`biodiversity'", replace
	}

	//9. Merge with District Data
	
	** a. Hotspots
	import delimited "${DATA}/csv/hotspots_dist_codes", clear
	drop if c_code_2011 == "NA"
	
	bys c_code_2011: gen n_hotspots = _N
	bys c_code_2011: keep if _n == 1
	keep c_code_2011 n_hotspots

	tempfile hotspots
	save "`hotspots'"
	
	** b. District Spatial Coverage
	import delimited "${DATA}/csv/coverage_dist_grid_10km_sample_`sampsize'", clear
	
	** Merge with hotspots
	merge 1:1 c_code_2011 using "`hotspots'", nogen
	
	** Merge with Census Data
	merge 1:1 c_code_2011 using "${DATA}/dta/2011_india_dist", nogen
	
	** Merge with Biodiversity Panel
	merge 1:m c_code_2011 using "`biodiversity'", nogen
	
	//10. Prep Variables
	
	** Log/IHS
	foreach var of varlist species_richness shannon_index simpson_index ///
		species_richness_bl coverage temperature_mean precipitation_mean {
		gen `var'_ln = log(`var')
		gen `var'_ihs = asinh(`var')
	}
	
	gen species_richness_ihs2 = species_richness_ihs
	replace species_richness_ihs2 = species_richness_bl_ihs if species_richness_ihs2 == .
	
	** Label
	la var temperature_mean "Temperature (C)"
	la var temperature_mean_ihs "Temperature (C)"
	la var precipitation_mean "Precipitation (mm)"
	la var precipitation_mean_ihs "Precipitation (mm)"
	la var coverage_ihs "Spatial Coverage"
	
	
	// Write
	order c_code_2011* state district year_month species* *_index coverage* n_* 
	sort c_code_2011 year_month
	save "${DATA}/dta/ebird_dist_biodiv_vet_`sampsize'.dta", replace
	
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
	reshape long district_ district_forest_ district_nonforest_, ///
		i(prop_no) j(dist_id)
	
	** Clean
	ren (district_ district_forest_ district_nonforest_) ///
		(district district_forest district_nonforest)
	drop if district == ""
	
	//3. Merge Census Codes
	** Manually Sync Names
	
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
	use "${DATA}/dta/2011_india_dist", clear
	keep state district c_code_2011
	replace state = lower(state)
	replace district = lower(district)
	merge 1:m state district using "`fc_district'", keep(3) nogen
	
	//4. Aggregate to District
	
	** Sector Indicator
	tab proj_cat, gen(proj_cat_)
	tab proj_shape, gen(proj_shape_)
	
	** Land Diversion by Project Type
	local projlist `" "Electricity" "Forest Village Relocation" "Industry" "Irrigation" "Mining" "Other" "Transport" "'
	local count : word count `projlist'
	local i = 1
	
	forvalues n = 1/`count' {
		
		* Forest Area
		gen forest_`n' = district_forest if proj_cat_`n' == 1
		gen nonforest_`n' = district_nonforest if proj_cat_`n' == 1
		bys c_code_2011 year_month: egen district_forest_c`n' = total(forest_`n')
		bys c_code_2011 year_month: egen district_nonforest_c`n' = total(nonforest_`n')
		drop forest_`n' nonforest_`n'
		
		* Label
		local type : word `i' of `projlist'
		la var district_forest_c`n' "Deforestation (`type')" 
		la var district_nonforest_c`n' "Non-forest Land Diversion (`type')" 
		local ++i
	
	}
	
	** Land Diversion by Project Shape
	local shapelist "Hybrid Linear Non-Linear"
	local i = 1
	forvalues n = 1/3 {
	
		gen forest_`n' = district_forest if proj_shape_`n' == 1
		gen nonforest_`n' = district_nonforest if proj_shape_`n' == 1
		bys c_code_2011 year_month: egen district_forest_s`n' = total(forest_`n')
		bys c_code_2011 year_month: egen district_nonforest_s`n' = total(nonforest_`n')
		drop forest_`n' nonforest_`n'
		
		* Label
		local type : word `i' of `shapelist'
		la var district_forest_s`n' "Deforestation (`type')" 
		la var district_nonforest_s`n' "Non-forest Land Diversion (`type')" 
		local ++i
		
	}
	
	** Land Diversion in Protected Area
	gen forest_pa = district_forest if proj_in_pa_esz_num == 1
	gen nonforest_pa = district_nonforest if proj_in_pa_esz_num == 1
	bys c_code_2011 year_month: egen district_forest_pa = total(forest_pa)
	la var district_forest_pa "Deforestation (Protected Area)"
	bys c_code_2011 year_month: egen district_nonforest_pa = total(nonforest_pa)
	la var district_nonforest_pa "Non-forest Land Diversion (Protected Area)"
	drop forest_pa nonforest_pa

	** Save Labels
	foreach v of varlist district_forest_* district_nonforest_* {
		local lab_`v' : var lab `v'
	}

	** Aggregate
	collapse (sum)  district_forest district_nonforest ///
			 (first) district_forest_* district_nonforest_*, ///
			 by(c_code_2011 year_month)
	
	** Label
	foreach v of varlist district_forest_* district_nonforest_* {
		la var `v' "`lab_`v''"
	}
	la var district_forest "Deforestation"
	la var district_nonforest "Non-forest Land Diversion"
	
	//5. Balance Panel
	
	** Merge with Census
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", keepus(c_code_2011)
	
	** Balance Panel
	encode c_code_2011, gen(c_code_2011_num)
	tsset c_code_2011_num year_month
	sort year_month 
	gen ym = year_month[1]
	replace year_month = ym if _m == 2
	tsfill, full
	
	** Clean
	drop c_code_2011 _merge ym
	decode c_code_2011_num, gen(c_code_2011)
	
	** Zeros
	foreach var of varlist district_forest-district_nonforest_pa {
		replace `var' = 0 if `var' == .
	}
	
	//6. Generate Variables
	foreach var of varlist district_forest-district_nonforest_pa {
		
		local vlab : var lab `var'
	
		** Cumulative
		sort c_code_2011 year_month
		by c_code_2011: gen `var'_cum = sum(`var')
		la var `var'_cum "Cum. `vlab'"
		
		** km2
		gen `var'_km2 = `var' / 100
		gen `var'_cum_km2 = `var'_cum / 100
		la var `var'_cum_km2 "Cum. `vlab'"
			
		** Inverse Hyperbolic Sine
		gen `var'_ihs = asinh(`var'_km2)
		gen `var'_cum_ihs = asinh(`var'_cum_km2)
		la var `var'_cum_ihs "Cum. `vlab'"
		
	}

	** State/district strings
	merge m:1 c_code_2011 using "${DATA}/dta/2011_india_dist", ///
		keepus(state district) nogen
	
	** Year, Month
	gen date = dofm(year_month)
	gen year = year(date)
	gen month = month(date)
	drop date
	
	** State Codes
	gen state_code_2011 = substr(c_code_2011, 1, 3)
	encode state_code_2011, gen(state_code_2011_num)
	
	//7. Merge eBird + Nightlights
	
	tempfile forest
	save "`forest'"

	** All Users
	use "${DATA}/dta/ebird_dist_biodiv_`sampsize'.dta", clear
	merge 1:1 c_code_2011 year_month using "`forest'", keep(3) nogen
	merge 1:1 c_code_2011 year_month using "${DATA}/dta/india_nightlights", ///
		keep(1 3) nogen
	order *_2011_num state district year_month district_*_cum* ///
		species* *_index coverage n_* *_lights  
	save "${DATA}/dta/fc_ebd_all_`sampsize'.dta", replace
	
	** Veterans
	use "${DATA}/dta/ebird_dist_biodiv_vet_`sampsize'.dta", clear
	merge 1:1 c_code_2011 year_month using "`forest'", keep(3) nogen
	merge 1:1 c_code_2011 year_month using "${DATA}/dta/india_nightlights", ///
		keep(1 3) nogen
	order *_2011_num state district year_month district_*_cum* ///
		species* *_index coverage n_*  *_lights
	save "${DATA}/dta/fc_ebd_vet_`sampsize'.dta", replace
	
}