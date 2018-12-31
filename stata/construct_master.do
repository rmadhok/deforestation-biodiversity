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
local biodiversity 		0
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
	order c_code_2011 year date_obs id id_event id_group tax_order count name latitude longitude

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
	bys id_even: egen num_check = count(count)
	drop if num_check == 0
	drop num_check
	
	//3. Diversity Indices
	
	** Species Richness
	bys id_event: gen species_richness = _N
	
	** Shannon Diversity Index
	bys id_event: egen sum = total(count), mi
	gen p = count / sum
	gen plnp = p*ln(p)
	bys id_event: egen sum_plnp = total(plnp)
	gen shannon_index = -sum_plnp
	drop sum p plnp sum_plnp
	
	
	//4. Aggregate
	
	** Format date
	gen date = date(date_obs, "20YMD")
	gen month = month(date)
	gen year_month = ym(year, month)
	format year_month %tmCCYY-NN

	** Aggregate to district
	collapse (mean) species_richness shannon_index ///
			 (first) state county year month, ///
			 by(c_code_2011 year_month)
			 
	** Write
	export delimited using "${DATA}/csv/ebird_dist_biodiv_`sampsize'.csv", replace
	
}

*===============================================================================
* FOREST CLEARANCE DISTRICT CODES
*===============================================================================
if `forest_codes' == 1 {
	
	
	use "${DATA}/dta/fc_clean.dta", clear
	
	

}
