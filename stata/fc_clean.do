********************************************************************************
*																			
* PROJECT: 	Deforestation and Biodiversity											
*																			
* PURPOSE: Clean raw forest clearance data			    
*																			
* START DATE: November 22, 2018																	
*																			
* AUTHOR:  Raahil Madhok 													
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

//Set Directory Paths
gl ROOT 	"/Users/rmadhok/Dropbox (Personal)/def_biodiv"
gl DATA 	"${ROOT}/data"


*===============================================================================
*CLEAN VARIABLE NAMES
*===============================================================================
//Read raw data
import delimited using "${DATA}/csv/fc_raw_use.csv", encoding("utf-8")  clear

//Reduce Variable List
drop adateofsubmissionofproposals astatusoftheenvironmentalclearan ///
	bdegradedforestareaonwhichcahast benvironmentalclearancefileno ///
	careaofnonforestorrevenueforestl ddateofgrantofenvironmentalclear ///
	iienvironmentalclearancefileno *_ccopyofkmlfile* *_dkhasra* *_epresentowner ///
	page_no *_fcopyofownership* *_gcopyofmouagree* cdateofapproval ///
	v10 *_hcopyofnonencumber* *_avillage villageswisebreakupsno* 

//Clean Variable Names
ren atotalnumberoffamilies fam_num
ren awhetherprojectorapartthereofisl proj_in_pa
ren bareaofnonforestorrevenueforestl ca_nfl_rfl_area
ren bnumberofscheduledcastefamilies sc_fam_disp
ren cnumberofscheduletribesfamilies st_fam_disp
ren dnumberofotherfamilies other_fam_disp
ren iproposalno prop_no
ren iinstalledpowergenerationcapacit power_cap
ren iwhetherprojectinvolvesdisplacem displacement
ren iwhethernonforestorrevenueforest  ca_nfl_rfl
ren iwhethertheprojectislikelytogene employment
ren iwhethertheprojectorapartthereof proj_in_pa_esz
ren v21 proj_scheduledarea
ren iipermanentregularemploymentnumb perm_emp
ren iinatureoftheproject proj_nature
ren iiwhethertheareaofnonforestlando ca_area_less_div
ren iiinoofdistrictsinvolvedforraisi ca_num_districts
ren iiitemporaryemploymentnumberofpe temp_emp
ren ivstate state
ren ixnonforestlandrequiredforthispr proj_area_nonforest
ren vcategoryoftheproposal proj_category
ren vishapeofforestlandproposedtobed proj_shape
ren viiestimatedcostoftheprojectrupe proj_cost
ren viiiareaofforestlandproposedford proj_area_forest
ren xiiilegalstatusofuseragency ua_legal_status

//Rename land-wise variables

** Districts
ds, has(varlabel *Copy* *copy*)
drop `r(varlist)'

foreach v of varlist districtwisebreakupdistrictname_-v527 {
   local x : variable label `v'
   local y = subinstr("`x'", " ", "", .)
   local y = subinstr("`y'", "(ha.)", "", .)
   local y = subinstr("`y'", "wisebreakup", "", .)
   local y = subinstr("`y'", "-", "", .)
   ren `v' `y'
}

**villages
foreach v of varlist villageswisebreakupforestlandha_-v2143 {
   local x : variable label `v'
   local y = subinstr("`x'", " ", "", .)
   local y = subinstr("`y'", "(ha.)", "", .)
   local y = subinstr("`y'", "wisebreakup", "", .)
   local y = subinstr("`y'", "-", "", .)
   ren `v' `y'
}

ren DistrictDistrictName* district*
ren DistrictForestLand* district_forest*
ren DistrictNonForestLand* district_nonforest*
ren villageswisebreakup* *
ren VillagesForestLand* village_forest*
ren VillagesNonForestLand* village_nonforest*
ren district# ca_district_#
ren district#_bareainha ca_district_nfl_#
*===============================================================================
* DATA CLEANING
*===============================================================================

//Drop Missing Rows
egen nmcount = rownonmiss(_all), strok
drop if nmcount == 0
drop nmcount

//Trim Strings
ds, has(type string) 
local strvars "`r(varlist)'"
foreach var of local strvars{
	replace `var'=trim(itrim(lower(`var')))
	}
	
//Drop Duplicates
duplicates drop prop_no, force

//Manage NIL Values

** missing
foreach var of varlist displacement proj_in_pa* proj_scheduledarea ///
	ua_legal_status employment  {
		
		replace `var' = "" if `var' == "nil"
	
	}

ds ca_district_nfl*, has(type string)
foreach var of varlist `r(varlist)' ca_nfl_rfl_area{
	replace `var' = "" if `var' == "nil"
	destring `var', replace
}
	
** zero
foreach var of varlist sc_fam_disp st_fam_disp other_fam_disp ///
	power_cap proj_cost ca_num_districts {
		
		replace `var' = "0" if `var' == "nil"
		destring `var', replace
		
		}

//Categorize Project Type
gen proj_cat = "electricity" if inlist(proj_category, "hydel", "sub station", "thermal", "transmission line", "village electricity", "wind power", "solar power")
replace proj_cat = "transport" if inlist(proj_category, "road", "approach access", "railway")
replace proj_cat = "irrigation" if inlist(proj_category, "canal", "irrigation", "drinking water")
replace proj_cat = "forest village relocation" if inlist(proj_category, "forest village conversion", "encroachments", "rehabilitation")
replace proj_cat = "mining" if inlist(proj_category, "mining", "quarrying")
replace proj_cat = "industry" if inlist(proj_category, "industry", "school", "dispensary/hospital")
replace proj_cat = "other" if proj_cat == ""

//Encode Binary Variables
foreach var of varlist displacement employment proj_in_pa* ///
	proj_scheduledarea ca_nfl_rfl ca_area_less_div {
		
		gen `var'_num = (`var' == "yes")
		replace `var'_num = . if mi(`var')
		drop `var'
		
		}

*===============================================================================
* MERGE APPROVAL DETAILS
*===============================================================================

//Tempfile
tempfile temp
save "`temp'"

//Read Approval File
import delimited "${DATA}/csv/fc_records.csv", clear

//Sync to FC data
keep proposal_no area_applied proposal_status date_of_recomm date_from_ua_to_nodal
ren (proposal_no area_applied proposal_status date_from_ua_to_nodal date_of_recomm) ///
	(prop_no proj_area_forest2 prop_status date_submitted date_recomm)
	
** Sync merge ID
replace prop_no = trim(itrim(lower(prop_no)))
replace prop_status = trim(itrim(lower(prop_status)))
duplicates drop prop_no, force

// Merge 
merge 1:1 prop_no using "`temp'", keep(2 3) nogen

//Format Dates
gen date_submit = date(date_submitted,"DM20Y")
gen date_rec = date(date_recomm, "DM20Y")
format date_submit date_rec %td

//Save
order state prop_no prop_status date_submit date_rec proj_area_forest* proj*
sort state prop_no
save "${DATA}/dta/fc_clean.dta", replace
