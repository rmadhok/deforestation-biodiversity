********************************************************************************
*																			
* PROJECT: 	Deforestation and Biodiversity											
*																			
* PURPOSE: Clean raw forest clearance data			    																																		
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
gl ROOT 	"/Volumes/Backup Plus/research/data/def_biodiv/parivesh"
gl DATA 	"/Users/rmadhok/Dropbox/def_biodiv/data"

*===============================================================================
*CLEAN VARIABLE NAMES
*===============================================================================

// Read raw data
import delimited using "${ROOT}/fc_raw_v01_use.csv", encoding("utf-8")  clear

// Reduce Variable List
drop adateofsubmissionofproposals astatusoftheenvironmentalclearan ///
	bdegradedforestareaonwhichcahast benvironmentalclearancefileno ///
	careaofnonforestorrevenueforestl ddateofgrantofenvironmentalclear ///
	iienvironmentalclearancefileno *_ccopyofkmlfile* *_dkhasra* *_epresentowner ///
	cdateofapproval *_fcopyofownership* *_gcopyofmouagree* ///
	v10 *_hcopyofnonencumber* *_avillage

// Clean Names
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
   local y = subinstr("`y'", "Districtwisebreakup", "", .)
   local y = subinstr("`y'", "-", "", .)
   ren `v' `y'
}

* Drop compensatory afforestation vars
drop district1-district9_bareainha

ren DistrictName* district*
ren ForestLand* dist_f*
ren NonForestLand* dist_nf*
*ren district# ca_dist_#
*ren district#_bareainha ca_dist_nfl_#

*===============================================================================
* DATA CLEANING
*===============================================================================

// Drop Missing Rows
egen nmcount = rownonmiss(_all), strok
drop if nmcount == 0
drop nmcount

// Trim Strings
ds, has(type string) 
local strvars "`r(varlist)'"
foreach var of local strvars{
	replace `var'=trim(itrim(lower(`var')))
	}
	
// Drop Duplicates
duplicates drop prop_no, force

//Manage NILs

** missing
foreach var of varlist displacement proj_in_pa* proj_scheduledarea ///
	ua_legal_status employment  {
		
		replace `var' = "" if `var' == "nil"
	}
	
** zero
foreach var of varlist sc_fam_disp st_fam_disp other_fam_disp ///
	power_cap proj_cost ca_num_districts ca_nfl_rfl_area {
		
		replace `var' = "0" if `var' == "nil"
		destring `var', replace
	}
		
// Categorize Project Type
gen proj_cat = "electricity" if inlist(proj_category, "hydel", "sub station", "thermal", "transmission line", "village electricity", "wind power", "solar power")
replace proj_cat = "transportation" if inlist(proj_category, "road", "approach access", "railway")
replace proj_cat = "irrigation" if inlist(proj_category, "canal", "irrigation", "drinking water")
replace proj_cat = "resettlement" if inlist(proj_category, "forest village conversion", "encroachments", "rehabilitation")
replace proj_cat = "mining" if inlist(proj_category, "mining", "quarrying")
replace proj_cat = "industry" if inlist(proj_category, "industry")
replace proj_cat = "underground" if inlist(proj_category, "optical fibre cable", "pipeline")
replace proj_cat = "other" if proj_cat == ""
replace proj_shape = "nonlinear" if proj_shape == "non linear"

// Encode Binary Variables
foreach var of varlist displacement employment proj_in_pa* ///
	proj_scheduledarea ca_nfl_rfl ca_area_less_div {
		
		gen `var'_num = (`var' == "yes")
		replace `var'_num = . if mi(`var')
		drop `var'
		
		}

*===============================================================================
* MERGE APPROVAL DETAILS
*===============================================================================

tempfile temp
save "`temp'"

// Read
import delimited "${ROOT}/fc_records_v2.csv", clear

// Sync to FC data
keep proposal_no area_applied proposal_status proposal_name date_of_recomm date_from_ua_to_nodal
ren (proposal_no area_applied proposal_status proposal_name date_from_ua_to_nodal date_of_recomm) ///
	(prop_no proj_area_forest2 prop_status prop_name date_submitted date_recomm)
	
replace prop_no = trim(itrim(lower(prop_no)))
replace prop_status = trim(itrim(lower(prop_status)))
duplicates drop prop_no, force

// Merge
merge 1:1 prop_no using "`temp'", keep(3) nogen

// Re-categorize "other"
replace prop_name = trim(itrim(lower(prop_name)))
replace prop_name = subinstr(prop_name, ".", "", .)
replace prop_name = subinstr(prop_name, "-", "", .)

* OFC
replace proj_cat = "underground" if regexm(prop_name, "optical") & proj_cat == "other"
replace proj_cat = "underground" if regexm(prop_name, "ofc") & proj_cat == "other"

* Oil, Gas Pipeline
replace proj_cat = "underground" if regexm(prop_name, "pipeline") & regexm(prop_name, "oil") & proj_cat == "other"
replace proj_cat = "underground" if regexm(prop_name, "pipeline") & regexm(prop_name, "gas") & proj_cat == "other"
replace proj_cat = "underground" if regexm(prop_name, "pipeline") & regexm(prop_name, "lpg") & proj_cat == "other"

* Irrigation Pipeline
replace proj_cat = "irrigation" if regexm(prop_name, "pipeline") & regexm(prop_name, "water") & proj_cat == "other"
replace proj_cat = "irrigation" if regexm(prop_name, "pipeline") & regexm(prop_name, "irrigation") & proj_cat == "other"

* Road
replace proj_cat = "transportation" if regexm(prop_name, "approach") & proj_cat == "other"

* Relocation
replace proj_cat = "resettlement" if regexm(prop_name, "relocation") & proj_cat == "other"

// Format Dates
gen date_submit = date(date_submitted,"DM20Y")
gen date_rec = date(date_recomm, "DM20Y")
format date_submit date_rec %td

// Save
drop prop_name proj_category // reduce file size
order state prop_no prop_status date_submit date_rec proj_area_forest* proj*
sort state prop_no
save "${DATA}/dta/fc_clean_v01.dta", replace
