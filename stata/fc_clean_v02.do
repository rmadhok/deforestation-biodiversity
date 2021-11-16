********************************************************************************
*																			
* PROJECT: 	Deforestation and Biodiversity											
*																			
* PURPOSE: Clean raw forest clearance data - updated data to 2021			    																																		
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
gl READ 	"/Volumes/Backup Plus/research/data/def_biodiv/parivesh"
gl SAVE		"/Users/rmadhok/Dropbox/def_biodiv/data"

*===============================================================================
* CLEAN VARIABLE NAMES
*===============================================================================

* Read raw data
import excel using "${READ}/fc_raw_v02_use.xlsx", first clear
ren *, lower

* Reduce
drop a page_no xtotalperiod* district*_* *khasra* *owner* ///
	bareainha avillage iiireason* iitotalcommand* ///
	bstatusofapprovalofthes ///
	
forvalues i=1/48 {
	drop district`i'
}

* Rename
ren iproposalno prop_no
ren iinameofprojectforwh proj_name 
ren iiishortnarrativeofthe proj_narr
ren ivstate state
ren vcategoryofthepropos proj_category
ren vishapeofforestlandp proj_shape
ren viiestimatedcostofthe proj_cost
ren viiiareaofforestlandpr proj_area_forest
ren ixnonforestlandrequired proj_area_nonforest
ren xiiilegalstatusofuserag ua_legal_status
ren iwhethertheprojectislike employment
ren iipermanentregularemploym perm_emp
ren iiitemporaryemploymentnumb temp_emp
ren iwhetherprojectinvolvesdi displacement
ren iwhethertheprojectrequire cba
ren s ec_needed
ren astatusoftheenvironmental ec_status
ren iwhethertheprojectorapa proj_in_pa_esz 
ren iwhethertheprojectorapar proj_scheduledarea
ren iwhethertheprocessforset proj_fra
ren iwhethernonforestorreven ca_nfl_rfl
ren iiwhethertheareaofnonfo ca_area_less_div
ren iienvironmentalclearancef ec_fileno
ren iinstalledpowergeneration power_cap
ren awhetherprojectorapartt proj_in_pa 
ren iistatusofapprovalofthe proj_nbwl 
ren bdegradedforestareaonwhi ca_degraded_area
ren careaofnonforestorreven ca_nfl_area
ren iiinoofdistrictsinvolved ca_num_districts
ren atotalnumberoffamilies tot_fam_disp
ren bnumberofscheduledcastef sc_fam_disp
ren cnumberofscheduletribesf st_fam_disp
ren dnumberofotherfamilies other_fam_disp
ren ck ca_nfl_area2
ren benvironmentalclearancefil ec_fileno2
ren ddateofgrantofenvironmen ec_date

* District-wise breakup
foreach v of varlist districtwisebreakupdistrict-districtwisebreakupnonforest ///
	ae-ag cn-li {
		
	local x : variable label `v'
	local y = subinstr("`x'", " ", "", .)
	local y = subinstr("`y'", "(ha.)", "", .)
	local y = subinstr("`y'", "Districtwisebreakup", "", .)
	local y = subinstr("`y'", "-", "", .)
	ren `v' `y'
}
ren DistrictName* district*
ren ForestLand* dist_f*
ren NonForestLand* dist_nf*

*===============================================================================
* DATA CLEANING
*===============================================================================

* Drop Missing Rows
egen nmcount = rownonmiss(_all), strok
drop if nmcount == 0
drop nmcount

* Trim Strings
ds, has(type string) 
local strvars "`r(varlist)'"
foreach var of local strvars{
	replace `var'=trim(itrim(lower(`var')))
}
	
* Drop Duplicates
duplicates drop prop_no, force // 10 duplicates

* missing
foreach v of varlist displacement proj_in_pa_esz ///
	proj_scheduledarea ua_legal_status employment ///
	proj_fra ca_nfl_area proj_cost power_cap ///
	ec_needed cba {
	
	replace `v' = "" if `v' == "nil" | `v' == "NIL" 
}

* zero
foreach var of varlist sc_fam_disp st_fam_disp other_fam_disp {
		
	replace `var' = "0" if `var' == "nil"
}

* Destring
destring proj_cost proj_area_forest proj_area_nonforest ///
	perm_emp temp_emp dist_*_* power_cap ca_num_districts ///
	ca_nfl_area2 *_fam_*, force replace

* Project Categorization
gen proj_cat = "electricity" if inlist(proj_category, "hydel", "sub station", "thermal", "transmission line", "village electricity", "wind power", "solar power")
replace proj_cat = "transportation" if inlist(proj_category, "road", "approach access", "railway")
replace proj_cat = "irrigation" if inlist(proj_category, "canal", "irrigation", "drinking water")
replace proj_cat = "resettlement" if inlist(proj_category, "forest village conversion", "encroachments", "rehabilitation")
replace proj_cat = "mining" if inlist(proj_category, "mining", "quarrying")
replace proj_cat = "industry" if inlist(proj_category, "industry")
replace proj_cat = "underground" if inlist(proj_category, "optical fibre cable", "pipeline")
replace proj_cat = "other" if proj_cat == ""
replace proj_shape = "nonlinear" if proj_shape == "non linear"

* Encode
foreach var of varlist employment displacement cba ec_needed ///
	proj_in_pa* proj_scheduledarea proj_fra {
		
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
import delimited "${READ}/fc_records_v2.csv", clear

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

* Format Dates
gen date_submit = date(date_submitted,"DM20Y")
gen date_rec = date(date_recomm, "DM20Y")
format date_submit date_rec %td

* Save
drop proj_name proj_narr prop_name
order state prop_no prop_status date_submit date_rec proj_area_forest* proj*
sort state prop_no
save "${SAVE}/dta/fc_clean_v02.dta", replace

