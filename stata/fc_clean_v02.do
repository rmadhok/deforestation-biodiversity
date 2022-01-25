********************************************************************************																			
* PROJECT: 	Deforestation and Biodiversity																												
* PURPOSE: Clean raw forest clearance data - updated data to 2021			    																																																				
* AUTHOR:  Raahil Madhok 																																
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
*gl READ 	"/Volumes/Backup Plus/research/data/def_biodiv/parivesh"
gl SAVE		"/Users/rmadhok/Dropbox/def_biodiv/data"

*===============================================================================
* CLEAN VARIABLE NAMES
*===============================================================================

* Read raw data
import excel using "${SAVE}/raw/fc_raw_v02_use.xlsx", first clear
ren *, lower
drop *copy* *reason*

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
ren q ec_needed
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
ren careaofnonforestorreven ca_nfl_area
ren iiinoofdistrictsinvolved ca_num_districts
ren atotalnumberoffamilies tot_fam_disp
ren bnumberofscheduledcastef sc_fam_disp
ren cnumberofscheduletribesf st_fam_disp
ren dnumberofotherfamilies other_fam_disp
ren au ca_nfl_area2
ren benvironmentalclearancefil ec_fileno2
ren ddateofgrantofenvironmen ec_date
ren dtotalareaoftheminingl area_mining_lease
ren eareaofforestlandlocate area_f_mining_lease
ren vnatureofminingundergrou mining_type
ren inoofminerals num_minerals
ren atotalareaoftheminingl area_mining_lease2
ren bareaofforestlandlocate area_f_mining_lease2

* Fill
replace area_mining_lease = area_mining_lease2 if area_mining_lease == ""
drop area_mining_lease2
replace ec_fileno = ec_fileno2 if ec_fileno == ""
drop ec_fileno2
replace area_f_mining_lease = area_f_mining_lease2 if area_f_mining_lease == ""
drop area_f_mining_lease2
replace ca_nfl_area = ca_nfl_area2 if ca_nfl_area == ""
drop ca_nfl_area2

* District-wise breakup
foreach v of varlist districtwisebreakupdistrict-districtwisebreakupnonforest ///
	ad-af ax-az bc-bw cc-ch {
		
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

* Patches (linear = segments; non-linear=patches)
ren division*_anoofpatches div*_patches
ren division*_bnoof* div*_segs
foreach v of varlist *_patches *_segs {
	replace `v' = trim(lower(`v'))
	replace `v' = "" if `v' == "nil"
	replace `v' = "1" if `v' == "one"
	replace `v' = "2" if `v' == "two"
	replace `v' = "3" if `v' == "three" 
	replace `v' = "4" if `v' == "four"
	replace `v' = "5" if `v' == "five"
	replace `v' = "6" if `v' == "six"
	replace `v' = "7" if `v' == "seven"
	replace `v' = "8" if `v' == "eight"
	replace `v' = "9" if `v' == "nine" 
	replace `v' = "10" if `v' == "ten"
	replace `v' = "11" if `v' == "eleven"
	replace `v' = "12" if `v' == "twelve"
	destring `v', replace
}
egen n_patches_nl = rowtotal(div*_patches), m
egen n_segs_lin = rowtotal(div*_segs), m
egen patches = rowtotal(n_patches_nl n_segs_lin)
drop div*_* n_patches_nl n_segs_lin

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
duplicates drop prop_no, force // 39 duplicates

* missing (yes/no's)
foreach v of varlist displacement proj_in_pa_esz proj_scheduledarea ///
	ua_legal_status employment proj_fra proj_cost ec_needed cba power_cap ///
	ca_nfl_rfl ca_area* ca_nfl_area area_mining_lease area_f_mining_lease ///
	num_minerals {
	
	replace `v' = "" if `v' == "nil" | `v' == "not applicable"
}

* zero
foreach var of varlist *_fam_disp {

	replace `var' = "0" if `var' == "nil" | `var' == ""
}

* Destring
foreach v of varlist proj_cost proj_area_forest proj_area_nonforest ///
	perm_emp temp_emp dist_*_* power_cap ca_num_districts ///
	ca_nfl_area *_fam_* area*mining* num_minerals {
		
		destring `v', replace force
		replace `v' = . if `v' < 0
	}
	
* Encode (yes/no)
foreach var of varlist employment displacement cba ec_needed ///
	proj_in_pa* proj_scheduledarea proj_fra {
		
		gen `var'_num = (`var' == "yes")
		replace `var'_num = . if mi(`var')
		drop `var'
	}

*------------------------------
* CATEGORIES
*------------------------------

* Project Categorization
replace mining_type = "opencast" if mining_type == "opencast+underground" // only two approved hybrid mines. Makes for fewer categories
gen proj_cat = "electricity" if inlist(proj_category, "hydel", "sub station", "thermal", "transmission line", "village electricity", "wind power", "solar power")
replace proj_cat = "transportation" if inlist(proj_category, "road", "railway") // approach road part of "other"
*replace proj_cat = "transportation" if inlist(proj_category, "road", "approach access", "railway")
replace proj_cat = "irrigation" if inlist(proj_category, "canal", "irrigation", "drinking water")
replace proj_cat = "resettlement" if inlist(proj_category, "forest village conversion", "rehabilitation")
replace proj_cat = "mining" if inlist(proj_category, "mining", "quarrying")
replace proj_cat = "industry" if inlist(proj_category, "industry")
replace proj_cat = "underground" if inlist(proj_category, "optical fibre cable", "pipeline")
replace proj_cat = "other" if proj_cat == ""
replace proj_shape = "nonlinear" if proj_shape == "non linear"

* Recategorize Others
foreach v of varlist proj_name proj_narr {
	
	* Resettlement
	replace proj_cat = "resettlement" if regexm(`v', "migrant")
	replace proj_cat = "resettlement" if regexm(`v', "refugee")
	replace proj_cat = "resettlement" if regexm(`v', "resettle")
	replace proj_cat = "resettlement" if regexm(`v', "pattayam")
	replace proj_cat = "resettlement" if regexm(`v', "relocat")
	
	* Roads
	*replace proj_cat = "transportation" if regexm(`v', "approach") & proj_cat == "other"
	*replace proj_cat = "transportation" if regexm(`v', "access") & proj_cat == "other"
	
	* Power
	replace proj_cat = "electricity" if regexm(`v', "power") & proj_cat == "other"
	replace proj_cat = "electricity" if regexm(`v', "substation") & proj_cat == "other"
	replace proj_cat = "electricity" if regexm(`v', "sub-station") & proj_cat == "other"
	replace proj_cat = "electricity" if regexm(`v', "kv") & proj_cat == "other"
	replace proj_cat = "electricity" if regexm(`v', "transmission line") & proj_cat == "other"

	* OFC
	replace proj_cat = "underground" if regexm(`v', "optical") & proj_cat == "other"
	replace proj_cat = "underground" if regexm(`v', "ofc") & proj_cat == "other"

	* Oil, Gas Pipeline
	replace proj_cat = "underground" if regexm(`v', "pipeline") & regexm(`v', "oil") & proj_cat == "other"
	replace proj_cat = "underground" if regexm(`v', "pipeline") & regexm(`v', "gas") & proj_cat == "other"
	replace proj_cat = "underground" if regexm(`v', "pipeline") & regexm(`v', "lpg") & proj_cat == "other"
	
	* Underground water
	replace proj_cat = "underground" if regexm(`v', "under") & regexm(`v', "water") & proj_cat == "other"
	replace proj_cat = "underground" if regexm(`v', "under") & regexm(`v', "irrigation") & proj_cat == "other"
}
	
* Company Status
g proj_type = "central" if regexm(ua_legal_status, "central")
replace proj_type = "state" if regexm(ua_legal_status, "state")
replace proj_type = "joint" if regexm(ua_legal_status, "joint")
replace proj_type = "private" if ua_legal_status == "private"
replace proj_type = "neither" if proj_type == ""
drop ua_legal_status
*===============================================================================
* MERGE APPROVAL DETAILS
*===============================================================================

tempfile temp
save "`temp'"

// Read
import delimited "${SAVE}/raw/fc_records_v3.csv", clear

// Sync to FC data
keep proposal_no area_applied proposal_status date_of_recomm date_from_ua_to_nodal
ren (proposal_no area_applied proposal_status date_from_ua_to_nodal date_of_recomm) ///
	(prop_no proj_area_forest2 prop_status date_submitted date_recomm)
	
replace prop_no = trim(itrim(lower(prop_no)))
replace prop_status = trim(itrim(lower(prop_status)))
duplicates drop prop_no, force

// Merge 
merge 1:1 prop_no using "`temp'", keep(3) nogen

* Format Dates
gen date_submit = date(date_submitted,"DM20Y")
gen date_rec = date(date_recomm, "DM20Y")
format date_submit date_rec %td

* Save
drop proj_name proj_narr
order state prop_no prop_status date_submit date_rec proj_area_forest* proj*
sort state prop_no
save "${SAVE}/dta/fc_clean_v02.dta", replace
