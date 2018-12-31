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
gl ROOT 	"/Users/rmadhok/Documents/ubc/research"
gl DATA 	"${ROOT}/def_biodiv/data"

*===============================================================================
*CLEAN VARIABLE NAMES
*===============================================================================
//Read raw data
import delimited using "${DATA}/csv/fc_raw_use.csv", encoding("utf-8")  clear

//Reduce Variable List
drop avillage adateofsubmissionofproposals anoofpatches ///
	atotalareaofsafetyzoneoftheminin bareainha bcurrentstatus ///
	bdegradedforestareaonwhichcahast bstatusofapprovalofthesupremecou ///
	cdateofapproval epresentowner iwhethertheforestlandisaquiredun ///
	iiwhethertheareaofnonforestlando iiinoofdistrictsinvolvedforraisi ///
	ivgender ivnoofpatches estimatedreservealongwithaccurac-v163 ///
	bareaofnonforestorrevenueforestl iwhethernonforestorrevenueforest ///
	mineralwisedetailsestimatedminer v177 v178 iiapprovalauthority ///
	detailsofdivisionsinvolveddivisi-v109 iareaofforestlandproposedtobediv ///
	bnoofsegments iinatureoftheproject


//Clean Variable Names
ren atotalareaoftheminingleaseinha area_mine_lease
ren amoeffileno moef_fileno
ren astatusoftheenvironmentalclearan ec_status 
ren atotalnumberoffamilies fam_num
ren awhetherprojectorapartthereofisl proj_in_pa
ren bareaofforestlandlocatedinthemin forest_area_in_mine
ren benvironmentalclearancefileno ec_fileno 
ren bnumberofscheduledcastefamilies sc_fam_disp
ren cnumberofscheduletribesfamilies st_fam_disp
ren dtotalareaoftheminingleaseinha area_mine_lease2
ren ddateofgrantofenvironmentalclear date_ec_granted
ren dnumberofotherfamilies other_fam_disp
ren eareaofforestlandlocatedinthemin forest_area_in_mine2
ren iproposalno prop_no
ren idateofapprovalofminingplan date_miningplan_approve
ren iinstalledpowergenerationcapacit power_cap
ren inoofminerals minerals_num
ren iproposeduseofthemineralspropose mineral_use
ren iwhetherprojectinvolvesdisplacem displacement
ren iwhetherdetailedprospectingtoass prospecting
ren iwhethertheprojectislikelytogene employment
ren iwhethertheprojectorapartthereof proj_in_pa_esz
ren iwhethertheprojectrequirescleara proj_clear_epa
ren iwhethertheprojectrequirescostbe proj_cba
ren iwhetherapprovalundertheforestco approval_prospecting
ren v41 proj_scheduledarea
ren iipermanentregularemploymentnumb perm_emp
ren iienvironmentalclearancefileno ec_fileno2
ren iistatusofapprovalofthestandingc status_sc_nbwl
ren iitotalcommandareaoftheprojectin command_area
ren iiitemporaryemploymentnumberofpe temp_emp
ren ivstate state 
ren ixnonforestlandrequiredforthispr proj_area_nonforest
ren vcategoryoftheproposal proj_category
ren vnatureofminingundergroundopenca mining_nature
ren vishapeofforestlandproposedtobed proj_shape
ren viiestimatedcostoftheprojectrupe proj_cost
ren viiiareaofforestlandproposedford proj_area_forest_div
ren xtotalperiodforwhichtheforestlan proj_time
ren xiiilegalstatusofuseragency ua_legal_status
ren (mineralwisedetailsminerals_0 mineralwisedetailsminerals_1 ///
	mineralwisedetailsminerals_2) (min_name_0 min_name_1 min_name_2)
ren (mineralwisedetailsestimatedlifeo v174 v175) (min_life_0 min_life_1 min_life_2)
ren (mineralwisedetailstotalestimated v183 v184) ///
	(min_total_est_extraction_0 min_total_est_extraction_1 min_total_est_extraction_2)
ren (mineralwisedetailsestimatedreser v165 v166) ///
	(min_est_reserve_nonforest_0 min_est_reserve_nonforest_1 min_est_reserve_nonforest_2)
ren (mineralwisedetailsestimatedannua v171 v172) ///
	(min_est_yr_extraction_0 min_est_yr_extraction_1 min_est_yr_extraction_2)
ren (v179 v180 v181) (min_est_reserve_forest_0 min_est_reserve_forest_1 min_est_reserve_forest_2) 	
order min_*, before(boreholesdrilledforprospectingno)


//Rename land-wise variables

**boreholes,  districts
foreach v of varlist boreholesdrilledforprospectingno-v139{
   local x : variable label `v'
   local y = subinstr("`x'", " ", "", .)
   local z = substr("`y'", 30, .)
   ren `v' `z'
}

**villages
foreach v of varlist villageswisebreakupvillage_0-v1396{
   local x : variable label `v'
   local y = subinstr("`x'", " ", "", .)
   local z = substr("`y'", 10, .)
   ren `v' `z'
}
ren gnoofboreholesinforestland* num_bh_forest*
ren gdiametersininchforestland* bh_diam_forest*
ren gdiametersininchnonforestland* bh_diam_nonforest*
ren gnoofboreholesinnonforestland* num_bh_nonforest*
ren me_* district_*
ren ha_* district_forest_*
ren andha_* district_nonforest_*
ren isebreakupvillage* village*
ren isebreakupforestlandha* village_forest*
ren isebreakupnonforestlandha* village_nonforest*

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
foreach var of varlist ec_status mineral_use displacement ///
	proj_in_pa_esz proj_clear_epa proj_cba proj_scheduledarea ///
	ua_legal_status employment  {
		replace `var' = "" if `var' == "nil"
	}
	
** zero
foreach var of varlist forest_area_in_mine* sc_fam_disp ///
	st_fam_disp other_fam_disp area_mine_lease2 power_cap ///
	minerals_num command_area proj_cost {
		replace `var' = "0" if `var' == "nil"
		destring `var', replace
		}
		
//Consolidate
replace area_mine_lease = area_mine_lease2 if area_mine_lease == .
drop area_mine_lease2
replace forest_area_in_mine = forest_area_in_mine2 if forest_area_in_mine == .
drop forest_area_in_mine
ren forest_area_in_mine2 forest_area_in_mine
replace ec_fileno = ec_fileno2 if ec_fileno == ""
drop ec_fileno2

//String-Numeric 
replace minerals_num = "1" if minerals_num == "one"
replace minerals_num = "2" if minerals_num == "two"
replace minerals_num = "3" if minerals_num == "three"
destring minerals_num, replace

//Encode Binary Variables
foreach var of varlist proj_in_pa displacement prospecting ///
	employment proj_in_pa_esz proj_clear_epa proj_cba ///
	approval_prospecting proj_scheduledarea {
		encode `var', gen(`var'_num)
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
** Reduce/Rename
keep proposal_no area_applied proposal_status date_of_recomm date_from_ua_to_nodal
ren (proposal_no area_applied proposal_status date_from_ua_to_nodal date_of_recomm) ///
	(prop_no proj_area_forest_div2 prop_status date_submitted date_recomm)
	
** Sync merge ID
replace prop_no=trim(itrim(lower(prop_no)))
duplicates drop prop_no, force

// Merge 
merge 1:1 prop_no using "`temp'"
drop if _merge == 1
drop _merge

//Format Dates
gen date_submit = date(date_submitted,"DM20Y")
gen date_rec = date(date_recomm, "DM20Y")
format date_submit date_rec %td

//Save
order state prop_no prop_status date_submit date_rec proj_area_forest_div* proj*
sort state prop_no
save "${DATA}/dta/fc_clean.dta", replace



