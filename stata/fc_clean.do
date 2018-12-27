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
*import delimited using "${DATA}/csv/fc_raw_use.csv", encoding("utf-8")  clear
*save "${DATA}/dta/fc_temp.dta", replace
use "${DATA}/dta/fc_temp.dta", replace

//reduce varlist size
drop avillage adateofsubmissionofproposals anoofpatches ///
	atotalareaofsafetyzoneoftheminin bareainha bcurrentstatus ///
	bdegradedforestareaonwhichcahast bstatusofapprovalofthesupremecou ///
	cdateofapproval epresentowner iwhethertheforestlandisaquiredun ///
	iiwhethertheareaofnonforestlando iiinoofdistrictsinvolvedforraisi ///
	ivgender ivnoofpatches estimatedreservealongwithaccurac-v163 ///
	bareaofnonforestorrevenueforestl iwhethernonforestorrevenueforest ///
	mineralwisedetailsestimatedminer v177 v178 iiapprovalauthority


//Clean Project Varnames
ren atotalareaoftheminingleaseinha area_mine_lease
ren amoeffileno moef_fileno
ren astatusoftheenvironmentalclearan ec_status 
ren atotalnumberoffamilies fam_num
ren awhetherprojectorapartthereofisl proj_in_pa
ren bareaofforestlandlocatedinthemin forest_area_in_mine
ren benvironmentalclearancefileno ec_fileno 
ren bnoofsegments segments_num
ren bnumberofscheduledcastefamilies sc_fam_disp
ren cnumberofscheduletribesfamilies st_fam_disp
ren dtotalareaoftheminingleaseinha area_mine_lease2
ren ddateofgrantofenvironmentalclear date_ec_granted
ren dnumberofotherfamilies other_fam_disp
ren eareaofforestlandlocatedinthemin forest_area_in_mine2
ren iproposalno prop_no
ren idateofapprovalofminingplan date_miningplan_approve
ren iareaofforestlandproposedtobediv proj_area_forest_div
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
ren iinatureoftheproject proj_nature
ren iistatusofapprovalofthestandingc status_sc_nbwl
ren iitotalcommandareaoftheprojectin command_area
ren iiitemporaryemploymentnumberofpe temp_emp
ren ivstate state 
ren ixnonforestlandrequiredforthispr proj_area_nonforest
ren vcategoryoftheproposal proj_category
ren vnatureofminingundergroundopenca mining_nature
ren vishapeofforestlandproposedtobed proj_shape
ren viiestimatedcostoftheprojectrupe proj_cost
ren viiiareaofforestlandproposedford proj_area_forest_div2
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
**boreholes, divisions, districts
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
ren isionname* division*
ren estlandha* divsion_forest*
ren forestlandha* division_nonforest*
ren me_* district_*
ren ha_* district_forest_*
ren andha_* district_nonforest_*
ren isebreakupvillage* village*
ren isebreakupforestlandha* village_forest*
ren isebreakupnonforestlandha* village_nonforest*

*===============================================================================
*DATA CLEANING
*===============================================================================
//Drop rows with all missing
egen nmcount = rownonmiss(_all), strok
drop if nmcount == 0
drop nmcount

//trim strings
ds, has(type string) 
local strvars "`r(varlist)'"
foreach var of local strvars{
	replace `var'=trim(itrim(lower(`var')))
	}	

//Clean NIL values
//missing
foreach var of varlist ec_status mineral_use displacement ///
	proj_in_pa_esz proj_clear_epa proj_cba proj_scheduledarea ///
	ua_legal_status employment  {
		replace `var' = "" if `var' == "nil"
	}
//zero
foreach var of varlist forest_area_in_mine* sc_fam_disp ///
	st_fam_disp other_fam_disp area_mine_lease2 power_cap ///
	minerals_num command_area proj_cost {
		replace `var' = "0" if `var' == "nil"
		destring `var', replace
		}
		
//consolidate variables
replace area_mine_lease = area_mine_lease2 if area_mine_lease == .
drop area_mine_lease2
replace forest_area_in_mine = forest_area_in_mine2 if forest_area_in_mine == .
drop forest_area_in_mine
replace ec_fileno = ec_fileno2 if ec_fileno == ""
drop ec_fileno2






