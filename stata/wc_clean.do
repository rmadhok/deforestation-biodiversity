********************************************************************************
*																			
* PROJECT: 	Deforestation and Biodiversity											
*																			
* PURPOSE: Clean raw wildlife clearance data			    
*																			
* START DATE: November 13, 2018																	
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

//Set Directory Paths
gl ROOT 	"/Users/rmadhok/Documents/ubc/research"
gl DATA 	"${ROOT}/def_biodiv/data"

*===============================================================================
*CLEAN VARIABLE NAMES
*===============================================================================
//Read raw data
import delimited using "${DATA}/csv/wc_raw_use.csv", encoding("utf-8")  clear

//Clean varnames
ren apermanentregularemploymentnumbe perm_emp
ren anoofpatches patches_num
ren astatusoftheenvironmentalclearan ec_status
ren atotalnumberoffamilies fam_num
ren acopyofthebiodiversityimpactasse biodiv_ia
ren aforestsclearancetype fc_type
ren bareaofprotectedlandlocatedinthe pa_area_mining
ren bdateofissueoftheletterofintentl datevar_loi
ren bnumberofscheduledcastefamilies sc_disp_num
ren btemporaryemploymentnumberofpers temp_emp 
ren benvironmentalclearancefileno ec_fileno
ren cnumberofscheduletribesfamilies st_disp_num
ren dtotalareaoftheminingleaseinha area_mine_lease
ren ddateofgrantofenvironmentalclear date_ec_granted
ren dnumberofotherfamilies fam_disp_num
ren eareaofprotectedlandlocatedinthe pa_area_mining2
ren idateofapprovalofminingplan date_mining_plan_approve
ren iforestclearancerequired fc_required
ren iinstalledpowergenerationcapacit power_cap
ren inoofminerals minerals_num 
ren iprojectareaunderprotectedareain proj_area_pa
ren iproposeduseofthemineralspropose mineral_use
ren iwhetherdetailedprospectingtoass prospecting
ren iwhetherprojectinvolvedisplaceme displacement
ren iwhetherprojectislikelytogenerat employment
ren iwhethertheprojectrequirescleara ec_required
ren iiproposalno prop_no
ren iinatureoftheproject proj_nature
ren iitotalculturablecommandareaofth proj_command_area
ren iienvironmentalclearancefileno ec_fileno2
ren iiinoofsegments segments_num
ren ixestimatedcostoftheprojectrupee proj_cost
ren vstate state
ren vnatureofminingundergroundopenca mining_nature
ren vicategoryoftheproject proj_cat
ren viishapeofprojectland proj_shape
ren viiidistanceoftheprojectfromtheb proj_dist_pa
ren xtotalperiodforwhichclearanceisr wc_time_yrs
ren xitotalprojectareainha proj_area
ren xiiareaofforestlandrequiredforth proj_area_forest
ren xiiprojectareaunderprotectedarea proj_area_pa2
ren xiiinonforestlandrequiredforthis proj_area_nonforest
ren xiiiprojectareaundernonprotected proj_area_nonpa
ren xiiilegalstatusofuseragency ua_legal_status
ren xivforestlandwithinprotectedarea proj_area_forest_pa
ren xivprojectareaoutsidewildlifesan proj_area_out_ws
ren xixprojectareainsideecologically proj_area_in_esz
ren xvnonforestlandwithinprotectedar proj_area_nonforest_pa
ren xvprojectareainsideconservationr proj_area_in_cr
ren xviprojectareaoutsideconservatio proj_area_out_cr
ren xviprojectareaunderprotectedarea proj_area_pa3
ren xviiprojectareainsidenationalpar proj_area_in_np
ren xviiprojectareaundernonprotected proj_area_nonpa2
ren xviiiprojectareainsidewildlifesa proj_area_in_ws
ren xviiiprojectareaoutsidenationalp proj_area_out_np
ren xxprojectareaoutsideecologically proj_area_out_esz
ren (boreholesdrilledforprospectingno v58) (num_bh_forest_0 num_bh_forest_1)
ren (boreholesdrilledforprospectingdi v60) (bh_diam_forest_0 bh_diam_forest_1)
ren (v61 v62) (bh_diam_nonforest_0 bh_diam_nonforest_1)
ren (v63 v64) (num_bh_nonforest_0 num_bh_nonforest_1)
ren (divisionwisedetailsoflandphysica v166 v167 v168 v169 v170) ///
	(div_disturb_0 div_disturb_1 div_disturb_2 div_disturb_3 div_disturb_4 div_disturb_5) 
ren (mineralwisedetailsestimatednonpr v176 mineralwisedetailsminerals_0 ///
	mineralwisedetailsminerals_1 mineralwisedetailsestimatedprote v180 ///
	mineralwisedetailsestimatedannua v182 mineralwisedetailsestimatedlifet ///
	v184 mineralwisedetailstotalestimated v188) ///
	(min_nonpa_amt_0 min_nonpa_amt_1 min_name_0 min_name_1 ///
	 min_pa_amt_0 min_pa_amt_1 min_extract_yr_0 min_extract_yr_1 ///
	 min_mine_ltime_0 min_mine_ltime_1 min_extract_tot_0 min_extract_tot_1)
	 
//Rename land-wise variables
foreach v of varlist componentwisebreakupprojectareau-v164{
   local x : variable label `v'
   local y = subinstr("`x'", " ", "", .)
   local z = substr("`y'", 30, .)
   ren `v' `z'
}
ren componentwisebreakupcomponent* comp_name*
ren eaundernonprotectedareaha* component_area_nonpa*
ren eaunderprotectedareaha* component_area_pa*
ren me_# dist_name_#
ren aundernonprotectedareaha* dist_proj_area_nonpa*
ren aunderprotectedareaha* dist_proj_area_pa*
ren sionname* div_name*
ren requiredindays* div_time*
ren ectareaunderprotectedarea* div_proj_area_pa*
ren ectedareaname* div_pa_name*

//Timeline 
ren (chiefwildlifewarden nbwlmoefccwl querybywildlifewardenforsubmitti ///
	 queryforshortcomingifanybywildli resubmissionofproposalbyuseragen ///
	 stategovernmentsbwl submittedbyuseragency uploadingbyuaofcopiesofreceiptre ///
	 wildlifewarden) ///
	 (datevar_cww_7 datevar_nbwl_9 datevar_query_ww_4 datevar_query_ww_2 datevar_resubmit_ua_3 ///
	 datevar_sbwl_8 datevar_submit_ua_1 datevar_upload_ua_5 datevar_ww_6)
	 
*===============================================================================
*DATA MANAGEMENT
*===============================================================================
//Drop unnecessary
drop estimatedreservealongwithaccurac v172 v173 v174 ///
	 mineralwisedetailsestimatedminer v186 biodiv_ia
//Drop rows with all missing
egen nmcount = rownonmiss(_all), strok
drop if nmcount == 1 | nmcount == 3

//consolidate variables
replace pa_area_mining = pa_area_mining2 if pa_area_mining== ""
replace ec_fileno = ec_fileno2 if ec_fileno == ""
drop pa_area_mining2 ec_fileno2 nmcount

//trim strings
ds, has(type string) 
local strvars "`r(varlist)'"
foreach var of local strvars{
	replace `var'=trim(itrim(lower(`var')))
	}

//String to numeric
foreach var of varlist patches_num minerals_num segment_num{
	replace `var' = "1" if `var' == "one"
	replace `var' = "2" if `var' == "two"
	replace `var' = "3" if `var' == "three"
	replace `var' = "4" if `var' == "four"
	replace `var' = "5" if `var' == "five"
	replace `var' = "6" if `var' == "six"
	replace `var' = "7" if `var' == "seven"
	replace `var' = "8" if `var' == "eight"
	replace `var' = "9" if `var' == "nine"
	replace `var' = "10" if `var' == "ten"
	replace `var' = "11" if `var' == "eleven"
	}

//Replace NIL
foreach var of varlist perm_emp patches_num pa_area_mining sc_disp_num ///
	temp_emp st_disp_num area_mine_lease fam_disp_num power_cap minerals_num ///
	proj_area_pa proj_command_area segment_num proj_area_forest_pa ///
	proj_area_out_ws proj_area_in_esz proj_area_nonforest_pa ///
	proj_area_in_cr proj_area_out_cr proj_area_in_ws proj_area_out_esz ///
	num_bh_forest_0 bh_diam_nonforest_0{
		replace `var' = "0" if `var' == "nil" | `var' == "na"
		destring `var', replace
		}
		
//encode categorical vars
foreach var of varlist mineral_use displacement employment ec_required ///
	proj_nature proj_shape{
		replace `var' = "" if `var' == "nil"
		}
	
foreach var of varlist ec_status fc_required mineral_use prospecting ///
	displacement employment ec_required proj_nature mining_nature ///
	proj_cat proj_shape ua_legal_status {
		encode `var', gen(`var'_code)
		}
		
//Handle dates
gen date_submit_ua_1 = date(datevar_submit_ua_1,"DMY")
gen date_loi = date(datevar_loi, "DMY")
format date_submit_ua_1 date_loi %td

*===============================================================================
*PREPARE FINAL DATASET
*===============================================================================
//Order variables
order state prop_no date_submit_ua_1 ua_legal_status_code ///
	proj_cat proj_area_pa proj_area_in_esz proj_area_in_ws proj_area_in_np ///
	proj_area_in_cr proj_area_forest proj_area proj_dist_pa proj_cost proj_* ///
	area_mine_lease mining_nature displacement employment dist_*
sort state date_submit_ua_1
	
//export
export delimited using "${DATA}/csv/wc_clean.csv", replace
save "${DATA}/dta/wc_clean.dta", replace






