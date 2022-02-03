*********************************************************************************
*																			
* PROJECT: 	Deforestation and Biodiversity										
*																			
* PURPOSE: 	Clean Pre-2014 projects
*																			 				 																									
* AUTHOR:  Raahil Madhok, madhokr@mail.ubc.ca											
*																				
********************************************************************************
*-------------------------------------------------------------------------------
*SET ENVIRONMENT
*-------------------------------------------------------------------------------
// Settings
clear all
pause on
cap log close
set more off
set maxvar 10000

//Set Directory Paths
gl READ		"/Users/rmadhok/Dropbox/def_biodiv/data"
cd "${READ}"

*-------------------------------------------------------------------------------
* PREP
*-------------------------------------------------------------------------------

* Read pre-2014 data entry
import excel using "./raw/fc_pre2014_dataentry.xlsx", first allstring clear
drop cost_comment prop_status AM-AP date_s1

* Rename
replace prop_no = subinstr(prop_no, "view for rediversion", "", .)
ren date_s2 date_recommended
ren district_name_* district_*
ren district_area_* dist_f_*
*-------------------------------------------------------------------------------
* CLEAN
*-------------------------------------------------------------------------------

* Strings
foreach v of varlist * {
	replace `v' = trim(itrim(lower(`v')))
}

* Remove
drop if date_recommended == "" // n=56 with stage-II date unknown
drop if district_0 == "" // n=4 with missing district (not found in report)
drop if regexm(district_0, "multi") | regexm(district_0, "mutli") // n=26 multidistrict w/ unknown pathway
drop if regexm(district_0, "report") | regexm(district_0, "access") | regexm(district_0, "2") // n=23 w/ no report
drop if regexm(district_0, "hindi") // n=14 reports in hindi
drop if regexm(district_0, "repeat")

* Form A
replace form_a = "yes" if regexm(form_a, "yes")

* Cost (very messy)
replace proj_cost = "" if form_a == "no"
replace proj_cost = subinstr(proj_cost, "lakhs", "lakh", .)
replace proj_cost = subinstr(proj_cost, "crores", "crore", .)
replace proj_cost = subinstr(proj_cost, "rs.", "rupees", .)
replace proj_cost = subinstr(proj_cost, "rs", "rupees", .)
replace proj_cost = subinstr(proj_cost, ",", "", .)
replace proj_cost = "NA" if !(regexm(proj_cost, "rupees") | regexm(proj_cost, "lakh") | regexm(proj_cost, "crore"))  
split proj_cost, parse(" ")
g temp = "rupees" if proj_cost1 == "rupees"
replace proj_cost1 = proj_cost2 if proj_cost1 == "rupees"
replace proj_cost2 = "rupees" if temp == "rupees"
drop temp
replace proj_cost1 = "NA" if proj_cost3 !="" // n=12 complicated cost descriptions

destring proj_cost1, force replace
replace proj_cost1 = proj_cost1 * 100000 if proj_cost2 == "lakh"
replace proj_cost1 = proj_cost1 * 10000000 if proj_cost2 == "crore"
replace proj_cost1 = proj_cost1 / 100000 // everything in lakhs
drop proj_cost proj_cost2-proj_cost8
ren proj_cost1 proj_cost // sync with post-2014 varname

* Employment (skip for now)
drop employment 

* Displacement
drop st_fam_disp sc_fam_disp // add back if you want. messy entry, easier to work with total
replace tot_fam_disp = "" if form_a == "no"
replace tot_fam_disp = "0" if form_a == "yes" & inlist(tot_fam_disp, "", "n/a", "no data")
split tot_fam_disp, parse(" ")
drop tot_fam_disp3-tot_fam_disp7 tot_fam_disp
ren tot_fam_disp1 tot_fam_disp
replace tot_fam_disp2 = "households" if inlist(tot_fam_disp2, "families", "houses")
destring tot_fam_disp, replace
replace tot_fam_disp = tot_fam_disp / 4.97 if tot_fam_disp2 == "people" // mean hh size = 4.97 (Census 2011); mean ST hh size = 4.77 (IHDS 2012)
drop tot_fam_disp2

* EC
replace ec_needed = "" if form_a == "no"
replace ec_needed = "no" if inlist(ec_needed, "n/a")
replace ec_needed = "" if ec_needed == "no data"
drop ec_obtained

*-------------------------------------------------------------------------------
* PROJECT CATEGORIZATION
*-------------------------------------------------------------------------------

* Categories
gen proj_cat = "electricity" if inlist(proj_category, "hydel", "thermal", "transmission line", "wind power")
replace proj_cat = "transportation" if inlist(proj_category, "road", "railway") // approach road part of "other"
replace proj_cat = "irrigation" if inlist(proj_category, "irrigation") 
replace proj_cat = "resettlement" if inlist(proj_category, "forest village conversion", "rehabilitation")
replace proj_cat = "mining" if inlist(proj_category, "mining", "quarrying", "borehole prospecting")
replace proj_cat = "industry" if proj_category == "industry"
replace proj_cat = "other" if proj_cat == ""

* Recategorize "other"
// Electricity
replace proj_cat = "electricity" if proj_cat == "other" & (regexm(proj_cat2, "elec") | inlist(proj_cat2, "hydel", "other (atomic power project)", "other (dam)", "other (transmission line)"))

// Transport
replace proj_cat = "transportation" if proj_cat == "other" & (regexm(proj_cat2, "passage") | inlist(proj_cat2, "other (road?)", "other (service road)", "other (up gradation of road)", "other (widening road)", "road", "road (bridge)") | regexm(proj_cat2, "transport")) 

// Irrigation
replace proj_cat = "irrigation" if proj_cat == "other" & regexm(proj_cat2, "canal")

// Resettlement
replace proj_cat = "resettlement" if proj_cat == "other" & proj_cat2 == "resettlement"

// Industry
replace proj_cat = "industry" if proj_cat == "other" & inlist(proj_cat2, "industry (steel)", "other (cement plant)", "other (chemical factory)", "other (factory)", "other (industrial model)", "other (mill)", "other (steel plant)")

// Mining (none)
drop proj_cat2
*-------------------------------------------------------------------------------
* FINALIZE
*-------------------------------------------------------------------------------

* Dates
g date_submit = date(date_submitted, "DM20Y") 
g date_rec = date(date_recommended, "DM20Y")
format date_submit date_rec %td

* Encode
g displacement_num = (tot_fam_disp > 0)
replace displacement_num = . if tot_fam_disp == .
g ec_needed_num = (ec_needed == "yes")
replace ec_needed_num = . if ec_needed == ""

* Destring
destring proj_area_forest2 dist_f_*, replace

* Save (n=1732 projects)
order state prop_no date_submit date_rec proj* tot_fam_disp
sort state prop_no
g pre2014 = 1
save "${SAVE}/dta/fc_pre2014_clean_v02.dta", replace
