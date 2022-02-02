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
ren date_s2 date_rec
ren st_displaced st_fam_disp
ren sc_displaced sc_fam_disp
ren total_displaced tot_fam_disp
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
drop if date_rec == "" // n=56 with stage-II date unknown
drop if district_1 == "" // n=4 with missing district (not found in report)
drop if regexm(district_1, "multi") | regexm(district_1, "mutli") // n=26 multidistrict w/ unknown pathway
drop if regexm(district_1, "report") | regexm(district_1, "access") | regexm(district_1, "2") // n=23 w/ no report
drop if regexm(district_1, "hindi") // n=14 reports in hindi
drop if regexm(district_1, "repeat")

* Form A
replace form_a = "yes" if regexm(form_a, "yes")

* Cost (very messy)
replace cost = "" if form_a == "no"
replace cost = subinstr(cost, "lakhs", "lakh", .)
replace cost = subinstr(cost, "crores", "crore", .)
replace cost = subinstr(cost, "rs.", "rupees", .)
replace cost = subinstr(cost, "rs", "rupees", .)
replace cost = subinstr(cost, ",", "", .)
replace cost = "NA" if !(regexm(cost, "rupees") | regexm(cost, "lakh") | regexm(cost, "crore"))  
split cost, parse(" ")
g temp = "rupees" if cost1 == "rupees"
replace cost1 = cost2 if cost1 == "rupees"
replace cost2 = "rupees" if temp == "rupees"
drop temp
replace cost1 = "NA" if cost3 !="" // n=12 complicated cost descriptions

destring cost1, force replace
replace cost1 = cost1 * 100000 if cost2 == "lakh"
replace cost1 = cost1 * 10000000 if cost2 == "crore"
replace cost1 = cost1 / 100000 // everything in lakhs
drop cost cost2-cost8
ren cost1 cost

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

// Mining (none)

*-------------------------------------------------------------------------------
* FINALIZE
*-------------------------------------------------------------------------------

* Dates
foreach v in submit rec {
	g d_`v' = date(date_`v', "DM20Y")
	drop date_`v'
	ren d_`v' date_`v'
	format date_`v' %td
}
drop proj_cat2

* Destring
destring proj_area_forest dist_f_*, replace

* Save (n=1732 projects)
order state prop_no date_submit date_rec proj_area_forest proj* cost tot_fam_disp
sort state prop_no
save "${SAVE}/dta/fc_pre2014_clean_v02.dta", replace






