*********************************************************************************
*																			
* PROJECT: 	Deforestation and Biodiversity										
*																			
* PURPOSE: Construct ASI Panel	    
*																			
* START DATE: Jan 3 2019     				 							
*																			
* AUTHOR:  Raahil Madhok, madhokr@mail.ubc.ca											
*	
* Note: https://eands.dacnet.nic.in/AWIS.htm for more detailed stats.																			
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
gl ROOT 		"/Volumes/Backup Plus/research/def_biodiv"
gl READ_DATA 	"${ROOT}/asi"
gl SAVE_DATA	"/Users/rmadhok/Dropbox (Personal)/def_biodiv/data"

// Modules
local construct 	0
local fcast			1

*===============================================================================
* CONSTRUCT PANEL
*===============================================================================
if `construct' == 1 {
*----------------------------------
* 2014-15
*----------------------------------

// Read State Codes
use "${READ_DATA}/201415/blka201415", clear // n=63,296
ren *, lower

* Sync names 
ren (status_unit state multilplier) (unit_status state_cd multiplier)

* Keep open plants 
keep if unit_status == 1 // n=51,654

* Recode Telangana as AP
replace state_cd = "28" if state_cd == "36"
destring state_cd, replace

* Save
keep dsl state multiplier
tempfile state_cd_15
save "`state_cd_15'"

// Read Labour Section
use "${READ_DATA}/201415/blke201415", clear
ren *, lower

* Keep total employees
keep if sno == 9 // total employees = sum over M, F, contracts, managerials, other

* Daily Wage Rate
gen wage_rate = wages / mandayspaid

* Merge with State Code
merge 1:1 dsl using "`state_cd_15'", keep(3) nogen //n = 51,653

* Collapse to State Mean
collapse wage_rate, by(state_cd)
gen year = 2015

tempfile temp
save "`temp'"

*----------------------------------
* 2015-16
*----------------------------------

* Read
use "${READ_DATA}/201516/blkA201516", clear // n = 65,110
ren *, lower

* Keep Open Units
keep if unit_status == 1 // n = 51,750
replace state_cd = 28 if state_cd == 36 // recode telangana to AP
keep dsl state_cd multiplier
tempfile state_cd_16
save "`state_cd_16'"

* Read labour section
use "${READ_DATA}/201516/blkE201516", clear
ren *, lower
keep if s_no == 9

* Daily Wage Rate
gen wage_rate = wagessalariesrs / noofmandayspaid

* Get state code
merge 1:1 dsl using "`state_cd_16'", keep(3) nogen // n = 51,760

* Collapse to State Mean
collapse wage_rate, by(state_cd)
gen year = 2016

append using "`temp'"
save "`temp'", replace

*----------------------------------
* 2016-17
*----------------------------------

* Read
use "${READ_DATA}/201617/blkA201617", clear // n = 68,105
ren *, lower

* Keep Open Units
keep if unit_status == 1 // n = 54,250
replace state_cd = "28" if state_cd == "36"
ren mult multiplier
destring dsl state_cd, replace
keep dsl state_cd multiplier
tempfile state_cd_17
save "`state_cd_17'"

* Read labour section
use "${READ_DATA}/201617/blkE201617", clear
ren *, lower
keep if sno == 9

* Daily Wage Rate
gen wage_rate = wagessalary / noofmandayspaidfor

* Get state code
merge 1:1 dsl using "`state_cd_17'", keep(3) nogen // n = 54,250

* Collapse to State Mean
collapse wage_rate, by(state_cd)
gen year = 2017
append using "`temp'"

** SAVE
save "${SAVE_DATA}/dta/asi_wage_st_panel", replace

}

*===============================================================================
* FORECAST 2018
*===============================================================================

if `fcast' == 1 {
	
	* Read 2015-17 ASI
	use "${SAVE_DATA}/dta/asi_wage_st_panel", clear
	
	* Add 2018 cell
	expand 2 if year == 2017, gen(dup)
	replace year = 2018 if dup
	replace wage_rate = . if dup
	drop dup
	
	* Forecast
	sort state_cd year
	tab state_cd, gen(st_)
	reg wage_rate year st_2-st_33
	predict wage_rate_hat, xb
	replace wage_rate = wage_rate_hat if wage_rate == .
	drop wage_rate_hat st_*
	
	*----------------------------------
	* DEFLATE
	*----------------------------------
	* CPI Deflator taken from RBI Report
	* https://m.rbi.org.in/Scripts/AnnualReportPublications.aspx?Id=1273
	* Category: Consumer Price Index for Industrial Workers
	* Base year = 2001 
	
	gen wage_rate_d = wage_rate / 3.1 if year == 2018
	replace wage_rate_d = wage_rate / 4.1 if year == 2017
	replace wage_rate_d = wage_rate / 5.6 if year == 2016
	replace wage_rate_d = wage_rate / 6.3 if year == 2015
	
	* Save
	save "${SAVE_DATA}/dta/asi_wage_st_panel", replace

}




