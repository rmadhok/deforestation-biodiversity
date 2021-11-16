********************************************************************************
*																			
* PROJECT: 	Deforestation and Biodiversity											
*																			
* PURPOSE: Clean pre-2014 FC data			    															
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
gl root 	"/Volumes/Backup Plus/research/data/def_biodiv/parivesh/"

*===============================================================================
* Generate File List
*===============================================================================

// Read raw data
import excel "${root}/fc_pre2014_raw.xlsx", firstrow clear
drop A page_no prop_name

* Clean strange characters
foreach v of varlist * {
	replace `v' = lower(`v')
	charlist `v' // get weird characters
	local tokill `r(sepchars)' 
	local good `c(alpha)' 0 1 2 3 4 5 6 7 8 9 : / . // keep a-z
	local tokill: list tokill - good
	foreach i of local tokill {
		replace `v' = subinstr(`v',`"`i'"',"",.)
	}
	replace `v' = strtrim(`v')
}
destring proj_area_forest, force replace

* Stage 1 and 2 dates
replace date_rec = subinstr(date_rec, " ", "", .)
foreach j in i ii {
	split date_rec, parse("stage`j':")
	drop date_rec1 date_rec3
	g date_s`j'_s = substr(date_rec2, 1, 9)
	g date_s`j' = date(date_s`j'_s, "DM20Y")
	format date_s`j' %td
	drop date_rec2
}
drop date_rec date_si_s date_sii_s
ren (date_si date_sii) (date_s1 date_s2)

* Post 2014 
keep if year(date_s2) >= 2014 // n = 1,854

* Save data entry sheet
export delimited "/Users/rmadhok/Dropbox/def_biodiv/data/raw/fc_pre2014_entry.csv", replace

