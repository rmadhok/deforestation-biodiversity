*********************************************************************************
*																			
* PROJECT: Deforestation and Biodiversity										
*																		
* PURPOSE: Benefits of projects  				 							
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
set matsize 10000

//Set Directory Paths
gl ROOT 	"/Users/rmadhok/Dropbox/def_biodiv"
gl DATA 	"${ROOT}/data"
gl TABLE	"${ROOT}/docs/jmp/tex_doc/"
cd "${TABLE}"

*-------------------------------------------------------------------------------
* FIRST STAGE
*-------------------------------------------------------------------------------

* Read
use "${DATA}/dta/benefits_iv_v02", clear

* Label
la var tot_pop_ha "Pop/ha."
la var sc_pop_ha "SC/ha."
la var st_pop_ha "ST/ha."
la var iv_tot_pop_ha "St Def'n $\times$ Pop/ha."
la var iv_st_pop_ha "St Def'n $\times$ ST/ha."
la var iv_sc_pop_ha "St Def'n $\times$ SC/ha."
la var iv_scst_pop_ha "St Def'n $\times$ Dalits/ha."
la var st_f_cum_km2_p "St Def'n" 
la var sc_pop_p "St Def'n $\times$ SC Pop."
la var st_pop_p "St Def'n $\times$ ST Pop."

* 1. Double interactions
foreach v of varlist tot_pop_ha sc_pop_ha st_pop_ha scst_pop_ha scst_pop_d_ha scst_pop_cfr_ha {
	
	eststo: reghdfe dist_f_cum_km2 iv_`v' st_f_cum_km2_p, ///
		a(c_code_2011_num state_code_2011_num#month year) vce(cl c_code_2011_num)
	
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
}

esttab using "${TABLE}/v3/tables/benefits_fra.tex", keep(iv_*) replace ///
	stats(dist_fe st_m_fe year_fe N r2, labels(`"District FEs"' ///
	`"State x Month FEs"' `"Year FEs"' `"N"' `"\(R^{2}\)"') ///
	fmt(0 0 0 0 3)) wrap nocons nonotes booktabs nomtitles ///
	star(* .1 ** .05 *** .01) label se b(%5.3f) width(\hsize)
eststo clear

* 2. Triple Interactions
la var iv_tot_pop_ha_r_seats "St Def'n $\times$ Pop/ha. $\times$ Reserved"
la var iv_sc_pop_ha_r_seats "St Def'n $\times$ SC/ha. $\times$ Reserved"
la var iv_st_pop_ha_r_seats "St Def'n $\times$ ST/ha. $\times$ Reserved"
la var iv_scst_pop_ha_r_seats "St Def'n $\times$ Dalits/ha. $\times$ Reserved"
la var tot_pop_ha_r_seats "Pop/ha. $\times$ Reserved"
la var st_pop_ha_r_seats "ST/ha. $\times$ Reserved"
la var sc_pop_ha_r_seats "SC/ha. $\times$ Reserved"

foreach v of varlist tot_pop_ha sc_pop_ha st_pop_ha scst_pop_ha scst_pop_d_ha scst_pop_cfr_ha {
	
	eststo: reghdfe dist_f_cum_km2 iv_`v'_r_seats iv_`v' iv_r_seats `v'_r_seats ///
		st_f_cum_km2_p r_seats st_pop_p, a(c_code_2011_num state_code_2011_num#month year) ///
		vce(cl c_code_2011_num)
	
		estadd local dist_fe "$\checkmark$"
		estadd local st_m_fe "$\checkmark$"
		estadd local year_fe "$\checkmark$"
}

esttab using "${TABLE}/v3/tables/benefits_fra_seats.tex", keep(iv_*_r_seats) replace ///
	stats(dist_fe st_m_fe year_fe N r2, labels(`"District FEs"' ///
	`"State x Month FEs"' `"Year FEs"' `"N"' `"\(R^{2}\)"') ///
	fmt(0 0 0 0 3)) wrap nocons nonotes booktabs nomtitles ///
	star(* .1 ** .05 *** .01) label se b(%5.3f) width(\hsize)
eststo clear


* BENEFITS
g ln_rad_mean = log(rad_mean)
g ln_rad_sum = log(rad_sum)
ivreghdfe ln_rad_sum (dist_f_cum_km2 = iv_scst_pop_cfr_ha) st_f_cum_km2_p s*_pop_p, ///
		a(c_code_2011_num state_code_2011_num#month year) cluster(c_code_2011_num)
ll
ivreghdfe ln_rad_mean (dist_f_cum_km2 = iv_tot_pop_ha_r_seats) iv_tot_pop_ha ///
	iv_r_seats tot_pop_ha_r_seats r_seats st_f_cum_km2_p s*_pop_p, ///
		a(c_code_2011_num state_code_2011_num#month year) cluster(c_code_2011_num)
kk

// NOTE: population per ha is significant in double interaction. negative for sc and tot pop.
// positive (null) for st pop. But with election interaction it becomes negative (use factor notation)

// same thing with st pop in forest vil (f_rev_st_pop); null in double interaction, negative significant in triple

// same with sc_pop_dist_ha

// same with f_vill (negative, null in triple interaction)






