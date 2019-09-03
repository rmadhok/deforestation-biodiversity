*********************************************************************************
*																			
* PROJECT: 	Deforestation and Biodiversity										
*																			
* PURPOSE: 	Summary Tables	    
*																			
* START DATE: January 7, 2019    				 							
*																			
* AUTHOR:  Raahil Madhok, madhokr@mail.ubc.ca											
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
set matsize 10000

//Set Directory Paths
gl ROOT 	"/Users/rmadhok/Dropbox (Personal)/def_biodiv"
gl DATA 	"${ROOT}/data"
gl TABLE	"${ROOT}/docs/tex_doc/"

// Modules
local sumstats		0
local plots			0
local analysis		1

*===============================================================================
* SUMMARY STATISTICS
*===============================================================================
if `sumstats' == 1 {
	
	//Read
	use "${DATA}/dta/fc_ebd.dta", clear
	
	//1. Format Data for Display
	
	** Deforestation Districts
	bys c_code_2011: egen any_def = max(district_forest > 0 & district_forest!=.)
	
	** Tag Districts
	egen tag_d = tag(c_code_2011)
	
	** Indent in latex markup
	
	* Label
	la var district_forest_cum "All Projects"
	la var district_forest_elec_cum "Electricity"
	la var district_forest_fvr_cum "Forest Village Relocation"
	la var district_forest_ind_cum "Industry"
	la var district_forest_irr_cum "Irrigation"
	la var district_forest_mine_cum "Mining"
	la var district_forest_o_cum "Other"
	la var district_forest_tran_cum "Transport"
	la var district_forest_hyb_cum "Hybrid"
	la var district_forest_lin_cum "Linear"
	la var district_forest_nl_cum "Non-Linear"
	la var district_forest_pa_cum "In Protected Area"
	
	foreach v of varlist district_forest_cum district_forest_*_cum ///
		tot_area s_* sh_* si_* pop_density coverage* n_hotspots ///
		distance_mean duration_mean n_birders n_trips { 
		
			label variable `v' `"\hspace{0.2cm} `: variable label `v''"'
			
		}
	
	** Collect Data
	local dist_vars tot_area pop_density coverage_all n_hotspots
	local trip_details n_birders n_trips distance_mean duration_mean
	local biodiversity s_richness_mean s_richness_d sh_index_mean sh_index_d si_index_mean si_index_d
	unab deforestation: district_forest_cum district_forest_*_cum
	
	//2. Summary Statistics
	
	** District level
	eststo clear
	eststo A: estpost tabstat `dist_vars' if tag_d, s(n mean sd) c(s)
	eststo B: estpost tabstat `dist_vars' if tag_d & any_def, s(n mean sd) c(s)
	eststo C: estpost tabstat `dist_vars' if tag_d & !any_def, s(n mean sd) c(s)
	esttab A B C using "${TABLE}/tables/sumstats_biodiv.tex", replace f main(mean) aux(sd) ///
		cells("mean(star fmt(2)) count(fmt(0))" "sd(par fmt(2))") ///
		width(\hsize) mgroups("All Districts" "Deforestation Districts" ///
		"\shortstack{Non-deforestation \\ Districts}", ///
		pattern(1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span ///
		erepeat(\cmidrule(lr){@span})) ///
		collabel("\specialcell{Mean}" "N", prefix({) suffix(})) ///
		refcat(tot_area "\emph{District Variables}" , nolabel) ///
		nomtitle booktabs noobs label unstack nonumber gap
		
	** Trip Details
	eststo clear
	eststo A: estpost tabstat `trip_details', s(n mean sd) c(s)
	eststo B: estpost tabstat `trip_details' if any_def, s(n mean sd) c(s)
	eststo C: estpost tabstat `trip_details' if !any_def, s(n mean sd) c(s)
	
	esttab A B C using "${TABLE}/tables/sumstats_biodiv.tex", append f main(mean) aux(sd) ///
		cells("mean(star fmt(2)) count(fmt(0))" "sd(par fmt(2))") ///
		width(\hsize) refcat(n_birders "\emph{Trip Details}" , nolabel) ///
		nomtitle collabel(none) booktabs noobs label unstack nonumber plain gap
	
	** Biodiversity
	eststo A: estpost tabstat `biodiversity', s(n mean sd) c(s)
	eststo B: estpost tabstat `biodiversity' if any_def, s(n mean sd) c(s)
	eststo C: estpost tabstat `biodiversity' if !any_def, s(n mean sd) c(s)
	
	esttab A B C using "${TABLE}/tables/sumstats_biodiv.tex", append f main(mean) aux(sd) ///
		cells("mean(star fmt(2)) count(fmt(0))" "sd(par fmt(2))") ///
		width(\hsize) refcat(s_richness_mean "\emph{Biodiversity}" , nolabel) ///
		nomtitle collabel(none) booktabs noobs label unstack nonumber plain gap
	
	** Stitch
	tex3pt "${TABLE}/tables/sumstats_biodiv.tex" using ///
		"${TABLE}/tables/sumstats_biodiversity.tex", ///
		replace floatplacement(!htpb) wide ///
		title(Biodiversity Descriptive Statistics (2014-2018)) ///
		note("Note: Spatial coverage is the fraction of grid cells in a district" ///
		"containing at least one bird observation over the study period using a" ///
		"10km $\times$ 10km grid") 	
	
	** Deforestation
	eststo clear
	eststo: estpost tabstat `deforestation' if any_def, s(n mean sd p50 min max) c(s)
	
	local numbers "& (1) & (2) & (3) & (4) & (5) & (6) \\ \midrule"
	local titles "& {N} & {Mean} & {SD} & {p50} & {Min} & {Max} \\"
	esttab using "${TABLE}/tables/sumstats_deforest.tex", replace ///	
		cell((count mean(fmt(%9.2f)) sd(fmt(%9.2f)) p50(fmt(%9.0f)) min(fmt(%9.0f)) ///
		max(fmt(%9.0f)))) width(\hsize) ///
		refcat(district_forest_elec_cum "\emph{By Project Type}" ///
		district_forest_hyb_cum "\emph{By Project Shape}", nolabel) ///
		posthead("`titles'" "`numbers'") nonumbers nomtitles collabels(none) ///
		mlabels(none) booktabs noobs gap label substitute("$" "\$")
		
}

*===============================================================================
* PLOTS
*===============================================================================
if `plots' == 1 {

	//Read
	use "${DATA}/dta/fc_ebd_master_5.dta", clear
	drop if year == 2014
	
	** Deforestation Districts
	bys c_code_2011: egen any_def = max(district_forest > 0 & district_forest!=.)
	
	label define labvar 1 "Development Districts" 0 "Non-Development Districts"
	label values any_def labvar
	
	** Bar
	cibar shannon_index, over1(any_def) ///
		bargap(3) barcolor(forest_green brown) ciopts(lcolor(black)) ///
		graphopts(title("Bird Species Diversity in Development and" "Non-Development Districts") ///
		ytitle(Mean Shannon Index) graphregion(color(white)) bgcolor(white) ///
		note(Note: 458 development districts and 182 non-development districts))
		
	graph export "/Users/rmadhok/Documents/ubc/research/def_biodiv/docs/conferences/lfs_conference/presentation/img/biodiv.png", replace
	
	** State-Year
	preserve
	
		statsby mean_sr=r(mean) ub=r(ub) lb=r(lb), by(state year any_def) clear: ci mean shannon_index

		scatter mean_sr year if state=="Andhra Pradesh" & any_def == 0, ms(O) mcolor(green) || ///
				rcap ub lb year if state=="Andhra Pradesh" & any_def == 0, lcolor(green) || /// 
		scatter mean_sr year if state=="Andhra Pradesh" & any_def == 1, ms(O) mcolor(brown) || ///
				rcap ub lb year if state=="Andhra Pradesh" & any_def == 1, lcolor(brown) ///
		title("Bird Species Diversity in Development and" "Non-Development Districts of Andhra Pradesh State") ///
		ytitle("Mean Shannon Index") xtitle("Year") ///
		graphregion(color(white)) bgcolor(white) ///
		legend(label(1 "Non-Development Districts") label(2 "95% CI") label(3 "Development Districts") label(4 "95% CI")) ///
		note(Note: 11 districts with no development; 12 districts with development)
		
		graph export "/Users/rmadhok/Documents/ubc/research/def_biodiv/docs/conferences/lfs_conference/presentation/img/andhra.png", replace
		
	
	restore

	** District
	preserve 
	
		statsby mean_sr=r(mean) ub=r(ub) lb=r(lb), by(state district year any_def) clear: ci mean shannon_index
		
		scatter mean_sr year if district=="Rangareddy", ms(O) mcolor(green) || ///
				rcap ub lb year if district=="Rangareddy", lcolor(green) || ///
		scatter mean_sr year if district=="Chittoor", ms(O) mcolor(brown) || ///
				rcap ub lb year if district=="Chittoor", lcolor(brown) ///
		title("Bird Species Diversity in Two Districts" "of Andhra Pradesh State") ///
		ytitle("Mean Shannon Index") xtitle("Year") ///
		graphregion(color(white)) bgcolor(white) ///
		legend(label(1 "Non-Development District") label(2 "95% CI") ///
		label(3 "Development District") label(4 "95% CI")) ///
		note(Note: mean over 12 months)
		
		graph export "/Users/rmadhok/Documents/ubc/research/def_biodiv/docs/conferences/lfs_conference/presentation/img/two_districts.png", replace
				
	restore
			
}

*===============================================================================
* ANALYSIS
*===============================================================================
if `analysis' == 1 {
	
	//1. Species Diversity on Deforestation

	use "${DATA}/dta/fc_ebd.dta", clear
	local controls district_nonforest_cum_ihs coverage_ihs temperature_mean_ihs precipitation_mean_ihs
	
	** Per User and All User
	foreach var in s_richness sh_index si_index {
		
		foreach j in mean d {
			
			* District FE
			eststo: qui reghdfe `var'_`j'_ihs district_forest_cum_ihs `controls', ///
				a(c_code_2011_num) vce(cl state_code_2011_num#year)
				
				test _b[district_forest_cum_ihs] = 0 
				estadd scalar p = r(p), replace
				estadd scalar nclust `e(N_clust)'
				estadd local dist_fe "$\checkmark$"
				estadd local smonth_fe ""
				estadd local year_fe ""
				estadd local st_y_fe ""
				estadd local clust "State $\times$ Year"
			
			* State-Year FE
			eststo: qui reghdfe `var'_`j'_ihs district_forest_cum_ihs `controls', ///
				a(c_code_2011_num state_code_2011_num#year) ///
				vce(cl state_code_2011_num#year)
				
				test _b[district_forest_cum_ihs] = 0 
				estadd scalar p = r(p), replace
				estadd scalar nclust `e(N_clust)'
				estadd local dist_fe "$\checkmark$"
				estadd local smonth_fe ""
				estadd local year_fe ""
				estadd local st_y_fe "$\checkmark$"
				estadd local clust "State $\times$ Year"
				
			* State-Month, Year FE
			eststo: qui reghdfe `var'_`j'_ihs district_forest_cum_ihs `controls', ///
				a(c_code_2011_num state_code_2011_num#month year) ///
				vce(cl state_code_2011_num#year)
				
				test _b[district_forest_cum_ihs] = 0 
				estadd scalar p = r(p), replace
				estadd scalar nclust `e(N_clust)'
				estadd local dist_fe "$\checkmark$"
				estadd local smonth_fe "$\checkmark$"
				estadd local year_fe "$\checkmark$"
				estadd local st_y_fe ""
				estadd local clust "State $\times$ Year"
			}
			
		esttab using "${TABLE}/tables/`var'_deforest.tex", replace ///
			wrap nocons stats(dist_fe st_y_fe smonth_fe year_fe N clust ///
			nclust r2 p, labels(`"District FEs"' `"State $\times$ Year FEs"' ///
			`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"SE Clusters"' ///
			`"Clusters"' `"\(R^{2}\)"' `"\(p\)-value"') fmt(0 0 0 0 0 0 0 3 3)) ///
			mgroups("Per Trip" "All Trips", pattern(1 0 0 1 0 0) ///
			prefix(\multicolumn{@span}{c}{) suffix(}) span ///
			erepeat(\cmidrule(lr){@span})) nomtitles star(* .1 ** .05 *** .01) ///
			label nonotes booktabs se b(%5.3f) se(%5.3f) width(\hsize)
		eststo clear
	}
	
	** OLS AND WLS
	foreach j in mean d {
		
		foreach var in s_richness sh_index si_index {
		
			* OLS
			eststo: qui reghdfe `var'_`j'_ihs district_forest_cum_ihs `controls', ///
				a(c_code_2011_num state_code_2011_num#month year) ///
				vce(cl state_code_2011_num#year)
				
				test _b[district_forest_cum_ihs] = 0 
				estadd scalar p = r(p), replace
				estadd scalar nclust `e(N_clust)'
				estadd local dist_fe "$\checkmark$"
				estadd local smonth_fe "$\checkmark$"
				estadd local year_fe "$\checkmark$"
				estadd local st_y_fe ""
				estadd local clust "State $\times$ Year"
			
			* WLS
			eststo: qui reghdfe `var'_`j'_ihs district_forest_cum_ihs ///
				`controls'[aweight = n_trips], ///
				a(c_code_2011_num state_code_2011_num#month year) ///
				vce(cl state_code_2011_num#year)
				
				test _b[district_forest_cum_ihs] = 0 
				estadd scalar p = r(p), replace
				estadd scalar nclust `e(N_clust)'
				estadd local dist_fe "$\checkmark$"
				estadd local smonth_fe "$\checkmark$"
				estadd local year_fe "$\checkmark$"
				estadd local st_y_fe ""
				estadd local clust "State $\times$ Year"
			}
		
		esttab using "${TABLE}/tables/`j'_deforest_weighted.tex", replace ///
			wrap nocons stats(dist_fe st_y_fe smonth_fe year_fe N clust ///
			nclust r2 p, labels(`"District FEs"' `"State $\times$ Year FEs"' ///
			`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"SE Clusters"' ///
			`"Clusters"' `"\(R^{2}\)"' `"\(p\)-value"') fmt(0 0 0 0 0 0 0 3 3)) ///
			mgroups("Species Richness" "Shannon Index" "Simpson Index", ///
			pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span ///
			erepeat(\cmidrule(lr){@span})) mlabels("OLS" "WLS" "OLS" "WLS" "OLS" "WLS") ///
			nomtitles star(* .1 ** .05 *** .01) label nonotes booktabs ///
			se b(%5.3f) se(%5.3f) width(\hsize)
		eststo clear	
	}
		
	//2. Species Diversity on Deforestation by Project Type
	
	foreach j in mean d {
	
		* Electricity
		foreach var in s_richness sh_index si_index {
	
			eststo: qui reghdfe `var'_`j'_ihs district_forest_elec_cum_ihs ///
				district_nonforest_elec_cum_ihs `controls', ///
				a(c_code_2011_num state_code_2011_num#month year) ///
				vce(cl state_code_2011_num#year)
			eststo: qui reghdfe `var'_`j'_ihs district_forest_elec_cum_ihs ///
				district_nonforest_elec_cum_ihs `controls' [aweight=n_trips], ///
				a(c_code_2011_num state_code_2011_num#month year) ///
				vce(cl state_code_2011_num#year)
			}
			esttab using "${TABLE}/tables/`j'_forest_projectwise.tex", replace ///
				f wrap keep(district_forest_elec_cum_ihs) ///
				mgroups("Species Richness" "Shannon Index" "Simpson Index", ///
				pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span ///
				erepeat(\cmidrule(lr){@span})) mlabels("OLS" "WLS" "OLS" "WLS" "OLS" "WLS") ///
				label noobs booktabs nonotes nocons nomtitles b(%5.3f) se ///
				star(* .1 ** .05 *** .01) se(%5.3f) 
			eststo clear
	
		* All Other Types
		foreach i in ind irr mine o tran fvr pa hyb lin {
		
			foreach var in s_richness sh_index si_index {
			
				eststo: qui reghdfe `var'_`j'_ihs district_forest_`i'_cum_ihs ///
					district_nonforest_`i'_cum_ihs `controls', ///
					a(c_code_2011_num state_code_2011_num#month year) ///
					vce(cl state_code_2011_num#year)
				eststo: qui reghdfe `var'_`j'_ihs district_forest_`i'_cum_ihs ///
					district_nonforest_`i'_cum_ihs `controls' [aweight=n_trips], ///
					a(c_code_2011_num state_code_2011_num#month year) ///
					vce(cl state_code_2011_num#year)
					}	
			esttab using "${TABLE}/tables/`j'_forest_projectwise.tex", append f ///
				keep(district_forest_`i'_cum_ihs) noobs wrap label booktabs ///
				nomtitles nonotes nocons nolines mlabels(none) nonumbers se ///
				b(%5.3f) se(%5.3f) star(* .1 ** .05 *** .01)
			eststo clear
		}
		
		* Non-Linear
		foreach var in s_richness sh_index si_index {
	
			eststo: qui reghdfe `var'_`j'_ihs district_forest_nl_cum_ihs ///
				district_nonforest_nl_cum_ihs `controls', ///
				a(c_code_2011_num state_code_2011_num#month year) ///
				vce(cl state_code_2011_num#year)
				
				test _b[district_forest_nl_cum_ihs] = 0 
				estadd scalar nclust `e(N_clust)'
				estadd local dist_fe "$\checkmark$"
				estadd local smonth_fe "$\checkmark$"
				estadd local year_fe "$\checkmark$"
				estadd local clust "{State $\times$ Year}"
			
			eststo: qui reghdfe `var'_`j'_ihs district_forest_nl_cum_ihs ///
				district_nonforest_nl_cum_ihs `controls' [aweight=n_trips], ///
				a(c_code_2011_num state_code_2011_num#month year) ///
				vce(cl state_code_2011_num#year)
	
				test _b[district_forest_nl_cum_ihs] = 0 
				estadd scalar nclust `e(N_clust)'
				estadd local dist_fe "$\checkmark$"
				estadd local smonth_fe "$\checkmark$"
				estadd local year_fe "$\checkmark$"
				estadd local clust "{State $\times$ Year}"
			}
		esttab using "${TABLE}/tables/`j'_forest_projectwise.tex", append f ///
			wrap keep(district_forest_nl_cum_ihs) stats(dist_fe smonth_fe ///
			year_fe N clust nclust r2, labels(`"District FEs"' ///
			`"State $\times$ Month FEs"' `"Year FE"' `"N"' `"SE Clusters"' ///
			`"Clusters"' `"\(R^{2}\)"') fmt(0 0 0 0 0 0 3)) label r2 booktabs ///
			nonotes nocons nomtitles nolines mlabels(none) b(%5.3f) se ///
			star(* .1 ** .05 *** .01) se(%5.3f) prefoot(\hline)
		eststo clear
	
	tex3pt "${TABLE}/tables/`j'_forest_projectwise.tex" using "${TABLE}/tables/`j'_deforest_projwise.tex", ///
		replace wide fontsize(small) land ///
		title("Impact of Deforestation on Species Diversity by Project Type (2015-2019)") ///
		note("$\sym{*}~p<$ .1, $\sym{**}~p<$ .05, $\sym{***}~p<$.01." ///
		"All specifications include controls for spatial coverage, mean temperature, and mean rainfall.")
	}
	
	//3. Lags
	
	foreach j in mean d {
	
		foreach var in s_richness sh_index si_index {
			
			* Model
			eststo: qui reghdfe `var'_`j'_ihs district_forest_cum_ihs ///
				district_nonforest_cum_ihs `controls' [aweight=n_trips], ///
				a(c_code_2011_num state_code_2011_num#month year) ///
				vce(cl state_code_2011_num#year)
			
			* Store Coeff
			gen `var'_df_`j'_b_l0 = _b[district_forest_cum_ihs]
			
			* Store CI
			gen `var'_df_`j'_ciu_l0 = _b[district_forest_cum_ihs] - invttail(e(df_r),0.025)*_se[district_forest_cum_ihs]
			gen `var'_df_`j'_cil_l0 = _b[district_forest_cum_ihs] + invttail(e(df_r),0.025)*_se[district_forest_cum_ihs]
			
			}
		}
		esttab using "${TABLE}/tables/bio_def_lag.tex", replace f wrap ///
			keep(district_forest_cum_ihs) stats(N, labels("N") fmt(0)) ///
			mgroups("Per Trip" "All Trips", ///
			pattern(1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span ///
			erepeat(\cmidrule(lr){@span})) mlabels("{Species Richness}" ///
			"{Shannon Index}" "{Simpson Index}" "{Species Richness}" ///
			"{Shannon Index}" "{Simpson Index}") label booktabs nonotes nocons ///
			nomtitles nolines posthead(\hline) b(%5.3f) se ///
			star(* .1 ** .05 *** .01) se(%5.3f) 
		eststo clear
	
	foreach i of numlist 1/12 {
		
		foreach j in mean d {
	
			foreach var in s_richness sh_index si_index {
		
				* Model
				eststo: qui reghdfe `var'_`j'_ihs district_forest_cum_ihs_l`i' ///
					district_nonforest_cum_ihs_l`i' `controls' [aweight=n_trips], ///
					a(c_code_2011_num state_code_2011_num#month year) ///
					vce(cl state_code_2011_num#year)
					
				* Store Coeff
				gen `var'_df_`j'_b_l`i' = _b[district_forest_cum_ihs_l`i']
			
				* Store CI
				gen `var'_df_`j'_ciu_l`i' = _b[district_forest_cum_ihs_l`i'] - invttail(e(df_r),0.025)*_se[district_forest_cum_ihs_l`i']
				gen `var'_df_`j'_cil_l`i' = _b[district_forest_cum_ihs_l`i'] + invttail(e(df_r),0.025)*_se[district_forest_cum_ihs_l`i']
	
				}
			}
		
		esttab using "${TABLE}/tables/bio_def_lag.tex", append f wrap ///
			keep(district_forest_cum_ihs_l`i') label booktabs /// 
			stats(N, labels("N") fmt(0))  nolines posthead(\hline) ///
			nonotes nocons mlabels(none) nonumbers se b(%5.3f) ///
			se(%5.3f) star(* .1 ** .05 *** .01)
		eststo clear
	}
			
	tex3pt "${TABLE}/tables/bio_def_lag.tex" using "${TABLE}/tables/bio_def_lagspec.tex.tex", ///
		replace wide fontsize(small) ///
		title("Impact of Lagged Deforestation on Species Diversity (2015-2019)") ///
		note("$\sym{*}~p<$ .1, $\sym{**}~p<$ .05, $\sym{***}~p<$.01." ///
		"All specifications include district, state-month, and year fixed effects." ///
		"All regressions include controls for spatial coverage, mean temperature, and mean rainfall.")

	** Coefficient Plot
	preserve
		
		* Prep
		keep *_df*l*
		keep if _n == 1
		gen id = _n
		
		reshape long s_richness_df_mean_b_l s_richness_df_d_b_l ///
			s_richness_df_mean_ciu_l s_richness_df_d_ciu_l ///
			s_richness_df_mean_cil_l s_richness_df_d_cil_l ///
			sh_index_df_mean_b_l sh_index_df_d_b_l ///
			sh_index_df_mean_ciu_l sh_index_df_d_ciu_l ///
			sh_index_df_mean_cil_l sh_index_df_d_cil_l ///
			si_index_df_mean_b_l si_index_df_d_b_l ///
			si_index_df_mean_ciu_l si_index_df_d_ciu_l ///
			si_index_df_mean_cil_l si_index_df_d_cil_l, ///
			i(id) j(lag)
			
		ren *_l *
		
		* Plot
		foreach var in s_richness si_index {
			
			foreach j in mean d {
		
				twoway rarea `var'_df_`j'_ciu `var'_df_`j'_cil lag, ///
						sort color(gs14) || ///
					scatter `var'_df_`j'_b lag || ///
					function y = 0, range(lag) color(gs8) ///
						xtitle("Lag (months)") xlabel(0(1)12) ///
						graphregion(color(white)) bgcolor(white) ///
						legend(order(1 "95% CI" 2 "Coefficient"))
			
				graph export "${TABLE}/fig/`var'_`j'_df_lag.png", replace
			
			}
		}
	
	restore
	
}



