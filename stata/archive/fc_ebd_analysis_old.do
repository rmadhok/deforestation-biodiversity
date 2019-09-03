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
	
	//Regression Program
	capture program drop reg_table
	program define reg_table
		
		** Syntax
		syntax varlist
		set more off
		eststo clear
		
		** Parse
		local depvar : word 1 of `varlist'
		di "`depvar'"
		local indepvar : word 2 of `varlist'
		di "`indepvar'"
		local controls = substr("`varlist'", length("`depvar'") + length("`indepvar'") + 3, length("`varlist'"))
		di "`controls'"
	
		** SE Clusters
		local se_clust clust
		
		//1. Correlation + Controls
		eststo: qui reg `depvar' `indepvar' `controls' [aweight=n_trips], robust cluster(`se_clust')
		
			test _b[`indepvar'] = 0 
			estadd scalar p = r(p), replace
			estadd scalar nclust `e(N_clust)'
			qui sum `depvar' if e(sample)
			estadd scalar mean_y = r(mean)
			estadd local dist_fe ""
			estadd local smonth_fe ""
			estadd local year_fe ""
			estadd local st_y_fe ""
			estadd local clust "State $\times$ Year"
		
		//2. District FE
		eststo: qui reghdfe `depvar' `indepvar' `controls' [aweight=n_trips], ///
			a(c_code_2011_num) vce(cl `se_clust')
		
			test _b[`indepvar'] = 0 
			estadd scalar p = r(p), replace
			estadd scalar nclust `e(N_clust)'
			qui sum `depvar' if e(sample)
			estadd scalar mean_y = r(mean)
			estadd local dist_fe "$\checkmark$"
			estadd local smonth_fe ""
			estadd local year_fe ""
			estadd local st_y_fe ""
			estadd local clust "State $\times$ Year"
		
		
		//3. District, State-Year FE
		eststo: qui reghdfe `depvar' `indepvar' `controls' [aweight=n_trips], ///
			a(c_code_2011_num state_code_2011_num#year) vce(cl `se_clust')
		
			test _b[`indepvar'] = 0 
			estadd scalar p = r(p), replace
			estadd scalar nclust `e(N_clust)'
			qui sum `depvar' if e(sample)
			estadd scalar mean_y = r(mean)
			estadd local dist_fe "$\checkmark$"
			estadd local smonth_fe ""
			estadd local year_fe ""
			estadd local st_y_fe "$\checkmark$"
			estadd local clust "State $\times$ Year"
		
	
		//4. District, State-month, Year FE
		eststo: qui reghdfe `depvar' `indepvar' `controls' [aweight=n_trips], ///
			a(c_code_2011_num state_code_2011_num#month year) vce(cl `se_clust')
		
			test _b[`2'] = 0 
			estadd scalar p = r(p), replace
			estadd scalar nclust `e(N_clust)'
			qui sum `1' if e(sample)
			estadd scalar mean_y = r(mean)
			estadd local dist_fe "$\checkmark$"
			estadd local smonth_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
			estadd local st_y_fe ""
			estadd local clust "State $\times$ Year"
		
		
		* Regression table
		local numbers "& (1) & (2) & (3) & (4) \\ \midrule"
		
		esttab using "${TABLE}/tables/table_`1'_`2'.tex", se star(* .1 ** .05 *** .01) ///
			label r2 replace wrap booktabs nonotes nocons ///
			mlabels(none) nonumbers posthead("`numbers'") ///
			stats(dist_fe st_y_fe smonth_fe year_fe N clust nclust r2 p, ///
			labels(`"District FEs"' `"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
			`"Year FE"' `"N"' `"SE Clusters"' `"Clusters"' `"\(R^{2}\)"' `"\(p\)-value"') ///
			fmt(0 0 0 0 0 0 0 3 3)) b(%5.3f) se(%5.3f) substitute("$" "\$") ///
			addnotes("$\sym{*}~p<$ .1, $\sym{**}~p<$ .05, $\sym{***}~p<$.01.")
		eststo clear
	
	end
	
	/*-----------------
	ANALYSIS
	------------------*/
	
	//1. ALL PROJECTS
	
	* Set Control Vectors	
	local ctrls coverage temperature_mean precipitation_mean
	local ctrls_ihs coverage_ihs temperature_mean_ihs precipitation_mean_ihs
		
	* Read
	use "${DATA}/dta/fc_ebd.dta", clear
	*drop if state_code_2011_num == 1 //Drop J&K
		
	egen clust = group(state_code_2011_num year)
	
	** All Projects
	
	reg_table species_richness_mean district_forest_cum_km2 district_nonforest_cum_km2 ca_district_forest_cum_km2 `ctrls'
	reg_table shannon_index_mean district_forest_cum_km2 district_nonforest_cum_km2 ca_district_forest_cum_km2 `ctrls'
	reg_table simpson_index_mean district_forest_cum_km2 district_nonforest_cum_km2 ca_district_forest_cum_km2 `ctrls'
	
	reg_table species_richness_mean_ihs district_forest_cum_ihs district_nonforest_cum_ihs ca_district_forest_cum_ihs `ctrls_ihs'
	reg_table shannon_index_mean_ihs district_forest_cum_ihs district_nonforest_cum_ihs ca_district_forest_cum_ihs `ctrls_ihs'
	reg_table simpson_index_mean_ihs district_forest_cum_ihs district_nonforest_cum_ihs ca_district_forest_cum_ihs `ctrls_ihs'
	
	* Birding Activity
	reg_table n_birders district_forest_cum_km2 district_nonforest_cum_km2 ca_district_forest_cum_km2 temperature_mean precipitation_mean
	reg_table n_trips district_forest_cum_km2 district_nonforest_cum_km2 ca_district_forest_cum_km2 temperature_mean precipitation_mean
	reg_table effort_distance_km_mean district_forest_cum_km2 district_nonforest_cum_km2 ca_district_forest_cum_km2 temperature_mean precipitation_mean
	reg_table duration_min_mean district_forest_cum_km2 district_nonforest_cum_km2 ca_district_forest_cum_km2 temperature_mean precipitation_mean
		
	//2. By Project Type
	
	foreach depvar of varlist species_richness_mean_ihs shannon_index_mean_ihs simpson_index_mean_ihs {
	
		eststo: qui reghdfe `depvar' district_forest_elec_cum_ihs district_nonforest_elec_cum_ihs ca_district_forest_elec_cum_ihs `ctrls_ihs' [aweight=n_trips], ///
				a(c_code_2011_num state_code_2011_num#month year) vce(cl state_code_2011_num#year)
	
			}
	esttab using "${TABLE}/tables/table_projtype.tex", replace f wrap ///
		keep(district_forest_elec_cum_ihs) label noobs booktabs nonotes ///
		nocons nomtitles mlabels("{Species Richness}" "{Shannon}" "{Simpson}") ///
		b(%5.3f) se star(* .1 ** .05 *** .01) se(%5.3f) substitute("$" "\$")
	eststo clear
	
	foreach i in ind irr mine o tran pa hyb lin nl {
		
		foreach depvar of varlist species_richness_mean_ihs shannon_index_mean_ihs simpson_index_mean_ihs {
			
			eststo: qui reghdfe `depvar' district_forest_`i'_cum_ihs district_nonforest_`i'_cum_ihs ca_district_forest_`i'_cum_ihs `ctrls_ihs' [aweight=n_trips], ///
				a(c_code_2011_num state_code_2011_num#month year) vce(cl state_code_2011_num#year)
	
				}	
		esttab using "${TABLE}/tables/table_projtype.tex", append f ///
			keep(district_forest_`i'_cum_ihs) noobs wrap label booktabs ///
			nonotes nocons nolines mlabels(none) nonumbers se b(%5.3f) ///
			se(%5.3f) star(* .1 ** .05 *** .01) substitute("," "")
		eststo clear
	}
		
	foreach depvar of varlist species_richness_mean_ihs shannon_index_mean_ihs simpson_index_mean_ihs {
	
		eststo: qui reghdfe `depvar' district_forest_fvr_cum_ihs district_nonforest_fvr_cum_ihs `ctrls_ihs' [aweight=n_trips], ///
				a(c_code_2011_num state_code_2011_num#month year) vce(cl state_code_2011_num#year)
	
			test _b[district_forest_fvr_cum_ihs] = 0 
			estadd scalar p = r(p), replace
			estadd scalar nclust `e(N_clust)'
			qui sum `depvar' if e(sample)
			estadd scalar mean_y = r(mean)
			estadd local dist_fe "$\checkmark$"
			estadd local smonth_fe "$\checkmark$"
			estadd local year_fe "$\checkmark$"
			estadd local st_y_fe ""
			estadd local clust "{State $\times$ Year}"
	}
	esttab using "${TABLE}/tables/table_projtype.tex", append f ///
		wrap keep(district_forest_fvr_cum_ihs) label r2 booktabs nonotes ///
		nocons nomtitles nolines stats(dist_fe st_y_fe smonth_fe year_fe N clust nclust r2, ///
		labels(`"District FEs"' `"State $\times$ Year FEs"' `"State $\times$ Month FEs"' ///
		`"Year FE"' `"N"' `"SE Clusters"' `"Clusters"' `"\(R^{2}\)"') ///
		fmt(0 0 0 0 0 0 0 3)) b(%5.3f) se star(* .1 ** .05 *** .01) se(%5.3f) ///
		substitute("," "") prefoot(\hline)
	eststo clear
			
	tex3pt "${TABLE}/tables/table_projtype.tex" using "${TABLE}/tables/stacktable_projtype.tex", ///
		replace wide fontsize(small) land ///
		title("Impact of Deforestation on Species Diversity by Project Type (2015-2019)") ///
		note("$\sym{*}~p<$ .1, $\sym{**}~p<$ .05, $\sym{***}~p<$.01." ///
		"All specifications include controls for spatial coverage, mean temperature, and mean rainfall.")
	
	//3. Mean vs. Median
	
	foreach depvar in species_richness shannon_index simpson_index {
		
		foreach stat in mean md {
		
			eststo: qui reghdfe `depvar'_`stat'_ihs district_forest_cum_ihs ///
				district_nonforest_cum_ihs ca_district_forest_cum_ihs ///
				`ctrls_ihs' [aweight=n_trips], ///
				a(c_code_2011_num state_code_2011_num#month year) ///
				vce(cl state_code_2011_num#year)
	
				test _b[district_forest_cum_ihs] = 0 
				estadd scalar p = r(p), replace
				estadd scalar nclust `e(N_clust)'
				estadd local dist_fe "$\checkmark$"
				estadd local smonth_fe "$\checkmark$"
				estadd local year_fe "$\checkmark$"
				estadd local clust "{State $\times$ Year}"
			}
		}
	
	esttab using "${TABLE}/tables/table_mean_md.tex", replace wrap nocons ///
		stats(dist_fe smonth_fe year_fe N clust ///
		nclust r2 p, labels(`"District FEs"' `"State $\times$ Month FEs"' ///
		`"Year FEs"' `"N"' `"SE Clusters"' `"Clusters"' `"\(R^{2}\)"' ///
		`"\$p\$-value"') fmt(0 0 0 0 0 0 3 3)) label ///
		mgroups("Species Richness" "Shannon Index" "Simpson Index", ///
		pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span ///
		erepeat(\cmidrule(lr){@span})) mlabel("Mean" "Median" "Mean" "Median" ///
		"Mean" "Median") nomtitles star(* .1 ** .05 *** .01) ///
		nonotes booktabs se b(%5.3f) se(%5.3f) width(\hsize)
	eststo clear
	
	//4. Lags
	
	foreach depvar in species_richness shannon_index simpson_index {
		
		foreach stat in mean md {
			
			* Model
			eststo: qui reghdfe `depvar'_`stat'_ihs district_forest_cum_ihs ///
				district_nonforest_cum_ihs ca_district_forest_cum_ihs ///
				`ctrls_ihs' [aweight=n_trips], ///
				a(c_code_2011_num state_code_2011_num#month year) ///
				vce(cl state_code_2011_num#year)
			
			* Store Coeff
			gen `depvar'_`stat'_df_b_l0 = _b[district_forest_cum_ihs]
			
			* Store CI
			gen `depvar'_`stat'_df_ciu_l0 = _b[district_forest_cum_ihs] - invttail(e(df_r),0.025)*_se[district_forest_cum_ihs]
			gen `depvar'_`stat'_df_cil_l0 = _b[district_forest_cum_ihs] + invttail(e(df_r),0.025)*_se[district_forest_cum_ihs]
			
			}
		}
	esttab using "${TABLE}/tables/table_lags.tex", replace f wrap ///
		keep(district_forest_cum_ihs) stats(N, labels("N") fmt(0)) ///
		label booktabs nonotes nocons nomtitles nolines posthead(\hline) ///
		mgroups("Species Richness" "Shannon Index" "Simpson Index", ///
		pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span ///
		erepeat(\cmidrule(lr){@span})) mlabel("{Mean}" "{Median}" "{Mean}" "{Median}" ///
		"{Mean}" "{Median}") b(%5.3f) se star(* .1 ** .05 *** .01) ///
		se(%5.3f) substitute("," "")
	eststo clear
	
	
	foreach i of numlist 1/12 {
		
		foreach depvar in species_richness shannon_index simpson_index {
			
			foreach stat in mean md {
		
				* Model
				eststo: qui reghdfe `depvar'_`stat'_ihs district_forest_cum_ihs_l`i' ///
					district_nonforest_cum_ihs_l`i' ca_district_forest_cum_ihs_l`i' ///
					`ctrls_ihs' [aweight=n_trips], ///
					a(c_code_2011_num state_code_2011_num#month year) ///
					vce(cl state_code_2011_num#year)
					
				* Store Coeff
				gen `depvar'_`stat'_df_b_l`i' = _b[district_forest_cum_ihs_l`i']
			
				* Store CI
				gen `depvar'_`stat'_df_ciu_l`i' = _b[district_forest_cum_ihs_l`i'] - invttail(e(df_r),0.025)*_se[district_forest_cum_ihs_l`i']
				gen `depvar'_`stat'_df_cil_l`i' = _b[district_forest_cum_ihs_l`i'] + invttail(e(df_r),0.025)*_se[district_forest_cum_ihs_l`i']
	
				}
			}
		
		esttab using "${TABLE}/tables/table_lags.tex", append f wrap ///
			keep(district_forest_cum_ihs_l`i') label booktabs /// 
			stats(N, labels("N") fmt(0))  nolines posthead(\hline) ///
			nonotes nocons mlabels(none) nonumbers se b(%5.3f) ///
			se(%5.3f) star(* .1 ** .05 *** .01) substitute("," "")
		eststo clear
	}
			
	tex3pt "${TABLE}/tables/table_lags.tex" using "${TABLE}/tables/table_lagspec.tex", ///
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
		
		reshape long species_richness_mean_df_b_l species_richness_md_df_b_l ///
			species_richness_mean_df_ciu_l species_richness_md_df_ciu_l ///
			species_richness_mean_df_cil_l species_richness_md_df_cil_l ///
			shannon_index_mean_df_b_l shannon_index_md_df_b_l ///
			shannon_index_mean_df_ciu_l shannon_index_md_df_ciu_l ///
			shannon_index_mean_df_cil_l shannon_index_md_df_cil_l ///
			simpson_index_mean_df_b_l simpson_index_md_df_b_l ///
			simpson_index_mean_df_ciu_l simpson_index_md_df_ciu_l ///
			simpson_index_mean_df_cil_l simpson_index_md_df_cil_l, ///
			i(id) j(lag)
			
		ren *_l *
		
		* Plot
		foreach var in species_richness simpson_index {
			
			foreach stat in mean md {
		
				twoway rarea `var'_`stat'_df_ciu `var'_`stat'_df_cil lag, ///
						sort color(gs14) || ///
					scatter `var'_`stat'_df_b lag || ///
					function y = 0, range(lag) color(gs8) ///
						xtitle("Lag (months)") xlabel(0(1)12) ///
						graphregion(color(white)) bgcolor(white) ///
						legend(order(1 "95% CI" 2 "Coefficient"))
			
				graph export "${TABLE}/fig/`var'_`stat'_df_lag.png", replace
			
			}
		}
	
	restore
	
		

}



