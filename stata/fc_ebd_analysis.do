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
gl ROOT 	"/Users/rmadhok/Documents/ubc/research"
gl DATA 	"${ROOT}/def_biodiv/data"
gl TABLE	"${ROOT}/def_biodiv/docs/tex_doc/tables"

// Modules
local sumstats		1
local analysis		0

*===============================================================================
* SUMMARY STATISTICS
*===============================================================================
if `sumstats' == 1 {
	
	//Read
	use "${DATA}/dta/fc_ebd_master_5.dta", clear
	
	//1. Format Data for Display
	
	** Deforestation Districts
	bys c_code_2011: egen any_def = max(district_forest > 0 & district_forest!=.)
	
	** Tag Districts
	egen tag_d = tag(c_code_2011)
	
	** Label
	la var species_richness "Species Richness"
	la var shannon_index "Shannon Biodiversity Index"
	
	
	** Indent in latex markup
	foreach v of varlist district_forest district_forest_cum ///
		district_nonforest district_nonforest_cum ///
		approach_access_cum-wind_power_cum hybrid_cum-non_linear_cum ///
		species_richness shannon_index{ 
		
			label variable `v' `"\hspace{0.2cm} `: variable label `v''"'
			
		}
	
	** Collect Data
	local biodiversity species_richness shannon_index
	local deforestation district_forest district_nonforest ///
						district_forest_cum district_nonforest_cum
	unab proj_cat_num: approach_access_cum-wind_power_cum
	local proj_shape_num linear_cum non_linear_cum hybrid_cum
	
	//2. Summary Statistics
	
	** Biodiversity
	eststo clear
	eststo: estpost tabstat `biodiversity', s(n mean sd) c(s)
	eststo: estpost tabstat `biodiversity' if any_def, s(n mean sd) c(s)
	eststo: estpost tabstat `biodiversity' if !any_def, s(n mean sd) c(s)
	
	esttab using "${TABLE}/sumstats_biodiv.tex", replace main(mean) aux(sd) ///
		cells("mean(star fmt(2)) count(fmt(0))" "sd(par fmt(2))") ///
		width(\hsize) mgroups("All Districts" "Deforestation Districts" ///
		"\shortstack{Non-deforestation \\ Districts}", ///
		pattern(1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span ///
		erepeat(\cmidrule(lr){@span})) ///
		collabel("\specialcell{Mean}" "N", prefix({) suffix(})) ///
		nomtitle booktabs noobs label unstack nonumber
	
	** Deforestation
	eststo clear
	eststo: estpost tabstat `deforestation', s(n mean sd min p25 p50 p75 max) c(s)
	
	*local numbers "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\ \midrule"
	local titles "& {N} & {Mean} & {SD} & {Min} & {q25} & {q5} & {q75} & {Max} \\ \midrule"
	esttab using "${TABLE}/sumstats_deforest.tex", replace f ///	
		cell((count mean(fmt(%9.3f)) sd(fmt(%9.2f)) min(fmt(%9.0f)) p25(fmt(%9.0f)) ///
		p50(fmt(%9.0f)) p75(fmt(%9.0f)) max(fmt(%9.0f)))) width(\hsize) ///
		refcat(district_forest "\emph{District Land Diversion}", nolabel) ///
		posthead("`titles'") nonumbers nomtitles collabels(none) ///
		mlabels(none) booktabs noobs label plain gap substitute("$" "\$")
	
	eststo clear
	eststo: estpost tabstat `proj_cat_num', s(n mean sd min p25 p50 p75 max) c(s)
	
	esttab using "${TABLE}/sumstats_deforest.tex", append f ///
		cell((count mean(fmt(%9.3f)) sd(fmt(%9.2f)) min(fmt(%9.0f)) p25(fmt(%9.0f)) ///
		p50(fmt(%9.0f)) p75(fmt(%9.0f)) max(fmt(%9.0f)))) width(\hsize) ///
		refcat(approach_access_cum "\emph{District Project Counts}", nolabel) ///
		collabel(none) nomtitle booktabs noobs label nonumber plain gap substitute("$" "\$")
	
	eststo clear
	eststo: estpost tabstat `proj_shape_num', s(n mean sd min p25 p50 p75 max) c(s)
		
	esttab using "${TABLE}/sumstats_deforest.tex", append f ///
		cell((count mean(fmt(%9.3f)) sd(fmt(%9.2f)) min(fmt(%9.0f)) p25(fmt(%9.0f)) ///
		p50(fmt(%9.0f)) p75(fmt(%9.0f)) max(fmt(%9.0f)))) width(\hsize) ///
		refcat(linear_cum "\emph{District Project Shape Counts}", nolabel) ///
		collabel(none) nomtitle booktabs noobs label plain gap substitute("$" "\$")
	
	
	** Stich
	tex3pt "${TABLE}/sumstats_deforest.tex" using ///
		"${TABLE}/sumstats_deforestation.tex", ///
		replace floatplacement(htp) ///
		title(Deforestation Descriptive Statistics) wide
		
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
		
		** SE Clusters
		local se_clust s_y
	
	
		//1. Correlation
		eststo: qui reg `1' `2', robust cluster(`se_clust')
		
			test _b[`2'] = 0 
			estadd scalar p = r(p), replace
			estadd scalar nclust `e(N_clust)'
			qui sum `1' if e(sample)
			estadd scalar mean_y = r(mean)
			estadd local dist_fe ""
			estadd local month_fe ""
			estadd local st_y_fe ""
			estadd local clust "State $\times$ Year"
		
		//2. District FE
		eststo: qui reghdfe `1' `2', a(c_code_2011_num) vce(cl `se_clust')
		
			test _b[`2'] = 0 
			estadd scalar p = r(p), replace
			estadd scalar nclust `e(N_clust)'
			qui sum `1' if e(sample)
			estadd scalar mean_y = r(mean)
			estadd local dist_fe "Yes"
			estadd local month_fe ""
			estadd local st_y_fe ""
			estadd local clust "State $\times$ Year"
		
		
		//3. District, State-Year FE
		eststo: qui reghdfe `1' `2', ///
			a(c_code_2011_num state_code_2011_num#year) vce(cl `se_clust')
		
			test _b[`2'] = 0 
			estadd scalar p = r(p), replace
			estadd scalar nclust `e(N_clust)'
			qui sum `1' if e(sample)
			estadd scalar mean_y = r(mean)
			estadd local dist_fe "Yes"
			estadd local month_fe ""
			estadd local st_y_fe "Yes"
			estadd local clust "State $\times$ Year"
		
		//4. District, Month, State-Year FE
		eststo: qui reghdfe `1' `2', ///
			a(c_code_2011_num month state_code_2011_num#year) vce(cl `se_clust')
		
			test _b[`2'] = 0 
			estadd scalar p = r(p), replace
			estadd scalar nclust `e(N_clust)'
			qui sum `1' if e(sample)
			estadd scalar mean_y = r(mean)
			estadd local dist_fe "Yes"
			estadd local month_fe "Yes"
			estadd local st_y_fe "Yes"
			estadd local clust "State $\times$ Year"
		
		* Regression table
		local numbers "& (1) & (2) & (3) & (4) \\ \midrule"
		
		esttab using "${TABLE}/table_`1'_`2'.tex", se star(* .1 ** .05 *** .01) ///
			label r2 replace wrap booktabs nonotes nocons ///
			mlabels(none) nonumbers posthead("`numbers'") ///
			addnotes( "$\sym{*}~p<$ .1, $\sym{**}~p<$ .05, $\sym{***}~p<$.01") ///
			stats(dist_fe st_y_fe month_fe N clust nclust r2 p, ///
			labels(`"District FEs"' `"State $\times$ Year FEs"' `"Month FEs"' ///
			`"N"' `"SE Clusters"' `"Clusters"' `"\(R^{2}\)"' `"\(p\)-value"') ///
			fmt(0 0 0 0 0 0 3 3)) b(%5.3f) se(%5.3f) substitute("$" "\$")
	end
	
	
	//Read Data
	use "${DATA}/dta/fc_ebd_master_5.dta", clear
	
	//Cluster
	egen s_y = group(state_code_2011_num year)
	
	//Analysis

	** Deforestation
	reg_table species_richness district_forest_km2
	reg_table shannon_index district_forest_km2
	
	** Cumulative Deforestation
	reg_table species_richness district_forest_cum_km2
	reg_table shannon_index district_forest_cum_km2
	
	** Placebo
	reg_table species_richness district_nonforest_km2
	reg_table shannon_index district_nonforest_km2
	
	reg_table species_richness district_nonforest_cum_km2
	reg_table shannon_index district_nonforest_cum_km2
	
}


















