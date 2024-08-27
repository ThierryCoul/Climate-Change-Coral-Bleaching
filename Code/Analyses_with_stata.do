cd "Path to the input file."

import delimited "data_Present_Future_dark_Spot.csv", clear

destring tsa_dhwmax average_bleaching sst_mean  sst_stdev sst_max sst_min ssta_mean ssta_min ssta_max ssta_freqstdev ssta_dhwmean ssta_dhwmax tsa_mean tsa_freqmean tsa_freqstdev climsst tsa_dhwmean turbidity_mean ssta_freqmean sst_mean_rcp85_2050 sst_mean_rcp85_2100 sst_max_rcp85_2050 sst_max_rcp85_2100 mean_tsa_dhw_rcp45_2050 max_tsa_dhw_rcp45_2050 mean_tsa_dhw_rcp45_2100 max_tsa_dhw_rcp45_2100 mean_tsa_dhw_rcp85_2050 max_tsa_dhw_rcp85_2050 mean_tsa_dhw_rcp85_2100 max_tsa_dhw_rcp85_2100 , replace ignore("NA")

drop if missing(average_bleaching) 
drop if missing(tsa_dhwmean) 
drop if missing(sst_mean) 

label var average_bleaching "Average Bleaching"
label var tsa_dhwmean "Thermal Heat Stress in DHW"
label var tsa_dhwmax "Max TSA in DHW"
label var ssta_dhwmean "SSTA in DHW"
label var days_since_19811231 "Number of days since 1981/12/31"
label var latitudedegrees "Latitute degrees"
label var depth "Depth"
label var sst_mean "Mean SST"
label var sst_stdev "SST stdv"
label var sst_max "Max SST"
label var sst_min "Min SST"
label var ssta_mean "SSTA mean"
label var ssta_max "Max SSTA"
label var ssta_min "Min SSTA"
label var ssta_freqmean "Frequency SSTA"
label var ssta_freqstdev "Frequency SSTA stdv "
label var ssta_dhwmax "SSTA maximum"
label var tsa_freqmean "Frequency  TSA mean"
label var tsa_freqstdev "Frequency TSA stdv"
label var climsst "Climatic SST"
label var human_pop "Population"
label var tsa_mean "Thermal heat stress"
label var turbidity_mean "Turbidity mean"

gen bleaching_dummy=(average_bleaching>0)
gen bleaching_dummy_10=(average_bleaching>10)
label var bleaching_dummy "Bleaching > 0%"
label var bleaching_dummy_10 "Bleaching > 10%"

global usedvariables average_bleaching bleaching_dummy bleaching_dummy_10 tsa_dhwmean tsa_dhwmax days_since_19811231 latitudedegrees depth sst_mean sst_stdev sst_max sst_min ssta_min ssta_max ssta_freqstdev ssta_dhwmean ssta_dhwmax  tsa_freqmean tsa_freqstdev climsst human_pop tsa_mean cyclone turbidity_mean ssta_mean ssta_freqmean longitudedegrees

keep $usedvariables reef_id longitudedegrees latitudedegrees ecoregion sst_mean_rcp45_2050 sst_mean_rcp45_2100 sst_max_rcp45_2050 sst_max_rcp45_2100 sst_mean_rcp85_2050 sst_mean_rcp85_2100 sst_max_rcp85_2050 sst_max_rcp85_2100 mean_tsa_dhw_rcp45_2050 max_tsa_dhw_rcp45_2050 mean_tsa_dhw_rcp45_2100 max_tsa_dhw_rcp45_2100 mean_tsa_dhw_rcp85_2050 max_tsa_dhw_rcp85_2050 mean_tsa_dhw_rcp85_2100 max_tsa_dhw_rcp85_2100 human_pop_2050_vals human_pop_2100_vals country_name ocean 

* Summary statistics
est clear
estpost sum $usedvariables
esttab using "SummaryStatistics.rtf", replace ///
cells("mean(fmt(%6.2fc)) sd(fmt(%6.2fc)) min(fmt(%6.0fc)) max(fmt(%15.0fc)) count(fmt(%15.0fc))") nonumber ///
nomtitle nonote noobs label collabels("Mean" "SD" "Min" "Max" "N")

* Preparing data for regessions
encode ecoregion, gen(ERG_encoded)
encode reef_id, gen(Reefid_encoded)


*days_since_19811231

/* We cannot use these variables because they are too much 
correlated with the variables of interest: tsa_dhwmean 
pwcorr tsa_dhwmean sst_mean sst_max tsa_dhwmax sst_min tsa_freqmean ssta_dhwmean sst_stdev sst_max climsst tsa_mean, star(.01)*/


*hist average_bleaching, bin(100) graphregion(color(white)) title("Bleaching level histogram", size(medsmall)) saving("histogram_bleaching_raw_mean_new.gph") 
*graph export "Histogram_plot.png", width(600)

// Step 1: Create a date variable
gen date_1981 = mdy(12, 31, 1981) + days_since_19811231

// Step 2: Convert the date to a Stata date format
format date_1981 %td

// Step 3: Calculate months since January 1981
gen months_since_1981_01 = (year(date_1981) - 1981) * 12 + (month(date_1981) - 1)


global main_vars tsa_dhwmean sst_mean months_since_1981_01
global all_controls latitudedegrees depth human_pop turbidity_mean cyclone

/*
* Regressions
**Poisson Model
poisson average_bleaching $main_vars i.ERG_encoded, vce(cluster reef_id)
outreg2 using "C:\Users\Coulibaly Yerema\OneDrive - Kyushu University\Work\Coral_reefs\Obs_level_Regression.doc", replace ///
label drop(i.ERG_encoded) addstat(Pseudo R-squared, `e(r2_p)')

poisson average_bleaching $main_vars $all_controls i.ERG_encoded, vce(cluster reef_id)
outreg2 using "C:\Users\Coulibaly Yerema\OneDrive - Kyushu University\Work\Coral_reefs\Obs_level_Regression.doc", append ///
label drop(i.ERG_encoded) addstat(Pseudo R-squared, `e(r2_p)')

**Non-negative binomial model
nbreg average_bleaching  $main_vars i.ERG_encoded, vce(cluster reef_id)
outreg2 using "C:\Users\Coulibaly Yerema\OneDrive - Kyushu University\Work\Coral_reefs\Obs_level_Regression.doc", append ///
label drop(i.ERG_encoded) addstat(Pseudo R-squared, `e(r2_p)')
nbreg average_bleaching  $main_vars $all_controls i.ERG_encoded, vce(cluster reef_id)
outreg2 using "C:\Users\Coulibaly Yerema\OneDrive - Kyushu University\Work\Coral_reefs\Obs_level_Regression.doc", append ///
label drop(i.ERG_encoded) addstat(Pseudo R-squared, `e(r2_p)')

** Logit regression
logit bleaching_dummy_10 $main_vars i.ERG_encoded, vce(cluster reef_id)
outreg2 using "C:\Users\Coulibaly Yerema\OneDrive - Kyushu University\Work\Coral_reefs\Obs_level_Regression.doc", append ///
label keep($main_vars $all_controls) addstat(Pseudo R-squared, `e(r2_p)')

logit bleaching_dummy_10 $main_vars $all_controls i.ERG_encoded, vce(cluster reef_id)
outreg2 using "C:\Users\Coulibaly Yerema\OneDrive - Kyushu University\Work\Coral_reefs\Obs_level_Regression.doc", append ///
label keep($main_vars $all_controls) addstat(Pseudo R-squared, `e(r2_p)')
*/

* Collapsing the data at the site level
collapse $usedvariables sst_mean_rcp45_2050 sst_mean_rcp45_2100 sst_max_rcp45_2050 sst_max_rcp45_2100 sst_mean_rcp85_2050 sst_mean_rcp85_2100 sst_max_rcp85_2050 sst_max_rcp85_2100 mean_tsa_dhw_rcp45_2050 max_tsa_dhw_rcp45_2050 mean_tsa_dhw_rcp45_2100 max_tsa_dhw_rcp45_2100 mean_tsa_dhw_rcp85_2050 max_tsa_dhw_rcp85_2050 mean_tsa_dhw_rcp85_2100 max_tsa_dhw_rcp85_2100 human_pop_2050_vals human_pop_2100_vals, by(reef_id ecoregion country_name ocean)
encode ecoregion, gen(ERG_encoded)

label var average_bleaching "Average Bleaching"
label var tsa_dhwmean "Thermal Heat Stress in DHW"
label var tsa_dhwmax "Max TSA in DHW"
label var ssta_dhwmean "SSTA in DHW"
label var days_since_19811231 "Number of days since 1981/12/31"
label var latitudedegrees "Latitute degrees"
label var depth "Depth"
label var sst_mean "Mean SST"
label var sst_stdev "SST stdv"
label var sst_max "Max SST"
label var sst_min "Min SST"
label var ssta_mean "SSTA mean"
label var ssta_max "Max SSTA"
label var ssta_min "Min SSTA"
label var ssta_freqmean "Frequency SSTA"
label var ssta_freqstdev "Frequency SSTA stdv "
label var ssta_dhwmax "SSTA maximum"
label var tsa_freqmean "Frequency  TSA mean"
label var tsa_freqstdev "Frequency TSA stdv"
label var climsst "Climatic SST"
label var human_pop "Population"
label var tsa_mean "Thermal heat stress"
label var turbidity_mean "Turbidity mean"
label var bleaching_dummy "Bleaching > 0%"
label var bleaching_dummy_10 "Bleaching > 10%"

// Step 1: Create a date variable
gen date_1981 = mdy(12, 31, 1981) + days_since_19811231

// Step 2: Convert the date to a Stata date format
format date_1981 %td

// Step 3: Calculate months since January 1981
gen months_since_1981_01 = (year(date_1981) - 1981) * 12 + (month(date_1981) - 1)


* Summary statistics
est clear
estpost sum average_bleaching $main_vars $all_controls
esttab using "SummaryStatistics.rtf", replace ///
cells("mean(fmt(%6.2fc)) sd(fmt(%6.2fc)) min(fmt(%6.0fc)) max(fmt(%15.0fc)) count(fmt(%15.0fc))") nonumber ///
nomtitle nonote noobs label collabels("Mean" "SD" "Min" "Max" "N")

* Regressions
**Poisson Model
poisson average_bleaching $main_vars i.ERG_encoded, vce(cluster reef_id)
outreg2 using "C:\Users\Coulibaly Yerema\OneDrive - Kyushu University\Work\Coral_reefs\Site_level_Regression.doc", replace ///
label drop(i.ERG_encoded) addstat(Pseudo R-squared, `e(r2_p)')

poisson average_bleaching $main_vars $all_controls i.ERG_encoded, vce(cluster reef_id)
outreg2 using "C:\Users\Coulibaly Yerema\OneDrive - Kyushu University\Work\Coral_reefs\Site_level_Regression.doc", append ///
label drop(i.ERG_encoded) addstat(Pseudo R-squared, `e(r2_p)')

**Non-negative binomial model
nbreg average_bleaching  $main_vars i.ERG_encoded, vce(cluster reef_id)
outreg2 using "C:\Users\Coulibaly Yerema\OneDrive - Kyushu University\Work\Coral_reefs\Site_level_Regression.doc", append ///
label drop(i.ERG_encoded) addstat(Pseudo R-squared, `e(r2_p)')
nbreg average_bleaching  $main_vars $all_controls i.ERG_encoded, vce(cluster reef_id)
outreg2 using "C:\Users\Coulibaly Yerema\OneDrive - Kyushu University\Work\Coral_reefs\Site_level_Regression.doc", append ///
label drop(i.ERG_encoded) addstat(Pseudo R-squared, `e(r2_p)')

** Logit regression
logit bleaching_dummy_10 $main_vars i.ERG_encoded, vce(cluster reef_id)
outreg2 using "C:\Users\Coulibaly Yerema\OneDrive - Kyushu University\Work\Coral_reefs\Site_level_Regression.doc", append ///
label keep($main_vars $all_controls) addstat(Pseudo R-squared, `e(r2_p)')

logit bleaching_dummy_10 $main_vars $all_controls i.ERG_encoded, vce(cluster reef_id)
outreg2 using "C:\Users\Coulibaly Yerema\OneDrive - Kyushu University\Work\Coral_reefs\Site_level_Regression.doc", append ///
label keep($main_vars $all_controls) addstat(Pseudo R-squared, `e(r2_p)')

* Export the data arcGIS
label var average_bleaching "Bleaching"
label var depth "Depth"
label var sst_mean "Mean_SST"
label var tsa_dhwmean "MEAN_TSA"
label var days_since_19811231 "Days"
label var longitudedegrees "long"
label var latitudedegrees "lat"
label var sst_mean_rcp45_2050 "SST_205045"
label var sst_mean_rcp45_2100 "SST_210045"
label var sst_mean_rcp85_2050 "SST_205085"
label var sst_mean_rcp85_2100 "SST_210085"
label var mean_tsa_dhw_rcp45_2050 "TSA_205045"
label var mean_tsa_dhw_rcp45_2100 "TSA_210045"
label var mean_tsa_dhw_rcp85_2050 "TSA_205085"
label var mean_tsa_dhw_rcp85_2100 "TSA_210085"
label var human_pop_2050_vals "POP_2050"
label var human_pop_2100_vals "POP_2100"
label var cyclone "CYCLONE"
label var turbidity_mean "TURBID"
label var depth "DEPTH"
label var months_since_1981_01 "MONTHS"

export excel average_bleaching depth sst_mean tsa_dhwmean days_since_19811231 months_since_1981_01 longitudedegrees latitudedegrees sst_mean_rcp45_2050 sst_mean_rcp45_2100 sst_mean_rcp85_2050 sst_mean_rcp85_2100 mean_tsa_dhw_rcp45_2050 mean_tsa_dhw_rcp45_2100 mean_tsa_dhw_rcp45_2100 mean_tsa_dhw_rcp85_2050 mean_tsa_dhw_rcp85_2100 human_pop_2050_vals human_pop_2100_vals cyclone turbidity_mean depth using "../Temporary/Average_at_site_level_1.xlsx", firstrow(varlabels) replace


sort ERG_encoded
tab ecoregion, gen(regions_)
poisson average_bleaching $main_vars regions_*
matrix b = e(b)

gen Constant = b[1,80]
gen TSA_coef = b[1,1]
gen SST_coef = b[1,2]
gen MONTH_coef = b[1,3]
gen regi_coef =. 

forvalues i=1/79 {
    local index = `i' + 3
    replace regi_coef = b[1,`index'] if ERG_encoded == `i'
}

drop regions_*

foreach i in "45" "85" {
    gen RCP`i'2050 = exp(Constant + SST_coef * (sst_mean_rcp`i'_2050 + 273.15) + TSA_coef * mean_tsa_dhw_rcp`i'_2050 + MONTH_coef * 828 + regi_coef)
	gen RCP`i'2100 = exp(Constant + SST_coef * (sst_mean_rcp`i'_2100 + 273.15) + TSA_coef * mean_tsa_dhw_rcp`i'_2100 + MONTH_coef * 957 + regi_coef)
}

rename average_bleaching Bleaching
rename depth Depth
rename sst_mean Mean_SST
rename tsa_dhwmean MEAN_TSA
rename days_since_19811231 Days
rename longitudedegrees Long
rename latitudedegrees lat
rename sst_mean_rcp45_2050 SST_205045
rename sst_mean_rcp45_2100 SST_210045
rename sst_mean_rcp85_2050 SST_205085
rename sst_mean_rcp85_2100 SST_210085
rename mean_tsa_dhw_rcp45_2050 TSA_205045
rename mean_tsa_dhw_rcp45_2100 TSA_210045
rename mean_tsa_dhw_rcp85_2050 TSA_205085
rename mean_tsa_dhw_rcp85_2100 TSA_210085
rename human_pop_2050_vals POP_2050
rename human_pop_2100_vals POP_2100
rename cyclone CYCLONE
rename turbidity_mean TURBID
rename months_since_1981_01 MONTHS


export dbase Long lat Bleaching Depth Mean_SST MEAN_TSA Long lat RCP452050 RCP452100 RCP852050 RCP852100 using  "..\Temporary\Stationnary_prediction_RCPs.dbf", replace
