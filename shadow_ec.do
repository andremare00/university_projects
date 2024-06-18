/* The aim of the study is to investigate
the relationship between cash usage and shadow economy.
In order to do that, the Act No. 394/2012 introduced in Slovakia,
entered into force on 1 January 2013, it is exploited.
The methodology used is the Synthetic Control Method. */


clear all
set more off
log using LogFile.log, replace

************************************************************
**# DATA IMPORT AND MANIPULATION ***************************
************************************************************
import excel "C:\Users\Andrea\Desktop\Portfolio\The impact of cash usage on shadow economy - a case-control study for Slovakia\Dataset.xlsx", sheet("Sheet1") firstrow

* Data manipulation
egen N = group(country) // country's progressive number
gen post = (year>=2013) // dummy variable for the introduction of the policy
gen treated = (country == "Slovakia") // dummy variable for Slovakia

gen ln_gdppc = ln(gdp_percapita)
gen ln_labprod = ln(productivity)

bysort year treated: egen mean_shadecon = mean(shad_econ) // annual mean of the target variable for treated and control group

sort country year // order the dataset
xtset N year, yearly // set the panel format 


************************************************************
**# EMPIRICAL ANALYSIS - SYNTHETIC CONTROL METHOD **********
************************************************************
* Generate a global for predictor variables
global predictors business_freedom labour_freedom ///
agr_va taxburden trade_open ln_labprod educ ln_gdppc

* Perform Synthetic Control Method
synth shad_econ $predictors, ///
trunit(10) trperiod(2013) unitnames(country) ///
nested allopt figure keep(synth, replace)

* Generate the plot for the comparison (NB: run from preserve to restore)
preserve
use synth.dta, clear
twoway line _Y_treated _time, sort || ///
	   line _Y_synthetic _time, sort ///
	   lpattern(dash) legend(label(1 "Slovakia") label(2 "synthetic Slovakia")) ///
	   xline(2013) xlabel(2005 2007 2009 2011 2013 2015 2017 2019) ///
	   ytitle("shadow economy (% GDP)") ///
	   text(12.5 2009.7 "Entrance of Act No. 394/2012 =>", width(msize(.1cm))) 
graph export comparison.jpg, replace // export plot
restore

* Store results
mat define synth_Slovakia = e(Y_synthetic) // save the synthetic control's shad_econ
mat define mspe_Slovakia = (e(RMSPE)*e(RMSPE)', ., ., ., ., ., ., ., ., ., ., ., ., ., .)' // save the Mean Square Prediction Error
mat define balance = e(X_balance) // save predictors balance
mat define weights = e(W_weights) // save countries's weights

* Generate tables from matrices
esttab matrix(balance) using balanceTable.tex, booktabs replace // table for predictors balance
esttab matrix(weights) using weightsTable.tex, booktabs replace // table for countries' weights


************************************************************
**# EMPIRICAL ANALYSIS - PLACEBO STUDY *********************
************************************************************
/* synth is performed for all countries except Slovakia, to check robustness.
Note: since Luxembourg presents outliers,
the computation of its synthetic control was not feasible.
For this reason, the placebo study exclude Luxembourg */

* Generate a global for countries' ID
global country_numbers 1 2 3 4 5 6 8 9 11

* Perform Synthetic Control Method
foreach n of global country_numbers {
	synth shad_econ $predictors, ///
	trunit(`n') trperiod(2013) unitnames(country) ///
	nested allopt
	
	* Store results
	mat define synth_`n' = e(Y_synthetic)
	mat define mspe_`n' = (e(RMSPE)*e(RMSPE)', ., ., ., ., ., ., ., ., ., ., ., ., ., .)' 

}

*** CREATE A VARIABLE WITH THE SYNTHETIC CONTROL'S SHAD_ECON AND RMSPE FOR EACH COUNTRY
* Add Slovakia and Luxembourg to the global countries
global countries Austria Cyprus Estonia Finland ///
Germany Ireland Luxembourg Malta Netherlands Slovakia Sweden

* Define a matrix with the synthetic control values of all countries
mat define null = (., ., ., ., ., ., ., ., ., ., ., ., ., ., .)' // null vector for Luxembourg
mat define synth = (synth_1, synth_2, ///
synth_3, synth_4, synth_5, synth_6, ///
null, synth_8, synth_9, synth_Slovakia, synth_11)
mat colnames synth = $countries
mat define synth = vec(synth)
mat list synth
svmat synth, names(synth_value) // add to dataset

* Define a matrix for MSPE
mat define null = (0, ., ., ., ., ., ., ., ., ., ., ., ., ., .)' /* null vector for Luxembourg
(the 0 is because of the option "nomissing" that will be used afterwards with mkmat in the creation of the table)*/
mat define MSPE = (mspe_1, mspe_2, mspe_3, ///
mspe_4, mspe_5, mspe_6, null, mspe_8, mspe_9, mspe_Slovakia, mspe_11)
mat colnames MSPE = $countries
mat define MSPE = vec(MSPE)
mat list MSPE
svmat MSPE, names(mspe) // add to dataset

* Export the table of MSPEs
mkmat mspe, matrix(mspe) nomissing rownames(country)
esttab matrix(mspe) using mspeTable.tex, booktabs replace // export the table

/* Generate the difference of country's shad_econ 
and the value of shad_econ for its synthetic control group*/
gen diff = shad_econ-synth_value

* Generate the plot for the placebo test
twoway line diff year if country == "Slovakia", sort lcolor(blue)  || ///
	   line diff year if country == "Austria", sort lpattern(solid) lcolor(gs13) || ///
	   line diff year if country == "Cyprus", sort lpattern(solid) lcolor(gs13) || ///
	   line diff year if country == "Estonia", sort lpattern(solid) lcolor(gs13) || ///
	   line diff year if country == "Finland", sort lpattern(solid) lcolor(gs13) || ///
	   line diff year if country == "Germany", sort lpattern(solid) lcolor(gs13) || ///
	   line diff year if country == "Ireland", sort lpattern(solid) lcolor(gs13) || ///
	   line diff year if country == "Malta", sort lpattern(solid) lcolor(gs13) || ///
	   line diff year if country == "Netherlands", sort lpattern(solid) lcolor(gs13) || ///
	   line diff year if country == "Sweden", sort lpattern(solid) lcolor(gs13) legend(order(1 "Slovakia" 2 "control states")) ///
	   xline(2013) yline(0, lpattern(dash)) yscale(range(-3 3)) ylabel(-3 -2 -1 0 1 2 3) ///
	   xlabel(2005(2)2019) ///
	   ytitle("gap in shadow economy (% GDP)") ///
	   text(-2.5 2009.7 "Entrance of Act No. 394/2012 =>", width(msize(.1cm))) 
graph export placebo.jpg, replace // export plot

* Generate the plot for the placebo test with best pre-intervention MSPE
* (defined as MSPE less than 2.5 times Slovakia's MSPE)
gen ratio = mspe/.03765623 // on the denominator, Slovakia's mspe
	   
list country if ratio<2.5
twoway line diff year if country == "Slovakia", sort lcolor(blue)  || ///
	   line diff year if country == "Finland", sort lpattern(solid) lcolor(gs13) || ///
	   line diff year if country == "Ireland", sort lpattern(solid) lcolor(gs13) || ///
	   line diff year if country == "Sweden", sort lpattern(solid) lcolor(gs13) ///
	   xline(2013) yline(0, lpattern(dash)) legend(order(1 "Slovakia" 2 "control states")) ///
	   xline(2013) yline(0, lpattern(dash)) yscale(range(-3 3)) ylabel(-3 -2 -1 0 1 2 3) ///
	   xlabel(2005(2)2019) ///
	   ytitle("gap in shadow economy (% GDP)") ///
	   text(-2.5 2009.7 "Entrance of Act No. 394/2012 =>", width(msize(.1cm)))
graph export placebo_best.jpg, replace // export plot

***************
log close






