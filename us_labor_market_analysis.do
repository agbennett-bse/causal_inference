* ===================================================================
*              ML AND CAUSAL INFERENCE - DSDM 2023-24
*                          Homework 2
* -------------------------------------------------------------------
* Andrew Bennett
* ===================================================================

* Clear workspace
clear all
set more off

* Load dataset
use "/Users/andrewbennett/Documents/bse/term2/ml_causal_inf/hw4/PS2.dta", clear

describe

* Filter outliers in earn_h
summarize earn_h, detail

* Calculate quartiles
local Q1 = r(p25)
local Q3 = r(p75)

* Calculate interquartile range (IQR)
local IQR = `Q3' - `Q1'

* Define lower and upper bounds for outliers
local lower_bound = `Q1' - 1.5 * `IQR'
local upper_bound = `Q3' + 1.5 * `IQR'

* Filter out outliers
drop if earn_h < `lower_bound' | earn_h > `upper_bound'

summ earn_h, d

gen y = earn_h 

gen lny = ln(y)

qui summ lny

 
*PART 1. Estimate Model (1) implementing the Heckman correction method (two-stage ap- proach) on the selection process described in Equation (4). Include, in the vector zi, education of the spouse and all regressors included in xi. As your out-of-work income variable bi you can use either benefits or its interaction with marital status.

heckman lny age education  married, select(lfp = benefits age education  married education_spouse ) twostep

*PART 2.Re-estimate the Heckman two-stage model using Matlab/Python without any pre- programmed routines.
* Estimating probit model for selection equation
probit lfp benefits age education married education_spouse
predict prob_lfp, xb

* Calculate mills ratio
gen imr = normalden(prob_lfp)/normprob(prob_lfp)

* OLS regression with mills ratio
regress lny age education married  imr


*PART 3.Estimate now the selection model by Maximum Likelihood using Matlab/Python without any pre-programmed routines (except Newton-Rhapson).

* Create y without missing values (for ln(0))
gen y_1 = earn_h + 1 

gen lny_1 = ln(y_1)

summ lny_1, d

capture program drop lf_sel
program lf_sel
  version 15
  args lnf xb zg lnsigma rho
  local d "$ML_y1"
  local w "$ML_y2"
  quietly replace `lnf' = ln(normal(`zg'+(`rho'/exp(`lnsigma'))*(`w'-`xb')/sqrt(1-`rho'^2)))+ln(normalden(`w', `xb',exp(`lnsigma'))) if `d'==1
  quietly replace `lnf' = ln(normal(-`zg')) if `d'==0
end

ml model lf lf_sel (lfp = benefits age education  married education_spouse)  (lny_1 = age education  married) (lnsigma:) (rho:) //had to cut out married, education
ml search rho: -1 1 //have to bound rho to be between -1 and 1 because it is a correlation coefficient
ml maximize


*PART 4. Estimate Model (1) by OLS on the sample of individuals that work. 

* OLS for those who work
regress lny age education married if lfp == 1 

* Save the predicted values
predict predicted_values, xb

* Summarize the predicted values by year
bysort year: egen mean_predicted_values = mean(predicted_values)

* Run the Heckman model
heckman lny age education married, select(lfp = benefits age education married education_spouse) twostep

* Save the predicted values from the Heckman model for each observation
predict predicted_values_heckman, xb

* Summarize the predicted values from the Heckman model by year
bysort year: egen mean_predicted_values_heckman = mean(predicted_values_heckman)

* Plot the means of the predicted values from both models by year on a timeline
twoway (line mean_predicted_values year, lcolor(blue) lpattern(solid) legend(label(1 "OLS Regression"))) ///
       (line mean_predicted_values_heckman year, lcolor(red) lpattern(solid) legend(label(2 "Heckman"))), ///
       xlabel(2001(2)2015) ylabel(2.6(0.1)3.2) ///
       title("Comparison of Predicted Hourly Earnings by Year") ///
       xtitle("Year") ytitle("log(Hourly Earnings)")
