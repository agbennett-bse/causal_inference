

// Exercise 2

// 2.1 Consider only the three largest parties: CDU/CSU, SPD and Die Grüne. (You can drop individuals who have not voted or voted for another party.) Using Model (2) with number of categories m= 3 and multinomial logit, estimate the impact of the same variables as in the previous exercise on the probability of choosing particular party. Interpret the estimates.

use "/Users/andrewbennett/Documents/bse/term2/ml_causal_inf/hw3/ALLBUS18.dta", clear

* Check the voting intention and remove those, we don't need for the exercise
tab pv01, m
tab pv01, m nolab
drop if !inlist(pv01, 1, 2, 4)
drop if pv01<0

* logit

mlogit pv01 i.sex age i.educ i.work, nolog

// 2.2 Compute marginal effects of the estimates for female for the median voter.

quietly mlogit pv01 i.sex age i.educ i.work, rrr nolog

margins, dydx(sex) atmeans

// 2.3 For which party would you expect the largest gain in voting share as the population becomes older on average?

margins, dydx(age) atmeans

// 2.4 Re-estimate the previous model in Julia/Matlab/Python/R without pre-programmed functions or in Stata using the ml command.

qui tab pv01, gen(pr)
global y1 "pr1"
global y2 "pr2"
global y3 "pr3"

capture program drop lfmlogit
program define lfmlogit
  version 17
  args lnf xb1 xb2
  tempvar p1 p2 p3
  quietly {
     gen double `p1' = 1/(1+exp(`xb1')+exp(`xb2'))
     gen double `p2' = exp(`xb1')/(1+exp(`xb1')+exp(`xb2'))
     gen double `p3' = exp(`xb2')/(1+exp(`xb1')+exp(`xb2'))
     replace `lnf' = $y1*ln(`p1') + $y2*ln(`p2') + $y3*ln(`p3')
   }
end

ml model lf lfmlogit (SPD: pv01 = sex age educ work)(Grüne: pv01 = sex age educ work)
ml maximize

// 2.5 Include alternative varying variables into Model (2). cu_assess, spd_assess and gre_assess measure how closely the party's position on the left-to-right spectrum aligns with the respondents own position. They are calculated as |partyposition − ownposition|, where both range from 1 to 10.

keep respid pv01 sex age educ work cdu_asses spd_assess gre_assess

* Generate variables for options
gen y_mnl=1 if pv01==1
replace y_mnl=2 if pv01==2
replace y_mnl=3 if pv01==4
			
* Generate choice variables
foreach k of numlist 1/3 {
	gen y`k' = 0
	replace y`k' = 1 if y_mnl == `k'
}

* reshape wide to long
reshape long y, i(respid) j(parties)
order parties, b(y_mnl)
drop y_mnl

* define the names of the parties			
lab def partynames 1 "CDU/CSU" 2 "SPD" 3 "GREENS"
lab val parties partynames
			
			
* generate distance measure
gen party_alignment = .
bysort respid: replace party_alignment = cdu_asses if _n == 1
bysort respid: replace party_alignment = spd_assess if _n == 2
bysort respid: replace party_alignment = gre_assess if _n == 3

			
* conditional logit
asclogit y party_alignment, case(respid) alternatives(parties) casevars(i.sex age i.educ i.work)

// For the remaining exercises, change the number of categories to include the six largest parties.

use "/Users/andrewbennett/Documents/bse/term2/ml_causal_inf/hw3/ALLBUS18.dta", clear

* Check the voting intention and remove those, we don't need for the exercise
tab pv01, m
tab pv01, m nolab
drop if !inlist(pv01, 1, 2, 3, 4, 6, 42)

// 2.6 Estimate a Multinomial Logit model. Include the regressors pa01 (respondent's own position on the left-right spectrum), sex, age, educ and work. Briefly comment on the coefficients (4-5 sentences).

* logit
mlogit pv01 pa01 i.sex age i.educ i.work, nolog
estimates store mlogit_results

estout mlogit_results, style(tex) 


// 2.7 Estimate a Nested Logit, with voters choosing between left and right in the first level, and then choosing between parties in each branch. 

* load and arrange data again
use "/Users/andrewbennett/Documents/bse/term2/ml_causal_inf/hw3/ALLBUS18.dta", clear
drop if !inlist(pv01, 1, 2, 3, 4, 6, 42)		
keep respid pv01 pa01 sex age educ work
order pv01, b(pa01)

* Generate variables for options
gen y_mnl=1 if pv01==1
replace y_mnl=2 if pv01==2
replace y_mnl=3 if pv01==3
replace y_mnl=4 if pv01==4
replace y_mnl=5 if pv01==6
replace y_mnl=6 if pv01==42

* Generate choice variables
gen y1=0
replace y1=1 if y_mnl==1
gen y2=0
replace y2=1 if y_mnl==2
gen y3=0
replace y3=1 if y_mnl==3
gen y4=0
replace y4=1 if y_mnl==4
gen y5=0
replace y5=1 if y_mnl==5
gen y6=0
replace y6=1 if y_mnl==6
			
			
* Set to missing given type (easier delete later)
nlogitgen type = pv01(left: 2|4|6, right: 1|3|42)
replace y1 = . if type == 1 
replace y3 = . if type == 1 
replace y6 = . if type == 1 		
replace y2 = . if type == 2
replace y4 = . if type == 2 
replace y5 = . if type == 2 
					
* reshape wide to long
reshape long y, i(respid) j(parties)
order parties, b(y_mnl)
drop y_mnl
			
* Drop if y == . such that options are exclusive for 'left' and 'right'
drop type
drop if y == .
			
* Set tree structure
lab drop lb_type
nlogitgen type = pv01(left: 2|4|6, right: 1|3|42)

* Put labels on values
lab def party_lab 1 "CDU/CSU" 2 "SPD" 3 "FDP" 4 "GREENS" 5 "LEFT" 6 "AFD"
lab val parties party_lab

* Visualize tree structure
nlogittree pv01 type

* nested logit
nlogit y || type: pa01 || parties: i.sex age i.educ i.work, case(respid) iterate(50)


predict plevel1 plevel2, pr
tab parties, summarize(plevel2)

*  ⁠Marginal effects of age on gre - have to be calculated manually
preserve 
	quietly summarize age
	gen delta=r(sd)/1000
	quietly replace age = age + delta 
		
	* Predict at the new values
	predict pnew1 pnew2, pr
		
	* Subtract the two predictions and divide by the amount of the change
	gen dagedgreen = (pnew2-plevel2)/delta
		
	* The AME is the average of the previous quantity
	tab parties, summarize(dagedgreen)
	
	*⁠The AME is the average of the previous quantity
	tab parties, summarize(dagedgreen)
	
restore


quietly summarize age
gen delta=r(sd)/1000
quietly replace age = age + delta 
	
* Predict at the new values
predict pnew1 pnew2, pr
	
* Subtract the two predictions and divide by the amount of the change
gen dagedgreen = (pnew2-plevel2)/delta
	
* The AME is the average of the previous quantity
tab parties, summarize(dagedgreen)

*⁠The AME is the average of the previous quantity
tab parties, summarize(dagedgreen)

// 2.8 Estimate a few different versions of Multinomial and Nested Logits (around 5-6 versions). 

* load data again
use "/Users/andrewbennett/Documents/bse/term2/ml_causal_inf/hw3/ALLBUS18.dta", clear
drop if !inlist(pv01, 1, 2, 3, 4, 6, 42)

* keep only variables of interest

* semi-racist
* px07 - G Foreigners should always marry people from their own ethnic group.
* px08 - H The Jews still have too much influence.

* conservative non conservative
* pr04 - A Reunification has brought more advantages than disadvantages for the people in the OLD FEDERAL STATES
* pr05 - B Reunification has brought more advantages than disadvantages for the

* german topical knowledge
* pk10 - What is the name of the current President of the European Commission?
* pk11 - Who is the Chancellor of the Federal Republic of Germany elected by?

* world topical knowledge
* pk12 - What does "secret ballot" mean?
* pk15 - Which country does not have a permanent seat in the UN Security Council?
* pk17 - The solidarity surcharge is a surcharge on income and corporation tax. What is its purpose?

keep respid pv01 px07 px08 pr04 pr05 pk10 pk11 pk12 pk15 pk17 sex age educ work


mlogit pv01 px07 px08 i.sex age i.educ i.work
estimates store model_1
esttab model_1 using "table1.tex", stats(r2_p) se varwidth(30) modelwidth(10) label unstack eform replace


mlogit pv01 pr04 pr05 i.sex age i.educ i.work
estimates store model_2
esttab model_2 using "table2.tex", stats(r2_p) se varwidth(30) modelwidth(10) label unstack eform replace


mlogit pv01 pk12 pk15 pk17 i.sex age i.educ i.work
estimates store model_3
esttab model_3 using "table3.tex", stats(r2_p) se varwidth(30) modelwidth(10) label unstack eform replace


mlogit pv01 pk10 pk11 i.sex age i.educ i.work
estimates store model_3
esttab model_3 using "table4.tex", stats(r2_p) se varwidth(30) modelwidth(10) label unstack eform replace


mlogit pv01 pk15 pk17 i.sex age i.educ i.work
estimates store model_4
esttab model_4 using "table5.tex", stats(r2_p) se varwidth(30) modelwidth(10) label unstack eform replace


mlogit pv01 px07 px08 pr04 pr05 pk10 pk11 pk12 pk15 pk17 sex age educ work
estimates store model_4
esttab model_4 using "table6.tex", stats(r2_p) se varwidth(30) modelwidth(10) label unstack eform replace

predict phat*, pr

gen maxprob = phat1
gen pred_category = 1

forval i = 2/6 { 
    replace maxprob = phat`i' if phat`i' > maxprob
    replace pred_category = `i' if phat`i' == maxprob
}
gen correct_prediction = (pred_category == pv01)
sum correct_prediction


gen predicted_category = .
local counter = 0 
quietly foreach i of numlist 1 2 3 4 6 42 {
	local counter = `counter' + 1

    replace predicted_category = `counter' if phat`counter' == max(phat1, phat2, phat3, phat4, phat5, phat6)
}

local counter = 0 
foreach i of numlist 1 2 3 4 6 42  {
	local counter = `counter' + 1
    egen correct_`counter' = total(pv01 == `i' & predicted_category == `counter')
    egen total_`counter' = total(pv01 == `i')
    scalar accuracy_`counter' = correct_`counter'/total_`counter'
    display "Accuracy for category `i': " accuracy_`counter'
}
display pv01















