*****************************************************************
* CODING SAMPLE (FROM A DATA TASK)

*******************************************************************
******* settig up the data****************************************
clear all
set maxvar 10000
log close 
********************************************************************

//generating log file
log using Patient, replace

//setting up directory and global path
global DataDir "C:\Users\sagra\OneDrive\Desktop\Test"
cd "$DataDir"
	
//installing coefplot package 
ssc instal coefplot
*************************************************************
 *	Q0
 
 import delimited "test_data.csv", clear //importing the data
 
 describe //all the time and date variables need to be converted into required STATA format
 

 
******************************Data Cleaning****************************
/* changing "noon" to "12 p.m." */
replace shift_start = "12 p.m." if shift_start == "noon"
replace shift_end = "12 p.m." if shift_end == "noon"

// transform shift_end and shift_start into stata date format
foreach v in "start" "end" {
	gen shift_`v'_fmt = shift_date + " " + shift_`v'
	gen shift_`v'_n = clock(shift_`v'_fmt, "DMYh")
	format shift_`v'_n %tc
	
}

drop shift_start_fmt shift_end_fmt

//transform shift_date to STATA date format
gen shift_date_fmt = clock(shift_date, "DMY")
format shift_date_fmt %tc

//transform arrive and leave variables into STATA format

foreach v in "arrive" "leave" {
	gen double `v'_fmt = clock(`v', "DMYhms")
	format `v'_fmt %tc
}

drop arrive leave 
rename arrive_fmt arrive
rename leave_fmt leave

drop shift_date shift_start shift_end
rename shift_date_fmt shift_date
rename shift_start_n shift_start
rename shift_end_n shift_end

//label variables

label var visit_num "Row identifier for patient visit"
label var phys_id "Physician identifier"
label var pred_lnlos "expected predicted length of stay/patient severity"
label var shift_start "Date and time at which physician's shift starts"
label var shift_end "Date and time at which physician's shift ends"
label var shift_date "Date on which physician's shift started"
label var arrive "Date and time at which patient arrived"
label var leave "Date and time at which patient left"
****************************************summarize dataset*********************

sum pred_lnlos, d //patient severity ranges from -0.27 to 2.12(in log terms)

sort shift_date

*first observation starts from 15 May, 1982

sort shift_start
*first shift starts on 15May, 1982 at 7 pm 
sort shift_end
*first shift ends on 15May, 1982 at 4 am

sort arrive
*first patient arrived at around 8 pm on 15May, 1982

sort leave
*first patient left at around 10 pm on 15May, 1982

tab visit_num, missing
 tab phys_id, missing
 tab shift_date, missing
 tab shift_end, missing // 10 am and 4 pm have very few observations 
 tab shift_start, missing // 10 am, 4 am, 6 am and noon have relatively less observations
 tab arrive, missing
 tab leave, missing
 tab pred_lnlos, missing 
 //there are no missing values 
 
 /* Looking at patient severity */
 hist pred_lnlos, percent ///
 ytitle("Percent (%)") xtitle("Patient severity") ///
		title("Patient severity", margin(medium) size(large)) ///
		lcolor(midblue) color(ltblue)
/* Patient visiting the emergency deprtament have very high severity/ they stay around 1.5 days in the ED */

graph export "$DataDir\Patient_severity.png", replace

/* Looking at the frequency of visits of each physician */

hist phys_id, percent ///
 ytitle("Percent (%)") xtitle("Physician") ///
		title("Frequency of Physician Visits", margin(medium) size(large)) ///
lcolor(midblue) color(ltblue) //All the physician have atleast one patient

graph export "$DataDir/Physician.png", replace
***************Data errors***********************************
//generating dummy variable indicating mistake in recording physician shifts
gen shift_mistake = shift_start> shift_end
tab shift_mistake , missing // 1067 physicians have shift start after shift end. This is a mistake 

//generating dummy variable indicating mistake in recording patient visits
gen arrive_mistake = (arrive > shift_end)
tab arrive_mistake, missing //1067 patient visited after the shift end and yet recorded as visited

tab shift_mistake arrive_mistake 
/* 1067 observations in both the variable implies this is a data entry mistake  */

/* removing data entry errors */

drop if shift_mistake == 1
drop shift_mistake arrive_mistake

sort phys_id visit_num

save "$DataDir/test_data_clean.dta", replace 

******************************************************************
* 	Q1
******************************************************************

// generating dummy variable reflecting patient wait and physicians working long hours.
gen patient_wait = (arrive< shift_start)
gen phy_wait = (leave > shift_end)
gen total_visit = _N

// calculating the percentage of total visits for patient waiting
egen total_patient_wait = sum(patient_wait)
gen percent_wait = total_patient_wait/total_visit 
/* 7.56% patients have to wait as they arrive before physician's shift starts */

// calculating the percentage of total visits where physicians work long hours
egen total_phy_wait = sum(phy_wait)
gen percent_phy_wait = total_phy_wait/total_visit

/*17.78% physicians have to work past their shift ends */


*********************************************************************
*	Q2
*********************************************************************

// generating hour from arrive_fmt variable

gen arrive_hr = hh(arrive)
tab arrive_hr, missing  // most of the patient arrive between 6 am to 7 pm

//generating average severity of the patients per hour 
bysort arrive_hr: egen avg_pred_lnlos = mean(pred_lnlos)
label var avg_pred_lnlos "average predicted length of stay/patient severity"

//plot relationship between average severity and arrival hour using linear regression line 
graph twoway (lfitci avg_pred_lnlos arrive_hr) (scatter avg_pred_lnlos arrive_hr), xtitle("Arrival hour", size(small)) ytitle("Average predicted length of stay/patient severity", size(small))  title("Patient arrival time and expected length of stay", margin(medium) size(large))
/* Average patient severity falls with arrival hour, though the best fit line is somewhat flat */

graph export $DataDir/predicted_Patient_Stay.png, replace 

//formally testing the relationship between the two variables 

regress avg_pred_lnlos arrive_hr, r 

/* If arrival hour increases by 1 hour, average patient severity falls by 0.001 units on average. The t value is -6.39 which is statitsically significant at 0.05% significance level */

*********************************************************************
*	Q3
*********************************************************************
******Census data******************************************

// Generating shift duration varaibles and patient arrival variables 
gen shift_start_hh = hh(shift_start)
gen shift_end_hh = hh(shift_end)
gen shift_length = shift_end_hh - shift_start_hh
gen shift_4 = shift_length +  4
gen stay_length = (leave - arrive)/3600000				// exact amount
gen leave_hr = arrive_hr + stay_length
gen leave_hh = hh(leave)
	
gen exact_time_to_be_seen = (arrive - shift_start)/3600000		// exact amount
gen lower_bound_to_be_seen = ((arrive_hr+1) - shift_start_hh)	// lower bound
gen upper_bound_to_be_seen = ((arrive_hr) - shift_start_hh)	    // upper bound 
 
save "$DataDir/clean_data.dta", replace


***********************************Creating exact census****************************************
// creating hour* variables that are dummy variables indicating whether patient were seen in each hour 
foreach n of numlist 1/12 {
		local i = 1 + `n'
		gen hour`n'= 0
		replace hour`n'= 1 if (exact_time_to_be_seen <`n') & (shift_4>`i') & (stay_length>`i')
	}

// reshape wide to long dataset, generating a count variable
	reshape long hour, i(visit_num) j(shift_hour)
	rename hour patient_seen_exact

// generate total patients under a physician's care variable
	bysort phys_id shift_date shift_hour: egen census_exact = total(patient_seen_exact)
		
save "$DataDir/census_exact.dta", replace


***************************************Creating upper bound census************************************
use $DataDir/clean_data.dta, clear

// creating hour* variables that are dummy variables indicating whether patient were seen in each hour 
foreach n of numlist 1/12 {
		local i = 1 + `n'
		gen hour`n'= 0
		replace hour`n'= 1 if (upper_bound_to_be_seen <`n') & (shift_4>`i') & (stay_length>`i')
	}

// reshape wide to long dataset, generating a count variable
	reshape long hour, i(visit_num) j(shift_hour)
	rename hour patient_seen_upper_bound

// generate total patients under a physician's care variable
	bysort phys_id shift_date shift_hour: egen census_exact = total(patient_seen_upper_bound)
	
save $DataDir/census_upper.dta, replace 

***************************************Creating lower bound census**************************************

use $DataDir/clean_data, clear

// creating hour* variables that are dummy variables indicating whether patient were seen in each hour 
foreach n of numlist 1/12 {
		local i = 1 + `n'
		gen hour`n'= 0
		replace hour`n'= 1 if (lower_bound_to_be_seen <`n') & (shift_4>`i') & (stay_length>`i')
	}

// reshape wide to long dataset, generating a count variable
	reshape long hour, i(visit_num) j(shift_hour)
	rename hour patient_seen_lower_bound

// generate total patients under a physician's care variable
	bysort phys_id shift_date shift_hour: egen census_exact = total(patient_seen_lower_bound)
	
save $DataDir/census_lower.dta, replace 

******************************************Merging the 3 datasets to create final census**************************
merge m:m shift_hour phys_id using $DataDir/census_exact
drop _merge
merge m:m shift_hour phys_id using $DataDir/census_upper
drop _merge

//converting time variables into string variable
foreach v in "exact_time_to_be_seen" "lower_bound_to_be_seen" "upper_bound_to_be_seen" {
	gen string_`v' = string(`v')
}

tab shift_hour census_exact, missing
/* census falls with increase in end of shift implying less patients at shift end hours. */

save $DataDir/Final_census.dta, replace 

**************************************************************************************************************
*	Q4
**************************************************************************************************************

use $DataDir/test_data_clean, clear
//generating log of lengty of stay variable
gen stay_length = (leave-arrive)/3600000
gen ln_stay_length = ln(stay_length)

reg ln_stay_length i.phys_id, r
coefplot, xline(0) ///
title("Physician ID and Log Length of Stay", ///
			margin(medium) size(large)) ///
		xtitle("Regression Coefficient") ///
		ytitle("Physician ID") ///
		note("Note: Robust standard errors are used.") ///
		ylabel(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" ///
			11 "11" 12 "12" 13 "13" 14 "14" 15 "15" 16 "16" 17 "17" 18 "18" ///
			19 "19" 20 "20" 21 "21" 22 "22" 23 "23" 24 "24" 25 "25" 26 "26" ///
			27 "27" 28 "28" 29 "29" 30 "30" 31 "31" 32 "32" 33 "33" 34 "34" ///
			35 "35" 36 "36" 37 "37" 38 "38" 39 "39" 40 "40" 41 "41" 42 "42" ///
			43 "43", labsize(vsmall))
			
/* physician 10 seems to be discharging the patient fastest as the coefficeint is -0.3124. That means with physician 10, log length of stay reduces by 0.3124 units. */
/* Potential threats can be omitted variables and step-wise regression can help identifying those missing varibales and seeing the robustness of physician effect on log length of stay. */
graph export $DataDir/Regression1.png, replace 

//adding expected predicted length of stay as additional control and checking the robustness 
regress ln_stay_length pred_lnlos i.phys_id, r
coefplot, drop(pred_lnlos) xline(0) ///
title("Physician ID and Log Length of Stay", ///
			margin(medium) size(large)) ///
		xtitle("Regression Coefficient") ///
		ytitle("Physician ID") ///
		note("Note: Robust standard errors are used.") ///
		ylabel(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" ///
			11 "11" 12 "12" 13 "13" 14 "14" 15 "15" 16 "16" 17 "17" 18 "18" ///
			19 "19" 20 "20" 21 "21" 22 "22" 23 "23" 24 "24" 25 "25" 26 "26" ///
			27 "27" 28 "28" 29 "29" 30 "30" 31 "31" 32 "32" 33 "33" 34 "34" ///
			35 "35" 36 "36" 37 "37" 38 "38" 39 "39" 40 "40" 41 "41" 42 "42" ///
			43 "43", labsize(vsmall))
			
/* physician 10 seems again to be discharging the patient fastest as the coefficeint is -0.3352 even after controlling for expected predicted length of stay. That means with physician 10, log length of stay reduces by 0.3352 units. */

graph export $DataDir/Regression2.png, replace 

//alternative specification with stay_length and not its log 
regress stay_length pred_lnlos i.phys_id, r
coefplot, drop(pred_lnlos) xline(0) ///
title("Physician ID and Length of Stay", ///
			margin(medium) size(large)) ///
		xtitle("Regression Coefficient") ///
		ytitle("Physician ID") ///
		note("Note: Robust standard errors are used.") ///
		ylabel(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" ///
			11 "11" 12 "12" 13 "13" 14 "14" 15 "15" 16 "16" 17 "17" 18 "18" ///
			19 "19" 20 "20" 21 "21" 22 "22" 23 "23" 24 "24" 25 "25" 26 "26" ///
			27 "27" 28 "28" 29 "29" 30 "30" 31 "31" 32 "32" 33 "33" 34 "34" ///
			35 "35" 36 "36" 37 "37" 38 "38" 39 "39" 40 "40" 41 "41" 42 "42" ///
			43 "43", labsize(vsmall))
			
/* physician 10 seems again to be discharging the patient fastest as the coefficeint is -0.3352 even after controlling for expected predicted length of stay. That means with physician 10, length of stay reduces by 0.3352 units. */

graph export $DataDir/Regression3.png, replace 

//generating dummy for patients who arrive early and physicians who work longer hours 
gen patient_wait = (arrive< shift_start)
gen phy_wait = (leave > shift_end)

//alternative specification with patient and physician wait time.
regress ln_stay_length patient_wait phy_wait pred_lnlos i.phys_id, r
coefplot, drop(pred_lnlos patient_wait phy_wait) xline(0) ///
title("Physician ID and Length of Stay", ///
			margin(medium) size(large)) ///
		xtitle("Regression Coefficient") ///
		ytitle("Physician ID") ///
		note("Note: Robust standard errors are used.") ///
		ylabel(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" ///
			11 "11" 12 "12" 13 "13" 14 "14" 15 "15" 16 "16" 17 "17" 18 "18" ///
			19 "19" 20 "20" 21 "21" 22 "22" 23 "23" 24 "24" 25 "25" 26 "26" ///
			27 "27" 28 "28" 29 "29" 30 "30" 31 "31" 32 "32" 33 "33" 34 "34" ///
			35 "35" 36 "36" 37 "37" 38 "38" 39 "39" 40 "40" 41 "41" 42 "42" ///
			43 "43", labsize(vsmall))
graph export $DataDir/Regression4.png, replace 
/* After accounting for patient wait time, working long hours physicians, expected predicted length of stay, physician 14 seems to be the fastest discharging physician with decrease in log length of stay by about 0.2968 units instead of phy 10 (coefficient falls from -0.3352 to -0.1995, . Hence, there might be some questions about robustness of the original specification. */

//closing the log file
log close