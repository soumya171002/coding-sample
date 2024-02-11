***************************************************
* Stata coding sample
* By SOUMYA AGRAWAL
* On Feb 10, 2024
***************************************************
clear all
set mem 500m
set maxvar 20000
set more off
global DataDir "C:\Users\sagra\OneDrive\Desktop\Evaluation"


**********************************Cleaning Datasets****************************************************
* Open the individual level dataset
*2.1
use $DataDir\rural_2007_individual_socioeconomics1.dta, clear

*2.2 // renaming variables 
rename folio hhid
rename renglon personid

*2.3 //creating new variable by combining old variables to create unique village identifier 
capture drop villid
egen villid = concat(locali entidad munici), pun(_)
lab var villid "Unique Village Identifier"

*2.4 //creating a variable that specifies familysize of every unique household
capture drop familysize 
egen familysize = count(personid), by(hhid)
label variable familysize "number of inidividuals in the family"

*2.5 //creating a new dummy variable which is 1 if the person understands and speaks indigeneous language
tab h0317, missing //tabulating h0317 to see the labels.
tab h0317, nolabel //tabulating h0317 without labels

capture drop langhead //creating the dummy variable 
gen langhead = 0
replace langhead = 1 if h0317 == 1 | h0317 ==2 //creating a dummy variable which is 1 if the individual speaks and understands the indigeneous language 
label var langhead "whether the individual speaks and understands indigeneous languages" //labelling langhead
label define YesNo 1  "Yes" 0 "No" 
label values langhead YesNo //encoding langhead

*2.6 //sorting by hhid and then personid and saving the new dataset
sort hhid personid
save "HW1_individualData.dta", replace



*open household level dataset
*3.1
use $DataDir\rural_2007_household_socioeconomics.dta, clear
save $DataDir/hmk1household.dta, replace

*3.2 renaming variables and labelling them
rename folio hhid
label variable hhid "Household Identification Number"

*3.3 creating new variable by combining old variables using punctuation
capture drop villid 
egen villid = concat(locali entidad munici), pun(_) //creating unique id by combining old variables 
label var villid "Unique Village Identifier" //labelling the new variable 

*3.4 tabualting survey questions variables with and without label 
tab h8016a
tab h8016b
tab h8016a, nolabel
tab h8016b, nolabel

* creating a dummy varibale if household owns different assets
egen hasTV = anymatch(h8016*), v(6)
egen hasVP = anymatch(h8016*), v(7)
egen hasVG = anymatch(h8016*), v(8)
egen hasRD = anymatch(h8016*), v(9)
egen hasST = anymatch(h8016*), v(10)
egen hasMU = anymatch(h8016*), v(11)
egen hasPC = anymatch(h8016*), v(12)
egen hasRF = anymatch(h8016*), v(13)
egen hasSV = anymatch(h8016*), v(14)
egen hasFS = anymatch(h8016*), v(15)
egen hasBL = anymatch(h8016*), v(16)

*labelling new variables
label var hasTV "Household owns or has - TV"
label var hasVP "Household owns or has - movie player apparatus"
label var hasVG "Household owns or has - video game"
label var hasRD "Household owns or has - radio"
label var hasST "Household owns or has - recorder or stereo"
label var hasMU "Household owns or has - other musical system"
label var hasPC "Household owns or has - computer"
label var hasRF "Household owns or has - refrigerator"
label var hasSV "Household owns or has - Stoves"
label var hasFS "Household owns or has - other fuel stove"
label var hasBL "Household owns or has - blender"

*encoding new variables 
label val hasTV YesNo
label val hasVP YesNo
label val hasVG YesNo
label val hasRD YesNo
label val hasST YesNo
label val hasMU YesNo
label val hasPC YesNo
label val hasRF YesNo
label val hasSV YesNo
label val hasFS YesNo
label val hasBL YesNo


*3.5 keeping important variables 
keep hhid villid has*

sort hhid //sorting by hhid
save $DataDir/HW1_HouseholdData.dta, replace //saving new dataset


******************************************Merging dataset*********************************************************************************
*4

use "HW1_individualData", clear

merge m:1 hhid using "HW1_HouseholdData.dta" //merging individual dataset with the household dataset
tab _merge //merged perfectly 
drop _merge
save $DataDir/HW1_MergedData.dtda, replace //saving the merged dataset


***********************************Data analysis******************************************************************
*5.1

* H0 : The proportion of males in the sample is 50%
* Ha : The proportion of males in the sample is not 50%

* conducting the t-test
ttest sex = 0.5 // one sample t-test

** P-value is approx 0 (middle value) therefore reject the null hypothesis. However magnitude of the difference is not economically meaningful for the program 

*5.2
* H0 : Proportion of TV ownership does not differ between households that speak and understand indigeneous language and other households  
* Ha : Proportion of TV ownership differs between households that speak and understand indigeneous language and other households  

ttest hasTV, by(langhead) // two sample t-test
 
** P-value is approx 0 therefore reject the null hypothesis.
** The difference is economically meaningful as just over half the households that speak and understands indigeneous language have TV compared to almost three-quarters of other households.

*5.3 

* H0 : Proportion of PC ownership does not differ between households that speak and understand indigeneous language and other households  
* Ha : Proportion of PC ownership differs between households that speak and understand indigeneous language and other households 

ttest hasPC, by(langhead)

** P-value is approx 0 therefore reject the null hypothesis.
** The difference is not economically meaningful as just 0.6% of the households that speak and understands indigeneous language have PC compared to almost 1% of other households. Both figures are quite close to each other.

*5.4

regress hasTV langhead

**The results are same as compared to t-test
* The model is given by = 0.774 - 0.236(langhead).
* When langhead = 0 , pr(hasTV) = 0.774. When langhead is 1, pr(hasTV) = 0.774-0.236 = 0.538. Same as the t-test. P value is also same. In all, households that speak and understands native language have lower probability of owining a TV as compared to the other households 

