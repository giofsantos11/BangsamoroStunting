******PREPARING DATASET FOR ANALYSIS*******
**by: Angelo Santos

***Create folder names
global filenamemac "/Users/angelogabriellesantos/OneDrive - George Mason University/PUBP 804/Research project/"
global datafile "${filenamemac}Datasets/"

*Load the individual dataset
use "${datafile}MEMBER_DATA.dta", clear
*use "${datafile}HFSA_BARMM_HOUSEHOLD_CHARACTERISTICS_DATA_ALL_CASES_(2_08_2020).dta", clear
drop _m

*Merge with household dataset using interview__key and keeping only matched
merge m:1 interview__key using "${datafile}HFSA_BARMM_HOUSEHOLD_CHARACTERISTICS_DATA_ALL_CASES_(2_08_2020).dta", ///
	keep(match) nogen ///see notes on merging

	
*Create interview variable in date format
gen ID12_datestring = substr(ID12, 1, 10)
gen interviewdate_ID12 = date(ID12_datestring, "YMD")
format interviewdate_ID12 %td

*Household head education
gen educhead = HR08_A if HR04==1
bys interview__key: egen hhhead_educ = max(educhead)

gen educspouse = HR08_A if HR04==2
bys interview__key: egen spouse_educ = max(educspouse)

*employed adults
gen HR11_rc = HR11==1
bys interview__key: egen totalemployed = sum(HR11_rc)

*Create age in days
gen sex = HR05 //male =1 
gen age_created_days = interviewdate_ID12 - birthdate_HR06 
gen age_created_months = age_created_days/30.4375
gen age_created_years = age_created_days/365.25

*gen number of children
gen child_0_5 = age_created_months<60
gen child_0_14 = age_created_months<12*14
gen adult = age_created_months>12*18

bys interview__key: egen totalchild_0_5 = sum(child_0_5)
bys interview__key: egen totalchild_0_14 = sum(child_0_14)
bys interview__key: egen totaladults = sum(adult)

*gen household size
gen person = 1
bys interview__key: egen totalmems = sum(person)


drop if age_created_months>=60 //left with 6314 obs

*Create age groups
gen age_integer = int(age_created_years)
gen age_category = "Under 1" if age_integer==0
replace age_category = "1" if age_integer ==1
replace age_category = "2" if age_integer ==2
replace age_category = "3" if age_integer ==3
replace age_category = "4" if age_integer ==4
replace age_category = "5" if age_integer ==5

********************************************************************************
****Apply HH weights to replicate stunting rate from official records***********
********************************************************************************
tempfile collapse cph

preserve

	collapse (count) HOUSEHOLD, by(ID01 age_category sex)

	/*
	reshape wide HOUSEHOLD, i(ID01 age_category) j(sex)
	rename HOUSEHOLD_ROSTER_CONTROL_NUMBER1 male
	rename HOUSEHOLD_ROSTER_CONTROL_NUMBER2 female
	*/

	decode ID01, gen(Province)
	gen Agegroup = age_category

	save `collapse'

	*use CPH data
	import excel "${datafile}/Tables_for_analysis.xlsx", sheet("ARMM 0-5 population") firstrow clear
	rename male count1
	rename female count2

	reshape long count, i(Province Agegroup) j(sex)
	replace Agegroup = "Under 1" if Agegroup =="Under  1"
	replace Province = "Tawi-Tawi" if Province == "Tawi-tawi"

	merge 1:1 Province Agegroup sex using `collapse', keep(match) nogen
	drop Province Agegroup
	gen weight = count/HOUSEHOLD
	save `cph'
	

restore

*Merge CPH numbers
merge m:1 ID01 age_category sex using `cph'

*Create anthropometric z-scores
gen ave_height = AI03_AI04
recode ave_height (999=.)

gen ave_weight = AI01_AI02
recode ave_weight (999=.)

*Using zscore06 command
zscore06, a(age_created_month) s(sex) h(ave_height) w(ave_weight)



save "${datafile}MERGED.dta", replace




