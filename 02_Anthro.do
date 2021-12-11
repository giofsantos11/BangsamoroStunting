******PREPARING ANTHROPOMETRIC INDICATORS AND OTHER PREDICTORS*******
**by: Angelo Santos

global filenamemac "/Users/angelogabriellesantos/OneDrive - George Mason University/PUBP 804/Research project/"
global datafile "${filenamemac}Datasets/"

use "${datafile}MERGED.dta", clear

*histogram haz06, freq bin(20)  xline(6, lcolor(red) lpattern(dash)) xline(-6, lcolor(red) lpattern(dash)) graphregion(color(white))

*Drop biologically impossible scores
keep if inrange(haz06, -6, 6)

*Create variable identifying stunted
gen stunted = haz06 <-2
gen severelystunted = haz06<-3

tab stunted [iw = weight] //same result as 2018 Natl Nutrition Survey

*Reverse the factor order 
foreach var of varlist H2_1 H2_2 H2_5{
gen `var'_rc = 1 if `var' ==5
replace `var'_rc = 2 if `var' == 4
replace `var'_rc = 4 if `var' == 2
replace `var'_rc = 3 if `var' == 3
replace `var'_rc = 5 if `var' == 1

}

*Create factor score for knowledge of stunting consequences
factor H2_1_rc H2_2_rc H2_5_rc, pcf
estat kmo
screeplot, graphregion(color(white))

predict F1

tab stunted, sum(F1)
 

ttest F1, by(stunted) //better to use this!

*Negative height perception

gen notgoodgrades = H2_1<=2
gen goodfood = H1_2 >=2

*REceive subsidy
gen receive_cashtransfer = HESC42__4==1

*BReastfeeding
gen breastfed = BP01 ==1 | BP01==2

*Child had diarrhea
gen diarrhea = HS25_11 ==1

*Child had tb
gen tb = HS25_16==1

*Child had measles
gen measles = HS25_15==1

*Child had UR infection
gen respiratory = HS25__8==1

*Child had flu
gen flu = HS25__2==1

*Child had colds
gen colds = HS25__1==1

*Child had infection
gen infection = diarrhea ==1 | tb ==1 | measles ==1 | ///
respiratory ==1 | flu ==1 | colds ==1

*Number of other stunted children in the HH
bys interview__key: egen totalstunted = sum(stunted)
gen otherstunted = totalstunted - stunted
gen w_otherstunted = otherstunted>=1

*Number of stunted children in the village
gen index = 1
bys BARANGAY: egen brgy_stunted = sum(stunted)
bys BARANGAY: egen brgy_kids =sum(index)

replace brgy_stunted = brgy_stunted - totalstunted
replace brgy_kids = brgy_kids - totalchild_0_5

*Recode HS32
gen HS32_rc = HS32
recode HS32_rc (1=0) (2=1) (3=2) (4=2) (5=2)

*Create religion
tab HESC03, gen(religion)

*Create age squared
gen agesquared = age_created_years^2

*Share of employed
gen sh_employed = totalemployed/totalmems

*Owns livestock
egen total_livestock = rowtotal(HESC55__1 - HESC55__5)
gen w_livestock = total_livestock>=1

*Food expenditures per capita
gen foodpc = HESC44/totalmems

*Primary caregiver
gen caregiver_mom = HESC11==1

***Create income vble in thousands
gen hh_income = Household_per/1000
ren religion2 Islam

replace hhhead_educ = 8 if hhhead_educ == 9

gen w_toilet = HESC32==1

gen w_philhealth = HS39==1

gen watersource = HESC27

gen stunted_brgy_sh = brgy_stunted/brgy_kids

save "${datafile}REGRESSION.dta", replace



/*
*Income round to nearest thousands
gen foodpc100 = foodpc/100
replace foodpc100 = int(foodpc100)

collapse (mean) haz06, by(foodpc100)
logit stunted Household_per i.ID01 i.Barangay age_created_months brgy_stunted ///
i.GIDA notgoodgrades goodfood d_20miles receive_cashtransfer i.HS32 HESC44 ///
breastfed HS39 HESC32 i.HS20[pw=weight]


logit stunted i.Household_income_gr i.ID01 i.Barangay age_created_months brgy_stunted ///
i.GIDA notgoodgrades goodfood d_20miles receive_cashtransfer i.HS32 HESC44 ///
breastfed HS39 HESC32 i.HS20[pw=weight]

gen cg_notmother = HESC11!=1
replace cg_notmother= . if HESC11==97


logit severelystunted i.Household_income_gr i.ID01 i.Barangay age_created_months brgy_stunted ///
i.GIDA notgoodgrades goodfood d_20miles receive_cashtransfer i.HS32 HESC44 ///
breastfed HS39 HESC32 i.HS20 cg_notmother [pw=weight] 
*/

*Factor analysis
/*

*Convert the 5-point Likert to 3-point
foreach num of num 1/9{
	gen H2n_`num' = H2_`num'
	recode H2n_`num' (2 = 1) (3 = 2) (4 = 3) (5 = 3)
}

foreach num of num 1/9{
	gen H2d_`num' = H2_`num'
	recode H2d_`num' (2 = 0) (1=0) (3 = 0) (4 = 1) (5 = 1)
}

logit haz06 Household_per i.ID01 i.Barangay age_created_years agesquared ///
i.GIDA HESC44 w_otherstunted brgy_stunted sh_employed i.HESC27 HESC59__5 HESC59__1 ///
HESC59__6 breastfed HS39 HESC32 i.HS20 F1 diarrhea i.hhhead w_livestock ///
religion2 foodpc caregiver_mom HS17 HS23 [pw=weight]

ivprobit stunted Household_per i.ID01 i.Barangay age_created_years agesquared ///
i.GIDA HESC44 w_otherstunted brgy_stunted sh_employed i.HESC27 HESC59__5 HESC59__1 ///
HESC59__6 breastfed HS39 HESC32 i.HS20 diarrhea i.hhhead w_livestock  ///
religion2 foodpc caregiver_mom HS17 HS23 (F1 = i.H2_3 i.H2_4 i.H2_9)


ivprobit stunted Household_per i.ID01 i.Barangay age_created_years agesquared ///
i.GIDA HESC44 w_otherstunted brgy_stunted sh_employed i.HESC27 HESC59__5 HESC59__1 ///
HESC59__6 breastfed HS39 HESC32 i.HS20 diarrhea i.hhhead w_livestock  ///
religion2 foodpc caregiver_mom HS17 HS23 (F1 = i.H2_3 i.H2_4 i.H2_8 i.H2_9)  [pw=weight]

ivprobit stunted Household_per i.ID01 i.Barangay age_created_years agesquared ///
i.GIDA HESC44 w_otherstunted brgy_stunted sh_employed i.HESC27 HESC59__5 HESC59__1 ///
HESC59__6 breastfed HS39 HESC32 i.HS20 diarrhea w_livestock  ///
religion2 foodpc caregiver_mom HS17 HS23 i.hhhead_educ (F1 = i.H2_3 i.H2_4  i.spouse_educ)
 
 reg F1 i.H2_3 i.H2_4  i.spouse_educ
 predict yhat2
 
 probit stunted Household_per i.ID01 i.Barangay age_created_years agesquared ///
i.GIDA HESC44 w_otherstunted brgy_stunted sh_employed i.HESC27 HESC59__5 HESC59__1 ///
HESC59__6 breastfed HS39 HESC32 i.HS20 diarrhea w_livestock  ///
religion2 foodpc caregiver_mom HS17 HS23 yhat2 i.hhhead


//shows significant result


logit stunted Household_per i.ID01 i.Barangay age_created_years agesquared ///
i.GIDA HESC44 w_otherstunted sh_employed i.HESC27 HESC59__5 HESC59__1 ///
HESC59__6 breastfed HS39 HESC32 i.HS20 F1 diarrhea i.hhhead w_livestock ///
religion2 foodpc caregiver_mom  HS17 HS23 [pw=weight]

ivprobit stunted Household_per i.ID01 i.Barangay age_created_years agesquared ///
i.GIDA HESC44 w_otherstunted sh_employed i.HESC27 HESC59__5 HESC59__1 ///
HESC59__6 breastfed HS39 HESC32 i.HS20 diarrhea i.hhhead w_livestock  ///
religion2 foodpc caregiver_mom HS17 HS23  (F1 = i.H2_3 i.H2_4  i.spouse_educ), twostep 

overid, depvar(stunted)

logit stunted Household_per i.ID01 i.Barangay age_created_years agesquared ///
i.GIDA HESC44 w_otherstunted brgy_stunted sh_employed i.HESC27 HESC59__5 HESC59__1 ///
HESC59__6 breastfed HS39 HESC32 i.HS20 yhat diarrhea i.hhhead w_livestock ///
religion2 foodpc caregiver_mom HS17 HS23 [pw=weight]

fsum stunted Household_per ID01 i.Barangay age_created_years agesquared ///
i.GIDA HESC44 w_otherstunted brgy_stunted sh_employed HESC27 HESC59__5 HESC59__1 ///
HESC59__6 breastfed HS39 HESC32 HS20 F1 diarrhea hhhead w_livestock ///
religion2 foodpc caregiver_mom HS17 HS23 



margins, dydx(*) //means all indp vbles

predict prob_dv, pr
* Calculating the probability of a subject being a libuser

gen pred_affrmact_b = 1 if prob >= .5 & prob ~= .
* Note that missing is defined as the largest number (inf) in stata.

replace pred_affrmact_b = 0 if prob < .5

tab pred_affrmact_b stunted, row



logit stunted Household_per i.ID01 i.Barangay age_created_months ///
i.GIDA i.HS32 HESC44 ///
breastfed HS39 HESC32 i.HS20 F1 diarrhea flu [pw=weight]

logit stunted Household_per i.ID01 i.Barangay age_created_months brgy_stunted ///
i.GIDA  receive_cashtransfer i.HS32 HESC44 ///
breastfed HS39 HESC32 i.HS20 goodsmall F1 
