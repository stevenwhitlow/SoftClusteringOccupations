cd /Users/steven/Documents/research/occupations

/*
Plot the estimated minimum and maximum degrees of membership
*/
import delimited "./model_output/classifications.csv", encoding(ISO-8859-1) clear
global plist group_freqs "freq1 freq2 freq3 freq4 freq5 freq6 freq7"
egen mode = rowmax(`group_freqs')
egen rank = rank(mode)
sort rank
twoway line mode rank

egen least = rowmin(`group_freqs')
egen rank_least = rank(least)
sort rank_least
twoway line least rank_least 

/*
Decomposition of occupational mean wages and rankings of occupations by wage
*/

use ./data/us_census_data_2000.dta, clear
rename occ occ00
drop if wkswork1 < 35
drop if uhrswork <35
merge m:m occ00 using onet_census00.dta
drop if _merge!=3
drop _merge
gen lnhrwage = ln(incwage/(wkswork1*uhrswork))
gen lnweeklywage = ln(incwage/(wkswork1))
egen mode = rowmax( freq1 freq2 freq3 freq4 freq5 freq6 freq7 )

gen educ_group = (educd<=61)
replace educ_group = 2 if educd>61 & educd<=65
replace educ_group = 3 if educd>65 & educd<101
replace educ_group = 4 if educd>=101
		
gen dropout = educ_group == 1
gen hsgrad = educ_group == 2
gen somecollege = educ_group == 3
gen collegeplus = educ_group == 4

gen high_educ = (educ_group == 3| educ_group==4)
		
gen age2 = age^2

//OLS regression with group membership vectors (freq7, low-skill service omitted), education FEs and quadratic in age
reg lnhrwage freq1 freq2 freq3 freq4 freq5 freq6 dropout-collegeplus age age2, cluster(occ00)

gen modal_type = .

foreach i in 1 2 3 4 5 6 7{
    replace modal_type = `i' if mode==freq`i'
}

//Generate ranking of occupations by hourly wage and its decomposition into task-specific and residual variation
preserve
	collapse (mean) lnhrwage freq1 freq2 freq3 freq4 freq5 freq6 freq7 modal_type dropout-collegeplus (rawsum) perwt [pweight=perwt], by(occ00)
	foreach i in 1 2 3 4 5 6 7{
        gen freq`i'_2 = freq`i'^2
    }
	reg lnhrwage i.modal_type, robust
	reg lnhrwage freq1 freq2 freq3 freq4 freq5 freq6 freq1_2-freq7_2, robust
    reg lnhrwage freq1 freq2 freq3 freq4 freq5 freq6 dropout-collegeplus, robust
	reg lnhrwage freq1 freq2 freq3 freq4 freq5 freq6, robust
	predict wage_task_hourly
	predict wage_resid_hourly, resid
	egen rank_task_hourly = rank(wage_task_hourly)
	egen rank_resid_hourly = rank(wage_resid_hourly)
	egen rank_all_hourly = rank(lnhrwage)
	keep occ00 rank_resid_hourly rank_task_hourly wage_task_hourly wage_resid_hourly rank_all_hourly
	save ranks_hourly.dta, replace
restore

preserve
	collapse (mean) lnweeklywage `group_freqs' (rawsum) perwt [pweight=perwt], by(occ00)
	reg lnweeklywage `group_freqs'
	predict wage_task_weekly
	predict wage_resid_weekly, resid
	egen rank_task_weekly = rank(wage_task_weekly)
	egen rank_resid_weekly = rank(wage_resid_weekly)
	egen rank_all_weekly = rank(lnweeklywage)
	keep occ00 rank_resid_weekly rank_task_weekly wage_task_weekly wage_resid_weekly rank_all_weekly
	save ranks_weekly.dta, replace 
restore

/*Merge weekly and hourly wage rankings*/
use ranks_hourly.dta, clear
merge 1:1 occ00 using ranks_weekly.dta, nogenerate
save ranks.dta, replace
