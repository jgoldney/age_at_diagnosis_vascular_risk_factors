
///************************************///
///Preparation///
///************************************///

cls

use "...", clear

keep patid exp age_index gender imd bmi eth hba1c ldl hypertension_5						
mdesc

egen age_bands = cut(age_index), at(0 16 23 30 37 44 51)
label define Ab 16 "16-22" 23 "23-29" 30 "30-36" 37 "37-43" 44 "44-50"
label values age_bands Ab
tabstat age_index, statistics(min max) by(age_bands)							

gen 	eths = 99
replace eths = . if missing(eth)
replace eths = 1 if eth == 0
replace eths = 2 if eth == 1 | eth == 2 | eth == 3
replace eths = 3 if eth == 4 | eth == 6 | eth == 7 
replace eths = 4 if eth == 8 | eth == 9 | eth == 10 | eth == 5
label define eths 1 "White" 2 "Black" 3 "South Asian" 4 "Other"
label values eths eths
tab eths, m																		

label define sex 1 "men" 2 "women"
label values gender sex
tab gender, m	
rename gender sex																

label define diab 0 "No_diabetes" 1 "Diabetes"
label values exp diab

tab eths, m																		
tab imd, m
															
mdesc	
replace hba1c=. if hba1c !=. & (hba1c<3 | hba1c>20)		
replace bmi=. if bmi !=. & (bmi<15 | bmi>80)
							
gen obesity2 = .
replace obesity2=1 if eths==1 & bmi>=30							
replace obesity2=1 if eths==2 & bmi>=27.5							
replace obesity2=1 if eths==3 & bmi>=27.5
replace obesity2=1 if eths==4 & bmi>=27.5							
replace obesity2=0 if missing(obesity2)
replace obesity2=. if missing(bmi)
sum obesity2 bmi

cls 																		
by obesity2, sort: tabstat bmi, statistics(count min max) by(eths) 			
label define obesity 0 "No obesity" 1 "Obesity"
label values obesity2 obesity 								

							

*********************************************************************************************
///Table 1: describe ethnicity composition in diabetes and diabetes free cohorts by age_bands
*********************************************************************************************

preserve 
keep if exp==0
dtable age_index i.sex i.eths i.imd bmi i.obesity2 ldl hba1c i.hypertension_5, by(age_bands) nformat(%9.1fc) continuous(age_index bmi ldl hba1c, statistics(median q1 q3)) export("...", modify sheet("Tab1No_Diabetes1"))
restore

preserve 
keep if exp==1
dtable age_index i.sex i.eths i.imd bmi i.obesity2 ldl hba1c i.hypertension_5, by(age_bands) nformat(%9.1fc) continuous(age_index bmi ldl hba1c, statistics(median q1 q3)) export"...", modify sheet("Tab1Diabetes1"))
restore
																	

********************************************************************************************************************************
////Suppl Fig 1: Predicted difference in obesity (coef of change) by age_group for each ethnicity or imd
********************************************************************************************************************************
preserve
bys age_bands: mdesc bmi if exp == 0										/*For baseline table*/
bys age_bands: mdesc bmi if exp == 1
bys exp: mdesc bmi 															/*For baseline table*/
drop if bmi == .
mdesc bmi
by exp, sort: tabstat bmi, statistics(count median p25 p75) by(age_bands)
restore

forval s = 1/2 {
	preserve
	keep if sex == `s'
	drop if bmi == .
	logit obesity2 i.eths##i.age_bands##i.exp
	margins r.exp, at(eths=(1 2 3 4) age_bands=(16 23 30 37 44)) expression(ln(predict())) /*
	*/	saving("...`s'", replace)
	
	logit obesity2 i.imd##i.age_bands##i.exp
	margins r.exp, at(imd=(1 2 3 4 5) age_bands=(16 23 30 37 44)) expression(ln(predict())) /*
	*/	saving("...`s'", replace)
	restore
}

******************************************************
////Figure 1: Predicted bmi by age_group and ethnicity and imd strat sex
****************************************************** 
forval s = 1/2 {
	preserve
	keep if sex == `s'
	drop if bmi == .
	regress bmi i.exp##i.eths##i.age_bands
	margins r.exp, at(eths=(1 2 3 4) age_bands=(16 23 30 37 44)) /*
	*/	saving("...`s'", replace) 
	margins exp,   at(eths=(1 2 3 4) age_bands=(16 23 30 37 44)) /*
	*/	saving("...`s'", replace) 


	regress bmi i.exp##i.imd##i.age_bands
	margins r.exp, at(imd=(1 2 3 4 5) age_bands=(16 23 30 37 44)) /*
	*/	saving("...`s'", replace) 
	margins exp,   at(imd=(1 2 3 4 5) age_bands=(16 23 30 37 44)) /*
	*/	saving("...`s'", replace) 
	restore
}


***********************************************************
////Figure 2: Predicted ldl by age_group for each ethnicity/imd 
***********************************************************

forval s = 1/2 {
	preserve 
	keep if sex == `s'
	drop if ldl == .								
	regress ldl i.exp##i.imd##i.age_bands
	margins r.exp, at(imd=(1 2 3 4 5) age_bands=(16 23 30 37 44)) /*
	*/	saving("...`s'", replace) 
	margins exp,   at(imd=(1 2 3 4 5) age_bands=(16 23 30 37 44)) /*
	*/	saving("...`s'", replace) 
	restore
}

forval s = 1/2 {
	preserve 
	keep if sex == `s'
	drop if ldl == .								
	regress ldl i.exp##i.eths##i.age_bands
	margins r.exp, at(eths=(1 2 3 4) age_bands=(16 23 30 37 44)) /*
	*/	saving("...`s'", replace) 
	margins exp,   at(eths=(1 2 3 4) age_bands=(16 23 30 37 44)) /*
	*/	saving("...`s'", replace) 
	restore
}


********************************************************************************************************************************
////Fig 3: Predicted difference in hypertension (coef of change) by age_group for each ethnicity or imd
********************************************************************************************************************************

forval s = 1/2 {
	preserve
	keep if sex == `s'
	logit hypertension_5 i.eths##i.age_bands##i.exp
	margins r.exp, at(eths=(1 2 3 4) age_bands=(16 23 30 37 44)) expression(ln(predict())) /*
	*/	saving("...`s'", replace)
	
	logit hypertension_5 i.imd##i.age_bands##i.exp
	margins r.exp, at(imd=(1 2 3 4 5) age_bands=(16 23 30 37 44)) expression(ln(predict())) /*
	*/	saving("...`s'", replace)
	restore
}


**********************************************************
////Fig 4: Predicted HbA1c by age_group for each ethnicity/imd 
**********************************************************
forval s = 1/2 {
	preserve
	keep if sex == `s'
	drop if exp == 0
	sum hba1c															
	bys age_bands: mdesc hba1c		 											/*For baseline table*/
	mdesc hba1c																		
	drop if hba1c == .																
	tab eths age_bands																
	regress hba1c i.eths##i.age_bands
	margins eths, at(age_bands=(16 23 30 37 44)) /*
	*/	saving("...`s'", replace)
	restore
}

forval s = 1/2 {
	preserve
	keep if sex == `s'
	drop if exp == 0
	sum hba1c																		
	bys age_bands: mdesc hba1c		 											/*For baseline table*/
	mdesc hba1c																		
	drop if hba1c == .																
	tab imd age_bands																
	regress hba1c i.imd##i.age_bands
	margins imd, at(age_bands=(16 23 30 37 44)) /*
	*/	saving("...`s'", replace)
	restore
}


******************
******************
////GRAPHS********
******************
******************


drop _all
graph drop _all
preserve
clear
tempfile testres
save `testres', emptyok replace
restore

cd "..."
local all_files : dir . files "*"
di `all_datafiles'
foreach datafile of local all_files {
	preserve
	use `datafile'
	gen file_name = "`datafile'"
	qui append using `testres', force
	qui save `testres', replace
	restore
}

use `testres', clear

gen outcome = subinstr(file_name, ".dta", "", .)
gen rrr = exp(_margin)
gen rrr_lci = exp(_ci_lb)
gen rrr_uci = exp(_ci_ub)
gen rrr2 = 1


	**********************************************************************************************
	///MEN
*
**********************************************************************************************
	////Figure 1: absolute bmi and change bmi across diabetes status, ethnicity or imd and age at diagnosis 
	**********************************************************************************************
	
graph drop _all

	 
	 
	///bmi absolute by diabetes status, ethnicity and age 
	preserve
	keep if outcome =="bmi_eth_predict1"
	label values _m1 diab
	label values _at2 eths
	label values _at3 Ab
	graph twoway scatter _margin _at3 if _at2==1 & _m1==1, mcolor(red)  || scatter _margin _at3 if _at2==2 & _m1==1, mcolor(green) || scatter _margin _at3 if _at2==3 & _m1==1, mcolor(dkorange) || scatter _margin _at3 if _at2==4 & _m1==1, mcolor(lavender) || rcap _ci_lb _ci_ub _at3 if _at2==1 & _m1==1, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2 &_m1==1, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3 & _m1==1, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4 & _m1==1, color(lavender%50)|| scatter _margin _at3 if _at2==1 & _m1==0, mcolor(red) msymbol(triangle) lpattern(dash dot dot)  || scatter _margin _at3 if _at2==2 & _m1==0, mcolor(green) msymbol(triangle) lpattern(dash dot dot) || scatter _margin _at3 if _at2==3 & _m1==0, mcolor(dkorange) msymbol(triangle) lpattern(dash dot dot)|| scatter _margin _at3 if _at2==4 & _m1==0, mcolor(lavender) msymbol(triangle) lpattern(dash dot dot) || rcap _ci_lb _ci_ub _at3 if _at2==1 & _m1==0, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2 & _m1==0, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3 & _m1==0, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4 &_m1==0, color(lavender%50) sort legend(order(2 4 3 1 10 12 11 9) label(1 "White + Diabetes") label(2 "Black + Diabetes") label(3 "South Asian+ Diabetes") label(4 "Other + Diabetes") label(9 "White + No diabetes") label(10 "Black + No diabetes") label(11 "South Asian+ No diabetes") label(12 "Other + No diabetes")) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("BMI (kg/m{superscript:2})") name("BMI_by_eth1") caption("B",pos(10) size(large)) legend(off)
	rename _m1 diabetes_status
	rename _at2 ethnicity
	rename _at3 age_bands
	decode ethnicity, generate(eth2)
	drop ethnicity
	rename eth2 ethnicity
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
	decode diabetes_status, generate(new)
	drop diabetes_status
	rename new diabetes_status
	keep outcome diabetes_status age_bands ethnicity _margin _ci_lb _ci_ub
	order outcome diabetes_status age_bands ethnicity _margin _ci_lb _ci_ub
	export excel using "...", sheet(Figure1B, replace) firstrow(variables) 
	restore

	preserve
keep if outcome =="bmi_imd_predict1"
label values _m1 diab
label values _at2 imd
label values _at3 Ab
graph twoway scatter _margin _at3 if _at2==1 & _m1==1, mcolor(red)  || scatter _margin _at3 if _at2==2 & _m1==1, mcolor(green) || scatter _margin _at3 if _at2==3 & _m1==1, mcolor(dkorange) || scatter _margin _at3 if _at2==4 & _m1==1, mcolor(lavender) || scatter _margin _at3 if _at2==5 & _m1==1, mcolor(brown*1.5) || rcap _ci_lb _ci_ub _at3 if _at2==1 & _m1==1, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2 &_m1==1, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3 & _m1==1, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4 & _m1==1, color(lavender%50)|| rcap _ci_lb _ci_ub _at3 if _at2==5 & _m1==1, color(brown*1.5%50)|| scatter _margin _at3 if _at2==1 & _m1==0, mcolor(red) msymbol(triangle) lpattern(dash dot dot)  || scatter _margin _at3 if _at2==2 & _m1==0, mcolor(green) msymbol(triangle) lpattern(dash dot dot) || scatter _margin _at3 if _at2==3 & _m1==0, mcolor(dkorange) msymbol(triangle) lpattern(dash dot dot)|| scatter _margin _at3 if _at2==4 & _m1==0, mcolor(lavender) msymbol(triangle) lpattern(dash dot dot) || scatter _margin _at3 if _at2==5 & _m1==0, mcolor(brown*1.5) || rcap _ci_lb _ci_ub _at3 if _at2==1 & _m1==0, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2 & _m1==0, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3 & _m1==0, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4 &_m1==0, color(lavender%50) || rcap _ci_lb _ci_ub _at3 if _at2==5 & _m1==0, color(brown*1.5%50) sort legend(order(1 2 3 4 5 11 12 13 14 15) label(1 "Quintile 1 + Diabetes") label(2 "Quintile 2 + Diabetes") label(3 "Quintile 3 + Diabetes") label(4 "Quintile 4 + Diabetes") label(5 "Quintile 5 + Diabetes")  label(11 "Quintile 1 + No diabetes") label(12 "Quintile 2 + No diabetes") label(13 "Quintile 3 + No diabetes") label(14 "Quintile 4 + No diabetes") label(15 "Quintile 5 + No diabetes")) yscale(range(20 41.5)) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("BMI (kg/m{superscript:2})") name("BMI_by_imd1") caption("F",pos(10) size(large)) legend(off)
rename _m1 diabetes_status
rename _at2 imd
rename _at3 age_bands
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
	decode diabetes_status, generate(new)
	drop diabetes_status
	rename new diabetes_status
keep outcome diabetes_status age_bands imd _margin _ci_lb _ci_ub
order outcome diabetes_status age_bands imd _margin _ci_lb _ci_ub
export excel using "...", sheet(Figure1F, replace) firstrow(variables) 
restore

///bmi diff eth
preserve
keep if outcome =="bmi_eth_constrast1"
label values _at2 eths
label values _at3 Ab
graph twoway scatter _margin _at3 if _at2==1, mcolor(red)  || scatter _margin _at3 if _at2==2, mcolor(green) || scatter _margin _at3 if _at2==3, mcolor(dkorange) || scatter _margin _at3 if _at2==4, mcolor(lavender) || rcap _ci_lb _ci_ub _at3 if _at2==1, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4, color(lavender%50) sort legend(order(2 4 3 1) label(1 "White") label(2 "Black") label(3 "South Asian") label(4 "Other")) xlabel(16 23 30 37 44,valuelabel) ylabel(1 5 10 15)  xtitle("Age group (years)")  ytitle("Difference in BMI (kg/m{superscript:2})" "(diabetes vs. no diabetes)")  name("Change_BMI_by_eth1") caption("D", pos(10) size(large)) yscale(range(2 17.5)) ylabel(5 10 15) legend(off)
rename _at2 ethnicity
rename _at3 age_bands
	decode ethnicity, generate(eth2)
	drop ethnicity
	rename eth2 ethnicity
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
keep outcome age_bands ethnicity _margin _ci_lb _ci_ub
order outcome age_bands ethnicity _margin _ci_lb _ci_ub
export excel using "...", sheet(Figure1D, replace) firstrow(variables) 
restore

preserve
keep if outcome =="bmi_imd_constrast1"
label values _at3 Ab
graph twoway scatter _margin _at3 if _at2==1, mcolor(red)  || scatter _margin _at3 if _at2==2, mcolor(green) || scatter _margin _at3 if _at2==3, mcolor(dkorange) || scatter _margin _at3 if _at2==4, mcolor(lavender)|| scatter _margin _at3 if _at2==5, mcolor(brown*1.5)  || rcap _ci_lb _ci_ub _at3 if _at2==1, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4, color(lavender%50) || rcap _ci_lb _ci_ub _at3 if _at2==5, color(brown*1.5%50) sort legend(order(1 2 3 4 5) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 2") label(3 "Quintile 3") label(4 "Quintile 4") label(5 "Quintile 5 (most deprived)")) xlabel(16 23 30 37 44,valuelabel) ylabel(1 5 10 15)  xtitle("Age group (years)")  ytitle("Difference in BMI (kg/m{superscript:2})" "(diabetes vs. no diabetes)") name("Change_BMI_by_imd1") caption("H", pos(10) size(large)) legend(off) yscale(range(5 20)) ylabel(5 10 15 20)
rename _at2 imd
rename _at3 age_bands
keep outcome age_bands imd _margin _ci_lb _ci_ub
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
order outcome age_bands imd _margin _ci_lb _ci_ub
export excel using "...", sheet(Figure1H, replace) firstrow(variables) 
restore



**********************************************************************************************
////Figure 2: absolute ldl and change ldl across diabetes status, ethnicity/imd and age at diagnosis 
**********************************************************************************************

///ldl absolute by diabetes status, ethnicity/imd and age 
 preserve
keep if outcome =="ldl_eths_predict1"
label values _m1 diab
label values _at2 eths
label values _at3 Ab
graph twoway scatter _margin _at3 if _at2==1 & _m1==1, mcolor(red)  || scatter _margin _at3 if _at2==2 & _m1==1, mcolor(green) || scatter _margin _at3 if _at2==3 & _m1==1, mcolor(dkorange) || scatter _margin _at3 if _at2==4 & _m1==1, mcolor(lavender) || rcap _ci_lb _ci_ub _at3 if _at2==1 & _m1==1, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2 & _m1==1, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3 & _m1==1, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4 & _m1==1, color(lavender%50)|| scatter _margin _at3 if _at2==1 & _m1==0, mcolor(red) msymbol(triangle) lpattern(dash dot dot)  || scatter _margin _at3 if _at2==2 & _m1==0, mcolor(green)msymbol(triangle) lpattern(dash dot dot) || scatter _margin _at3 if _at2==3 & _m1==0, mcolor(dkorange)msymbol(triangle) lpattern(dash dot dot)|| scatter _margin _at3 if _at2==4 & _m1==0, mcolor(lavender) msymbol(triangle) lpattern(dash dot dot) || rcap _ci_lb _ci_ub _at3 if _at2==1 & _m1==0, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2 & _m1==0, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3 & _m1==0, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4 & _m1==0, color(lavender%50) sort legend(order(1 2 3 4 9 10 11 12) label(1 "White + Diabetes") label(2 "Black + Diabetes") label(3 "South Asian+ Diabetes") label(4 "Other + Diabetes") label(9 "White + No diabetes") label(10 "Black + No diabetes") label(11 "South Asian+ No diabetes") label(12 "Other + No diabetes")) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("LDL cholesterol (mmol/L)") name("LDL_by_eth1") caption("B", pos(10) size(large)) yscale(range(1.8 4)) legend(off)
rename _at2 ethnicity
rename _m1 diabetes_status
rename _at3 age_bands
	decode ethnicity, generate(eth2)
	drop ethnicity
	rename eth2 ethnicity
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
	decode diabetes_status, generate(new)
	drop diabetes_status
	rename new diabetes_status
	keep outcome diabetes_status age_bands ethnicity _margin _ci_lb _ci_ub
keep outcome diabetes_status age_bands ethnicity _margin _ci_lb _ci_ub
order outcome diabetes_status age_bands ethnicity _margin _ci_lb _ci_ub
export excel using "...", sheet(Figure2B, replace) firstrow(variables)
restore

preserve
keep if outcome =="ldl_imd_predict1"
label values _m1 diab
label values _at2 imd
label values _at3 Ab
graph twoway scatter _margin _at3 if _at2==1 & _m1==1, mcolor(red)  || scatter _margin _at3 if _at2==2 & _m1==1, mcolor(green) || scatter _margin _at3 if _at2==3 & _m1==1, mcolor(dkorange) || scatter _margin _at3 if _at2==4 & _m1==1, mcolor(lavender) || scatter _margin _at3 if _at2==5 & _m1==1, mcolor(brown*1.5) || rcap _ci_lb _ci_ub _at3 if _at2==1 & _m1==1, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2 &_m1==1, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3 & _m1==1, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4 & _m1==1, color(lavender%50)|| rcap _ci_lb _ci_ub _at3 if _at2==5 & _m1==1, color(brown*1.5%50)|| scatter _margin _at3 if _at2==1 & _m1==0, mcolor(red) msymbol(triangle) lpattern(dash dot dot)  || scatter _margin _at3 if _at2==2 & _m1==0, mcolor(green) msymbol(triangle) lpattern(dash dot dot) || scatter _margin _at3 if _at2==3 & _m1==0, mcolor(dkorange) msymbol(triangle) lpattern(dash dot dot)|| scatter _margin _at3 if _at2==4 & _m1==0, mcolor(lavender) msymbol(triangle) lpattern(dash dot dot) || scatter _margin _at3 if _at2==5 & _m1==0, mcolor(brown*1.5) || rcap _ci_lb _ci_ub _at3 if _at2==1 & _m1==0, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2 & _m1==0, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3 & _m1==0, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4 &_m1==0, color(lavender%50) || rcap _ci_lb _ci_ub _at3 if _at2==5 & _m1==0, color(brown*1.5%50) sort legend(order(1 2 3 4 5 11 12 13 14 15) label(1 "Quintile 1 + Diabetes") label(2 "Quintile 2 + Diabetes") label(3 "Quintile 3 + Diabetes") label(4 "Quintile 4 + Diabetes") label(5 "Quintile 5 + Diabetes")  label(11 "Quintile 1 + No diabetes") label(12 "Quintile 2 + No diabetes") label(13 "Quintile 3 + No diabetes") label(14 "Quintile 4 + No diabetes") label(15 "Quintile 5 + No diabetes")) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("LDL cholesterol (mmol/L)") name("LDL_by_imd1") caption("F", pos(10) size(large)) yscale(range(1 4)) ylabel(1 2 3 4) legend(off) 
rename _m1 diabetes_status
rename _at2 imd
rename _at3 age_bands
keep outcome diabetes_status age_bands imd _margin _ci_lb _ci_ub
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
	decode diabetes_status, generate(new)
	drop diabetes_status
	rename new diabetes_status
order outcome diabetes_status age_bands imd _margin _ci_lb _ci_ub
export excel using "...", sheet(Figure2F, replace) firstrow(variables) 
restore

///ldl diff 
preserve
keep if outcome =="ldl_eths_constrast1"
label values _at2 eths
label values _at3 Ab
gen null=0
graph twoway scatter _margin _at3 if _at2==1, mcolor(red)  || scatter _margin _at3 if _at2==2, mcolor(green) || scatter _margin _at3 if _at2==3, mcolor(dkorange) || scatter _margin _at3 if _at2==4, mcolor(lavender) || rcap _ci_lb _ci_ub _at3 if _at2==1, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4, color(lavender%50) || line null _at3, lpattern(dash) lcolor(black%50) sort legend(order(1 2 3 4) label(1 "White") label(2 "Black") label(3 "South Asian") label(4 "Other")) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("Difference in LDL cholesterol (mmol/l)" "(diabetes vs. no diabetes)") name("Change_LDL_by_eth1") caption("D", pos(10) size(large)) legend(off) yscale(range(-1 1.5)) ylabel(-1 -0.5 0 0.5 1 1.5)
rename _at2 ethnicity
rename _at3 age_bands
	decode ethnicity, generate(eth2)
	drop ethnicity
	rename eth2 ethnicity
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
keep outcome age_bands ethnicity _margin _ci_lb _ci_ub
order outcome age_bands ethnicity _margin _ci_lb _ci_ub
export excel using "...", sheet(Figure2D, replace) firstrow(variables)
restore

preserve
keep if outcome =="ldl_imd_constrast1"
label values _at3 Ab
gen null=0
graph twoway scatter _margin _at3 if _at2==1, mcolor(red)  || scatter _margin _at3 if _at2==2, mcolor(green) || scatter _margin _at3 if _at2==3, mcolor(dkorange) || scatter _margin _at3 if _at2==4, mcolor(lavender)|| scatter _margin _at3 if _at2==5, mcolor(brown*1.5)  || rcap _ci_lb _ci_ub _at3 if _at2==1, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4, color(lavender%50) || rcap _ci_lb _ci_ub _at3 if _at2==5, color(brown*1.5%50) || line null _at3, lpattern(dash) lcolor(black%50) sort legend(order(1 2 3 4 5) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 2") label(3 "Quintile 3") label(4 "Quintile 4") label(5 "Quintile 5 (most deprived)")) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("Difference in LDL cholesterol (mmol/l)" "(diabetes vs. no diabetes)") name("Change_LDL_by_imd1") caption("H", pos(10) size(large)) legend(off) yscale(range(-1.1 2.1)) ylabel(-1 -0.5 0 0.5 1 1.5 2)
rename _at2 imd
rename _at3 age_bands
keep outcome age_bands imd _margin _ci_lb _ci_ub
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
order outcome age_bands imd _margin _ci_lb _ci_ub
export excel using "...", sheet(Figure2H, replace) firstrow(variables) 
restore


**********************************************************************************************
///Fig 3: RRR htn by eth/imd
**********************************************************************************************
preserve
keep if outcome =="htn_by_eth1"
label values _at2 Ab
label values _at1 eths
graph twoway scatter rrr _at2 if _at1==1, mcolor(red)  || scatter rrr _at2 if _at1==2, mcolor(green) || scatter rrr _at2 if _at1==3, mcolor(dkorange) || scatter rrr _at2 if _at1==4, mcolor(lavender) || rcap rrr_lci rrr_uci _at2 if _at1==1, color(red%50) || rcap rrr_lci rrr_uci _at2 if _at1==2, color(green%50) || rcap rrr_lci rrr_uci _at2 if _at1==3, color(dkorange%50) || rcap rrr_lci rrr_uci _at2 if _at1==4, color(lavender%50)|| line rrr2 _at2, lpattern(dash) lcolor(black) sort legend(order(1 2 3 4) label(1 "White") label(2 "Black") label(3 "South Asian") label(4 "Other")) yscale(log) xlabel(16 23 30 37 44,valuelabel)  xtitle("Age group (years)")  ytitle("Relative risk") name("RR_htn_by_eth1") caption("B", pos(10) size(large))  legend(off) yscale(range(0.3 250)) ylabel(0.3 0.5 1 4 10 25 50 100 250)
rename _at2 age_bands
rename _at1 ethnicity
keep outcome age_bands ethnicity rrr rrr_lci rrr_uci
	decode ethnicity, generate(eth2)
	drop ethnicity
	rename eth2 ethnicity
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
order outcome age_bands ethnicity rrr rrr_lci rrr_uci
export excel using "...", sheet(SuppFigureS2B, replace) firstrow(variables)
restore

preserve
keep if outcome =="htn_by_imd1"
label values _at2 Ab
label values _at1 imd
graph twoway scatter rrr _at2 if _at1==1, mcolor(red)  || scatter rrr _at2 if _at1==2, mcolor(green) || scatter rrr _at2 if _at1==3, mcolor(dkorange) || scatter rrr _at2 if _at1==4, mcolor(lavender)|| scatter rrr _at2 if _at1==5, mcolor(brown*1.5)  || rcap rrr_lci rrr_uci _at2 if _at1==1, color(red%50) || rcap rrr_lci rrr_uci _at2 if _at1==2, color(green%50) || rcap rrr_lci rrr_uci _at2 if _at1==3, color(dkorange%50) || rcap rrr_lci rrr_uci _at2 if _at1==4, color(lavender%50) || rcap rrr_lci rrr_uci _at2 if _at1==5, color(brown*1.5%50) || line rrr2 _at2, lpattern(dash) lcolor(black) sort legend(order(1 2 3 4 5) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 2") label(3 "Quintile 3") label(4 "Quintile 4") label(5 "Quintile 5 (most deprived)")) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("Relative risk") yscale(log) name("RR_htn_by_imd1") caption("D", pos(10) size(large))  legend(off) yscale(range(0.25 110)) ylabel(0.25 0.5 1 4 10 25 50 100)
rename _at2 age_bands
rename _at1 imd
keep outcome age_bands imd rrr rrr_lci rrr_uci
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
order outcome age_bands imd rrr rrr_lci rrr_uci
export excel using "...", sheet(SuppFigureS2D, replace) firstrow(variables)
restore


**********************************************************************************************
////Figure 4: absolute hba1c by ethnicity/imd and age at diagnosis 
**********************************************************************************************

///HbA1c

preserve
keep if outcome =="hba1c_eth1"
gen hba1c_mmolmol = .
gen hba1c_mmolmol_lci = .
gen hba1c_mmolmol_uci = .
replace hba1c_mmolmol = _margin * 10.93 - 23.5
replace hba1c_mmolmol_lci = _ci_lb * 10.93 - 23.5
replace hba1c_mmolmol_uci = _ci_ub * 10.93 - 23.5
label values _m1 eths
label values _at2 Ab
graph twoway scatter _margin _at2 if _m1==1, mcolor(red)  || scatter _margin _at2 if _m1==2, mcolor(green) yaxis(1) || scatter _margin _at2 if _m1==3, mcolor(dkorange) || scatter _margin _at2 if _m1==4, mcolor(lavender) || scatter hba1c_mmolmol _at2 if _m1==1, mcolor(red%0) yaxis(2) || scatter hba1c_mmolmol _at2 if _m1==2, mcolor(green%0) yaxis(2) || scatter hba1c_mmolmol _at2 if _m1==3, mcolor(dkorange%0) yaxis(2) || scatter hba1c_mmolmol _at2 if _m1==4, mcolor(lavender%0) yaxis(2)  ||  rcap _ci_lb _ci_ub _at2 if _m1==1, color(red%50) || rcap _ci_lb _ci_ub _at2 if _m1==2, color(green%50) || rcap _ci_lb _ci_ub _at2 if _m1==3, color(dkorange%50) || rcap _ci_lb _ci_ub _at2 if _m1==4, color(lavender%50) sort legend(order(1 2 3 4) label(1 "White") label(2 "Black") label(3 "South Asian") label(4 "Other")) xlabel(16 23 30 37 44,valuelabel)   xtitle("Age group (years)")  ytitle("HbA1c (%)") ytitle("HbA1c (mmol/mol)", axis(2) orientation(rvertical)) name("HbA1c_by_eth1")  caption("B", pos(10) size(large)) legend(off) yscale(range(5.7 11) axis(1)) yscale(range(39 97) axis(2)) ylabel(6 7 8 9 10 11, axis(1)) ylabel(40 50 60 70 80 90,axis(2))
rename _at2 age_bands
rename _m1 ethnicity
	decode ethnicity, generate(eth2)
	drop ethnicity
	rename eth2 ethnicity
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
keep outcome age_bands ethnicity _margin _ci_lb _ci_ub hba1c_mmolmol hba1c_mmolmol_lci hba1c_mmolmol_uci
order outcome age_bands ethnicity _margin _ci_lb _ci_ub hba1c_mmolmol hba1c_mmolmol_lci hba1c_mmolmol_uci
export excel using "...", sheet(Figure3B, replace) firstrow(variables)
restore


preserve
keep if outcome =="hba1c_imd1"
gen hba1c_mmolmol = .
gen hba1c_mmolmol_lci = .
gen hba1c_mmolmol_uci = .
replace hba1c_mmolmol = _margin * 10.93 - 23.5
replace hba1c_mmolmol_lci = _ci_lb * 10.93 - 23.5
replace hba1c_mmolmol_uci = _ci_ub * 10.93 - 23.5
label values _m1 imd
label values _at2 Ab
graph twoway scatter _margin _at2 if _m1==1, mcolor(red)  || scatter _margin _at2 if _m1==2, mcolor(green) || scatter _margin _at2 if _m1==3, mcolor(dkorange) || scatter _margin _at2 if _m1==4, mcolor(lavender)|| scatter _margin _at2 if _m1==5, mcolor(brown*1.5)  || rcap _ci_lb _ci_ub _at2 if _m1==1, color(red%50) || rcap _ci_lb _ci_ub _at2 if _m1==2, color(green%50) || rcap _ci_lb _ci_ub _at2 if _m1==3, color(dkorange%50) || rcap _ci_lb _ci_ub _at2 if _m1==4, color(lavender%50) || rcap _ci_lb _ci_ub _at2 if _m1==5, color(brown*1.5%50)  || scatter hba1c_mmolmol _at2 if _m1==1, mcolor(red%0) yaxis(2)  || scatter hba1c_mmolmol _at2 if _m1==2, mcolor(green%0) yaxis(2) || scatter hba1c_mmolmol _at2 if _m1==3, mcolor(dkorange%0) yaxis(2) || scatter hba1c_mmolmol _at2 if _m1==4, mcolor(lavender%0) yaxis(2) || scatter hba1c_mmolmol _at2 if _m1==5, mcolor(brown*1.5%0)  yaxis(2) sort legend(order(1 2 3 4 5) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 2") label(3 "Quintile 3") label(4 "Quintile 4") label(5 "Quintile 5 (most deprived)")) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("HbA1c (%)") ytitle("HbA1c (mmol/mol)", axis(2) orientation(rvertical)) name("HbA1c_by_imd1") caption("D", pos(10) size(large)) legend(off) yscale(range(5 11.5) axis(1)) yscale(range(31.1 102.2) axis(2)) ylabel(5 6 7 8 9 10 11, axis(1)) ylabel(30 40 50 60 70 80 90 100, axis(2))
rename _at2 age_bands
rename _m1 imd
keep outcome age_bands imd _margin _ci_lb _ci_ub hba1c_mmolmol hba1c_mmolmol_lci hba1c_mmolmol_uci
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
order outcome age_bands imd _margin _ci_lb _ci_ub hba1c_mmolmol hba1c_mmolmol_lci hba1c_mmolmol_uci
export excel using "...", sheet(Figure3D, replace) firstrow(variables)
restore


**********************************************************************************************
///Supp Fig 1: RRR obesity by eth/imd
**********************************************************************************************
preserve
keep if outcome =="obesity_by_eth1"
label values _at2 Ab
label values _at1 eths
graph twoway scatter rrr _at2 if _at1==1, mcolor(red)  || scatter rrr _at2 if _at1==2, mcolor(green) || scatter rrr _at2 if _at1==3, mcolor(dkorange) || scatter rrr _at2 if _at1==4, mcolor(lavender) || rcap rrr_lci rrr_uci _at2 if _at1==1, color(red%50) || rcap rrr_lci rrr_uci _at2 if _at1==2, color(green%50) || rcap rrr_lci rrr_uci _at2 if _at1==3, color(dkorange%50) || rcap rrr_lci rrr_uci _at2 if _at1==4, color(lavender%50)|| line rrr2 _at2, lpattern(dash) lcolor(black) sort legend(order(1 2 3 4) label(1 "White") label(2 "Black") label(3 "South Asian") label(4 "Other")) yscale(log) yscale(range(1 20)) ylabel(1 2 3 5 10 15 20)  xlabel(16 23 30 37 44,valuelabel)  xtitle("Age group (years)")  ytitle("Relative risk") name("RR_obesity_by_eth1") caption("B", pos(10) size(large))  legend(off)
rename _at2 age_bands
rename _at1 ethnicity
keep outcome age_bands ethnicity rrr rrr_lci rrr_uci
	decode ethnicity, generate(eth2)
	drop ethnicity
	rename eth2 ethnicity
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
order outcome age_bands ethnicity rrr rrr_lci rrr_uci
export excel using "...", sheet(SuppFigureS1B, replace) firstrow(variables)
restore

preserve
keep if outcome =="obesity_by_imd1"
label values _at2 Ab
label values _at1 imd
graph twoway scatter rrr _at2 if _at1==1, mcolor(red)  || scatter rrr _at2 if _at1==2, mcolor(green) || scatter rrr _at2 if _at1==3, mcolor(dkorange) || scatter rrr _at2 if _at1==4, mcolor(lavender)|| scatter rrr _at2 if _at1==5, mcolor(brown*1.5)  || rcap rrr_lci rrr_uci _at2 if _at1==1, color(red%50) || rcap rrr_lci rrr_uci _at2 if _at1==2, color(green%50) || rcap rrr_lci rrr_uci _at2 if _at1==3, color(dkorange%50) || rcap rrr_lci rrr_uci _at2 if _at1==4, color(lavender%50) || rcap rrr_lci rrr_uci _at2 if _at1==5, color(brown*1.5%50) || line rrr2 _at2, lpattern(dash) lcolor(black) sort legend(order(1 2 3 4 5) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 2") label(3 "Quintile 3") label(4 "Quintile 4") label(5 "Quintile 5 (most deprived)")) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("Relative risk") yscale(log) yscale(range(1 25)) ylabel(1 2 3 5 10 15 20 25) name("RR_obesity_by_imd1") caption("D", pos(10) size(large))  legend(off)
rename _at2 age_bands
rename _at1 imd
keep outcome age_bands imd rrr rrr_lci rrr_uci
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
order outcome age_bands imd rrr rrr_lci rrr_uci
export excel using "...", sheet(SuppFigureS1D, replace) firstrow(variables)
restore

	
	
	
	
	
	**********************************************************************************************
	///WOMEN
*
**********************************************************************************************
	////Figure 1: absolute bmi and change bmi across diabetes status, ethnicity or imd and age at diagnosis 
	**********************************************************************************************

	 
	///bmi absolute by diabetes status, ethnicity/imd and age 
	preserve
	keep if outcome =="bmi_eth_predict2"
	label values _m1 diab
	label values _at2 eths
	label values _at3 Ab
	graph twoway scatter _margin _at3 if _at2==1 & _m1==1, mcolor(red)  || scatter _margin _at3 if _at2==2 & _m1==1, mcolor(green) || scatter _margin _at3 if _at2==3 & _m1==1, mcolor(dkorange) || scatter _margin _at3 if _at2==4 & _m1==1, mcolor(lavender) || rcap _ci_lb _ci_ub _at3 if _at2==1 & _m1==1, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2 &_m1==1, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3 & _m1==1, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4 & _m1==1, color(lavender%50)|| scatter _margin _at3 if _at2==1 & _m1==0, mcolor(red) msymbol(triangle) lpattern(dash dot dot)  || scatter _margin _at3 if _at2==2 & _m1==0, mcolor(green) msymbol(triangle) lpattern(dash dot dot) || scatter _margin _at3 if _at2==3 & _m1==0, mcolor(dkorange) msymbol(triangle) lpattern(dash dot dot)|| scatter _margin _at3 if _at2==4 & _m1==0, mcolor(lavender) msymbol(triangle) lpattern(dash dot dot) || rcap _ci_lb _ci_ub _at3 if _at2==1 & _m1==0, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2 & _m1==0, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3 & _m1==0, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4 &_m1==0, color(lavender%50) sort legend(order(2 4 3 1 10 12 11 9) label(1 "White + Diabetes") label(2 "Black + Diabetes") label(3 "South Asian+ Diabetes") label(4 "Other + Diabetes") label(9 "White + No diabetes") label(10 "Black + No diabetes") label(11 "South Asian+ No diabetes") label(12 "Other + No diabetes")) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("BMI (kg/m{superscript:2})") name("BMI_by_eth2") caption("A",pos(10) size(large)) legend(off)
	rename _m1 diabetes_status
	rename _at2 ethnicity
	rename _at3 age_bands
		decode ethnicity, generate(eth2)
	drop ethnicity
	rename eth2 ethnicity
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
	decode diabetes_status, generate(new)
	drop diabetes_status
	rename new diabetes_status
	keep outcome diabetes_status age_bands ethnicity _margin _ci_lb _ci_ub
	order outcome diabetes_status age_bands ethnicity _margin _ci_lb _ci_ub
	export excel using "...", sheet(Figure1A, replace) firstrow(variables) 
	restore


preserve
keep if outcome =="bmi_imd_predict2"
label values _m1 diab
label values _at2 imd
label values _at3 Ab
graph twoway scatter _margin _at3 if _at2==1 & _m1==1, mcolor(red)  || scatter _margin _at3 if _at2==2 & _m1==1, mcolor(green) || scatter _margin _at3 if _at2==3 & _m1==1, mcolor(dkorange) || scatter _margin _at3 if _at2==4 & _m1==1, mcolor(lavender) || scatter _margin _at3 if _at2==5 & _m1==1, mcolor(brown*1.5) || rcap _ci_lb _ci_ub _at3 if _at2==1 & _m1==1, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2 &_m1==1, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3 & _m1==1, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4 & _m1==1, color(lavender%50)|| rcap _ci_lb _ci_ub _at3 if _at2==5 & _m1==1, color(brown*1.5%50)|| scatter _margin _at3 if _at2==1 & _m1==0, mcolor(red) msymbol(triangle) lpattern(dash dot dot)  || scatter _margin _at3 if _at2==2 & _m1==0, mcolor(green) msymbol(triangle) lpattern(dash dot dot) || scatter _margin _at3 if _at2==3 & _m1==0, mcolor(dkorange) msymbol(triangle) lpattern(dash dot dot)|| scatter _margin _at3 if _at2==4 & _m1==0, mcolor(lavender) msymbol(triangle) lpattern(dash dot dot) || scatter _margin _at3 if _at2==5 & _m1==0, mcolor(brown*1.5) || rcap _ci_lb _ci_ub _at3 if _at2==1 & _m1==0, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2 & _m1==0, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3 & _m1==0, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4 &_m1==0, color(lavender%50) || rcap _ci_lb _ci_ub _at3 if _at2==5 & _m1==0, color(brown*1.5%50) sort legend(order(1 2 3 4 5 11 12 13 14 15) label(1 "Quintile 1 + Diabetes") label(2 "Quintile 2 + Diabetes") label(3 "Quintile 3 + Diabetes") label(4 "Quintile 4 + Diabetes") label(5 "Quintile 5 + Diabetes")  label(11 "Quintile 1 + No diabetes") label(12 "Quintile 2 + No diabetes") label(13 "Quintile 3 + No diabetes") label(14 "Quintile 4 + No diabetes") label(15 "Quintile 5 + No diabetes")) xlabel(16 23 30 37 44,valuelabel) yscale(range(20 41.5)) xtitle("Age group (years)")  ytitle("BMI (kg/m{superscript:2})") name("BMI_by_imd2") caption("E",pos(10) size(large)) legend(off)
rename _m1 diabetes_status
rename _at2 imd
rename _at3 age_bands
keep outcome diabetes_status age_bands imd _margin _ci_lb _ci_ub
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
	decode diabetes_status, generate(new)
	drop diabetes_status
	rename new diabetes_status
order outcome diabetes_status age_bands imd _margin _ci_lb _ci_ub
export excel using "...", sheet(Figure1E, replace) firstrow(variables) 
restore

///bmi diff eth
preserve
keep if outcome =="bmi_eth_constrast2"
label values _at2 eths
label values _at3 Ab
graph twoway scatter _margin _at3 if _at2==1, mcolor(red)  || scatter _margin _at3 if _at2==2, mcolor(green) || scatter _margin _at3 if _at2==3, mcolor(dkorange) || scatter _margin _at3 if _at2==4, mcolor(lavender) || rcap _ci_lb _ci_ub _at3 if _at2==1, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4, color(lavender%50) sort legend(order(2 4 3 1) label(1 "White") label(2 "Black") label(3 "South Asian") label(4 "Other")) xlabel(16 23 30 37 44,valuelabel) ylabel(1 5 10 15)  xtitle("Age group (years)")  ytitle("Difference in BMI (kg/m{superscript:2})" "(diabetes vs. no diabetes)") name("Change_BMI_by_eth2") caption("C", pos(10) size(large)) legend(off) yscale(range(2 17.5)) ylabel(5 10 15)
rename _at2 ethnicity
rename _at3 age_bands
keep outcome age_bands ethnicity _margin _ci_lb _ci_ub
	decode ethnicity, generate(eth2)
	drop ethnicity
	rename eth2 ethnicity
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
order outcome age_bands ethnicity _margin _ci_lb _ci_ub
export excel using "...", sheet(Figure1C, replace) firstrow(variables) 
restore

preserve
keep if outcome =="bmi_imd_constrast2"
label values _at3 Ab
graph twoway scatter _margin _at3 if _at2==1, mcolor(red)  || scatter _margin _at3 if _at2==2, mcolor(green) || scatter _margin _at3 if _at2==3, mcolor(dkorange) || scatter _margin _at3 if _at2==4, mcolor(lavender)|| scatter _margin _at3 if _at2==5, mcolor(brown*1.5)  || rcap _ci_lb _ci_ub _at3 if _at2==1, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4, color(lavender%50) || rcap _ci_lb _ci_ub _at3 if _at2==5, color(brown*1.5%50) sort legend(order(1 2 3 4 5) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 2") label(3 "Quintile 3") label(4 "Quintile 4") label(5 "Quintile 5 (most deprived)")) xlabel(16 23 30 37 44,valuelabel) ylabel(1 5 10 15)  xtitle("Age group (years)")  ytitle("Difference in BMI (kg/m{superscript:2})" "(diabetes vs. no diabetes)") name("Change_BMI_by_imd2") caption("G", pos(10) size(large)) legend(off) yscale(range(5 20)) ylabel(5 10 15 20)
rename _at2 imd
rename _at3 age_bands
keep outcome age_bands imd _margin _ci_lb _ci_ub
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
order outcome age_bands imd _margin _ci_lb _ci_ub
export excel using "...", sheet(Figure1G, replace) firstrow(variables) 
restore



**********************************************************************************************
////Figure 2: absolute ldl and change ldl across diabetes status, ethnicity and age at diagnosis 
**********************************************************************************************

///ldl absolute by diabetes status, ethnicity and age 
 preserve
keep if outcome =="ldl_eths_predict2"
label values _m1 diab
label values _at2 eths
label values _at3 Ab
graph twoway scatter _margin _at3 if _at2==1 & _m1==1, mcolor(red)  || scatter _margin _at3 if _at2==2 & _m1==1, mcolor(green) || scatter _margin _at3 if _at2==3 & _m1==1, mcolor(dkorange) || scatter _margin _at3 if _at2==4 & _m1==1, mcolor(lavender) || rcap _ci_lb _ci_ub _at3 if _at2==1 & _m1==1, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2 & _m1==1, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3 & _m1==1, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4 & _m1==1, color(lavender%50)|| scatter _margin _at3 if _at2==1 & _m1==0, mcolor(red) msymbol(triangle) lpattern(dash dot dot)  || scatter _margin _at3 if _at2==2 & _m1==0, mcolor(green)msymbol(triangle) lpattern(dash dot dot) || scatter _margin _at3 if _at2==3 & _m1==0, mcolor(dkorange)msymbol(triangle) lpattern(dash dot dot)|| scatter _margin _at3 if _at2==4 & _m1==0, mcolor(lavender) msymbol(triangle) lpattern(dash dot dot) || rcap _ci_lb _ci_ub _at3 if _at2==1 & _m1==0, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2 & _m1==0, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3 & _m1==0, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4 & _m1==0, color(lavender%50) sort legend(order(1 2 3 4 9 10 11 12) label(1 "White + Diabetes") label(2 "Black + Diabetes") label(3 "South Asian+ Diabetes") label(4 "Other + Diabetes") label(9 "White + No diabetes") label(10 "Black + No diabetes") label(11 "South Asian+ No diabetes") label(12 "Other + No diabetes")) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("LDL cholesterol (mmol/L)") name("LDL_by_eth2") caption("A", pos(10) size(large)) yscale(range(1.8 4)) legend(off)
rename _at2 ethnicity
rename _m1 diabetes_status
rename _at3 age_bands
	decode ethnicity, generate(eth2)
	drop ethnicity
	rename eth2 ethnicity
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
	decode diabetes_status, generate(new)
	drop diabetes_status
	rename new diabetes_status
keep outcome diabetes_status age_bands ethnicity _margin _ci_lb _ci_ub

order outcome diabetes_status age_bands ethnicity _margin _ci_lb _ci_ub
export excel using "...", sheet(Figure2A, replace) firstrow(variables)
restore

preserve
keep if outcome =="ldl_imd_predict2"
label values _m1 diab
label values _at2 imd
label values _at3 Ab
graph twoway scatter _margin _at3 if _at2==1 & _m1==1, mcolor(red)  || scatter _margin _at3 if _at2==2 & _m1==1, mcolor(green) || scatter _margin _at3 if _at2==3 & _m1==1, mcolor(dkorange) || scatter _margin _at3 if _at2==4 & _m1==1, mcolor(lavender) || scatter _margin _at3 if _at2==5 & _m1==1, mcolor(brown*1.5) || rcap _ci_lb _ci_ub _at3 if _at2==1 & _m1==1, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2 &_m1==1, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3 & _m1==1, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4 & _m1==1, color(lavender%50)|| rcap _ci_lb _ci_ub _at3 if _at2==5 & _m1==1, color(brown*1.5%50)|| scatter _margin _at3 if _at2==1 & _m1==0, mcolor(red) msymbol(triangle) lpattern(dash dot dot)  || scatter _margin _at3 if _at2==2 & _m1==0, mcolor(green) msymbol(triangle) lpattern(dash dot dot) || scatter _margin _at3 if _at2==3 & _m1==0, mcolor(dkorange) msymbol(triangle) lpattern(dash dot dot)|| scatter _margin _at3 if _at2==4 & _m1==0, mcolor(lavender) msymbol(triangle) lpattern(dash dot dot) || scatter _margin _at3 if _at2==5 & _m1==0, mcolor(brown*1.5) || rcap _ci_lb _ci_ub _at3 if _at2==1 & _m1==0, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2 & _m1==0, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3 & _m1==0, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4 &_m1==0, color(lavender%50) || rcap _ci_lb _ci_ub _at3 if _at2==5 & _m1==0, color(brown*1.5%50) sort legend(order(1 2 3 4 5 11 12 13 14 15) label(1 "Quintile 1 + Diabetes") label(2 "Quintile 2 + Diabetes") label(3 "Quintile 3 + Diabetes") label(4 "Quintile 4 + Diabetes") label(5 "Quintile 5 + Diabetes")  label(11 "Quintile 1 + No diabetes") label(12 "Quintile 2 + No diabetes") label(13 "Quintile 3 + No diabetes") label(14 "Quintile 4 + No diabetes") label(15 "Quintile 5 + No diabetes")) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("LDL cholesterol (mmol/L)") name("LDL_by_imd2") caption("E", pos(10) size(large)) legend(off) yscale(range(1 4)) ylabel(1 2 3 4) 
rename _m1 diabetes_status
rename _at2 imd
rename _at3 age_bands
keep outcome diabetes_status age_bands imd _margin _ci_lb _ci_ub
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
	decode diabetes_status, generate(new)
	drop diabetes_status
	rename new diabetes_status
order outcome diabetes_status age_bands imd _margin _ci_lb _ci_ub
export excel using "...", sheet(Figure2E, replace) firstrow(variables) 
restore

///ldl diff 
preserve
keep if outcome =="ldl_eths_constrast2"
label values _at2 eths
label values _at3 Ab
gen null=0
graph twoway scatter _margin _at3 if _at2==1, mcolor(red)  || scatter _margin _at3 if _at2==2, mcolor(green) || scatter _margin _at3 if _at2==3, mcolor(dkorange) || scatter _margin _at3 if _at2==4, mcolor(lavender) || rcap _ci_lb _ci_ub _at3 if _at2==1, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4, color(lavender%50) || line null _at3, lpattern(dash) lcolor(black%50) sort legend(order(1 2 3 4) label(1 "White") label(2 "Black") label(3 "South Asian") label(4 "Other")) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("Difference in LDL cholesterol (mmol/l)" "(diabetes vs. no diabetes)") name("Change_LDL_by_eth2") caption("C", pos(10) size(large)) legend(off) yscale(range(-1 1.5)) ylabel(-1 -0.5 0 0.5 1 1.5)
rename _at2 ethnicity
rename _at3 age_bands
keep outcome age_bands ethnicity _margin _ci_lb _ci_ub
	decode ethnicity, generate(eth2)
	drop ethnicity
	rename eth2 ethnicity
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
	keep outcome age_bands ethnicity _margin _ci_lb _ci_ub
order outcome age_bands ethnicity _margin _ci_lb _ci_ub
export excel using "...", sheet(Figure2C, replace) firstrow(variables)
restore

preserve
keep if outcome =="ldl_imd_constrast2"
label values _at2 eths
label values _at3 Ab
gen null=0
graph twoway scatter _margin _at3 if _at2==1, mcolor(red)  || scatter _margin _at3 if _at2==2, mcolor(green) || scatter _margin _at3 if _at2==3, mcolor(dkorange) || scatter _margin _at3 if _at2==4, mcolor(lavender)|| scatter _margin _at3 if _at2==5, mcolor(brown*1.5)  || rcap _ci_lb _ci_ub _at3 if _at2==1, color(red%50) || rcap _ci_lb _ci_ub _at3 if _at2==2, color(green%50) || rcap _ci_lb _ci_ub _at3 if _at2==3, color(dkorange%50) || rcap _ci_lb _ci_ub _at3 if _at2==4, color(lavender%50) || rcap _ci_lb _ci_ub _at3 if _at2==5, color(brown*1.5%50) || line null _at3, lpattern(dash) lcolor(black%50) sort legend(order(1 2 3 4 5) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 2") label(3 "Quintile 3") label(4 "Quintile 4") label(5 "Quintile 5 (most deprived)")) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("Difference in LDL cholesterol (mmol/l)" "(diabetes vs. no diabetes)") name("Change_LDL_by_imd2") caption("G", pos(10) size(large)) legend(off) yscale(range(-1.1 2.1)) ylabel(-1 -0.5 0 0.5 1 1.5 2)
rename _at2 imd
rename _at3 age_bands
keep outcome age_bands imd _margin _ci_lb _ci_ub
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
order outcome age_bands imd _margin _ci_lb _ci_ub
export excel using "...", sheet(Figure2G, replace) firstrow(variables) 
restore



**********************************************************************************************
///Fig 3: RRR HTN by eth/imd
**********************************************************************************************
preserve
keep if outcome =="htn_by_eth2"
label values _at2 Ab
label values _at1 eths
graph twoway scatter rrr _at2 if _at1==1, mcolor(red)  || scatter rrr _at2 if _at1==2, mcolor(green) || scatter rrr _at2 if _at1==3, mcolor(dkorange) || scatter rrr _at2 if _at1==4, mcolor(lavender) || rcap rrr_lci rrr_uci _at2 if _at1==1, color(red%50) || rcap rrr_lci rrr_uci _at2 if _at1==2, color(green%50) || rcap rrr_lci rrr_uci _at2 if _at1==3, color(dkorange%50) || rcap rrr_lci rrr_uci _at2 if _at1==4, color(lavender%50)|| line rrr2 _at2, lpattern(dash) lcolor(black) sort legend(order(1 2 3 4) label(1 "White") label(2 "Black") label(3 "South Asian") label(4 "Other")) yscale(log) xlabel(16 23 30 37 44,valuelabel)  xtitle("Age group (years)")  ytitle("Relative risk") name("RR_htn_by_eth2") caption("A", pos(10) size(large))  legend(off) yscale(range(0.3 250)) ylabel(0.3 0.5 1 4 10 25 50 100 250)
rename _at2 age_bands
rename _at1 ethnicity
keep outcome age_bands ethnicity rrr rrr_lci rrr_uci
	decode ethnicity, generate(eth2)
	drop ethnicity
	rename eth2 ethnicity
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
order outcome age_bands ethnicity rrr rrr_lci rrr_uci
export excel using "...", sheet(SuppFigureS2A, replace) firstrow(variables)
restore

preserve
keep if outcome =="htn_by_imd2"
label values _at2 Ab
label values _at1 imd
graph twoway scatter rrr _at2 if _at1==1, mcolor(red)  || scatter rrr _at2 if _at1==2, mcolor(green) || scatter rrr _at2 if _at1==3, mcolor(dkorange) || scatter rrr _at2 if _at1==4, mcolor(lavender)|| scatter rrr _at2 if _at1==5, mcolor(brown*1.5)  || rcap rrr_lci rrr_uci _at2 if _at1==1, color(red%50) || rcap rrr_lci rrr_uci _at2 if _at1==2, color(green%50) || rcap rrr_lci rrr_uci _at2 if _at1==3, color(dkorange%50) || rcap rrr_lci rrr_uci _at2 if _at1==4, color(lavender%50) || rcap rrr_lci rrr_uci _at2 if _at1==5, color(brown*1.5%50) || line rrr2 _at2, lpattern(dash) lcolor(black) sort legend(order(1 2 3 4 5) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 2") label(3 "Quintile 3") label(4 "Quintile 4") label(5 "Quintile 5 (most deprived)")) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("Relative risk") yscale(log) yscale(range(1 25)) ylabel(1 2 3 5 10 15 20 25) ylabel(1 2 4 6 8 10) name("RR_htn_by_imd2") caption("C", pos(10) size(large))  legend(off) yscale(range(0.25 110)) ylabel(0.25 0.5 1 4 10 25 50 100)
rename _at2 age_bands
rename _at1 imd
keep outcome age_bands imd rrr rrr_lci rrr_uci
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
order outcome age_bands imd rrr rrr_lci rrr_uci
export excel using "...", sheet(SuppFigureS2C, replace) firstrow(variables)
restore


**********************************************************************************************
////Figure 4: absolute hba1c by ethnicity and age at diagnosis 
**********************************************************************************************

///HbA1c

preserve
keep if outcome =="hba1c_eth2"
gen hba1c_mmolmol = .
gen hba1c_mmolmol_lci = .
gen hba1c_mmolmol_uci = .
replace hba1c_mmolmol = _margin * 10.93 - 23.5
replace hba1c_mmolmol_lci = _ci_lb * 10.93 - 23.5
replace hba1c_mmolmol_uci = _ci_ub * 10.93 - 23.5
label values _m1 eths
label values _at2 Ab
graph twoway scatter _margin _at2 if _m1==1, mcolor(red)  || scatter _margin _at2 if _m1==2, mcolor(green) yaxis(1) || scatter _margin _at2 if _m1==3, mcolor(dkorange) || scatter _margin _at2 if _m1==4, mcolor(lavender) || scatter hba1c_mmolmol _at2 if _m1==1, mcolor(red%0) yaxis(2) || scatter hba1c_mmolmol _at2 if _m1==2, mcolor(green%0) yaxis(2) || scatter hba1c_mmolmol _at2 if _m1==3, mcolor(dkorange%0) yaxis(2) || scatter hba1c_mmolmol _at2 if _m1==4, mcolor(lavender%0) yaxis(2)  ||  rcap _ci_lb _ci_ub _at2 if _m1==1, color(red%50) || rcap _ci_lb _ci_ub _at2 if _m1==2, color(green%50) || rcap _ci_lb _ci_ub _at2 if _m1==3, color(dkorange%50) || rcap _ci_lb _ci_ub _at2 if _m1==4, color(lavender%50) sort legend(order(1 2 3 4) label(1 "White") label(2 "Black") label(3 "South Asian") label(4 "Other")) xlabel(16 23 30 37 44,valuelabel)   xtitle("Age group (years)")  ytitle("HbA1c (%)") ytitle("HbA1c (mmol/mol)", axis(2) orientation(rvertical)) name("HbA1c_by_eth2")  caption("A", pos(10) size(large)) legend(off) yscale(range(5.7 11) axis(1)) yscale(range(39 97) axis(2)) ylabel(6 7 8 9 10 11, axis(1)) ylabel(40 50 60 70 80 90,axis(2))
rename _at2 age_bands
rename _m1 ethnicity
keep outcome age_bands ethnicity _margin _ci_lb _ci_ub hba1c_mmolmol hba1c_mmolmol_lci hba1c_mmolmol_uci
	decode ethnicity, generate(eth2)
	drop ethnicity
	rename eth2 ethnicity
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
order outcome age_bands ethnicity _margin _ci_lb _ci_ub hba1c_mmolmol hba1c_mmolmol_lci hba1c_mmolmol_uci
export excel using "...", sheet(Figure3A, replace) firstrow(variables)
restore

preserve
keep if outcome =="hba1c_imd2"
gen hba1c_mmolmol = .
gen hba1c_mmolmol_lci = .
gen hba1c_mmolmol_uci = .
replace hba1c_mmolmol = _margin * 10.93 - 23.5
replace hba1c_mmolmol_lci = _ci_lb * 10.93 - 23.5
replace hba1c_mmolmol_uci = _ci_ub * 10.93 - 23.5
label values _m1 imd
label values _at2 Ab
graph twoway scatter _margin _at2 if _m1==1, mcolor(red)  || scatter _margin _at2 if _m1==2, mcolor(green) || scatter _margin _at2 if _m1==3, mcolor(dkorange) || scatter _margin _at2 if _m1==4, mcolor(lavender)|| scatter _margin _at2 if _m1==5, mcolor(brown*1.5)  || rcap _ci_lb _ci_ub _at2 if _m1==1, color(red%50) || rcap _ci_lb _ci_ub _at2 if _m1==2, color(green%50) || rcap _ci_lb _ci_ub _at2 if _m1==3, color(dkorange%50) || rcap _ci_lb _ci_ub _at2 if _m1==4, color(lavender%50) || rcap _ci_lb _ci_ub _at2 if _m1==5, color(brown*1.5%50)  || scatter hba1c_mmolmol _at2 if _m1==1, mcolor(red%0) yaxis(2)  || scatter hba1c_mmolmol _at2 if _m1==2, mcolor(green%0) yaxis(2) || scatter hba1c_mmolmol _at2 if _m1==3, mcolor(dkorange%0) yaxis(2) || scatter hba1c_mmolmol _at2 if _m1==4, mcolor(lavender%0) yaxis(2) || scatter hba1c_mmolmol _at2 if _m1==5, mcolor(brown*1.5%0)  yaxis(2) sort legend(order(1 2 3 4 5) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 2") label(3 "Quintile 3") label(4 "Quintile 4") label(5 "Quintile 5 (most deprived)")) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("HbA1c (%)") ytitle("HbA1c (mmol/mol)", axis(2) orientation(rvertical)) name("HbA1c_by_imd2") caption("C", pos(10) size(large)) legend(off) yscale(range(5 11.5) axis(1)) yscale(range(31.1 102.2) axis(2)) ylabel(5 6 7 8 9 10 11, axis(1)) ylabel(30 40 50 60 70 80 90 100, axis(2))
rename _at2 age_bands
rename _m1 imd
keep outcome age_bands imd _margin _ci_lb _ci_ub hba1c_mmolmol hba1c_mmolmol_lci hba1c_mmolmol_uci
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
order outcome age_bands imd _margin _ci_lb _ci_ub hba1c_mmolmol hba1c_mmolmol_lci hba1c_mmolmol_uci
export excel using "...", sheet(Figure3C, replace) firstrow(variables)
restore

	
**********************************************************************************************
///Supp Fig 1: RRR obesity by eth/imd
**********************************************************************************************
preserve
keep if outcome =="obesity_by_eth2"
label values _at2 Ab
label values _at1 eths
graph twoway scatter rrr _at2 if _at1==1, mcolor(red)  || scatter rrr _at2 if _at1==2, mcolor(green) || scatter rrr _at2 if _at1==3, mcolor(dkorange) || scatter rrr _at2 if _at1==4, mcolor(lavender) || rcap rrr_lci rrr_uci _at2 if _at1==1, color(red%50) || rcap rrr_lci rrr_uci _at2 if _at1==2, color(green%50) || rcap rrr_lci rrr_uci _at2 if _at1==3, color(dkorange%50) || rcap rrr_lci rrr_uci _at2 if _at1==4, color(lavender%50)|| line rrr2 _at2, lpattern(dash) lcolor(black) sort legend(order(1 2 3 4) label(1 "White") label(2 "Black") label(3 "South Asian") label(4 "Other")) yscale(log) yscale(range(1 20)) ylabel(1 2 3 5 10 15 20)  xlabel(16 23 30 37 44,valuelabel)  xtitle("Age group (years)")  ytitle("Relative risk") name("RR_obesity_by_eth2") caption("A", pos(10) size(large))  legend(off)
rename _at2 age_bands
rename _at1 ethnicity
keep outcome age_bands ethnicity rrr rrr_lci rrr_uci
	decode ethnicity, generate(eth2)
	drop ethnicity
	rename eth2 ethnicity
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
order outcome age_bands ethnicity rrr rrr_lci rrr_uci
export excel using "...", sheet(SuppFigureS1A, replace) firstrow(variables)
restore

preserve
keep if outcome =="obesity_by_imd2"
label values _at2 Ab
label values _at1 imd
graph twoway scatter rrr _at2 if _at1==1, mcolor(red)  || scatter rrr _at2 if _at1==2, mcolor(green) || scatter rrr _at2 if _at1==3, mcolor(dkorange) || scatter rrr _at2 if _at1==4, mcolor(lavender)|| scatter rrr _at2 if _at1==5, mcolor(brown*1.5)  || rcap rrr_lci rrr_uci _at2 if _at1==1, color(red%50) || rcap rrr_lci rrr_uci _at2 if _at1==2, color(green%50) || rcap rrr_lci rrr_uci _at2 if _at1==3, color(dkorange%50) || rcap rrr_lci rrr_uci _at2 if _at1==4, color(lavender%50) || rcap rrr_lci rrr_uci _at2 if _at1==5, color(brown*1.5%50) || line rrr2 _at2, lpattern(dash) lcolor(black) sort legend(order(1 2 3 4 5) label(1 "Quintile 1 (least deprived)") label(2 "Quintile 2") label(3 "Quintile 3") label(4 "Quintile 4") label(5 "Quintile 5 (most deprived)")) xlabel(16 23 30 37 44,valuelabel) xtitle("Age group (years)")  ytitle("Relative risk") yscale(log) yscale(range(1 25)) ylabel(1 2 3 5 10 15 20 25) name("RR_obesity_by_imd2") caption("C", pos(10) size(large))  legend(off)
rename _at2 age_bands
rename _at1 imd
keep outcome age_bands imd rrr rrr_lci rrr_uci
	decode age_bands, generate(new)
	drop age_bands
	rename new age_bands
order outcome age_bands imd rrr rrr_lci rrr_uci
export excel using "...", sheet(SuppFigureS1C, replace) firstrow(variables)
restore
	
	
**********************************************************************************************
///Figs combine
**********************************************************************************************

graph combine BMI_by_eth2 BMI_by_eth1 Change_BMI_by_eth2 Change_BMI_by_eth1  BMI_by_imd2 BMI_by_imd1 Change_BMI_by_imd2 Change_BMI_by_imd1, row(4) col(2) ysize(12) xsize(11) scale(0.35) name(BMI_combine)
graph export "...", as(svg) name("BMI_combine") replace

graph combine LDL_by_eth2 LDL_by_eth1 Change_LDL_by_eth2 Change_LDL_by_eth1 LDL_by_imd2 LDL_by_imd1 Change_LDL_by_imd2 Change_LDL_by_imd1, row(4) col(2) ysize(12) xsize(11) name(LDL_combine) scale(0.35)
graph export "...", as(svg) name("LDL_combine") replace

graph combine RR_htn_by_eth2 RR_htn_by_eth1 RR_htn_by_imd2 RR_htn_by_imd1, row(2) col(2) name(htn_combine) ysize(9) xsize(11) scale(0.5) 
graph export "...", as(svg) name("htn_combine") replace

graph combine HbA1c_by_eth2 HbA1c_by_eth1 HbA1c_by_imd2 HbA1c_by_imd1, row(2) col(2) ysize(12) xsize(14) name(HbA1c_combine) scale(0.5)
graph export "...", as(svg) name("HbA1c_combine") replace

graph combine RR_obesity_by_eth2 RR_obesity_by_eth1 RR_obesity_by_imd2 RR_obesity_by_imd1, row(2) col(2) name(obesity_combine) ysize(9) xsize(11) scale(0.5) 
graph export "...", as(svg) name("obesity_combine") replace


