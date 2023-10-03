

cls
clear all
set more off


*#################################################################################################################################*
																* WALKING PACE *
*#################################################################################################################################*
cap log close
log using "...\logfile_wp", replace


**************************************
**Preparation*************************
**************************************
use "...\dataset_HD", replace
mdesc
distinct id
drop if bcancer == 1
drop if bcvd == 1
drop if cause_death == "Unknown"
distinct id
tab nmiss
drop fitness maxwl maxhr targethr ntrend durfit bcancer bcvd bckd bdiab n_22190_0_0 n_22191_0_0 n_22192_0_0 n_30000_0_0 cacvd t_cacvd nmiss probacc overacc wearacc
gen tdied = (dodn-date0n)/365.24
foreach cde in CVD Cancer Other {
	gen `cde' 	  = 1 if cause_death == "`cde'"
	replace `cde' = 0 if `cde' == .
}
describe
label variable tdied "Time_to_death"
mdesc
save "...\db_cmpr", replace


**************************************
**Descriptive*************************
**************************************
use "...\db_cmpr", clear

/*baseline tables by sex*/
baselinetable   											/*
*/	age0(cts tab("p50 (p25, p75)")) 						/*
*/  msbp(cts tab("p50 (p25, p75)") medianformat(%5.0f))		/*
*/  ldl(cts  tab("p50 (p25, p75)"))							/*
*/	bmi(cts tab("p50 (p25, p75)"))							/*
*/	tws(cts tab("p50 (p25, p75)")) 							/*
*/	tple(cts tab("p50 (p25, p75)")) 						/*
*/	smok(cat countformat(%15.0fc))							/*
*/	wp(cat countformat(%15.0fc))							/*
*/	, by(sex, total) countformat(%15.0fc) notable meanformat(%5.2f) sdformat(%5.1f) /*
*/	exportexcel("Results\table_1", replace)

/*baseline tables by wp_sex*/
gen wp_sex = "wp" + string(wp) + "_sex" + string(sex)
baselinetable   											/*
*/	age0(cts tab("p50 (p25, p75)")) 						/*
*/  msbp(cts tab("p50 (p25, p75)") medianformat(%5.0f))		/*
*/  ldl(cts  tab("p50 (p25, p75)"))							/*
*/	bmi(cts tab("p50 (p25, p75)"))							/*
*/	tws(cts tab("p50 (p25, p75)")) 							/*
*/	tple(cts tab("p50 (p25, p75)")) 						/*
*/	smok(cat countformat(%15.0fc))							/*
*/	, by(wp_sex, total) countformat(%15.0fc) notable meanformat(%5.2f) sdformat(%5.2f) /*
*/	exportexcel("Results\table_s1", replace)
drop wp_sex

stset tdied, f(died==1) id(id)
sum tdied, d
foreach var of varlist died CVD Cancer Other {
	stset tdied, f(`var'==1) id(id)
	strate sex wp, per(1000) output("Results\rate_sexwp_`var'", replace)
}
use "Results\rate_sexwp_CVD", clear
gen out = "CVD"
append using "Results\rate_sexwp_Cancer"
replace out = "Cancer" if out == ""
append using "Results\rate_sexwp_Other"
replace out = "Other" if out == ""
append using "Results\rate_sexwp_died"
replace out = "All_cause" if out == ""
export excel using "Results\table_s2.xls", firstrow(variables) replace
erase "Results\rate_sexwp_CVD.dta"
erase "Results\rate_sexwp_Cancer.dta"
erase "Results\rate_sexwp_Other.dta"
erase "Results\rate_sexwp_died.dta"


***************************************
**Associations*************************
***************************************

*----------------------------------------------------------*
***Comparing models***
use "...\db_cmpr", clear
foreach q in CVD Cancer Other {
	forvalues s = 0/1 {
		qui stset tdied, f(`q'==1) id(id)
		qui stpm2 c.age0 i.wp msbp ldl i.smok tws bmi tple if sex == `s', scale(h) df(4)
		qui estimates store no_`q'_`s'
		qui stpm2 c.age0##i.wp msbp ldl i.smok tws bmi tple if sex == `s', scale(h) df(4)
		qui estimates store y_`q'_`s'
	}
}

clear
tempfile mdes
save `mdes', emptyok replace
foreach q in CVD Cancer Other {
	forvalues s = 0/1 {
		qui lrtest (no_`q'_`s') (y_`q'_`s')
		qui local p_lrt = r(p)
		qui estimates stats no_`q'_`s' y_`q'_`s'
		qui matrix s = r(S)
		qui set obs 2
		qui gen model 	  = "no_`q'_`s'" in 1
		qui replace model = "y_`q'_`s'"	 in 2
		qui gen df 		  = s[1,4]		 in 1
		qui replace df    = s[2,4]		 in 2
		qui gen AIC		  = s[1,5]		 in 1
		qui replace AIC   = s[2,5]		 in 2
		qui gen BIC       = s[1,6]		 in 1
		qui replace BIC   = s[2,6]		 in 2
		qui gen plrt = `p_lrt'
		qui append using `mdes'
		qui save `mdes', replace
		qui clear
	}
}
use `mdes', clear
split model, p("_")
replace model1 = "yes"   if model1 == "y"
replace model3 = "Men"   if model3 == "1"
replace model3 = "Women" if model3 == "0"
renames model1-model3 \ inter cause sex
drop model AIC df
reshape wide BIC, i(cause sex) j(inter) string
gen difBIC = BICyes-BICno
export delimited using "...models_lrt_bic.csv", datafmt replace

***Shapes***
use "...db_cmpr", clear
gen studydes = 1
gen studyid = 1
foreach nm in CVD Cancer Other {
	forvalues s = 0/1 {
		mvshape age0 if sex == `s', studyid(studyid) subjid(id) studydes(studydes) timevar(tdied) failure(`nm'==1) ngrp(10) adjvar(i.wp msbp ldl i.smok tws bmi tple) ///
		saveres("Results\shape_sex`s'_`nm'")
	}
}
graph close _all

clear
set obs 1
gen out = ""
gen sex = .
foreach nm in CVD Cancer Other {
	forvalues s = 0/1 {
		append using "Results\shape_sex`s'_`nm'"
		replace out = "`nm'" if out == ""
		replace sex = `s' if sex == .
	}
}
rename age010 age0_deciles
save "Results\shapes", replace

foreach nm in CVD Cancer Other {
	forvalues s = 0/1 {
		erase "Results\shape_sex`s'_`nm'.dta"
	}
}


*----------------------------------------------------------*
***Absolute (standsurv) - cumulative incidences of causes*** 
clear all
use "...\db_cmpr", clear
tab wp, gen(dwp)
tab smok, gen(dsmok)
foreach q in CVD Cancer Other {
	forvalues s = 0/1 {
		qui stset tdied, f(`q'==1) id(id)
		qui stpm2 dwp2 dwp3 age0 msbp ldl dsmok2 dsmok3 tws bmi tple if sex == `s', scale(h) df(4)
		qui estimates store `q'_`s'
	}
}

*Until 10y (no CI) - cumulative incidences*
forvalues k = 45(10)75 {
	forvalues s = 0/1 {
		preserve
		keep if sex == `s'
		range tt 0 10 51
		standsurv, crmodels(CVD_`s' Cancer_`s' Other_`s') cif timevar(tt) atvar(sl_ av_ br_) /*
		*/ 	at1(dwp2 0 dwp3 0 age0 `k') /*
		*/	at2(dwp2 1 dwp3 0 age0 `k') /*
		*/	at3(dwp2 0 dwp3 1 age0 `k')	   							
		qui keep tt sl_* av_* br_*
		qui drop if tt == .   		
		qui gen age0 = `k'
		qui gen sex = `s'
		renames sl__CVD_? sl__Cancer_? sl__Other_? av__CVD_? av__Cancer_? av__Other_? br__CVD_? br__Cancer_? br__Other_? \ /*
		*/		sl__CVD   sl__Cancer   sl__Other   av__CVD   av__Cancer   av__Other   br__CVD   br__Cancer   br__Other
		tempfile r`k'`s'			
		save `r`k'`s'', replace			
		restore
	}
}
preserve
clear
forvalues k = 45(10)75 {
	forvalues s = 0/1 {
		append using `r`k'`s''
	}
}
save "Results\cmpage", replace
restore

*Until 10y (no CI) - overall failure (all-cause death)*
forvalues k = 45(10)75 {
	forvalues s = 0/1 {
		preserve
		keep if sex == `s'
		range tt 0 10 51
		standsurv, failure crmodels(CVD_`s' Cancer_`s' Other_`s') timevar(tt) atvar(sl_failure av_failure br_failure) /*
		*/ 	at1(dwp2 0 dwp3 0 age0 `k') /*
		*/	at2(dwp2 1 dwp3 0 age0 `k') /*
		*/	at3(dwp2 0 dwp3 1 age0 `k')	   							
		qui keep tt sl_* av_* br_*
		qui drop if tt == .   		
		qui gen age0 = `k'
		qui gen sex = `s'
		tempfile r`k'`s'			
		save `r`k'`s'', replace			
		restore
	}
}
preserve
clear
forvalues k = 45(10)75 {
	forvalues s = 0/1 {
		append using `r`k'`s''
	}
}
save "Results\failage", replace	
restore

*Only at 10y (with CI) - cumulative incidences*
forvalues k = 45(10)75 {
	forvalues s = 0/1 {
		preserve
		keep if sex == `s'
		gen tt = 10 in 1
		standsurv, crmodels(CVD_`s' Cancer_`s' Other_`s') cif timevar(tt) atvar(sl_ av_ br_) contrast(difference) ci /*
		*/ 	at1(dwp2 0 dwp3 0 age0 `k') /*
		*/	at2(dwp2 1 dwp3 0 age0 `k') /*
		*/	at3(dwp2 0 dwp3 1 age0 `k')	   							
		qui keep tt _contrast*
		qui drop if tt == .
		qui gen age0 = `k'
		qui gen sex = `s'
		renames _contrast2_CVD_? _contrast2_CVD_?_lci _contrast2_CVD_?_uci _contrast2_Cancer_? _contrast2_Cancer_?_lci _contrast2_Cancer_?_uci _contrast2_Other_? _contrast2_Other_?_lci _contrast2_Other_?_uci \ /*
		*/		_contrast2_CVD   _contrast2_CVD_lci   _contrast2_CVD_uci   _contrast2_Cancer   _contrast2_Cancer_lci   _contrast2_Cancer_uci   _contrast2_Other   _contrast2_Other_lci   _contrast2_Other_uci
		renames _contrast3_CVD_? _contrast3_CVD_?_lci _contrast3_CVD_?_uci _contrast3_Cancer_? _contrast3_Cancer_?_lci _contrast3_Cancer_?_uci _contrast3_Other_? _contrast3_Other_?_lci _contrast3_Other_?_uci \ /*
		*/		_contrast3_CVD   _contrast3_CVD_lci   _contrast3_CVD_uci   _contrast3_Cancer   _contrast3_Cancer_lci   _contrast3_Cancer_uci   _contrast3_Other   _contrast3_Other_lci   _contrast3_Other_uci
		tempfile r`k'`s'			
		tempfile r`k'`s'			
		save `r`k'`s'', replace			
		restore
	}
}
preserve
clear
forvalues k = 45(10)75 {
	forvalues s = 0/1 {
		append using `r`k'`s''
	}
}
save "Results\cmpage10", replace
restore


*----------------------------------------------------------*
***Relative: The Stata Journal (2020) 20, Number 4, pp. 763–784***
clear all
use "...\db_cmpr", clear
tab wp, gen(dwp)
tab smok, gen(dsmok)

forvalues s = 0/1 {
	
	merlin (tdied dwp2 dwp3 age0 msbp ldl dsmok2 dsmok3 tws bmi tple if sex == `s', family(rp, df(4) failure(CVD))) 	/*
	*/	   (tdied dwp2 dwp3 age0 msbp ldl dsmok2 dsmok3 tws bmi tple if sex == `s', family(rp, df(4) failure(Cancer))) 	/*
	*/	   (tdied dwp2 dwp3 age0 msbp ldl dsmok2 dsmok3 tws bmi tple if sex == `s', family(rp, df(4) failure(Other)))
	estimate store cmphr
	
	preserve
	parmest, fast label
	keep if eq == "_cmp_1_1_1" | eq == "_cmp_1_2_1" | eq == "_cmp_2_1_1" | eq == "_cmp_2_2_1" | eq == "_cmp_3_1_1" | eq == "_cmp_3_2_1"
	keep eq estimate stderr min95 max95 
	split eq, p(_)
	replace eq3 = "CVD"    	if eq3 == "1"
	replace eq3 = "Cancer" 	if eq3 == "2"
	replace eq3 = "Other"  	if eq3 == "3"
	replace eq4 = "Average"	if eq4 == "1"
	replace eq4 = "Brisk"	if eq4 == "2"
	drop eq1 eq2 eq5
	renames eq3 eq4 \ cause wp
	sort wp cause
	tempfile hr
	save `hr', replace
	restore
	
	preserve
	nlcom (_cmp_2_1_1__av_Cancer_CVD: exp(- _b[_cmp_2_1_1:_cons] + _b[_cmp_1_1_1:_cons])), post
	parmest, fast
	tempfile f1
	save `f1', replace

	estimate restore cmphr
	nlcom (_cmp_3_1_1__av_Other_CVD:  exp(- _b[_cmp_3_1_1:_cons] + _b[_cmp_1_1_1:_cons])), post
	parmest, fast
	tempfile f2
	save `f2', replace

	estimate restore cmphr
	nlcom (_cmp_2_2_1__br_Cancer_CVD: exp(- _b[_cmp_2_2_1:_cons] + _b[_cmp_1_2_1:_cons])), post
	parmest, fast
	tempfile f3
	save `f3', replace

	estimate restore cmphr
	nlcom (_cmp_3_2_1__br_Other_CVD:  exp(- _b[_cmp_3_2_1:_cons] + _b[_cmp_1_2_1:_cons])), post
	parmest, fast
	tempfile f4
	save `f4', replace

	clear
	forvalues k = 1/4 {
		append using `f`k''
	}
	
	split parm, p(__)
	renames parm1 parm2 estimate stderr min95 max95 \ eq comparison rhr rhr_se rhr_min95 rhr_max95
	drop parm z p
	merge 1:1 eq using `hr', update
	sort wp cause
	drop _merge eq
	order wp cause estimate stderr min95 max95 comparison rhr rhr_se rhr_min95 rhr_max95
	gen hr = exp(estimate), after(estimate)
	gen sex = `s'
	tempfile releff`s'
	save `releff`s'', replace
	restore
}

clear
forvalues s = 0/1 {
	append using `releff`s''
}
save "Results\releff", replace
log close


**********************************
*GRAPHS***************************
**********************************

**Figure 1 [relative risk - scatter forest plot]**
use "Results\releff", replace
replace rhr = 1 if rhr == .
replace min95 = exp(min95)
replace max95 = exp(max95)
renames min95 max95 \ hr_min95 hr_max95

replace rhr = (rhr - 1)*100
replace rhr_min95 = (rhr_min95 - 1)*100
replace rhr_max95 = (rhr_max95 - 1)*100

tostring sex, replace
replace sex = "Women" if sex == "0"
replace sex = "Men" if sex == "1"

export excel using "...\All_figures.xls", sheet("Fig1", modify) firstrow(variables) keepcellfmt
foreach sp in Average Brisk {
	foreach nm in Cancer Other {
		foreach sex in Women Men {
		
		sum hr 		  if wp == "`sp'" & cause == "`nm'" & sex == "`sex'", meanonly
		local `sp'_hr_`nm'_`sex' = `r(mean)'

		sum rhr_min95 if wp == "`sp'" & cause == "`nm'" & sex == "`sex'", meanonly
		local `sp'_lb_`nm'_`sex' = `r(mean)'

		sum rhr_max95 if wp == "`sp'" & cause == "`nm'" & sex == "`sex'", meanonly
		local `sp'_ub_`nm'_`sex' = `r(mean)'
		}
	}
}
	
foreach sex in Women Men {
	
	tw (scatter hr rhr if wp == "Average" & cause == "CVD" 	  & sex == "`sex'", sort mlabel(cause) mlabpos(5) mlabcolor(orange) msymbol(square)  msize(small) mcolor(orange))	///
	(scatter 	hr rhr if wp == "Average" & cause == "Cancer" & sex == "`sex'", sort mlabel(cause) mlabpos(5) mlabcolor(orange) msymbol(circle)  msize(small) mcolor(orange))	///	
	(scatter 	hr rhr if wp == "Average" & cause == "Other"  & sex == "`sex'", sort mlabel(cause) mlabpos(5) mlabcolor(orange) msymbol(diamond) msize(small) mcolor(orange))	///	
	(rspike	 	hr_min95 hr_max95 rhr if wp == "Average" 	  & sex == "`sex'", sort lcolor(orange)) 																			///
	(scatteri 	`Average_hr_Cancer_`sex'' `Average_lb_Cancer_`sex'' `Average_hr_Cancer_`sex'' `Average_ub_Cancer_`sex'' if wp == "Average", recast(line) lcolor(orange))		///
	(scatteri 	`Average_hr_Other_`sex''  `Average_lb_Other_`sex''  `Average_hr_Other_`sex''  `Average_ub_Other_`sex''  if wp == "Average", recast(line) lcolor(orange))		///
	(scatter 	hr rhr if wp == "Brisk" & cause == "CVD"    & sex == "`sex'", sort mlabel(cause) mlabpos(5) mlabcolor(forest_green) msymbol(square)  msize(small) mcolor(forest_green))	///
	(scatter 	hr rhr if wp == "Brisk" & cause == "Cancer" & sex == "`sex'", sort mlabel(cause) mlabpos(5) mlabcolor(forest_green) msymbol(circle)  msize(small) mcolor(forest_green))	///	
	(scatter 	hr rhr if wp == "Brisk" & cause == "Other"  & sex == "`sex'", sort mlabel(cause) mlabpos(5) mlabcolor(forest_green) msymbol(diamond) msize(small) mcolor(forest_green))	///	
	(rspike 	hr_min95 hr_max95 rhr if wp == "Brisk" 		& sex == "`sex'", sort lcolor(forest_green)) 																				///
	(scatteri 	`Brisk_hr_Cancer_`sex'' `Brisk_lb_Cancer_`sex'' `Brisk_hr_Cancer_`sex'' `Brisk_ub_Cancer_`sex'' if wp == "Brisk", recast(line) lcolor(forest_green))			///
	(scatteri 	`Brisk_hr_Other_`sex''  `Brisk_lb_Other_`sex''  `Brisk_hr_Other_`sex''  `Brisk_ub_Other_`sex'' if wp == "Brisk",  recast(line) lcolor(forest_green))			///
	, legend(off) xlab(-60(10)80, format(%5.0f) grid gmax labsize(small)) ylab(#10, format(%5.1f) angle(h) gmax gmin labsize(small)) 											///
	ymtick(##2) xmtick(##2) ytitle("Hazard ratio vs slow") xtitle("Relative effect compared to CVD mortality risk reduction (%)", size(small)) 									///
	graphregion(fcolor(white)) title("`sex'", size(medsmall) color(black)) name("Fig_1_`sex'", replace) nodraw
}
graph combine Fig_1_Women Fig_1_Men, ycommon cols(2) graphregion(fcolor(white)) xsize(6) ysize(3.5) name("Fig_1", replace)
graph save "Fig_1" "...\Fig_1.gph", replace   


**Figure 2 [Cause-specific risk up to 10y]**
use "Results\cmpage", replace

gen stack0_Slow = sl__CVD
gen stack1_Slow = sl__CVD  + sl__Cancer
gen stack2_Slow = sl__CVD  + sl__Cancer  + sl__Other

gen stack0_Average = av__CVD
gen stack1_Average = av__CVD  + av__Cancer
gen stack2_Average = av__CVD  + av__Cancer  + av__Other

gen stack0_Brisk = br__CVD
gen stack1_Brisk = br__CVD  + br__Cancer
gen stack2_Brisk = br__CVD  + br__Cancer  + br__Other

drop sl* av* br*

order age0 tt, last
sort sex age tt
foreach var of varlist stack* {
	replace `var' = `var' * 100
}

reshape long stack0_ stack1_ stack2_, i(sex age0 tt) j(wp) string
gen wpn = 1 if wp == "Slow"
replace wpn = 2 if wp == "Average"
replace wpn = 3 if wp == "Brisk"
sencode wp, gsort(wpn) replace
sort sex age0 wp tt
tostring sex, replace
replace sex = "Women" if sex == "0"
replace sex = "Men" if sex == "1"
sencode sex, replace
gen age0n = string(age0) + " years"

export excel using "...\All_figures.xls", sheet("Fig2", modify) firstrow(variables) keepcellfmt
twoway (area stack0_ tt, sort fcolor(blue%65) lwidth(none)) 										///
	   (rarea stack1_ stack0_ tt, sort fcolor(gold%65) lwidth(none))								///
	   (rarea stack2_ stack1_ tt, sort fcolor(maroon%65) lwidth(none))								///
	   , by(sex wp age0n, cols(6) colfirst legend(off) note("") graphregion(fcolor(white)))			///
	   subtitle(, fcolor(none) lcolor(black) size(small)) ymtick(##2)								///
	   xsize(5.5) ysize(4) ylab(#5, angle(h) labsize(small) gmax) xlab(#10, grid labsize(small))	///
	   xtitle("Time (years)", size(small)) ytitle("10-year mortality risk (%)", size(small))		///
	   name("Fig_2", replace)
graph save "Fig_2" "...\Fig_2.gph", replace


**Figure 3 [10y risk difference - forest plot]**
use "Results\cmpage10", replace
drop tt
foreach var of varlist _contrast* {
	replace `var' = `var'*100
}
foreach nm in CVD Cancer Other {
	rename _contrast2_`nm' 		_contrast2_es_`nm'
	rename _contrast2_`nm'_lci	_contrast2_lci_`nm'
	rename _contrast2_`nm'_uci	_contrast2_uci_`nm'
	rename _contrast3_`nm' 		_contrast3_es_`nm'
	rename _contrast3_`nm'_lci	_contrast3_lci_`nm'
	rename _contrast3_`nm'_uci	_contrast3_uci_`nm'
}

reshape long _contrast2_es _contrast2_lci _contrast2_uci  _contrast3_es _contrast3_lci _contrast3_uci,  i(sex age) j(cause) string
renames _contrast2_es _contrast2_lci _contrast2_uci _contrast3_es _contrast3_lci _contrast3_uci \ es_AS lb_AS ub_AS es_BS lb_BS ub_BS 
replace cause = subinstr(cause, "_", "",.)
reshape long es_ lb_ ub_, i(sex age0 cause) j(comp) string
gen se = (ub_-lb_)/3.92
replace comp = "Average" if comp == "AS"
replace comp = "Brisk" if comp == "BS"
tostring sex, replace
replace sex = "Women" if sex == "0"
replace sex = "Men" if sex == "1"
label variable comp  "vs Slow"
label variable cause "Cause"
label variable age0  "Age (years)"
gsort age0 cause comp -sex

export excel using "...\All_figures.xls", sheet("Fig3", modify) firstrow(variables) keepcellfmt

qui forestplot es_ lb_ ub_ if sex == "Women", effect("Difference, %") lcols(age0 cause comp)                        									///
					   nonull nonames noov nosu nowt dp(2) classic boxscale(70) astext(60) textsize(160) xlabel(-12(2)0, labsize(vsmall) nogrid) 		///
					   spacing(2.5) yline(2.5(2)22.5, lwidth(vthin) lpattern(vshortdash) lcolor(black)) xtitle("10-year risk difference", size(7pt))	///
					   leftjustify ciopts(lwidth(vthin)) plotid(comp)         									///
					   box1opts(mcolor(red)) ci1opts(lcolor(red)) box2opts(mcolor(blue)) ci2opts(lcolor(blue))  ///
					   xline(0, lcolor(black) lpattern(solid) lwidth(vthin)) 									///
					   title("Women", size(small)) name("Fig_3w", replace) xsize(6) ysize(4) scale(0.75) nodraw
qui forestplot es_ lb_ ub_ if sex == "Men", effect("Difference, %") lcols(age0 cause comp)                        										///
					   nonull nonames noov nosu nowt dp(2) classic boxscale(70) astext(60) textsize(160) xlabel(-12(2)0, labsize(vsmall) nogrid) 		///
					   spacing(2.5) yline(2.5(2)22.5, lwidth(vthin) lpattern(vshortdash) lcolor(black)) xtitle("10-year risk difference", size(7pt))	///
					   leftjustify ciopts(lwidth(vthin)) plotid(comp)         									///
					   box1opts(mcolor(red)) ci1opts(lcolor(red)) box2opts(mcolor(blue)) ci2opts(lcolor(blue))  ///
					   xline(0, lcolor(black) lpattern(solid) lwidth(vthin)) 									///
					   title("Men", size(small)) name("Fig_3m", replace) xsize(6) ysize(4) scale(0.75) nodraw 					   
graph combine Fig_3w Fig_3m, name("Fig_3", replace) scale(1.2) graphregion(fcolor(white))
graph save "Fig_3" "...\Fig_3.gph", replace


**Figure S1 [Flowchart]**

**Figure S2 [Cause-specific shapes]**   
use "Results\shapes", clear
keep out sex age0_deciles frr frrlci frruci pmage0
drop if frr == .
tostring sex, replace
replace sex = "Men" if sex == "1"
replace sex = "Women" if sex == "0"

export excel using "...\All_figures.xls", sheet("FigS2", modify) firstrow(variables) keepcellfmt
twoway (scatter frr pmage0, sort mcolor(black) msize(vsmall) msymbol(square)) 						///
	   (rspike frrlci frruci pmage0, sort lcolor(black) lwidth(vthin))								///
	   , ytitle("Hazard ratio") yscale(log) ylabel(1 2 4 8 16, angle(horizontal) labsize(small)) 	///
	   xtitle("Age (years)") xlabel(40(5)70, grid labsize(small)) xmtick(##5) 						///
	   by(, legend(off) graphregion(fcolor(white))) by(sex out, cols(3) note("")) xsize(6) ysize(4) ///
	   subtitle(, fcolor(none) lcolor(black) size(small)) name("Fig_S2", replace)
graph save "Fig_S2" "...\Fig_S2.gph", replace

**Fig S3 [Other causes of death]**
use "...\db_cmpr", clear
tab cause_death died, m
groups icd10 if cause_death == "Other", order(h) format(%7.2f) saving(freqICD, replace)

use "...\freqICD", clear
keep if _Percent<=75
icd10 clean icd10, generate(check)
icd10 generate term = icd10, description
compress
replace term = "COVID-19, virus identified" 									 if check == "U07.1"
replace term = "Drug resistant tuberculosis, drug unspecified" 					 if check == "U50.9"
replace term = "Fall on and from stairs and steps" 								 if check == "W10.0"
replace term = "Unspecified fall home while engaged in sports activity" 		 if check == "W19.0"
replace term = "Intentional self-harm by hanging, strangulation and suffocation" if check == "X70.0"
replace term = subinstr(term, "Chronic obstructive pulmonary disease", "COPD",.)
mdesc

export excel using "...\All_figures.xls", sheet("FigS3", modify) firstrow(variables) keepcellfmt
graph hbar (asis) _percent, over(term, sort(_percent) descending label(labsize(tiny))) 			///
	  bar(1, fcolor(%50) lcolor(none)) ytitle("%", size(small)) ylabel(#10, grid) 				///
	  ylabel(0(1)10, labsize(small)) ymtick(##2) xsize(9) ysize(11) graphregion(fcolor(white)) 	///
	  name("Fig_S3", replace)
graph save "Fig_S3" "...\Fig_S3.gph", replace
graph close _all

**Figure S4 [Total mortality risk by WP until 10y]**   
use "...\failage", replace
renames sl_failure av_failure br_failure \ Slow Average Brisk
foreach var of varlist Slow Average Brisk {
	replace `var' = `var' * 100
}
sort sex age0 tt
tostring sex, replace
replace sex = "Men" if sex == "1"
replace sex = "Women" if sex == "0"
gen age0n = string(age0) + " years"
sencode sex, replace

export excel using "...\All_figures.xls", sheet("FigS4", modify) firstrow(variables) keepcellfmt
twoway (line Slow tt, 	 sort lcolor(red) lwidth(medium) lpattern(shortdash)) 			///
	   (line Average tt, sort lcolor(orange) lwidth(medium) lpattern(dash_dot)) 		///
	   (line Brisk tt,   sort lcolor(forest_green) lwidth(medium) lpattern(solid))		///
	   , by(, legend(position(12))) legend(rows(1) nobox region(fcolor(none) lcolor(none)) bmargin(zero) size(small)) 	///
	   by(sex age0n, note("") cols(4) graphregion(fcolor(white))) subtitle(, fcolor(none) lcolor(black) size(small)) 	///
	   xsize(8.5) ysize(5) ylab(0(5)30, angle(h) labsize(small) gmax) xlab(#10, grid labsize(small))	///
	   ymtick(##2) xtitle("Time (years)", size(small)) ytitle("Mortality risk (%)", size(small))		///
	   name("Fig_S4", replace)
graph save "Fig_S4" "...\Fig_S4.gph", replace
graph close _all




*#################################################################################################################################*
											* WALKING PACE ---- REMOVING THOSE WITH COVID DEATHS [U07.1 & U07.2] *
*#################################################################################################################################*
use "...\db_cmpr", clear
icd10 clean icd10, generate(check)
icd10 generate term = icd10, description
compress
replace term = "COVID-19, virus identified"     if check == "U07.1"
replace term = "COVID-19, virus not identified" if check == "U07.2"
drop if check == "U07.1" | check == "U07.2"
describe, s
save "...\db_cmpr_c19", replace

*----------------------------------------------------------*
***Relative***
clear all
use "...\db_cmpr_c19", clear
tab wp, gen(dwp)
tab smok, gen(dsmok)

forvalues s = 0/1 {
	
	merlin (tdied dwp2 dwp3 age0 msbp ldl dsmok2 dsmok3 tws bmi tple if sex == `s', family(rp, df(4) failure(CVD))) 	/*
	*/	   (tdied dwp2 dwp3 age0 msbp ldl dsmok2 dsmok3 tws bmi tple if sex == `s', family(rp, df(4) failure(Cancer))) 	/*
	*/	   (tdied dwp2 dwp3 age0 msbp ldl dsmok2 dsmok3 tws bmi tple if sex == `s', family(rp, df(4) failure(Other)))
	estimate store cmphr
	
	preserve
	parmest, fast label
	keep if eq == "_cmp_1_1_1" | eq == "_cmp_1_2_1" | eq == "_cmp_2_1_1" | eq == "_cmp_2_2_1" | eq == "_cmp_3_1_1" | eq == "_cmp_3_2_1"
	keep eq estimate stderr min95 max95 
	split eq, p(_)
	replace eq3 = "CVD"    	if eq3 == "1"
	replace eq3 = "Cancer" 	if eq3 == "2"
	replace eq3 = "Other"  	if eq3 == "3"
	replace eq4 = "Average"	if eq4 == "1"
	replace eq4 = "Brisk"	if eq4 == "2"
	drop eq1 eq2 eq5
	renames eq3 eq4 \ cause wp
	sort wp cause
	tempfile hr
	save `hr', replace
	restore
	
	preserve
	nlcom (_cmp_2_1_1__av_Cancer_CVD: exp(- _b[_cmp_2_1_1:_cons] + _b[_cmp_1_1_1:_cons])), post
	parmest, fast
	tempfile f1
	save `f1', replace

	estimate restore cmphr
	nlcom (_cmp_3_1_1__av_Other_CVD:  exp(- _b[_cmp_3_1_1:_cons] + _b[_cmp_1_1_1:_cons])), post
	parmest, fast
	tempfile f2
	save `f2', replace

	estimate restore cmphr
	nlcom (_cmp_2_2_1__br_Cancer_CVD: exp(- _b[_cmp_2_2_1:_cons] + _b[_cmp_1_2_1:_cons])), post
	parmest, fast
	tempfile f3
	save `f3', replace

	estimate restore cmphr
	nlcom (_cmp_3_2_1__br_Other_CVD:  exp(- _b[_cmp_3_2_1:_cons] + _b[_cmp_1_2_1:_cons])), post
	parmest, fast
	tempfile f4
	save `f4', replace

	clear
	forvalues k = 1/4 {
		append using `f`k''
	}
	
	split parm, p(__)
	renames parm1 parm2 estimate stderr min95 max95 \ eq comparison rhr rhr_se rhr_min95 rhr_max95
	drop parm z p
	merge 1:1 eq using `hr', update
	sort wp cause
	drop _merge eq
	order wp cause estimate stderr min95 max95 comparison rhr rhr_se rhr_min95 rhr_max95
	gen hr = exp(estimate), after(estimate)
	gen sex = `s'
	tempfile releff`s'
	save `releff`s'', replace
	restore
}

clear
forvalues s = 0/1 {
	append using `releff`s''
}
save "Results\C19releff", replace


**Figure S5 [relative risk - scatter forest plot]**
use "Results\C19releff", replace
replace rhr = 1 if rhr == .
replace min95 = exp(min95)
replace max95 = exp(max95)
renames min95 max95 \ hr_min95 hr_max95

replace rhr = (rhr - 1)*100
replace rhr_min95 = (rhr_min95 - 1)*100
replace rhr_max95 = (rhr_max95 - 1)*100

tostring sex, replace
replace sex = "Women" if sex == "0"
replace sex = "Men" if sex == "1"

export excel using "...\All_figures.xls", sheet("FigS5", modify) firstrow(variables) keepcellfmt
foreach sp in Average Brisk {
	foreach nm in Cancer Other {
		foreach sex in Women Men {
		
		sum hr 		  if wp == "`sp'" & cause == "`nm'" & sex == "`sex'", meanonly
		local `sp'_hr_`nm'_`sex' = `r(mean)'

		sum rhr_min95 if wp == "`sp'" & cause == "`nm'" & sex == "`sex'", meanonly
		local `sp'_lb_`nm'_`sex' = `r(mean)'

		sum rhr_max95 if wp == "`sp'" & cause == "`nm'" & sex == "`sex'", meanonly
		local `sp'_ub_`nm'_`sex' = `r(mean)'
		}
	}
}
	
foreach sex in Women Men {
	tw (scatter hr rhr if wp == "Average" & cause == "CVD" 	  & sex == "`sex'", sort mlabel(cause) mlabpos(5) mlabcolor(orange) msymbol(square)  msize(small) mcolor(orange))	///
	(scatter 	hr rhr if wp == "Average" & cause == "Cancer" & sex == "`sex'", sort mlabel(cause) mlabpos(5) mlabcolor(orange) msymbol(circle)  msize(small) mcolor(orange))	///	
	(scatter 	hr rhr if wp == "Average" & cause == "Other"  & sex == "`sex'", sort mlabel(cause) mlabpos(5) mlabcolor(orange) msymbol(diamond) msize(small) mcolor(orange))	///	
	(rspike	 	hr_min95 hr_max95 rhr if wp == "Average" 	  & sex == "`sex'", sort lcolor(orange)) 																			///
	(scatteri 	`Average_hr_Cancer_`sex'' `Average_lb_Cancer_`sex'' `Average_hr_Cancer_`sex'' `Average_ub_Cancer_`sex'' if wp == "Average", recast(line) lcolor(orange))		///
	(scatteri 	`Average_hr_Other_`sex''  `Average_lb_Other_`sex''  `Average_hr_Other_`sex''  `Average_ub_Other_`sex''  if wp == "Average", recast(line) lcolor(orange))		///
	(scatter 	hr rhr if wp == "Brisk" & cause == "CVD"    & sex == "`sex'", sort mlabel(cause) mlabpos(5) mlabcolor(forest_green) msymbol(square)  msize(small) mcolor(forest_green))	///
	(scatter 	hr rhr if wp == "Brisk" & cause == "Cancer" & sex == "`sex'", sort mlabel(cause) mlabpos(5) mlabcolor(forest_green) msymbol(circle)  msize(small) mcolor(forest_green))	///	
	(scatter 	hr rhr if wp == "Brisk" & cause == "Other"  & sex == "`sex'", sort mlabel(cause) mlabpos(5) mlabcolor(forest_green) msymbol(diamond) msize(small) mcolor(forest_green))	///	
	(rspike 	hr_min95 hr_max95 rhr if wp == "Brisk" 		& sex == "`sex'", sort lcolor(forest_green)) 																				///
	(scatteri 	`Brisk_hr_Cancer_`sex'' `Brisk_lb_Cancer_`sex'' `Brisk_hr_Cancer_`sex'' `Brisk_ub_Cancer_`sex'' if wp == "Brisk", recast(line) lcolor(forest_green))			///
	(scatteri 	`Brisk_hr_Other_`sex''  `Brisk_lb_Other_`sex''  `Brisk_hr_Other_`sex''  `Brisk_ub_Other_`sex'' if wp == "Brisk",  recast(line) lcolor(forest_green))			///
	, legend(off) xlab(-60(10)80, format(%5.0f) grid gmax labsize(small)) ylab(#10, format(%5.1f) angle(h) gmax gmin labsize(small)) 											///
	ymtick(##2) xmtick(##2) ytitle("Hazard ratio vs slow") xtitle("Relative effect compared to CVD mortality risk reduction (%)", size(small)) 									///
	graphregion(fcolor(white)) title("`sex'", size(medsmall) color(black)) name("Fig_S5_`sex'", replace) nodraw
}
graph combine Fig_S5_Women Fig_S5_Men, ycommon cols(2) graphregion(fcolor(white)) xsize(6) ysize(3.5) name("Fig_S5", replace)
graph save "Fig_S5" "...\Fig_S5.gph", replace   
graph close _all
