ssc install fre

*****************************
**#1 Preparing 2021 SASI data
*******************************
clonevar race=ppethm
clonevar age=ppage
gen hhinc=ppinc7
recode hhinc (1=5)(2=17.5)(3=37.5)(4=62.5)(5=87.5)(6=125)(7=175)
lab var hhinc "Household income in thousands of dollars"
clonevar female=ppgender
recode female (1=0)(2=1)
lab def female 1 "Female" 0 "Male"
lab val female female

** Attributions from GSS (recode so very likely=4, and drop refusals)
clonevar charactr=Q1_CHARACTR
clonevar imbalnce=Q2_IMBALNCE
clonevar wayraise=Q3_WAYRAISE
clonevar genetics=Q4_GENETICS

recode charactr-genetics (-1=.)(1=4)(2=3)(3=2)(1=4)
lab def rev 1 "Least likely" 4 "Most likely"
label val charactr-genetics rev
fre Q1_CHARACTR-Q5_GENETICS charactr-genetics

* Social Distance from GSS 2018 but note friend question is modified (1=definitely willing, 4 = definitely unwilling; drop refusals)
clonevar vignei=Q15_VIGNEI
clonevar vigsoc=Q16_VIGSOC
clonevar vigjob=Q17_VIGJOB
clonevar viggrp=Q18_VIGGRP
clonevar vigmar=Q19_VIGMAR
clonevar vigfrnd=Q20_VIGFRND

recode vignei-vigfrnd (-1=.)
fre Q15_VIGNEI-Q20_VIGFRND vignei-vigfrnd
factor vignei-vigfrnd
alpha vignei-vigfrnd

egen socdistgss=rowmean(vignei-vigfrnd)
lab var socdistgss "Social distance scale (GSS), hi=more stigma"

** Structural stigma questions: employment, healthcare, school, housing
clonevar ppjob1=Q35_PPJOB1
clonevar ppjob2=Q36_PPJOB2
clonevar pphlth1=Q37_PPHLTH1
clonevar pphlth2=Q38_PPHLTH2
clonevar ppschl=Q39_PPSCHL
clonevar pplndlrd=Q40_PPLNDLRD
clonevar crime=Q41_POLICY1

        *Recode items so higher = MORE support for structural stigma, drop refusals
recode ppjob1-crime (-1=.)
recode ppjob1 ppschl pplndlrd (1=4)(2=3)(3=2)(4=1)
label val ppjob1 ppschl pplndlrd agree

fre Q35_PPJOB1-Q41_POLICY1 ppjob1-crime
factor ppjob1-crime
alpha ppjob1-crime
        * Based on factor EFA, Brea found these items to be the best fit for a structural stigma scale
alpha ppjob2-ppschl crime,item
egen structural=rowmean(ppjob2-ppschl crime)

* Personal experience with SUD
clonevar know=Q50_KNOW1
        *Recode to drop refusals
        recode know (-1=.)(2=0)(3=.)
lab var know "Knows someone with vignette problem"

** Create new vignette categories
gen vigactive=xvignettes
recode vigactive (1/2=1)(3/4=1)(5=1)(6/7=0)(8/9=0)(10=0)
lab var vigactive "Vignette - 1=active, 0=recovery"

gen dx=.
replace dx=1 if aud==1
replace dx=2 if poud==1
replace dx=3 if hud==1
replace dx=4 if mud==1
lab def dx 1 "Alcohol" 2 "Rx Opioids" 3 "Heroin" 4 "Meth"
lab val dx dx
lab var dx "Vignette diagnosis"

** Define populations (Health professionals)
gen hprof=0
replace hprof=1 if S2!=.
replace hprof=0 if S2==291040 | S2==291130 | S2==292010 | S2==292030 | S2==292050 | S2==292070 | S2==292090 | S2==319094 | S2==319099
replace hprof=. if PPWORKA_1==-1
lab var hprof "Healthcare professional"

lab def hprof 0 "Gen. Pop." 1 "HCP" 
lab val hprof hprof
lab def know 0 "No Contact" 1 "Know Someone" 
lab val know know

*****************************
**#2 Demographics
*****************************
gen wt=Genpop_wt
replace wt=HCP_wt if xsud==3
lab var wt "Sampling weights for combine general population with HCP oversamples"

drop if xsud ==2
drop if xsud ==4

tabulate S2
tabulate S2 if hprof > 0
tabulate (ppinc7) (hprof)
tabulate (ppeduc5) (hprof)
tabulate (race) (hprof)
tabulate (female) (hprof)
by hprof, sort : summarize age
tabulate hprof know, chi2
ttest close if know > 0, by(hprof)


foreach x in charactr wayraise imbalnce genetics socdistgss {
	egen std`x' = std(`x')
}


*****************************
**#3 Hypothesis 1
*****************************

svyset [pw=wt]

svy: reg stdcharactr hprof#know i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
testparm know#hprof, equal
pwcompare know#hprof, effects mcompare(bonferroni)
test 0.hprof#0.know - 0.hprof#1.know = -.43
predict p_char
anova p_char know##hprof
margins know#hprof
marginsplot, recast(bar) by(know) ytitle("Bad Character Attributions") ylabel(-.4 (.2) .4) xtitle("") plotop(barw(.8) fintensity(inten30)) ciop(msize(vlarge) lw(medthick)) name(fig1a, replace)

svy: reg stdwayraise hprof#know i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
testparm know#hprof, equal
pwcompare know#hprof, effects mcompare(bonferroni)
predict p_wayraise
anova p_wayraise know##hprof
margins know#hprof
marginsplot, recast(bar) by(know) ytitle("Way Raised Attributions") ylabel(-.4 (.2) .4) xtitle("") plotop(barw(.8) fintensity(inten30)) ciop(msize(vlarge) lw(medthick)) name(fig1b, replace)

svy: reg stdimbalnce hprof#know i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
testparm know#hprof, equal
pwcompare know#hprof, effects mcompare(bonferroni)
test 0.hprof#1.know - 0.hprof#0.know = .124
predict p_imb
anova p_imb know##hprof
margins know#hprof
marginsplot, recast(bar) by(know) ti("") ytitle("Chemical Imbalance Attributions") ylabel(-.4 (.2) .4) xtitle("") xlabel() plotop(barw(.8) fintensity(inten30)) ciop(msize(vlarge) lw(medthick)) name(fig1c, replace)

svy: reg stdgenetics hprof#know i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
testparm know#hprof, equal
pwcompare know#hprof, effects mcompare(bonferroni)
test 0.hprof#1.know - 0.hprof#0.know = .195
predict p_gen
anova p_gen know##hprof
margins know#hprof
marginsplot, xdim(hprof) recast(bar) by(know) ti("") ytitle("Genetic Attributions") ylabel(-.4 (.2) .4) xtitle("") xlabel() plotop(barw(.8) fintensity(inten30)) ciop(msize(vlarge) lw(medthick)) name(fig1d, replace)

*create composite Figure 1
graph combine fig1a fig1b fig1c fig1d, row(2) col(2)
graph export "\\Client\F$\krendl lab\Lucas\Stigma & SUDs\Healthcare Professionals\Figure 1.png", as(png) name("Figure 1")

****closeness swapped in for knowing
svy: reg stdcharactr hprof#c.close i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
testparm c.close#hprof, equal
margins, at(close=(1(1)9)) by(hprof)
marginsplot, ti("") xtitle("") ytitle("Bad Character Attributions") name(close1, replace)
svy: reg stdwayraise hprof#c.close i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
testparm c.close#hprof, equal
margins, at(close=(1(1)9)) by(hprof)
marginsplot, ti("") xtitle("") ytitle("Way Raised Attributions") name(close2, replace)
svy: reg stdimbalnce hprof#c.close i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
testparm c.close#hprof, equal
margins, at(close=(1(1)9)) by(hprof)
marginsplot, ti("") xtitle("") ytitle("Chemical Imbalance Attributions") name(close3, replace)
svy: reg stdgenetics hprof#c.close i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
testparm c.close#hprof, equal
margins, at(close=(1(1)9)) by(hprof)
marginsplot, ti("") xtitle("") ytitle("Genetic Attributions") name(close4, replace)

graph combine close1 close2 close3 close4, name(close_combined, replace)


*****************************
**#4 Hypothesis 2A+B
*****************************
svy: reg stdsocdistgss stdcharactr stdwayraise stdimbalnce stdgenetics hprof#know i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
predict p_socdist
test stdcharactr=stdwayraise
test stdgenetics=stdimbalnce
margins, at(stdcharactr=(-1 -.5 0 .5 1)) at(stdwayraise=(-1 -.5 0 .5 1)) at(stdimbalnce=(-1 -.5 0 .5 1)) at(stdgenetics=(-1 -.5 0 .5 1))
marginsplot, xdim(stdcharactr stdwayraise stdimbalnce stdgenetics, allsim nosep) recast(bar) xlabel(5.5 10.5 15.5, grid) ti("") plotopts(barw(.8)) name(fig2a, replace)

svy: reg stdsocdistgss hprof##know i.vigactive i.dx female i.racecat age ppeduc5 ppinc7

	
***************************************
**#5 Hypothesis 2C - SEM
***************************************

* hprof == 0 
xi: svy: sem (know -> stdcharactr stdwayraise stdimbalnce stdgenetics stdsocdistgss) ///
	(stdcharactr stdwayraise stdimbalnce stdgenetics -> stdsocdistgss) ///
	(vigactive i.dx female i.racecat age ppeduc5 ppinc7 -> know stdcharactr stdwayraise stdimbalnce stdgenetics stdsocdistgss) ///
	if hprof==0
	
	estat teffects, compact  
	// total effect of hprof on stdsocdistgss: -.0580774 (N.S.)
	// total indirect effect: -.0393042 (p=.001), also calculated by hand via the denominator in the nlcom below: 
	
	// indirect effect of each attribution:
	foreach var in stdcharactr stdwayraise stdimbalnce stdgenetics {
	
		disp "                     "
		disp "                     "
		disp "******** `var' ******"
		nlcom _b[stdsocdistgss:`var']*_b[`var':know] // indirect effect of each attribution
		
		nlcom (_b[stdsocdistgss:`var']*_b[`var':know])/ ///
			  (_b[stdsocdistgss:stdcharactr]*_b[stdcharactr:know] + ///
			  _b[stdsocdistgss:stdwayraise]*_b[stdwayraise:know] + ///
			  _b[stdsocdistgss:stdimbalnce]*_b[stdimbalnce:know] + ///
			  _b[stdsocdistgss:stdgenetics]*_b[stdgenetics:know]) // contribution of each mediator to the indirect effect 	  
	}	  
	
* hprof == 1
set pformat %5.4f
	xi: svy: sem (know -> stdcharactr stdwayraise stdimbalnce stdgenetics stdsocdistgss) ///
	(stdcharactr stdwayraise stdimbalnce stdgenetics -> stdsocdistgss) ///
	(vigactive i.dx female i.racecat age ppeduc5 ppinc7 -> know stdcharactr stdwayraise stdimbalnce stdgenetics stdsocdistgss) ///
	if hprof==1
	
	estat teffects, compact  
	// total effect of hprof on stdsocdistgss: -.0580774 (N.S.)
	// total indirect effect: -.0393042 (p=.001), also calculated by hand via the denominator in the nlcom below: 
	
	// indirect effect of each attribution:
	foreach var in stdcharactr stdwayraise stdimbalnce stdgenetics {
	
		disp "                     "
		disp "                     "
		disp "******** `var' ******"
		nlcom _b[stdsocdistgss:`var']*_b[`var':know] // indirect effect of each attribution
		
		nlcom (_b[stdsocdistgss:`var']*_b[`var':know])/ ///
			  (_b[stdsocdistgss:stdcharactr]*_b[stdcharactr:know] + ///
			  _b[stdsocdistgss:stdwayraise]*_b[stdwayraise:know] + ///
			  _b[stdsocdistgss:stdimbalnce]*_b[stdimbalnce:know] + ///
			  _b[stdsocdistgss:stdgenetics]*_b[stdgenetics:know]) // contribution of each mediator to the indirect effect 	  
	}	  


**************************************
**#6 Hypothesis 2C - Comparing Effects
***************************************
// SOCIAL DISTANCE		
eststo m1: svy: reg stdsocdistgss  know stdcharactr  vigactive i.dx female i.racecat age ppeduc5 ppinc7 stdwayraise stdimbalnce stdgenetics if hprof==0
eststo m2: svy: reg stdsocdistgss  know vigactive i.dx female i.racecat age ppeduc5 ppinc7 stdwayraise stdimbalnce stdgenetics if hprof==0 & e(sample)
suest m1 m2
lincom [m1]know - [m2]know

eststo m3: svy: reg stdsocdistgss  know stdcharactr  vigactive i.dx female i.racecat age ppeduc5 ppinc7 stdwayraise stdimbalnce stdgenetics if hprof==1
eststo m4: svy: reg stdsocdistgss  know vigactive i.dx female i.racecat age ppeduc5 ppinc7 stdwayraise stdimbalnce stdgenetics if hprof==1 & e(sample)
suest m3 m4
lincom [m3]know - [m4]know	  
			  
suest m1 m2 m3 m4
lincom ([m1]know - [m2]know) - ([m3]know - [m4]know) 
	// conditioning on the other 3 causal attributions, the change in effect size on "know" is significantly larger for hprofs (p<.05)
	
	
*****************************
**#7 Exploratory HCP Analysis
*****************************
gen hprof_grp = S2*hprof
recode hprof_grp (0=.)
recode hprof_grp (291120 = 1) (291140 292060=2) (292040=3) (291060 291170=4) (291050 =5) (291070 292020 319091 319092=6) (311010 312010=7) (291020 291190 =8)
lab def hprof_grp 1 "Therapists" 2 "Registered Nurse" 3 "EMTs" 4 "Physician & Nurse Practitioners" 5 "Pharmacists" 6 "Assistants" 7 "Health Aides" 8 "Other"
lab val hprof_grp hprof_grp

anova p_socdist hprof_grp
anova p_struct hprof_grp

gen EMT = hprof_grp
recode EMT (1 2 4 5 6 7 8 = 0) (3=1)
anova p_struct EMT##vigactive
margins EMT
anova p_socdist EMT##vigactive
margins EMT
generate hprof_noEMT = hprof
replace hprof_noEMT = 0 if hprof_grp ==3
svy: reg stdsocdistgss stdcharactr stdwayraise stdimbalnce stdgenetics hprof_noEMT#know i.vigactive i.dx female i.racecat age ppeduc5 ppinc
 
