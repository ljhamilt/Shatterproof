****Project: Addiction Stigma
****Author:  Lucas Hamilton
****Date:    2022/04/30
****Purpose: Code for paper on HCP and Gen Pop within 2021 Shatterproof Addiction Stigma Index


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

** Treatment effectiveness from GSS (did not recode, scale as it stands is 1=very likely, 4 = not at all likely; drop refusals)
clonevar imprvtrt=Q10_IMPRVTRT
clonevar hlthylife=Q11_HLTHYLIFE
clonevar prformjob=Q12_PRFORMJOB
        
recode imprvtrt-prformjob (-1=.)
fre Q10_IMPRVTRT-Q12_PRFORMJOB imprvtrt-prformjob

factor imprvtrt-prformjob
alpha imprvtrt-prformjob

egen untreatable=rowmean(imprvtrt-prformjob)
lab var untreatable "Addiction is untreatable, HI=more stigma"

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

* Traditional prejudice (unpredictable, violent to others, violent to self, trustworthy, competent; recode so higher number = more prejudice)
clonevar unpredict=Q21_MHUNSURE
clonevar hurtoth=Q22_HURTOTH
clonevar hurtself=Q23_HURTSELF
clonevar trust=Q24_MHTRUST
clonevar competent=Q25_MHCOMP

recode unpredict-competent (-1=.)
recode unpredict hurtoth hurtself (1=4)(2=3)(3=2)(1=4)
lab def agree 1 "Strongly disagree" 4 "Strongly agree"
label val unpredict hurtoth hurtself agree

fre Q21_MHUNSURE-Q25_MHCOMP unpredict-competent 
factor unpredict-competent
alpha unpredict-competent
egen prejudice=rowmean(unpredict-competent)
lab var prejudice "Traditional prejudice scale, hi=more prejudice"

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

gen vigmed=xvignettes
recode vigmed (1=1)(6=1)(5=0)(10=0)(2=.)(3=.)(4=.)(7=.)(8=.)(9=.)
lab var vigmed "Vignette - 1=Medical prescription onset, 0=Party prescription onset"

gen vigrx=xvignettes
recode vigrx (2=0)(5=1)(7=0)(10=1)(1=1)(3=.)(4=.)(6=1)(7=.)(8=.)(9=.)
lab var vigrx "Vignette - 1= Rx opioids, 0=Heroin"

gen oud=xvignettes
 recode oud (1=1)(2=1)(3=0)(4=0)(5=1)(6=1)(7=1)(8=0)(9=0)(10=1)
lab var oud " Vignette - Any opioid addiction"

gen poud=xvignettes
recode poud (1=1)(2=0)(3=0)(4=0)(5=1)(6=1)(7=0)(8=0)(9=0)(10=1)
lab var poud " Vignette - Prescription opioid addiction"

gen hud=xvignettes
recode hud (1=0)(2=1)(3=0)(4=0)(5=0)(6=0)(7=1)(8=0)(9=0)(10=0)
lab var hud " Vignette - Heroin addiction"

gen mud=xvignettes
recode mud (1=0)(2=0)(3=1)(4=0)(5=0)(6=0)(7=0)(8=1)(9=0)(10=0)
lab var mud "Vignette - Methamphetamine addiction"

gen aud=xvignettes
recode aud (1=0)(2=0)(3=0)(4=1)(5=0)(6=0)(7=0)(8=0)(9=1)(10=0)
lab var aud "Vignette - Alcohol addiction"

gen poudmed=xvignettes
recode poudmed (1=1)(2=0)(3=0)(4=0)(5=0)(6=1)(7=0)(8=0)(9=0)(10=0)
lab var poudmed " Vignette - Prescription opioid addiction, pain"

gen poudparty=xvignettes
recode poudparty (1=0)(2=0)(3=0)(4=0)(5=1)(6=0)(7=0)(8=0)(9=0)(10=1)
lab var poudparty " Vignette - Prescription opioid addiction, party"

gen dx=.
replace dx=1 if aud==1
replace dx=2 if poud==1
replace dx=3 if hud==1
replace dx=4 if mud==1
lab def dx 1 "Alcohol" 2 "Rx Opioids" 3 "Heroin" 4 "Meth"
lab val dx dx
lab var dx "Vignette diagnosis"
tab xvignette, gen(vig)

lab var vig1 "Vignette - Rx opioids, active, medical onset"
lab var vig2 "Vignette - Heroin, active"
lab var vig3 "Vignette - Meth, active"
lab var vig4 "Vignette - Alcohol, active"
lab var vig5 "Vignette - Rx opioids, active, party onset"
lab var vig6 "Vignette - Rx opioids, MOUD recovery, medical onset"
lab var vig7 "Vignette - Heroin, recovery"
lab var vig8 "Vignette - Meth, recovery"
lab var vig9 "Vignette - Alcohol, recovery"
lab var vig10 "Vignette - Rx opioids, unspecified recovery, medical onset"

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
**#2 Code for paper
*****************************

tabulate S2
tabulate S2 if hprof > 0
tabulate (ppinc7) (hprof)
tabulate (ppeduc5) (hprof)
tabulate (race) (hprof)
tabulate (female) (hprof)
by hprof, sort : summarize age
tabulate hprof know, chi2
ttest close if know > 0, by(hprof)

gen wt=Genpop_wt
replace wt=HCP_wt if xsud==3
replace wt=ct_wt if xsud==2
replace wt=SUD_wt if xsud==4 
lab var wt "Sampling weights for combining general population with oversamples"

foreach x in charactr wayraise imbalnce genetics socdistgss prejudice untreatable structural {
	egen std`x' = std(`x')
}

svyset wt

*****************************
**#3 Hypothesis 1
*****************************
svy: reg stdcharactr hprof#know i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
margins know#hprof
marginsplot, xdim(hprof) recast(bar) by(know) ti("") ytitle("Bad Character Attributions") ylabel(-.4 (.2) .4) xtitle("") xlabel() plotop(barw(.8) fintensity(inten30)) ciop(msize(vlarge) lw(medthick)) name(fig1a, replace)

svy: reg stdwayraise hprof#know i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
margins know#hprof
marginsplot, xdim(hprof) recast(bar) by(know) ti("") ytitle("Way Raised Attributions") ylabel(-.4 (.2) .4) xtitle("") xlabel() plotop(barw(.8) fintensity(inten30)) ciop(msize(vlarge) lw(medthick)) name(fig1b, replace)

svy: reg stdimbalnce hprof#know i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
margins know#hprof
marginsplot, xdim(hprof) recast(bar) by(know) ti("") ytitle("Chemical Imbalance Attributions") ylabel(-.4 (.2) .4) xtitle("") xlabel() plotop(barw(.8) fintensity(inten30)) ciop(msize(vlarge) lw(medthick)) name(fig1c, replace)

svy: reg stdgenetics hprof#know i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
margins know#hprof
marginsplot, xdim(hprof) recast(bar) by(know) ti("") ytitle("Genetic Attributions") ylabel(-.4 (.2) .4) xtitle("") xlabel() plotop(barw(.8) fintensity(inten30)) ciop(msize(vlarge) lw(medthick)) name(fig1d, replace)

*create composite Figure 1
graph combine fig1a fig1b fig1c fig1d, row(2) col(2)
graph export "\\Client\F$\krendl lab\Lucas\Stigma & SUDs\Healthcare Professionals\Figure 1.png", as(png) name("Graph")


*****************************
**#4 Hypothesis 2 & 3
*****************************
svy: reg stdsocdistgss stdcharactr stdwayraise stdimbalnce stdgenetics hprof##know i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
test (1.a=2.a) (1.a=3.a), mtest(sidak)
margins know#hprof
marginsplot, xdim(hprof know) recast(bar)
margins, at(stdcharactr=(-1 -.5 0 .5 1)) at(stdwayraise=(-1 -.5 0 .5 1)) at(stdimbalnce=(-1 -.5 0 .5 1)) at(stdgenetics=(-1 -.5 0 .5 1))
marginsplot, xdim(stdcharactr stdwayraise stdimbalnce stdgenetics, allsim nosep) recast(bar) xlabel(5.5 10.5 15.5, grid) ti("") plotopts(barw(.8)) name(fig2a, replace)

svy: reg stdprejudice stdcharactr stdwayraise stdimbalnce stdgenetics hprof know i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
margins, at(stdcharactr=(-1 -.5 0 .5 1)) at(stdwayraise=(-1 -.5 0 .5 1)) at(stdimbalnce=(-1 -.5 0 .5 1)) at(stdgenetics=(-1 -.5 0 .5 1))
marginsplot, xdim(stdcharactr stdwayraise stdimbalnce stdgenetics, allsim nosep) recast(bar) xlabel(5.5 10.5 15.5, grid) ti("") plotopts(barw(.8)) name(fig2b, replace)

svy: reg stduntreatable stdcharactr stdwayraise stdimbalnce stdgenetics hprof know i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
margins, at(stdcharactr=(-1 -.5 0 .5 1)) at(stdwayraise=(-1 -.5 0 .5 1)) at(stdimbalnce=(-1 -.5 0 .5 1)) at(stdgenetics=(-1 -.5 0 .5 1))
marginsplot, xdim(stdcharactr stdwayraise stdimbalnce stdgenetics, allsim nosep) recast(bar) xlabel(5.5 10.5 15.5, grid) ti("") plotopts(barw(.8))  name(fig2c, replace)

svy: reg stdstructural stdcharactr stdwayraise stdimbalnce stdgenetics hprof know i.vigactive i.dx female i.racecat age ppeduc5 ppinc7
margins, at(stdcharactr=(-1 -.5 0 .5 1)) at(stdwayraise=(-1 -.5 0 .5 1)) at(stdimbalnce=(-1 -.5 0 .5 1)) at(stdgenetics=(-1 -.5 0 .5 1))
marginsplot, xdim(stdcharactr stdwayraise stdimbalnce stdgenetics, allsim nosep) recast(bar) xlabel(5.5 10.5 15.5, grid) ti("") plotopts(barw(.8))  name(fig2d, replace)
margins know#hprof
marginsplot, xdim(hprof know) recast(bar)

**create composite Figure 2
graph combine fig2a fig2b fig2c fig2d, row(2) col(2)
