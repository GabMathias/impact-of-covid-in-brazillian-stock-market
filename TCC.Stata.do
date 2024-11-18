*Regression using STATA.

import data 
import excel "C:\Users\20182CE005\Desktop\TCC.stata.xlsx", sheet("Sheet 1") firstrow

*transform string variables to categorical 
encode Subsector_B3, gen(sector)

*Create Dummy Sector variables
tabulate sector, gen(s)

* set as panel data
xtset ID date

*Pooled OLS estimator
regress DR L.LMCAP L.MTB L.DGTCC

*multicollinearity test ...there is no multicollinearity
correl DR LMCAP MTB DGTCC DGTDC
estat vif

* Population-averaged estimator
xtreg DR L.LMCAP L.MTB L.DGTCC, pa
 
* Between estimator
xtreg DR L.LMCAP L.MTB L.DGTCC, be

* Fixed effects or within estimator
xtreg DR L.LMCAP L.MTB L.DGTCC, fe

* First-differences estimator
reg D.(DR L.LMCAP L.MTB L.DGTCC), noconstant

* Random effects estimator
xtreg DR L.LMCAP L.MTB L.DGTCC, re theta

* Breusch-Pagan LM test for random effects versus OLS  ... p-value < 0.05, random effects model is a better choice
*there is evidence of significant differences across firms(heteroscedasticity), therefore I can't run a simple OLS *regression.
quietly xtreg DR L.LMCAP L.MTB L.DGTCC, re
xttest0

* Hausman test for fixed versus random effects model  ...p-value < 0.05, fixed effects model is a better choice.
*there is a correlation between the effects and the regressors and, consequently, the estimators of the random *effects	model will not be consistent
quietly xtreg DR L.LMCAP L.MTB L.DGTCC, fe
estimates store fixed
quietly xtreg DR L.LMCAP L.MTB L.DGTCC, re
estimates store random
hausman fixed random

* Recovering individual-specific effects
quietly xtreg DR L.LMCAP L.MTB L.DGTCC, fe
predict alphafehat, u
sum alphafehat

*Autocorrelation test
xtserial DR LMCAP MTB DGTCC

*Cross-section dependence
xtpcse DR L.LMCAP L.MTB L.DGTCC, pairwise corr(ar1)
xtpcse DR L.LMCAP L.MTB L.DGTCC s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31 s32 s33 s34 s35 s36 s37 s38, pairwise corr(ar1)
 
 * Correction of serial autocorrelation 
 xtregar DR L.LMCAP L.MTB L.DGTCC, fe rhotype(tscorr)
 xtregar DR L.LMCAP L.MTB L.DGTDC, fe rhotype(tscorr)
 