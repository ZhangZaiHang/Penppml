******************************************************************************************************************************
******************************************************************************************************************************
***
*** Table 4-6: Descriptive Statistics of the Regression Variable Coefficients(Ln_MSE)
*** 
******************************************************************************************************************************
******************************************************************************************************************************

**#Out of sample MSE
** (1) All variables
	use ${usr_data}\data_sdid_medium, clear
	drop if trade == .
	bys id_ci_cj: gen train = runiform()<0.7

	forvalues i = 1991/2021 {
		g f`i' = year == `i'
	}
	
	egen cohort = sum(RTA), by(id_ci_cj)
	forvalues i = 1991/2021 {
		g cohort`i' = cohort == 2022 - `i'
		qui replace cohort`i' = 0 if cohort`i' == .
	}
	
	qui compress
	
	local ppml ""
	forvalues c = 1991/2021 {
		forvalues y = 1991/2021 {
			if `y' >= `c' {
			local ppml "`ppml' c.cohort`c'#c.f`y'"
			}
		}
	}

	ppmlhdfe trade `ppml' if train == 1, abs(idt_ci idt_cj id_ci_cj) cluster(id_ci_cj) d
	
	bysort id_ci_cj: egen mean_ppmlhdfe_d = mean(_ppmlhdfe_d)
	replace _ppmlhdfe_d = mean_ppmlhdfe_d if missing(_ppmlhdfe_d)
	drop mean_ppmlhdfe_d
	
	predict yhat if train == 0, mu
	gen mse = (trade-yhat)^2 if train == 0
	sum mse
	scalar mse_mean = r(mean)
	gen mean = ln(mse_mean)
	
	estimates save "${usr_data}\1991_2021.ster", replace
	
** (2) Plug-in lasso
	use "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\data\usr\data_sdid_medium_DTA_1991_2021.dta", clear
	drop if trade == .
	egen idt_ci = group(exporter year)
	egen idt_cj = group(importer year)
	egen id_ci_cj = group(exporter importer)
	bys id_ci_cj: gen train = runiform()<0.7

	ppmlhdfe trade cohort1991f1991 cohort1991f1992 cohort2004f2004 cohort2004f2009 cohort2004f2010 cohort2004f2011 cohort2004f2012 cohort2004f2013 cohort2004f2014 cohort2004f2015 cohort2004f2016 cohort2004f2017 cohort2007f2017 if train == 1, abs(idt_ci idt_cj id_ci_cj) cluster(id_ci_cj) d
	
	bysort id_ci_cj: egen mean_ppmlhdfe_d = mean(_ppmlhdfe_d)
	replace _ppmlhdfe_d = mean_ppmlhdfe_d if missing(_ppmlhdfe_d)
	drop mean_ppmlhdfe_d
	
	predict yhat if train == 0, mu
	gen mse = (trade-yhat)^2 if train == 0
	sum mse
	scalar mse_mean = r(mean)
	gen mean = ln(mse_mean)
	
	estimates save "${usr_data}\1991_2021_postlasso.ster",replace
	
	
	
	