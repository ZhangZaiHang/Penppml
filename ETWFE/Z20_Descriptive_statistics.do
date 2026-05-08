******************************************************************************************************************************
******************************************************************************************************************************
***
*** Table 3-2: Observations along different dimensions
*** 
******************************************************************************************************************************
******************************************************************************************************************************

//////////////////////////////////////////////////
/// I) Observations along different dimensions ///
//////////////////////////////////////////////////

use ${usr_data}\data_sdid_medium, clear

// replace year = year + 3
keep if year >= first_treat

bysort first_treat: gen obs = _N
bysort first_treat: egen pairs = nvals(id_ci_cj)
bysort first_treat: egen exps = nvals(id_ci)
bysort first_treat: egen imps = nvals(id_cj)

duplicates drop first_treat , force
keep first_treat obs pairs exps imps
list

/////////////////////////////////////////////////////////////////
/// II) Summary statistics of covariates for different groups ///
/////////////////////////////////////////////////////////////////

use ${usr_data}\data_sdid_medium, clear

replace DIST = exp(DIST)
collapse (mean) DIST CNTG LANG CLNY , by( first_treat )

list