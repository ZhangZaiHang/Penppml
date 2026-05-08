use "temp_trade_only.dta"  , clear
merge m:m iso1 iso2 year using "temp_provisions_largedataset_essential_Jan302021.dta" 
drop if _merge==2
drop _merge

vl clear
qui vl create x = (*_prov_*)
foreach v in $x {
	qui replace `v' = 0 if missing(`v')
}

* Generate RTA dummy and drop obsolete agreements
egen provcount = rowtotal($x)
gen  rta = provcount>1 if !missing(provcount)
gen excluded = rta == 0 & fta_eia == 1
drop if excluded

* Generate clustering variables
egen exp_id = group(iso1)
egen imp_id = group(iso2)
egen pair = group(iso1 iso2)
replace id = 0 if missing(id)
replace id = 0 if rta==0
  * CL is the clustering variable for post lasso
  sort pair, stable
  by pair: egen cl=max(id)
  replace cl=1000000+pair if cl==0



global plugin = "ad_prov_14 cp_prov_23 tbt_prov_02 tbt_prov_29 tbt_prov_07 tbt_prov_08 tbt_prov_33 tf_prov_45"


***** ICEBERG LASSO
drop if rta==0 // Otherwise we predict zeros with zeros

* Drop provisions that are always zero
local zeros = ""
foreach t in $x {
qui su `t'
	if r(sd)==0 local zeros = "`zeros'" + " `t'"
}
su `zeros'
qui vl drop (`zeros'), user

* Generate list of provisions not selected by PI lasso
qui vl drop ($plugin), user

* Iceberg loop
local iceberg = ""
foreach v in $plugin {
	di
	di " ************************************************* "
	di "    **************  `v'  **************    "
	di " ************************************************* "
	di
    rlasso `v' $x , robust cluster(id) maxpsiiter(15)    
	qui corr `v' `e(selected)' 
	matrix A = r(C)
	matrix Correlations = A[2..colsof(A), 1]
    mat list Correlations
	di
	local iceberg = "`iceberg' " + "`e(selected)'"
	reg `v' `e(selected)', vce(cluster cl) 
}

local iceberg : list sort iceberg
local iceberg : list uniq local(iceberg)

* All variables selected
local full_iceberg = " $plugin `iceberg'"
local full_iceberg : list sort full_iceberg
di "`full_iceberg'"

local wc=wordcount("`full_iceberg'")
di as error "Number of variables selected = " `wc' 

* Check if variables selected by IL are collinear with variables not selected
_rmcoll `full_iceberg', forcedrop
local il_rmcoll `r(varlist)'
local wc=wordcount("`il_rmcoll'")
vl clear
qui vl create x1 = (*_prov_*)
qui vl drop (`il_rmcoll'), user

foreach v in `il_rmcoll' {
	foreach vv in $x1 {
	qui corr `v' `vv'
	if r(rho)>0.975&r(rho)!=. {
		di "`v'   `vv'  "  r(rho)
		if r(rho)>1-1e-6 local wc = `wc'+1
		}
	}
}
di as error "Total number of variables selected by IL = " `wc' 
