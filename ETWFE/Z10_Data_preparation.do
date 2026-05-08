******************************************************************************************************************************
******************************************************************************************************************************
***
*** Data preparation file
*** Written by: Zaihang Zhang
***
******************************************************************************************************************************
******************************************************************************************************************************

*************************************************************************
*1. Start with CEPII Data, which will be used for the gravity variables* 
*https://www.cepii.fr/DATA_DOWNLOAD/gravity/data/Gravity_dta_V202211.zip*
*************************************************************************
************************
*a. Combine CEPII files*
************************
use "C:\Users\ZaihangZhang\Desktop\MachineLearninginAgriculturalTradeResearch\农业贸易数据\Gravity_V202211.dta", clear

******************************************************
*b. Create some additional variables and label others*
******************************************************
rename iso3_o exporter
rename iso3_d importer
gen DIST=ln(dist)
rename contig CNTG
rename comlang_off LANG
rename comcol CLNY

gen pair = exporter + importer
egen id_pair = group(pair)

bysort id_pair : fillmissing DIST , with(previous)
bysort id_pair : fillmissing DIST , with(next)
bysort id_pair : fillmissing CNTG , with(previous)
bysort id_pair : fillmissing CNTG , with(next)
bysort id_pair : fillmissing LANG , with(previous)
bysort id_pair : fillmissing LANG , with(next)
bysort id_pair : fillmissing CLNY , with(previous)
bysort id_pair : fillmissing CLNY , with(next)

drop pair id_pair
// rename member_eu_joint EU_DGD
// egen WTO_DGD=rowmax(member_gatt_joint member_wto_joint)
// egen RTA_DGD=rowmax(agree_pta_goods agree_pta_services agree_fta agree_eia agree_cu agree_psa agree_fta_eia agree_cu_eia agree_pta)
// rename agree_pta_goods PTA_GOODS_DGD
// rename agree_pta_services PTA_SERVS_DGD
// rename agree_fta FTA_DGD
// rename agree_eia EIA_DGD
// rename agree_cu CU_DGD
// rename agree_psa PSA_DGD
// rename agree_pta PTA_DGD
// rename member_eu_o eu_exp
// rename member_eu_d eu_imp
**************************************************************
*c. Adjust some names to facilitate combining with other data*
**************************************************************
replace exporter="BUR" if exporter=="MMR" & year<1989
replace importer="BUR" if importer=="MMR" & year<1989
duplicates drop year exporter importer, force
*********************************
*NOTE: EUN appears as duplicates*
*********************************
drop if exporter=="EUN" | importer=="EUN"
duplicates list year exporter importer
*****************************
*d. Keep only some variables*
*****************************
keep exporter importer year DIST CNTG LANG CLNY gdp_d gdp_o
compress
save ${tmp_data}\cepii_sdid, replace

**********************************************************************
*2. Start with DTA Data, which will be used for the RTA variables* 
*https://datacatalogfiles.worldbank.org/ddh-published/0065624/DR0093615/DTA%201.0%20-%20Horizontal%20Content%20(v2).xlsx*
**********************************************************************
**********************
*a. Combine RTA files*
**********************
cd "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\深度数据"

	do 处理深度数据.do
******************************************************
*b. Create some additional variables and label others*
******************************************************
	use "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\深度数据\rta深度1958-2023Hofmann.dta", clear
	keep iso1 iso2 year id entry_year rta norm_rta_depth
	replace id = 0 if id == .
	rename rta RTA_DTA
	rename iso1 exporter
	rename iso2 importer

**************************************************************
*c. Adjust some names to facilitate combining with other data*
**************************************************************
	replace exporter="BUR" if exporter=="MMR" & year<1989
	replace importer="BUR" if importer=="MMR" & year<1989
	duplicates list year exporter importer
*********************************
*NOTE: EUN appears as duplicates*
*********************************
	drop if exporter=="EUN" | importer=="EUN"
	duplicates list year exporter importer
*****************************
*d. Keep only some variables*
*****************************
	save "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\农业贸易数据\dta_sdid.dta", replace

*********************************************************************
*3. Prepare agricultural trade data from UNITED STATES INTERNATIONAL TRADE COMMISSION. Download on June-18-25*
* https://www.usitc.gov/data/gravity/itpde.htm*
*********************************************************************
cd "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\农业贸易数据"

	clear all

	import delimited "C:\Users\ZaihangZhang\Desktop\ITPDE_R03.csv"
	keep if broad_sector == "Agriculture"
	drop industry_id flag_mirror industry_descr broad_sector importer_name importer_iso3_dynamic exporter_name exporter_iso3_dynamic flag_zero
	save "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\农业贸易数据\ITPDE_R03_AGRICULTURE_1986_2022", replace
	
	use "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\农业贸易数据\ITPDE_R03_AGRICULTURE_1986_2022.dta", clear
	tostring year, replace
	gen pd = exporter_iso3 + importer_iso3 + year
	collapse (sum) trade, by(pd)
	gen exporter = substr(pd, 1,3)
	gen importer = substr(pd, 4,3)
	gen year = substr(pd, 7,4)
	destring year, replace
	order year exporter importer trade 
	drop pd 
	replace importer="ROU" if importer=="ROM"
	replace exporter="ROU" if exporter=="ROM"
	
	egen id_ci_cj = group(exporter importer)
	xtset id_ci_cj year 
	sort id_ci_cj year
	tsfill, full
	
	bysort id_ci_cj : fillmissing exporter importer , with(previous)
	drop if missing(exporter)
	drop id_ci_cj
	save "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\农业贸易数据\trade_itpde", replace

*********************
*4. Combine the data*
*********************
******************************
*a. Start with the trade data*
******************************
	use "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\农业贸易数据\trade_itpde", clear //农业贸易总量，百万美元(1986-2022)
	replace trade = 1000000*trade
	drop if year < 1991 | year > 2021
	
// 	use "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\农业贸易数据\trade_agri_baci.dta", clear //农业贸易分产品，千美元(1996-2023)
// 	replace value = 1000*value
// 	tostring year, replace
// 	gen ID = exporter + importer + year
// 	bys ID : egen trade = sum(value)
// 	destring year, replace
// 	duplicates drop ID, force
// 	drop ID quantity value product
// 	save "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\农业贸易数据\trade_agri_baci_sum.dta", replace
//	
// 	use "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\农业贸易数据\trade_agri_baci_sum", clear 
**************************
*b. Add gravity variables*
**************************
// 	merge 1:1 exporter importer year using "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\农业贸易数据\dta_sdid.dta"
	
// 	joinby exporter importer year using ${tmp_data}\dgd_sdid, unmatched(master) //DGD数据库，包含RTA签订时间和其他引力变量数据
// 	tab _m
// 	drop _m
	
	joinby exporter importer year using "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\农业贸易数据\dta_sdid.dta", unmatched(master) //DTA深度数据库 没有DGD数据完整 需要深度数据时可考虑
	tab _m
	drop _m
	
// 	joinby exporter importer year using "C:\Users\ZaihangZhang\Desktop\MachineLearninginAgriculturalTradeResearch\data\tmp\cepii_sdid.dta", unmatched(master) //引力变量
// 	tab _m
// 	drop _m
	
// 	gen pair = exporter + importer
// 	egen id_pair = group(pair)
	
// 	forvalues i = 1(1)52 {
// 		fillmissing a`i'
// 	}
	
// 	bysort id_pair : fillmissing id , with(next)
//	
// 	bysort id_pair : fillmissing DIST , with(previous)
// 	bysort id_pair : fillmissing DIST , with(next)
// 	bysort id_pair : fillmissing CNTG , with(previous)
// 	bysort id_pair : fillmissing CNTG , with(next)
// 	bysort id_pair : fillmissing LANG , with(previous)
// 	bysort id_pair : fillmissing LANG , with(next)
// 	bysort id_pair : fillmissing CLNY , with(previous)
// 	bysort id_pair : fillmissing CLNY , with(next)
	
// 	replace entry_year = 0 if entry_year == .
	replace RTA_DTA = 0 if RTA_DTA == .
	replace norm_rta_depth = 0 if norm_rta_depth == .
	replace id = 0 if id == .
	drop entry_year
// 	sort id_pair
// 	drop pair id_pair
******************************************************************************************
*c. Make sure all gravity covariates, except distance, are set to zero for domestic trade*
******************************************************************************************
// 	drop if exporter == importer

// 	local gravvars "CNTG LANG CLNY EU WTO RTA_DGD RTA_DTA FTA" 
// 	foreach x of local gravvars{
// 	replace `x'=0 if exporter == importer
// 	}
*************************
*d. Check for duplicates*
*************************
	duplicates list year exporter importer
*************************************
*e. Create some additional variables*
*************************************
	egen idt_ci = group(exporter year)
	egen idt_cj = group(importer year)
	egen id_ci_cj = group(exporter importer)
	
	compress
	save ${usr_data}\data_large, replace
**************************
*f. Create medium sample*
**************************	
	use ${usr_data}\data_large.dta, clear
	keep if exporter!=importer
	collapse (sum) trade, by(exporter)
	sort trade
	gen sum_trade_cum=sum(trade)
	egen sum_trade_all=sum(trade)
	gen ratio= sum_trade_cum/sum_trade_all
	drop if ratio<0.01
	keep exporter
	save ${tmp_data}\exp_small.dta, replace
	rename exporter importer
	save ${tmp_data}\imp_small.dta, replace
	use ${usr_data}\data_large.dta, clear
	joinby importer using ${tmp_data}\imp_small.dta
	joinby exporter using ${tmp_data}\exp_small.dta
	keep year exporter importer trade id RTA_DTA norm_rta_depth idt_ci idt_cj id_ci_cj
	save ${usr_data}\data_medium.dta, replace
**************************
*g. Create small sample*
**************************
	use ${usr_data}\data_large.dta, clear
	keep if exporter!=importer
	collapse (sum) trade, by(exporter)
	sort trade
	gen sum_trade_cum=sum(trade)
	egen sum_trade_all=sum(trade)
	gen ratio= sum_trade_cum/sum_trade_all
	drop if ratio<0.02
	keep exporter
	save ${tmp_data}\exp_small.dta, replace
	rename exporter importer
	save ${tmp_data}\imp_small.dta, replace
	use ${usr_data}\data_large.dta, clear
	joinby importer using ${tmp_data}\imp_small.dta
	joinby exporter using ${tmp_data}\exp_small.dta
	keep year exporter importer trade id RTA_DTA norm_rta_depth idt_ci idt_cj id_ci_cj
	save ${usr_data}\data_small.dta, replace
*********************************************************
*5. Add additional data sources and transform variables *
*********************************************************
// loop over different data set sizes
foreach dtaset in small medium large {

	// load trade data
	use ${usr_data}\data_`dtaset'.dta, clear
	
	* preliminary variable definitions
	rename RTA_DTA RTA
	egen id_ci = group(exporter)
	egen id_cj = group(importer)

	* set panel IDs
	xtset id_ci_cj year, yearly

	* determine treatment cohorts
	egen first_treat=csgvar(RTA), tvar(year) ivar(id_ci_cj)

	* shift RTA/WTO 3 years forward for anticipation effects
// 	gen RTA_3 = F3.RTA
// 	egen first_treat_3 = csgvar(RTA_3), tvar(year) ivar(id_ci_cj)
// 	gen WTO_3 = F3.WTO
	
	* define exclude dummy for three anticipation years
// 	gen exclude = (first_treat-1==year | first_treat-2==year | first_treat-3==year)
	
	// drop always-treated units
// 	drop if first_treat_3 == 1986
// 	drop if first_treat == 1989

	// drop exits
	sort exporter importer year
	gen D_RTA = 0
	replace D_RTA = RTA-RTA[_n-1] if id_ci_cj==id_ci_cj[_n-1]
	egen exit = min(D_RTA), by(id_ci_cj)
	drop if exit==-1
	drop exit D_RTA

	// define 5-year interval
// 	gen int_5yr = mod(year,5)
	
	// define border-time ID
// 	gen brdr=1 if exporter!=importer
// 	replace brdr=0 if brdr==.
// 	egen brdr_time=group(brdr year)
	
	// define variables
// 	gen lnDIST = ln(DIST)
// 	gen ln_trade = ln(trade)

	// save data set
	save ${usr_data}\data_sdid_`dtaset'.dta, replace

}


****************************
*6. Delete temporary files *
****************************
erase ${tmp_data}\trade_itpde.dta
erase ${tmp_data}\dta_sdid.dta
erase ${tmp_data}\cepii_sdid.dta
erase ${tmp_data}\exp_small.dta
erase ${tmp_data}\imp_small.dta	
	