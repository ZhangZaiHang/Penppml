cls
clear all
capture log close
cd "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\code\Simulation"
log using "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\code\Simulation\Sims_updated.log" , replace

program drop _all
program define tbs
syntax  [if] [in]
marksample touse  
capture lasso poisson y (z) x* if `touse', selection(plugin)
if _rc==0 { 
local regsbs `e(othervars_sel)'
global tots $tots `e(othervars_sel)'
gettoken regs1 : regsbs
if "`regs1'"=="x1" 	local bs=1 
else local bs=0 
mat bs=`bs'*I(1)
global ttots=$ttots+1
mat b$ttots=e(b)
global btots $btots b$ttots
}
end 

qui local rep = 1000
qui local od  = 0.3 
qui local beta = 0.2   
foreach obs in 250 1000 4000 { // 
qui local tph = 100+`obs'
foreach rho in  0.99 0.90 0.75 { // 0.75 0.90  0.99
foreach f in 5 10 20 { //
qui local p = 5*ceil(sqrt(`obs'))
qui local smpl= max(`tph',`rep')
qui set obs `smpl'

qui g cv=. if _n<=`rep'
qui g ad=. if _n<=`rep'
qui g pi=. if _n<=`rep'
qui g pii=. if _n<=`rep'
qui g bs=. if _n<=`rep'
qui g en=. if _n<=`rep'

qui g kcv=. if _n<=`rep'
qui g kad=. if _n<=`rep'
qui g kpi=. if _n<=`rep'
qui g kpii=. if _n<=`rep'
qui g kbs=. if _n<=`rep'
qui g ken=. if _n<=`rep'

qui g mcv=. if _n<=`rep'
qui g mad=. if _n<=`rep'
qui g mpi=. if _n<=`rep'
qui g mpii=. if _n<=`rep'
qui g mbs=. if _n<=`rep'
qui g men=. if _n<=`rep'

qui g lcv=. if _n<=`rep'
qui g lad=. if _n<=`rep'
qui g lpi=. if _n<=`rep'
qui g lpii=. if _n<=`rep'
qui g lbs=. if _n<=`rep'
qui g len=. if _n<=`rep'

qui g zero=. if _n<=`rep'
 
set seed 20022021
qui local f1=`f'+1

forvalues r = 1/`rep' {	
mat v=J(`f',`f',`rho')+(1-`rho')*I(`f')

drawnorm x1-x`f' , corr(v) cstorage(full) 
drawnorm x`f1'-x`p' , 
qui g z=rnormal()
*forvalues j = 1/`p' {	
    *qui replace x`j'=x`j'<invnormal(0.5) // if _n<=`obs'/2
	*qui replace x`j'=0 if _n>`obs'/2
*}

qui g y=(exp(1+`beta'*x1+z+`od'*rnormal())) if _n<=`tph'

*****  BS *****
global tots
global btots
global ttots=0
capture bs bs[1,1], rep(19): tbs if _n<=`obs'
if _rc==0 { 
mat _bbs=e(b)
mat bb_bbs=e(b_bs)
if _bbs[1,1]==1|bb_bbs[1,1]>0 qui replace bs=1 in `r'
else qui replace bs=0 in `r'
 
qui _rmcoll $tots if _n<=`obs', forcedrop
qui replace kbs=wordcount("`r(varlist)'") in `r'
capture ppml y `r(varlist)' z if _n<=`obs'
if _rc==0 { 
qui predict fit if _n<=`tph'&_n>`obs', mu 
qui g res=(y-fit)^2  
su res , meanonly
qui replace mbs=r(mean) in `r'
drop fit res
}

qui g double fit=0 if _n<=`tph'&_n>`obs'
local c_b=0
foreach betas in $btots {
local c_b=`c_b'+1	
qui mat score f=`betas' if _n<=`tph'&_n>`obs'
qui replace fit=fit+exp(f) if _n<=`tph'&_n>`obs'
drop f
}
qui g res=(y-fit/`c_b')^2  
su res, meanonly
qui replace lbs=r(mean) in `r'
drop fit res
macro drop tots ttots btots
}

*****  CV *****
capture lasso poisson y (z) x* if _n<=`obs'
if _rc==0 { 
qui replace kcv=e(k_nonzero_sel)-1 in `r'
if e(k_nonzero_sel)==1  qui replace cv=0 in `r'
else {
local regs `e(othervars_sel)'
gettoken regs1 : regs
if "`regs1'"=="x1" qui replace cv=1 in `r'
else  qui replace cv=0 in `r'
}
qui predict fit if _n<=`tph'&_n>`obs', n 
qui g res=(y-fit)^2
su res, meanonly
qui replace lcv=r(mean) in `r'
drop fit res
 
qui predict fit if _n<=`tph'&_n>`obs', n post 
qui g res=(y-fit)^2
su res, meanonly
qui replace mcv=r(mean) in `r'
drop fit res
}

*****  AL *****
capture lasso poisson y (z) x* if _n<=`obs', selection(adaptive)
if _rc==0 { 
qui replace kad=e(k_nonzero_sel)-1 in `r'
if e(k_nonzero_sel)==1 qui replace ad=0 in `r'
else {
local regsad `e(othervars_sel)'
gettoken regs2 : regsad
if "`regs2'"=="x1" qui replace ad=1 in `r'
else qui replace ad=0 in `r'
}
qui predict fit if _n<=`tph'&_n>`obs', n 
qui g res=(y-fit)^2 if _n<=`tph'&_n>`obs'
su res if _n<=`tph'&_n>`obs' , meanonly
qui replace lad=r(mean) in `r'
drop fit res

qui predict fit if _n<=`tph'&_n>`obs', n post
qui g res=(y-fit)^2 if _n<=`tph'&_n>`obs'
su res if _n<=`tph'&_n>`obs' , meanonly
qui replace mad=r(mean) in `r'
drop fit res
}

*****  EN *****

capture elasticnet poisson y (z) x* if _n<=`obs'
if _rc==0 { 
qui replace ken=e(k_nonzero_sel)-1 in `r'
if e(k_nonzero_sel)==1 qui replace en=0 in `r'
else {
local regsen `e(othervars_sel)'
gettoken regs2 : regsen
if "`regs2'"=="x1" qui replace en=1 in `r'
else qui replace en=0 in `r'
}
qui predict fit if _n<=`tph'&_n>`obs', n 
qui g res=(y-fit)^2 if _n<=`tph'&_n>`obs'
su res if _n<=`tph'&_n>`obs' , meanonly
qui replace len=r(mean) in `r'
drop fit res

qui predict fit if _n<=`tph'&_n>`obs', n post
qui g res=(y-fit)^2 if _n<=`tph'&_n>`obs'
su res if _n<=`tph'&_n>`obs' , meanonly
qui replace men=r(mean) in `r'
drop fit res
}

*****  PI *****
local skip=0
capture lasso poisson y (z) x* if _n<=`obs', selection(plugin)
if _rc==0 {
local regz `e(othervars_sel)'
qui predict fit if _n<=`tph'&_n>`obs', n 
qui g res=(y-fit)^2 if _n<=`tph'&_n>`obs'
su res if _n<=`tph'&_n>`obs' , meanonly
qui replace lpi=r(mean) in `r'
drop fit res

qui predict fit if _n<=`tph'&_n>`obs', n post 
qui g res=(y-fit)^2 if _n<=`tph'&_n>`obs'
su res if _n<=`tph'&_n>`obs' , meanonly
qui replace mpi=r(mean) in `r'
drop fit res

qui replace kpi=e(k_nonzero_sel)-1 in `r'
if e(k_nonzero_sel)==1  {
   qui replace kpii=0 in `r'   
   qui replace pi=0 in `r'
   qui replace pii=0 in `r' 
   qui replace mpii=mpi in `r'  
   qui replace lpii=lpi in `r' 
   qui replace zero=1 in `r'
}
else {
qui replace zero=0 in `r'
local regs `e(othervars_sel)'
gettoken regs1 : regs
if "`regs1'"=="x1" {
    qui replace pi=1 in `r'
	qui replace pii=1 in `r'
}
else {
    qui replace pi=0 in `r'
    qui replace pii=0 in `r' 
}

*****  IL *****
qui vl create xvars = (x*)
qui vl drop (`regz'), user
     qui local regsx `regz'
foreach v in `regs' {
 capture lasso linear `v' $xvars if _n<=`obs', selection(plugin, heteroskedastic)
 if _rc!=0 local skip=1
 qui local regsx `regsx' `e(allvars_sel)'
 qui local regsi `e(allvars_sel)'
 gettoken regs1i : regsi
 if "`regs1i'"=="x1" qui replace pii=1 in `r'
}
	qui _rmcoll `regsx' if _n<=`obs', forcedrop
	local regz `r(varlist)'
	qui replace kpii=wordcount("`r(varlist)'") in `r'
qui vl clear, user

capture ppml y `regz' z if _n<=`obs'
if _rc==0 { 
qui predict fit if _n<=`tph'&_n>`obs', mu 
qui g res=(y-fit)^2 if _n<=`tph'&_n>`obs'
su res if _n<=`tph'&_n>`obs', meanonly
qui replace mpii=r(mean) in `r'
drop fit res
}

capture lasso poisson y (z) `regz'  if _n<=`obs', selection(plugin)
if _rc==0 { 
qui predict fit if _n<=`tph'&_n>`obs'
qui g res=(y-fit)^2 if _n<=`tph'&_n>`obs'
su res if _n<=`tph'&_n>`obs', meanonly
qui replace lpii=r(mean) in `r'
drop fit res
}

if `skip'==1 {
    qui replace kpii=. in `r'
	qui replace pii=. in `r'
	qui replace mpii=. in `r'  
    qui replace lpii=. in `r'
}
}
}
drop y x* z
}
di
di  "P = " `p' ", OD = " `od' ", Rho = " `rho' ", N = " `obs' ", Beta = " `beta'  ", f = " `f'
 
su cv  ad  pi  bs  pii  en if _n<=`rep'&cv!=.&ad!=.&pi!=.&pii!=.&en!=.
su kcv kad kpi kbs kpii ken if _n<=`rep'&kcv!=.&kad!=.&kpi!=.&kpii!=.&ken!=.
su lcv lad lpi lbs lpii len if _n<=`rep'&lcv!=.&lad!=.&lpi!=.&lpii!=.&len!=.
su mcv mad mpi mbs mpii men if _n<=`rep'&mcv!=.&mad!=.&mpi!=.&mpii!=.&men!=.
su zero
drop cv* ad* pi* kcv kad kpi* mcv mad mpi* lcv lad lpi* zero bs kbs mbs lbs en ken men len
}
}
}
capture log close
