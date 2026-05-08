*! v1.1 FRA 8/5/2022 Redef not yet treated. 
*! v1   FRA 8/5/2022 Has almost everything we need
program jwdidstacked, eclass
	version 14
	syntax varlist [if] [in] [pw], Ivar(varname) Tvar(varname) Gvar(varname) [never group method(name) Fe(string) exovar(string) addint(varlist) trendvar(varlist) trendt trendg trendij restriction(name) binT(numlist min=1 max=1 >0 integer) interval(numlist min=1 max=1 >1 integer) binpre(numlist min=1 max=1 <0 integer) ifcond(varname)] [cvar(string)]
	marksample  touse
	markout    `touse' `ivar' `tvar' `gvar'
	gettoken y x:varlist 
	
	** Count gvar
	/*qui:count if `gvar'==0 & `touse'==1 
	if `r(N)'==0 {
		*qui:sum `gvar' if `touse'==1 , meanonly
		
	}*/
	** Take out of sample units that have always been treated.
	tempvar tvar2
	qui:bysort `touse' `ivar': egen long `tvar2'=min(`tvar')
	qui:replace `touse'=0 if `touse'==1 & `tvar2'>=`gvar' & `gvar'!=0 & `tvar'>=`gvar'
	** If no never treated
	qui:count if `gvar'==0 & `touse'==1 
	if `r(N)'==0 {
		qui:sum `gvar' if `touse'==1 , meanonly
		qui:replace `touse'=0 if `touse'==1 & `tvar'>=`r(max)' 
	}
	qui:capture drop __tr__
	qui:gen byte __tr__=0 if `touse'
	qui:replace  __tr__=1 if `tvar'>=`gvar' & `gvar'>0  & `touse'
	qui:replace  __tr__=1 if `touse' & `gvar'>0 & "`never'"!=""
	qui:capture drop __etr__
	qui:gen byte __etr__=0 if `touse'
	qui:replace  __etr__=1 if `touse' & `tvar'>=`gvar' & `gvar'>0
	qui: replace __tr__=1 if `touse' & RTA_3==1
	qui: replace __etr__=1 if `touse' & RTA_3==1
	
	if "`cvar'"=="" {
		local cvar = "`ivar'"
	}
	
	if "`trendt'"!="" | "`trendg'"!="" | "`trendij'"!="" {
		capture drop tvar_1toT
		qui: sum `tvar', d
		qui: gen tvar_1toT = `tvar'-`r(min)'
		if "`trendt'"!="" {
			capture drop TREAT
			qui: egen TREAT = max(__tr__), by(`ivar')
			local trend "i.TREAT#c.tvar_1toT"
		}	
		if "`trendg'"!="" {
			local trend "i.`trendvar'#c.tvar_1toT"
		}	
		if "`trendij'"!="" {
			capture drop TREAT
			qui: egen TREAT = max(__tr__), by(`ivar')
			capture drop TREAT_`ivar'
			qui: gen TREAT_`ivar' = TREAT*`ivar'
			local trend "i.TREAT_`ivar'#c.tvar_1toT"
		}	
	}
	
	/*
	capture drop __event__
	tempvar aux
	qui:bysort `ivar':egen `aux'=min(`tvar') if e(sample)	
	qui:gen __event__ =  `tvar'-`gvar'+1
	qui: sum __event__ if __etr__==1
	qui: replace __event__=0 if __event__<0 | __event__>`r(max)'
	*/
	capture drop __event__
	tempvar aux
	qui:bysort `ivar':egen `aux'=min(`tvar') if e(sample)	
	qui:gen __event__ =  `tvar'-`gvar'+1
	qui: sum __event__ if __tr__==1
	if "`binpre'"=="" & "`never'"=="" {
		qui: replace __event__=0 if __event__<0 | __event__>`r(max)'
	}
	else if "`never'"=="" {
		qui: replace __event__=0 if __event__>`r(max)'	
	}
	if "`binT'"!="" & "`never'"==""  {
		local max_event = `binT'
		qui: sum __event__ if __etr__==1		
		qui: replace __event__=`binT' if __event__>`binT' & `binT'<`r(max)'
	}
	else if "`never'"==""  {
		local max_event = `r(max)'
	}
	if "`binpre'"!="" & "`never'"=="" {
		qui: sum __event__ if __tr__==1
		qui: replace __event__=`binpre' if __event__<`binpre' & `binpre'>`r(min)'
		qui: sum __event__ if __tr__==1
		qui: replace __event__ = __event__-(`r(min)') if __tr__==1
		qui: sum __event__ if __tr__==1
		local max_event = `r(max)'
	}	
	if "`interval'"!="" & "`never'"=="" {
		capture drop __intevent__
		qui gen __intevent__ = 0
		local i_interval = 1
		forval i=0(1)`max_event' {
			qui replace __intevent__ = `i_interval' if __event__ == `i'
			if `i'>0 & mod(`i',`interval')==0 {
				local i_interval = `i_interval'+1
			}
			
		}
		qui sum __intevent__, d
		local max_intevent = `r(max)'
		
	}
	
	qui sum `tvar', d
	local max_T = `r(max)'
	
	qui:levels `gvar' if `touse' & `gvar'>0, local(glist)
	sum `tvar' if `touse' , meanonly
	qui:levels `tvar' if `touse' & `tvar'>r(min), local(tlist)
	** Center Covariates
	if "`weight'"!="" local wgt aw
	if "`x'"!="" {
			capture drop _x_*
			qui:hdfe `y' `x' if `touse'	[`wgt'`exp'], abs(`gvar') 	keepsingletons  gen(_x_)
			capture drop _x_`y'
			local xxvar _x_*
	}
	***		
	foreach i of local glist {
		
		if "`restriction'"=="group"  {
				local xvar `xvar' c.__tr__#i`i'.`gvar' ///
							 i`i'.`gvar'#c.(`xxvar')   
		}		
		
		if "binT"!="" & "`never'"=="" {
			local g_max_event = `max_event'
		}
		else {
			local g_max_event = `max_T'
		}
		
		if "`interval'"=="" & "`addint'"=="" & "`binT'"=="" & "`binpre'"=="" {			
			local counter = 0
			if "`never'"!="" & "`restriction'"=="" {
				sum `tvar' if `touse'  & `gvar'==`i', meanonly
				qui:levels `tvar' if `touse' & `tvar'!=`gvar'-1 & `gvar'==`i', local(nevertlist)
				foreach j of local nevertlist {
					local xvar `xvar' c.__tr__#i`i'.`gvar'#i`j'.`tvar' ///
						i`i'.`gvar'#i`j'.`tvar'#c.(`xxvar')
				}
			}
			if "`never'"=="" & "`restriction'"=="" {
				foreach j of local tlist {
					if `j'>=`i' {
						local xvar `xvar' c.__tr__#i`i'.`gvar'#i`j'.`tvar' ///
									 i`i'.`gvar'#i`j'.`tvar'#c.(`xxvar') 
					}
				}
			}
		}
		else if "`interval'"=="" & "`addint'"!="" & "`binT'"==""  {
			foreach j of local tlist {
				if `j'>=`i' & "`restriction'"=="" {
					qui:levels `addint' if `touse' & `gvar'==`i', local(addintlist)
					foreach k of local addintlist {
						local xvar `xvar' c.__tr__#i`i'.`gvar'#i`j'.`tvar'#i`k'.`addint' ///
									 i`i'.`gvar'#i`j'.`tvar'#i`k'.`addint'#c.(`xxvar')   						
					}
				}

			}
		}
		else if "`interval'"=="" & "`addint'"=="" & "`binT'"!="" {
			qui sum __event__ if `gvar'==`i', d
			local g_max_event = `r(max)'
			forval j=1(1)`g_max_event' {
				if "`restriction'"=="" & "`never'"=="" {
					local xvar `xvar' c.__tr__#i`i'.`gvar'#i`j'.__event__ ///
								 i`i'.`gvar'#i`j'.__event__#c.(`xxvar')   
				}

			}				
		}
		else if "`interval'"=="" & "`addint'"=="" & "`binT'"=="" & "`binpre'"!="" {
			if "`restriction'"=="" {
				qui sum __event__ if `gvar'==`i', d
				forval j=0(1)`r(max)' {
					if `j'+1!=-`binpre' {
						local xvar `xvar' c.__tr__#i`i'.`gvar'#i`j'.__event__ ///
									 i`i'.`gvar'#i`j'.__event__#c.(`xxvar')   
					}
				}				
			}
		}
		else {
			qui sum __intevent__ if `gvar'==`i', d
			local g_max_intevent = `r(max)'
			forval j=1(1)`g_max_intevent' {
				if "`restriction'"=="" & "`never'"=="" {
					local xvar `xvar' c.__tr__#i`i'.`gvar'#i`j'.__intevent__ ///
								 i`i'.`gvar'#i`j'.__intevent__#c.(`xxvar')   
				}

			}	
			
		}
		
	}
	if "`restriction'"=="calendar"  {
		foreach j of local tlist {
			local xvar `xvar' c.__tr__#i`j'.`tvar' ///
				       i`i'.`gvar'#c.(`xxvar')   
		}		
	}
	if "`restriction'"=="event" & "`binpre'"==""  {
		forval i=0(1)`max_event' {
			local xvar `xvar' c.__tr__#i`i'.__event__ ///
				        i`i'.__event__#c.(`xxvar')   
		}		
	}
	if "`restriction'"=="event" & "`binpre'"!="" {
		forval i=0(1)`max_event' {
			if `i'+1!=-`binpre' {
				local xvar `xvar' c.__tr__#i`i'.__event__ ///
							i`i'.__event__#c.(`xxvar') 
			}
		}		
	}	
	if "`restriction'"=="exporter" & "`interval'"==""  {
		qui:levels `gvar' if __tr__, local(ilist)
		foreach i of local ilist {
			qui:levels `addint' if `touse' & `gvar'==`i', local(explist)
			foreach k of local explist {
				local xvar `xvar' c.__tr__#i`i'.`gvar'#i`k'.`addint'
			}
		}
	}	
	if "`restriction'"=="exporter" & "`interval'"!=""  {
		qui:levels `gvar' if __tr__, local(ilist)
		foreach i of local ilist {
			qui sum __intevent__ if `gvar'==`i', d
			local g_max_intevent = `r(max)'
			qui:levels `addint' if `touse' & `gvar'==`i', local(explist)
			forval j=1(1)`g_max_intevent' {
				foreach k of local explist {
					local xvar `xvar' c.__tr__#i`i'.`gvar'#i`j'.__intevent__#i`k'.`addint'
				}					
			}
		}
	}	
	if "`restriction'"=="ij" & "`interval'"==""  {
		qui:levels `ivar' if __tr__, local(ilist)
		foreach i of local ilist {
			local xvar `xvar' c.__tr__#i`i'.`ivar' ///
						 i`i'.`ivar'#c.(`xxvar') 
		}
	}	
	if "`restriction'"=="ij" & "`interval'"!=""  {
		qui:levels `ivar' if __tr__, local(ilist)
		foreach i of local ilist {
			qui sum __intevent__ if `ivar'==`i', d
			local g_max_intevent = `r(max)'
			forval j=1(1)`g_max_intevent' {
				local xvar `xvar' c.__tr__#i`i'.`ivar'#i`j'.__intevent__ ///
						 i`i'.`ivar'#i`j'.__intevent__#c.(`xxvar')   
			}
		}
	}
			
	** for xs	
	foreach i of local glist {
		local ogxvar `ogxvar' i`i'.`gvar'#c.(`x')
	}
	if "`interval'"=="" {
		foreach j of local tlist {
			local otxvar `otxvar' i`j'.`tvar'#c.(`x')
		}
	}
	
	if "`interval'"!="" {
		qui sum __intevent__, d
		local g_max_intevent = `r(max)'
		forval j=1(1)`g_max_intevent' {
			local otxvar `otxvar' i`j'.__intevent__#c.(`x')
		}
	}
	
 	
	if "`method'"=="" {
		if "`group'"=="" & "`fe'"==""  {
			reghdfe `y' `xvar'   `otxvar' `exovar'	 	///
				if `touse' [`weight'`exp'], abs(`ivar' `tvar' `trend') cluster(`cvar') keepsingletons
		}
		else if "`group'"=="" & "`fe'"!="" {
			reghdfe `y' `xvar'   `otxvar' `exovar'	 	///
				if `touse' [`weight'`exp'], abs(`ivar' `tvar' `fe' `trend') cluster(`cvar') keepsingletons
		}
		else {		 
			reghdfe `y'  `xvar'  `x'  `ogxvar' `otxvar'	 `exovar'  ///
			if `touse' [`weight'`exp'], abs(`gvar' `tvar' `trend') cluster(`cvar') keepsingletons
		}
	}
	else if "`method'"=="ppmlhdfe" {
		if "`ifcond'"!="" {
			replace `touse'=0 if `ifcond'==0
		}
		
		if "`group'"=="" & "`fe'"==""  {
			ppmlhdfe `y' `xvar'   `otxvar' `exovar'	 ///
				if `touse' [`weight'`exp'], abs(`ivar' `tvar' `trend') cluster(`cvar') keepsingletons d
		}
		else if "`group'"=="" & "`fe'"!="" {
			ppmlhdfe `y'   `xvar'   `otxvar'	 `exovar' ///
				if `touse' [`weight'`exp'], abs(`ivar' `tvar' `fe' `trend') cluster(`cvar') keepsingletons d
		}
		else {		 
			ppmlhdfe `y' `xvar'  `x'  `ogxvar' `otxvar' `exovar'	   ///
			if `touse' [`weight'`exp'], abs(`gvar' `tvar' `trend') cluster(`cvar') keepsingletons d
		}
	}	
	else {
		`method'  `y' `exovar' `xvar'  `x'  `ogxvar' `otxvar' i.`gvar' i.`tvar' `trend' i.`fe' ///
		if `touse' [`weight'`exp'], cluster(`cvar') 
	}
	
	if "`method'"=="ppmlhdfe" {
		ereturn local cmd ppmlhdfe
		ereturn local cmdline ppmlhdfe `0'
	}
	else {
		ereturn local cmd jwdid
		ereturn local cmdline jwdid `0'	
	}
	ereturn local estat_cmd jwdid_estat
	if "`never'"!="" ereturn local type  never
	else 			 ereturn local type  notyet
	ereturn local xvar `xvar'
	ereturn local ivar `ivar'
	ereturn local tvar `tvar'
	ereturn local gvar `gvar'
	
end

