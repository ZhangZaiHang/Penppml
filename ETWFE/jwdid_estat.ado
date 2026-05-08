*! v1 8/5/2022 FRA. Adds margins the right way
program jwdid_estat, sortpreserve   
	version 14
    syntax anything, [*]
        /*
		if "`e(cmd)'" != "jwdid" {
                error 301
        }
		*/
        gettoken key rest : 0, parse(", ")
        if inlist("`key'","simple","simple_ppml","group","group_ppml","calendar","calendar_ppml","event","event_ppml","event2_ppml") {
			jwdid_`key'  `rest'
        }
		else {
			display in red "Option `key' not recognized"
				error 199
		}

end

program jwdid_simple, rclass
		syntax, [*]
		//tempvar aux
		//qui:bysort `e(ivar)':egen `aux'=min(`e(tvar)') if e(sample)
		margins  ,  subpop(if __etr__==1) at(__tr__=(0 1)) ///
					noestimcheck contrast(atcontrast(r)) `options'
		matrix bb=r(b)
		matrix VV=r(V)
		return matrix b = bb
		return matrix V = VV
end

program jwdid_simple_ppml, rclass
		syntax, [*]
		//tempvar aux
		//qui:bysort `e(ivar)':egen `aux'=min(`e(tvar)') if e(sample)
		margins  ,  subpop(if __etr__==1) at(__tr__=(0 1)) ///
					noestimcheck `options' post
		qui nlcom ln(_b[2._at])-ln(_b[1._at])
		matrix bb=r(b)
		matrix VV=r(V)
		return matrix b = bb
		return matrix V = VV
end

program jwdid_group, rclass
		syntax, [*]
		tempvar aux
		qui:bysort `e(ivar)':egen `aux'=min(`e(tvar)') if e(sample)
		capture drop __group__
		qui:clonevar __group__ =  `e(gvar)' if __etr__==1 & `aux'<`e(gvar)'
		margins , subpop(if __etr__==1) at(__tr__=(0 1)) ///
				  over(__group__) noestimcheck contrast(atcontrast(r)) `options'	
		
	mat table = r(table)
	mat bb = table["b",1...]'
	mat lo = table["ll",1...]'
	mat hi = table["ul",1...]'
	local n=rowsof(bb)
	mat idx=J(`n',1,.)
	local rownms: rown bb
	forval i=1(1)`n' {
		local rowname: word `i' of `rownms'
		mat idx[`i',1] = real(substr("`rowname'",11,4))
	}
	
	capture drop __group__
	return matrix b = bb
	return matrix ll = lo
	return matrix ul = hi
	return matrix index = idx
		
end

program jwdid_group_ppml, rclass
		syntax, [Ivar(string) Tvar(string) Gvar(string) GROUPvar(string)]
		if "`gvar'"!="" {
			local gvar `gvar'
		}
		else {
			local gvar `e(gvar)'
		}
		if "`ivar'"!="" {
			local ivar `ivar'
		}
		else {
			local ivar `e(ivar)'
		}
		if "`tvar'"!="" {
			local tvar `tvar'
		}
		else {
			local tvar `e(tvar)'
		}
		if "`groupvar'"!="" {
			capture drop __group__
			gen __group__ = `groupvar'
		}
		else {
			tempvar aux
			qui:bysort `e(ivar)':egen `aux'=min(`e(tvar)') if e(sample)
			capture drop __group__
			qui:clonevar __group__ =  `e(gvar)' if __etr__==1 & `aux'<`e(gvar)'
		}
		margins , subpop(if __etr__==1) at(__tr__=(0 1)) ///
				  over(__group__) noestimcheck contrast(atcontrast(r)) predict(xb)
		
		mat table = r(table)
		mat bb = table["b",1...]'
		mat lo = table["ll",1...]'
		mat hi = table["ul",1...]'
		local n=rowsof(bb)
		mat idx=J(`n',1,.)
		local rownms: rown bb
		forval i=1(1)`n' {
			local rowname: word `i' of `rownms'
			mat idx[`i',1] = real(substr("`rowname'",11,4))
		}
		
		capture drop __group__
		return matrix b = bb
		return matrix ll = lo
		return matrix ul = hi
		return matrix index = idx
		
end

program jwdid_calendar, rclass
syntax, [*]
		capture drop __calendar__
		tempvar aux
		qui:bysort `e(ivar)':egen `aux'=min(`e(tvar)') if e(sample)
		qui:clonevar __calendar__ =  `e(tvar)' if __etr__==1 & `aux'<`e(gvar)'
		margins , subpop(if __etr__==1) at(__tr__=(0 1)) ///
				over(__calendar__) noestimcheck contrast(atcontrast(r)) `options'
	
		mat table = r(table)
		mat bb = table["b",1...]'
		mat lo = table["ll",1...]'
		mat hi = table["ul",1...]'
		local n=rowsof(bb)
		mat idx=J(`n',1,.)
		local rownms: rown bb
		forval i=1(1)`n' {
			local rowname: word `i' of `rownms'
			mat idx[`i',1] = real(substr("`rowname'",11,4))
		}

		capture drop __calendar__
		return matrix b = bb
		return matrix ll = lo
		return matrix ul = hi
		return matrix index = idx
		
end

program jwdid_calendar_ppml, rclass
		syntax, [Ivar(string) Tvar(string) Gvar(string)]
		if "`gvar'"!="" {
			local gvar `gvar'
		}
		else {
			local gvar `e(gvar)'
		}
		if "`ivar'"!="" {
			local ivar `ivar'
		}
		else {
			local ivar `e(ivar)'
		}
		if "`tvar'"!="" {
			local tvar `tvar'
		}
		else {
			local tvar `e(tvar)'
		}
		capture drop __calendar__
		tempvar aux
		qui:bysort `e(ivar)':egen `aux'=min(`e(tvar)') if e(sample)
		qui:clonevar __calendar__ =  `e(tvar)' if __etr__==1 & `aux'<`e(gvar)'
		margins , subpop(if __etr__==1) at(__tr__=(0 1)) ///
				over(__calendar__) noestimcheck `options' post
	
		local n=colsof(r(b))/2	
		mat bb = J(`n',1,.)
		mat lo = J(`n',1,.)
		mat hi = J(`n',1,.)
		mat idx = J(`n',1,.)
		
		local colnms: coln r(b)
		
		forval i=1(1)`n' {
			local i_minus = `i'-1
			local colname: word `i' of `colnms'
			local word_short = substr("`colname'",9,4)
			qui nlcom ln(_b[2._at#`word_short'.__calendar__])-ln(_b[1._at#`word_short'.__calendar__])
			mat V_temp = r(V)
			mat bb[`i',1] = r(b)
			mat lo[`i',1] = r(b)-sqrt(V_temp[1,1])*1.96
			mat hi[`i',1] = r(b)+sqrt(V_temp[1,1])*1.96
			mat idx[`i',1] = real("`word_short'")
		}
		
		capture drop __calendar__
		return matrix b = bb
		return matrix ll = lo
		return matrix ul = hi
		return matrix index = idx

end

program jwdid_event, rclass
syntax, [*]
		capture drop __event__
		tempvar aux
		qui:bysort `e(ivar)':egen `aux'=min(`e(tvar)') if e(sample)
		
		qui:gen __event__ =  `e(tvar)'-`e(gvar)' 
		
		margins , subpop(if __etr__==1) at(__tr__=(0 1)) ///
				over(__event__) noestimcheck contrast(atcontrast(r)) `options'
		
		mat table = r(table)
		mat bb = table["b",1...]'
		mat lo = table["ll",1...]'
		mat hi = table["ul",1...]'
		local n=rowsof(bb)
		mat idx=J(`n',1,.)
		forval i=1(1)`n' {
			mat idx[`i',1] = `i'-1
		}
		
		capture drop __event__
		return matrix b = bb
		return matrix ll = lo
		return matrix ul = hi
		return matrix index = idx		
end

program jwdid_event_ppml, rclass
		syntax, [Ivar(string) Tvar(string) Gvar(string) binT(numlist min=1 max=1 >0 integer)]
		if "`gvar'"!="" {
			local gvar `gvar'
		}
		else {
			local gvar `e(gvar)'
		}
		if "`ivar'"!="" {
			local ivar `ivar'
		}
		else {
			local ivar `e(ivar)'
		}
		if "`tvar'"!="" {
			local tvar `tvar'
		}
		else {
			local tvar `e(tvar)'
		}
		capture drop __event__
		tempvar aux
		qui:bysort `ivar':egen `aux'=min(`tvar') if e(sample)
		
		qui:gen __event__ =  `tvar'-`gvar' 
		
		if "`binT'"!="" {
			qui: sum __event__ if __etr__==1		
			replace __event__=`binT' if __event__>`binT' & `binT'<`r(max)'
		}
		
		margins , subpop(if __etr__==1) at(__tr__=(0 1)) ///
				over(__event__) noestimcheck contrast(atcontrast(r)) predict(xb)
		
		mat table = r(table)
		mat bb = table["b",1...]'
		mat lo = table["ll",1...]'
		mat hi = table["ul",1...]'
		local n=rowsof(bb)
		mat idx=J(`n',1,.)
		forval i=1(1)`n' {
			mat idx[`i',1] = `i'-1
		}
		
		capture drop __event__
		return matrix b = bb
		return matrix ll = lo
		return matrix ul = hi
		return matrix index = idx	
end

program jwdid_event2_ppml, rclass
		syntax, [Ivar(string) Tvar(string) Gvar(string) Evar(string) shift(numlist min=1 max=1 >0 integer) binpre(numlist min=1 max=1 <0 integer) binT(numlist min=1 max=1 >0 integer)]
		if "`gvar'"!="" {
			local gvar `gvar'
		}
		else {
			local gvar `e(gvar)'
		}
		if "`ivar'"!="" {
			local ivar `ivar'
		}
		else {
			local ivar `e(ivar)'
		}
		if "`tvar'"!="" {
			local tvar `tvar'
		}
		else {
			local tvar `e(tvar)'
		}
		if "`evar'"!="" & "`binpre'"!="" {
			local evar `evar'
			local event_min = `binpre'
			replace `evar' = `evar'-`shift' if __tr__==1
			replace `evar' = `binpre' if `evar'<`binpre' & __tr__==1
			replace `evar' = `evar'-`binpre' if __tr__==1
		}
		else {
			capture drop __event__
			tempvar aux
			qui:bysort `ivar':egen `aux'=min(`tvar') if e(sample)		
			qui: gen __event__ =  `tvar'-`gvar'	
			qui: sum __event__ if __tr__==1, d
			if `r(min)'<0 {
				qui: replace __event__ = __event__-(`r(min)') if __tr__==1
				local event_min = `r(min)'
			}
			local evar __event__
		}
		if "`binT'"!="" {
			qui: sum `evar' if __tr__==1		
			replace `evar'=`binT'-`binpre' if `evar'>`binT'-`binpre' & `binT'-`binpre'<`r(max)'
		}
		
		margins , subpop(if __tr__==1) at(__tr__=(0 1)) ///
				over(`evar') noestimcheck `options' post
		local n=colsof(r(b))/2	
		mat bb = J(`n',1,.)
		mat lo = J(`n',1,.)
		mat hi = J(`n',1,.)
		mat idx = J(`n',1,.)

		qui:levels `evar' if __tr__==1, local(elist)
		local i = 0
		foreach e of local elist {
			local i = `i'+1
			qui nlcom ln(_b[2._at#`e'.`evar'])-ln(_b[1._at#`e'.`evar'])
			mat V_temp = r(V)
			mat bb[`i',1] = r(b)
			mat lo[`i',1] = r(b)-sqrt(V_temp[1,1])*1.96
			mat hi[`i',1] = r(b)+sqrt(V_temp[1,1])*1.96
			mat idx[`i',1] = `e'+`event_min'
		}
		
		capture drop `evar'
		return matrix b = bb
		return matrix ll = lo
		return matrix ul = hi
		return matrix index = idx
end

// End of File