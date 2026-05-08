******************************************************************************************************************************
******************************************************************************************************************************
***
*** Table 4-5 PPML, PPML-lasso, and post-lasso PPML results for plug-in approach
*** 
******************************************************************************************************************************
******************************************************************************************************************************
	use ${usr_data}\data_sdid_medium, clear
**# 基准结果第1列
	drop if trade == .
	ppmlhdfe trade RTA, abs(idt_ci idt_cj id_ci_cj) cluster(id_ci_cj)

**# 基准结果第2列
***方法1(推荐)
	use ${usr_data}\data_sdid_medium, clear
	drop if trade == .

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
	
	ppmlhdfe trade `ppml', abs(idt_ci idt_cj id_ci_cj) cluster(id_ci_cj)
	
	estimates save "${usr_data}\1991_2021.ster", replace

***方法2
	jwdid trade, ivar(id_ci_cj) tvar(year) gvar(first_treat) method(ppmlhdfe) fe(idt_ci idt_cj)
	estimates save "${usr_data}\1991_2022.ster", replace
	estat simple, predict(xb)
	
**# ETWFE + Lasso
*** (I) 构造交乘项，为lasso回归生成相关变量
	forvalues i = 1991/2021 {
		g f`i' = year == `i'
	}
	
	egen cohort = sum(RTA), by(id_ci_cj)
	forvalues i = 1991/2021 {
		qui g cohort`i' = norm_rta_depth if cohort == 2022 - `i'
		qui replace cohort`i' = 0 if cohort`i' == .
	}

	local ppml ""
	forvalues c = 1991/2021 {
		forvalues y = 1991/2021 {
			if `y' >= `c' {
			qui g cohort`c'f`y' = c.cohort`c'#c.f`y'
        }
    }
}

	drop cohort cohort1991 cohort1992 cohort1993 cohort1994 cohort1995 cohort1996 cohort1997 cohort1998 cohort1999 cohort2000 cohort2001 cohort2002 cohort2003 cohort2004 cohort2005 cohort2006 cohort2007 cohort2008 cohort2009 cohort2010 cohort2011 cohort2012 cohort2013 cohort2014 cohort2015 cohort2016 cohort2017 cohort2018 cohort2019 cohort2020 cohort2021 f* idt_ci idt_cj id_ci_cj id_ci id_cj

	qui compress

	save "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\data\usr\data_sdid_medium_DTA_1991_2021.dta", replace
	

**#基准结果第5列(Mean-post-lasso)
	ppmlhdfe trade cohort1991f1991 cohort1991f1992 cohort2004f2004 cohort2004f2009 cohort2004f2010 cohort2004f2011 cohort2004f2012 cohort2004f2013 cohort2004f2014 cohort2004f2015 cohort2004f2016 cohort2004f2017 cohort2007f2017, abs(idt_ci idt_cj id_ci_cj) cluster(id_ci_cj)
	
***计算平均处理效应ATT
* 1. 提取回归后的系数向量和协方差矩阵
	matrix b = e(b)
	matrix V = e(V)

* 2. 获取矩阵总列数并初始化变量名列表
	local total_cols = colsof(b)
	local col_names : colnames b

* 3. 初始化：计数器（有效变量数）和权重矩阵
	local valid_count = 0
	matrix W = J(1, `total_cols', 0)

* 4. 遍历系数：识别有效参与估计的变量（排除 _cons 和 omitted）
	local i = 1
	foreach var in `col_names' {
    * 仅处理你生成的交互项，排除常数项
		if "`var'" != "_cons" {
        * 检查变量是否被省略 (只有未被省略的变量，其方差才不为0)
			if el(V, `i', `i') != 0 {
				local valid_count = `valid_count' + 1
				matrix W[1, `i'] = 1
			}
		}
		local i = `i' + 1
	}

* 5. 归一化权重：将标记为 1 的位置转为 1/valid_count
	if `valid_count' > 0 {
		matrix W = W / `valid_count'
	}
	else {
		display as error "错误：未发现有效回归系数，请检查变量是否全被 dropped。"
		exit
	}

* 6. 矩阵运算：点估计与方差
	matrix est_m = b * W'
	matrix var_m = W * V * W'

* 7. 提取标量并执行 z 分布统计推断
	scalar estimate = est_m[1,1]
	scalar se       = sqrt(var_m[1,1])
	scalar z_val    = estimate / se
	scalar p_val    = 2 * normal(-abs(z_val))
	scalar ci_low   = estimate - invnormal(0.975) * se
	scalar ci_high  = estimate + invnormal(0.975) * se

* 8. 结果美化输出
	display ""
	display "{hline 62}"
	display "  PPML 动态系数线性组合 (z-distribution 推断)"
	display "{hline 62}"
	display "  有效系数个数 (n):  " %9.0f `valid_count'
	display "  平均点估计值:      " %9.6f estimate
	display "  标准误 (Std. Err.):" %9.6f se
	display "  z-统计量:          " %9.2f z_val
	display "  P > |z|:           " %9.4f p_val
	display "  95% 置信区间:     [" %9.6f ci_low ", " %9.6f ci_high "]"
	display "{hline 62}"

**#基准结果第6列
	ppmlhdfe trade RTA cohort1991f1991 cohort1991f1992 cohort2004f2004 cohort2004f2009 cohort2004f2010 cohort2004f2011 cohort2004f2012 cohort2004f2013 cohort2004f2014 cohort2004f2015 cohort2004f2016 cohort2004f2017 cohort2007f2017, abs(idt_ci idt_cj id_ci_cj) cluster(id_ci_cj) d
	

