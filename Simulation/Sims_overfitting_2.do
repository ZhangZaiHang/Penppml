cls
clear all
capture log close

* ==============================================================================
* 1. 环境设置与日志
* ==============================================================================
* 请根据您的实际路径修改 cd 和 log 路径
cd "C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\code\Simulation"
log using "Sims_Overfitting_3.log" , replace

* ==============================================================================
* 2. 参数设定
* ==============================================================================
* rep: 蒙特卡洛模拟次数
* od:  过度离散参数 (Over-dispersion)
* beta: 真实变量 x1 的系数
qui local rep = 1
qui local od  = 0.3 
qui local beta = 0.2   

* ==============================================================================
* 3. 主循环开始
* ==============================================================================
* obs: 样本量 (250, 1000, 4000)
foreach obs in /*25000 100000*/ 200000 { 
    
    * tph: 预测样本的截止点 (训练集 + 100个测试样本)
    qui local tph = 10000 + `obs'
    
    * rho: 变量间的相关系数
    foreach rho in 0.99 /*0.90*/ 0.75 { 
        
        * f: 具有相关性的变量数量块大小
        foreach f in 50 /*10*/ 200 { 
            
            * p: 总变量数量 (随着样本量增加而增加)
            qui local p = 5*ceil(sqrt(`obs'))
			
            * 设置总观测值数量 (取 rep 和 tph 的较大值以确保空间足够)
            qui local smpl= max(`tph', `rep')
            qui set obs `smpl'

            * ==================================================================
            * 4. 初始化结果存储变量
            * ==================================================================
            
            * --- 变量选择准确性 (1=选中正确变量x1, 0=未选中或选错) ---
            qui g cv = .  if _n <= `rep'  // CV Lasso
            qui g ad = .  if _n <= `rep'  // Adaptive Lasso
            qui g pi = .  if _n <= `rep'  // Plug-in Lasso

            * --- 选中变量的个数 (Number of Selected Variables) ---
            qui g kcv = . if _n <= `rep'
            qui g kad = . if _n <= `rep'
            qui g kpi = . if _n <= `rep'

            * --- 预测误差 MSE (Mean Squared Error) ---
            * m* 系列变量存储 Post-Lasso (选变量后重跑PPML) 的 MSE
            qui g mcv = .  if _n <= `rep'
            qui g mad = .  if _n <= `rep'
            qui g mpi = .  if _n <= `rep'
            
            * l* 系列变量存储 Lasso 原始输出 (仅选了变量) 的 MSE
            qui g lcv = .  if _n <= `rep'
            qui g lad = .  if _n <= `rep'
            qui g lpi = .  if _n <= `rep'

            qui g zero = . if _n <= `rep' // 记录是否所有变量都被剔除

            * --- 基准模型 (Benchmarks) 的 MSE ---
            qui g mse_under = . if _n <= `rep' // 欠拟合 (Bias)
            qui g mse_ideal = . if _n <= `rep' // 理想拟合 (Oracle)
            qui g mse_full  = . if _n <= `rep' // 过拟合 (Full Model)

            set seed 20260128
            qui local f1 = `f' + 1

            * ==================================================================
            * 5. 模拟迭代 (Replications)
            * ==================================================================
            forvalues r = 1/`rep' {	
                
                * --- A. 数据生成过程 (DGP) ---
                * 生成相关矩阵 V
                mat v = J(`f', `f', `rho') + (1 - `rho') * I(`f')

                * 生成相关变量 x1-xf
                drawnorm x1-x`f' , corr(v) cstorage(full) 
                * 生成独立变量 x(f+1)-xp
                drawnorm x`f1'-x`p' 
                * 生成控制变量 z
                qui g z = rnormal()
                
                * 生成因变量 y
                * 真实模型：y 取决于 x1, z 和随机误差。x2-xp 是噪音变量。
                * 注意：预测集 (_n > obs) 的 y 也在这里生成，用于计算 MSE
                qui g y = (exp(1 + `beta'*x1 + z + `od'*rnormal())) if _n <= `tph'

                * ==============================================================
                * 6. 基准模型评估 (Benchmarks)
                * ==============================================================

                * --- (1) 欠拟合 (Under-fitting) ---
                * 场景：遗漏重要变量 x1，只回归 z
                capture poisson y z if _n <= `obs'
                if _rc == 0 {
                    * 在测试集 (obs < n <= tph) 上预测
                    qui predict fit_u if _n <= `tph' & _n > `obs', n
                    qui g res_u = (y - fit_u)^2
                    su res_u, meanonly
                    qui replace mse_under = r(mean) in `r'
                    drop fit_u res_u
                }

                * --- (2) 理想拟合 (Ideal-fitting / Oracle) ---
                * 场景：已知真实模型，只回归 x1 和 z
                capture poisson y x1 z if _n <= `obs'
                if _rc == 0 {
                    qui predict fit_i if _n <= `tph' & _n > `obs', n
                    qui g res_i = (y - fit_i)^2
                    su res_i, meanonly
                    qui replace mse_ideal = r(mean) in `r'
                    drop fit_i res_i
                }

                * --- (3) 过拟合 (Over-fitting / Full Model) ---
                * 场景：不做筛选，回归所有 x 和 z。展示高维噪音带来的方差。
                capture poisson y z x* if _n <= `obs'
                if _rc == 0 {
                    qui predict fit_f if _n <= `tph' & _n > `obs', n
                    qui g res_f = (y - fit_f)^2
                    su res_f, meanonly
                    qui replace mse_full = r(mean) in `r'
                    drop fit_f res_f
                }

                * ==============================================================
                * 7. Lasso 变体模型评估
                * ==============================================================

                * --------------------------------------------------------------
                * 变体 1: CV Lasso (Cross-Validation)
                * --------------------------------------------------------------
                capture lasso poisson y (z) x* if _n <= `obs' // 默认 selection(cv)
                if _rc == 0 { 
                    * 记录选中的变量个数
                    qui replace kcv = e(k_nonzero_sel) - 1 in `r'
                    
                    * 记录是否正确选中 x1
                    * e(k_nonzero_sel)==1 意味着只选中了 z (因为 z 是必须包含的)，说明漏选了 x1
                    if e(k_nonzero_sel) == 1  qui replace cv = 0 in `r'
                    else {
                        local regs `e(othervars_sel)'
                        gettoken regs1 : regs
                        * 如果选中的第一个变量是 x1 (简单判断逻辑)，记为正确
                        if "`regs1'" == "x1" qui replace cv = 1 in `r'
                        else  qui replace cv = 0 in `r'
                    }

                    * 计算 MSE (Linear Prediction converted to counts)
                    qui predict fit if _n <= `tph' & _n > `obs', n 
                    qui g res = (y - fit)^2
                    su res, meanonly
                    qui replace lcv = r(mean) in `r'
                    drop fit res
                    
                    * 计算 MSE (Post-Lasso: 用选中变量重跑 PPML)
                    qui predict fit if _n <= `tph' & _n > `obs', n post 
                    qui g res = (y - fit)^2
                    su res, meanonly
                    qui replace mcv = r(mean) in `r'
                    drop fit res
                }

                * --------------------------------------------------------------
                * 变体 2: Adaptive Lasso
                * --------------------------------------------------------------
                * 第一步：运行 Ridge 或 Lasso 得到权重；第二步：加权 Lasso
                capture lasso poisson y (z) x* if _n <= `obs', selection(adaptive)
                if _rc == 0 { 
                    qui replace kad = e(k_nonzero_sel) - 1 in `r'
                    if e(k_nonzero_sel) == 1 qui replace ad = 0 in `r'
                    else {
                        local regsad `e(othervars_sel)'
                        gettoken regs2 : regsad
                        if "`regs2'" == "x1" qui replace ad = 1 in `r'
                        else qui replace ad = 0 in `r'
                    }
                    
                    * MSE (Lasso Linear)
                    qui predict fit if _n <= `tph' & _n > `obs', n 
                    qui g res = (y - fit)^2 if _n <= `tph' & _n > `obs'
                    su res if _n <= `tph' & _n > `obs', meanonly
                    qui replace lad = r(mean) in `r'
                    drop fit res

                    * MSE (Post-Lasso)
                    qui predict fit if _n <= `tph' & _n > `obs', n post
                    qui g res = (y - fit)^2 if _n <= `tph' & _n > `obs'
                    su res if _n <= `tph' & _n > `obs', meanonly
                    qui replace mad = r(mean) in `r'
                    drop fit res
                }

                * --------------------------------------------------------------
                * 变体 3: Plug-in Lasso
                * --------------------------------------------------------------
                * 使用理论导出的惩罚项 lambda，通常比 CV 更快且选择变量更保守
                capture lasso poisson y (z) x* if _n <= `obs', selection(plugin)
                if _rc == 0 {
                    local regz `e(othervars_sel)'
                    
                    * MSE (Lasso Linear)
                    qui predict fit if _n <= `tph' & _n > `obs', n 
                    qui g res = (y - fit)^2 if _n <= `tph' & _n > `obs'
                    su res if _n <= `tph' & _n > `obs', meanonly
                    qui replace lpi = r(mean) in `r'
                    drop fit res

                    * MSE (Post-Lasso)
                    qui predict fit if _n <= `tph' & _n > `obs', n post 
                    qui g res = (y - fit)^2 if _n <= `tph' & _n > `obs'
                    su res if _n <= `tph' & _n > `obs', meanonly
                    qui replace mpi = r(mean) in `r'
                    drop fit res

                    qui replace kpi = e(k_nonzero_sel) - 1 in `r'
                    
                    * 检查变量选择准确性
                    if e(k_nonzero_sel) == 1  {
                        * 没选中任何 x，只剩 z
                        qui replace pi = 0 in `r'
                        qui replace zero = 1 in `r'
                    }
                    else {
                        qui replace zero = 0 in `r'
                        local regs `e(othervars_sel)'
                        gettoken regs1 : regs
                        if "`regs1'" == "x1" {
                            qui replace pi = 1 in `r'
                        }
                        else {
                            qui replace pi = 0 in `r'
                        }
                    }
                }
                
                * 清理当次循环的数据，准备下一次 Monte Carlo
                drop y x* z
            } 
            * End of Rep Loop

            * ==================================================================
            * 8. 结果汇报
            * ==================================================================
            di  "------------------------------------------------------------------"
            di  "Parameters: P = " `p' ", N = " `obs' ", Rho = " `rho' ", f = " `f'
			
			di
            di  "SECTION 1: BENCHMARKS PREDICTION ERROR (MSE)"
            di  "Under (Bias) | Ideal (Oracle) | Full (Variance)"
            su mse_under mse_ideal mse_full
            
            di
            di  "SECTION 2: LASSO VARIANTS - SELECTION ACCURACY (Correctly picked x1?)"
            di  "CV Lasso | Adaptive | Plug-in"
            su cv ad pi if _n <= `rep'

            di 
            di  "SECTION 3: LASSO VARIANTS - NUMBER OF VARS SELECTED"
            su kcv kad kpi if _n <= `rep'
            
            di
            di  "SECTION 4: LASSO VARIANTS - PREDICTION ERROR (Post-Lasso MSE)"
            su mcv mad mpi if _n <= `rep'
            
            di "------------------------------------------------------------------"
            
            * 清理结果变量，准备下一次参数组合
            drop cv* ad* pi* kcv kad kpi* mcv mad mpi* lcv lad lpi* zero mse_under mse_ideal mse_full
        }
    }
}
capture log close