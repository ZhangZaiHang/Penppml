  
  # ==============================================================================
  ### 第一步：加载R包
  # ==============================================================================
  library(dplyr)
  library(haven)
  library(data.table) # 数据处理
  library(ggplot2) # 画图
  library(fixest)
  
  # ==============================================================================
  ### 第二步：导入处理数据
  # ==============================================================================
  dss <- readRDS("C:/Users/ZaihangZhang/OneDrive/MasterThesis/MachineLearninginAgriculturalTradeResearch/data/usr/data_sdid_small_etwfe.rds")
  setDT(dss)
  dss$lgdp_o <- log(dss$gdp_pwt_cur_o)
  dss$lgdp_d <- log(dss$gdp_pwt_cur_d)
  dss$lDIST <- log(dss$DIST)
  
  # ==============================================================================
  ### 第三步：划分训练集和测试集 (Stratified Split)
  # ==============================================================================
  
  # 1. 计算每个 ID 的观测数量
  dss[, obs_count := .N, by = id_ci_cj]
  
  # 2. 生成划分标记 (is_train)
  # 设定种子，保证可复现
  set.seed(2026)
  
  # 第一步：按组进行加权随机采样 (这里设为 50% 训练，50% 测试)
  # 注意：by = id_ci_cj 是关键，它在每个 ID 内部独立操作
  dss[, is_train := sample(c(TRUE, FALSE), size = .N, replace = TRUE, prob = c(0.5, 0.5)), 
     by = id_ci_cj]
  
  # 3. 关键修正：处理“孤儿样本”和“运气不好”的情况
  # 情况 A：如果某个 ID 只有 1 条数据，必须放入训练集 (否则无法估计 FE)
  dss[obs_count == 1, is_train := TRUE]
  
  # 情况 B：(极少数情况) 如果某个 ID 有多条数据，但随机抽样导致全部分到了测试集(FALSE)
  # 我们强制把该 ID 的第一条数据改为训练集
  dss[, train_count := sum(is_train), by = id_ci_cj]
  dss[train_count == 0, is_train := c(TRUE, rep(FALSE, .N - 1)), by = id_ci_cj]
  
  # 4. 拆分数据
  dss_train <- dss[is_train == TRUE]
  dss_test  <- dss[is_train == FALSE]
  
  # 5. 验证
  # 检查训练集是否覆盖了所有的 ID
  n_total_ids <- uniqueN(dss$id_ci_cj)
  n_train_ids <- uniqueN(dss_train$id_ci_cj)
  
  print(paste("总 ID 数:", n_total_ids))
  print(paste("训练集覆盖 ID 数:", n_train_ids))
  
  if (n_total_ids == n_train_ids) {
    message("成功！所有 ID 都出现在训练集中。")
  } else {
    message("警告：仍有 ID 未被覆盖。")
  }
  
  # ==============================================================================
  ### 第四步：定义不同的模型规格 (Model Specifications)
  # ==============================================================================
  # 我们创建一个列表，存放不同的回归公式
  # 对应 Stata 语法: ppmlhdfe trade [变量], abs(固定效应)
  
  models_list <- list(
    # 模型 1: 传统引力模型 (仅 GDP + 距离)
    # 注意：由于使用了 pair FE，距离(dist)通常会被吸收，除非是时变距离。
    # 这里假设 dist 是时不变的，所以被 id_ci_cj 吸收，只放 lgdp
    "Traditional" = trade ~ lgdp_o + lgdp_d + lDIST + RTA,
    
    # 模型 2: 加入标准 RTA 变量
    "2-way" = trade ~ lDIST + RTA | idt_ci + idt_cj,
    
    # 模型 3: ETWFE 动态效应模型 (你最关心的 cohort * year)
    # 使用 fixest 的 i() 语法自动生成交互项，无需手动生成 cohort2000 等变量
    "3-way" = trade ~ RTA | idt_ci + idt_cj + id_ci_cj,
    
    "1-way" = trade ~ lgdp_o + lgdp_d + RTA | id_ci_cj,
    
    "ETWFE" = trade ~ RTA | idt_ci + idt_cj + id_ci_cj
  )
  
  # ==============================================================================
  ### 第五步：循环训练、预测并计算均方根误差RMSE
  # ==============================================================================
  # 创建一个空的 data.table 来存储结果
  results_table <- data.table(Model = character(), MSE = numeric(), RMSE = numeric(), MAE = numeric())
  
  message("开始模型训练与评估...")
  
  for (model_name in names(models_list)) {
    
    # --- A. 训练模型 (Training) ---
    # fepois 对应 ppmlhdfe
    fit <- fepois(models_list[[model_name]], 
                  data = dss_train
                  )
    
    # --- B. 样本外预测 (Testing) ---
    # type = "response" 预测贸易流量的水平值 (Level)
    # newdata = dt_test 指定测试集
    y_hat <- predict(fit, newdata = dss_test, type = "response")
    
    # --- C. 处理潜在的 NA ---
    # 如果测试集中出现了训练集没见过的固定效应，fixest 可能会返回 NA。
    # 我们只基于有效的预测值计算 RMSE
    valid_idx <- !is.na(y_hat)
    actual <- dss_test$trade[valid_idx]
    predicted <- y_hat[valid_idx]
    
    # --- D. 计算误差指标 ---
    # MSE: 均方误差
    mse_val <- mean((actual - predicted)^2)
    
    # RMSE: 均方根误差 (对大误差敏感)
    rmse_val <- sqrt(mean((actual - predicted)^2))
    
    # MAE: 平均绝对误差 (更稳健)
    mae_val  <- mean(abs(actual - predicted))
    
    # --- E. 存储结果 ---
    results_table <- rbind(results_table, 
                           data.table(Model = model_name, MSE = mse_val, RMSE = rmse_val, MAE = mae_val))
    
    message(paste("完成:", model_name, "| MSE =", round(mse_val, 2)))
  }
  
  # ==============================================================================
  ### 第六步：结果展示
  # ==============================================================================
  print("=== 模型性能对比 ===")
  print(results_table)
  
  # 可视化 RMSE 对比
  ggplot(results_table, aes(x = reorder(Model, RMSE), y = RMSE, fill = Model)) +
    geom_bar(stat = "identity", width = 0.6) +
    coord_flip() +
    theme_minimal() +
    labs(title = "不同模型规格的预测误差对比 (RMSE)",
         subtitle = "越低越好 (Lower is Better)",
         x = "模型",
         y = "均方根误差 (RMSE)")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  