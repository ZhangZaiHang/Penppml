  rm(list = ls())

  # =======================================================
  ### 第一步：加载R包
  # =======================================================
  library(penppml)
  library(haven)
  library(dplyr)
  library(ggplot2)
  # library(pheatmap)
  library(tidyr)
  
  # =======================================================
  ### 第二步：导入处理数据
  # =======================================================
  data_sdid <- read_dta("C:/Users/ZaihangZhang/OneDrive/MasterThesis/MachineLearninginAgriculturalTradeResearch/data/usr/data_sdid_medium_DTA_1991_2021.dta")
  data_sdid <- as.data.frame(data_sdid)
  data_sdid <- na.omit(data_sdid)
  
  # =======================================================
  ### 第三步：构建相关变量
  # =======================================================
  # 构建固定效应
  exp  <- factor(data_sdid$exporter)
  imp  <- factor(data_sdid$importer)
  time <- factor(data_sdid$year)
  exp_time <- interaction(exp, time)
  imp_time <- interaction(imp, time)
  pair     <- interaction(exp, imp)
  
  
  # 按照协定聚类
  data_sdid$alt_id <- data_sdid$id
  data_sdid$alt_id[is.na(data_sdid$alt_id)] <- 0
  data_sdid$pair <- pair
  # data_sdid <- within(data_sdid, {alt_id2 = ave(alt_id,pair,FUN=max)} ) # 根据国家对筛选其中id最大的贸易协定
  data_sdid$alt_id[data_sdid$alt_id==0] <- data_sdid$pair[data_sdid$alt_id==0]
  
  alt_id <- factor(data_sdid$alt_id)
  
  data_sdid$exp_time = exp_time
  data_sdid$imp_time = imp_time
  data_sdid$pair = pair
  data_sdid$alt_id = alt_id # 交叉验证中，有协定的按照协定聚类，无协定的按照国家对聚类
  
  # =======================================================
  ### 第四步：使用 HDFE 进行无惩罚的 PPML 估计
  # =======================================================
  reg_rta <- hdfeppml(data = data_sdid, indep = 7,
                      dep = "trade",
                      cluster = "pair",
                      fixed = list("exp_time","imp_time","pair"))
  
  results <- data.frame(prov = rownames(reg_rta$coefficients), b = reg_rta$coefficients, se = 0)
  results$se[!is.na(reg_rta$coefficients)] <- reg_rta$se
  results
  
  # =======================================================
  ### 第五步：使用 HDFE 进行带有惩罚项的 PPML 估计
  # =======================================================
  ## LASSO回归
  lambdas <- c(0.01, 0.0095, 0.009, 0.0085, 0.008, 0.0075, 0.007, 0.0065, 0.006, 0.0055, 0.005, 0.0045, 0.004, 0.0035, 0.003, 0.0025, 0.002, 0.0015, 0.001, 0.0005, 0.0001)
  lambdas <- c(0.0075, 0.005, 0.0025, 0.001, 0.00075, 0.0005,0.00045,0.0004, 0.00035, 0.0003, 0.0002, 0)
  
  reg_lambdas <- mlfitppml(data = data_sdid, indep = 6:637,
                           dep = "trade",
                           cluster = "pair",
                           fixed = list("exp_time","imp_time","pair"),
                           penalty = "lasso",
                           lambdas = lambdas)
  
  reg_cohortyear_small <- penhdfeppml(data = cohortyear_sdid_small, indep = 91:756,
                                      dep = "trade",
                                      cluster = "pair",
                                      fixed = list("exp_time","imp_time","pair"),
                                      penalty = "lasso",
                                      lambda = 0.005)
  ## 岭回归(开发中)
  lambdas <- seq(0.0001, 0, length.out = 10) 
  
  reg4 <- mlfitppml(data = data_sdid,
                    dep = "trade",
                    cluster = "pair",
                    fixed = list("exp_time","imp_time","pair"),
                    penalty = "ridge",
                    lambdas = lambdas)
  
  # =======================================================
  ### 第六步：Penalty selection
  # =======================================================
  ## 1 使用Plugin lasso确定惩罚参数(基准回归)
  plugin_alltrade <- mlfitppml(data = data_sdid,
                               indep = 8:503,
                               dep = "trade",
                               penalty = "lasso",
                               method = "plugin",
                               cluster = "pair",
                               fixed = list("exp_time","imp_time","pair"),
                               colcheck_x = FALSE,
                               colcheck_x_fes = FALSE,
                               post=TRUE, phipost=TRUE)
  
  results_alltrade <- data.frame(prov = rownames(plugin_alltrade$beta), b_pre = plugin_alltrade$beta_pre, b = plugin_alltrade$beta, se = 0)
  results_alltrade$se <- plugin_alltrade$ses[1,]
  results_alltrade
  
  ## 2 交叉验证Cross-validation确定惩罚参数
  # id <- unique(data_sdid$alt_id) # 按照国家对/贸易协定ID分折
  # nfolds <- 10
  # unique_ids <- data.frame(alt_id = id, fold = sample(1:nfolds, size = length(id), replace = TRUE))
  # cross_ids <- merge(data_sdid[, "alt_id", drop = FALSE], unique_ids, by = "alt_id", all.x = TRUE)

  set.seed(20260304) # 设置随机种子以确保结果可重复
  id <- unique(data_sdid[, 5]) # 按照协定分折
  nfolds <- 10
  unique_ids <- data.frame(id = id, fold = sample(1:nfolds, size = length(id), replace = TRUE))
  cross_ids <- merge(data_sdid[, 5, drop = FALSE], unique_ids, by = "id", all.x = TRUE)
  
  # lambdas <- c(0.025, 0.01, 0.0075, 0.005, 0.0025, 0.001, 0.00075, 0.0005, 0.00025, 0.0001, 0.00005, 0.00001)
  lambdas <- c(0.03, 0.009, 0.006, 0.003, 0.0009, 0.0006, 0.0003, 0.00009, 0.00006, 0.00003, 0.00001)
  
  cv_alltrade <- mlfitppml(data = data_sdid,
                    indep = 8:503,
                    dep = "trade",
                    penalty = "lasso",
                    fixed = list("exp_time","imp_time","pair"),
                    lambdas = lambdas,
                    xval = TRUE,
                    IDs =  cross_ids$fold)
  cv_alltrade$rmse
  
  # 绘图(1)：Cross-validation MSE vs. tuning parameter
    # 提取核心数据
  cv_data <- data.frame(
    log_lambdas = log(cv_alltrade$lambdas),
    cv_mse = cv_alltrade$rmse[, 2]
  )
  
  cv_mse <- ggplot(cv_data, aes(x = log_lambdas, y = cv_mse)) +
    # 损失折线
    geom_line(color = "black", linewidth = 0.5) +
    # 学术图表标签
    labs(x = "Log of the scaled tuning parameter", 
         y = "Cross-validation MSE") +
    # 简洁主题，适配论文
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9),
          panel.grid = element_blank())
  
  save_path <- "C:/Users/ZaihangZhang/OneDrive/MasterThesis/MachineLearninginAgriculturalTradeResearch/results/figures"
  ggsave(file.path(save_path, "CV_mse.png"), plot = cv_mse, width = 7, height = 5, dpi = 300)
  
  # 绘图(2)：Regularization path for selected interactions
    #I 提取系数矩阵(Variables x Lambdas)
    beta_mat <- cv_alltrade$beta
    colnames(beta_mat) <- as.character(cv_alltrade$lambdas)
      # 将矩阵转换为数据框，并添加变量名列
    plot_data <- as.data.frame(beta_mat)
    plot_data$Variable <- rownames(beta_mat)
      # 转为长格式，每一行代表：一个变量在一个Lambda下的系数值
    plot_data_long <- plot_data %>%
      pivot_longer(cols = -Variable, names_to = "Lambdas", values_to = "Coefficient") %>%
      mutate(
        Lambdas = as.numeric(Lambdas)
      )
    
    #II 变量分组
    vars_per_plot <- 50 # 设定每张图显示的变量数量 (建议 20-50 之间)
    all_vars <- unique(plot_data_long$Variable)
      # 创建一个查找表，给每个变量分配一个 Group ID
    var_groups <- data.frame(
      Variable = all_vars,
      Group_ID = ceiling(seq_along(all_vars) / vars_per_plot)
    )
      # 将分组信息合并回主数据
    plot_data_final <- plot_data_long %>%
      left_join(var_groups, by = "Variable")
      # 确保变量在Y轴上的顺序是固定的（例如按原顺序或聚类顺序）
    plot_data_final$Variable <- factor(plot_data_final$Variable, levels = rev(all_vars))
    
    #III 循环绘图
      # 我们获取所有唯一的 Lambda 值，并按降序排列 (从大到小)
    unique_lambdas <- sort(unique(plot_data_final$Lambdas), decreasing = TRUE)
      # 为了显示整洁，保留5位小数（根据需要调整），并转为字符
    lambda_labels <- formatC(unique_lambdas, format = "f", digits = 5)
    
      # 将数据中的 Lambda_Value 转换为因子，指定 levels 顺序为从大到小
      # 这样 ggplot 就会认为它们是离散的类别，从而等距排列
    plot_data_final$Lambdas_Factor <- factor(
      formatC(plot_data_final$Lambdas, format = "f", digits = 5),
      levels = lambda_labels
    )
      # 获取总组数
    total_groups <- max(plot_data_final$Group_ID)
    plot_list <- list()
    
    for (i in 1:total_groups) {
      
      # 筛选当前组的数据
      sub_data <- plot_data_final %>% filter(Group_ID == i)
      
      # 绘图
      p_heatmap <- ggplot(sub_data, aes(x = Lambdas_Factor, y = Variable, fill = Coefficient)) +
        geom_tile() +
        # 使用发散色系：蓝色为负，白色为0，红色为正 (区分度高)
        scale_fill_gradient2(
          low = "#2166AC",   # 负值颜色 (深蓝)
          mid = "white",     # 零值颜色
          high = "#B2182B",  # 正值颜色 (深红)
          midpoint = 0,
          name = "Coefficient"
        ) +
        # 对 X 轴 (Lambda) 进行对数变换，通常正则化路径在对数尺度下更好看
        scale_x_discrete() + 
        labs(
          title = paste0("Regularization Path Heatmap - Part ", i, "/", total_groups),
          x = "Lambdas",
          y = "Variables"
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_text(size = 7), # 调整Y轴字体大小
          axis.text.x = element_text(angle = 45, hjust = 1), # 旋转X轴标签，防止Lambda数值重叠（如果有需要）
          panel.grid = element_blank() # 去除网格线，让热图更清晰
        )
      
      # 将图存入列表
      plot_list[[i]] <- p_heatmap
      
      # 如果你想直接保存图片，可以取消下面代码的注释
      heatmap_name <- paste0("PPML_Lasso_正则化路径热图_组", i, ".png")
      ggsave(filename = file.path(save_path, heatmap_name), plot = p_heatmap, width = 10, height = 8, dpi = 300)
    }
    
    #IV 展示结果
      # 查看第一张图 (前50个变量)
    print(plot_list[[1]])
      # 查看第二张图 (第51-100个变量)
    print(plot_list[[2]])
  
  
    # ==========================================
    # 提取和计算非零系数的描述性统计
    # ==========================================
    coef_stats <- plot_data_long %>%
    # 1. 过滤出被选中的系数 (系数不等于 0)
    # 注意：由于浮点数精度问题，有时 0 可能会是极小的数，可以设定一个极小的阈值如 1e-6
    filter(abs(Coefficient) > 1e-6) %>%
  
    # 2. 按 Lambda 值分组
    group_by(Lambdas) %>%
  
    # 3. 计算各个统计量
    summarise(
    # 被选中的变量个数 (自由度/非零系数数)
    Num_Selected_Vars = n(), 
    
    # 描述性统计
    Mean = mean(Coefficient),
    SD = sd(Coefficient),
    Min = min(Coefficient),
    Median = median(Coefficient),
    Max = max(Coefficient),
    
    .groups = "drop" # 解除分组
  )
    # ==========================================
    # 展示结果
    # ==========================================
    # 查看结果的前几行
  print(head(coef_stats))

    # 如果您希望按照 Lambda 值从大到小排列 (如前所述)
  coef_stats <- coef_stats %>%
    arrange(desc(Lambdas))

  print(coef_stats)




    
  # 计算非零平均数
  non_zero_means <- apply(beta_mat, 2, function(col) {
    non_zero_values <- col[col != 0]
    if (length(non_zero_values) == 0) {
      return(0)  # 或者 return(0)，取决于你希望如何处理全是0的列
    } else {
      return(mean(non_zero_values))
    }
  })
  print(non_zero_means)
  # 显示非零观测值数量
  non_zero_counts <- colSums(beta_mat != 0)
  print(non_zero_counts)
  
  
  