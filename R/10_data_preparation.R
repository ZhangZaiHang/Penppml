# ==============================================================================
# 0. 加载必要的包
# ==============================================================================
# 如果没有安装，请取消下面两行的注释进行安装
# install.packages("haven")      # 用于读取 .dta 文件
# install.packages("data.table") # 用于高效数据处理

library(haven)
library(data.table)

# ==============================================================================
# 1. 导入 Stata 数据 (.dta)
# ==============================================================================
# 请将路径替换为你实际的文件路径
dta_path <- "C:/Users/ZaihangZhang/OneDrive/MasterThesis/MachineLearninginAgriculturalTradeResearch/data/usr/data_sdid_small.dta" 

# 读取数据
data_sdid_samll <- read_dta(dta_path)

# 将数据转换为 data.table 格式 (原地转换，速度极快)
# 这一步对于处理贸易引力模型这种大数据集至关重要
dss <- as.data.table(data_sdid_samll)

# 释放原始数据的内存 (可选)
rm(data_sdid_samll)
gc()

# ==============================================================================
# 2. 复刻 Stata 逻辑：生成 f{year} 和 cohort{year}
# ==============================================================================

# ------------------------------------------------------------------
# Stata 原文: 
# forvalues i = 2000/2019 {
#     g f`i' = year == `i'
# }
# ------------------------------------------------------------------

# 批量生成 f2000 - f2019
# 注意：我们直接在循环中操作，data.table 的 := 语法会原地修改数据，不占用额外内存
for (i in 1987:2019) {
  col_name <- paste0("f", i)
  
  # 逻辑：如果 year 等于 i，则为 1，否则为 0
  # as.integer 用于确保生成的是整数类型 (0/1)，节省内存
  dss[, (col_name) := as.integer(year == i)]
}

# ------------------------------------------------------------------
# Stata 原文: 
# egen cohort = sum(RTA), by(id_ci_cj)
# ------------------------------------------------------------------

# 按 id_ci_cj 分组计算 RTA 的总和
# na.rm = TRUE 对应 Stata 默认忽略缺失值的行为
dss[, cohort := sum(RTA, na.rm = TRUE), by = id_ci_cj]

# ------------------------------------------------------------------
# Stata 原文: 
# forvalues i = 2000/2019 {
#     g cohort`i' = cohort == 2020 - `i'
#     replace cohort`i' = 0 if cohort`i' == .
# }
# ------------------------------------------------------------------

for (i in 1987:2019) {
  col_name <- paste0("cohort", i)
  target_val <- 2020 - i
  
  # 1. 生成虚拟变量 (TRUE 转 1, FALSE 转 0)
  # 注意：如果 cohort 是 NA，这里的比较结果也是 NA
  dss[, (col_name) := as.integer(cohort == target_val)]
  
  # 2. 处理缺失值 (对应 Stata 的 replace ... = 0 if ... == .)
  # 在 data.table 中，我们使用 is.na() 快速定位并填充 0
  dss[is.na(get(col_name)), (col_name) := 0]
}

# ==============================================================================
# 3. (可选) 查看结果或保存
# ==============================================================================

# 查看前几行
head(dss)

# 如果需要将处理后的数据导回 Stata (保存为 .dta)
# write_dta(dt, "C:/Your/Path/To/processed_data.dta")

# 保存处理好的 data.table 对象
saveRDS(dss, file = "data_sdid_small_etwfe.rds")



# 生成年份变量和队列变量
# for (i in 1986:2021) {
#  cohortyear_sdid_small[[paste0("f", i)]] <- ifelse(cohortyear_sdid_small$year == i, 1, 0)
# }

# cohortyear_sdid_small <- cohortyear_sdid_small %>%
#  group_by(id_ci_cj) %>%
#  mutate(cohort = sum(RTA, na.rm = TRUE)) %>%
#  ungroup()
# for (i in 1986:2021) {
#  cohortyear_sdid_small[[paste0("cohort", i)]] <- ifelse(cohortyear_sdid_small$cohort == 2022-i, 1, 0)
# }

### 生成模型必要回归变量
# 设置年份范围
years <- 1987:2019
# 循环生成交互变量
for (c in years) {
  for (y in years) {
    if (y >= c) {
      cohort_var <- paste0("cohort", c)
      fyear_var <- paste0("f", y)
      new_var <- paste0("cohort", c, "f", y)
      # 创建交互项变量
      dss[[new_var]] <- dss[[cohort_var]] * dss[[fyear_var]]
    }
  }
}

saveRDS(dss, file = "data_sdid_small_etwfe.rds")