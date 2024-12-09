# 设置随机种子，确保结果可重复
set.seed(42)

# 模拟自变量 Working_hours，范围为100到200小时（生成整数）
Working_hours <- sample(100:200, 100, replace = TRUE)

# 增强回归关系：减少误差的影响，增加自变量对因变量的影响
# 假设回归模型为： Monthly_salary = 2 * Working_hours + 1000 + 误差项
# 误差项服从正态分布，生成整数误差项
error <- round(rnorm(100, mean = 0, sd = 30))

# 计算因变量 Monthly_salary，结果四舍五入为整数
Monthly_salary <- round(3 * Working_hours + 3000 + error)

# 创建数据框
data <- data.frame(Working_hours, Monthly_salary)

# 查看前几行数据
head(data)

# 计算回归模型
model <- lm(Monthly_salary ~ Working_hours, data = data)

# 输出回归结果和判定系数 R-squared
summary(model)

# 导出数据到CSV文件
write.csv(data, "employee_salary_data.csv", row.names = FALSE)


# 设置随机种子，确保结果可重复
set.seed(42)

# 模拟自变量 Working_hours，范围为100到200小时（生成整数）
Working_hours <- sample(100:200, 100, replace = TRUE)

# 假设回归模型为： Health_index = 100 - 0.3 * Working_hours + 误差项
# 误差项服从正态分布，生成整数误差项
error <- round(rnorm(100, mean = 0, sd = 5))

# 计算因变量 Health_index，结果四舍五入为整数
Health_index <- round(100 - 0.3 * Working_hours + error)

# 创建数据框
data <- data.frame(Working_hours, Health_index)

# 查看前几行数据
head(data)

# 计算回归模型
model <- lm(Health_index ~ Working_hours, data = data)

# 输出回归结果和判定系数 R-squared
summary(model)

# 导出数据到CSV文件
write.csv(data, "employee_health_data.csv", row.names = FALSE)

