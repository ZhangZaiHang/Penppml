import matplotlib.pyplot as plt
import numpy as np

from sklearn.linear_model import LinearRegression
from sklearn.model_selection import cross_val_score
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import PolynomialFeatures

# 解决中文乱码问题
plt.rcParams["font.sans-serif"] = ["SimSun"]  # 如果在非Windows环境可能需要改为 'Arial Unicode MS' 或 'Hei'
plt.rcParams["axes.unicode_minus"] = False    # 解决负号显示问题
plt.rcParams["font.family"] = "sans-serif"

def true_fun(X):
    """
    模拟引力模型中的距离衰减效应。
    贸易额 (y) 随 距离 (X) 增加而呈现非线性下降。
    这里使用指数衰减函数模拟：Trade = A * exp(-B * Distance)
    """
    return 3 * np.exp(-3 * X)

np.random.seed(42) # 固定随机种子以复现结果

n_samples = 30
# 阶数选择：
# 1阶：简单的线性回归（欠拟合，无法捕捉边际递减规律）
# 4阶：能较好拟合曲线趋势（理想拟合）
# 15阶：为了强行穿过每个噪点而产生剧烈震荡（过拟合）
degrees = [1, 4, 15]
titles = ["欠拟合 (线性设定)", "理想拟合 (捕捉引力规律)", "过拟合 (过度敏感)"]

# X 代表“两国间的地理距离”（归一化到 0-1 之间，0代表邻国，1代表地球对面）
X = np.sort(np.random.rand(n_samples))

# y 代表“农产品贸易额”
# 真实规律 + 随机噪音 (噪音代表关税、文化差异、自贸协定等非距离因素的扰动)
y = true_fun(X) + np.random.randn(n_samples) * 0.3

plt.figure(figsize=(15, 6))

for i in range(len(degrees)):
    ax = plt.subplot(1, len(degrees), i + 1)
    plt.setp(ax, xticks=(), yticks=())

    polynomial_features = PolynomialFeatures(degree=degrees[i], include_bias=False)
    linear_regression = LinearRegression()
    pipeline = Pipeline(
        [
            ("polynomial_features", polynomial_features),
            ("linear_regression", linear_regression),
        ]
    )
    pipeline.fit(X[:, np.newaxis], y)

    # 使用交叉验证评估模型 (使用负均方误差)
    scores = cross_val_score(
        pipeline, X[:, np.newaxis], y, scoring="neg_mean_squared_error", cv=10
    )

    X_test = np.linspace(0, 1, 100)
    
    # 绘图部分
    plt.plot(X_test, pipeline.predict(X_test[:, np.newaxis]), 'b-', linewidth=2, label="估计模型")
    plt.plot(X_test, true_fun(X_test), 'r--', linewidth=2, label="真实引力规律")
    plt.scatter(X, y, edgecolor="k", c="orange", s=40, label="观察到的贸易数据")
    
    plt.xlabel("地理距离 (Distance)", fontsize=12)
    plt.ylabel("农产品贸易额 (Trade Flow)", fontsize=12)
    plt.xlim((0, 1))
    plt.ylim((-1, 4)) # 根据指数函数的范围调整Y轴
    
    plt.legend(loc="upper right", fontsize=10)
    plt.title(
        "{} \n MSE = {:.2e}(+/- {:.2e})".format(
            titles[i], -scores.mean(), scores.std()
        ),
        fontsize=14
    )

plt.suptitle("农业贸易引力模型中的过拟合问题示意", fontsize=18, y=1.05)
plt.tight_layout()
plt.show()