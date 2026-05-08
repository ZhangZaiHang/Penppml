import matplotlib.pyplot as plt
import numpy as np

from sklearn.linear_model import LinearRegression
from sklearn.model_selection import cross_val_score
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import PolynomialFeatures

# 解决中文乱码问题
plt.rcParams["font.sans-serif"]=["SimSun"]
plt.rcParams["font.family"]="sans-serif"

def true_fun(X):
    return np.cos(1.5 * np.pi * X)

np.random.seed(0)

n_samples = 30
degrees = [1, 4, 15]
titles = ["欠拟合","理想拟合","过拟合"]

X = np.sort(np.random.rand(n_samples))
y = true_fun(X) + np.random.randn(n_samples) * 0.1

plt.figure(figsize=(14, 5))
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

    # 使用交叉验证评估模型
    scores = cross_val_score(
        pipeline, X[:, np.newaxis], y, scoring="neg_mean_squared_error", cv=10
    )

    X_test = np.linspace(0, 1, 100)
    plt.plot(X_test, pipeline.predict(X_test[:, np.newaxis]), 'b-', label="预测模型")
    plt.plot(X_test, true_fun(X_test), 'r--', label="真实模型")
    plt.scatter(X, y, edgecolor="r", c="r", s=20, label="原始数据")
    plt.xlabel("x", fontsize=15)
    plt.ylabel("y", fontsize=15)
    plt.xlim((0, 1))
    plt.ylim((-2, 2))
    plt.legend(loc="best")
    plt.title(
        "{} \n MSE = {:.2e}(+/- {:.2e})".format(
            titles[i], -scores.mean(), scores.std()
        ),
        fontsize = 15
    )
plt.show()
