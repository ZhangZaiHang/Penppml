import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches

# ==================== 1. 数学模型与切点精准计算 ====================
# 定义 OLS 全局最优解 (中心点)
beta1, beta2 = 1.5, 2.5

# 定义二次型损失函数 (带协方差的椭圆)
def compute_Z(w1, w2):
    return (w1 - beta1)**2 + 0.8 * (w1 - beta1) * (w2 - beta2) + (w2 - beta2)**2

# --- 计算 Lasso (L1) 切点与最大等高线值 ---
lasso_tangent_x, lasso_tangent_y = 0.0, 1.0
z_lasso_max = compute_Z(lasso_tangent_x, lasso_tangent_y)

# --- 计算 Ridge (L2) 切点与最大等高线值 ---
theta_arr = np.linspace(0, 2 * np.pi, 2000)
circle_x, circle_y = np.cos(theta_arr), np.sin(theta_arr)
Z_circle = compute_Z(circle_x, circle_y)
min_idx = np.argmin(Z_circle)
ridge_tangent_x, ridge_tangent_y = circle_x[min_idx], circle_y[min_idx]
z_ridge_max = Z_circle[min_idx]

# --- 核心修改：动态生成等高线层级 ---
# 使用开方后等距的方法生成层级，这样内圈密集，外圈稀疏，更符合二次函数的视觉效果
# 并且严格保证最后一个值等于 z_max，也就是最外圈必然是切线！
num_levels = 5  # 你可以修改这个数值来决定画几个圈
levels_ridge = np.linspace(0.3, np.sqrt(z_ridge_max), num_levels)**2
levels_lasso = np.linspace(0.3, np.sqrt(z_lasso_max), num_levels)**2

# 生成网格数据用于画底图
w1_grid = np.linspace(-1.5, 3.5, 500)
w2_grid = np.linspace(-1.5, 3.5, 500)
W1, W2 = np.meshgrid(w1_grid, w2_grid)
Z = compute_Z(W1, W2)

# ==================== 2. 图形初始化与样式设置 ====================
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 6), facecolor='white')

def format_axes(ax):
    ax.spines['left'].set_position('zero')
    ax.spines['bottom'].set_position('zero')
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    ax.set_xticks([])
    ax.set_yticks([])
    ax.set_xlim(-1.5, 3.5)
    ax.set_ylim(-1.5, 3.5)
    ax.set_aspect('equal')
    
    # 添加黑色箭头
    ax.plot(3.5, 0, ">k", clip_on=False, markersize=6)
    ax.plot(0, 3.5, "^k", clip_on=False, markersize=6)
    
    # 添加斜体数学轴标签
    ax.text(3.3, -0.3, r'$\theta_1$', fontsize=14)
    ax.text(-0.3, 3.3, r'$\theta_2$', fontsize=14)

# 学术配色
contour_color = '#555555'      # 等高线深灰
fill_color = '#E5E5E5'         # 约束域浅灰
edge_color = 'black'           # 边界黑
line_width = 1.2

# ==================== 3. 绘制 Ridge (左图) ====================
format_axes(ax1)
ax1.set_title("Ridge\n$J = ||e||_2 + ||\\theta||_2$", fontsize=16, pad=20)

# 绘制等高线，最外圈即为 z_ridge_max
ax1.contour(W1, W2, Z, levels=levels_ridge, colors=contour_color, linewidths=line_width, zorder=1)

# 绘制圆
circle = patches.Circle((0, 0), 1, facecolor=fill_color, edgecolor=edge_color, linewidth=line_width, zorder=2)
ax1.add_patch(circle)

# 标记点
ax1.plot(beta1, beta2, 'ko', markersize=5, zorder=3)
ax1.plot(ridge_tangent_x, ridge_tangent_y, 'ko', markersize=5, zorder=3)
ax1.text(ridge_tangent_x + 0.1, ridge_tangent_y - 0.25, r'$\hat{\theta}$', fontsize=12)

# ==================== 4. 绘制 Lasso (右图) ====================
format_axes(ax2)
ax2.set_title("Lasso\n$J = ||e||_2 + ||\\theta||_1$", fontsize=16, pad=20)

# 绘制等高线，最外圈即为 z_lasso_max
ax2.contour(W1, W2, Z, levels=levels_lasso, colors=contour_color, linewidths=line_width, zorder=1)

# 绘制菱形
diamond = patches.Polygon([(-1,0), (0,1), (1,0), (0,-1)], 
                          facecolor=fill_color, edgecolor=edge_color, linewidth=line_width, zorder=2)
ax2.add_patch(diamond)

# 标记点
ax2.plot(beta1, beta2, 'ko', markersize=5, zorder=3)
ax2.plot(lasso_tangent_x, lasso_tangent_y, 'ko', markersize=5, zorder=3)
ax2.text(lasso_tangent_x - 0.2, lasso_tangent_y + 0.15, r'$\hat{\theta}$', fontsize=12)

# ==================== 5. 添加学术注释和箭头 ====================
arrow_props = dict(facecolor='black', edgecolor='black', arrowstyle='->', lw=1.0)

ax2.annotate('Data term only:\nall $\\theta_i$ non-zero', 
             xy=(beta1, beta2), xytext=(1.8, 1.2),
             arrowprops=arrow_props, fontsize=11, color='black', ha='left')

ax2.annotate('Regularized estimate:\nsome $\\theta_i$ may be zero', 
             xy=(lasso_tangent_x, lasso_tangent_y), xytext=(1.3, 0.2),
             arrowprops=arrow_props, fontsize=11, color='black', ha='left')

plt.tight_layout()

plt.savefig(r'C:\Users\ZaihangZhang\OneDrive\MasterThesis\MachineLearninginAgriculturalTradeResearch\results\figures\L1_L2_regularization_comparison_clean.png', dpi=300, bbox_inches='tight')

plt.show()