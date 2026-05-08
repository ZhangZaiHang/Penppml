import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics import mean_squared_error

x = np.linspace(0, 10, 100)
y_true = np.sin(x)
y_pred = y_true + np.random.normal(0, 0.1, 100)

mse = mean_squared_error(y_true, y_pred)
print("MSE:", mse)

plt.figure(figsize=(8, 6))
plt.plot(x, y_true, label='True Values')
plt.plot(x, y_pred, label='Predicted Values')
plt.title('MSE Curve')
plt.xlabel('X')
plt.ylabel('Y')
plt.legend()
plt.show()