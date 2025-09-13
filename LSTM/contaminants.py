import torch
import torch.nn as nn
from torch.utils.data import DataLoader, TensorDataset
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler

data = pd.read_csv("Centro")
data = data[["PM10 (ug/m3)","Fecha y hora"]]
data["Fecha y hora"] = pd.to_datetime(data["Fecha y hora"], unit="s")

# Extract UTC hour of day
data["díasem"] = data["Fecha y hora"].dt.day_of_week
#data["mes"] = data["Fecha y hora"].dt.month
data["Fecha y hora"] = data["Fecha y hora"].dt.hour

print(data.tail())
# scale features
scaler_x = MinMaxScaler()
scaler_y = MinMaxScaler()
horizon = 1
lookback = 10
features = data.drop(columns=["PM10 (ug/m3)"])
target = data[["PM10 (ug/m3)"]]

scaled_features = scaler_x.fit_transform(features)
scaled_target = scaler_y.fit_transform(target)

data_scaled = np.hstack([scaled_target, scaled_features])  

# ---------------------------
# Create sequences
def create_sequences(data, seq_length, horizon):
    xs, ys = [], []
    for i in range(len(data) - seq_length - horizon + 1):
        x = data[i:i+seq_length, 1:]        # input features
        y = data[i+seq_length:i+seq_length+horizon, 0] 
        xs.append(x)
        ys.append(y)
    xs = np.array(xs, dtype=np.float32)
    ys = np.array(ys, dtype=np.float32)   # shape = (samples, horizon)
    return torch.from_numpy(xs), torch.from_numpy(ys)

train_size = int(len(data_scaled) * 0.81)

train_data = data_scaled[:train_size]
test_data  = data_scaled[train_size:]

x_train, y_train = create_sequences(train_data, lookback,horizon)
x_test, y_test   = create_sequences(test_data, lookback,horizon)

# ---------------------------
# Define LSTM
# ---------------------------
class LSTM(nn.Module):
    def __init__(self, input_size, hidden_size, num_layers, output_size):
        super().__init__()
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        self.lstm = nn.LSTM(input_size, hidden_size, num_layers, batch_first=True)
        self.fc = nn.Linear(hidden_size, output_size)
    
    def forward(self, x):
        out, _ = self.lstm(x)
        out = self.fc(out[:, -1, :])  # take last time step
        return out

input_size = x_train.shape[2]  # 8
hidden_size = 50
num_layers = 2
output_size = horizon

model = LSTM(input_size, hidden_size, num_layers, output_size)

# ---------------------------
# Training setup
# ---------------------------
criterion = nn.MSELoss()
optimizer = torch.optim.Adam(model.parameters(), lr=0.001)
num_epochs = 11
batch_size = 16

train_loader = DataLoader(TensorDataset(x_train, y_train), batch_size=batch_size, shuffle=True)

# ---------------------------
# Training loop
# ---------------------------
for epoch in range(num_epochs):
    model.train()
    for X_batch, y_batch in train_loader:
        optimizer.zero_grad()
        y_pred = model(X_batch)
        loss = criterion(y_pred, y_batch)
        loss.backward()
        optimizer.step()
    
    if (epoch+1) % 5 == 0: # validate every 5 epochs
        model.eval()
        with torch.no_grad():
            train_pred = model(x_train)
            test_pred = model(x_test)
            train_rmse = torch.sqrt(criterion(train_pred, y_train)).item()
            test_rmse = torch.sqrt(criterion(test_pred, y_test)).item()
        print(f"Epoch {epoch+1}: Train RMSE={train_rmse:.4f}, Test RMSE={test_rmse:.4f}")

# ---------------------------
# Predict and inverse scale
# ---------------------------
model.eval()
with torch.no_grad():
    # Initialize arrays with NaN
    train_plot = np.ones_like(data["PM10 (ug/m3)"]) * np.nan
    test_plot  = np.ones_like(data["PM10 (ug/m3)"]) * np.nan

    # ---- Train predictions ----
    y_pred_train = model(x_train).cpu().numpy()   # shape (n_train_samples, 24)
    y_pred_test  = model(x_test).cpu().numpy()    # shape (n_test_samples, 24)

    # inverse transform each horizon
    y_train_pred_inv = scaler_y.inverse_transform(y_pred_train)
    y_test_pred_inv  = scaler_y.inverse_transform(y_pred_test)

    # Train: step ahead 24 each time
    i = 0
    while i < len(y_pred_train):
        start_idx = lookback + i
        end_idx   = start_idx + horizon
        train_plot[start_idx:end_idx] = y_train_pred_inv[i]
        i += horizon   # jump ahead 24

    # Test: step ahead 24 each time
    i = 0
    while i < len(y_pred_test):
        start_idx = train_size + lookback + i
        end_idx   = start_idx + horizon
        test_plot[start_idx:end_idx] = y_test_pred_inv[i]
        i += horizon   # jump ahead 24

# ---------------------------
# Plot results
# ---------------------------
plt.figure(figsize=(12,5))
plt.plot(data["PM10 (ug/m3)"], label="Actual", c='k')
plt.plot(train_plot, label="Train Prediction", c='r')
plt.plot(test_plot, label="Test Prediction", c='g')
# Extract day of week (0 = Monday, ..., 6 = Sunday)

# Indices where Sunday starts
sundays = data.index[data["díasem"] == 6]

for idx in sundays:
    plt.axvline(x=idx, color='blue', linestyle=':', alpha=0.4, label="Sunday" if idx == sundays[0] else "")



plt.legend()
plt.show()

