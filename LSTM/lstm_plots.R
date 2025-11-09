library(tidyverse)
library(readxl)

# Plot Optimal Lag Mean
lags = read_excel("optimal_lag_mean.xlsx") %>% 
  mutate(lags = 1:8)

ggplot(lags, aes(x = lags, y = rmse)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Optimális késleltetés kiválasztása az RMSE alapján az LSTM modellekhez",
       x = "Eredményváltozó legmagasabb időbeli késleltetése",
       y = "Átlagos RMSE (Forintban)") +
  scale_x_continuous(breaks = 1:8) +
  theme_minimal()+
  theme(text = element_text(size = 16))

ggsave("plots/optimal_lag_mean.png", width = 16, height = 8)

# Plot c.p. results
cp_results = read_csv("ceteris_paribus_results.csv")

ggplot(cp_results, aes(x = pct_change, y = prediction, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "Ceteris Paribus elemzés az LSTM modellekhez",
       x = "Változó százalékos változása",
       y = "Predikció (Forintban)",
       color = "Változó") +
  theme_minimal()+
  theme(text = element_text(size = 16))
