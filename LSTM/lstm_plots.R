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

y_test = read_csv("full_dataset_20250830.csv") %>% 
  tail(175) %>% 
  .$eur_close

a = cp_results %>%
  group_split(variable) %>%
  map_df(~{
    var_name <- unique(.x$variable)
    
    mean_preds <- .x %>%
      group_by(pct_change) %>%
      summarise(mean_prediction = mean(prediction), .groups = "drop")
    
    actual <- head(y_test, nrow(mean_preds))
    diff <- mean_preds$mean_prediction / mean(actual) - 1
    
    tibble(
      variable = var_name,
      pct_change = mean_preds$pct_change,
      mean_prediction = mean_preds$mean_prediction,
      diff = diff
    )
  })

var_map = data.frame(
  mapped = c("Y[-2]", "Y[-1]", "FED", "MNB", "ECB", "BUX", "CETOP",
               "MGY", "VM", "NM", "KI", "EUR/CZK", "EUR/PLN", "EUR/RON"),
  original = c("Y_-2", "Y_-1", "FED", "MNB", "ECB", "BUX", "cetop",
             "mean_matolcsy", "mean_varga", "mean_nagy", "mean_kinfo",
             "eurczk", "eurpln", "eurron"),
  stringsAsFactors = FALSE
)

a = a %>% 
  left_join(var_map, by = c("variable" = "original"))


ggplot(a, aes(x = pct_change, y = diff, color = mapped)) +
  geom_line(linewidth = 1) +
  labs(title = "Ceteris Paribus elemzés az LSTM modellekhez",
       x = "Változó c.p. változása (%)",
       y = "Eredményváltozóra mért hatás (%)",
       color = "Változó") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_viridis_d(option = "turbo") +
  theme_minimal()+
  theme(text = element_text(size = 16))

ggsave("plots/ceteris_paribus_lstm.png", width = 16, height = 8)

