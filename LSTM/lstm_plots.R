library(tidyverse)
library(readxl)

# Plot Optimal Lag Mean
lags = read_excel("optimal_lag_mean.xlsx") %>% 
  mutate(lags = 1:8)

ggplot(lags, aes(x = lags, y = rmse)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    # title = "Optimális késleltetés kiválasztása az RMSE alapján az LSTM modellekhez",
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
  group_by(variable, pct_change) %>%
  arrange(sample_idx, .by_group = TRUE) %>%
  summarise(
    diff = {
      idxs <- as.integer(sample_idx)
      if (any(idxs == 0)) idxs <- idxs + 1L
      actuals <- y_test[idxs]
      mean(prediction / actuals - 1)
    },
    .groups = "drop_last"
  ) %>%
  ungroup()

var_map = data.frame(
  mapped = c("Y[-2]", "Y[-1]", "FED", "MNB", "ECB", "BUX", "CETOP",
               "MGY", "VM", "NM", "KI", "EUR/CZK", "EUR/PLN", "EUR/RON"),
  original = c("Y_-2", "Y_-1", "FED", "MNB", "ECB", "BUX", "cetop",
             "mean_matolcsy", "mean_varga", "mean_nagy", "mean_kinfo",
             "eurczk", "eurpln", "eurron"),
  group = c("Régiós FX piac és Y időbeli késleltetettjei",
            "Régiós FX piac és Y időbeli késleltetettjei",
            "Alapkamatok", "Alapkamatok", "Alapkamatok",
            "Régiós tőkepiacok", "Régiós tőkepiacok",
            "Gazdasági döntéshozók", "Gazdasági döntéshozók",
            "Gazdasági döntéshozók", "Gazdasági döntéshozók", 
            "Régiós FX piac és Y időbeli késleltetettjei",
            "Régiós FX piac és Y időbeli késleltetettjei",
            "Régiós FX piac és Y időbeli késleltetettjei"),
  stringsAsFactors = FALSE
)

variable_colors <- c(
  "Y[-1]"     = "#00BA38",
  "Y[-2]"     = "#F8766D",
  "EUR/RON"   = "#619CFF",
  "EUR/CZK"   = "#F564E3",
  "EUR/PLN"   = "#00BFC4",
  "MNB"       = "#00BA38",
  "FED"       = "#F8766D",
  "ECB"       = "#619CFF",
  "BUX"       = "#F8766D",
  "CETOP"     = "#00BA38",
  "MGY"       = "#F8766D",
  "VM"        = "#00BA38",
  "NM"        = "#619CFF",
  "KI"        = "#F564E3"
)

a = a %>% 
  left_join(var_map, by = c("variable" = "original"))


ggplot(a, aes(x = pct_change, y = diff, color = mapped)) +
  geom_line(linewidth = 1) +
  labs(
    # title = "Ceteris Paribus elemzés az LSTM modellekhez",
       x = "Változó c.p. változása (%)",
       y = "Eredményváltozóra mért hatás (%)",
       color = "Változó") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1e-2)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  # scale_color_viridis_d(option = "turbo") +
  scale_color_manual(values = variable_colors, breaks = names(variable_colors)) +
  facet_wrap(~ factor(group, levels = c("Régiós FX piac és Y időbeli késleltetettjei",
                                        "Régiós tőkepiacok",
                                        "Alapkamatok",
                                        "Gazdasági döntéshozók"
                                        )), ncol = 2, scales = "free") +
  theme_minimal()+
  theme(text = element_text(size = 16))

ggsave("plots/ceteris_paribus_lstm_grid.png", width = 16, height = 8)

