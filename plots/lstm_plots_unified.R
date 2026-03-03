setwd("C:/repos/TDK-Eur-Huf")

library(tidyverse)
library(readxl)

theme_set(
  theme_minimal() +
    theme(legend.position = "bottom")
)

## Optimal lags -----------------

optimal_lags = read_xlsx("LSTM/optimal_lag_mean.xlsx")

optimal_lags$lag = 1:8

ggplot(optimal_lags, aes(x = lag, y = rmse))+
  geom_line() +
  labs(x = "Lag in the dependent variable", y = "Mean RMSE")

ggsave("plots/lag_rmse.pdf")

## Ceteris paribus -------------

cet_par = read.csv("LSTM/ceteris_paribus_results.csv")

y_test = read_csv("LSTM/full_dataset_20250830.csv") %>% 
  tail(175) %>% 
  .$eur_close

cp_aggregated = cet_par %>%
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
  group = c("Regional FX market and Y time lags",
            "Regional FX market and Y time lags",
            "Base rates", "Base rates", "Base rates",
            "Regional capital markets", "Regional capital markets",
            "Economic decision makers", "Economic decision makers",
            "Economic decision makers", "Economic decision makers", 
            "Regional FX market and Y time lags",
            "Regional FX market and Y time lags",
            "Regional FX market and Y time lags"),
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

cp_aggregated = cp_aggregated %>% 
  left_join(var_map, by = c("variable" = "original"))

ggplot(cp_aggregated, aes(x = pct_change, y = diff, color = mapped)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Ceteris paribus change in variable (%)",
    y = "Change in the dependent variable (%)",
    color = "Variable") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1e-2)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = variable_colors, breaks = names(variable_colors)) +
  facet_wrap(~ factor(group, levels = c("Regional FX market and Y time lags",
                                        "Regional capital markets",
                                        "Base rates",
                                        "Economic decision makers"
  )), ncol = 2, scales = "free")

ggsave("plots/ceteris_paribus_lstm_grid.pdf")

### EUR/HUF timeseries ------------------------

df_full = read.csv("LSTM/full_dataset_20250830.csv")


df_long = df_full %>%
  rename(y = eur_close) %>% 
  mutate(kinfo = ifelse(mean_kinfo != 0, y, NA),
         vm = ifelse(mean_varga != 0, y, NA),
         nm = ifelse(mean_nagy != 0, y, NA),
         mgy = ifelse(mean_matolcsy != 0, y, NA),
         Date = as.Date(Date)) %>%
  select(Date, y, kinfo, vm, nm, mgy) %>% 
  pivot_longer(cols = c(kinfo, vm, nm, mgy), names_to = "variable")

ggplot(df_long, aes(x = Date))+
  geom_line(aes(y = y), color = "blue", linewidth = 1.2)+
  geom_point(aes(y = value, color = variable), alpha = .8, size = 2)+
  labs(y = "EUR-HUF rate",
       color = "Article publsihed")+
  scale_x_date(breaks = "year", date_labels = "%Y")+
  scale_y_continuous(breaks = seq(300, 450, by = 25))

ggsave("plots/eurhuf_ts.pdf")
