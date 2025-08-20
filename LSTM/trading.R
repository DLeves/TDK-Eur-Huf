library(tidyverse)

df = read.csv("LSTM/preds_huf.csv")
df$date = as.Date(df$date)

# Example dataframe structure:
# df$ActualPrice: Actual price for each day
# df$PredictedPrice: Predicted price for each day

model = "alapmodell"

trading_backtest = function(df, model) {
  # Generate trading signals and simulate trades
  df$Signal <- c(NA)  # Initialize Signal column
  
  # Logic for trading signals
  for (i in 2:nrow(df)) {
    # Compare previous day's actual price with current day's predicted price
    if (df$eredeti[i - 1] < df[i, model]) {
      df$Signal[i] <- 1  # Buy signal
    } else if (df$eredeti[i - 1] > df[i, model]) {
      df$Signal[i] <- -1  # Sell signal
    } else {
      df$Signal[i] <- 0  # Hold signal
    }
  }
  
  # On the last day, sell everything (signal = -1)
  df$Signal[nrow(df)] <- -1
  
  # Calculate returns based on trading signals
  df$Returns <- diff(df$eredeti) / lag(df$eredeti, 1)
  df$StrategyReturns <- df$Signal * df$Returns
  
  # Performance metrics
  cumulative_strategy_returns <- prod(1 + na.omit(df$StrategyReturns)) - 1
  cumulative_market_returns <- prod(1 + na.omit(df$Returns)) - 1
  
  cat("Cumulative Strategy Returns:", cumulative_strategy_returns, "\n")
  cat("Cumulative Market Returns:", cumulative_market_returns, "\n")
  
  df[1, 10:12] = 0
  
  # Visualization
  # plot(1:nrow(df), cumsum(df$StrategyReturns), type = "l", col = "blue",
  #      xlab = "Day", ylab = "Cumulative Returns", main = "Trading Strategy Backtest")
  # lines(1:nrow(df), cumsum(df$Returns), col = "red")
  # legend("topright", legend = c("Strategy", "Market"), col = c("blue", "red"), lty = 1)
  
  return(list(
    model,
    cumulative_strategy_returns_eod = cumulative_strategy_returns,
    cumulative_strategy_returns = cumsum(df$StrategyReturns)
  ))
  
}

alapmodell = trading_backtest(df, "alapmodell")
teljes = trading_backtest(df, "teljes")
kinfo = trading_backtest(df, "kinfo")
vm = trading_backtest(df, "vm")
nm = trading_backtest(df, "nm")
mgy = trading_backtest(df, "mgy")

returns = data.frame(
  date = df$date,
  alapmodell = alapmodell$cumulative_strategy_returns,
  teljes = teljes$cumulative_strategy_returns,
  kinfo = kinfo$cumulative_strategy_returns,
  vm = vm$cumulative_strategy_returns,
  nm = nm$cumulative_strategy_returns,
  mgy = mgy$cumulative_strategy_returns
)

linewidth = .9

ggplot(returns, aes(x = date))+
  geom_line(aes(y = alapmodell, color = "Alapmodell"), linewidth = linewidth)+
  geom_line(aes(y = teljes, color = "Teljes modell"), linewidth = linewidth)+
  geom_line(aes(y = vm, color = "VM"), linewidth = linewidth)+
  geom_line(aes(y = nm, color = "NM"), linewidth = linewidth)+
  geom_line(aes(y = mgy, color = "MGY"), linewidth = linewidth)+
  geom_line(aes(y = kinfo, color = "Kinfo"), linewidth = linewidth)+
  scale_color_manual(
    name = "Modellek",  # Legend title
    values = c(
      "Alapmodell" = "#1b9e77",
      "Kinfo" = "#d95f02",
      "VM" = "#7570b3",
      "NM" = "#e7298a",
      "MGY" = "#66a61e",
      "Teljes modell" = "#e6ab02"
    )
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent, breaks = seq(-0.05, 0.05, by = 0.01))+
  theme_bw() +
  labs(y = "Hozam")+
  theme(axis.title.x = element_blank(),
        text = element_text(size = 20))
