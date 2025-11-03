library(tidyverse)
library(readxl)

options(scipen = 999)


# TIC = sqrt(sum (y-y_hat)^2) / (sqrt(sum y_hat^2) + sqrt(sum y^2))
calc_tic = function(y, y_hat){
  a = sqrt(sum((y-y_hat)^2))
  b = sqrt(sum(y_hat^2)) 
  c = sqrt(sum(y^2))
  return(a/(b+c))
}

calc_measures = function(sheet){
  
  # y and y_log
  y = read_csv("./LSTM/full_dataset_20250830.csv")
  y = y %>%
    filter(Date >= as.Date("2024-06-13")) %>% 
    rename(y = `eur_close`) %>% 
    .$y
  
  y_log = diff(log(y))

  error = read_excel("./LSTM/errors_results.xlsx", sheet = sheet, .name_repair = "minimal")[-1]
  y_hat = sapply(error, function(x) y-x)
  y_hat = as.data.frame(y_hat)  
  
  y_log_hat = apply(y_hat, 2, function(x) diff(log(x)))
  y_log_hat = as.data.frame(y_log_hat)
  
  
  y_log = y_log[-length(y_log)]
  # y_log_hat = y_log_hat[-1,]
  
  
  
  tic = c()
  rmse = c()
  mae = c()
  
  for (i in 1:ncol(y_log_hat)) {
    tic[i] = calc_tic(y_log, y_log_hat[,i])
    rmse[i] = sqrt(mean((y_log-y_log_hat[,i])^2))
    mae[i] = mean(abs(y_log-y_log_hat[,i]))
  }
  
  return(data.frame(mae = mae, rmse = rmse, tic= tic))
}


measures = data.frame()
sheets = c('no_dummies', 'only_kinfo', 'only_vm', 'only_nm', 'only_mgy', 'all_variables')

for (sheet in sheets) {
  temp_measures = calc_measures(sheet)
  temp_measures$model = sheet
  measures = rbind(measures, temp_measures)
}

measure_pivot = measures %>%
  group_by(model) %>%
  summarise(
    # mean_mae = round(mean(mae),6),
    # median_mae = round(median(mae),6),
    mean_rmse = round(mean(rmse),6),
    median_rmse = round(median(rmse),6)#,
    # mean_tic = round(mean(tic),6),
    # median_tic = round(median(tic),6)
      )
 
# measures = calc_measures('no_dummies')
# summary(measures)
# 
# measures = calc_measures('only_kinfo')
# summary(measures)
# 
# measures = calc_measures('only_vm')
# summary(measures)
# 
# measures = calc_measures('only_nm')
# summary(measures)
# 
# measures = calc_measures('only_mgy')
# summary(measures)
# 
# measures = calc_measures('all_variables')
# summary(measures)



# write.xlsx(measures, "LSTM_fit_measures.xlsx")
################################################################################

calc_best_fit_mean = function(sheet){
  
  # y and y_log
  y = read_csv("./LSTM/full_dataset_20250830.csv")
  y = y %>%
    filter(Date >= as.Date("2024-06-13")) %>% 
    rename(y = `eur_close`) %>% 
    .$y
  
  y_log = diff(log(y))
  
  error = read_excel("./LSTM/errors_results.xlsx", sheet = sheet, .name_repair = "minimal")
  y_hat = sapply(error, function(x) y-x)
  y_hat = as.data.frame(y_hat)
  
  
  y_log_hat = apply(y_hat, 2, function(x) diff(log(x)))
  y_log_hat = as.data.frame(y_log_hat)
  
  
  y_log = y_log[-length(y_log)]
  # y_log_hat = y_log_hat[-1,]
  
  
  tic = rmse = mae = ncol(y_log_hat)
  
  for (i in 1:ncol(y_log_hat)) {
    tic[i] = calc_tic(y_log, y_log_hat[,i])
    rmse[i] = sqrt(mean((y_log-y_log_hat[,i])^2))
    mae[i] = mean(abs(y_log-y_log_hat[,i]))
  }
  
  bottom_quantile = order(rmse)[1:floor(0.25 * length(rmse))]
  
  bottom_quantile_preds = y_log_hat[,bottom_quantile]
  # bottom_quantile_preds = y_hat[,bottom_quantile]
  
  mean_preds = rowMeans(bottom_quantile_preds)
  
  return(mean_preds)
}

y = read_csv("./LSTM/full_dataset_20250830.csv")
y = y %>%
  filter(Date >= as.Date("2024-06-13")) %>% 
  rename(y = `eur_close`) %>%
  select(y, Date)

y_log = diff(log(y$y))

best_preds = data.frame(
  date = y$Date[-c(1,175)],
  eredeti = y_log[-174],
  alapmodell = calc_best_fit_mean("no_dummies"),
  kinfo = calc_best_fit_mean("only_kinfo"),
  vm = calc_best_fit_mean("only_vm"),
  nm = calc_best_fit_mean("only_nm"),
  mgy = calc_best_fit_mean("only_mgy"),
  teljes = calc_best_fit_mean("all_variables")
)

a = data.frame(
  date = best_preds$date,
  eredeti = best_preds$eredeti,
  alapmodell = best_preds$alapmodell,
  kinfo = best_preds$kinfo,
  vm = best_preds$vm,
  nm = best_preds$nm,
  mgy = best_preds$mgy,
  teljes = best_preds$teljes
)



linewidth = .75
alpha = .6

ggplot(a, aes(x = date)) +
  geom_line(aes(y = eredeti, color = "Eredeti idősor"), linewidth = 1.5) +
  geom_line(aes(y = alapmodell, color = "Alapmodell"), linewidth = linewidth, alpha = alpha) +
  geom_line(aes(y = kinfo, color = "Kinfo"), linewidth = linewidth, alpha = alpha) +
  geom_line(aes(y = vm, color = "VM"), linewidth = linewidth, alpha = alpha) +
  geom_line(aes(y = nm, color = "NM"), linewidth = linewidth, alpha = alpha) +
  geom_line(aes(y = mgy, color = "MGY"), linewidth = linewidth, alpha = alpha) +
  geom_line(aes(y = teljes, color = "Teljes modell"), linewidth = linewidth, alpha = alpha) +
  scale_color_manual(
    name = "Modellek",  # Legend title
    values = c(
      "Eredeti idősor" = "black",
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
  scale_y_continuous(labels = scales::percent)+
  theme_bw() +
  labs(y = "EUR-HUF loghozam")+
  theme(axis.title.x = element_blank(),
        text = element_text(size = 20))

##################################################################################################

a = data.frame(
  date = y$Date[-175],
  eredeti = y$y[-175],
  alapmodell = calc_best_fit_mean("no_dummies")[-1],
  kinfo = calc_best_fit_mean("only_kinfo")[-1],
  vm = calc_best_fit_mean("only_vm")[-1],
  nm = calc_best_fit_mean("only_nm")[-1],
  mgy = calc_best_fit_mean("only_mgy")[-1],
  teljes = calc_best_fit_mean("all_variables")[-1]
)


write.csv(a, "LSTM/preds_huf.csv")
