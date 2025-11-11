library(tidyverse)
library(readxl)
library(openxlsx)

# RMSE Boxplots
rmse=read_excel("./LSTM/rmse_results.xlsx")
rmse = rmse[,-1]

rmse_long = rmse %>%
  pivot_longer(cols = rmse_all_variables:rmse_only_mgy, names_to = "name", values_to = "val") %>% 
  mutate(name = substring(name, 6))


ggplot(rmse_long, aes(x = name, y = val))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 10))+
  theme_bw()

# rmse_long_filt = rmse[-1,] %>%
#   pivot_longer(cols = rmse_all_variables:rmse_only_mgy, names_to = "name", values_to = "val") %>% 
#   mutate(name = substring(name, 6))
# 
# 
# ggplot(rmse_long_filt, aes(x = name, y = val))+
#   geom_boxplot()+
#   labs(y = 'RMSE',
#        title = "LSTM modellek RMSE értékeinek dobozábrái", caption = "Saját számítás és szerkesztés.")+
#   scale_x_discrete(labels = c("Teljes modell","Alapmodell","Kormányinfó","Matolcsy György","Nagy Márton","Varga Mihály"))+
#   theme_bw()+
#   theme(axis.title.x = element_blank())

ggsave("./plots/rmse_box_filt.jpg")


# optimal lags
opt_lag = read_excel("./LSTM/optimal_lag_mean.xlsx")
opt_lag$lag = 1:8

ggplot(opt_lag, aes(x = lag, y = rmse))+
  geom_line(size = .7)+
  labs(y = 'Átlagos RMSE', x = "Eredményváltozó időbeli késleltetése"#,
       # title = "Az egyes eredményváltozó késleltetéshez tartozó átlagos RMSE-k",
       # subtitle = "50 darabos LSTM modell mintánkon",
       # caption = "Saját számítás és szerkesztés."
  )+
  theme_bw()+
  theme(text =  element_text(size = 20))

ggsave("./plots/optimal_lags.jpg")


#mae
error = read_excel("./LSTM/errors_results.xlsx", sheet = 'only_vm')[-1]
maes = sapply(error, function(x) mean(abs(x)))

# TIC-----------------------------------------------------------------------------------------------

# y and y_log
y = read_csv("./LSTM/full_dataset.csv")
y = y %>%
  filter(Date >= as.Date("2024-06-13")) %>% 
  rename(y = `eur_close`) %>% 
  .$y

y_log = diff(log(y))

# y_hat and y_log_hat
# error = read_excel("./LSTM/errors_results.xlsx", sheet = 'no_dummies')[-1]
error = read_excel("./LSTM/errors_results.xlsx", sheet = 'only_vm')[-1]
y_hat = sapply(error, function(x) y-x)
y_hat = as.data.frame(y_hat)  

y_log_hat = apply(y_hat, 2, function(x) diff(log(x)))
y_log_hat = as.data.frame(y_log_hat)


y_log = y_log[-length(y_log)]
y_log_hat = y_log_hat[-1,]

plot_df = data.frame(y = y_log, y_hat = y_log_hat$sim_15, t = 1:length(y_log))



ggplot(plot_df, aes(x = t))+
  geom_line(aes(y = y), color = "blue", size = 1)+
  geom_line(aes(y = y_hat), color = "red", size = 1, alpha = .8)+
  labs(title = "A 15. LSTM modell becslése a loghozamra és az aktuális adatok")+
  theme_bw()


# TIC = sqrt(sum (y-y_hat)^2) / (sqrt(sum y_hat^2) + sqrt(sum y^2))
calc_tic = function(y, y_hat){
  a = sqrt(sum((y-y_hat)^2))
  b = sqrt(sum(y_hat^2)) 
  c = sqrt(sum(y^2))
  return(a/(b+c))
}


tic = c()
rmse = c()
mae = c()

for (i in 1:500) {
  tic[i] = calc_tic(y_log, y_log_hat[,i])
  rmse[i] = sqrt(mean((y_log-y_log_hat[,i])^2))
  mae[i] = mean(abs(y_log-y_log_hat[,i]))
}

measures = data.frame(mae = mae, rmse = rmse, tic= tic)
summary(measures)

write.xlsx(measures, "LSTM_fit_measures.xlsx")

# ts plot with dots---------------------------------------------------------------------------------
df_full = read_csv("./full_dataset_20250830.csv")

df = df_full %>%
  rename(y = eur_close) %>% 
  mutate(kinfo = ifelse(mean_kinfo != 0, y, NA),
         vm = ifelse(mean_varga != 0, y, NA),
         nm = ifelse(mean_nagy != 0, y, NA),
         mgy = ifelse(mean_matolcsy != 0, y, NA)
  ) %>%
  select(Date, y, kinfo, vm, nm, mgy)

alpha = .9
color = "red"
size = 3

ggplot(df, aes(x = Date))+
  geom_line(aes(y = y), color = "blue", linewidth = 1.2)+
  geom_point(aes(y = kinfo), color = color, alpha = alpha, size = size)+
  geom_point(aes(y = vm), color = color, alpha = alpha, size = size)+
  geom_point(aes(y = nm), color = color, alpha = alpha, size = size)+
  geom_point(aes(y = mgy), color = color, alpha = alpha, size = size)+
  labs(y = "EUR-HUF árfolyam"#,
       # title = "Az Euró-Forint árfolyamának alakulása",
       # subtitle = "2018.5.31. és 2025.2.14. között, a vizsgált cikkekkel és alapkamat módosításokkal",
       # caption = "Forrás: Yahoo Finance, Magyar Hang, MNB. Saját szerkesztés."
  )+
  scale_x_date(breaks = "year", date_labels = "%Y")+
  scale_y_continuous(breaks = seq(300, 450, by = 25))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        text = element_text(size = 20))

ggsave("./plots/eurhuf_line.jpg")


df_long = df_full %>%
  rename(y = eur_close) %>% 
  mutate(kinfo = ifelse(mean_kinfo != 0, y, NA),
         vm = ifelse(mean_varga != 0, y, NA),
         nm = ifelse(mean_nagy != 0, y, NA),
         mgy = ifelse(mean_matolcsy != 0, y, NA)
  ) %>%
  select(Date, y, kinfo, vm, nm, mgy) %>% 
  pivot_longer(cols = c(kinfo, vm, nm, mgy), names_to = "variable") %>% 
  mutate(variable = toupper(variable))

ggplot(df_long, aes(x = Date))+
  geom_line(aes(y = y), color = "blue", linewidth = 1.2)+
  geom_point(aes(y = value, color = variable), alpha = 1, size = size)+
  labs(y = "EUR-HUF árfolyam",
       color = "Változó")+
  scale_x_date(breaks = "year", date_labels = "%Y")+
  scale_y_continuous(breaks = seq(300, 450, by = 25))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        text = element_text(size = 20))

ggsave("./plots/eurhuf_line_col_dots.jpg")

df_long_not_na = df_long %>%
  mutate(variable_date = ifelse(!is.na(value), Date, as.Date(NA)))

ggplot(df_long_not_na, aes(x = Date))+
  geom_vline(aes(xintercept = variable_date, color = variable), alpha = 1, size = 1, linetype = "dashed")+
  geom_line(aes(y = y), color = "blue", linewidth = 1.2)+
  labs(y = "EUR-HUF árfolyam",
       color = "Változó")+
  scale_x_date(breaks = "year", date_labels = "%Y")+
  scale_y_continuous(breaks = seq(300, 450, by = 25))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        text = element_text(size = 20))

ggsave("./plots/eurhuf_line_col_vline.jpg")
