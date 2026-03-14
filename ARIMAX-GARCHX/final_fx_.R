##------------------------ -
# 0. Packages (keep minimal; fail gracefully with a clear hint) :))) ----
##------------------------ -
req <- c("aTSA", "broom", "Cairo", "cowplot", "dplyr", "FinTS",
         "forecast", "ggcorrplot", "ggplot2", "kableExtra", "knitr",
         "lmtest", "pastecs", "rugarch", "stringr", "tidyr", "tseries",
         "this.path", "rlang", "ICglm", "progressr")

to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
invisible(lapply(req, library, character.only = TRUE))

setwd(this.path::here())
getwd()
rm(to_install, req)

##------------------------ -
# 1. Data and helper functions ----
##------------------------ -
Training <- as.data.frame(read.csv("full_dataset_20250830.csv"))
Training$Date <- as.Date(Training$Date)

# Color definition
primary_col <- "#022576"
secondary_col <- "#FF0000"
third_col <- "#008000"
fourth_col <- "#820800"

# Forecasting
# Train-test split
train_start <- as.Date("2018-06-03")
test_start <- as.Date("2024-06-13")
test_end <- as.Date("2025-02-14")
train_idx <- Training$Date >= train_start & Training$Date <  test_start
test_idx  <- Training$Date >= test_start  & Training$Date <= test_end
dates_train <- Training$Date[train_idx]
dates_test  <- Training$Date[test_idx]

# Ggplot Theme
#' @title Custom ggplot2 Theme
#' @description Defines a consistent, publication-ready visual style for all plots in the pipeline, using Times New Roman and clean backgrounds.
#' @return A ggplot2 theme object.
theme_regina <- function() {
  theme(
    plot.title   = element_text(family = "Times New Roman", size = 34, face = "bold",
                                hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(family = "Times New Roman", size = 20, face = "bold"),
    axis.title.x = element_text(family = "Times New Roman", size = 30, margin = margin(t = 10)),
    axis.title.y = element_text(family = "Times New Roman", size = 30, margin = margin(r = 10)),
    axis.text.x  = element_text(family = "Times New Roman", size = 22, angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y  = element_text(family = "Times New Roman", size = 24),
    legend.title = element_text(family = "Times New Roman", size = 26, face = "bold"),
    legend.text  = element_text(family = "Times New Roman", size = 24),
    legend.position = "right",                        
    legend.justification = "center",
    plot.caption = element_text(family = "Times New Roman", size = 22, hjust = 0, margin = margin(t = 15)),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background  = element_rect(fill = "white", color = "white"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  )
}

#' @title SVG Export Wrapper
#' @description Saves a ggplot object as an SVG file using the Cairo graphics device inside a "Plots" directory.
#' Converts pixel inputs to inches to prevent memory overflow.
#' @param plot_obj The ggplot object to save.
#' @param file The output filename.
#' @param width Plot width in pixels (default 1600).
#' @param height Plot height in pixels (default 900).
#' @param dpi The resolution used to convert pixels to inches (default 100).
export_svg <- function(plot_obj, file, width = 1600, height = 900, dpi = 100) {
  if (!dir.exists("Plots")) {
    dir.create("Plots", recursive = TRUE)
  }
  
  full_path <- file.path("Plots", basename(file))
  
  # Convert pixels to inches for CairoSVG to prevent memory crash
  width_in <- width / dpi
  height_in <- height / dpi
  
  Cairo::CairoSVG(file = full_path, width = width_in, height = height_in)
  print(plot_obj)
  dev.off()
}

#' @title PNG Export Wrapper
#' @description Saves a ggplot object as a PNG file using the Cairo graphics device inside a "Plots" directory.
#' @param plot_obj The ggplot object to save.
#' @param file The output filename.
#' @param width Plot width (default 1600).
#' @param height Plot height (default 900).
export_png <- function(plot_obj, file, width = 1600, height = 900) {
  # Create directory if it doesn't exist
  if (!dir.exists("Plots")) {
    dir.create("Plots", recursive = TRUE)
  }
  
  full_path <- file.path("Plots", basename(file))
  
  # Note: CairoPNG uses 'filename'
  Cairo::CairoPNG(filename = full_path, width = width, height = height)
  print(plot_obj)
  dev.off()
}

#' @title Plot Two Time Series Lines
#' @description A useful template for plotting an actual vs. forecasted time series line chart with custom styling.
#' @param df Dataframe containing the data.
#' @param x The column name for the x-axis (usually Date).
#' @param y1 The column name for the actual values.
#' @param y2 The column name for the forecasted values.
#' @param ylab The y-axis label.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param file The output SVG filename.
plot_two_lines <- function(df, x, y1, y2, ylab, title, subtitle, caption, file) {
  p <- ggplot(df, aes(x = {{x}})) +
    geom_line(aes(y = {{y1}}, color = "Actual"), linewidth = 1.2) +
    geom_line(aes(y = {{y2}}, color = "Forecasted"),   linewidth = 1.2) +
    scale_color_manual(values = c("Actual" = primary_col, "Forecasted" = secondary_col), name = NULL) +
    labs(y = ylab, title = title, subtitle = subtitle, caption = caption) +
    theme_regina()
  export_svg(p, file)
  export_png(p, file)
}

#' @title Plot Conditional Standard Deviation (Sigma)
#' @description Extracts and plots the conditional standard deviation from a fitted ugarchfit object over time.
#' @param fit A fitted ugarchfit object.
#' @param dates A vector of corresponding dates.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param file The output SVG filename.
plot_sigma_svg <- function(fit, dates, title, subtitle, file) {
  sig <- sigma(fit)
  df  <- data.frame(Date = dates[seq_along(sig)], Sigma = as.numeric(sig))
  p <- ggplot(df, aes(Date, Sigma)) +
    geom_line(linewidth = 1.2, color = primary_col) +
    labs(y = "Standard deviation", title = title, subtitle = subtitle, caption = "Source: Authors' own calculations.") +
    theme_regina()
  export_svg(p, file)
  export_png(p, file)
}

#' @title Plot Rolling VaR (Value at Risk)
#' @description Extracts and plots the actual log-returns against the predicted VaR from a ugarchroll object.
#' @param roll_obj A fitted ugarchroll object.
#' @param dates A vector of corresponding dates.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param file The output SVG filename.
#' @param alpha_lab The label for the VaR line in the legend (default "VaR 95%").
plot_var_roll_svg <- function(roll_obj, dates, title, subtitle, file, alpha_lab = "VaR 95%") {
  vr <- as.data.frame(roll_obj@forecast$VaR)
  n  <- nrow(vr)
  vr$Date <- dates[seq_len(n)]
  p <- ggplot(vr, aes(x = Date)) +
    geom_line(aes(y = realized,    color = "Actual log-return"), linewidth = 1.2) +
    geom_line(aes(y = `alpha(5%)`, color = alpha_lab),            linewidth = 1.2) +
    scale_color_manual(values = c("Actual log-return" = primary_col, !!alpha_lab := secondary_col), name = NULL) +
    labs(y = "Log-return", title = title, subtitle = subtitle, caption = "Source: Authors' own calculations.") +
    theme_regina()
  export_svg(p, file)
  export_png(p, file)
}

#' @title Calculate HQIC manually
#' @description A fallback function to calculate the Hannan-Quinn Information Criterion.
#' @param fit A fitted model object that supports logLik().
#' @param n The sample size.
#' @return A numeric value representing the HQIC.
HQIC_fun <- function(fit, n) {
  ll <- as.numeric(logLik(fit))
  k  <- attr(logLik(fit), "df")
  -2*ll + 2*k*log(log(n))
}

#' @title Compute Variance Inflation Factor
#' @description Calculates the VIF for a set of numeric variables in a dataframe to detect multicollinearity. Automatically drops constant columns and handles NAs.
#' @param data The input dataframe.
#' @param vars A character vector of variable names to check.
#' @return A named numeric vector of VIF values.
compute_vif <- function(data, vars) {
  stopifnot(all(vars %in% names(data)))
  df <- data[, vars, drop = FALSE]
  # Only numeric columns
  non_num <- names(df)[!vapply(df, is.numeric, TRUE)]
  if (length(non_num)) stop("Non-numeric column(s): ", paste(non_num, collapse = ", "))
  # Drop NAs
  df <- stats::na.omit(df)
  if (!nrow(df)) stop("No non-NA rows in the selected variables.")
  # Drop constant columns
  const_cols <- names(df)[vapply(df, function(x) isTRUE(var(x) == 0), TRUE)]
  if (length(const_cols)) {
    message("Constant column dropped for VIF: ", paste(const_cols, collapse = ", "))
    df <- df[, setdiff(names(df), const_cols), drop = FALSE]
  }
  vars2 <- names(df)
  vifs <- setNames(numeric(length(vars2)), vars2)
  
  for (v in vars2) {
    others <- setdiff(vars2, v)
    if (!length(others)) { vifs[v] <- 1; next }
    fit <- tryCatch(
      stats::lm(df[[v]] ~ ., data = as.data.frame(df[, others, drop = FALSE])),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      vifs[v] <- Inf
    } else {
      r2 <- summary(fit)$r.squared
      vifs[v] <- if (is.finite(r2) && r2 < 1) 1/(1 - r2) else Inf
    }
  }
  # Return to the full list of names (NAs for constants)
  out <- setNames(rep(NA_real_, length(vars)), vars)
  out[names(vifs)] <- vifs
  out
}


#' @title Calculate Design Matrix Condition Number
#' @description Computes the condition number (kappa) of a centered design matrix to assess multicollinearity. High values indicate near rank-deficiency.
#' @param data The input dataframe.
#' @param vars A character vector of variable names.
#' @return A numeric value representing the condition number.
design_kappa <- function(data, vars) {
  X <- as.matrix(stats::na.omit(data[, vars, drop = FALSE]))
  if (!nrow(X) || !ncol(X)) return(NA_real_)
  # Center, without scaling
  Xc <- scale(X, center = TRUE, scale = FALSE)
  # High kappa in case of rank deficiency
  kappa(Xc)
}


#' @title Iterative VIF Pruning
#' @description Iteratively drops the variable with the highest VIF until all remaining variables have a VIF below the specified threshold.
#' @param data The input dataframe.
#' @param vars A character vector of initial variable names.
#' @param threshold The VIF threshold for dropping variables (default 10).
#' @param max_iter Maximum number of iterations (default 50).
#' @param verbose Logical; if TRUE, prints progress messages.
#' @return A list containing the final variables, final VIFs, removal steps, and final condition number.
vif_iterative_prune <- function(data, vars, threshold = 10, max_iter = 50, verbose = TRUE) {
  keep <- vars
  steps <- data.frame(step = integer(), removed = character(),
                      max_vif_before = double(), stringsAsFactors = FALSE)
  
  for (s in seq_len(max_iter)) {
    vifs <- compute_vif(data, keep)
    max_v <- max(vifs, na.rm = TRUE)
    if (!is.finite(max_v) || is.na(max_v)) {
      # If everything is NA/Inf for some reason: drop the most problematic one, or stop
      bad <- names(vifs)[which.max(ifelse(is.finite(vifs), -Inf, 1))]
      if (length(bad)) {
        steps <- rbind(steps, data.frame(step = s, removed = bad, max_vif_before = max_v))
        keep <- setdiff(keep, bad)
        if (verbose) message(sprintf("[Step %d] Removed (NA/Inf VIF): %s", s, bad))
        next
      } else break
    }
    if (max_v <= threshold) {
      final <- sort(vifs)
      return(list(final_vars = keep,
                  final_vif = final,
                  steps = steps,
                  kappa = design_kappa(data, keep)))
    }
    # Drop the one with the highest VIF
    drop_var <- names(vifs)[which.max(vifs)]
    steps <- rbind(steps, data.frame(step = s, removed = drop_var, max_vif_before = max_v))
    keep <- setdiff(keep, drop_var)
    if (verbose) message(sprintf("[Step %d] Removed (VIF=%.2f): %s", s, max_v, drop_var))
    if (length(keep) <= 1) break
  }
  list(final_vars = keep,
       final_vif = compute_vif(data, keep),
       steps = steps,
       kappa = design_kappa(data, keep))
}


#' @title Formatted VIF Table
#' @description Generates an HTML/LaTeX formatted table using kableExtra for a set of VIF values, highlighting problematic ones in red.
#' @param vif_named A named numeric vector of VIF values.
#' @param threshold The threshold above which to warn (default 10).
#' @param caption The table caption.
#' @return A kableExtra styled table.
vif_kable <- function(vif_named, threshold = 10, caption = "VIF Diagnostics") {
  stopifnot(requireNamespace("kableExtra", quietly = TRUE))
  df <- data.frame(Variable = names(vif_named),
                   VIF = as.numeric(vif_named),
                   stringsAsFactors = FALSE)
  df$Indicator <- ifelse(is.na(df$VIF), "NA",
                         ifelse(is.infinite(df$VIF), "∞",
                                ifelse(df$VIF > threshold, "⚠", "")))
  # Formatting
  df$VIF_fmt <- formatC(df$VIF, format = "f", digits = 3)
  df$VIF_fmt[df$Indicator == "∞"] <- "&infin;"
  df$VIF_fmt[df$Indicator == "NA"] <- "NA"
  
  # Highlighting
  df$VIF_fmt <- ifelse(df$Indicator == "⚠",
                       kableExtra::cell_spec(df$VIF_fmt, bold = TRUE, color = "red"),
                       df$VIF_fmt)
  
  kableExtra::kbl(df[, c("Variable", "VIF_fmt", "Indicator")],
                  escape = FALSE, align = "c",
                  col.names = c("Variable", "VIF", "Indicator"),
                  caption = caption) |>
    kableExtra::kable_styling(full_width = FALSE, position = "center") |>
    kableExtra::footnote(general = sprintf("Threshold: VIF > %g → strong suspicion of multicollinearity.", threshold),
                         general_title = "", escape = FALSE)
}

#' @title Generate Significance Stars
#' @description Converts numeric p-values into standard significance star notation (*, **, ***).
#' @param p A numeric p-value.
#' @return A character string of stars.
sig_code <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***",
                ifelse(p < 0.05, "**",
                       ifelse(p < 0.10, "*", "")
                )
         )
  )
}

# Pre-allocating an empty dataframe for manual modeling results
garch_modell_results <- data.frame(modell = character(),
                                   AIC = numeric(), 
                                   BIC = numeric(), 
                                   HQ = numeric(),
                                   RMSE = numeric(), 
                                   MAE = numeric(), 
                                   TIC = numeric())


##======================== -
# 2. Visualization of variables ----
##======================== -

# Find present interest rate columns
rate_cols <- intersect(c("MNB", "ECB", "FED", "Fed"), names(Training))

# Identify days when interest rates changed
rate_changing_days <- Training %>%
  dplyr::select(Date, dplyr::all_of(rate_cols)) %>%
  tidyr::pivot_longer(-Date, names_to = "Institution", values_to = "Rate") %>%
  dplyr::group_by(Institution) %>%
  dplyr::arrange(Date, .by_group = TRUE) %>%
  dplyr::mutate(changed = !is.na(Rate) & !is.na(dplyr::lag(Rate)) & (Rate != dplyr::lag(Rate))) %>%
  dplyr::filter(changed) %>%
  dplyr::ungroup()

# Colors
rate_colors <- c(MNB = fourth_col, ECB = third_col, FED = primary_col)

# --- Exchange rate + Interest rate change lines ---
p_fx <- ggplot(Training, aes(x = Date)) +
  geom_line(aes(y = eur_close), color = primary_col, linewidth = 1.2) +
  # Single geom_vline with change days, color-coded by institution
  geom_vline(data = rate_changing_days,
             aes(xintercept = as.numeric(Date), color = Institution),
             linetype = "dashed", linewidth = 1) +
  geom_point(aes(y = eur_close), color = secondary_col, size = 0.4, alpha = 0.6) +
  labs(title = "Evolution of the EUR Exchange Rate",
       subtitle = "May 31, 2018 - February 14, 2025",
       x = "Time", y = "EUR Exchange Rate",
       caption = "Source: Yahoo Finance, Magyar Hang, MNB. Authors' own editing.") +
  scale_x_date(breaks = "year", date_labels = "%Y",
               limits = as.Date(c("2018-05-31", "2025-02-14"))) +
  scale_color_manual(values = rate_colors, breaks = intersect(names(rate_colors), unique(rate_changing_days$Institution)),
                     name = "Institution") +
  theme_regina()

export_svg(p_fx, "exchange_rate_with_base_rates.svg", 800, 600 )
export_png(p_fx, "exchange_rate_with_base_rates.png", 800, 600 )


# --- Exchange rate + Sentiment lines
q_fx <- ggplot(Training, aes(x = Date)) +
  geom_line(aes(y = eur_close), color = primary_col, linewidth = 1.2) +
  
  geom_vline(data = subset(Training, mean_matolcsy != 0),
             aes(xintercept = as.numeric(Date), color = "Matolcsy"),
             linetype = "dashed", linewidth = 1) +
  geom_vline(data = subset(Training, mean_nagy != 0),
             aes(xintercept = as.numeric(Date), color = "Márton Nagy"),
             linetype = "dashed", linewidth = 1) +
  geom_vline(data = subset(Training, mean_varga != 0),
             aes(xintercept = as.numeric(Date), color = "Mihály Varga"),
             linetype = "dashed", linewidth = 1) +
  geom_vline(data = subset(Training, mean_kinfo != 0),
             aes(xintercept = as.numeric(Date), color = "Gov. Press Conf."),
             linetype = "dashed", linewidth = 1) +
  
  geom_point(aes(y = eur_close), color = secondary_col, size = 0.4, alpha = 0.6) +
  labs(title = "Evolution of the EUR Exchange Rate",
       subtitle = "May 31, 2018 - February 14, 2025",
       x = "Time", y = "EUR Exchange Rate",
       caption = "Source: Yahoo Finance, Magyar Hang, MNB. Authors' own editing.") +
  scale_x_date(breaks = "year", date_labels = "%Y",
               limits = as.Date(c("2018-05-31", "2025-02-14"))) +
  scale_color_manual(
    values = c("Matolcsy" = fourth_col,
               "Márton Nagy" = primary_col,
               "Mihály Varga" = third_col,
               "Gov. Press Conf." = "purple"),
    name = "Sentiment Source"
  ) +
  theme_regina() +
  theme(legend.position = "bottom")

export_svg(q_fx, "exchange_rate_with_news.svg", 800, 600 )
export_png(q_fx, "exchange_rate_with_news.png", 800, 600 )

rm(q_fx, p_fx, rate_changing_days)


# Representing interest rate changes over time
rates_long <- Training %>% 
  dplyr::select(Date, FED, ECB, MNB) %>% 
  tidyr::pivot_longer(cols = c(FED, ECB, MNB), 
                      names_to = "Institution", 
                      values_to = "Rate Level")


p_rates <- ggplot(rates_long, aes(x = Date, y = `Rate Level`, color = Institution)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Evolution of Interest Rate Changes",
       subtitle = "Fed, ECB and MNB (2018-2025)",
       x = "Date", y = "Base Interest Rate (%)",
       caption = "Source: MNB, ECB, FED. Authors' own editing.") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_regina()

## [Gemini Review]: Same note regarding the extension .png vs the SVG export function.
export_svg(p_rates, "base_rates_over_time.svg", 800, 600)
export_png(p_rates, "base_rates_over_time.png", 800, 600)
rm(p_rates, rates_long)

##======================== -
# 3. Testing and Preparation -----
##======================== -

# Augmented Dickey-Fuller test
adf.test(Training$eur_close) 

# Transformation to stationary
inputs  <- c("eur_close", "eurczk", "eurpln", "eurron", "BUX", "cetop")
outputs <- c("d_eur", "d_czk", "d_pln", "d_ron", "d_bux", "d_cetop")
for (k in seq_along(inputs)) {
  x <- Training[[inputs[k]]]
  Training[[outputs[k]]] <- c(NA, diff(log(x)))
}

Training <-  Training[-1,]
stat.desc(Training)    
adf.test(Training$d_eur) 
kpss.test(Training$d_eur) 

# 1. Calculating changes
rate_levels <- Training[, c("FED", "MNB", "ECB")]
rate_levels_changes <- sapply(rate_levels, function(x) sum(diff(x) != 0, na.rm = TRUE))

# 2. Counting non-zero days for sentiment variables
sentiment_variables <- Training[, c("mean_matolcsy", "mean_kinfo", "mean_varga", "mean_nagy")]
sentiment_active_days <- sapply(sentiment_variables, function(x) sum(x != 0, na.rm = TRUE))

# Concatenating results
results <- data.frame(
  Variable = c(names(rate_levels_changes), names(sentiment_active_days)),
  Value = c(rate_levels_changes, sentiment_active_days),
  Type = c(rep("Interest rate change", length(rate_levels_changes)),
           rep("Non-zero days", length(sentiment_active_days)))
)

results %>%
  kbl(digits = 8,        
      format.args = list(scientific = FALSE), 
      align = "c", 
      caption = "Number of variables in the database") %>%
  kable_styling(full_width = FALSE, position = "center")

rm(results, rate_levels, sentiment_variables, rate_levels_changes, rate_colors, rate_cols, sentiment_active_days)


# Log-return plot
p_ret <- ggplot(Training, aes(x = Date)) +
  geom_line(aes(y = d_eur), color = primary_col, linewidth = 1.2) +
  labs(title = "Evolution of the EUR exchange rate log-return",
       subtitle = "June 03, 2018 – February 14, 2025",
       y = "EUR-HUF log-return",
       caption = "Source: Authors' own calculations and editing.") +
  scale_x_date(breaks = "year", date_labels = "%Y",
               limits = as.Date(c("2018-06-03", "2025-02-14"))) +
  theme_regina()


export_svg(p_ret, "evolution_of_log_return.svg", 1200, 600)
export_png(p_ret, "evolution_of_log_return.png", 1200, 600)
rm(p_ret)

# Durbin-Watson test      
dwtest(Training$d_eur ~ 1)      

# Correlograms
pacf(Training$d_eur)      
acf(Training$d_eur)      

pacf((Training$d_eur)^2) 
acf((Training$d_eur)^2)

# Breusch-Godfrey test
lmtest::bgtest((Training$d_eur)^2 ~ 1, order = 30)      


##------------------------ -
## 3.1 Time series properties ----
##------------------------ -

tsTarining <- as.ts(Training$d_eur)
# Calculating ACF and PACF values for the differenced EUR exchange rate
acf_values <- acf(Training$d_eur, lag.max = 15, plot = FALSE)
pacf_values <- pacf(Training$d_eur, lag.max = 15, plot = FALSE)

# Ljung-Box test to calculate Q-statistics for each lag
q_stat <- sapply(1:15, function(lag) Box.test(Training$d_eur, lag = lag, type = "Ljung-Box")$statistic)
p_values <- sapply(1:15, function(lag) Box.test(Training$d_eur, lag = lag, type = "Ljung-Box")$p.value)


korrelogram_data <- data.frame(Lag = 1:15,
                               Autocorrelation = round(acf_values$acf[-1], 3), 
                               "Partial Autocorrelation" = round(pacf_values$acf, 3),
                               "Q-Stat" = round(q_stat, 2),
                               "Prob" = round(p_values, 3),
                               check.names = FALSE)

korrelogram_data %>%
  kbl(digits = 8,        
      format.args = list(scientific = FALSE), 
      align = "c", 
      caption = "Correlogram for the differenced EUR exchange rate") %>%
  kable_styling(full_width = FALSE, position = "center")


# https://rh8liuqy.github.io/ACF_PACF_by_ggplot2.html
#' @title Plot Custom ACF and PACF
#' @description Generates a grid of custom-styled Autocorrelation (ACF) and Partial Autocorrelation (PACF) plots using ggplot2.
#' @param data The numeric vector or time series data.
#' @param lag.max Maximum number of lags to display.
#' @param ci Confidence interval level (default 0.95).
#' @param large.sample.size Logical; if TRUE, uses large sample approximation for variance.
#' @param horizontal Logical; if TRUE, aligns plots side-by-side. If FALSE, stacks them vertically.
#' @param ylim_min Minimum y-axis value.
#' @param ylim_max Maximum y-axis value.
#' @param ... Additional arguments.
#' @return A cowplot grid object containing both ACF and PACF plots.
ggplot.corr <- function(data, lag.max = 24, 
                        ci = 0.95, 
                        large.sample.size = TRUE, 
                        horizontal = TRUE, 
                        ylim_min = -1, 
                        ylim_max = 1, 
                        ...) {
  data <- na.omit(data)
  if(horizontal == TRUE) {numofrow <- 1} else {numofrow <- 2}
  
  list.acf <- acf(data, lag.max = lag.max, type = "correlation", plot = FALSE)
  N <- as.numeric(list.acf$n.used)
  df1 <- data.frame(lag = list.acf$lag, acf = list.acf$acf)
  
  list.pacf <- acf(data, lag.max = lag.max, type = "partial", plot = FALSE)
  df2 <- data.frame(lag = list.pacf$lag, pacf = list.pacf$acf)
  
  plot.acf <- ggplot(data = df1, 
                     aes(x = lag, 
                         y = acf)) +
    geom_hline(yintercept = 0, 
               color = secondary_col, 
               size = 0.7) +
    geom_col(fill = primary_col, 
             width = 0.7) +
    geom_hline(yintercept = qnorm((1 + ci) / 2) / sqrt(N), 
               color = third_col, 
               linetype = "dashed") +
    geom_hline(yintercept = -qnorm((1 + ci) / 2) / sqrt(N), 
               color = third_col, 
               linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, max(df1$lag), 1)) +
    scale_y_continuous(name = element_blank(), 
                       limits = c(ylim_min, 
                                  ylim_max)) +
    ggtitle("ACF") +
    theme_regina() +
    labs(y="Rate of correlation",
         x="Lags")
  
  plot.pacf <- ggplot(data = df2, 
                      aes(x = lag, 
                          y = pacf)) +
    geom_hline(yintercept = 0, 
               color = secondary_col, 
               size = 0.7) +  
    geom_col(fill = primary_col, 
             width = 0.7) +
    geom_hline(yintercept = qnorm((1 + ci) / 2) / sqrt(N), 
               color = third_col, 
               linetype = "dashed") +
    geom_hline(yintercept = -qnorm((1 + ci) / 2) / sqrt(N), 
               color = third_col, 
               linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, max(df2$lag, na.rm = TRUE), 1)) +
    scale_y_continuous(name = element_blank(), 
                       limits = c(ylim_min, 
                                  ylim_max)) +
    ggtitle("PACF") +
    theme_regina() +
    labs(y="Rate of correlation",
         x="Lags"
    )
  
  cowplot::plot_grid(plot.acf, plot.pacf, nrow = numofrow)
}

# caption = "Source: Calculations and editing https://rh8liuqy.github.io/ACF_PACF_by_ggplot2.html ."
c <- ggplot.corr(data = Training$d_eur, 
                 lag.max = 15, 
                 ci = 0.95, 
                 large.sample.size = TRUE, 
                 horizontal = TRUE, 
                 ylim_min = -0.1, 
                 ylim_max = 0.1) 

export_svg(c, "Plots/acf_pacf_vertical.svg", 1200, 900)
export_png(c, "Plots/acf_pacf_vertical.png", 1200, 900)

rm(tsTarining, acf_values, pacf_values, q_stat, p_values, korrelogram_data, ggplot.corr)

##======================== -
# 4. ARMA IC Tables 01 ----
##======================== -
AIC_table_arima <- matrix(NA, nrow = 7, ncol = 7)
rownames(AIC_table_arima) <- c("AR0", "AR1","AR2","AR3","AR4","AR5","AR6")
colnames(AIC_table_arima) <- c("MA0", "MA1","MA2","MA3","MA4","MA5", "MA6")

BIC_table_arima <- matrix(NA, nrow = 7, ncol = 7)
rownames(BIC_table_arima) <- c("AR0", "AR1","AR2","AR3","AR4","AR5","AR6")
colnames(BIC_table_arima) <- c("MA0", "MA1","MA2","MA3","MA4","MA5", "MA6")

HQ_table_arima <- matrix(NA, nrow = 7, ncol = 7)
rownames(HQ_table_arima) <- c("AR0", "AR1","AR2","AR3","AR4","AR5","AR6")
colnames(HQ_table_arima) <- c("MA0", "MA1","MA2","MA3","MA4","MA5", "MA6")

# Base ARMA (p, q)
for (i in 0:6) {
  for (j in 0:6) {
    arima_eur <- forecast::Arima(Training$d_eur, order = c(i, 0, j))
    AIC_table_arima[i +1, j +1] <- AIC(arima_eur)
    BIC_table_arima[i +1, j +1] <- BIC(arima_eur)
    HQ_table_arima[i +1, j +1] <- ICglm::HQIC(arima_eur)
  }
}

#' @title Highlight Minimum Kable
#' @description Highlights the global minimum cell(s) in bold and renders the kable table.
highlight_min_kable <- function(mat, caption) {
  min_val <- min(mat, na.rm = TRUE)
  pos <- which(mat == min_val, arr.ind = TRUE)   # highlight all if multiple identical minimums
  
  # Numbered, formatted character matrix (8 decimals, non-scientific)
  fmt <- function(x) formatC(x, format = "f", digits = 8)
  df_num <- as.data.frame(apply(mat, 2, fmt), stringsAsFactors = FALSE, check.names = FALSE)
  df <- data.frame(AR = rownames(mat), df_num, check.names = FALSE)
  
  # Bold at the minimum(s)
  for (k in seq_len(nrow(pos))) {
    i <- pos[k, 1]
    j <- pos[k, 2] + 1   # +1 because of the first "AR" column
    df[i, j] <- kableExtra::cell_spec(df[i, j], bold = TRUE)
  }
  
  kableExtra::kbl(df, escape = FALSE, align = "c", caption = caption) %>%
    kableExtra::kable_styling(full_width = FALSE, position = "center")
}

#' @title Report Minimum Value
#' @description Prints the minimum value and the corresponding (p,q) pair.
report_min <- function(mat, name = "AIC") {
  min_val <- min(mat, na.rm = TRUE)
  pos <- which(mat == min_val, arr.ind = TRUE)[1, ]  # first occurrence
  ar_lab <- rownames(mat)[pos[1]]  # e.g., "AR2"
  ma_lab <- colnames(mat)[pos[2]]  # e.g., "MA0"
  p <- as.integer(sub("^AR", "", ar_lab))
  q <- as.integer(sub("^MA", "", ma_lab))
  cat(sprintf("Min %s = %.8f → pair: (%d, %d)  [row: %s, col: %s]  → ARIMA(%d,0,%d)\n",
              name, min_val, p, q, ar_lab, ma_lab, p, q))
}

highlight_min_kable(
  AIC_table_arima,
  "Akaike Information Criterion for ARIMA(p,q) models"
)
report_min(AIC_table_arima, "AIC")

# BIC
highlight_min_kable(
  BIC_table_arima,
  "Bayesian-Schwarz Information Criterion for ARIMA(p,q) models"
)
report_min(BIC_table_arima, "BIC")

# HQ
highlight_min_kable(
  HQ_table_arima,
  "Hannan-Quinn Information Criterion for ARIMA(p,q) models"
)
report_min(HQ_table_arima, "HQ")
rm(AIC_table_arima, arima_eur, BIC_table_arima, HQ_table_arima, j, i)


# Baseline ARIMA(2,0,0)
y_train <- Training$d_eur[Training$Date >= train_start & Training$Date <  test_start]
y_test  <- Training$d_eur[Training$Date >= test_start  & Training$Date <= test_end]

# Fitting only on TRAIN
fit_arima200 <- forecast::Arima(y_train, order = c(2,0,0))
summary(fit_arima200)
aic_arima200 <- AIC(fit_arima200)
bic_arima200 <- BIC(fit_arima200)
hq_arima200  <- HQIC_fun(fit_arima200, length(y_train))

#  ROLLING 1-step forecast (dynamic, state update)
fit_roll <- fit_arima200
y_all <- Training$d_eur[Training$Date <= test_end]
y_pred_roll <- numeric(length(y_test))

for (i in seq_along(y_test)) {
  y_pred_roll[i] <- forecast::forecast(fit_roll, h = 1)$mean[1]
  # Update state with the next actual value, keeping parameters fixed (model=fit_roll)
  fit_roll <- forecast::Arima(y_all[1:(sum(train_idx)+i)], model = fit_roll)
}

rmse_r <- sqrt(mean((y_test - y_pred_roll)^2))
mae_r  <- mean(abs(y_test - y_pred_roll))
tic_r  <- sqrt(sum((y_test - y_pred_roll)^2)) /
  (sqrt(sum(y_test^2)) + sqrt(sum(y_pred_roll^2)))

cat(sprintf("ROLLING (1-step) | RMSE=%.6f  MAE=%.6f  TIC=%.6f\n",
            rmse_r, mae_r, tic_r))

#Coefficient Table (with stars
ct <- lmtest::coeftest(fit_arima200)
coeftab <- data.frame(
  Value     = round(ct[, "Estimate"],   4),
  Std_Error = round(ct[, "Std. Error"], 4),
  `z_value` = round(ct[, "z value"],    4),
  `p_value` = round(ct[, "Pr(>|z|)"],   4),
  Sign.     = sig_code(ct[, "Pr(>|z|)"]),
  check.names = FALSE
)
coeftab %>%
  kableExtra::kbl(digits = 8, format.args = list(scientific = FALSE),
                  align = "c", caption = "ARIMA(2,0,0) model coefficients") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center") %>%
  kableExtra::footnote(general = "Significance: *** p<0.01; ** p<0.05; * p<0.10.",
                       general_title = "", escape = FALSE)

# STATIC and ROLLING separate
df_roll <- data.frame(
  Date = dates_test,
  Actual = y_test,
  `Forecasted_rolling` = y_pred_roll,
  check.names = FALSE
)

p_roll <- ggplot(df_roll, aes(Date)) +
  geom_line(aes(y = Actual,             color = "Actual"), linewidth = 1.2) +
  geom_line(aes(y = Forecasted_rolling, color = "Forecasted (rolling)"), linewidth = 1.2) +
  scale_color_manual(values = c("Actual" = primary_col, "Forecasted (rolling)" = "red"), name = NULL) +
  labs(title = "Test period: actual vs. forecasted",
       subtitle = "ARIMA(2,0,0) – rolling 1-step forecast",
       y = "EUR/HUF log-return", caption = "Source: Authors' own calculations.") +
  theme_regina()

export_png(p_roll, "arima_forecasted_logreturn.png", 1600, 900)
export_svg(p_roll, "arima_forecasted_logreturn.svg", 1600, 900)


# ARIMAX(2,0,0) model estimation 
xreg_train_eur <- Training[, c("d_eur", 
                               "FED", "MNB", "ECB", 
                               "mean_matolcsy", "max_matolcsy", 
                               "mean_kinfo", "max_kinfo", 
                               "max_varga", "mean_varga", 
                               "max_nagy", "mean_nagy", 
                               #   "eurczk", "eurpln", "eurron",
                               "d_czk", "d_pln", "d_ron", 
                               #  "BUX", "cetop",
                               "d_bux", "d_cetop")]
cor(xreg_train_eur) 
cor_matrix <- cor(xreg_train_eur, use = "complete.obs")

corr_plott <- ggcorrplot(cor_matrix,
                         method = "square",    
                         type = "lower",      
                         lab = TRUE,          
                         lab_size = 3,
                         colors = c(secondary_col, "white", third_col), 
                         title = "Correlation matrix",
                         ggtheme = theme_minimal())
export_png(corr_plott, "correlation_matrix_d_all_d.png", 1200, 750)
# itt hal meg
export_svg(corr_plott, "correlation_matrix_d_all_d.svg", 1200, 750)


x_cols <- intersect(c( 
  "FED", "MNB", "ECB", 
  "mean_matolcsy", #"max_matolcsy", 
  "mean_kinfo", #"max_kinfo", 
  "max_varga", #"mean_varga", 
  "max_nagy", #"mean_nagy", 
  #   "eurczk", "eurpln", "eurron",
  "d_czk", "d_pln", "d_ron", 
  #  "BUX", "cetop",
  "d_bux", "d_cetop"), 
  names(Training))

X_train <- as.matrix(Training[train_idx, x_cols, drop = FALSE])
X_test  <- as.matrix(Training[test_idx,  x_cols, drop = FALSE])

# Train NA check
stopifnot(all(complete.cases(cbind(y_train, X_train))))

## Initial fitting (train) 
v_raw <- compute_vif(Training, x_cols)
vif_kable(v_raw, threshold = 10, caption = "VIF (raw)")

fit_arimax <- forecast::Arima(y_train, order = c(2,0,0), xreg = X_train)

# ICs (train)
aic_arimax <- AIC(fit_arimax); bic_arimax <- BIC(fit_arimax); hq_arimax <- HQIC_fun(fit_arimax, length(y_train))

# Cleaning test-window from NAs (X or y) 
test_dates <- Training$Date[test_idx]
ok_test <- complete.cases(cbind(y_test, X_test))
if (!all(ok_test)) {
  message("Test rows dropped due to NA: ", sum(!ok_test))
}
y_test_v   <- y_test[ok_test]
X_test_v   <- X_test[ok_test, , drop = FALSE]
dates_test <- test_dates[ok_test]

h <- nrow(X_test_v)

## STATIC h-step (reference) – without PI 
fc_static <- suppressWarnings(forecast::forecast(fit_arimax, h = h, xreg = X_test_v, level = 0))
y_pred_static <- as.numeric(fc_static$mean)

rmse_s <- sqrt(mean((y_test_v - y_pred_static)^2))
mae_s  <- mean(abs(y_test_v - y_pred_static))
tic_s  <- sqrt(sum((y_test_v - y_pred_static)^2)) /
  (sqrt(sum(y_test_v^2)) + sqrt(sum(y_pred_static^2)))

# ROLLING 1-step – expanding window
fit_i <- fit_arimax
y_pred_roll <- numeric(h)

for (i in seq_len(h)) {
  newx <- matrix(X_test_v[i, ], nrow = 1)
  y_pred_roll[i] <- suppressWarnings(forecast::forecast(fit_i, h = 1, xreg = newx, level = 0)$mean[1])
  
  # update with current actual + xreg integration
  y_upd <- c(y_train, y_test_v[seq_len(i)])
  X_upd <- rbind(X_train, X_test_v[seq_len(i), , drop = FALSE])
  fit_i <- forecast::Arima(y_upd, model = fit_i, xreg = X_upd)
}

rmse_r <- sqrt(mean((y_test_v - y_pred_roll)^2))
mae_r  <- mean(abs(y_test_v - y_pred_roll))
tic_r  <- sqrt(sum((y_test_v - y_pred_roll)^2)) /
  (sqrt(sum(y_test_v^2)) + sqrt(sum(y_pred_roll^2)))

## Summary table 
data.frame(
  Model = c("ARIMAX(2,0,0) – static h-step", "ARIMAX(2,0,0) – rolling 1-step"),
  RMSE   = c(rmse_s, rmse_r),
  MAE    = c(mae_s,  mae_r),
  TIC    = c(tic_s,  tic_r),
  AIC    = c(aic_arimax, NA),
  BIC    = c(bic_arimax, NA),
  HQ     = c(hq_arimax,  NA)
)


ct <- lmtest::coeftest(fit_arimax)
coeftab <- data.frame(
  Value     = round(ct[, "Estimate"],   4),
  Std_Error = round(ct[, "Std. Error"], 4),
  `z_value` = round(ct[, "z value"],    4),
  `p_value` = round(ct[, "Pr(>|z|)"],   4),
  Sign.     = sig_code(ct[, "Pr(>|z|)"]),
  check.names = FALSE
)
coeftab %>%
  kableExtra::kbl(digits = 4, format.args = list(scientific = FALSE),
                  align = "c", caption = "ARIMAX(2,0,0) model coefficients") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center") %>%
  kableExtra::footnote(general = "Significance: *** p<0.01; ** p<0.05; * p<0.10.",
                       general_title = "", escape = FALSE)


lmtest::bgtest(fit_arimax$residuals ~ 1, order = 15)  #p-value = 0.04599
hist(fit_arimax$residuals)
shapiro.test(fit_arimax$residuals)  #p-value < 2.2e-16


## ------------------ -
# 5. GARCH tests ----
## ----------------- -
# Engle's ARCH test to check for conditional heteroscedasticity
ArchTest(Training$d_eur, lags=15) # p-value < 2.2e-16

spec_garch <- ugarchspec(
  mean.model     = list(armaOrder = c(2,0)),         # ARMA(2,0) in the mean equation
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  distribution.model = "sstd"
)

fit_train <- ugarchfit(spec = spec_garch, data = y_train)

# ICs (train)
ic_train <- infocriteria(fit_train)
data.frame(AIC = ic_train[1], BIC = ic_train[2], HQ = ic_train[4])

# Coefficient table (robust SE) + significance stars
coefmat <- as.data.frame(fit_train@fit$robust.matcoef)
colnames(coefmat) <- c("Value", "Std_Error", "t_value", "p_value")
coefmat$Sign. <- sig_code(coefmat$`p_value`)

coefmat %>%
  kbl(digits = 6, align = "c",
      caption = "GARCH(1,1) ARMA(2,0) – Train fit, robust coefficients") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  footnote(general = "Significance: *** p<0.01; ** p<0.05; * p<0.10.",
           general_title = "", escape = FALSE)

# Train standard deviation (σ_t) plot
sigma_train <- as.numeric(sigma(fit_train))
df_sigma_tr <- data.frame(Date = dates_train, Sigma = sigma_train)

p_sigma_tr <- ggplot(df_sigma_tr, aes(Date, Sigma)) +
  geom_line(linewidth = 1.2, color = primary_col) +
  labs(title = "Forecasted standard deviation (σ) by time – TRAIN",
       subtitle = "ARMA(2,0)–GARCH(1,1), sstd",
       y = "Standard deviation",
       caption = "Source: Authors' own calculations.") +
  theme_regina()

export_png(p_sigma_tr, "garch_train_sigma.png", width = 1200, height = 750)
export_svg(p_sigma_tr, "garch_train_sigma.svg", width = 1200, height = 750)

## ======================== -
## 5.1 Estimation of GARCH(i,j) models and IC tables ----
## ======================== -
arma_order <- c(2, 0)
distribution_model_wox <- "sstd"
ic_results <- data.frame(i = integer(), 
                         j = integer(), 
                         AIC = numeric(), 
                         BIC = numeric(), 
                         HQ = numeric())

for (i in 0:2) {
  for (j in 0:2) {
    tryCatch({
      modell_spec <- ugarchspec(mean.model = list(armaOrder = arma_order),
                                variance.model = list(model = "sGARCH", garchOrder = c(i, j)),
                                distribution.model = distribution_model_wox)
      
      modell_fit <- ugarchfit(spec = modell_spec, data = Training$d_eur)
      
      ic <- infocriteria(modell_fit)
      ic_results <- rbind(ic_results, data.frame(i = i, j = j, AIC = ic[1], BIC = ic[2], HQ = ic[4]))
      
      cat("\nCoefficient matrix for GARCH(", i, ",", j, "):\n")
      print(modell_fit@fit$robust.matcoef)
      print("\n")
      
      becs_szoras <- sigma(modell_fit)
      
      ## [Gemini Review]: Warning - this base R plot will execute 9 times rapidly 
      ## in the viewer, meaning you will only realistically see the final GARCH(2,2) plot.
      plot(x = Training$Date,
           y = becs_szoras,
           type = "l",
           main = paste("Standard deviation over time GARCH(", i, ",", j, ")"),
           ylab = "Standard deviation")
      
    }, error = function(e) {
      cat("\nError fitting GARCH(", i, ",", j, "):\n")
      print(e$message)
      
    }, warning = function(w) {
      cat("\nWarning for GARCH(", i, ",", j, "):\n")
      print(w$message)
    })
  }
}

#' @title Highlight Minimum Value in GARCH Kable
#' @description Formats an Information Criterion matrix (spread by i, j orders) into an HTML/LaTeX table, automatically bolding the global minimum value(s).
#' @param tab The input dataframe/matrix.
#' @param caption The table caption.
#' @param digits Number of decimal places to format (default 8).
#' @param index_col Optional name of the index column to exclude from numeric minimum searches.
highlight_min_kable_garch <- function(tab, caption, digits = 8, index_col = NULL) {
  df <- as.data.frame(tab, stringsAsFactors = FALSE, check.names = FALSE)
  
  # 1) Separate the index column and the numeric part
  if (!is.null(index_col) && index_col %in% names(df)) {
    idx_name <- index_col
    idx_vals <- df[[index_col]]
    num_df   <- df[ , setdiff(names(df), index_col), drop = FALSE]
  } else if (ncol(df) >= 2 && (!is.numeric(df[[1]]) || names(df)[1] %in% c("i","AR","p"))) {
    idx_name <- names(df)[1]
    idx_vals <- df[[1]]
    num_df   <- df[ , -1, drop = FALSE]
  } else {
    idx_name <- "i"
    idx_vals <- if (!is.null(rownames(df))) rownames(df) else seq_len(nrow(df))
    num_df   <- df
  }
  
  # 2) Numeric matrix + location of minimum(s)
  num_mat <- apply(num_df, 2, as.numeric)
  min_val <- suppressWarnings(min(num_mat, na.rm = TRUE))
  pos     <- which(num_mat == min_val, arr.ind = TRUE)
  
  # 3) Formatting and bolding
  fmt <- function(x) formatC(x, format = "f", digits = digits)
  num_fmt <- apply(num_mat, 2, fmt)
  
  if (nrow(pos) > 0) {
    for (k in seq_len(nrow(pos))) {
      r <- pos[k, 1]; c <- pos[k, 2]
      num_fmt[r, c] <- kableExtra::cell_spec(num_fmt[r, c], bold = TRUE)
    }
  }
  
  out <- data.frame(idx_vals, as.data.frame(num_fmt, check.names = FALSE), check.names = FALSE)
  names(out)[1] <- idx_name
  
  kableExtra::kbl(out, escape = FALSE, align = "c", caption = caption) %>%
    kableExtra::kable_styling(full_width = FALSE, position = "center")
}

# AIC
aic_table_garch <- ic_results %>%
  select(i, j, AIC) %>%
  spread(j, AIC)

# BIC
bic_table_garch <- ic_results %>%
  select(i, j, BIC) %>%
  spread(j, BIC)

# HQ
hq_table_garch <- ic_results %>%
  select(i, j, HQ) %>%
  spread(j, HQ)

# AIC
highlight_min_kable_garch(
  aic_table_garch,
  "Akaike Information Criterion for GARCH(i,j) models"
)
# BIC
highlight_min_kable_garch(
  bic_table_garch,
  "Bayesian-Schwarz Information Criterion for GARCH(i,j) models"
)
# HQ
highlight_min_kable_garch(
  hq_table_garch,
  "Hannan-Quinn Information Criterion for GARCH(i,j) models"
)

# Display best models
cat("\nBest model based on AIC:\n")
print(ic_results[which.min(ic_results$AIC), ]) #1,1

cat("\nBest model based on BIC:\n")
print(ic_results[which.min(ic_results$BIC), ]) #1,1

cat("\nBest model based on HQ:\n")
print(ic_results[which.min(ic_results$HQ), ]) #1,1

# Example usage 
regressors <- c("FED","MNB","ECB","mean_matolcsy","mean_kinfo","mean_varga","mean_nagy",
                "d_czk","d_pln","d_ron","d_bux", "d_cetop" )

# Raw VIFs
v_raw <- compute_vif(Training[Training$Date >= train_start & Training$Date < test_start, ], regressors)
vif_kable(v_raw, threshold = 10, caption = "VIF (raw)")

# Iterative VIF-pruning with threshold 10
v_res <- vif_iterative_prune(Training, regressors, threshold = 10)
v_res$steps          # removal steps
v_res$final_vars     # kept variables
vif_kable(v_res$final_vif, threshold = 10, caption = "VIF (after pruning)")
v_res$kappa          # condition number of the design matrix on the final set


## ============================================================ -
# 6. Full ARM(A|X)–GARCH(X) model preparation and selection pipeline ----
## ============================================================ -

#' @title Normalize Target Variable Name
#' @description Safely extracts the string name of the target variable, regardless of how it was passed (bare, quoted, or as a variable).
normalize_target <- function(x) {
  expr <- rlang::enexpr(x)
  if (rlang::is_string(expr)) return(as.character(expr))
  val <- tryCatch(rlang::eval_bare(expr, env = parent.frame()), error = function(e) NULL)
  if (is.character(val) && length(val)==1) return(val)
  rlang::as_name(expr)
}

## - 6.1 Branch pruning memory (Caching failed combinations) ----
.bad_env <- new.env(parent = emptyenv())
.bad_env$min_bad <- list()

add_min_bad <- function(vars) {
  vars <- sort(unique(vars))
  for (s in .bad_env$min_bad) if (all(s %in% vars)) return(invisible(FALSE))
  keep <- vapply(.bad_env$min_bad, function(s) !all(vars %in% s), logical(1))
  .bad_env$min_bad <- .bad_env$min_bad[keep]
  .bad_env$min_bad <- append(.bad_env$min_bad, list(vars))
  TRUE
}
is_superset_of_any_bad <- function(vars) {
  vars <- sort(unique(vars))
  for (s in .bad_env$min_bad) if (all(s %in% vars)) return(TRUE)
  FALSE
}

## 6.2 Matrix diagnostics ----
#' @title Check Matrix Design
#' @description Checks if a design matrix is bad (contains NAs, is rank-deficient, or has a condition number exceeding a threshold).
is_bad_design <- function(X, cond_thresh = 1e8) {
  if (is.null(X) || length(X) == 0) return(FALSE)
  X <- as.matrix(X)
  if (any(!is.finite(X))) return(TRUE)
  if (qr(X)$rank < ncol(X)) return(TRUE)
  s <- tryCatch(svd(X, nu=0, nv=0)$d, error=function(e) NA_real_)
  if (any(is.na(s))) return(TRUE)
  if (min(s) == 0) return(TRUE)
  (max(s)/min(s)) > cond_thresh
}

## 6.3) Cache ----
#' @title Generate Unique Cache Key
#' @description Creates an MD5 hash key based on the model parameters to enable caching of already-run models.
mk_key <- function(prefix, vars, extra=list()){
  vv <- paste(sort(vars), collapse="|")
  ee <- if (length(extra)) paste(paste(names(extra), unlist(extra), sep="="), collapse=";") else ""
  digest::digest(paste(prefix, vv, ee, sep="::"))
}

cache_put <- function(dir, key, obj){
  dir.create(dir, showWarnings=FALSE, recursive=TRUE)
  saveRDS(obj, file=file.path(dir, paste0(key, ".rds")))
}

cache_get <- function(dir, key){
  f <- file.path(dir, paste0(key, ".rds"))
  if (file.exists(f)) tryCatch(readRDS(f), error=function(e) NULL) else NULL
}

## 6.4 ARIMAX Fast IC -----
HQ_from_ll <- function(loglik, k, n){
  L <- suppressWarnings(log(log(n))); if (!is.finite(L) || L<=0) L <- 1
  as.numeric(-2*loglik + 2*k*L)
}

#' @title Quick ARIMAX Information Criteria
#' @description Quickly fits an ARIMAX model and extracts AIC, BIC, and HQIC on a per-observation scale to match rugarch output.
quick_arimax_ic <- function(y, X, order = c(2, 0, 0), max_sec = 100) {
  out <- c(AIC = Inf, BIC = Inf, HQ = Inf)
  try({
    R.utils::withTimeout({
      fit <- forecast::Arima(y, order = order, xreg = X)
      ll  <- as.numeric(logLik(fit))
      k   <- length(coef(fit))
      n   <- length(y)
      # Total ICs
      AIC_tot <- -2 * ll + 2 * k
      BIC_tot <- -2 * ll + k * log(n)
      HQ_tot  <- {
        L <- suppressWarnings(log(log(n)))
        if (!is.finite(L) || L <= 0) L <- 1
        -2 * ll + 2 * k * L
      }
      # Scaled per-observation (rugarch compatibility)
      out["AIC"] <- AIC_tot / n
      out["BIC"] <- BIC_tot / n
      out["HQ"]  <- HQ_tot  / n
    }, timeout = max_sec, onTimeout = "error")
  }, silent = TRUE)
  out
}

## 6.5 GARCHX (variance) Fast IC ----
#' @title Quick GARCHX Variance Information Criteria
#' @description Fits a GARCHX model with external regressors in the variance equation and returns Information Criteria.
quick_garchx_var_ic <- function(y, X, armaOrder=c(2,0), garchOrder=c(1,1),
                                model="sGARCH", submodel=NULL, dist="sstd",
                                max_sec=100){
  out <- c(AIC=Inf, BIC=Inf, HQ=Inf)
  if (!is.null(X)) {
    X <- as.matrix(X)
    if (NROW(X) != length(y)) {
      n <- min(NROW(X), length(y)); X <- X[seq_len(n), , drop=FALSE]; y <- y[seq_len(n)]
    }
  }
  try({
    spec <- rugarch::ugarchspec(
      mean.model=list(armaOrder=armaOrder, external.regressors=NULL),
      variance.model=list(model=model, submodel=submodel, garchOrder=garchOrder,
                          external.regressors=X),
      distribution.model=dist
    )
    R.utils::withTimeout({
      fit <- rugarch::ugarchfit(spec=spec, data=y, solver="hybrid")
      ic  <- suppressWarnings(rugarch::infocriteria(fit))
      out["AIC"] <- ic[1]; out["BIC"] <- ic[2]; out["HQ"] <- ic[4]
    }, timeout=max_sec, onTimeout="error")
  }, silent=TRUE)
  out
}

rank_by_ic <- function(tbl, metric = c("AIC", "BIC", "HQ"), top = NULL) {
  metric <- match.arg(metric)
  cand1  <- metric
  cand2  <- paste0(metric, "_perobs")
  col_ic <- if (cand1 %in% names(tbl)) cand1 else if (cand2 %in% names(tbl)) cand2 else
    stop("IC column not found: '", cand1, "' or '", cand2, "'.")
  
  out <- tbl[is.finite(tbl[[col_ic]]), , drop = FALSE]
  if (!nrow(out)) return(tbl[0, , drop = FALSE])
  out <- out[order(out[[col_ic]], decreasing = FALSE), , drop = FALSE]
  if (!is.null(top)) out <- head(out, top)
  out
}
## 6.6 Subsets -----
#' @title Generate All Subsets
#' @description Given a vector of variables, returns a list of all possible variable combinations.
all_subsets <- function(v){
  v <- sort(unique(v)); n <- length(v); if (n==0) return(list())
  out <- vector("list", 2^n - 1); idx <- 1L
  for (k in 1:n) {
    cmb <- utils::combn(v, k, simplify=FALSE)
    for (x in cmb) { out[[idx]] <- x; idx <- idx + 1L }
  }
  out
}

## 6.7 Gain Estimator: generating_arima_garch ----
#' @title Generate ARIMAX-GARCHX Forecasts
#' @description Fits an ARIMAX-GARCHX model on a training set, performs a rolling forecast on a test set, and returns ICs, error metrics, and VaR estimates.
generating_arima_garch <- function(
    data,
    target_variable,                     # bare or "string" - for compatibility
    train_start, test_start, test_end,
    arima_order        = c(2,0),         # rugarch mean: c(p,q)
    arima_regressors   = NULL,
    garch_model        = "sGARCH",
    sub_model          = NULL,
    garch_order        = c(1,1),
    garch_regressors   = NULL,
    distribution_model = "sstd",
    VAR_alpha          = 0.05,
    refit_every        = 1,
    refit_window       = c("moving","expanding"),
    plots              = FALSE,
    out_prefix         = "arma_garch",
    y_col              = NULL            # <- Actual target column name (string)
){
  refit_window <- match.arg(refit_window)
  if (is.null(y_col)) y_col <- rlang::as_name(rlang::ensym(target_variable))
  has_reg  <- function(v) !is.null(v) && length(v) > 0
  safe_TIC <- function(a, f) {
    num <- sum((a - f)^2); den <- (sqrt(sum(a^2)) + sqrt(sum(f^2)))
    if (den == 0) NA_real_ else sqrt(num)/den
  }
  build_spec <- function(xm, xv){
    rugarch::ugarchspec(
      mean.model     = list(armaOrder=arima_order, external.regressors=xm),
      variance.model = list(model=garch_model, submodel=sub_model,
                            garchOrder=garch_order, external.regressors=xv),
      distribution.model = distribution_model
    )
  }
  garch_label <- if (!is.null(sub_model) && nzchar(sub_model)) sprintf("%s-%s", garch_model, sub_model) else garch_model
  
  stopifnot("Date" %in% names(data))
  all_needed <- c("Date", y_col,
                  if (has_reg(arima_regressors)) arima_regressors,
                  if (has_reg(garch_regressors)) garch_regressors)
  miss <- setdiff(all_needed, names(data))
  if (length(miss)) stop("Missing column(s): ", paste(miss, collapse=", "))
  
  df <- data[, all_needed, drop=FALSE]
  df$Date <- as.Date(df$Date)
  df <- df[stats::complete.cases(df), , drop=FALSE]
  if (!nrow(df)) stop("No non-NA rows in the required columns.")
  
  train_idx <- df$Date >= train_start & df$Date <  test_start
  test_idx  <- df$Date >= test_start  & df$Date <= test_end
  if (!any(train_idx) || !any(test_idx)) stop("Train/test slice is empty. Check dates.")
  
  y_all   <- df[[y_col]]
  y_train <- y_all[train_idx]
  y_test  <- y_all[test_idx]
  
  Xmean_full <- if (has_reg(arima_regressors)) as.matrix(df[, arima_regressors, drop=FALSE]) else NULL
  Xvar_full  <- if (has_reg(garch_regressors))  as.matrix(df[, garch_regressors,  drop=FALSE]) else NULL
  Xmean_tr   <- if (!is.null(Xmean_full)) Xmean_full[train_idx, , drop=FALSE] else NULL
  Xvar_tr    <- if (!is.null(Xvar_full))  Xvar_full[train_idx,  , drop=FALSE] else NULL
  
  try_orders <- expand.grid(mean_rev=c(FALSE,TRUE), var_rev=c(FALSE,TRUE), KEEP.OUT.ATTRS=FALSE)
  fit_train <- NULL; used_perm <- NULL; last_err <- NULL
  for (r in seq_len(nrow(try_orders))) {
    mean_rev <- try_orders$mean_rev[r]; var_rev <- try_orders$var_rev[r]
    xm_tr <- if (!is.null(Xmean_tr) && mean_rev && ncol(Xmean_tr)>1) Xmean_tr[, ncol(Xmean_tr):1, drop=FALSE] else Xmean_tr
    xv_tr <- if (!is.null(Xvar_tr)  && var_rev  && ncol(Xvar_tr) >1) Xvar_tr[,  ncol(Xvar_tr) :1, drop=FALSE] else Xvar_tr
    spec_tr <- build_spec(xm_tr, xv_tr)
    cat(sprintf("[train] TRY mean_rev=%s var_rev=%s | model=%s\n", mean_rev, var_rev, garch_label)); flush.console()
    fit_train <- tryCatch(rugarch::ugarchfit(spec=spec_tr, data=y_train, solver="hybrid"),
                          error=function(e){ last_err <<- conditionMessage(e); NULL })
    if (!is.null(fit_train)) { used_perm <- list(mean_rev=mean_rev, var_rev=var_rev); break }
  }
  if (is.null(fit_train)) stop("TRAIN fitting failed in all orders: ", last_err)
  
  ic_vec <- suppressWarnings(rugarch::infocriteria(fit_train))
  n_tr   <- length(y_train)
  ic_tbl <- data.frame(Metric=c("AIC","BIC","HQ"),
                       PerObs=c(ic_vec[1], ic_vec[2], ic_vec[4]),
                       Total =c(ic_vec[1], ic_vec[2], ic_vec[4]) * n_tr,
                       stringsAsFactors=FALSE)
  
  mu_tr <- as.numeric(fitted(fit_train))
  train_err <- data.frame(Set="Train",
                          RMSE=sqrt(mean((y_train-mu_tr)^2)),
                          MAE =mean(abs(y_train-mu_tr)),
                          TIC =safe_TIC(y_train, mu_tr))
  
  coef_raw <- as.data.frame(fit_train@fit$robust.matcoef)
  names(coef_raw) <- c("Coefficient","Std_Error","t_value","p_value")
  coef_raw$Parameter <- rownames(coef_raw)
  coef_raw$Stars <- sig_code(coef_raw$`p_value`)
  coef_tbl <- coef_raw[, c("Parameter","Coefficient","Std_Error","t_value","p_value","Stars")]
  params_table <- coef_tbl
  
  ## rolling
  dates_all <- df$Date; dates_train <- df$Date[train_idx]; dates_test <- df$Date[test_idx]
  if (FALSE && plots) {  # (optional σ-plot: set TRUE if needed)
    df_sigma_tr <- data.frame(Date=dates_train, Sigma=as.numeric(rugarch::sigma(fit_train)))
    p_sigma <- ggplot2::ggplot(df_sigma_tr, ggplot2::aes(Date, Sigma)) +
      ggplot2::geom_line(linewidth=1.2, color=primary_col) +
      ggplot2::labs(title="Forecasted standard deviation (σ) – TRAIN",
                    subtitle=sprintf("ARMA(%d,%d)–%s(%d,%d), %s | order: mean_rev=%s, var_rev=%s",
                                     arima_order[1], arima_order[2], garch_label,
                                     garch_order[1], garch_order[2],
                                     distribution_model, used_perm$mean_rev, used_perm$var_rev),
                    y="Standard deviation", caption="Source: Authors' own calculations.") +
      theme_regina()
    invisible(export_png(p_sigma, sprintf("%s_train_sigma.png", out_prefix)))
  }
  
  n_start <- sum(train_idx)
  Xmean_full_used <- if (!is.null(Xmean_full) && used_perm$mean_rev && ncol(Xmean_full)>1)
    Xmean_full[, ncol(Xmean_full):1, drop=FALSE] else Xmean_full
  Xvar_full_used  <- if (!is.null(Xvar_full)  && used_perm$var_rev  && ncol(Xvar_full) >1)
    Xvar_full[,  ncol(Xvar_full) :1, drop=FALSE] else Xvar_full
  
  roll <- NULL; last_err_roll <- NULL
  for (r in seq_len(nrow(try_orders))) {
    mean_rev2 <- try_orders$mean_rev[r]; var_rev2 <- try_orders$var_rev[r]
    xm_full <- if (!is.null(Xmean_full) && mean_rev2 && ncol(Xmean_full)>1)
      Xmean_full[, ncol(Xmean_full):1, drop=FALSE] else Xmean_full
    xv_full <- if (!is.null(Xvar_full) && var_rev2 && ncol(Xvar_full)>1)
      Xvar_full[, ncol(Xvar_full):1, drop=FALSE] else Xvar_full
    
    spec_full_try <- rugarch::ugarchspec(
      mean.model     = list(armaOrder=arima_order, external.regressors=xm_full),
      variance.model = list(model=garch_model, submodel=sub_model,
                            garchOrder=garch_order, external.regressors=xv_full),
      distribution.model = distribution_model
    )
    cat(sprintf("[roll] TRY mean_rev=%s var_rev=%s | model=%s | refit=%d/%s\n",
                mean_rev2, var_rev2, garch_label, refit_every, refit_window)); flush.console()
    
    roll <- tryCatch(
      rugarch::ugarchroll(
        spec=spec_full_try, data=y_all, n.start=n_start,
        refit.every=refit_every, refit.window=refit_window,
        calculate.VaR=TRUE, VaR.alpha=VAR_alpha, solver="hybrid", keep.coef=TRUE
      ),
      error=function(e){ last_err_roll <<- conditionMessage(e); NULL }
    )
    if (!is.null(roll)) break
  }
  if (is.null(roll)) stop("ROLLING forecast failed: ", last_err_roll)
  
  dens <- as.data.frame(roll@forecast$density)
  oos_from <- n_start + 1
  end_idx  <- min(oos_from + nrow(dens) - 1, length(dates_all))
  dens <- dens[seq_len(end_idx - oos_from + 1), , drop=FALSE]
  dens$Date <- dates_all[oos_from:end_idx]
  dens_test <- subset(dens, Date >= test_start & Date <= test_end)
  stopifnot(nrow(dens_test) == length(y_test))
  
  y_pred_oos <- dens_test$Mu
  test_err <- data.frame(Set="Test (rolling OOS)",
                         RMSE=sqrt(mean((y_test-y_pred_oos)^2)),
                         MAE=mean(abs(y_test-y_pred_oos)),
                         TIC=safe_TIC(y_test, y_pred_oos))
  errors_table <- dplyr::bind_rows(train_err, test_err)
  
  vr  <- as.data.frame(roll@forecast$VaR)
  vr  <- vr[seq_len(end_idx - oos_from + 1), , drop=FALSE]
  vr$Date <- dates_all[oos_from:end_idx]
  vr_test <- subset(vr, Date >= test_start & Date <= test_end)
  alpha_col <- grep("^alpha\\(", names(vr_test), value=TRUE)[1]
  var_table <- data.frame(Date=vr_test$Date,
                          Realized=vr_test$realized,
                          VaR_level=alpha_col,
                          VaR_value=vr_test[[alpha_col]],
                          Breach=vr_test$realized < vr_test[[alpha_col]])
  
  list(
    tables = list(
      info_criteria = ic_tbl,
      errors        = errors_table,
      var           = var_table,
      coefs_train   = coef_tbl,
      params_all    = params_table,
      orders_used   = data.frame(mean_reversed=used_perm$mean_rev,
                                 var_reversed =used_perm$var_rev)
    ),
    plots = list(),
    objects = list(fit_train=fit_train, roll=roll)
  )
}

## 6.8 Model Grid -----
garch_model_grid <- tibble::tribble(
  ~garch_model, ~sub_model,
  "sGARCH",     NA_character_,
  "eGARCH",     NA_character_,
  "fGARCH",     "TGARCH",
  "fGARCH",     "GJRGARCH"
)

## 6.9 Single Level Scanner (MEAN or VAR) ----
`%or%` <- function(a,b) if (is.null(a) || (length(a)==1 && is.na(a))) b else a

#' @title Variable Combination Scanner
#' @description Evaluates all possible combinations of external regressors for either the Mean or Variance equation using parallel processing.
scan_level <- function(
    data,
    date_col = "Date",
    y_col,                                 # <- ALWAYS STRING COLUMN NAME!
    train_start, test_start, test_end,
    regressors,
    role = c("mean","var"),
    arima_order = c(2,0,0),
    garch_order = c(1,1),
    garch_model_grid = tibble::tibble(garch_model="sGARCH", sub_model=NA_character_),
    dist = "sstd",
    cond_thresh = 1e8,
    max_sec_each = 100,
    cache_dir = "cache_scan",
    cores = max(1, parallel::detectCores()-1),
    log_each = TRUE,
    drop_constant_cols = TRUE
){
  role <- match.arg(role)
  if (!date_col %in% names(data)) stop("Missing date column: ", date_col)
  if (!y_col   %in% names(data)) stop("Missing target variable column: ", y_col)
  
  regressors <- unique(regressors)
  if (!all(regressors %in% names(data))) {
    miss <- setdiff(regressors, names(data))
    stop("Missing regressor(column): ", paste(miss, collapse=", "))
  }
  
  data[[date_col]] <- as.Date(data[[date_col]])
  train_idx <- data[[date_col]] >= train_start & data[[date_col]] < test_start
  if (!any(train_idx)) stop("Train slice is empty – check train_start/test_start dates.")
  
  y_train <- data[[y_col]][train_idx]
  if (any(!is.finite(y_train))) stop("Train target variable contains non-finite values.")
  
  Xall_tr <- as.matrix(data[train_idx, regressors, drop=FALSE])
  colnames(Xall_tr) <- regressors
  
  if (drop_constant_cols) {
    sds <- apply(Xall_tr, 2, sd, na.rm=TRUE)
    keep <- is.finite(sds) & sds > 0
    if (any(!keep) && log_each) cat(sprintf("[scan][%s] DROP constant column(s): %s\n",
                                            role, paste(colnames(Xall_tr)[!keep], collapse=", ")))
    Xall_tr <- Xall_tr[, keep, drop=FALSE]
    regressors <- colnames(Xall_tr)
  }
  if (ncol(Xall_tr) == 0) stop("Train regressor matrix became empty (all columns degenerate).")
  
  combos <- all_subsets(regressors)
  if (length(combos)==0) stop("Failed to create subsets from regressors.")
  
  jobs <- if (role=="mean") {
    tibble::tibble(vars_list=combos, garch_model=NA_character_, sub_model=NA_character_)
  } else {
    tidyr::crossing(tibble::tibble(vars_list=combos), garch_model_grid)
  }
  
  if (nrow(jobs)==0) stop("Job list is empty – nothing to scan.")
  if (log_each) { cat(sprintf("[scan][%s] started | number of jobs: %d\n", role, nrow(jobs))); flush.console() }
  
  oplan <- future::plan(); on.exit(future::plan(oplan), add=TRUE)
  future::plan(future::multisession, workers=cores)
  
  progressr::with_progress({
    p <- progressr::progressor(steps=nrow(jobs))
    res <- future.apply::future_lapply(
      seq_len(nrow(jobs)),
      FUN=function(ii){
        vars <- jobs$vars_list[[ii]]
        gm   <- jobs$garch_model[[ii]]
        sm   <- jobs$sub_model[[ii]]
        
        if (is_superset_of_any_bad(vars)) {
          out <- data.frame(role=role, vars=paste(vars, collapse=","),
                            garch_model=gm, sub_model=sm,
                            AIC=Inf, BIC=Inf, HQ=Inf,
                            elapsed_sec=0, status="SKIP:superset-pruned",
                            conv=NA_integer_, msg=NA_character_, cache=FALSE,
                            stringsAsFactors=FALSE)
          if (log_each) cat(sprintf("[scan][%s] PRUNE | vars={%s}\n", role, out$vars))
          p(); return(out)
        }
        
        X_tr <- as.matrix(Xall_tr[, vars, drop=FALSE])
        if (is_bad_design(X_tr, cond_thresh)) {
          add_min_bad(vars)
          out <- data.frame(role=role, vars=paste(vars, collapse=","),
                            garch_model=gm, sub_model=sm,
                            AIC=Inf, BIC=Inf, HQ=Inf,
                            elapsed_sec=0, status="SKIP:design",
                            conv=NA_integer_, msg="bad design (rank/condition/NA)",
                            cache=FALSE, stringsAsFactors=FALSE)
          if (log_each) cat(sprintf("[scan][%s] SKIP design | vars={%s}\n", role, out$vars))
          p(); return(out)
        }
        
        key <- mk_key(sprintf("scan_%s", role), vars,
                      list(ar=toString(arima_order),
                           gar=toString(garch_order),
                           gm=gm, sm=sm, d=dist,
                           ts=as.character(train_start)))
        
        cs <- cache_get(cache_dir, key)
        if (!is.null(cs)) {
          if (log_each) cat(sprintf("[scan][%s] CACHE | vars={%s} | model=%s/%s | BIC=%.6f\n",
                                    role, paste(vars, collapse=","), (gm %or% "ARIMAX"), (sm %or% "NA"), cs$BIC))
          cs$cache <- TRUE; p(); return(cs)
        }
        
        t0 <- Sys.time()
        ic <- c(AIC=Inf, BIC=Inf, HQ=Inf); status <- "OK"
        if (log_each) cat(sprintf("[scan][%s] START | vars={%s} | model=%s/%s\n",
                                  role, paste(vars, collapse=","), (gm %or% "ARIMAX"), (sm %or% "NA")))
        
        try({
          if (role=="mean") {
            ic <- quick_arimax_ic(y=y_train, X=X_tr, order=arima_order, max_sec=max_sec_each)
          } else {
            ic <- quick_garchx_var_ic(y=y_train, X=X_tr,
                                      armaOrder=arima_order[1:2], garchOrder=garch_order,
                                      model=gm, submodel=sm, dist=dist, max_sec=max_sec_each)
          }
          status <- if (all(is.finite(ic))) "OK" else "FAIL"
        }, silent=TRUE)
        
        elapsed <- as.numeric(difftime(Sys.time(), t0, units="secs"))
        out <- data.frame(role=role, vars=paste(vars, collapse=","), garch_model=gm, sub_model=sm,
                          AIC=ic["AIC"], BIC=ic["BIC"], HQ=ic["HQ"],
                          elapsed_sec=elapsed, status=status,
                          conv=NA_integer_, msg=NA_character_, cache=FALSE,
                          stringsAsFactors=FALSE)
        cache_put(cache_dir, key, out)
        if (log_each) cat(sprintf("[scan][%s] DONE  | vars={%s} | model=%s/%s | AIC=%.6f | BIC=%.6f | HQ=%.6f | t=%.2fs\n",
                                  role, out$vars, (gm %or% "ARIMAX"), (sm %or% "NA"),
                                  out$AIC, out$BIC, out$HQ, out$elapsed_sec))
        p(); out
      },
      future.seed=TRUE,
      future.packages=c("rlang","digest","R.utils","forecast","rugarch",
                        "data.table","tibble","tidyr","dplyr","ggplot2")
    )
    data.table::rbindlist(res, fill=TRUE)
  })
}

## 6.10 MIXED Runner ----
#' @title Mixed Combinations Runner
#' @description Takes the top Mean models and top Variance models and runs full ARIMAX-GARCHX pipelines on their cross-product combinations.
run_mixed_heavy <- function(
    data, y_col,                      # <- STRING target column name
    train_start, test_start, test_end,
    mean_top, var_top,
    garch_model_grid,
    dist="sstd", arima_order=c(2,0), garch_order=c(1,1),
    refit_every=10, refit_window="moving",
    max_pairs=400, cores=max(1, parallel::detectCores()-1),
    cache_dir="cache_mixed", plots=FALSE
){
  stopifnot(y_col %in% names(data))
  
  pairs <- tidyr::crossing(
    tibble::tibble(mean = mean_top$vars),
    tibble::tibble(var  = var_top$vars),
    garch_model_grid
  )
  if (nrow(pairs) > max_pairs) pairs <- pairs[seq_len(max_pairs), , drop=FALSE]
  
  oplan <- future::plan(); on.exit(future::plan(oplan), add=TRUE)
  future::plan(future::multisession, workers=cores)
  
  progressr::with_progress({
    p <- progressr::progressor(steps=nrow(pairs))
    res <- future.apply::future_lapply(
      seq_len(nrow(pairs)),
      FUN=function(i){
        m  <- unlist(strsplit(pairs$mean[i], ",", fixed=TRUE))
        v  <- unlist(strsplit(pairs$var[i],  ",", fixed=TRUE))
        gm <- pairs$garch_model[[i]]
        sm <- pairs$sub_model[[i]]
        
        key <- mk_key("mixed",
                      vars=c(paste0("mean:", sort(m)), paste0("var:", sort(v))),
                      extra=list(ar=toString(arima_order),
                                 gar=toString(garch_order),
                                 gm=gm, sm=sm, d=dist,
                                 tr=as.character(train_start),
                                 te=as.character(test_end)))
        cs <- cache_get(cache_dir, key)
        if (!is.null(cs)) {
          cat(sprintf("[mixed] CACHE | mean={%s} var={%s} | %s/%s | BIC=%.6f\n",
                      paste(m, collapse="+"), paste(v, collapse="+"),
                      (gm %or% "NA"), (sm %or% "NA"), cs$BIC_perobs %or% NA_real_))
          flush.console(); p(); return(cs)
        }
        
        cat(sprintf("[mixed] START | mean={%s} | var={%s} | model=%s/%s\n",
                    paste(m, collapse="+"), paste(v, collapse="+"),
                    (gm %or% "NA"), (sm %or% "NA"))); flush.console()
        
        out <- tryCatch({
          r <- generating_arima_garch(
            data               = data,
            target_variable    = y_col,      # compat param, but y_col also goes:
            y_col              = y_col,      # <- this is the decisive one
            train_start        = train_start,
            test_start         = test_start,
            test_end           = test_end,
            arima_order        = arima_order,
            arima_regressors   = if (length(m)) m else NULL,
            garch_model        = gm,
            sub_model          = sm,
            garch_order        = garch_order,
            garch_regressors   = if (length(v)) v else NULL,
            distribution_model = dist,
            VAR_alpha          = 0.05,
            refit_every        = refit_every,
            refit_window       = refit_window,
            plots              = plots,
            out_prefix         = sprintf("MIX_%s_%s", (gm %or% "NA"), (sm %or% "NA"))
          )
          ic  <- r$tables$info_criteria
          err <- r$tables$errors
          tab <- tibble::tibble(
            mean = paste(m, collapse="+"), var = paste(v, collapse="+"),
            garch_model = gm, sub_model = sm,
            AIC_perobs = ic$PerObs[ic$Metric=="AIC"],
            BIC_perobs = ic$PerObs[ic$Metric=="BIC"],
            HQ_perobs  = ic$PerObs[ic$Metric=="HQ"],
            Train_RMSE = err$RMSE[err$Set=="Train"],
            Test_RMSE  = err$RMSE[grepl("Test", err$Set)],
            Train_MAE  = err$MAE[err$Set=="Train"],
            Test_MAE   = err$MAE[grepl("Test", err$Set)],
            Train_TIC  = err$TIC[err$Set=="Train"],
            Test_TIC   = err$TIC[grepl("Test", err$Set)],
            status     = "OK"
          )
          cat(sprintf("[mixed] DONE  | %s/%s | BIC=%.6f | TestRMSE=%.6f\n",
                      (gm %or% "NA"), (sm %or% "NA"), tab$AIC_perobs, tab$Test_RMSE))
          flush.console()
          cache_put(cache_dir, key, tab)
          tab
        }, error=function(e){
          cat(sprintf("[mixed] ERROR | mean={%s} | var={%s} | %s/%s | %s\n",
                      paste(m, collapse="+"), paste(v, collapse="+"),
                      (gm %or% "NA"), (sm %or% "NA"), conditionMessage(e)))
          flush.console()
          tibble::tibble(
            mean = paste(m, collapse="+"), var = paste(v, collapse="+"),
            garch_model = gm, sub_model = sm,
            AIC_perobs = NA_real_, BIC_perobs = NA_real_, HQ_perobs = NA_real_,
            Train_RMSE = NA_real_, Test_RMSE  = NA_real_,
            Train_MAE  = NA_real_, Test_MAE   = NA_real_,
            Train_TIC  = NA_real_, Test_TIC   = NA_real_,
            status     = paste("ERROR:", conditionMessage(e))
          )
        })
        p(); out
      },
      future.seed=TRUE,
      future.packages=c("rlang","digest","R.utils","forecast","rugarch",
                        "data.table","tibble","tidyr","dplyr","ggplot2")
    )
    data.table::rbindlist(res, fill=TRUE)
  })
}

## 6.11 Orchestrator -----
#' @title Full Pipeline Orchestrator
#' @description Wraps the mean scan, variance scan, and mixed combinations into a single automated pipeline execution.
run_full_pipeline <- function(
    data, target_variable, regressors,
    train_start, test_start, test_end,
    arima_order=c(2,0,0),
    garch_order=c(1,1),
    garch_model_grid=garch_model_grid,
    dist="sstd",
    cond_thresh=1e8,
    top_mean=60, top_var=60,
    top_mean_for_mixed=20, top_var_for_mixed=20, max_pairs=400,
    t_mean_each=8, t_var_each=12,
    cores=max(1, parallel::detectCores()-1),
    cache_scan_dir="cache_scan",
    cache_mixed_dir="cache_mixed",
    plots_mixed=FALSE
){
  # target column name decided in ONE PLACE
  y_col <- rlang::as_name(rlang::ensym(target_variable))   # e.g. d_eur -> "d_eur"
  
  cat("[orchestrator] MEAN-scan started…\n"); flush.console()
  mean_scan <- scan_level(
    data=data, y_col=y_col,
    train_start=train_start, test_start=test_start, test_end=test_end,
    regressors=regressors, role="mean",
    arima_order=arima_order,
    garch_model_grid=tibble::tibble(garch_model=NA_character_, sub_model=NA_character_),
    dist=dist, cond_thresh=cond_thresh, max_sec_each=t_mean_each,
    cache_dir=cache_scan_dir, cores=cores
  ) %>% dplyr::arrange(BIC)
  mean_top <- rank_by_ic(mean_scan, metric = "BIC", top = top_mean)
  cat(sprintf("[orchestrator] MEAN-scan done. Top=%d\n", nrow(mean_top))); flush.console()
  
  cat("[orchestrator] VAR-scan started (modelgrid)…\n"); flush.console()
  var_scan <- scan_level(
    data=data, y_col=y_col,
    train_start=train_start, test_start=test_start, test_end=test_end,
    regressors=regressors, role="var",
    arima_order=arima_order, garch_order=garch_order,
    garch_model_grid=garch_model_grid,
    dist=dist, cond_thresh=cond_thresh, max_sec_each=t_var_each,
    cache_dir=cache_scan_dir, cores=cores
  ) %>% dplyr::arrange(BIC)
  var_top  <- rank_by_ic(var_scan,  metric = "BIC", top = top_var)
  cat(sprintf("[orchestrator] VAR-scan done. Top=%d\n", nrow(var_top))); flush.console()
  
  mean_k <- min(nrow(mean_top), top_mean_for_mixed)
  var_k  <- min(nrow(var_top),  top_var_for_mixed)
  cat(sprintf("[orchestrator] MIXED started: %d×%d×%d (mean×var×models)\n",
              mean_k, var_k, nrow(garch_model_grid))); flush.console()
  
  mixed_res <- run_mixed_heavy(
    data=data, y_col=y_col,
    train_start=train_start, test_start=test_start, test_end=test_end,
    mean_top=mean_top[seq_len(mean_k), , drop=FALSE],
    var_top =var_top [seq_len(var_k),  , drop=FALSE],
    garch_model_grid=garch_model_grid,
    dist=dist, arima_order=arima_order[1:2], garch_order=garch_order,
    refit_every=10, refit_window="moving",
    max_pairs=max_pairs, cores=cores,
    cache_dir=cache_mixed_dir, plots=plots_mixed
  )
  mixed_res <- rank_by_ic(mixed_res, metric = "BIC")
  
  cat("[orchestrator] MIXED done.\n"); flush.console()
  list(mean_scan=mean_scan, var_scan=var_scan, mixed=mixed_res)
}

## ======= Example Call =======d
regressors <- c("FED","MNB","ECB","mean_matolcsy","mean_kinfo","mean_varga","mean_nagy",
                "d_czk","d_pln","d_ron","d_bux","d_cetop")


normalize_target(d_eur)          # "d_eur"

res03 <- run_full_pipeline(
  data = Training,
  target_variable = d_eur,   # bare is fine -> becomes "d_eur"
  regressors = regressors,
  train_start = as.Date("2018-06-03"),
  test_start  = as.Date("2024-06-13"),
  test_end    = as.Date("2025-02-14"),
  arima_order = c(2,0,0),
  garch_order = c(1,1),
  garch_model_grid = garch_model_grid,
  dist = "sstd",
  top_mean = 100, top_var = 100,
  top_mean_for_mixed = 100, top_var_for_mixed = 100,
  max_pairs = 10000,
  t_mean_each = 100, t_var_each = 100,
  cores = max(1, parallel::detectCores()-3),
  plots_mixed = FALSE
)

save.image(file = file.path(this.path::here(), "full_pipeline_output1.RData"))

head(res03$mean_scan, n = 10)
head(res03$var_scan, n = 10)

res03$mixed %>%
  filter(is.finite(AIC_perobs)) %>%
  arrange(Test_MAE) %>%
  slice_head(n = 30)

table(res03$mixed$mean)

sd_train <- sd(Training$d_eur[Training$Date >= train_start & Training$Date <  test_start], na.rm=TRUE)
sd_test  <- sd(Training$d_eur[Training$Date >= test_start  & Training$Date <= test_end],  na.rm=TRUE)
sd_train / sd_test # 1.544794

rmse_tr <- 0.00445
rmse_te <- 0.002806  

# Scale-normalized errors
rmse_tr_norm <- rmse_tr / sd_train
rmse_te_norm <- rmse_te / sd_test
c(train=rmse_tr_norm, test=rmse_te_norm)
# train  test 
# 0.8712 0.8486



## 6.12 MIXED Only ----
# Function for re-running (ARIMAX–GARCHX) combinations
#   - res$: mean_scan, var_scan (AND optionally mixed) already exist
#   - NO re-scanning of mean/var, only the mixing (generating_arima_garch)
#   - Avoids duplication: skips already evaluated pairs based on res$mixed
#   - Integrates with the cache the exact same way (mk_key, cache_get/put)

#' @title Run Mixed Combinations from Previous Results
#' @description Takes a previously generated results object containing mean and variance scans, and runs the ARIMAX-GARCHX combinations for the top K models. Intelligently skips combinations that have already been evaluated.
#' @param res The results list (containing mean_scan, var_scan, and optionally mixed).
#' @param data The time series dataframe.
#' @param target_variable The dependent variable column name.
#' @param train_start Start date of the training set.
#' @param test_start Start date of the testing set.
#' @param test_end End date of the testing set.
#' @param mean_top_k Number of top mean models to select.
#' @param var_top_k Number of top variance models to select.
#' @param mean_select Optional specific mean specifications to filter by.
#' @param var_select Optional specific variance specifications to filter by.
#' @param select_by Which Information Criterion to rank by ("BIC", "AIC", "HQ").
#' @param garch_model_grid Optional grid of GARCH models. If NULL, uses the var_scan's own GARCH/sub model.
#' @param dist Distribution model (default "sstd").
#' @param arima_order ARIMA (p,q) order.
#' @param garch_order GARCH (p,q) order.
#' @param refit_every Rolling forecast refit frequency.
#' @param refit_window Rolling forecast window type ("moving" or "expanding").
#' @param max_pairs Maximum number of combinations to run.
#' @param cores Number of CPU cores for parallel processing.
#' @param cache_dir Directory to save/load cached results.
#' @param plots Logical; if TRUE, generates plots.
#' @param skip_existing Logical; if TRUE, skips models already present in res$mixed.
#' @param verbose Logical; if TRUE, prints progress messages.
#' @return A tibble of the new mixed combination results.
run_mixed_from_res <- function(
    res, data, target_variable,
    train_start, test_start, test_end,
    mean_top_k = NULL, var_top_k = NULL,
    mean_select = NULL, var_select = NULL,
    select_by = c("BIC","AIC","HQ"),
    garch_model_grid = NULL,          # if NULL: var-row's own garch/sub model is used
    dist = "sstd",
    arima_order = c(2,0),             # rugarch: armaOrder = c(p,q)
    garch_order = c(1,1),
    refit_every = 10,
    refit_window = c("moving","expanding"),
    max_pairs = 40000,
    cores = max(1, parallel::detectCores()-1),
    cache_dir = "cache_mixed",
    plots = FALSE,
    skip_existing = TRUE,
    verbose = TRUE
){
  refit_window <- match.arg(refit_window)
  select_by    <- match.arg(select_by)
  or_else <- function(a,b) if (is.null(a) || (length(a)==1 && is.na(a))) b else a
  
  stopifnot(is.list(res), "mean_scan" %in% names(res), "var_scan" %in% names(res))
  ms <- res$mean_scan
  vs <- res$var_scan
  if (!all(c("vars","BIC") %in% names(ms))) stop("res$mean_scan is incomplete (requires: 'vars','BIC').")
  if (!all(c("vars","BIC","garch_model","sub_model") %in% names(vs))) {
    stop("res$var_scan is incomplete (requires: 'vars','BIC','garch_model','sub_model').")
  }
  
  # Target column
  y_col <- rlang::as_name(rlang::ensym(target_variable))
  if (!y_col %in% names(data)) stop("Missing target column in data: ", y_col)
  
  # Only OK + finite IC
  is_ok <- function(df){
    ok <- rep(TRUE, nrow(df))
    if ("status" %in% names(df)) ok <- ok & (df$status %in% c("OK","CACHE",""))
    if ("BIC" %in% names(df))    ok <- ok & is.finite(df$BIC)
    ok
  }
  ms <- ms[is_ok(ms), , drop = FALSE]
  vs <- vs[is_ok(vs), , drop = FALSE]
  
  # Sorting by chosen IC
  ic_col <- switch(select_by, BIC="BIC", AIC="AIC", HQ="HQ")
  if (!ic_col %in% names(ms)) stop("IC column missing in MS: ", ic_col)
  if (!ic_col %in% names(vs)) stop("IC column missing in VS: ", ic_col)
  ms <- ms[order(ms[[ic_col]]), , drop = FALSE]
  vs <- vs[order(vs[[ic_col]]), , drop = FALSE]
  
  # Explicit filtering / top_k
  if (!is.null(mean_select)) {
    ms <- ms[ms$vars %in% mean_select, , drop = FALSE]
    if (!nrow(ms)) stop("mean_select did not match anything in res$mean_scan.")
  }
  if (!is.null(var_select)) {
    vs <- vs[vs$vars %in% var_select, , drop = FALSE]
    if (!nrow(vs)) stop("var_select did not match anything in res$var_scan.")
  }
  if (!is.null(mean_top_k)) ms <- utils::head(ms, mean_top_k)
  if (!is.null(var_top_k))  vs <- utils::head(vs, var_top_k)
  
  # Task list creation
  if (is.null(garch_model_grid)) {
    var_picks <- unique(vs[, c("vars","garch_model","sub_model")])
    rownames(var_picks) <- NULL
    names(var_picks)[names(var_picks)=="vars"] <- "var"
    pairs <- tidyr::crossing(
      tibble::tibble(mean = ms$vars),
      var_picks
    )
  } else {
    if (!all(c("garch_model","sub_model") %in% names(garch_model_grid))) {
      stop("garch_model_grid requires: garch_model, sub_model.")
    }
    pairs <- tidyr::crossing(
      tibble::tibble(mean = ms$vars),
      tibble::tibble(var  = vs$vars),
      garch_model_grid
    )
  }
  if (!nrow(pairs)) stop("Nothing to run (empty pairs).")
  
  # Dropping already evaluated pairs
  if (skip_existing && "mixed" %in% names(res) && nrow(res$mixed)) {
    canon <- function(x) gsub("\\s+", "", gsub(",", "+", x), perl = TRUE)
    already <- paste(
      canon(res$mixed$mean),
      canon(res$mixed$var),
      or_else(res$mixed$garch_model, NA_character_),
      or_else(res$mixed$sub_model,   NA_character_),
      sep="|"
    )
    todo <- paste(
      canon(pairs$mean),
      canon(pairs$var),
      or_else(pairs$garch_model, NA_character_),
      or_else(pairs$sub_model,   NA_character_),
      sep="|"
    )
    keep <- !(todo %in% already)
    if (verbose) cat(sprintf("[mixed-from-res] Already completed skipped: %d / %d\n",
                             sum(!keep), length(keep)))
    pairs <- pairs[keep, , drop = FALSE]
  }
  
  # max_pairs restriction
  if (nrow(pairs) > max_pairs) {
    if (verbose) cat(sprintf("[mixed-from-res] Truncating pairs: %d -> %d\n", nrow(pairs), max_pairs))
    pairs <- utils::head(pairs, max_pairs)
  }
  if (!nrow(pairs)) {
    if (verbose) cat("[mixed-from-res] No new pairs to run (all done / skip_existing active).\n")
    return(tibble::tibble())
  }
  
  # Parallel execution
  oplan <- future::plan(); on.exit(future::plan(oplan), add = TRUE)
  future::plan(future::multisession, workers = cores)
  
  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(pairs))
    
    res_list <- future.apply::future_lapply(
      seq_len(nrow(pairs)),
      FUN = function(i){
        m  <- unlist(strsplit(pairs$mean[i], ",", fixed = TRUE))
        v  <- unlist(strsplit(pairs$var[i],  ",", fixed = TRUE))
        gm <- pairs$garch_model[[i]]
        sm <- pairs$sub_model[[i]]
        
        key <- mk_key("mixed",
                      vars = c(paste0("mean:", sort(m)), paste0("var:", sort(v))),
                      extra = list(ar = toString(arima_order),
                                   gar = toString(garch_order),
                                   gm = gm, sm = sm, d = dist,
                                   tr = as.character(train_start),
                                   te = as.character(test_end)))
        cs <- cache_get(cache_dir, key)
        if (!is.null(cs)) {
          if (verbose) cat(sprintf("[mixed] CACHE | mean={%s} var={%s} | %s/%s | BIC=%.6f\n",
                                   paste(m, collapse="+"), paste(v, collapse="+"),
                                   or_else(gm,"NA"), or_else(sm,"NA"),
                                   or_else(cs$BIC_perobs, NA_real_)))
          p(); return(cs)
        }
        
        if (verbose) cat(sprintf("[mixed] START | mean={%s} | var={%s} | model=%s/%s\n",
                                 paste(m, collapse="+"), paste(v, collapse="+"),
                                 or_else(gm,"NA"), or_else(sm,"NA"))); flush.console()
        
        out <- tryCatch({
          r <- generating_arima_garch(
            data               = data,
            target_variable    = y_col,
            y_col              = y_col,
            train_start        = train_start,
            test_start         = test_start,
            test_end           = test_end,
            arima_order        = arima_order,
            arima_regressors   = if (length(m)) m else NULL,
            garch_model        = gm,
            sub_model          = sm,
            garch_order        = garch_order,
            garch_regressors   = if (length(v)) v else NULL,
            distribution_model = dist,
            VAR_alpha          = 0.05,
            refit_every        = refit_every,
            refit_window       = refit_window,
            plots              = plots,
            out_prefix         = sprintf("MIX_%s_%s", or_else(gm,"NA"), or_else(sm,"NA"))
          )
          ic  <- r$tables$info_criteria
          err <- r$tables$errors
          
          tab <- tibble::tibble(
            mean = paste(m, collapse="+"),
            var  = paste(v, collapse="+"),
            garch_model = gm,
            sub_model   = sm,
            AIC_perobs  = ic$PerObs[ic$Metric=="AIC"],
            BIC_perobs  = ic$PerObs[ic$Metric=="BIC"],
            HQ_perobs   = ic$PerObs[ic$Metric=="HQ"],
            Train_RMSE  = err$RMSE[err$Set=="Train"],
            Test_RMSE   = err$RMSE[grepl("Test", err$Set)],
            Train_MAE   = err$MAE[err$Set=="Train"],
            Test_MAE    = err$MAE[grepl("Test", err$Set)],
            Train_TIC   = err$TIC[err$Set=="Train"],
            Test_TIC    = err$TIC[grepl("Test", err$Set)],
            status      = "OK"
          )
          
          if (verbose) cat(sprintf("[mixed] DONE  | %s/%s | BIC=%.6f | TestRMSE=%.6f\n",
                                   or_else(gm,"NA"), or_else(sm,"NA"),
                                   tab$BIC_perobs, tab$Test_RMSE))
          cache_put(cache_dir, key, tab)
          tab
        }, error = function(e){
          if (verbose) cat(sprintf("[mixed] ERROR | mean={%s} | var={%s} | %s/%s | %s\n",
                                   paste(m, collapse="+"), paste(v, collapse="+"),
                                   or_else(gm,"NA"), or_else(sm,"NA"), conditionMessage(e)))
          tibble::tibble(
            mean = paste(m, collapse="+"), var = paste(v, collapse="+"),
            garch_model = gm, sub_model = sm,
            AIC_perobs = NA_real_, BIC_perobs = NA_real_, HQ_perobs = NA_real_,
            Train_RMSE = NA_real_, Test_RMSE  = NA_real_,
            Train_MAE  = NA_real_, Test_MAE   = NA_real_,
            Train_TIC  = NA_real_, Test_TIC   = NA_real_,
            status     = paste("ERROR:", conditionMessage(e))
          )
        })
        p(); out
      },
      future.seed = TRUE,
      future.packages = c("rlang","digest","R.utils","forecast","rugarch",
                          "data.table","tibble","tidyr","dplyr","ggplot2")
    )
    data.table::rbindlist(res_list, fill = TRUE)
  })
}


mixed_more <- run_mixed_from_res(
  res             = res03,
  data            = Training,
  target_variable = d_eur,
  train_start     = as.Date("2018-06-03"),
  test_start      = as.Date("2024-06-13"),
  test_end        = as.Date("2025-02-14"),
  mean_top_k      = 100,
  var_top_k       = 100,
  garch_model_grid = NULL,        # <- var-row's own GARCH/sub model
  arima_order     = c(2,0),
  garch_order     = c(1,1),
  dist            = "sstd",
  refit_every     = 10,
  refit_window    = "moving",
  max_pairs       = 10000,        # 100 × 100
  cores           = max(1, parallel::detectCores()-3),
  cache_dir       = "cache_mixed",
  plots           = FALSE,
  skip_existing   = TRUE,
  verbose         = TRUE
)

save.image(file = file.path(this.path::here(), "tdk03_full_pipeline_output11.09.RData"))

## ============================================================ -
# 7. Post-Processing and Results Extraction ----
## ============================================================ -

#' @title Format Numbers for English
#' @description Formats numeric values to a specific number of decimal places using standard English notation (dot for decimal, space for thousands).
format_en <- function(x, d=6) formatC(x, format="f", digits=d, decimal.mark=".", big.mark=" ")

#' @title Split Variables
#' @description Splits a string of variables separated by "+" into a character vector. Returns an empty vector if NA or empty.
split_vars <- function(s) {
  ifelse(is.na(s) | s=="", list(character(0)), strsplit(s, "\\+", fixed = FALSE))
}

#' @title Check Variable Presence
#' @description Checks if a specific variable name exists exactly within a "+" separated string.
has_var <- function(x, v) {
  # Exact match + (start|end|middle '+')
  grepl(paste0("(^|\\+)", v, "(\\+|$)"), x)
}

# Base Extracts
mean_scan <- res03$mean_scan %>% arrange(BIC)  # Here BIC is per-obs from the scan
var_scan  <- res03$var_scan  %>% arrange(BIC)
mm        <- mixed_more %>% filter(is.finite(BIC_perobs)) %>% arrange(BIC_perobs, Test_RMSE)

# "Champion" model quick extract
champ <- mm %>% slice(1)
champ
cat(sprintf(
  "\nCHAMPION: mean={%s} | var={%s} | %s/%s | BIC/obs=%.6f | Test RMSE=%.6f | Test MAE=%.6f | Test TIC=%.6f\n",
  champ$mean, champ$var, champ$garch_model, champ$sub_model %||% "NA",
  champ$BIC_perobs, champ$Test_RMSE, champ$Test_MAE, champ$Test_TIC
))

# Top 15 mixed models LaTeX table
top15 <- mm %>%
  transmute(
    Mean = mean, Var = var, Mod = paste0(garch_model, ifelse(is.na(sub_model),"", paste0("–", sub_model))),
    `BIC/obs` = format_en(BIC_perobs, 6),
    `RMSE`    = format_en(Test_RMSE, 6),
    `MAE`     = format_en(Test_MAE, 6),
    `TIC`     = format_en(Test_TIC, 6)
  ) %>% slice_head(n = 15)

kbl(top15, align="l", booktabs = TRUE,
    caption = "Top 15 mixed ARIMAX–GARCHX models (by RMSE per observation).") |>
  kable_styling(full_width = FALSE, position = "center") |>
  footnote(general = "Source: Authors' calculations.")

# Variable frequency in the top K mixed models
K <- 100
mmK <- mm %>% slice_head(n = K)

#' @title Count Variables
#' @description Counts the frequency of individual variables within a vector of "+" separated strings.
count_vars <- function(strings) {
  s <- as.character(strings)
  s <- s[!is.na(s)]
  tokens <- unlist(strsplit(s, "\\+"), use.names = FALSE)
  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens)) return(tibble::tibble(Var = character(0), Freq = integer(0)))
  tab <- sort(table(tokens), decreasing = TRUE)
  tibble::tibble(Var = names(tab), Freq = as.integer(tab))
}

# Usage
freq_mean <- count_vars(mmK$mean) %>% dplyr::mutate(Role = "Mean")
freq_var  <- count_vars(mmK$var)  %>% dplyr::mutate(Role = "Var")
freq_all  <- dplyr::bind_rows(freq_mean, freq_var)

kbl(freq_all, align="c", booktabs=TRUE,
    caption = sprintf("Occurrence of variables in the top %d mixed models (by mean/var breakdown).", K)) |>
  kable_styling(full_width = FALSE, position = "center")

# "Marginal" effect estimation: is the variable included?
vars_of_interest <- c("FED","MNB","ECB",
                      "mean_matolcsy","mean_kinfo","mean_varga","mean_nagy",
                      "d_czk","d_pln","d_ron","d_bux","d_cetop")

#' @title Evaluate Variable Presence
#' @description Calculates the average difference in performance metrics (BIC, RMSE, MAE, TIC) for models that include a specific variable vs. models that exclude it.
presence_eval <- function(df, v, where = c("mean","var")) {
  where <- match.arg(where)
  idx <- has_var(df[[where]], v)
  tibble(
    var = v, role = where,
    n_in  = sum(idx),   n_out = sum(!idx),
    bic_in  = mean(df$BIC_perobs[idx],  na.rm=TRUE),
    bic_out = mean(df$BIC_perobs[!idx], na.rm=TRUE),
    d_bic   = bic_in - bic_out,      # <0 → improves on average
    rmse_in  = mean(df$Test_RMSE[idx],  na.rm=TRUE),
    rmse_out = mean(df$Test_RMSE[!idx], na.rm=TRUE),
    d_rmse   = rmse_in - rmse_out,    # <0 → improves on average
    mae_in  = mean(df$Test_MAE[idx],  na.rm=TRUE),
    mae_out = mean(df$Test_MAE[!idx], na.rm=TRUE),
    d_mae   = mae_in - mae_out,
    tic_in  = mean(df$Test_TIC[idx],  na.rm=TRUE),
    tic_out = mean(df$Test_TIC[!idx], na.rm=TRUE),
    d_tic   = tic_in - tic_out
  )
}

marg_tbl <- bind_rows(
  lapply(vars_of_interest, presence_eval, df = mmK, where = "mean"),
  lapply(vars_of_interest, presence_eval, df = mmK, where = "var")
) %>% arrange(d_bic, d_rmse)

marg_latex <- marg_tbl %>%
  transmute(
    Role = ifelse(role=="mean","Mean","Var"), Variable = var,
    `ΔBIC/obs` = format_en(d_bic, 6),
    `ΔRMSE`    = format_en(d_rmse, 6),
    `ΔMAE`     = format_en(d_mae, 6),
    `ΔTIC`     = format_en(d_tic, 6),
    `n_in` = n_in, `n_out` = n_out
  )

kbl(marg_latex, align="c", booktabs=TRUE,
    caption = sprintf("Average marginal effect of variables on performance metrics in the top %d (presence vs. absence).", K)) |>
  kable_styling(full_width = FALSE, position = "center") |>
  footnote(general = "Negative Δ value: the presence of the variable improves the metric on average.")

# Diagnostics: number of regressors → performance?
mm %>%
  mutate(k_mean = lengths(split_vars(mean)),
         k_var  = lengths(split_vars(var))) %>%
  group_by(k_mean, k_var) %>%
  summarise(BIC = mean(BIC_perobs, na.rm=TRUE),
            RMSE = mean(Test_RMSE, na.rm=TRUE),
            .groups="drop") -> grid_perf


# Refitting the champion model for detailed output (if needed)
# Use this to retrieve the coefficient table, σ_t curve, VaR, etc.
best_fit <- generating_arima_garch(
  data               = Training,
  target_variable    = d_eur,
  train_start        = as.Date("2018-06-03"),
  test_start         = as.Date("2024-06-13"),
  test_end           = as.Date("2025-02-14"),
  arima_order        = c(2,0),                 
  arima_regressors   = strsplit(champ$mean, "\\+")[[1]],
  garch_model        = champ$garch_model,
  sub_model          = if (is.na(champ$sub_model)) NULL else as.character(champ$sub_model),
  garch_order        = c(1,1),
  garch_regressors   = strsplit(champ$var, "\\+")[[1]],
  distribution_model = "sstd",
  VAR_alpha          = 0.05,
  refit_every        = 10,
  
  refit_window       = "moving",
  plots              = TRUE,
  out_prefix         = "champion"
)
best_fit$tables$coefs_train 
best_fit$plots


## 7.1 BAR CHART – Frequency based on marg_latex ----
freq_df <- marg_latex %>%
  transmute(
    Variable = as.character(`Variable`),
    Role = factor(`Role`), 
    Count = as.numeric(`n_in`)
  )

levels(freq_df$Role) <- c("Mean" = "Mean", "Var" = "Variance") 

# Name rewriting (BEFORE factoring)
if (exists("name_map")) {
  freq_df$Variable <- dplyr::recode(freq_df$Variable, !!!name_map)
}

# Updating sorting with rewritten names
ord <- freq_df %>% 
  group_by(Variable) %>% 
  summarise(Total = sum(Count, na.rm = TRUE), .groups="drop") %>%
  arrange(desc(Total)) %>% 
  pull(Variable)

freq_df$Variable <- factor(freq_df$Variable, levels = ord)

# Plot 
p_freq <- ggplot(freq_df, aes(x = Variable, y = Count, fill = Role)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("Mean" = primary_col, "Variance" = secondary_col),
    name = NULL,
    na.value = "grey50" # If NA remains, at least you see it
  ) +
  labs(
    subtitle = "Division by role (Mean - Variance)",
    x = "Variable",
    y = "Frequency of occurrences"
  ) +
  theme_regina()

export_png(p_freq, "variable_frequency_mean_vs_var.png", width = 1200, height = 700)
export_svg(p_freq, "variable_frequency_mean_vs_var.svg", width = 1200, height = 700)

## BOXPLOTS – Marginal effects in Top 100 models
if (!exists("mmK")) {
  stopifnot(exists("mixed_more"))
  mmK <- mixed_more %>% arrange(BIC_perobs) %>% slice_head(n = 100)
}

# helper: "is the variable included in the given role?"
has_var <- function(x, v) grepl(paste0("(^|\\+)", v, "(\\+|$)"), x)

vars_of_interest <- unique(freq_df$Variable)

mk_delta_long <- function(df, v, role = c("mean","var")) {
  role <- match.arg(role)
  idx_in  <- has_var(df[[role]], v)
  idx_out <- !idx_in
  if (!any(idx_in) || !any(idx_out)) return(NULL)  # if no baseline for comparison
  
  # "average out" references
  ref <- list(
    BIC  = mean(df$BIC_perobs [idx_out], na.rm = TRUE),
    RMSE = mean(df$Test_RMSE  [idx_out], na.rm = TRUE),
    MAE  = mean(df$Test_MAE   [idx_out], na.rm = TRUE),
    TIC  = mean(df$Test_TIC   [idx_out], na.rm = TRUE)
  )
  tibble(
    Variable = v,
    Role     = ifelse(role=="mean","Mean","Var"),
    `ΔBIC/obs` = df$BIC_perobs [idx_in] - ref$BIC,
    `ΔRMSE`    = df$Test_RMSE  [idx_in] - ref$RMSE,
    `ΔMAE`     = df$Test_MAE   [idx_in] - ref$MAE,
    `ΔTIC`     = df$Test_TIC   [idx_in] - ref$TIC
  )
}

# generate full sample
deltas_list <- list()
for (v in vars_of_interest) {
  deltas_list[[paste0(v,"-mean")]] <- mk_delta_long(mmK, v, "mean")
  deltas_list[[paste0(v,"-var")]]  <- mk_delta_long(mmK, v, "var")
}
deltas <- bind_rows(deltas_list) %>%
  pivot_longer(cols = c(`ΔBIC/obs`, `ΔRMSE`, `ΔMAE`, `ΔTIC`),
               names_to = "Metric", values_to = "Delta") %>%
  filter(is.finite(Delta))

# sort by median ΔBIC (improvement: negative)
ord_bic <- deltas %>%
  filter(Metric == "ΔBIC/obs") %>%
  group_by(Variable) %>%
  summarise(med = median(Delta, na.rm = TRUE), .groups="drop") %>%
  arrange(med) %>% pull(Variable)
deltas$Variable <- factor(deltas$Variable, levels = ord_bic)

p_box <- ggplot(deltas, aes(x = Variable, y = Delta, fill = Role)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.alpha = 0.25, width = 0.65, position = position_dodge(width = 0.8)) +
  facet_wrap(~ Metric, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = c("Mean" = primary_col, "Var" = secondary_col), name = NULL) +
  labs(title = "Marginal effect of variables in the top 100 models",
       subtitle = "Difference from the other models lacking the given variable",
       x = "Variable", y = "Δ (current model - missing mean)",
       caption = "Negative values suggest a better fit on average. Source: Authors' own calculations.") +
  theme_regina() +
  theme(strip.text = element_text(face = "bold"))

export_png(p_box, "variables_marginal_effects_box_rmse.png", width = 1600, height = 1000)
export_svg(p_box, "variables_marginal_effects_box_rmse.svg", width = 1600, height = 1000)


res03$var_scan %>% 
  arrange(BIC) %>%
  head(n = 500) %>%
  group_by(garch_model, sub_model) %>%
  summarise(n_models = n(), .groups="drop") %>%
  arrange(desc(n_models)) %>%
  kbl(align="c", booktabs=TRUE,
      caption = "Frequency of GARCH models and submodels in the top 500 variance models.") |>
  kable_styling(full_width = FALSE, position = "center")

## ============================================================ -
# 8. Top K models ----
## ============================================================ -

# Saving coefficients into a per-model list
# (Estimate, SE, t, p, star, role, variable name, standardized effect)
K_top        <- 100
y_col        <- "d_eur"
train_start  <- as.Date("2018-06-03")
test_start   <- as.Date("2024-06-13")
test_end     <- as.Date("2025-02-14")

## ---- Top K + IC-weight
mmK <- mixed_more %>%
  filter(is.finite(BIC_perobs)) %>%
  arrange(BIC_perobs) %>%
  slice_head(n = K_top) %>%
  mutate(
    dBIC  = BIC_perobs - min(BIC_perobs, na.rm = TRUE),
    w_raw = exp(-0.5 * dBIC),
    w_ic  = w_raw / sum(w_raw, na.rm = TRUE)
  )

`%+%` <- function(a,b) paste0(a,b)

## ---- Helper Functions
#' @title Standardize Beta Coefficient
#' @description Calculates the standardized beta coefficient to compare the relative strength of regressors.
std_beta <- function(beta, x, y) {
  n <- min(length(x), length(y)); x <- x[seq_len(n)]; y <- y[seq_len(n)]
  sx <- stats::sd(x, na.rm = TRUE); sy <- stats::sd(y, na.rm = TRUE)
  if (!is.finite(sx) || !is.finite(sy) || sy == 0) return(NA_real_)
  beta * (sx / sy)
}


#' @title Classify Parameters
#' @description Maps the generic parameter names outputted by rugarch (e.g., 'mxreg1', 'vxreg2', 'alpha1') back to their structural equation and actual variable names.
classify_param <- function(garch_model, pname, mean_vars, var_vars) {
  # default
  role <- "Other"; eq <- "Other"; varname <- NA_character_
  p <- tolower(pname)
  if (grepl("^mxreg\\d+$", p)) {
    idx <- as.integer(sub("^mxreg", "", p))
    role <- "Mean"; eq <- "Mean"; varname <- if (idx %in% seq_along(mean_vars)) mean_vars[idx] else NA_character_
  } else if (grepl("^vxreg\\d+$", p)) {
    idx <- as.integer(sub("^vxreg", "", p))
    role <- "Var";  eq <- "Variance"; varname <- if (idx %in% seq_along(var_vars)) var_vars[idx] else NA_character_
  } else if (grepl("^ar\\d+$", p)) {
    role <- "AR"; eq <- "Mean"
  } else if (grepl("^ma\\d+$", p)) {
    role <- "MA"; eq <- "Mean"
  } else if (p == "mu") {
    role <- "Intercept"; eq <- "Mean"
  } else if (p == "omega") {
    role <- "Omega"; eq <- "Variance"
  } else if (grepl("^alpha\\d+$", p)) {
    role <- "ARCH"; eq <- "Variance"
  } else if (grepl("^beta\\d+$", p)) {
    role <- "GARCH"; eq <- "Variance"
  } else if (grepl("^gamma\\d+$", p)) {
    role <- if (tolower(garch_model) %in% c("egarch","gjrgarch","tgarch")) "Leverage/Asym" else "Gamma"
    eq   <- "Variance"
  } else if (p %in% c("shape","skew","eta","delta","nu")) {
    role <- "Distribution"; eq <- "Distribution"
  }
  list(role = role, eq = eq, varname = varname)
}


#' @title Extract Coefficient Table (Single Model)
#' @description Refits a specific ARIMAX-GARCHX combination to extract robust standard errors and standardized parameter effects.
extract_coef_table_one <- function(row) {
  mean_vars <- if (is.na(row$mean) || row$mean == "") character(0) else strsplit(row$mean, "\\+")[[1]]
  var_vars  <- if (is.na(row$var)  || row$var  == "") character(0) else strsplit(row$var,  "\\+")[[1]]
  gmod      <- row$garch_model
  smod      <- if (is.na(row$sub_model)) NULL else as.character(row$sub_model)
  
  fit <- tryCatch(
    generating_arima_garch(
      data               = Training,
      target_variable    = d_eur,
      train_start        = train_start,
      test_start         = test_start,
      test_end           = test_end,
      arima_order        = c(2,0),
      arima_regressors   = if (length(mean_vars)) mean_vars else NULL,
      garch_model        = gmod,
      sub_model          = smod,
      garch_order        = c(1,1),
      garch_regressors   = if (length(var_vars))  var_vars  else NULL,
      distribution_model = "sstd",
      VAR_alpha          = 0.05,
      refit_every        = 10,
      refit_window       = "moving",
      plots              = FALSE,
      out_prefix         = "coefscan"
    ),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  
  fit_train <- fit$objects$fit_train
  
  # Robust or classic coefficient table
  Krob <- tryCatch(as.data.frame(fit_train@fit$robust.matcoef), error = function(e) NULL)
  Kcls <- tryCatch(as.data.frame(fit_train@fit$matcoef),        error = function(e) NULL)
  Ktab_src <- if (!is.null(Krob)) Krob else Kcls
  if (is.null(Ktab_src) || nrow(Ktab_src) == 0) return(NULL)
  
  Ktab <- Ktab_src
  colnames(Ktab) <- c("Estimate","StdErr","t","p")
  Ktab$Param <- rownames(Ktab)
  
  # Train slice for standardization
  idx_tr <- Training$Date >= train_start & Training$Date < test_start
  y_tr   <- Training[[y_col]][idx_tr]
  
  # σ_t on train (for scaling var-effects outside of the eGARCH family)
  sigma_tr <- tryCatch(as.numeric(rugarch::sigma(fit_train)), error = function(e) NA_real_)
  s2bar    <- if (all(is.finite(sigma_tr))) mean(sigma_tr^2, na.rm = TRUE) else NA_real_
  is_eg    <- identical(tolower(gmod), "egarch")
  
  # Parameter annotation + standardized effect
  mapped <- purrr::pmap_dfr(
    list(Ktab$Param, Ktab$Estimate, Ktab$StdErr, Ktab$t, Ktab$p),
    function(param, est, se, tval, pval) {
      info <- classify_param(gmod, param, mean_vars, var_vars)
      effe <- NA_real_
      if (info$role == "Mean" && isTRUE(nzchar(info$varname))) {
        x_tr <- Training[[info$varname]][idx_tr]
        effe <- std_beta(est, x_tr, y_tr)
      } else if (info$role == "Var" && isTRUE(nzchar(info$varname))) {
        effe <- if (is_eg) est else if (is.finite(s2bar) && s2bar > 0) est / s2bar else NA_real_
      }
      tibble(
        Param     = param,
        Role      = info$role,
        Equation  = info$eq,
        Variable  = info$varname,
        Estimate  = est,
        StdErr    = se,
        t         = tval,
        p         = pval,
        Stars     = sig_codes(pval),
        Effect    = effe
      )
    }
  )
  
  model_id <- row$mean %+% " | " %+% row$var %+% " | " %+% row$garch_model %+%
    ifelse(is.null(smod),"", paste0("-", smod))
  
  mapped %>%
    mutate(
      ModelID     = model_id,
      Mean_spec   = row$mean,
      Var_spec    = row$var,
      GARCH_model = row$garch_model,
      Sub_model   = if (is.null(smod)) NA_character_ else smod,
      BIC_perobs  = row$BIC_perobs,
      Test_RMSE   = row$Test_RMSE,
      Test_MAE    = row$Test_MAE,
      Test_TIC    = row$Test_TIC,
      IC_weight   = row$w_ic
    ) %>%
    select(ModelID, Mean_spec, Var_spec, GARCH_model, Sub_model,
           BIC_perobs, Test_RMSE, Test_MAE, Test_TIC, IC_weight,
           Param, Role, Equation, Variable, Estimate, StdErr, t, p, Stars, Effect)
}

## Run for every selected model
coef_tbls <- purrr::pmap(
  mmK,
  function(...) extract_coef_table_one(tibble::as_tibble_row(list(...)))
)

# Drop failed/NULL models
valid_idx <- which(purrr::map_lgl(coef_tbls, ~ !is.null(.x) && nrow(.x) > 0))
coef_tbls <- coef_tbls[valid_idx]

# Model IDs as names
model_ids <- purrr::map_chr(coef_tbls, ~ unique(.x$ModelID)[1])
names(coef_tbls) <- model_ids

# LIST: sub-table per model (Estimate, SE, t, p, star, Effect, meta)
model_coefs_list <- coef_tbls

# LONG: concatenated table
model_coefs_long <- dplyr::bind_rows(model_coefs_list, .id = "ModelID_check")

## Quick Save to disk
saveRDS(model_coefs_list, file = "model_coefs_list_topK.rds")
readr::write_csv(model_coefs_long, file = "model_coefs_long_topK.csv")

cat(sprintf("Completed: Coefficients for %d models saved.\n", length(model_coefs_list)))
cat("• model_coefs_list_topK.rds  (list, sub-tables per model)\n")
cat("• model_coefs_long_topK.csv  (concatenated long table)\n")


## ============================================================ -
## 9 Cleaning: Only relevant coefficients ----
## ============================================================ -

# Param, Role (Mean/Var), Equation (Mean/Var), Variable, Estimate, StdErr, p, Stars, etc.

coef_clean <- model_coefs_long %>%
  filter(Equation %in% c("Mean","Var"),
         !is.na(Variable),
         is.finite(Estimate))

# Sorting order: by median (absolute)
ord <- coef_clean %>%
  group_by(Variable) %>%
  summarise(med_abs = median(abs(Estimate), na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(med_abs)) %>% pull(Variable)

coef_clean$Variable <- factor(coef_clean$Variable, levels = ord)


# Joint boxplot (Mean and Var together, dodged) 
p_box_both <- ggplot(coef_clean, aes(x = Variable, y = Estimate, fill = Role)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.alpha = 0.25, width = 0.65,
               position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("Mean" = primary_col, "Var" = secondary_col), name = NULL) +
  labs(title = "Distribution of Coefficients",
       subtitle = "Unweighted, broken down by role (Mean vs Var)",
       x = "Variable", y = "Estimated coefficient",
       caption = "Source: Authors' own calculations, model_coefs_list") +
  theme_regina() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

export_png(p_box_both, "coefficients_boxplot_mean_var_dodge.png", width = 1600, height = 1000)
export_svg(p_box_both, "coefficients_boxplot_mean_var_dodge.svg", width = 1600, height = 1000)

# Faceted version (separate panel for Mean/Var) 
p_box_facet <- ggplot(coef_clean, aes(x = Variable, y = Estimate, fill = Role)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.alpha = 0.25, width = 0.65) +
  facet_wrap(~ Role, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Mean" = primary_col, "Var" = secondary_col), guide = "none") +
  labs(title = "Distribution of Coefficients by Role",
       subtitle = "Unweighted (Faceted display)",
       x = "Variable", y = "Estimated coefficient",
       caption = "Source: Authors' own calculations, model_coefs_list") +
  theme_regina() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"))

export_png(p_box_facet, "coefficients_boxplot_mean_var_facet.png", width = 1600, height = 1200)
export_svg(p_box_facet, "coefficients_boxplot_mean_var_facet.svg", width = 1600, height = 1200)


head(model_coefs_long)


#' @title Map Parameter to Human-Readable Term
#' @description Maps raw parameter names (e.g., 'ar1', 'alpha1') to cleaner labels (e.g., 'AR(1)', 'α1').
map_param_to_term <- function(param, variable) {
  # If there is a specific Variable, use it (renamed if name_map exists)
  if (!is.na(variable)) {
   
    if (exists("name_map")) {
      out <- dplyr::recode(variable, !!!name_map, .default = variable)
      return(out)
    } else {
      return(variable)
    }
  }
  # Otherwise, name based on Param
  if (is.na(param)) return(NA_character_)
  if (param %in% c("mu","intercept")) return("(Intercept)")
  if (str_detect(param, "^ar\\d+$"))  return(paste0("AR(", str_remove(param, "^ar"), ")"))
  if (str_detect(param, "^ma\\d+$"))  return(paste0("MA(", str_remove(param, "^ma"), ")"))
  # GARCH base parameters – Greek letters
  if (str_detect(param, "^alpha\\d*$")) return(str_replace(param, "^alpha", "α"))
  if (str_detect(param, "^beta\\d*$"))  return(str_replace(param, "^beta",  "β"))
  if (str_detect(param, "^gamma\\d*$")) return(str_replace(param, "^gamma", "γ"))
  if (param == "omega") return("ω")
  # Otherwise, keep as is
  param
}

# Unified "Term" column (which is either the short Variable or the human name of Param)
coefs_clean <- model_coefs_long %>%
  mutate(
    Term = mapply(map_param_to_term, Param, Variable),
    Equation = ifelse(is.na(Equation), Role, Equation)
  ) %>%
  # only actual coefficients
  filter(!is.na(Estimate), !is.na(Term), !is.na(Equation))

### 99% z critical value 
alpha <- 0.01
z_crit <- qnorm(1 - alpha/2)  # 0.995 quantile

### Unweighted (simple) average and 99% CI by Term×Equation 
coef_summary_unweighted <- coefs_clean %>%
  group_by(Equation, Term) %>%
  summarise(
    n         = n(),
    mean_est  = mean(Estimate, na.rm = TRUE),
    sd_est    = sd(Estimate, na.rm = TRUE),
    se_mean   = sd_est / sqrt(n),
    ci_lower  = mean_est - z_crit * se_mean,
    ci_upper  = mean_est + z_crit * se_mean,
    .groups   = "drop"
  ) %>%
  mutate(sig_99 = ifelse(ci_lower > 0 | ci_upper < 0, TRUE, FALSE)) %>%
  arrange(Equation, Term)

coef_ci_99 <- coef_summary_unweighted %>%
  # Sorting: Mean/Var first, then Intercept/AR/... upfront, then alphabetical
  mutate(
    eq_order  = ifelse(Equation %in% c("Mean","mean"), 1L, 2L),
    term_rank = dplyr::case_when(
      Term == "(Intercept)" ~ 1L,
      str_detect(Term, "^AR\\(") ~ 2L,
      str_detect(Term, "^MA\\(") ~ 3L,
      TRUE ~ 4L
    )
  ) %>%
  arrange(eq_order, term_rank, Term) %>%
  select(-eq_order, -term_rank)

print(head(coef_ci_99, 40))


#' @title Normalize Equation Names
#' @description Standardizes various string representations of model equations into two clean categories: "Mean" or "Var". 
#' If a string doesn't match the known aliases, it leaves the original string intact.
#' @param x A character vector of equation names (e.g., "mu", "variance eq").
#' @return A standardized character vector.
normalize_equation <- function(x) {
  lx <- tolower(trimws(x))
  
  # Map known aliases to "Mean" and "Var"
  out <- ifelse(lx %in% c("mean","mu"), "Mean",
                ifelse(lx %in% c("var","variance","vol","volatility","garch","sigma","variance eq"),
                       "Var", NA_character_))
  ifelse(is.na(out), x, out)
}

#' @title Prepare Dataframe for Plotting
#' @description Cleans and formats a coefficient summary dataframe for use in ggplot2. It normalizes equation names, creates formatted axis labels showing sample size, and generates a clean legend category for significance.
#' @param df A dataframe containing 'Equation', 'Term', 'n', and 'sig_99' columns.
#' @return A mutated dataframe ready for visualization.
prep_df <- function(df) {
  df %>%
    mutate(
      Eq_norm = normalize_equation(Equation),
      Term_label   = sprintf("%s (n=%d)", Term, n),
      Significance = ifelse(sig_99, "Significant (99%)", "Insignificant")
    )
}

#' @title Plot Coefficient Panel
#' @description Generates a forest plot of the coefficients with confidence intervals for either the Mean or Variance equation.
plot_coef_panel <- function(df, eq_label,
                            file,                 # path with OR without extension
                            out = c("svg","png"), # if no extension in 'file'
                            width = 12, height = 10, dpi = 300,
                            only_exogen_mean = FALSE) {
  
  out <- match.arg(out)
  
  # Extension handling
  ext <- tolower(tools::file_ext(file))
  if (nzchar(ext)) {
    if (!ext %in% c("svg","png")) stop("Only .svg or .png allowed.")
    out <- ext
    file_out <- file
  } else {
    file_out <- paste0(file, ".", out)
  }
  
  # Ensure target directory
  dir.create(dirname(file_out), recursive = TRUE, showWarnings = FALSE)
  
  dd <- prep_df(df) %>% dplyr::filter(tolower(Eq_norm) == tolower(eq_label))
  if (nrow(dd) == 0) {
    avail <- df %>% mutate(Eq_norm = normalize_equation(Equation)) %>%
      distinct(Eq_norm) %>% pull(Eq_norm) %>% sort() %>% paste(collapse = ", ")
    stop("No data (Equation = ", eq_label, "). Available: ", avail)
  }
  
  if (tolower(eq_label) == "mean" && isTRUE(only_exogen_mean)) {
    dd <- dd %>%
      dplyr::filter(!(Term %in% c("(Intercept)")),
                    !stringr::str_detect(Term, "^(AR|MA)\\("))
    if (nrow(dd) == 0) stop("No rows left after Mean-exogenous filtering.")
  }
  
  dd <- dd %>%
    mutate(Term_label = factor(Term_label,
                               levels = Term_label[order(abs(mean_est), decreasing = TRUE)]))
  
  p <- ggplot(dd, aes(x = Term_label, y = mean_est, colour = Significance)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, colour = "grey40") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0, linewidth = 0.9, alpha = 0.95) +
    geom_point(size = 2.6) +
    coord_flip() +
    labs(
      title = ifelse(tolower(eq_label)=="mean","Mean equation","Variance Equation"),
      x = "Variable (sample size in brackets)",
      y = "Coefficient rate",
      caption = "Source: Authors' own calculations."
    ) +
    scale_colour_manual(values = c("Significant (99%)" = "#2ca02c",
                                   "Insignificant"   = "#d62728"),
                        name = NULL) +
    theme_regina() +
    theme(legend.position = "bottom", legend.title = element_blank())
  
# Save: Cairo -> svglite -> ggsave fallback
  if (out == "svg") {
    ok <- FALSE
    if (requireNamespace("Cairo", quietly = TRUE)) {
      res <- try({
        Cairo::CairoSVG(file_out, width = width, height = height)
        print(p); grDevices::dev.off()
      }, silent = TRUE)
      if (!inherits(res, "try-error")) ok <- TRUE else message("CairoSVG not available, starting fallback...")
    }
    if (!ok && requireNamespace("svglite", quietly = TRUE)) {
      res <- try({
        svglite::svglite(file_out, width = width, height = height)
        print(p); grDevices::dev.off()
      }, silent = TRUE)
      if (!inherits(res, "try-error")) ok <- TRUE else message("svglite also failed, falling back to ggsave…")
    }
    if (!ok) {
      ggplot2::ggsave(filename = file_out, plot = p, device = "svg",
                      width = width, height = height, dpi = dpi, bg = "white")
    }
  } else { # png
    ok <- FALSE
    px_w <- round(width  * dpi)
    px_h <- round(height * dpi)
    if (requireNamespace("Cairo", quietly = TRUE)) {
      res <- try({
        Cairo::CairoPNG(file_out, width = px_w, height = px_h, bg = "white")
        print(p); grDevices::dev.off()
      }, silent = TRUE)
      if (!inherits(res, "try-error")) ok <- TRUE else message("CairoPNG not available, falling back to ggsave…")
    }
    if (!ok) {
      ggplot2::ggsave(filename = file_out, plot = p, device = "png",
                      width = width, height = height, dpi = dpi, bg = "white")
    }
  }
  
  message("Saved: ", normalizePath(file_out, mustWork = FALSE))
  invisible(p)
}


# Separate images
plot_coef_panel(coef_ci_99, "Mean", "Plots/coef_mean", out = "svg", only_exogen_mean = TRUE)
plot_coef_panel(coef_ci_99, "Var", "Plots/coef_var", out = "svg", only_exogen_mean = TRUE)


#' @title Format Small Numbers
#' @description Formats numeric values. If very small, uses scientific notation; otherwise fixed decimal.
fmt <- function(x){
  ifelse(abs(x) < 1e-3, formatC(x, digits=3, format="e"),
         formatC(x, digits=4, format="f"))
}


#' @title Shorten Variable Names
#' @description Standardizes and shortens variable names for clean plotting and tables.
short_var <- function(v){
  dplyr::recode(v,
                "d_bux"="DBUX","d_cetop"="DCETOP","d_czk"="DCZK","d_pln"="DPLN","d_ron"="DRON",
                "MNB"="MNB","ECB"="ECB","FED"="FED",
                "mean_kinfo"="KI","mean_matolcsy"="MGY","mean_varga"="VM","mean_nagy"="NM",
                .default = toupper(v)
  )
}

#' @title Prettify Terms for LaTeX
#' @description Converts model output names into readable LaTeX strings (e.g., alpha1 -> $\alpha_1$).
pretty_term <- function(Param, Variable, Equation){
  par <- tolower(trimws(Param %||% ""))
  var <- Variable %||% NA_character_
  eq_is_var <- tolower(trimws(Equation)) %in% c("var","variance","vol","volatility","garch","sigma")
  
  if (!eq_is_var) {
    if (par %in% c("mu","(intercept)","intercept")) return("(Intercept)")
    if (grepl("^ar\\d+$", par)) return(sprintf("AR(%s)", sub("^ar","",par)))
    if (grepl("^ma\\d+$", par)) return(sprintf("MA(%s)", sub("^ma","",par)))
    if (grepl("^(m?x?reg\\d*|mxreg|xreg|reg)$", par)) return(short_var(var))
  } else {
    if (par %in% c("omega","w"))  return("$\\omega$")
    if (par %in% c("alpha1","a1","alpha")) return("$\\alpha_1$")
    if (par %in% c("beta1","b1","beta"))   return("$\\beta_1$")
    if (par %in% c("gamma1","g1","gamma")) return("$\\gamma_1$")
    if (grepl("^(v?x?reg\\d*|vxreg|zreg|vreg)$", par)) return(short_var(var))
  }
  
  if (!is.na(var) && nzchar(var)) paste0(Param, ":", short_var(var)) else Param
}

#' @title Make Top K Coefficient Table
#' @description Generates a wide-format, publication-ready table comparing the coefficients of the top K performing models.
make_topk_coef_table <- function(df, k = 5, rank_by = c("BIC_perobs","Test_RMSE")){
  rank_by <- match.arg(rank_by)
  
  models <- df %>%
    dplyr::group_by(ModelID) %>%
    dplyr::summarise(
      BIC_perobs = dplyr::first(BIC_perobs),
      Test_RMSE  = dplyr::first(Test_RMSE),
      Mean_spec  = dplyr::first(Mean_spec),
      Var_spec   = dplyr::first(Var_spec),
      Mod        = paste0(dplyr::first(GARCH_model),
                          ifelse(!is.na(dplyr::first(Sub_model)),
                                 paste0("(", dplyr::first(Sub_model), ")"), "")),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data[[rank_by]]) %>%
    dplyr::slice_head(n = k) %>%
    dplyr::mutate(ModelCol = sprintf("#%d %s", dplyr::row_number(), Mod))
  
  name_map <- setNames(models$ModelCol, models$ModelID)
  
  coef_long <- df %>%
    dplyr::filter(ModelID %in% models$ModelID) %>%
    dplyr::mutate(
      Stars = ifelse(is.na(Stars) & "p" %in% names(.), sig_code(p), Stars),
      Stars = ifelse(is.na(Stars), "", Stars),
      Term  = mapply(pretty_term, Param, Variable, Equation, USE.NAMES = FALSE),
      EqTag = ifelse(tolower(Equation) %in% c("var","variance","vol","garch","sigma"),
                     "[Var] ", "[Mean] "),
      Row   = paste0(EqTag, Term),
      Cell  = paste0(fmt(Estimate), Stars, " (", fmt(StdErr), ")")
    ) %>%
    dplyr::select(ModelID, Row, Cell)
  
  wide <- coef_long %>%
    dplyr::mutate(ModelCol = name_map[ModelID]) %>%
    dplyr::select(-ModelID) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = ModelCol, values_from = Cell)
  
  wide[is.na(wide)] <- ""
  
  # Sort: Mean-block, then Var-block; base params first
  base_mean <- c("[Mean] (Intercept)","[Mean] AR(1)","[Mean] AR(2)","[Mean] MA(1)","[Mean] MA(2)")
  base_var  <- c("[Var] $\\omega$","[Var] $\\alpha_1$","[Var] $\\beta_1$","[Var] $\\gamma_1$")
  ord <- c(
    base_mean,
    sort(setdiff(wide$Row[grepl("^\\[Mean\\]", wide$Row)], base_mean)),
    base_var,
    sort(setdiff(wide$Row[grepl("^\\[Var\\]",  wide$Row)], base_var))
  )
  ord <- unique(ord)
  wide <- wide %>% dplyr::arrange(factor(Row, levels = ord, ordered = TRUE))
  
  # Meta
  model_meta <- models %>%
    dplyr::transmute(
      Model = ModelCol,
      `Mean spec` = Mean_spec,
      `Var spec`  = Var_spec,
      `BIC/obs`   = sprintf("%.5f", BIC_perobs),
      RMSE        = sprintf("%.6f", Test_RMSE)
    )
  
  list(table = wide, models = model_meta)
}

## --- EXECUTE SAME AS BEFORE ---
out <- make_topk_coef_table(model_coefs_long, k = 5, rank_by = "BIC_perobs")
top5_coef_table  <- out$table
knitr::kable(top5_coef_table, format="latex", booktabs=TRUE, escape=FALSE,
             caption = "Coefficients of the top 5 mixed ARIMAX--GARCHX models (cells: estimate*** \\\\(SE\\\\)).")


## ============================================================ -
## 10. Top-N VaR (alpha = 0.01), refit 10 days, rolling window ----
## ============================================================ -

# settings
DATE_COL     <- "Date"        
Y_COL_NAME   <- "d_eur"        
ALPHA        <- 0.01   
REFIT_EVERY  <- 10
REFIT_WINDOW <- "moving"
ARIMA_ORDER  <- c(2,0)
GARCH_ORDER  <- c(1,1)
DIST_NAME    <- "sstd"
OUT_DIR      <- "var_output"
PLOT_DIR     <- file.path(OUT_DIR, "plots")
LOG_DIR      <- file.path(OUT_DIR, "logs")
TOP_N        <- 5

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(PLOT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(LOG_DIR, showWarnings = FALSE, recursive = TRUE)

# helpers
`%||%` <- function(a, b) if (is.null(a) || (length(a)==1 && is.na(a))) b else a

#' @title Parse Variables
#' @description Splits a "+" separated string into a character vector.
parse_vars <- function(x){
  if (is.null(x) || length(x)==0 || is.na(x) || !nzchar(x)) return(character(0))
  y <- unlist(strsplit(x, "+", fixed = TRUE))
  y <- trimws(y)
  y[nzchar(y)]
}

#' @title Kupiec POF Test
#' @description Performs the Kupiec Proportion of Failures test to validate VaR accuracy.
kupiec_pof <- function(hits, n, alpha){
  if (n <= 0) return(tibble::tibble(LRuc = NA_real_, pval = NA_real_))
  x <- hits
  phat <- if (n>0) x/n else NA_real_
  if (is.na(phat) || phat %in% c(0,1)) {
    return(tibble::tibble(LRuc = NA_real_, pval = NA_real_))
  }
  logL0 <- (n - x)*log(1 - alpha) + x*log(alpha)
  logL1 <- (n - x)*log(1 - phat)  + x*log(phat)
  LRuc  <- -2*(logL0 - logL1)
  pval  <- 1 - pchisq(LRuc, df = 1)
  tibble::tibble(LRuc = as.numeric(LRuc), pval = as.numeric(pval))
}

# simple log writer
log_msg <- function(file, ...) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%F %T"), sprintf(...)),
      file = file, append = TRUE)
}

#' @title Manual Rolling VaR Backtest
#' @description Performs a manual rolling window refit and extracts step-by-step VaR calculations based on the exact distribution parameters.
roll_var_for_model <- function(data, date_col, y_col,
                               mean_vars, var_vars,
                               garch_model, sub_model = NA_character_,
                               train_start, test_start, test_end,
                               arima_order = c(2,0), garch_order = c(1,1),
                               dist = "sstd",
                               refit_every = 10, refit_window = c("moving","expanding"),
                               alpha = 0.01,
                               save_series_rds = NULL, save_series_csv = NULL,
                               log_file = NULL) {
  refit_window <- match.arg(refit_window)
  if (!is.null(log_file)) {
    log_msg(log_file, "START model | mean={%s} | var={%s} | %s/%s | train=%s..%s | test=%s..%s",
            paste(mean_vars, collapse="+") %||% "-", paste(var_vars, collapse="+") %||% "-",
            as.character(garch_model), as.character(sub_model %||% "NA"),
            as.character(train_start), as.character(test_start),
            as.character(test_start), as.character(test_end))
  }
  
  stopifnot(date_col %in% names(data), y_col %in% names(data))
  dt <- data.table::as.data.table(data)
  dt[[date_col]] <- as.Date(dt[[date_col]])
  
  sel <- dt[[date_col]] >= as.Date(train_start) & dt[[date_col]] <= as.Date(test_end)
  if (!any(sel)) stop("Empty slice: train_start..test_end")
  
  y   <- dt[[y_col]][sel]
  Xm  <- if (length(mean_vars)) as.matrix(dt[sel, ..mean_vars, drop=FALSE]) else NULL
  Xv  <- if (length(var_vars))  as.matrix(dt[sel, ..var_vars, drop=FALSE])  else NULL
  dx  <- dt[[date_col]][sel]
  
  CC <- if (!is.null(Xm) && !is.null(Xv)) complete.cases(y, Xm, Xv) else
    if (!is.null(Xm)) complete.cases(y, Xm) else
      if (!is.null(Xv)) complete.cases(y, Xv) else complete.cases(y)
  y  <- y[CC]; dx <- dx[CC]
  if (!is.null(Xm)) Xm <- Xm[CC,, drop=FALSE]
  if (!is.null(Xv)) Xv <- Xv[CC,, drop=FALSE]
  
  tr_idx <- dx >= as.Date(train_start) & dx <  as.Date(test_start)
  te_idx <- dx >= as.Date(test_start)  & dx <= as.Date(test_end)
  if (!any(tr_idx)) stop("Train slice is empty after CC (complete cases).")
  if (!any(te_idx)) stop("Test slice is empty after CC (complete cases).")
  
  y_tr  <- y[tr_idx]
  Xm_tr <- if (!is.null(Xm)) Xm[tr_idx,,drop=FALSE] else NULL
  Xv_tr <- if (!is.null(Xv)) Xv[tr_idx,,drop=FALSE] else NULL
  
  vm <- list(model = garch_model, garchOrder = garch_order)
  if (!is.na(sub_model) && !is.null(sub_model)) vm$submodel <- sub_model
  if (!is.null(Xv_tr)) vm$external.regressors <- Xv_tr
  
  mm <- list(armaOrder = arima_order, include.mean = TRUE)
  if (!is.null(Xm_tr)) mm$external.regressors <- Xm_tr
  
  spec <- rugarch::ugarchspec(
    variance.model     = vm,
    mean.model         = mm,
    distribution.model = dist
  )
  
  fit0 <- try(rugarch::ugarchfit(spec = spec, data = y_tr,
                                 solver = "hybrid", solver.control = list(trace = 0)),
              silent = TRUE)
  if (inherits(fit0, "try-error")) {
    if (!is.null(log_file)) log_msg(log_file, "ERROR initial fit: %s", as.character(fit0))
    stop(fit0)
  }
  
  te_pos <- which(te_idx)
  tr_pos <- which(tr_idx)
  ntrain0 <- length(y_tr)
  
  mu_te    <- rep(NA_real_, length(te_pos))
  sig_te   <- rep(NA_real_, length(te_pos))
  var_te   <- rep(NA_real_, length(te_pos))
  act_te   <- y[te_idx]
  
  current_fit <- fit0
  

 get_pars_for_q <- function(fit){
    cf <- try(coef(fit), silent = TRUE)
    s  <- if (!inherits(cf,"try-error") && "skew"  %in% names(cf))  as.numeric(cf["skew"])  else 1
    sh <- if (!inherits(cf,"try-error") && "shape" %in% names(cf))  as.numeric(cf["shape"]) else 5
    list(skew = s, shape = sh)
  }
  
  refit_on_window <- function(i1,i2){
    y_w  <- y[i1:i2]
    Xm_w <- if (!is.null(Xm)) Xm[i1:i2,, drop=FALSE] else NULL
    Xv_w <- if (!is.null(Xv)) Xv[i1:i2,, drop=FALSE] else NULL
    vm <- list(model = garch_model, garchOrder = garch_order)
    if (!is.na(sub_model) && !is.null(sub_model)) vm$submodel <- sub_model
    if (!is.null(Xv_w)) vm$external.regressors <- Xv_w
    mm <- list(armaOrder = arima_order, include.mean = TRUE)
    if (!is.null(Xm_w)) mm$external.regressors <- Xm_w
    spec_w <- rugarch::ugarchspec(variance.model = vm, mean.model = mm,
                                  distribution.model = dist)
    rugarch::ugarchfit(spec = spec_w, data = y_w,
                       solver = "hybrid", solver.control = list(trace = 0))
  }
  
  for (j in seq_along(te_pos)){
    i <- te_pos[j]
    need_refit <- (j == 1L) || ((j-1L) %% refit_every == 0L)
    if (need_refit){
      if (REFIT_WINDOW == "expanding"){
        i1 <- min(tr_pos); i2 <- i-1L
      } else {
        i2 <- i - 1L
        i1 <- max(min(tr_pos), i2 - ntrain0 + 1L)
      }
      if (i2 > i1) {
        current_fit <- try(refit_on_window(i1,i2), silent = TRUE)
        if (inherits(current_fit, "try-error")) {
          if (!is.null(log_file)) log_msg(log_file, "WARN refit failed at j=%d, keep prev fit. Err=%s", j, as.character(current_fit))
          current_fit <- fit0
        }
      }
    }
    
    mex <- if (!is.null(Xm)) matrix(Xm[i, , drop=FALSE], nrow=1) else NULL
    vex <- if (!is.null(Xv)) matrix(Xv[i, , drop=FALSE], nrow=1) else NULL
    fcast <- rugarch::ugarchforecast(current_fit, n.ahead = 1,
                                     external.forecasts = list(mex = mex, vex = vex))
    mu  <- as.numeric(fitted(fcast))[1]
    sg  <- as.numeric(sigma(fcast))[1]
    pr  <- get_pars_for_q(current_fit)
    q   <- rugarch::qdist(dist, p = alpha, mu = 0, sigma = 1,
                          skew = pr$skew, shape = pr$shape)
    mu_te[j]  <- mu
    sig_te[j] <- sg
    var_te[j] <- mu + sg * q
  }
  
  out <- tibble::tibble(
    Date   = dx[te_idx],
    Actual = act_te,
    Mu     = mu_te,
    Sigma  = sig_te,
    VaR    = var_te,
    Hit    = Actual < VaR
  )
  
  # summary + Kupiec
  smry <- out %>%
    summarise(
      n_test = n(),
      breaches = sum(Hit, na.rm = TRUE),
      breach_rate = breaches / n_test,
      expected = alpha * n_test,
      .groups = "drop"
    )
  kp <- kupiec_pof(hits = smry$breaches, n = smry$n_test, alpha = alpha)
  smry <- bind_cols(smry, kp)
  
  if (!is.null(log_file)) {
    log_msg(log_file, "SUMMARY n=%d | breaches=%d (rate=%.4f, exp=%.2f) | LRuc=%.4f | p=%.4f",
            smry$n_test, smry$breaches, smry$breach_rate, smry$expected,
            smry$LRuc, smry$pval)
    log_msg(log_file, "END model")
  }
  
  # save files
  if (!is.null(save_series_rds)) saveRDS(out, save_series_rds)
  if (!is.null(save_series_csv)) readr::write_csv(out, save_series_csv)
  
  list(series = out, summary = smry)
}


#' @title Plot Saved VaR Backtest Series
#' @description Plots the actual log-returns against the 1% VaR limit and highlights breaches (hits).
save_var_plot <- function(series_or_path,
                          file,                 # path with OR without extension
                          out = c("svg","png"), # if no extension
                          width = 12, height = 6, dpi = 150){ 
  
  out <- match.arg(out)
  ext <- tolower(tools::file_ext(file))
  if (nzchar(ext)) {
    if (!ext %in% c("svg","png")) stop("Only .svg or .png allowed.")
    out <- ext
    file_out <- file
  } else {
    file_out <- paste0(file, ".", out)
  }
  dir.create(dirname(file_out), recursive = TRUE, showWarnings = FALSE)
  
  # read, if character (RDS)
  ser <- if (is.character(series_or_path)) {
    readRDS(series_or_path)
  } else {
    series_or_path
  }
  stopifnot(all(c("Date","Actual","VaR","Hit") %in% names(ser)))
  ser$Date <- as.Date(ser$Date)
  
  p <- ggplot(ser, aes(x = Date)) +
    geom_line(aes(y = Actual, colour = "Actual log-return"), linewidth = 0.7) +
    geom_line(aes(y = VaR,    colour = "VaR = 1%"),        linewidth = 0.9) +
    geom_point(
      data = ser %>% dplyr::filter(Hit),
      aes(y = Actual, colour = "VaR-violance"),
      size = 1.8, alpha = 0.8
    ) +
    scale_x_date(
      date_labels = "%Y-%m",
      date_breaks = "1 month",
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_color_manual(
      name   = NULL,
      values = c("Actual log-return" = "grey20",
                 "VaR = 1%"        = "#1f77b4",
                 "VaR-violance"      = "#d62728"),
      breaks = c("Actual log-return","VaR = 1%","VaR-violance")
    ) +
    labs(x = "Test period", y = "Log-return") +
    theme_regina() +
    theme(legend.position = "bottom")

  
  if (out == "svg") {
    ok <- FALSE
    if (requireNamespace("Cairo", quietly = TRUE)) {
      res <- try({ Cairo::CairoSVG(file_out, width = width, height = height)
        print(p); grDevices::dev.off() }, silent = TRUE)
      if (!inherits(res, "try-error")) ok <- TRUE
    }
    if (!ok && requireNamespace("svglite", quietly = TRUE)) {
      res <- try({ svglite::svglite(file_out, width = width, height = height)
        print(p); grDevices::dev.off() }, silent = TRUE)
      if (!inherits(res, "try-error")) ok <- TRUE
    }
    if (!ok) {
      ggplot2::ggsave(filename = file_out, plot = p, device = "svg",
                      width = width, height = height, dpi = dpi, bg = "white")
    }
  } else { # png
    ok <- FALSE
    px_w <- round(width  * dpi)
    px_h <- round(height * dpi)
    if (requireNamespace("Cairo", quietly = TRUE)) {
      res <- try({ Cairo::CairoPNG(file_out, width = px_w, height = px_h, bg = "white")
        print(p); grDevices::dev.off() }, silent = TRUE)
      if (!inherits(res, "try-error")) ok <- TRUE
    }
    if (!ok) {
      ggplot2::ggsave(filename = file_out, plot = p, device = "png",
                      width = width, height = height, dpi = dpi, bg = "white")
    }
  }
  
  message("Saved: ", normalizePath(file_out, mustWork = FALSE))
  invisible(p)
}

# Select Top N from the mixed table (base: BIC_perobs)
select_top_models <- function(mixed_tbl, top_n = 5, by = c("BIC_perobs","Test_RMSE")){
  by <- match.arg(by)
  stopifnot(by %in% names(mixed_tbl))
  mixed_tbl %>%
    dplyr::filter(is.finite(.data[[by]])) %>%
    dplyr::arrange(.data[[by]]) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(model_rank = dplyr::row_number())
}

## ============================================================ -
## 11. EXECUTION (plot is a separate step) ----
## ============================================================ -

mixed_tbl <- res03$mixed
top_tbl   <- select_top_models(mixed_tbl, top_n = TOP_N, by = "BIC_perobs")

results_list <- vector("list", nrow(top_tbl))
summary_list <- vector("list", nrow(top_tbl))


## VaR rolling forecast, and stores the results systematically without overwhelming memory.
for (ii in seq_len(nrow(top_tbl))){
  rowi <- top_tbl[ii,]
  mean_vars <- parse_vars(rowi$mean)
  var_vars  <- parse_vars(rowi$var)
  gmod      <- rowi$garch_model %||% "eGARCH"
  smod      <- rowi$sub_model   %||% NA_character_
  
  series_rds <- file.path(OUT_DIR, sprintf("VaR_series_alpha%03d_top%02d.rds", round(ALPHA*100), ii))
  series_csv <- file.path(OUT_DIR, sprintf("VaR_series_alpha%03d_top%02d.csv", round(ALPHA*100), ii))
  log_file   <- file.path(LOG_DIR, sprintf("VaR_alpha%03d_top%02d.log", round(ALPHA*100), ii))
  
  cat(sprintf("[VaR] RUN #%d | Mean={%s} | Var={%s} | %s/%s\n",
              ii, paste(mean_vars, collapse="+") %||% "-", paste(var_vars, collapse="+") %||% "-",
              as.character(gmod), as.character(smod)))
  
  r <- roll_var_for_model(
    data = Training, date_col = DATE_COL, y_col = Y_COL_NAME,
    mean_vars = mean_vars, var_vars = var_vars,
    garch_model = gmod, sub_model = smod,
    train_start = min(Training[[DATE_COL]]),
    test_start  = min(Training[[DATE_COL]][Training[[DATE_COL]] >= as.Date("2024-06-13")]),
    test_end    = max(Training[[DATE_COL]]),
    arima_order = ARIMA_ORDER, garch_order = GARCH_ORDER,
    dist = DIST_NAME,
    refit_every = REFIT_EVERY, refit_window = REFIT_WINDOW,
    alpha = ALPHA,
    save_series_rds = series_rds,
    save_series_csv = series_csv,
    log_file = log_file
  )
  
  results_list[[ii]] <- r$series %>% dplyr::mutate(model_rank = ii)
  summary_list[[ii]] <- r$summary %>% dplyr::mutate(model_rank = ii)
}

var_series_all <- data.table::rbindlist(results_list, fill = TRUE)
var_summary    <- data.table::rbindlist(summary_list, fill = TRUE)

# Join metadata (BIC, RMSE) to the VaR summary
var_summary <- top_tbl %>%
  dplyr::select(model_rank, mean, var, garch_model, sub_model, BIC_perobs, Test_RMSE) %>%
  dplyr::right_join(var_summary, by = "model_rank") %>%
  dplyr::arrange(model_rank)

message("=== VaR Summary (alpha = ", ALPHA, ") ===")
print(var_summary)

# Save consolidated results
readr::write_csv(var_summary, file.path(OUT_DIR, sprintf("VaR_summary_alpha%03d.csv", round(ALPHA*100))))
saveRDS(var_series_all, file.path(OUT_DIR, sprintf("VaR_series_all_alpha%03d.rds", round(ALPHA*100))))
message("Files saved to the '", OUT_DIR, "' directory.")

## ============================================================ -
## SEPARATE STEP: PLOT BASED ON SAVED RDS

# Save top 1 figure in SVG and PNG
series_rds_top1 <- file.path(OUT_DIR, sprintf("VaR_series_alpha%03d_top%02d.rds", round(ALPHA*100), 1))

save_var_plot(series_rds_top1,
              file = file.path(PLOT_DIR, "VaR_alpha001_top01"), out = "svg",
              width = 12, height = 6, dpi = 300)

save_var_plot(series_rds_top1,
              file = file.path(PLOT_DIR, "VaR_alpha001_top01.png"), # extension overrides 'out'
              out = "png", width = 12, height = 6, dpi = 150)

# End of File 1
save.image(file = file.path(this.path::here(), "tdk03_full_pipeline_output.RData"))