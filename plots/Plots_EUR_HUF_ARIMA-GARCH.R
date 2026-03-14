# ============================================================================ -
# VISUALIZATION SCRIPT - DUAL EXPORT (PNG & SVG)
# - Saves all plots to a "Plots" subfolder
# - Automatically generates both .png and .svg formats for each figure
# ============================================================================ -
load(file.path(this.path::here(), "tdk03_full_pipeline_output.RData"))
req <- c("Cairo", "cowplot", "dplyr", "ggplot2", "stringr", "tidyr", "this.path")

to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
invisible(lapply(req, library, character.only = TRUE))

setwd(this.path::here())
getwd()
rm(to_install, req)

# 0. Global Aesthetics & Export Wrappers ---------------------------------------
primary_col   <- "#022576"
secondary_col <- "#FF0000"
third_col     <- "#008000"
fourth_col    <- "#820800"

theme_regina <- function() {
  theme(
    plot.title   = element_text(family = "Times New Roman", size = 28, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(family = "Times New Roman", size = 18, face = "bold"),
    axis.title.x = element_text(family = "Times New Roman", size = 24, margin = margin(t = 10)),
    axis.title.y = element_text(family = "Times New Roman", size = 24, margin = margin(r = 10)),
    axis.text.x  = element_text(family = "Times New Roman", size = 18, angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y  = element_text(family = "Times New Roman", size = 18),
    legend.title = element_text(family = "Times New Roman", size = 20, face = "bold"),
    legend.text  = element_text(family = "Times New Roman", size = 18),
    legend.position = "bottom",                        
    legend.justification = "center",
    plot.caption = element_text(family = "Times New Roman", size = 16, hjust = 0, margin = margin(t = 15)),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background  = element_rect(fill = "white", color = "white"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  )
}

# Export wrapper for PNG
export_png <- function(plot_obj, file_base, width = 1600, height = 900, dpi = 300) {
  if (!dir.exists("Plots")) dir.create("Plots", recursive = TRUE)
  full_path <- file.path("Plots", paste0(file_base, ".png"))
  ggsave(filename = full_path, plot = plot_obj, device = "png", 
         width = width/dpi, height = height/dpi, dpi = dpi, bg = "white")
}

# Export wrapper for SVG (using Cairo to prevent memory overflow)
export_svg <- function(plot_obj, file_base, width = 1600, height = 900, dpi = 100) {
  if (!dir.exists("Plots")) dir.create("Plots", recursive = TRUE)
  full_path <- file.path("Plots", paste0(file_base, ".svg"))
  Cairo::CairoSVG(file = full_path, width = width/dpi, height = height/dpi)
  print(plot_obj)
  dev.off()
}

# Unified wrapper to trigger both
export_both <- function(plot_obj, file_base, width = 1200, height = 800) {
  export_png(plot_obj, file_base, width = width, height = height)
  export_svg(plot_obj, file_base, width = width, height = height)
  message("Saved PNG and SVG for: Plots/", file_base)
}


# ============================================================================ -
# 1. ACF and PACF Cowplot ----
# ============================================================================ -
# Requires 'Training' dataframe in memory

ggplot.corr <- function(data, lag.max = 24, ci = 0.95, ylim_min = -0.1, ylim_max = 0.1) {
  data <- na.omit(data)
  
  list.acf <- acf(data, lag.max = lag.max, type = "correlation", plot = FALSE)
  N <- as.numeric(list.acf$n.used)
  df1 <- data.frame(lag = list.acf$lag, acf = list.acf$acf)
  
  list.pacf <- acf(data, lag.max = lag.max, type = "partial", plot = FALSE)
  df2 <- data.frame(lag = list.pacf$lag, pacf = list.pacf$acf)
  
  plot.acf <- ggplot(data = df1, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0, color = secondary_col, linewidth = 0.7) +
    geom_col(fill = primary_col, width = 0.7) +
    geom_hline(yintercept = qnorm((1 + ci) / 2) / sqrt(N), color = third_col, linetype = "dashed") +
    geom_hline(yintercept = -qnorm((1 + ci) / 2) / sqrt(N), color = third_col, linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, max(df1$lag), 1)) +
    scale_y_continuous(name = element_blank(), limits = c(ylim_min, ylim_max)) +
    ggtitle("ACF") + theme_regina() + labs(x="Lags")
  
  plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
    geom_hline(yintercept = 0, color = secondary_col, linewidth = 0.7) +  
    geom_col(fill = primary_col, width = 0.7) +
    geom_hline(yintercept = qnorm((1 + ci) / 2) / sqrt(N), color = third_col, linetype = "dashed") +
    geom_hline(yintercept = -qnorm((1 + ci) / 2) / sqrt(N), color = third_col, linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, max(df2$lag, na.rm = TRUE), 1)) +
    scale_y_continuous(name = element_blank(), limits = c(ylim_min, ylim_max)) +
    ggtitle("PACF") + theme_regina() + labs(x="Lags")
  
  cowplot::plot_grid(plot.acf, plot.pacf, nrow = 1)
}

c_plot <- ggplot.corr(data = Training$d_eur, lag.max = 15, ylim_min = -0.1, ylim_max = 0.1) 
export_both(c_plot, "acf_pacf_vertical", width = 1400, height = 700)


# ============================================================================ -
# 2. Variable Frequency Bar Chart  ----
# ============================================================================ -
# Assumes 'freq_df' exists in memory

p_freq <- ggplot(freq_df, aes(x = Variable, y = Count, fill = Role)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Mean" = primary_col, "Variance" = secondary_col), name = NULL) +
  labs(subtitle = "Division by role (Mean - Variance)", x = "Variable", y = "Frequency of occurrences") +
  theme_regina()

export_both(p_freq, "valtozo_gyakorisag_mean_vs_var", width = 1200, height = 800)


# ============================================================================ -
# 3-4. Coefficient Forest Plots  ----
# ============================================================================ -
# Assumes 'coef_ci_99' dataframe exists in memory

normalize_equation <- function(x) {
  lx <- tolower(trimws(x))
  out <- ifelse(lx %in% c("mean","mu"), "Mean",
                ifelse(lx %in% c("var","variance","vol","volatility","garch","sigma","variance eq"),
                       "Var", NA_character_))
  ifelse(is.na(out), x, out)
}

plot_coef_panel_dual <- function(df, eq_label, file_base, width = 1200, height = 1000) {
  dd <- df %>%
    mutate(Eq_norm = normalize_equation(Equation),
           Term_label = sprintf("%s (n=%d)", Term, n),
           Significance = ifelse(sig_99, "Significant (99%)", "Insignificant")) %>%
    dplyr::filter(tolower(Eq_norm) == tolower(eq_label))
  
  dd <- dd %>% dplyr::filter(!(Term %in% c("(Intercept)")), !stringr::str_detect(Term, "^(AR|MA)\\("))
  dd <- dd %>% mutate(Term_label = factor(Term_label, levels = Term_label[order(abs(mean_est), decreasing = TRUE)]))
  
  p <- ggplot(dd, aes(x = Term_label, y = mean_est, colour = Significance)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, colour = "grey40") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0, linewidth = 0.9, alpha = 0.95) +
    geom_point(size = 2.6) +
    coord_flip() +
    labs(title = ifelse(tolower(eq_label)=="mean", "Mean equation", "Variance Equation"),
         x = "Variable (sample size in brackets)", y = "Coefficient rate",
         caption = "Source: Authors' own calculations.") +
    scale_colour_manual(values = c("Significant (99%)" = "#2ca02c", "Insignificant" = "#d62728"), name = NULL) +
    theme_regina()
  
  export_both(p, file_base, width = width, height = height)
}

plot_coef_panel_dual(coef_ci_99, "Mean", "coef_mean")
plot_coef_panel_dual(coef_ci_99, "Var", "coef_var")


# ============================================================================ -
# 5. Value at Risk Backtest Series Plot  ----
# ============================================================================ -
# Assumes 'series_rds_top1' points to a valid RDS file or dataframe

save_var_plot_dual <- function(series_data, file_base, width = 1400, height = 700) { 
  ser <- if (is.character(series_data)) readRDS(series_data) else series_data
  ser$Date <- as.Date(ser$Date)
  
  p <- ggplot(ser, aes(x = Date)) +
    geom_line(aes(y = Actual, colour = "Actual log-return"), linewidth = 0.7) +
    geom_line(aes(y = VaR,    colour = "VaR = 1%"),        linewidth = 0.9) +
    geom_point(data = ser %>% dplyr::filter(Hit),
               aes(y = Actual, colour = "VaR-violance"), size = 1.8, alpha = 0.8) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month", expand = expansion(mult = c(0.01, 0.01))) +
    scale_color_manual(name = NULL,
                       values = c("Actual log-return" = "grey20", "VaR = 1%" = "#1f77b4", "VaR-violance" = "#d62728")) +
    labs(x = "Test period", y = "Log-return") +
    theme_regina()
  
  export_both(p, file_base, width = width, height = height)
}

save_var_plot_dual(series_rds_top1, "VaR_alpha001_top01")