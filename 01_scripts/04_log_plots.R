#### PLOTS OF LOG-RESIDUALS WITH DOC LABEL ####
library(ggplot2)

residual_summary <- read.csv("03_results/residual_log.csv") %>%
  filter(Site != "USF17")

# --- Helper functions ---
plot_residual <- function(data, y_mean, y_SE, y_label, color_var) {
  ggplot(data, aes_string(x = "mean_precip_mm", y = y_mean, colour = color_var)) +
    geom_point(size = 3) +
    geom_errorbar(aes_string(ymin = paste0(y_mean, " - ", y_SE),
                             ymax = paste0(y_mean, " + ", y_SE)),
                  width = 0.1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(x = "Mean Precipitation (mm)",
         y = y_label,
         colour = color_var)
}

plot_residual_slope <- function(data, y_mean, y_SE, y_label, color_var) {
  ggplot(data, aes_string(x = "mean_slope_deg", y = y_mean, colour = color_var)) +
    geom_point(size = 3) +
    geom_errorbar(aes_string(ymin = paste0(y_mean, " - ", y_SE),
                             ymax = paste0(y_mean, " + ", y_SE)),
                  width = 0.1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(x = "Mean Slope (deg)",
         y = y_label,
         colour = color_var)
}

# --- Q residuals ---
p <- plot_residual(residual_summary, "res_logQ_mean", "res_logQ_SE", "Mean Log Q Residual", "location")
print(p)
p <- plot_residual(residual_summary, "res_logQ_mean", "res_logQ_SE", "Mean Log Q Residual", "Network.Role")
print(p)

# --- DOC (NPOC) residuals ---
p <- plot_residual(residual_summary, "res_logNPOC_mean", "res_logNPOC_SE", "Mean Log DOC Residual", "location")
print(p)
p <- plot_residual(residual_summary, "res_logNPOC_mean", "res_logNPOC_SE", "Mean Log DOC Residual", "Network.Role")
print(p)

# --- NO3 residuals ---
p <- plot_residual(residual_summary, "res_logNO3_mean", "res_logNO3_SE", "Mean Log NO3 Residual", "location")
print(p)
p <- plot_residual(residual_summary, "res_logNO3_mean", "res_logNO3_SE", "Mean Log NO3 Residual", "Network.Role")
print(p)

# --- Q, DOC, NO3 vs slope ---
p <- plot_residual_slope(residual_summary, "res_logQ_mean", "res_logQ_SE", "Mean Log Q Residual", "location")
print(p)
p <- plot_residual_slope(residual_summary, "res_logQ_mean", "res_logQ_SE", "Mean Log Q Residual", "Network.Role")
print(p)

p <- plot_residual_slope(residual_summary, "res_logNPOC_mean", "res_logNPOC_SE", "Mean Log DOC Residual", "location")
print(p)
p <- plot_residual_slope(residual_summary, "res_logNPOC_mean", "res_logNPOC_SE", "Mean Log DOC Residual", "Network.Role")
print(p)

p <- plot_residual_slope(residual_summary, "res_logNO3_mean", "res_logNO3_SE", "Mean Log NO3 Residual", "location")
print(p)
p <- plot_residual_slope(residual_summary, "res_logNO3_mean", "res_logNO3_SE", "Mean Log NO3 Residual", "Network.Role")
print(p)


#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/residual_log.png",      #Change solute
       plot = p,
       width = 6,
       height = 4,
       dpi = 300)
