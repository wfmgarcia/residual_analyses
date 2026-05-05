#### PLOTS OF LOG-RESIDUALS WITH DOC LABEL ####
library(ggplot2)
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(lubridate)
library(dataRetrieval)
library(viridis)
library(openxlsx)
library(ggpmisc)
library(ggrepel)

residual_summary <- read.csv("03_results/residual_log.csv") %>%
  filter(site != "USF17")

# --- Helper functions ---
plot_residual <- function(data, y_mean, y_SE, y_label, color_var) {
  
  data$ymin <- data[[y_mean]] - data[[y_SE]]
  data$ymax <- data[[y_mean]] + data[[y_SE]]
  
  ggplot(data, aes_string(x = "mean_annual_ppt_mm",
                          y = y_mean,
                          colour = color_var,
                          label = "site")) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax),
                  width = 0.1) +
    geom_text_repel(
      size = 3,
      min.segment.length = 0,
      box.padding = 0.6,
      point.padding = 0.5,
      max.overlaps = Inf,
      segment.color = NA
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(x = "Mean Precipitation (mm)",
         y = y_label,
         colour = color_var)
}


plot_residual_slope <- function(data, y_mean, y_SE, y_label, color_var) {
  
  data$ymin <- data[[y_mean]] - data[[y_SE]]
  data$ymax <- data[[y_mean]] + data[[y_SE]]
  
  ggplot(data, aes_string(x = "mean_slope",
                          y = y_mean,
                          colour = color_var,
                          label = "site")) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax),
                  width = 0.1) +
    geom_text_repel(
      size = 3,
      min.segment.length = 0,
      box.padding = 0.6,
      point.padding = 0.5,
      max.overlaps = Inf,
      segment.color = NA
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(x = "Mean Slope (deg)",
         y = y_label,
         colour = color_var)
}

plot_residual_aspect <- function(data, y_mean, y_SE, y_label, color_var) {
  
  data$ymin <- data[[y_mean]] - data[[y_SE]]
  data$ymax <- data[[y_mean]] + data[[y_SE]]
  
  ggplot(data, aes_string(x = "mean_aspect",
                          y = y_mean,
                          colour = color_var,
                          label = "site")) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax),
                  width = 0.1) +
    geom_text_repel(
      size = 3,
      min.segment.length = 0,
      box.padding = 0.6,
      point.padding = 0.5,
      max.overlaps = Inf,
      segment.color = NA
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(x = "Mean Aspect (deg)",
         y = y_label,
         colour = color_var)
}

# Remove middle sites
#residual_summary <- residual_summary %>%
#  filter(location %in% c("lower", "upper"))

# --- Q residuals ---
p <- plot_residual(residual_summary, "res_logQ_mean", "res_logQ_se", "Mean Log Discharge Residual", "location")
print(p)

#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/Qresidual_precip.png",      #Change solute
       plot = p,
       width = 12,
       height = 6,
       dpi = 300)



p <- plot_residual(residual_summary, "res_logQ_mean", "res_logQ_se", "Mean Log Q Residual", "network_role")
print(p)

#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/Qresidual_precip_role.png",      #Change solute
       plot = p,
       width = 12,
       height = 6,
       dpi = 300)





# --- DOC (NPOC) residuals ---
p <- plot_residual(residual_summary, "res_logNPOC_mean", "res_logNPOC_se", "Mean Log DOC Residual", "location")
print(p)

#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/DOCresidual_precip.png",      #Change solute
       plot = p,
       width = 12,
       height = 6,
       dpi = 300)

p <- plot_residual(residual_summary, "res_logNPOC_mean", "res_logNPOC_se", "Mean Log DOC Residual", "network_role")
print(p)

#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/DOCresidual_precip_role.png",      #Change solute
       plot = p,
       width = 12,
       height = 6,
       dpi = 300)





# --- NO3 residuals ---
p <- plot_residual(residual_summary, "res_logNO3_mean", "res_logNO3_se", "Mean Log NO3 Residual", "location")
print(p)

#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/NO3residual_precip.png",      #Change solute
       plot = p,
       width = 12,
       height = 6,
       dpi = 300)


p <- plot_residual(residual_summary, "res_logNO3_mean", "res_logNO3_se", "Mean Log NO3 Residual", "network_role")
print(p)

#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/NO3residual_precip_role.png",      #Change solute
       plot = p,
       width = 12,
       height = 6,
       dpi = 300)

# --- PO4 residuals ---
p <- plot_residual(residual_summary, "res_logPO4_mean", "res_logPO4_se", "Mean Log PO4 Residual", "location")
print(p)

#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/PO4residual_precip.png",      #Change solute
       plot = p,
       width = 12,
       height = 6,
       dpi = 300)


p <- plot_residual(residual_summary, "res_logPO4_mean", "res_logPO4_se", "Mean Log PO4 Residual", "network_role")
print(p)

#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/PO4residual_precip_role.png",      #Change solute
       plot = p,
       width = 12,
       height = 6,
       dpi = 300)



# --- TSS residuals ---
p <- plot_residual(residual_summary, "res_logTSS_mean", "res_logTSS_se", "Mean Log TSS Residual", "location")
print(p)

#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/TSSresidual_precip.png",      #Change solute
       plot = p,
       width = 12,
       height = 6,
       dpi = 300)

p <- plot_residual(residual_summary, "res_logTSS_mean", "res_logTSS_se", "Mean Log TSS Residual", "network_role")
print(p)

#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/TSSresidual_precip_role.png",      #Change solute
       plot = p,
       width = 12,
       height = 6,
       dpi = 300)

tribs_only <- residual_summary %>%
  filter(network_role == "Tributary")


# --- Q, DOC, NO3 vs slope ---
p <- plot_residual_slope(residual_summary, "res_logQ_mean", "res_logQ_se", "Mean Log Q Residual", "location")
print(p)

#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/slope/Qresidual_slope.png",      #Change solute
       plot = p,
       width = 12,
       height = 6,
       dpi = 300)


# --- Q, DOC, NO3 vs slope ---
p <- plot_residual_slope(tribs_only, "res_logQ_mean", "res_logQ_se", "Mean Log Q Residual", "location")
print(p)

#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/slope/Qresidual_slope_tribs.png",      #Change solute
       plot = p,
       width = 12,
       height = 6,
       dpi = 300)




p <- plot_residual_slope(residual_summary, "res_logQ_mean", "res_logQ_SE", "Mean Log Q Residual", "Network.Role")
print(p)

p <- plot_residual_slope(residual_summary, "res_logNPOC_mean", "res_logNPOC_se", "Mean Log DOC Residual", "location")
print(p)

#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/slope/DOCresidual_slope.png",      #Change solute
       plot = p,
       width = 12,
       height = 6,
       dpi = 300)




p <- plot_residual_slope(residual_summary, "res_logNPOC_mean", "res_logNPOC_se", "Mean Log DOC Residual", "network_role")
print(p)

p <- plot_residual_slope(residual_summary, "res_logNO3_mean", "res_logNO3_se", "Mean Log NO3 Residual", "location")
print(p)

#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/slope/NO3residual_slope.png",      #Change solute
       plot = p,
       width = 12,
       height = 6,
       dpi = 300)



p <- plot_residual_slope(residual_summary, "res_logNO3_mean", "res_logNO3_SE", "Mean Log NO3 Residual", "Network.Role")
print(p)

p <- plot_residual_slope(residual_summary, "res_logTSS_mean", "res_logTSS_se", "Mean Log TSS Residual", "location")
print(p)

#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/slope/TSSresidual_slope.png",      #Change solute
       plot = p,
       width = 12,
       height = 6,
       dpi = 300)



# --- Q, DOC, NO3 vs aspect ---
p <- plot_residual_aspect(residual_summary, "res_logQ_mean", "res_logQ_se", "Mean Log Q Residual", "location")
print(p)

#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/aspect/Qresidual_aspect.png",      #Change solute
       plot = p,
       width = 12,
       height = 6,
       dpi = 300)



p <- plot_residual_aspect(residual_summary, "res_logQ_mean", "res_logQ_SE", "Mean Log Q Residual", "Network.Role")
print(p)

p <- plot_residual_aspect(residual_summary, "res_logNPOC_mean", "res_logNPOC_SE", "Mean Log DOC Residual", "location")
print(p)
p <- plot_residual_aspect(residual_summary, "res_logNPOC_mean", "res_logNPOC_SE", "Mean Log DOC Residual", "Network.Role")
print(p)

p <- plot_residual_aspect(residual_summary, "res_logNO3_mean", "res_logNO3_SE", "Mean Log NO3 Residual", "location")
print(p)
p <- plot_residual_aspect(residual_summary, "res_logNO3_mean", "res_logNO3_SE", "Mean Log NO3 Residual", "Network.Role")
print(p)



#Save your desired plot by printing and saving! 
ggsave("04_figures/residual_plots/TSS_slope.png",      #Change solute
       plot = p,
       width = 6,
       height = 4,
       dpi = 300)


p <- plot_residual(residual_summary, "res_logPO4_mean", "res_logPO4_SE", "Mean Log PO4 Residual", "location")
print(p)


