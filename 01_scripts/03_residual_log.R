# This script is used to calculate residuals for the USF project water chemistry data.

# Load Packages
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(lubridate)
library(dataRetrieval)
library(viridis)
library(openxlsx)
library(ggpmisc)

##### Read in data and compute log-residuals per campaign

# --- Read CSV and initial filtering ---
usf_data <- read.csv("02_data/combined_data.csv") %>%
  filter(!is.na(q), campaign > 6) %>%     # remove incomplete campaigns
  group_by(site) %>%
  filter(n() > 2) %>%                     # remove sites with ≤2 observations
  ungroup() %>%
  select(-contains("mg"), -contains("ug"))


# --- List of solute mass columns ---
mass_cols <- c("npoc_mass", "no3_mass", "tdn_mass", 
               "po4_mass", "cl_mass", "so4_mass", "na_mass",
               "k_mass","ca_mass", "tss_mass", "afdm_mass")


# --- Compute site-level mean per solute and Q (for regression) ---
site_summary <- usf_data %>%
  group_by(site) %>%
  summarise(
    area_m2        = first(area_m2),
    location       = first(location),
    network_role   = first(network_role),
    mean_precip_mm = first(mean_annual_ppt_mm),
    mean_slope_deg = first(mean_slope),
    mean_aspect_deg = first(mean_aspect),
    
    across(all_of(c("q", mass_cols)),
           ~mean(.x, na.rm = TRUE),
           .names = "{.col}_mean"),
    .groups = "drop"
  ) %>%
  mutate(
    logArea = log10(area_m2),
    
    logQ    = log10(ifelse(q_mean > 0, q_mean, NA)),
    logNPOC = log10(ifelse(npoc_mass_mean > 0, npoc_mass_mean, NA)),
    logNO3  = log10(ifelse(no3_mass_mean > 0, no3_mass_mean, NA)),
    logTDN  = log10(ifelse(tdn_mass_mean > 0, tdn_mass_mean, NA)),
    logPO4  = log10(ifelse(po4_mass_mean > 0, po4_mass_mean, NA)),
    logCl   = log10(ifelse(cl_mass_mean > 0, cl_mass_mean, NA)),
    logSO4  = log10(ifelse(so4_mass_mean > 0, so4_mass_mean, NA)),
    logNa   = log10(ifelse(na_mass_mean > 0, na_mass_mean, NA)),
    logK    = log10(ifelse(k_mass_mean > 0, k_mass_mean, NA)),
    logTSS  = log10(ifelse(tss_mass_mean > 0, tss_mass_mean, NA)),
    logAFDM = log10(ifelse(afdm_mass_mean > 0, afdm_mass_mean, NA))
  )


# --- Fit log-log linear models ---
lm_list <- list(
  logQ    = lm(logQ ~ logArea, data = site_summary),
  logNPOC = lm(logNPOC ~ logArea, data = site_summary),
  logNO3  = lm(logNO3 ~ logArea, data = site_summary),
  logTDN  = lm(logTDN ~ logArea, data = site_summary),
  logPO4  = lm(logPO4 ~ logArea, data = site_summary),
  logCl   = lm(logCl ~ logArea, data = site_summary),
  logSO4  = lm(logSO4 ~ logArea, data = site_summary),
  logNa   = lm(logNa ~ logArea, data = site_summary),
  logK    = lm(logK ~ logArea, data = site_summary),
  logTSS  = lm(logTSS ~ logArea, data = site_summary),
  logAFDM = lm(logAFDM ~ logArea, data = site_summary)
)


# --- Graph example (DOC) ---
ggplot(site_summary, aes(x = logArea, y = logNPOC)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 5
  ) +
  labs(
    x = expression(Log[10]~Subcatchment~Area~(m^2)),
    y = expression(Log[10]~Site~Avg.~DOC~Mass),
    title = "Log–Log Relationship Between Site-Averaged DOC Mass \nand Subcatchment Area"
  ) +
  theme_classic(base_size = 14)


# Save plot
ggsave(
  filename = "04_figures/linear_models/DOC_regression_LOG.png",
  plot = last_plot(),
  width = 6, height = 4, dpi = 300
)


# --- Add log-transformed values to full dataset ---
usf_data_log <- usf_data %>%
  mutate(
    logArea = log10(area_m2),
    
    logQ    = log10(ifelse(q > 0, q, NA)),
    logNPOC = log10(ifelse(npoc_mass > 0, npoc_mass, NA)),
    logNO3  = log10(ifelse(no3_mass > 0, no3_mass, NA)),
    logTDN  = log10(ifelse(tdn_mass > 0, tdn_mass, NA)),
    logPO4  = log10(ifelse(po4_mass > 0, po4_mass, NA)),
    logCl   = log10(ifelse(cl_mass > 0, cl_mass, NA)),
    logSO4  = log10(ifelse(so4_mass > 0, so4_mass, NA)),
    logNa   = log10(ifelse(na_mass > 0, na_mass, NA)),
    logK    = log10(ifelse(k_mass > 0, k_mass, NA)),
    logTSS  = log10(ifelse(tss_mass > 0, tss_mass, NA)),
    logAFDM = log10(ifelse(afdm_mass > 0, afdm_mass, NA))
  )


# --- Predict values + residuals ---
for(solute in names(lm_list)) {
  
  pred_name <- paste0("PL_", solute)
  res_name  <- paste0("res_", solute)
  
  usf_data_log[[pred_name]] <- predict(lm_list[[solute]], newdata = usf_data_log)
  
  usf_data_log[[res_name]] <-
    usf_data_log[[solute]] - usf_data_log[[pred_name]]
}


# --- Summarize residuals per site across campaigns ---
res_cols <- grep("^res_", names(usf_data_log), value = TRUE)

residual_summary <- usf_data_log %>%
  group_by(site, location, area_m2, network_role,
           mean_annual_ppt_mm, mean_slope, mean_aspect) %>%
  summarise(
    across(all_of(res_cols),
           list(
             mean = ~mean(.x, na.rm = TRUE),
             se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
           ),
           .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )


# --- Save final residual dataset ---
write.csv(residual_summary,
          "03_results/residual_log.csv",
          row.names = FALSE)
