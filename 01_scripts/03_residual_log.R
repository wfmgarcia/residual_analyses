#This script is used to calculate residuals for the USF project water chemistry data.

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
usf_data <- read.csv("02_data/usf_data.csv") %>%
  filter(!is.na(Q), Campaign > 6) %>%     # remove incomplete campaigns
  group_by(Site) %>%
  filter(n() > 2) %>%                     # remove sites with ≤2 observations
  ungroup() %>%
  select(-contains("mg"), -contains("ug")) # keep only characterizations and solute mass


# --- List of solute mass columns ---
mass_cols <- c("npoc_mass", "no3_mass", "tdn_mass", 
               "po4_mass", "cl_mass", "so4_mass", "na_mass",
               "k_mass","ca_mass", "tss_mass", "afdm_mass")



# --- Compute site-level mean per solute and Q (for regression) ---
site_summary <- usf_data %>%
  group_by(Site) %>%
  summarise(
    Area.m2       = first(Area.m2),
    location      = first(location),
    Network.Role  = first(Network.Role),
    mean_precip_mm = first(mean_precip_mm),
    mean_slope_deg = first(mean_slope_deg),
    across(all_of(c("Q", mass_cols)), ~mean(.x, na.rm = TRUE), .names = "{.col}_mean"),
    .groups = "drop")%>%
  mutate(
    logArea  = log10(Area.m2),
    logQ     = log10(Q_mean),
    logNPOC  = log10(npoc_mass_mean),
    logNO3   = log10(no3_mass_mean),
    logTDN   = log10(tdn_mass_mean),
    logPO4   = log10(po4_mass_mean),
    logCl    = log10(cl_mass_mean),
    logSO4   = log10(so4_mass_mean),
    logNa    = log10(na_mass_mean),
    logK     = log10(k_mass_mean),
    logTSS   = log10(tss_mass_mean),
    logAFDM  = log10(afdm_mass_mean)
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

# --- Add log-transformed values to full dataset ---
usf_data_log <- usf_data %>%
  mutate(
    logQ     = log10(Q),
    logNPOC  = log10(npoc_mass),
    logNO3   = log10(no3_mass),
    logTDN   = log10(tdn_mass),
    logPO4   = log10(po4_mass),
    logCl    = log10(cl_mass),
    logSO4   = log10(so4_mass),
    logNa    = log10(na_mass),
    logK     = log10(k_mass),
    logTSS   = log10(tss_mass),
    logAFDM  = log10(afdm_mass)
  )

# Ensure logArea exists
usf_data_log <- usf_data_log %>% mutate(logArea = log10(Area.m2))

# Compute predicted values and residuals
for(solute in names(lm_list)) {
  pred_name <- paste0("PL_", solute)
  res_name  <- paste0("res_", solute)
  
  usf_data_log[[pred_name]] <- predict(lm_list[[solute]], newdata = usf_data_log)
  usf_data_log[[res_name]]  <- usf_data_log[[paste0("log", substr(solute, 4, nchar(solute)) )]] - usf_data_log[[pred_name]]
}


# --- Summarize residuals per site across campaigns ---
res_cols <- grep("^res_", names(usf_data_log), value = TRUE)

residual_summary <- usf_data_log %>%
  group_by(Site, location, Area.m2, Network.Role, mean_precip_mm, mean_slope_deg) %>%
  summarise(
    across(all_of(res_cols),
           list(mean = ~ mean(.x, na.rm = TRUE),
                SE   = ~ sd(.x, na.rm = TRUE) / sqrt(n())),
           .names = "{.col}_{.fn}"),
    .groups = "drop"
  )

# --- Save final residual dataset ---
write.csv(residual_summary, "03_results/residual_log.csv", row.names = FALSE)

