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

#### Read in data

# --- Read CSV ---
usf_data <- read.csv("02_data/usf_data.csv") %>%
  filter(!is.na(Q), Campaign > 6) %>%     #removes campaigns that were not quite the entire watershed (prior to 2024-05-23)
  group_by(Site) %>%                    
  filter(n() > 2) %>%                     #removes any site that have 2 or less observations, as no average can be conducted
  ungroup() %>%
  select(-contains("mg"),
         -contains("ug"))                 #removes original chem data, only retains characterizations and solute mass
#filter(Network.Role == "tributary")    #To do the analysis only on tributaries

# --- List of solute mass columns ---
# "npoc_mass", "no3_mass", "nh4_mass", "tdn_mass", 
# "po4_mass", "cl_mass", "so4_mass", "na_mass", "k_mass", 
# "ca_mass", "tss_mass", "afdm_mass"

mass_cols <- c("npoc_mass", "no3_mass", "tdn_mass", 
               "po4_mass", "cl_mass", "so4_mass", "na_mass", #NH4 not included due to data missing for an entire campaign
               "k_mass","ca_mass", "tss_mass", "afdm_mass")  #For my brownbag, I only chose NPOD, NO3




####Creating artificial sites for residual anchoring

# --- Global min/max for Area.m2 ---
min_area <- min(usf_data$Area.m2, na.rm = TRUE)        #Determining smallest watershed (USF28)
max_area <- max(usf_data$Area.m2, na.rm = TRUE)        #Determining largest watershed (USF12)

# --- Reshape to long format for campaign- and solute-specific extremes ---
usf_long <- usf_data %>%
  pivot_longer(cols = all_of(mass_cols),
               names_to = "Solute",
               values_to = "Mass")

# ---  Compute min/max per campaign and solute ---
campaign_solute_extremes <- usf_long %>%
  group_by(Campaign, Solute) %>%
  summarise(
    min_Mass = min(Mass, na.rm = TRUE),
    max_Mass = max(Mass, na.rm = TRUE),
    min_Q = min(Q, na.rm = TRUE),
    max_Q = max(Q, na.rm = TRUE),
    .groups = "drop"
  )

# --- Build artificial Site0 (min) ---
site0_all <- campaign_solute_extremes %>%
  mutate(Mass = min_Mass / 2, Q = min_Q / 2, Artificial = "Site0") %>%     #Half the smallest mass and q, per campaign.
  select(Campaign, Solute, Mass, Q, Artificial) %>%
  pivot_wider(names_from = Solute, values_from = Mass) %>%
  mutate(Site = "Site0",
         Area.m2 = min_area / 2)                                          #Half the smallest area.

# --- Build artificial SiteX (max) ---
siteX_all <- campaign_solute_extremes %>%
  mutate(Mass = max_Mass * 2, Q = max_Q * 2, Artificial = "SiteX") %>%    #Double the largest mass and q, per campaign.
  select(Campaign, Solute, Mass, Q, Artificial) %>%
  pivot_wider(names_from = Solute, values_from = Mass) %>%
  mutate(Site = "SiteX",
         Area.m2 = max_area * 2)                                          #Double the largest area.

# ---  Combine original and artificial sites ---
usf_data_anchor <- usf_data %>%
  mutate(Artificial = "Original") %>%
  bind_rows(site0_all, siteX_all)


#### Normalizing all data

# ---  Normalize mass per solute per campaign ---
norm_usf_data <- usf_data_anchor %>%
  group_by(Campaign) %>%
  mutate(
    norm_Q = (Q - min(Q, na.rm = TRUE)) / (max(Q, na.rm = TRUE) - min(Q, na.rm = TRUE)),
    norm_npoc = (npoc_mass - min(npoc_mass, na.rm = TRUE)) / (max(npoc_mass, na.rm = TRUE) - min(npoc_mass, na.rm = TRUE)),
    norm_no3 = (no3_mass - min(no3_mass, na.rm = TRUE)) / (max(no3_mass, na.rm = TRUE) - min(no3_mass, na.rm = TRUE)),
    norm_tdn = (tdn_mass - min(tdn_mass, na.rm = TRUE)) / (max(tdn_mass, na.rm = TRUE) - min(tdn_mass, na.rm = TRUE)),
    norm_po4 = (po4_mass - min(po4_mass, na.rm = TRUE)) / (max(po4_mass, na.rm = TRUE) - min(po4_mass, na.rm = TRUE)),
    norm_cl  = (cl_mass - min(cl_mass, na.rm = TRUE)) / (max(cl_mass, na.rm = TRUE) - min(cl_mass, na.rm = TRUE)),
    norm_so4 = (so4_mass - min(so4_mass, na.rm = TRUE)) / (max(so4_mass, na.rm = TRUE) - min(so4_mass, na.rm = TRUE)),
    norm_na  = (na_mass - min(na_mass, na.rm = TRUE)) / (max(na_mass, na.rm = TRUE) - min(na_mass, na.rm = TRUE)),
    norm_k   = (k_mass - min(k_mass, na.rm = TRUE)) / (max(k_mass, na.rm = TRUE) - min(k_mass, na.rm = TRUE)),
    norm_tss = (tss_mass - min(tss_mass, na.rm = TRUE)) / (max(tss_mass, na.rm = TRUE) - min(tss_mass, na.rm = TRUE)),
    norm_afdm = (afdm_mass - min(afdm_mass, na.rm = TRUE)) / (max(afdm_mass, na.rm = TRUE) - min(afdm_mass, na.rm = TRUE))) %>%
  ungroup()


#This code does the normalization in a conicse way. Should give the same output as above.But did nto work.
#usf_data <- usf_data %>%
#  group_by(Campaign) %>%
#  mutate(
#    norm_Q = (Q - min(Q, na.rm = TRUE)) / (max(Q, na.rm = TRUE) - min(Q, na.rm = TRUE)),
#    across(all_of(mass_cols),
#           ~ (.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)),
#           .names = "norm_{.col}")
#  ) %>%
#  ungroup()



# ---  Compute mean and SE per site ---
norm_cols <- grep("^norm_", names(usf_data), value = TRUE)

norm_site_stats <- norm_usf_data %>%
  group_by(Site) %>%
  summarise(
    across(
      all_of(norm_cols),
      list(
        mean = ~ mean(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop")     #Check that it normalized effective (Site0 and SiteX should be 0 and 1 respectively)

# --- Join mean back to the main dataset ---
norm_usf_data <- usf_data %>%
  left_join(norm_site_stats, by = "Site")




#Log transform data before averaging
log_usf_data <- usf_data %>%
  mutate(
    across(
      all_of(mass_cols),
      ~ log1p(.x),     # safe log for zeros
      .names = "{.col}_log"
    ),
    Area.m2_log = log1p(Area.m2))




### Calculate raw means (no normalization)
raw_site_stats <- usf_data %>%
  group_by(Site) %>%
  summarise(
    across(
      all_of(mass_cols),
      list(
        mean = ~ mean(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# --- Join mean/SE back to the main dataset ---
raw_usf_data <- usf_data %>%
  left_join(raw_site_stats, by = "Site")





# Get one row per site
site_summary <- usf_data %>%
  distinct(Site, .keep_all = TRUE)   # keeps first row per Site, including norm_Q_mean and Area.m2


#For linear models of solutes across all campaigns
lm_q_all     <- lm(norm_Q_mean ~ Area.m2 + 0, data = site_summary)
lm_npoc_all  <- lm(norm_npoc_mean ~ Area.m2 + 0, data = site_summary)
lm_no3_all   <- lm(norm_no3_mean ~ Area.m2 + 0, data = site_summary)
lm_tdn_all   <- lm(norm_tdn_mean ~ Area.m2 + 0, data = site_summary)
lm_po4_all   <- lm(norm_po4_mean ~ Area.m2 + 0, data = site_summary)
lm_cl_all    <- lm(norm_cl_mean ~ Area.m2 + 0, data = site_summary)
lm_so4_all   <- lm(norm_so4_mean ~ Area.m2 + 0, data = site_summary)
lm_na_all    <- lm(norm_na_mean ~ Area.m2 + 0, data = site_summary)
lm_k_all     <- lm(norm_k_mean ~ Area.m2 + 0, data = site_summary)
lm_tss_all   <- lm(norm_tss_mean ~ Area.m2 + 0, data = site_summary)
lm_afdm_all  <- lm(norm_afdm_mean ~ Area.m2 + 0, data = site_summary)


##For linear models of log transformed solute masses and area across all campaigns
lm_q_all <- lm(log(norm_Q_mean) ~ log(Area.m2) + 0, data = site_summary)
#lm_npoc_log <- lm(log(norm_npoc_mean) ~ log(Area.m2) + 0, data = site_summary)


# Plotting linear regression of Normalized Mean Q
ggplot(site_summary, aes(x = Area.m2, y = norm_Q_mean)) +
  geom_point(size = 2, color = "blue") +
  labs(
    x = "Watershed Area (m²)",
    y = "Mean Normalized Q",
    title = "Mean Normalized Q vs Area"
  ) +
  theme_minimal()

# Save the last ggplot to a PNG file
ggsave(
  filename = "figures/brown/mean_norm_Q_raw.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300                                        # resolution
)

# To not include extremes
site_summary %>%
  filter(!Site %in% c("Site0", "SiteX")) %>%   # <-- exclude these sites
  ggplot(aes(x = Area.m2, y = norm_Q_mean)) +
  geom_point(size = 2, color = "blue") +
  labs(
    x = "Watershed Area (m²)",
    y = "Mean Normalized Streamflow",
    title = "Mean Normalized Streamflow"
  ) +
  theme_minimal()

site_summary %>%
  #filter(!Site %in% c("Site0", "SiteX")) %>%   # <-- exclude these sites
  ggplot(aes(x = Area.m2, y = norm_Q_mean)) +
  geom_point(size = 2, color = "blue") +
  labs(
    x = "Watershed Area (m²)",
    y = "Mean Normalized Streamflow",
    title = "Mean Normalized Streamflow"
  ) +
  theme_minimal()


coef_q <- coef(lm_q_all)[1]

ggplot(filter(site_summary, !Site %in% c("Site0", "SiteX")),
       aes(x = Area.m2, y = norm_Q_mean)) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = coef_q, color = "red") +
  theme_minimal()



# Plotting linear regression of Normalized Mean Q
ggplot(site_summary, aes(x = Area.m2, y = norm_Q_mean)) +
  geom_point(size = 2, color = "blue") +
  geom_smooth(method = "lm", formula = y ~ x + 0, se = TRUE, color = "red") +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x + 0,
    parse = TRUE) +
  labs(
    x = "Watershed Area (m²)",
    y = "Mean Normalized Discharge",
    title = "Linear Regression of Mean Normalized Discharge"
  ) +
  theme_minimal()

# Save the last ggplot to a PNG file
ggsave(
  filename = "figures/brown/mean_norm_Q_regression.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300                                        # resolution
)


# Plotting linear regression of Normalized Mean NPOC Mass
ggplot(site_summary, aes(x = Area.m2, y = norm_npoc_mean)) +
  geom_point(size = 2, color = "blue") +
  geom_smooth(method = "lm", formula = y ~ x + 0, se = TRUE, color = "red") +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x + 0,
    parse = TRUE)+
  labs(
    x = "Watershed Area (m²)",
    y = "Mean Normalized DOC Mass",
    title = "Linear Regression of Mean Normalized DOC Mass vs Area"
  ) +
  theme_minimal()


# Save the last ggplot to a PNG file
ggsave(
  filename = "figures/brown/mean_norm_npoc_mass_regression.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300)                                       # resolution)


# Plotting linear regression of Normalized Mean NO3 Mass
ggplot(site_summary, aes(x = Area.m2, y = norm_no3_mean)) +
  geom_point(size = 2, color = "blue") +
  geom_smooth(method = "lm", formula = y ~ x + 0, se = TRUE, color = "red") +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x + 0,
    parse = TRUE) +
  labs(
    x = "Watershed Area (m²)",
    y = "Mean Normalized NO3",
    title = "Linear Regression of Mean Normalized NO3 vs Area"
  ) +
  theme_minimal()

# Save the last ggplot to a PNG file
ggsave(
  filename = "figures/brown/mean_norm_no3_mass_regression.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300)                                       # resolution



######## Residuals

#Slope and intercept vectors
a_linear <- 0                       # Intercept
b_linearq <- coef(lm_q_all) [1]      # Slope
b_linearnpoc <- coef(lm_npoc_all) [1] 
b_linearno3 <- coef(lm_no3_all) [1] 
b_lineartss <- coef(lm_tss_all) [1] 

##Remove site 0 and Site x
usf_data <- usf_data %>%
  filter(Site != "Site0", Site != "SiteX")



#Predict for the entire data set (PL = predicted linear)
usf_data <- usf_data %>%
  mutate(PL_q_all = a_linear + b_linearq * Area.m2) %>%
  mutate(PL_npoc_all = a_linear + b_linearnpoc * Area.m2) %>%
  mutate(PL_no3_all = a_linear + b_linearno3 * Area.m2) %>%
  mutate(PL_tss_all = a_linear + b_lineartss * Area.m2)


usf_data <- usf_data %>%
  mutate(residuals_q_all = norm_Q - PL_q_all)%>%
  mutate(residuals_npoc_all = norm_npoc - PL_npoc_all)%>%
  mutate(residuals_no3_all = norm_no3 - PL_no3_all)%>%
  mutate(residuals_tss_all = norm_tss - PL_tss_all)

usf_data_filtered <- usf_data %>%
  group_by(Site) %>%
  mutate(
    n_q     = sum(!is.na(residuals_q_all)),
    n_npoc  = sum(!is.na(residuals_npoc_all)),
    n_no3   = sum(!is.na(residuals_no3_all)),
    n_tss   = sum(!is.na(residuals_tss_all)),
  ) %>%
  # Keep only sites with >2 usable residuals for ALL variables
  filter(
    n_q    > 2,
    n_npoc > 2,
    n_no3  > 2,
    n_tss  > 2,
  ) %>%
  ungroup()

usf_data_filtered <- usf_data_filtered %>%
  group_by(Site) %>%
  mutate(
    Qres_mean      = mean(residuals_q_all, na.rm = TRUE),
    Qres_SE        = sd(residuals_q_all, na.rm = TRUE) / sqrt(sum(!is.na(residuals_q_all))),
    
    NPOCres_mean   = mean(residuals_npoc_all, na.rm = TRUE),
    NPOCres_SE     = sd(residuals_npoc_all, na.rm = TRUE) / sqrt(sum(!is.na(residuals_npoc_all))),
    
    tssres_mean    = mean(residuals_tss_all, na.rm = TRUE),
    tssres_SE      = sd(residuals_tss_all, na.rm = TRUE) / sqrt(sum(!is.na(residuals_tss_all))),
    
    no3res_mean     = mean(residuals_no3_all, na.rm = TRUE),
    no3res_SE       = sd(residuals_no3_all, na.rm = TRUE) / sqrt(sum(!is.na(residuals_no3_all))),
    
  ) %>%
  ungroup()


residual_summary <- usf_data_filtered %>%
  select(
    Site,location, Area.m2, Network.Role, mean_elevation_m, mean_slope_deg, mean_aspect_deg, mean_precip_mm,
    Qres_mean, Qres_SE,
    NPOCres_mean, NPOCres_SE,
    no3res_mean, no3res_SE,
    tssres_mean, tssres_SE,
  ) %>%
  distinct() %>%              # One row per site
  arrange(Site)

######################PLOTS

## Q Residual Mean Precip color location
residual_summary %>%
  ggplot(aes(x = mean_precip_mm, y = Qres_mean, colour = location)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Qres_mean - Qres_SE,
                    ymax = Qres_mean + Qres_SE),
                width = 0.1) +
  
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal() +
  labs(x = "Mean Precipitation (mm)",
       y = "Mean Discharge Residual") 

# Save the last ggplot to a PNG file
ggsave(
  filename = "figures/brown/mean_norm_q_precip_location.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300)   


## Q Residual Mean Precip color role
residual_summary %>%
  ggplot(aes(x = mean_precip_mm, y = Qres_mean, colour = Network.Role)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Qres_mean - Qres_SE,
                    ymax = Qres_mean + Qres_SE),
                width = 0.1) +
  
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal() +
  labs(x = "Mean Precipitation (mm)",
       y = "Mean Q Residual") 

# Save the last ggplot to a PNG file
ggsave(
  filename = "figures/brown/mean_norm_q_precip_role.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300)  


## NPOC Residual Mean Precip
residual_summary %>%
  ggplot(aes(x = mean_precip_mm, y = NPOCres_mean, colour = location)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = NPOCres_mean - NPOCres_SE,
                    ymax = NPOCres_mean + NPOCres_SE),
                width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal() +
  labs(x = "Mean Precipitation (mm)",
       y = "Mean DOC Residual") 

# Save the last ggplot to a PNG file
ggsave(
  filename = "figures/brown/mean_norm_doc_precip_location.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300)  



## NPOC Residual Mean Precip
residual_summary %>%
  ggplot(aes(x = mean_precip_mm, y = NPOCres_mean, colour = Network.Role)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = NPOCres_mean - NPOCres_SE,
                    ymax = NPOCres_mean + NPOCres_SE),
                width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal() +
  labs(x = "Mean Precipitation (mm)",
       y = "Mean DOC Residual") 

# Save the last ggplot to a PNG file
ggsave(
  filename = "figures/brown/mean_norm_doc_precip_role.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300)  



## NO3 Residual Mean Precip
residual_summary %>%
  ggplot(aes(x = mean_precip_mm, y = no3res_mean, colour = Network.Role)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = no3res_mean - no3res_SE,
                    ymax = no3res_mean + no3res_SE),
                width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal() +
  labs(x = "Mean Precipitation (mm)",
       y = "Mean NO3 Residual") 

# Save the last ggplot to a PNG file
ggsave(
  filename = "figures/brown/mean_norm_no3_precip_role.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300)  


## NO3 Residual Mean Precip
residual_summary %>%
  ggplot(aes(x = mean_slope_deg, y = no3res_mean, colour = location)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = no3res_mean - no3res_SE,
                    ymax = no3res_mean + no3res_SE),
                width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal() +
  labs(x = "Mean Slope (deg)",
       y = "Mean NO3 Residual") 

# Save the last ggplot to a PNG file
ggsave(
  filename = "figures/brown/mean_norm_no3_slope_location.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300)  






## NPOC Residual Mean Slope
residual_summary %>%
  ggplot(aes(x = mean_slope_deg, y = NPOCres_mean, colour = Network.Role)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = NPOCres_mean - NPOCres_SE,
                    ymax = NPOCres_mean + NPOCres_SE),
                width = 0.1) +
  
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal() +
  labs(x = "Mean Slope (deg)",
       y = "Mean DOC Residual")


# Save the last ggplot to a PNG file
ggsave(
  filename = "figures/brown/mean_norm_doc_slope_role.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300) 





























# Elevation v Slope
residual_summary %>%
  #filter(Network.Role == "tributary") %>%     # <-- keep only tributary sites
  ggplot(aes(x = log(Area.m2), y = NPOCres_mean, colour = Network.Role)) +
  geom_point(size = 3) +
  #geom_errorbar(aes(ymin = NPOCres_mean - NPOCres_SE,
  #                  ymax = NPOCres_mean + NPOCres_SE),
  #              width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(x = "Area (m2) - Log",
       y = "Mean NPOC (deg)")


