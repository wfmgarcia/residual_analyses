#### PLOTS OF LOG-RESIDUALS WITH DOC LABEL ####
library(ggplot2)

residual_summary <- read.csv("03_results/residual_norm.csv") %>%
  filter(Site != "USF17")


####
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
  filename = "04_figures/residual_plots/Q_residual1.png",  # file path and name
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
  filename = "04_figures/residual_plots/Q_residual2.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300)


## Q Residual Mean Slope
residual_summary %>%
  ggplot(aes(x = mean_slope_deg, y = Qres_mean, colour = location)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Qres_mean - Qres_SE,
                    ymax = Qres_mean + Qres_SE),
                width = 0.1) +
  
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal() +
  labs(x = "Mean Slope (deg)",
       y = "Mean Q Residual")


# Save the last ggplot to a PNG file
ggsave(
  filename = "04_figures/residual_plots/Q_residual3.png",  # file path and name
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
  filename = "04_figures/residual_plots/DOC_residual1.png",  # file path and name
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
  filename = "04_figures/residual_plots/DOC_residual2.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300)  


## NPOC Residual Mean Slope
residual_summary %>%
  ggplot(aes(x = mean_slope_deg, y = NPOCres_mean, colour = location)) +
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
  filename = "04_figures/residual_plots/DOC_residual3.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300) 







## NO3 Residual Mean Precip (1)
residual_summary %>%
  ggplot(aes(x = mean_precip_mm, y = no3res_mean, colour = location)) +
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
  filename = "04_figures/residual_plots/NO3_residual1.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300)  


## NO3 Residual Mean Precip (2)
residual_summary %>%
  ggplot(aes(x = mean_slope_deg, y = no3res_mean, colour = Network.Role)) +
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
  filename = "04_figures/residual_plots/NO3_residual2.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300)  


## NO3 Residual Mean Slope
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
  filename = "04_figures/residual_plots/NO3_residual3.png",  # file path and name
  plot = last_plot(),                              # the last plot you created
  width = 6, height = 4,                           # size in inches
  dpi = 300) 

