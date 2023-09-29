# Analysis - R - secondary sources Template


# Set folder path to where you downloaded the data

rm(list=ls())

# read data 
municipality_database <- read.csv(here("data", "municipality.csv"))

state_database <- read.csv(here("data", "state.csv"))


# Task 1: Create Summary Statistics ------------------------

# For Municipality
# Note: Use dplyr functions such as summarise and across to calculate summary statistics like mean, sd, etc.
# ....
# Use gt package to create nice looking tables.

summary_municipality <- municipality_database %>%
# only numeric variables 
summarise(across(avg_u_mbps_winsorized:clinic, list(mean = ~mean(.x, na.rm = TRUE), 
                                                    sd = ~sd(.x, na.rm = TRUE), 
                                                    cilower = ~quantile(.x, 0.025, na.rm = TRUE), 
                                                    ciupper = ~quantile(.x, 0.975, na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(),
               names_to = c("Statistic", "Variable"),
               values_to = "value", 
               names_sep = "_(?=[^_]+$)") %>% 
  pivot_wider(names_from = Variable) %>% 
  rename(Mean = "mean", SD = "sd", `95% CI Lower` = "cilower", `95% CI Upper` = "ciupper")

# stargazer(summary_municipality, type = "latex", title="Descriptive statistics", digits=1, out= here("outputs","municipality_DS.tex"))

summary_municipality %>% 
  gt() %>%
  gtsave(here("outputs", "tables","table_municipality_summary.tex"))



# For State
# Note: Similarly, create summary statistics for the state database
# ....


summary_state <- state_database %>%
  # only numeric variables 
  summarise(across(avg_u_mbps_winsorized:clinic, list(mean = ~mean(.x, na.rm = TRUE), 
                                                      sd = ~sd(.x, na.rm = TRUE), 
                                                      cilower = ~quantile(.x, 0.025, na.rm = TRUE), 
                                                      ciupper = ~quantile(.x, 0.975, na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(),
               names_to = c("Statistic", "Variable"),
               values_to = "value", 
               names_sep = "_(?=[^_]+$)") %>% 
  pivot_wider(names_from = Variable) %>% 
  rename(Mean = "mean", SD = "sd", `95% CI Lower` = "cilower", `95% CI Upper` = "ciupper")

summary_municipality %>% 
  gt() %>%
  gtsave(here("outputs", "tables", "table_municipality_summary.tex"))



# Task 2: Visualization of Individual Variables ------------

# Histogram
# Note: Use ggplot2 package to create histograms. Use geom_histogram() function for histograms.
(hist_avg_d_mbps_winsorized <- municipality_database %>%
  ggplot(aes(x = avg_d_mbps_winsorized, fill = as.factor(trimester))) +
  geom_histogram(position = "dodge")+
  theme_minimal())

hist_avg_u_mbps_winsorized <- municipality_database %>%
  ggplot(aes(x = avg_u_mbps_winsorized, fill = as.factor(trimester))) +
  geom_histogram(position = "dodge")+
  theme_minimal()

hist_diff_avg_u_winsorized <- municipality_database %>%
  ggplot(aes(x = diff_avg_u_winsorized)) +
  geom_histogram()+
  theme_minimal()

hist_diff_avg_d_winsorized <- municipality_database %>%
  ggplot(aes(x = diff_avg_d_winsorized)) +
  geom_histogram()+
  theme_minimal()

(hist_school <- municipality_database %>%
  ggplot(aes(x = school)) +
  geom_histogram()+
  theme_minimal())

hist_college <- municipality_database %>%
  ggplot(aes(x = college)) +
  geom_histogram()+
  theme_minimal()

hist_university <- municipality_database %>%
  ggplot(aes(x = university)) +
  geom_histogram()

hist_hospital <- municipality_database %>%
  ggplot(aes(x = hospital)) +
  geom_histogram()+
  theme_minimal()

hist_clinic <- municipality_database %>%
  ggplot(aes(x = clinic)) +
  geom_histogram() +
  theme_minimal()

# save histogram plots
ggsave(here("outputs", "plots", "hist_download_winsorized.png"), hist_avg_d_mbps_winsorized)
ggsave(here("outputs", "plots", "hist_upload_winsorized.png"), hist_avg_u_mbps_winsorized)
#ggsave(here("outputs", "plots", "hist_download_diff_winsorized.png"), hist_diff_avg_d_winsorized)
ggsave(here("outputs", "plots", "hist_upload_diff_winsorized.png"), hist_diff_avg_u_winsorized)
ggsave(here("outputs", "plots", "hist_schools.png"), hist_school)
ggsave(here("outputs", "plots", "hist_colleges.png"), hist_college)
ggsave(here("outputs", "plots", "hist_universitys.png"), hist_university)
ggsave(here("outputs", "plots", "hist_hospitals.png"),hist_hospital)
ggsave(here("outputs", "plots", "hist_clinics.png"),hist_clinic)


# Boxplot
# Note: Use geom_boxplot() function to create box plots.
(plot_avg_d_mbps_winsorized <- municipality_database %>%
    ggplot(aes(x = avg_d_mbps_winsorized, fill = as.factor(trimester))) +
    geom_boxplot(position = "dodge")+
    theme_minimal())

plot_avg_u_mbps_winsorized <- municipality_database %>%
  ggplot(aes(x = avg_u_mbps_winsorized, fill = as.factor(trimester))) +
  geom_boxplot(position = "dodge")+
  theme_minimal()

plot_diff_avg_u_winsorized <- municipality_database %>%
  ggplot(aes(x = diff_avg_u_winsorized)) +
  geom_boxplot()+
  theme_minimal()

plot_diff_avg_d_winsorized <- municipality_database %>%
  ggplot(aes(x = diff_avg_d_winsorized)) +
  geom_boxplot()+
  theme_minimal()

plot_school <- municipality_database %>%
  ggplot(aes(x = school)) +
  geom_boxplot()+
  theme_minimal()

(plot_college <- municipality_database %>%
  ggplot(aes(y = college)) +
  geom_boxplot() +
  #xlim(c(0, 10)) +
  theme_minimal())

plot_university <- municipality_database %>%
  ggplot(aes(x = university)) +
  geom_boxplot()

plot_hospital <- municipality_database %>%
  ggplot(aes(x = hospital)) +
  geom_boxplot()+
  theme_minimal()

plot_clinic <- municipality_database %>%
  ggplot(aes(x = clinic)) +
  geom_boxplot() +
  theme_minimal()



# Save the plots using ggsave()
# ....

# Task 3: Regression Analysis ------------------------

# Building Simple Linear Regression Model
# Note: Use lm() function to create a linear model. Use summary() function to get a summary of the model.

fit1 <- lm(types_infrastructure ~ avg_d_mbps_winsorized, data = municipality_database)
summary(fit1)

# Building Panel Data Model
# Note: Use pdata.frame() to create a panel data frame and use plm() function for panel data models.
# ....
municipality_panel <- pdata.frame(municipality_database, c("ADM2_PC", "trimester"))
panel_reg <- plm(types_infrastructure ~ avg_d_mbps_winsorized + trimester, data = municipality_panel, model = "between")



# Multiple Regression Model with Clustered Standard Errors
# Note: Use plm() function to build the model and vcovHC() and coeftest() functions to get clustered standard errors.



# Save the model using stargazer
# Note: Use the stargazer package to create a neat table of your regression results. 
# Set different parameters in the stargazer function to customize the table according to your needs.




# Task 4: Visual Analysis ------------------------

# Relationship Analysis
# Note: Use ggplot2 for scatter plots and add trend lines using geom_smooth() function. 
# Analyze the relationship between different variables visually.
# ....

# Change in Connectivity Analysis
# Note: Use ggplot2 to create a bar plot to visualize changes in connectivity. 
#You can use dplyr functions like filter, group_by, and summarize to process the data before plotting.
# ....

# Task 5: Correlation Analysis ------------------------

# Note: Use cor.test() function to perform correlation tests. Use cor() function to create a correlation matrix.



# Visualize the correlation matrix
# Note: Use corrplot() function from corrplot package to visualize the correlation matrix.
# ....