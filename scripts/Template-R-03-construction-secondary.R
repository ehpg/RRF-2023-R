# Construction - R - secondary sources
# Load necessary packages --------------------------

# install.packages("pacman")
# Set path of our data 

# Set folder path to where you downloaded the data

# read data 

rm(list=ls())

# from cleaned
colombia_connectivity_clean <- read.csv(here("data","connectivity_clean.csv"), encoding = "UTF-8")
colombia_connectivity_clean <- read.csv(here("data","colombia_connectivity_cleaned.csv"), encoding = "UTF-8")

# from tidying  
colombia_infraestructure <- read.csv(here("data", "colombia_infrastructure_wide.csv"), encoding = "UTF-8")


##### Task 1  --------------------------
# Plan construct outputs
# Add your code here to plan how to construct the outputs
# two data tables for analysis: one at municipality and one at state level
# current state of connectivity:  upload and download speed at municipality and state level
# 


##### Task 2  --------------------------
# Standardize units
# Add your code here to standardize units from KB to MB
kb_to_mb <- 1024

colombia_connectivity_clean <- colombia_connectivity_clean %>%
  mutate(avg_u_mbps = avg_u_kbps/kb_to_mb, 
         avg_d_mbps = avg_d_kbps/kb_to_mb)



##### Task 3  --------------------------
# Handle outliers 
# - Visual identification of outliers through plots
# - Define a function to handle outliers
# Define the function
winsor_function <- function(dataset, var, min = 0.00, max = 0.99){
  var_sym <- sym(var)
  
  percentiles <- quantile(
    dataset %>% pull(!!var_sym), probs = c(min, max), na.rm = TRUE
  )
  
  min_percentile <- percentiles[1]
  max_percentile <- percentiles[2]
  
  dataset %>%
    mutate(
      !!paste0(var, "_winsorized") := case_when(
        is.na(!!var_sym) ~ NA_real_,
        !!var_sym <= min_percentile ~ percentiles[1],
        !!var_sym >= max_percentile ~ percentiles[2],
        TRUE ~ !!var_sym
      )
    )
}

# - Apply the function to your dataset
winsor_percentile <- 0.99

colombia_connectivity_clean <-  winsor_function(colombia_connectivity_clean, var = "avg_u_mbps", max = winsor_percentile)
colombia_connectivity_clean <-  winsor_function(colombia_connectivity_clean, var = "avg_d_mbps", max = winsor_percentile)


## check outliers after winsorization
ggplot(colombia_connectivity_clean, aes(y = avg_d_mbps)) + 
  geom_boxplot() +
  facet_wrap(~trimester)

ggplot(colombia_connectivity_clean, aes(y = avg_d_mbps_winsorized)) + 
  geom_boxplot() +
  facet_wrap(~trimester)

ggplot(colombia_connectivity_clean, aes(y = avg_d_mbps)) + 
  geom_histogram() 

ggplot(colombia_connectivity_clean, aes(y = avg_d_mbps_winsorized)) +
  geom_histogram()

##### Task 4  --------------------------
# Create indicators
# - Create a connectivity state database
connectivity_state <- colombia_connectivity_clean %>%
  group_by(ADM1_PC, trimester) %>%
  summarise(avg_d_mbps = mean(avg_d_mbps, na.rm = T),
            avg_u_mbps = mean(avg_u_mbps, na.rm = T),
            avg_d_mbps_winsorized = mean(avg_d_mbps_winsorized, na.rm = T),
            avg_u_mbps_winsorized = mean(avg_u_mbps_winsorized, na.rm = T)) %>%
  ungroup()
  
# - Create a connectivity municipality database
connectivity_municipality <- colombia_connectivity_clean %>%
  group_by(ADM2_PC, trimester) %>%
  summarise(avg_d_mbps = mean(avg_d_mbps, na.rm = T),
            avg_u_mbps = mean(avg_u_mbps, na.rm = T),
            avg_d_mbps_winsorized = mean(avg_d_mbps_winsorized, na.rm = T),
            avg_u_mbps_winsorized = mean(avg_u_mbps_winsorized, na.rm = T)) %>%
  ungroup()



# create state level infrastructure 
infrastructure_state <- colombia_infraestructure %>%
  group_by(ADM1_PC) %>%
  summarise(college = sum(college, na.rm = T),
            clinic = sum(clinic, na.rm = T),
            university = sum(university, na.rm = T),
            school = sum(school, na.rm = T),
            hospital = sum(hospital, na.rm = T)) 


infrastructure_state[infrastructure_state == 0] <- NA

# count infrastructure types at state level
infrastructure_state <- infrastructure_state %>%
  rowwise() %>%
  mutate(types_infrastructure = sum(!is.na(c(college, clinic, university, school, hospital)))) %>%
  ungroup()


# count infrastructure types at municipality level
infrastructure_municipality <- colombia_infraestructure %>%
  rowwise() %>%
  mutate(types_infrastructure = sum(!is.na(c(college, clinic, university, school, hospital)))) %>%
  ungroup()

# merge 
# municipality
municipality <- connectivity_municipality %>%
  left_join(infrastructure_municipality)

# state
state <- connectivity_state %>%
  left_join(infrastructure_state)


## create lagged variable
# municipality level
municipality <- municipality %>%
  group_by(ADM2_PC) %>%
  arrange(trimester) %>%
  mutate(avg_d_mbps_winsorized_lag = lag(avg_d_mbps_winsorized),
         avg_u_mbps_winsorized_lag = lag(avg_u_mbps_winsorized),
         avg_d_mbps_lag = lag(avg_d_mbps),
         avg_u_mbps_lag = lag(avg_u_mbps), 
         diff_avg_d_winsorized = avg_d_mbps_winsorized - avg_d_mbps_winsorized_lag,
         diff_avg_u_winsorized = avg_d_mbps_winsorized - avg_u_mbps_winsorized_lag, 
         diff_avg_d = avg_d_mbps - avg_d_mbps_lag, 
         diff_avg_u = avg_d_mbps - avg_u_mbps_lag) %>%
  ungroup() %>%
  arrange(ADM2_PC)

# state level
state <- state %>%
  group_by(ADM1_PC) %>%
  arrange(trimester) %>%
  mutate(avg_d_mbps_winsorized_lag = lag(avg_d_mbps_winsorized),
         avg_u_mbps_winsorized_lag = lag(avg_u_mbps_winsorized),
         avg_d_mbps_lag = lag(avg_d_mbps),
         avg_u_mbps_lag = lag(avg_u_mbps), 
         diff_avg_d_winsorized = avg_d_mbps_winsorized - avg_d_mbps_winsorized_lag,
         diff_avg_u_winsorized = avg_d_mbps_winsorized - avg_u_mbps_winsorized_lag, 
         diff_avg_d = avg_d_mbps - avg_d_mbps_lag, 
         diff_avg_u = avg_d_mbps - avg_u_mbps_lag) %>%
  ungroup() %>%
  arrange(ADM1_PC)


##### Task 5  --------------------------
# Save the final datasets
# - Remove unnecessary variables
# - Save the datasets as CSV files

state_save <- state %>%
  select(ADM1_PC, trimester, avg_u_mbps_winsorized, avg_d_mbps_winsorized, diff_avg_u_winsorized, diff_avg_d_winsorized, 
         school, college, university, hospital, clinic, types_infrastructure)

write.csv(state_save, here("data", "state.csv"), row.names = F)



municipality_save <- municipality %>%
  select(ADM1_PC, ADM2_PC, trimester, avg_u_mbps_winsorized, avg_d_mbps_winsorized, diff_avg_u_winsorized, diff_avg_d_winsorized, 
         school, college, university, hospital, clinic, types_infrastructure)

write.csv(municipality_save, here("data", "municipality.csv"), row.names = F)














