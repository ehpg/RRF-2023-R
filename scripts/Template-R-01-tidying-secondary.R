# Tidying data - R - secondary sources - template
# Load necessary packages --------------------------

# install.packages("pacman")

rm(list = ls())


# Exercise 1 ---------------------------------------

# Step 1: Read the wide format data
connectivity_wide <- read.csv(here("data","colombia_connectivity_wide.csv"))

# Step 2: Remove duplicates 
connectivity_wide <- connectivity_wide %>%
  distinct()

# Step 3: Reshape data
connectivity_long <- connectivity_wide %>%
  pivot_longer(cols = ends_with(c("_01", "_04")), 
               names_to = c(".value", "quarter"),
               names_pattern = "(.+)_(\\d+)", 
               values_to = ".value") 


# Step 4: Verify your dataset has the desired structure

# save
write.csv(connectivity_long, here("data", "connectivity_long.csv"), row.names = F)

# Exercise 2 ----------------------------

# Step 1: Read the long format data for infrastructure 
infrastructure_long <- read.csv(here("data", "colombia_infrastructure_lng.csv"))

# Step 2: Explore the data 

names(infrastructure_long)
# Step 3: Reshape the data. 
infrastructure_wide <- infrastructure_long %>%
  pivot_wider(id_cols = c("ADM2_PC", "ADM2_ES", "ADM1_PC", "ADM1_ES", "ADM0_PC", "ADM0_ES"), 
              names_from = "amenities", 
              values_from = "value")




write.csv(infrastructure_wide, here("data", "colombia_infrastructure_wide.csv"))



# Challenges  --------------------------

##### Part 1: municipality with more download speed
max_d <- connectivity_long %>%
  filter(quarter == "04") %>%
  group_by(ADM2_ES) %>%
  summarise(avg_d_kbps = mean(avg_d_kbps, na.rm = TRUE))

max_dw <- connectivity_wide %>%
  group_by(ADM2_ES) %>%
  summarise(avg_d_kbps_04 = mean(avg_d_kbps_04, na.rm = TRUE))


# rest of the exercise 

##### Part 2: municipality with more educational institutions 




# rest of the exercise 

