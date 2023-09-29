# Cleaning data - R - secondary sources - template
# Load necessary packages --------------------------




# Exercise 1 part 1 ---------------------------------------

# Step 0: Read data
# Read the connectivity data from your CSV file
connectivity <- read.csv(here("data", "colombia_connectivity_decleaned.csv"), encoding = "UTF-8")

connectivity <- connectivity %>%
  select(-id_test_data)

# Step 1: Remove duplicate entries
# Identify and remove duplicate rows in your data
connectivity <- connectivity %>%
  distinct()

# Step 2: Ensure there is at least one identifying variable in the data
# Verify that your data has at least one identifying variable
test_id <- connectivity %>%
  group_by(quadkey, ADM2_PC) %>%
  distinct()
# still 79950 observations so uniquely identified by these two variables


# Step 3: Encode choice questions and ensure correct data types
# Check data types of different columns and modify them if necessary
str(connectivity)


# Step 4: Handle missing values
# Handle rows and columns with missing values in a way that suits your analysis
connectivity_clean <- connectivity %>%
  rowwise() %>%
  filter(!all(is.na(c(avg_d_kbps, avg_u_kbps, avg_lat_ms, tests, devices)))) %>%
  ungroup()


# Step 5: Drop data collection metadata variables not needed for analysis
# Remove unnecessary columns from your dataset


# Step 6: Ensure all variables have English names and no special characters
# Standardize the column names by removing special characters
connectivity_translate <- connectivity_clean %>%
  mutate(ADM1_ES = stringi::stri_trans_general(ADM1_ES, id = "Latin-ASCII"), 
         ADM2_ES = stri_trans_general(ADM2_ES, id = "Latin-ASCII"))


# Step 7: Adding variable labels
# Add descriptive labels to your variables with a maximum of 80 characters each
var_label(connectivity_clean) <- list(
  quadkey = "Tile ID", 
  ADM0_PC = "Country ID", 
  ADM0_ES = "Country name",
  ADM1_PC = "Admin level 1 ID", 
  ADM1_ES = "Admin level 1 name",
  ADM2_PC = "Admin level 2 ID", 
  ADM2_ES = "Admin level 2 name",
  connection = "Connection type", 
  trimester = "Quarter of the year",
  avg_d_kbps = "Average download speed in kilobites per second",
  avg_u_kbps = "Average upload speed in kilobites per second",
  avg_lat_ms = "Average latency in milliseconds", 
  tests = "The number of tests taken", 
  devices = "The number of unique devices")  

  
# Exercise 1 part 2 ---------------------------------------

# Metadata
# Step 0: Get data type and labels for each column
# Get the class/type and labels of each column in your cleaned data


# Step 1: Create a data frame for the codebook
# Create a codebook data frame using the info gathered in the previous step
metadata <- as.data.frame(cbind(
  "Variable name" = as.character(names(connectivity_clean)), 
  "Variable label" = as.character(var_label(connectivity_clean)),
  "Variable type" = as.character(sapply(connectivity_clean, class)) 
))


# Step 2: Save the cleaned data and the codebook
# Save your cleaned data and the codebook as CSV files
write.csv(connectivity_clean, here("data", "connectivity_clean.csv"))
write.csv(metadata, here("data", "codebook.csv"), row.names = F)










