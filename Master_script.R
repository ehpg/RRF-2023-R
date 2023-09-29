# Master script


# load all required packages
install.packages("pacman")

packages <- c("tidyr", 
              "dplyr", 
              "ggplot2", 
              "corrplot", 
              "gt", 
              "plm", 
              "lmtest", 
              "here", 
              "labelled", 
              "stringi", 
              "Hmisc", 
              "stringi", 
              "stargazer")

pacman::p_load(packages,
               character.only = TRUE,
               install = TRUE) # Change to TRUE to install the necessary packages


# run tidying script
# inputs:
# outputs:
source(here("scripts", "Template-R-01-tidying-secondary.R"))


# run cleaning script
# inputs:
# outputs:
source(here("scripts", "Template-R-02-cleaning-secondary.R"))


# run construction script
# inputs:
# outputs:
source(here("scripts", "Template-R-03-construction-secondary.R"))


# run analysis script
# inputs:
# outputs:
source(here("scripts", "Template-R-04-analysis-secondary.R"))

