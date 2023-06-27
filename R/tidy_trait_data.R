# tidy body mass data -----------------------------------------------------
# Body mass data for > 700 families taken from Rainford et al. 2016.

# Load required libraries
library(tidyverse)    # Provides a set of packages for data manipulation and visualization
library(janitor)      # Helps with data cleaning tasks
library(readxl)       # Allows reading Excel files
library(VIM)          # Provides functions for handling missing data

# FinBIF API for taxa names

# data --------------------------------------------------------------------

# Read raw body mass data from CSV file and clean the column names
raw_body_mass <- read_csv2("data/raw_data/body_mass_rainford_2016.csv") %>% clean_names()

# Read raw feeding niche data from Excel files and clean the column names
raw_feeding_niche_ronquist <- read_xlsx("data/raw_data/feeding_habitat_niche_ronquist_2020_.xlsx") %>% clean_names()
raw_feeding_niche_hörren <- read_xlsx("data/raw_data/larval_niche_feeding_niche_hörren_2022.xlsx") %>%  
  select(-29) %>% row_to_names(1) %>% clean_names()

# body mass data -------------------------------------------------------------

# Process body mass data
tidy_body_mass <- raw_body_mass %>% 
  mutate(length_min = as.numeric(length_min),
         length_max = as.numeric(length_max),
         taxon = str_replace_all(taxon, " ", "_")) %>% 
  mutate(higher_taxon = str_extract(taxon, "^([^_]+)"),
         family = str_extract(taxon, "(?<=_)[^_]+")) %>% # Note that some of these classifications are super orders / families
  select(higher_taxon, family, length_min, length_max, mass_reference = reference)

# larval feeding_niche -------------------------------------------------------

# Process feeding niche data from Hörren's dataset
tidy_feeding_niche_hörren <- raw_feeding_niche_hörren %>% 
  select(3:28) %>% 
  mutate_if(is.character, ~as.numeric(.x)) %>% 
  bind_cols(select(raw_feeding_niche_hörren, 1:2)) %>% 
  select(higher_taxon, family, everything()) %>% 
  select(-sum_sp)

# feeding niche ------------------------------------------------------

# Process feeding niche data from Ronquist's dataset
tidy_feeding_niche_ronquist <- raw_feeding_niche_ronquist %>%
  select(higher_taxon = order, 
         family = taxon_dyntaxa_2017, 
         main_feeding_niche, 
         main_feeding_habitat) %>% 
  rowwise() %>%
  mutate(sub_family = str_split(family, "-")[[1]][2],
         family     = str_extract(family, "([^-])+")) %>% 
  select(higher_taxon, family, sub_family, main_feeding_niche, main_feeding_habitat) %>% 
  ungroup()

# metabolic trait data ----------------------------------------------------

# Note: The script does not have metabolic trait data for insects, only body size for a few families already recorded in Rainford 2016.

# check -------------------------------------------------------------------
View(tidy_body_mass)                 # Visualize tidy body mass data
View(tidy_feeding_niche_hörren)      # Visualize tidy feeding niche data from Hörren's dataset
View(tidy_feeding_niche_ronquist)    # Visualize tidy feeding niche data from Ronquist's dataset

# join --------------------------------------------------------------------

# Join all the tidy data frames into one
all_traits <- full_join(tidy_body_mass, tidy_feeding_niche_hörren, by = c("higher_taxon", "family")) %>%
  full_join(tidy_feeding_niche_ronquist, by = c("higher_taxon", "family"), multiple = "all")

# Save the combined data frame as an RDS file
saveRDS(all_traits, "data/tidydata/all_traits.rds")

# Inspect missingness -----------------------------------------------------

# Visualize missingness of variables in the combined data frame
dev.new()
all_traits %>% select(-matches("reference|family|taxon|sub_family")) %>% 
  VIM::aggr(., cex.axis = 1, combined = TRUE)

# Number of families in complete cases:
all_traits %>% select(-sub_family) %>% drop_na() %>% pull(family) # 433 Families with complete trait data. 

# Filter the complete cases
complete_traits <- filter(all_traits) %>% drop_na(-sub_family)
