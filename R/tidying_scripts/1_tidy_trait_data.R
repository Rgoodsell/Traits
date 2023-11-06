# tidy body mass data -----------------------------------------------------
# Body mass data for > 700 families taken from Rainford et al. 2016.

# Load required libraries
library(tidyverse)    # Provides a set of packages for data manipulation and visualization
library(janitor)      # Helps with data cleaning tasks
library(readxl)       # Allows reading Excel files
library(VIM)          # Provides functions for handling missing data

# raw data --------------------------------------------------------------------

# cluster taxonomy
IBA_taxa <- read_tsv("../IBA_data_April2023/data/Sweden/CO1_cleaned_nochimera_cluster_taxonomy_SE_2019.tsv") |>
            filter(Phylum == "Arthropoda")

# Read raw body mass data from CSV file and clean the column names
raw_body_mass <- read_csv2("data/raw_data/body_mass_rainford_2016.csv") |> clean_names()

# Read raw feeding niche data from Excel files and clean the column names
raw_feeding_niche_ronquist <- read_xlsx("data/raw_data/feeding_habitat_niche_ronquist_2020_.xlsx") |> clean_names()
raw_feeding_niche_hörren   <- read_xlsx("data/raw_data/larval_niche_feeding_niche_hörren_2022.xlsx") |>  
                              select(-29) |> row_to_names(1) |> clean_names() |> 
                               slice(-587:-602)

# Raw feeding niche from estonian soil arthropods
raw_soil_feeding_niche <- read_xlsx("data/raw_data/soil_feeding_niche_laelatu.xlsx") |> clean_names() |> 
                          select(-x6,-x16 , -all_inclusive) |>
                          filter(str_detect(consensus_taconomical_identity , "Arthropoda")) |> 
                          rename(taxonomy = "consensus_taconomical_identity")



# data filled by expert taxonomists ---------------------------------------

TR_traits <- read_csv2("data/raw_data/taxonomist_assessments/TR_filled_traits.csv") |> clean_names()  # Assessments done by Tomas Roslin
AR_traits <- read_csv2("data/raw_data/taxonomist_assessments/PB_Arachnida_traits.csv") |> clean_names()  # Traits for spiders
CM_traits <-  read_xlsx("data/raw_data/taxonomist_assessments/MV_springtails_mites.xlsx") |> clean_names()  # Traits for collembola and mites    

# body mass data -------------------------------------------------------------

# Process body mass data
tidy_body_mass <- raw_body_mass |> 
                  mutate(length_min = as.numeric(length_min),
                         length_max = as.numeric(length_max),
                         taxon = str_replace_all(taxon, " ", "_")) |> 
                  mutate(higher_taxon = str_extract(taxon, "^([^_]+)"),
                         family = str_extract(taxon, "(?<=_)[^_]+")) |> # Note that some of these classifications are super orders / families
                  select(higher_taxon, family, length_min, length_max, mass_reference = reference) |> 
                 mutate(higher_taxon = str_trim(higher_taxon , "right") , 
                        family       = str_trim(family , "right")) |> 
                  mutate(higher_taxon = recode(higher_taxon  , 
                                               Auchenorrhyncha = "Hemiptera" , 
                                               Sternorrhyncha  = "Hemiptera",
                                               Heteroptera     = "Hemiptera" , 
                                               Dictyoptera     = "Blattodea",
                                               Psocoptera      =  "Psocodea")) |> distinct()



# larval feeding_niche -------------------------------------------------------

# Process feeding niche data from Hörren's dataset
tidy_feeding_niche_hörren <- raw_feeding_niche_hörren |> 
                              select(4:28) |> 
                              mutate_if(is.character, ~as.numeric(.x)) |> 
                              bind_cols(select(raw_feeding_niche_hörren, 1:2)) |> 
                              select(higher_taxon, family, everything()) |> 
                              mutate(higher_taxon = str_trim(higher_taxon , "right") , 
                                     family = str_trim(family , "right")) |> 
                              mutate(family = recode(family, Trogositidae = "Trogossitidae")) |> 
                              mutate(higher_taxon = recode(higher_taxon  , 
                                                           Auchenorrhyncha = "Hemiptera" , 
                                                           Sternorrhyncha  = "Hemiptera",
                                                           Heteroptera     = "Hemiptera" , 
                                                           Dictyoptera     = "Blattodea",
                                                           Psocoptera      =  "Psocodea")) |> 
                              distinct()



# feeding niche ------------------------------------------------------

# Process feeding niche data from Ronquist's dataset
tidy_feeding_niche_ronquist <- raw_feeding_niche_ronquist |>
                                select(higher_taxon = order, 
                                       family = taxon_dyntaxa_2017, 
                                       main_feeding_niche, 
                                       main_feeding_habitat) |> 
                                rowwise() |>
                                mutate(sub_family = str_split(family, "-")[[1]][2],
                                       family     = str_extract(family, "([^-])+")) |> 
                                select(higher_taxon, family, sub_family, 
                                       main_feeding_niche_ronquist = main_feeding_niche, 
                                       main_feeding_habitat_ronquist = main_feeding_habitat) |> 
                                ungroup() |> 
                                mutate(higher_taxon = str_trim(higher_taxon , "right") , 
                                             family = str_trim(family , "right")) |> 
                                mutate(higher_taxon = recode(higher_taxon  , 
                                                             Auchenorrhyncha = "Hemiptera" , 
                                                             Sternorrhyncha  = "Hemiptera",
                                                             Heteroptera     = "Hemiptera" , 
                                                             Dictyoptera     = "Blattodea",
                                                             Psocoptera      =  "Psocodea")) |>
                                distinct()


# soil feeding data -------------------------------------------------------

tidy_feeding_niche_soil <- raw_soil_feeding_niche |> 
                            separate(taxonomy , c("Kingdom" , "Phylum" , "Class" , "Order" , 
                                                  "Family" , "Genus" , "Species")  , ";") |> 
                            filter(!is.na(Family)) |> 
                            pivot_longer(matches("fungi|bacteriv|litter|root|plant|algal|predator|parasit") , names_to = "niche_original") |> 
                            filter(!is.na(value)) |> 
                            filter(Class %in% c("Arachnida" , "Collembola")) |> 
                            mutate(main_feeding_niche_ronquist = 
                                       case_when(str_detect(niche_original , "fungi|bacteriv|litter|root|algal") ~ "Saprophagous", 
                                                 str_detect(niche_original , "plant")                            ~ "Phytophagous",
                                                 str_detect(niche_original , "predator")                         ~  "Predator" , 
                                                 str_detect(niche_original , "parasitic")                        ~  "Parasite")) |> 
    mutate(Family = str_remove(Family , "_environmental_sample")) |> 
    mutate(main_feeding_niche_ronquist = case_when(Family == "Acaridae"         ~ "Saprophagous" , 
                                                   Family == "Heterosminthurus" ~ "Saprophagous" , 
                                                              TRUE              ~ main_feeding_niche_ronquist)) |> 
  select(higher_taxon = Order , family = Family , main_feeding_niche_ronquist)
  

# TR traits ---------------------------------------------------------------

tidy_TR <- TR_traits |> 
            select(-n_otu , -n_obs , -reads , -in_iba) |> 
            mutate(length_min = as.numeric(length_min) , 
                   length_max = as.numeric(length_max)) |> 
            filter(!is.na(main_feeding_niche_ronquist) | !is.na(main_feeding_habitat_ronquist)) |> 
            filter(family != "Ichneumonidae")

# spiders -----------------------------------------------------------------

tidy_AR <- AR_traits |> 
           select(higher_taxon , family , length_min, length_max = lenght_max , matches("main")) 

# mites -------------------------------------------------------------------
# Tidy mite and collembola data

# Body length data comes in categories - switch this to min / max values


tidy_CM  <- CM_traits |> 
                      distinct() |> 
                      select(-x13  , -comment) |> 
                      mutate_at(vars(matches("length_m")) , ~as.numeric(.x)) |> 
                      group_by(higher_taxon , family) |> 
                      mutate(length_min = min(length_min , na.rm = TRUE) , length_max = max(length_max , na.rm = TRUE)) |> 
                      ungroup() |> distinct() |> 
                      pivot_longer(c("length_1" , "length_1_5" , "length_5_10" , "length_10") , names_to = "new_length") |> 
                      drop_na(value) |> 
                      mutate(new_length = str_remove(new_length , "length_")) |> 
                      separate(new_length , into=c("new_length_min" , "new_length_max") , sep = "_") |> 
                      mutate(length_min = case_when(is.infinite(length_min) ~ as.numeric(new_length_min) , TRUE ~ length_min) , 
                             length_max = case_when(is.infinite(length_max) ~ as.numeric(new_length_max) , TRUE ~ length_max)) |>
                      mutate(length_max = case_when(length_min == 1 & is.na(length_max) ~ 1 , TRUE ~ length_max),
                             higher_taxon = str_remove(higher_taxon , ".*,\\s")) |> 
                      select(higher_taxon , family , length_min , length_max , main_feeding_niche_ronquist , main_feeding_habitat_ronquist) |> 
                      group_by(higher_taxon , family) |> 
                      slice(1) |>  # take first of duplicates (?)
                      ungroup() |> distinct()

# check -------------------------------------------------------------------
# View(tidy_body_mass)                 # Visualize tidy body mass data
# View(tidy_feeding_niche_hörren)      # Visualize tidy feeding niche data from Hörren's dataset
# View(tidy_feeding_niche_ronquist)    # Visualize tidy feeding niche data from Ronquist's dataset

# join --------------------------------------------------------------------

# Join all the tidy data frames into one & tidy
all_traits_sub <- full_join(tidy_body_mass, tidy_feeding_niche_hörren, by = c("higher_taxon", "family")) |>
                  full_join(tidy_feeding_niche_ronquist, by = c("higher_taxon", "family"), multiple = "all") |> 
                  full_join(tidy_feeding_niche_soil , by = c("higher_taxon", "family" , "main_feeding_niche_ronquist"), multiple = "all") |> 
              mutate(main_feeding_niche_ronquist = case_when(is.na(main_feeding_niche_ronquist) & saprophagous > .5  ~ "Saprophagous" , 
                                                             is.na(main_feeding_niche_ronquist) & zoophagous   > .5  ~ "Predator"     , 
                                                             is.na(main_feeding_niche_ronquist) & phytophagous > .5  ~ "Phytophagous" , 
                                                             TRUE ~ main_feeding_niche_ronquist) , 
                     main_feeding_habitat_ronquist = case_when(is.na(main_feeding_habitat_ronquist) & larva_aquatic > .5 ~ "Water"  , 
                                                               is.na(main_feeding_habitat_ronquist) & phytophagous  > .5 ~ "Plants" , 
                                                               is.na(main_feeding_habitat_ronquist) & mycetophagous > .5 ~ "Fungi"  , 
                                                               is.na(main_feeding_habitat_ronquist) & coprophagous  > .5 ~ "Temporary habitats"   , 
                                                               is.na(main_feeding_habitat_ronquist) & saproxylic    > .5 & phytophagous >.5 ~ "Wood" , 
                                                               is.na(main_feeding_habitat_ronquist) & roots         > .5 ~ "Soil" , 
                                                               TRUE ~ main_feeding_habitat_ronquist)) |> 
          mutate(main_feeding_niche_ronquist = case_when(sub_family == "Mymarommatidae" ~ "Phytophage-parasitoid" , 
                                                         sub_family == "Pyrgotidae"     ~ "Saprophage-parasitoid" , 
                                                         sub_family == "Eucharitidae"   ~ "Phytophage-parasitoid" , 
                                                         sub_family == "Stephanidae"    ~ "Phytophate-parasitoid" , 
                                                         sub_family == "Mantispidae"    ~ "Predator" , 
                                                         sub_family == "Cryptochetidae" ~ "Phytophate-parasitoid" , 
                                                         sub_family == "Eginiidae"      ~ "Saprophage-parasitoid" , 
                                                         sub_family == "Leucospidae"    ~ "Phytophage-parasitoid" , 
                                                         higher_taxon == "Aranea"       ~ "Predators",
                                                         TRUE ~ main_feeding_niche_ronquist)) |> 
        mutate(higher_taxon = case_when(family == "Isotomidae" ~ "Entomobryomorpha" , TRUE ~ higher_taxon))


# Final back fill of missing body size data
all_traits_s1 <- filter(all_traits_sub , !higher_taxon %in% c("Protura","Raphidioptera","Siphonaptera","Strepsiptera"))
all_traits_s2 <- filter(all_traits_sub ,  higher_taxon %in% c("Protura","Raphidioptera","Siphonaptera","Strepsiptera")) |>  
                  group_by(higher_taxon) |>   
                  fill(length_min , length_max , .direction = "downup") |> ungroup()


# Get all families from IBA 
IBA_families <- select(IBA_taxa ,class = Class, higher_taxon = Order , family = Family) |> distinct()

# final join
all_traits <- bind_rows(all_traits_s2 , all_traits_s1) |>
              full_join(IBA_families) |> 
              mutate(in_IBA = family %in% IBA_taxa$Family) 



# join with expert taxonomist data ----------------------------------------

all_traits <- all_traits |> 
              rows_patch(tidy_TR , by = c("higher_taxon" , "family") , unmatched = "ignore")  |>  
              rows_patch(tidy_CM , by = c("higher_taxon" , "family") , unmatched = "ignore") |> 
              rows_patch(tidy_AR , by = c("higher_taxon" , "family") , unmatched = "ignore")


# save --------------------------------------------------------------------

# Save the combined data frame 
write.csv(all_traits, "data/tidydata/all_traits.csv")


# extra stuff ---------------------------------------------------------------------------------


# select families with missing life-history stage data --------------------

missing_aq_ter <- select(all_traits , matches(c("higher_taxon|^family|terrestrial|aquatic|in_IBA"))) |> 
                  filter(in_IBA)

write.csv(missing_aq_ter , "data/tidydata/missing_lh.csv")


# Inspect missingness -----------------------------------------------------

# Visualize missingness of variables in the combined data frame
dev.new()
all_traits |> select(-matches("reference|family|taxon|sub_family|in_IBA")) |> 
  VIM::aggr(., cex.axis = 1, combined = TRUE)


# Number of families in complete cases:
all_traits |> select(-sub_family) |> drop_na() |> pull(family) # 493 Families with complete trait data. 

# Filter the complete cases
complete_traits <- filter(all_traits) |> drop_na(-sub_family)






