# format community data -----------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(lubridate)
library(taxizedb)
library(data.table)

# load community data -----------------------------------------------------------------------------------

# OTU data
IBA_taxa         <- fread("../IBA_data_April2023/data/Sweden/results_October2023/consensus.SE.taxonomy.tsv")  |> clean_names()
IBA_clusters     <- fread("../IBA_data_April2023/data/Sweden/results_October2023/CO1_lysate_2019_SE.cleaned.clusters.counts.tsv") |> clean_names()
IBA_seq_meta     <- fread("../IBA_data_April2023/data/Sweden/results_October2023/metadata_files/CO1_sequencing_metadata_SE.tsv")  |> clean_names()
IBA_malaise_meta <- fread("../IBA_data_April2023/data/Sweden/results_October2023/metadata_files/samples_metadata_malaise_SE_2019.tsv") |> clean_names()
IBA_trap_meta    <- fread("../IBA_data_April2023/data/Sweden/results_October2023/metadata_files/traps_metadata_SE_2019.tsv") |> clean_names()


# functions -----------------------------------------------------------------------------------

# function to retrieve family to genus level information 
get_sf <- function(class_tr_sub){
  try(class_tr_sub |> 
        filter(rank %in% c("family", "subfamily" , "genus")) |> select(-id) |> distinct() |> 
        pivot_wider(names_from = rank , values_from = name) )
}

# tidy data -----------------------------------------------------------------------------------


# Assemble seq data
taxa_counts <- full_join(IBA_taxa , IBA_clusters , by = "cluster") |> 
                melt(id.vars = 1:9 , variable.name = "sample_id_ngi" , value.name = "read_count") |> 
                filter(read_count > 0 , phylum == "Arthropoda")|> 
                 mutate(sample_id_ngi = str_replace(sample_id_ngi , "p" , "P"))


# Assemble Meta data 
full_meta <- full_join(IBA_seq_meta , IBA_malaise_meta) %>%
             full_join(IBA_trap_meta) |> 
              select(matches("sample_id_ngi|trap|type|date|lat|lon")) %>% 
              mutate(week_year = week(as.Date(collecting_date ,format = "%d/%m/%Y")))

# Add together
OTU_data <- full_join(taxa_counts , full_meta) 


# load trait data -----------------------------------------------------------------------------

# trait data
feeding_traits <- read_csv("data/tidydata/all_traits.csv") %>% 
  select(family , subfamily=sub_family, feeding_niche = main_feeding_niche_ronquist) %>% 
  drop_na(family) %>% distinct() %>% 
  mutate()


# get sub-family level information ------------------------------------------------------------

# Retrieve full classification info for organisms with sub-family info
families_tr <- c("Braconidae" , "Ichneumonidae" , "Staphylinidae")
# tax_tr      <- filter(OTU_data , family %in% families_tr) |> ungroup()
# ftr_genus   <- tax_tr$genus %>% unique()
# tr_id       <- taxize::get_ids(ftr_genus , db="ncbi")
# class_tr    <- taxizedb::classification(tr_id$ncbi , db="ncbi")
# 
# # save classifications
# saveRDS(class_tr , "data/tidydata/subfamily_classifications.rds")


# get subfamily level information
class_tr <- readRDS("data/tidydata/subfamily_classifications.rds")
subfamilies <- sapply(seq_along(class_tr) , function(x) get_sf(class_tr[[x]])) 
subfamilies <- subfamilies[sapply(subfamilies, function(x) !inherits(x, "try-error"))] |> bind_rows()


# add in subfamily info into OTU table --------------------------------------------------------

# get portion of OTU table with subfamily info
OTU_subfamily <- OTU_data |> 
                  filter(family %in% families_tr) |> 
                  left_join(subfamilies , by = c("family" , "genus")) 

# get remainder
OTU_remainder <- OTU_data |> 
                  filter(!family %in% families_tr) |> 
                  mutate(sub_family = NA)

# join ----------------------------------------------------------------------------------------

# join OTU and trait data
OTU_subfamily_traits <- OTU_subfamily |> left_join( feeding_traits , by = c("family" , "subfamily")) # subfamily level
OTU_rm_traits        <- OTU_remainder |> left_join( feeding_traits , by = "family") # the remainder

# bind and save -------------------------------------------------------------------------------

all_data <- bind_rows(OTU_rm_traits , OTU_subfamily_traits) 

saveRDS(all_data , "data/tidydata/OTU_trait_data.rds")


# species richness data -----------------------------------------------------------------------

nicheData <- all_data %>% 
                filter(class == "Insecta" , 
                       lab_sample_type == "sample") |> 
                group_by(feeding_niche, week_year, trap_id) |> 
                summarise(n_OTU = n_distinct(cluster)) |>  
                drop_na(feeding_niche) |>  
                mutate(feeding_niche = factor(feeding_niche))  |>  
                filter(!str_detect(feeding_niche,"General|Parasite")) |> 
                mutate(feeding_niche = factor(feeding_niche  , 
                                              levels = c("Phytophagous" , "Predator" , "Saprophagous" , 
                                                         "Phytophage-parasitoid" , "Predator-parasitoid" , "Saprophage-parasitoid"))) |> 
                ungroup()


saveRDS(nicheData , "data/tidydata/niche_SR_data.rds")


