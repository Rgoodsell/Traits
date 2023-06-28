# Missing traits versus read count & sample count -------------------------

# To do list
# Table of missing traits vs reads + observations
# Count occurrences of non insects (spring tails, spiders, etc)

library(tidyverse)

# data --------------------------------------------------------------------


all_traits       <- read_csv("data/tidydata/all_traits.csv")
IBA_taxa         <- read_tsv("../IBA_data_April2023/data/Sweden/CO1_cleaned_nochimera_cluster_taxonomy_SE_2019.tsv") %>% rename(cluster = 1)
IBA_clusters     <- read_tsv("../IBA_data_April2023/data/Sweden/CO1_cleaned_nochimera_cluster_counts_SE_2019.tsv")
IBA_seq_meta     <- read_tsv("../IBA_data_April2023/data/Sweden/CO1_sequencing_metadata_SE_2019.tsv") 
IBA_malaise_meta <- read_tsv("../IBA_data_April2023/data/Sweden/malaise_samples_metadata_SE_2019.tsv") 


# Assemble seq data
taxa_counts <- full_join(IBA_taxa , IBA_clusters , by = "cluster") %>% 
                pivot_longer(11:ncol(.) , names_to = "sampleID_LAB" , values_to = "read_count") %>% 
                filter(read_count > 0)

# Assemble Meta data 
full_meta <- full_join(IBA_seq_meta , IBA_malaise_meta) %>% select(matches("sampleID|trap"))

# Add together
all_data <- full_join(taxa_counts , full_meta) 

# sample counts and read numbers by families -------------------------------------

family_reads <- all_data %>% 
                filter(read_count >0) %>% 
                group_by(Family) %>% 
                summarise(n_otu = length(unique(cluster)) , 
                          n_obs = length(unique(trapID)),
                          reads = sum(read_count))


# families by missing traits
missing_traits <- all_traits %>% select(higher_taxon , Family=family , 
                                        length_min , length_max , 
                                        main_feeding_niche_ronquist , main_feeding_habitat_ronquist, 
                                        in_IBA) %>% 
                      filter_at(vars(matches("main|length")),any_vars(is.na(.)))

                      mutate(n_missing = rowSums(is.na(.))) %>% 
                      select(higher_taxon , Family = family , n_missing)

# Combine                      
traits_obs <- full_join(missing_traits , family_reads) %>% filter(in_IBA)

# plot
aggr(traits_obs %>%  select(matches("length|main")) , combined = TRUE , oma = c(15,3,3,3))

# table of number of missing families by order, and total number of missing traits
missing_trait_sum <- missing_traits %>% 
                      mutate(n_missing = rowSums(is.na(.))) %>% 
                      select(higher_taxon , Family , n_missing) %>% 
                       filter(n_missing > 0) %>% 
                       group_by(higher_taxon) %>% 
                       summarise(n_missing_families = n() , total_missing_traits = sum(n_missing)) %>% 
                       arrange(-n_missing_families)

# save --------------------------------------------------------------------

# missing traits by family & sequencing info
write.csv(traits_obs , "data/tidydata/missing_traits_seq.csv")

# number of missing traits by family
write.csv(missing_traits_sum , "data/tidydata/missing_traits_summary.csv")


