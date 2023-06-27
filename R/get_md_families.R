# Get malagasy families for brian
library(tidyverse)

# Families for 20% of the MD data
cluster_taxonomy <- read_tsv("../IBA_data_April2023/data/Madagascar/CO1_cleaned_nochimera_cluster_taxonomy_MG_2020.tsv") %>% 
  rename(cluster = 1)

families_MD <- as.data.frame(table(cluster_taxonomy$Family)) %>% 
  rename(Family = 1 , n_clusters = Freq)

write.csv(families_MD , "data/families_MD.csv")

