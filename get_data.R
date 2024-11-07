library("readr")
library("tidyverse")
library("FUNGuildR")


occ_colombia <- rgbif::occ_search(country = "CO",
                                 elevation = '0,1500',
                                 kingdomKey = 5,
                                 year = '2023,2024',
                                 limit = 30000)
view(occ_colombia$data)

data <- read_rds("data/data_raw/bosque_fung_annot_clean_agg.rds")

motus_df <- data$motus |> 
  rename(Taxonomy = `Full classification`) |> 
  funguild_assign() |> 
  select(c(lineage_dnabarcoder, 79:89)) |> 
  mutate(trophicMode = str_trim(trophicMode, side = "left")) |> 
  unique()

table(motus_df$trophicMode)
write.csv(motus_df, "data/data_modified/motus_bosque_fung_annot_clean_agg_func.csv")

unique(motus_df$taxon)
