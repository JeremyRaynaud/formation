library("readr")
library("tidyverse")
library("FUNGuildR")

data <- read_rds("data/data_raw/bosque_fung_annot_clean_agg.rds")

motus_df <- data$motus |> 
  rename(Taxonomy = `Full classification`) |> 
  funguild_assign() |> 
  select(c(lineage_dnabarcoder, 79:89)) |> 
  mutate(trophicMode = str_trim(colonne_texte, side = "left"))

table(motus_df$trophicMode)
write.csv(motus_df, "data/data_modified/motus_bosque_fung_annot_clean_agg_func.csv")
