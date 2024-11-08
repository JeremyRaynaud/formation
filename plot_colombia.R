download.file("https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_COL_0.json", # url téléchargement
              destfile = "data/data_raw/gadm41_COL_0.json", # url destination
              mode = "wb") 

col_borders <- sf::st_read("data/data_raw/gadm41_COL_0.json") |> 
  sf::st_transform(crs = 4326)

ggplot(col_borders)+
  geom_sf()
