library(shiny)
library(tidyverse)
library(sf)
library(leaflet)

# UI ----
ui <- fluidPage(
  titlePanel("Exploration des données d'occurrences en Colombie"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("rank", "Rang taxonomique pour correspondance :", 
                  choices = c("kingdom", "phylum", "order", "family", "genus"), 
                  selected = "phylum"),
      actionButton("match_btn", "Mettre à jour les correspondances"),
      hr(),
      checkboxInput("show_matched", "Afficher uniquement les occurrences appariées", value = FALSE)
    ),
    
    mainPanel(
      leafletOutput("map"),
      tableOutput("summary_table")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  # Charger les données initiales ----
  occ_data <- reactive({
    list_occ_colombia <- list(read.csv("data/data_raw/occ_fungi_col/gbif_colomb_2001.csv"))
    
    for (i in 1:24) {
      list_occ_colombia[[i + 1]] <- read.csv(
        str_c("data/data_raw/occ_fungi_col/gbif_colomb_", as.character(2001:2024)[i], ".csv")
      )
    }
    
    occ_colombia <- bind_rows(list_occ_colombia)
    occ_colombia$Taxonomy <- str_c(
      occ_colombia$kingdom, occ_colombia$phylum, occ_colombia$order, 
      occ_colombia$family, occ_colombia$genus, occ_colombia$acceptedScientificName, sep = "_"
    )
    occ_colombia
  })
  
  assign_data <- reactive({
    tab_assign <- read.csv("data/data_raw/tab_otu_func.csv")
    tab_assign$Pred_no_species <- str_split_fixed(tab_assign$Prediction, " ", n = 2)[, 1]
    tab_assign$match_gbif_taxa <- "None"
    tab_assign$match_gbif_rank <- "None"
    tab_assign
  })
  
  # Correspondance réactive ----
  observeEvent(input$match_btn, {
    occ_colombia <- occ_data()
    tab_assign <- assign_data()
    
    for (i in 1:nrow(tab_assign)) {
      for (r in input$rank) {
        if (any(str_detect(occ_colombia[[r]], tab_assign$Pred_no_species[i]), na.rm = TRUE)) {
          tab_assign$match_gbif_rank[i] <- r
          match_gbif <- occ_colombia[[r]][str_detect(occ_colombia[[r]], tab_assign$Pred_no_species[i])]
          tab_assign$match_gbif_taxa[i] <- match_gbif[!is.na(match_gbif)][1]
        }
      }
    }
    
    occ_colombia$matched_ray <- FALSE
    for (i in 1:nrow(tab_assign)) {
      if (tab_assign$match_gbif_taxa[i] != "None") {
        rk <- tab_assign$match_gbif_rank[i]
        tx <- tab_assign$match_gbif_taxa[i]
        occ_colombia$matched_ray[occ_colombia[[rk]] == tx] <- TRUE
      }
    }
    
    assign("occ_colombia", occ_colombia, envir = .GlobalEnv)
    assign("tab_assign", tab_assign, envir = .GlobalEnv)
  })
  
  # Visualisation des données ----
  output$map <- renderLeaflet({
    req(occ_data())
    
    occ_colombia <- get("occ_colombia", envir = .GlobalEnv)
    
    if (input$show_matched) {
      occ_colombia <- occ_colombia |> filter(matched_ray)
    }
    
    col_borders <- sf::st_read("data/data_raw/gadm41_COL_0.json") |> 
      sf::st_transform(crs = 4326)
    
    occ_colombia_coord <- occ_colombia |>
      sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove = FALSE)
    
    leaflet() |>
      addTiles() |>
      addPolygons(data = col_borders, color = "blue", weight = 2, fill = NA) |>
      addCircleMarkers(data = occ_colombia_coord, 
                       lng = ~decimalLongitude, 
                       lat = ~decimalLatitude, 
                       color = ~phylum, 
                       radius = 3,
                       popup = ~paste("Phylum:", phylum))
  })
  
  output$summary_table <- renderTable({
    req(assign_data())
    tab_assign <- get("tab_assign", envir = .GlobalEnv)
    tab_assign |> select(Pred_no_species, match_gbif_taxa, match_gbif_rank)
  })
}

# Application ----
shinyApp(ui, server)
