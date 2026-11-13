#### Avant de commencer ####  

# import des packages ----
library(shiny)
library(tidyverse)
library(bslib)
library(plotly)

# import des données ----
## données depuis le site ODRE : https://opendata.reseaux-energies.fr/
donnees_odre <- read_delim(
  "https://odre.opendatasoft.com/api/explore/v2.1/catalog/datasets/registre-national-installation-production-stockage-electricite-agrege/exports/csv?lang=fr&refine=regime%3A%22En%20service%22&facet=facet(name%3D%22regime%22%2C%20disjunctive%3Dtrue)&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B",
  col_select = c(region, filiere, tensionRaccordement, puisMaxInstallee)
) |> 
  drop_na(region) |> 
  group_by(
    region,
    filiere,
    tensionRaccordement
  ) |> 
  summarise(
    puissance_max_cumulee = sum(puisMaxInstallee)/1000
  ) |> 
  ungroup()

## création du fond de carte
fond_de_carte <-
  map_data("france") |> 
  left_join(
    read_delim(
      "https://www.data.gouv.fr/api/1/datasets/r/987227fb-dcb2-429e-96af-8979f97c9c84"
    ) |> 
      mutate(
        nom_dept = dep_name |> 
          str_replace_all("[éèëê]", "e") |> 
          str_replace_all("[ôö]", "o") |> 
          str_replace("d'O", "Do") |> 
          str_replace("d'A", "Da") |> 
          str_replace("Corse-du-Sud", "Corse du Sud")
      ),
    by = join_by("region" == "nom_dept")
  ) |> 
  select(long, lat, group, region = region_name)

#### Partie UI ####   
ui <- page_sidebar(
  title = "Puissance max installée en fonction des filières et des tensions de raccordement",
  sidebar = 
    sidebar(
      width = 600,
      radioButtons(
        "filiere",
        "Choix d'une filière",
        choices = donnees_odre |> 
          count(filiere) |> 
          pull(filiere)
      ),
      selectizeInput(
        "tension",
        "Choix d'une ou plusieurs tension de raccordement",
        choices = donnees_odre |> 
          count(tensionRaccordement) |> 
          pull(tensionRaccordement),
        multiple = TRUE
      )
    ),
  plotlyOutput("carte")
)

#### Partie SERVER ####  
server <- function(input, output, session) {
  
  output$carte <- renderPlotly(
    full_join(
      fond_de_carte,
      donnees_odre |> 
        filter(
          filiere == input$filiere,
          tensionRaccordement %in% input$tension
        ),
      by = join_by("region" == "region")
    ) |> 
      ggplot() +
      aes(
        x = long,
        y = lat,
        group = group,
        fill = puissance_max_cumulee
      ) +
      geom_polygon(color = "black") +
      labs(
        title = 
          paste("Tension de raccordement : ", input$tension |> str_c(collapse = ' / '), " \n Filière : ", input$filiere)
      ) +
      theme_void() +
      scale_fill_gradient(low = "pink", high = "darkblue")
  )
  
}

#### Pour lancer l'appli (A NE PAS SUPPRIMER) ####  
shinyApp(ui, server)