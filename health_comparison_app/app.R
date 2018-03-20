library(shiny)
library(shinymaterial)
library(leaflet)
library(tidyverse)

health_data <- readRDS("./data/infections_hosp_tidy.rds")

# Wrap shinymaterial apps in material_page
ui <- material_page(
        title = "Comparison of hospital related infections",
        nav_bar_fixed = TRUE,
        nav_bar_color = "blue",
        # Place side-nav in the beginning of the UI
        material_side_nav(
                fixed = TRUE,
                # Place side-nav tabs within side-nav
                material_side_nav_tabs(
                        side_nav_tabs = c(
                                "Example Side-Nav Tab 1" = "example_side_nav_tab_1",
                                "Example Side-Nav Tab 2" = "example_side_nav_tab_2"
                        ),
                        icons = c("cast", "insert_chart")
                )
        ),
        # Define side-nav tab content
        material_side_nav_tab_content(
                side_nav_tab_id = "example_side_nav_tab_1",
                tags$br(),
                material_row(material_column(width = 3,
                        material_dropdown(input_id = "state",
                                                label = "State:",
                                                choices = sort(unique(health_data$state)),
                                                color = "blue",
                                          multiple = TRUE)),
                        material_column(width = 5,
                                        material_dropdown(
                                                input_id = "procedure",
                                                label = "Procedure:",
                                                choices = list("Clostridium difficile (C.diff.) intestinal infections" = "c_diff_observed",
                                                               "Catheter-associated urinary tract infections (CAUTI)" = "cauti_observed",
                                                               "Surgical site infections (SSI) from colon surgery" = "ssi_colon_observed",
                                                               "Surgical site infections (SSI) from abdominal hysterectomy" = "ssi_abdom_observed",
                                                               "Methicillin-resistant Staphylococcus Aureus (MRSA) blood infections" = "mrsa_observed",
                                                               "Central line-associated bloodstream infections (CLABSI)" = "clabsi_observed"),
                                                color = "red",
                                                multiple = TRUE
                                                
                                        ))
                ),
                        
                material_card(
                        title = "Complications with infections by state",
                        leafletOutput(outputId = "map", width = "100%", height = 400),
                        depth = 2
                )
        ),
        material_side_nav_tab_content(
                side_nav_tab_id = "example_side_nav_tab_2",
                tags$h1("Second Side-Nav Tab Content"),
                absolutePanel(),
                depth = 2
        )
)

server <- function(input, output, session) {
        
        
        ## Map output
        
        ## Get reactive output
        
        map_data <- health_data %>% filter(state %in% input$state) %>% 
                filter(observed %in% input$procedure) %>% group_by(hospital.name, observed, latitude, longitude) %>% 
                summarise(sum = sum(amount))
        
        #create a pop up (onClick)
        popup <- paste0("<strong>Name: </strong>", map_data$hospital.name, "<br>",
                        "<strong>Incidences: </strong>", map_data$sum)
        
        colors <- case_when(test$sum <= (mean(map_data$sum) - 2) ~ "green",
                            test$sum == mean(map_data$sum) - 1 | mean(map_data$sum) + 1 ~ "yellow",
                            test$sum >= (mean(map_data$sum) + 2) ~ "red")
        
        output$map <- renderLeaflet({
        
                
                leaflet() %>% 
                        addProviderTiles("CartoDB.Positron") %>% 
                        setView(-98.35, 39.7,
                                zoom = 4)

        })
        
        
}
shinyApp(ui = ui, server = server)