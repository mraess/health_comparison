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
                                          multiple = FALSE)),
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
                                                multiple = FALSE
                                                
                                        ))
                ),
                        
                material_card(
                        title = "Complications with infections by state",
                        plotOutput(outputId = "map"),
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
        
        
        output$map <- renderPlot({
                
                health_data %>% filter(state == input$state) %>% mutate(state = as.factor(state)) %>% 
                        filter(observed == input$procedure) %>% group_by(hospital.name, state, observed) %>% 
                        summarise(sum = sum(amount)) %>%  
                        ggplot(aes(hospital.name, sum - mean(sum))) + 
                        geom_col() + 
                        ylab("Observed incidences - difference to state-mean") +
                        coord_flip() +
                        theme(axis.title.y = element_blank())
                        
        })
        
        
}
shinyApp(ui = ui, server = server)