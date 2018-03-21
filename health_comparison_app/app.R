library(shiny)
library(shinymaterial)
library(leaflet)
library(tidyverse)
library(RColorBrewer)

health_data <- readRDS("./data/infections_hosp_tidy.rds")

# Wrap shinymaterial apps in material_page
ui <- material_page(
        title = "Comparison of hospital related infections for 2016-2017",
        nav_bar_fixed = TRUE,
        nav_bar_color = "blue",
        # Place side-nav in the beginning of the UI
        material_side_nav(
                fixed = TRUE,
                # Place side-nav tabs within side-nav
                material_side_nav_tabs(
                        side_nav_tabs = c(
                                "Explore the data" = "nav1",
                                "Data source" = "nav2"
                        ),
                        icons = c("insert_chart", "explore")
                )
        ),
        # Define side-nav tab content
        material_side_nav_tab_content(
                side_nav_tab_id = "nav1",
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
                        
                        plotOutput(outputId = "map"),
                        depth = 2
                )
        ),
        material_side_nav_tab_content(
                side_nav_tab_id = "nav2",
                tags$br(),
                material_row(
                        material_column(
                                width = 10,
                                material_card(
                                        title = "Data source",
                                        tags$a(href = "https://data.medicare.gov/data/hospital-compare",
                                               target = "_blank",
                                               "Medicare")
                                )
                        )
                ),
                depth = 2
        )
)

server <- function(input, output, session) {
        
        
        output$map <- renderPlot({
                
                #--- Show the spinner ---#
                material_spinner_show(session, "map")
                
                plot_data <- health_data %>% filter(observed == input$procedure) %>% 
                        mutate(nat_mean = mean(amount, na.rm = TRUE)) %>% 
                        filter(state == input$state) %>%
                        mutate(state = as.factor(state),
                               state_mean = mean(amount),
                               trend = ifelse(amount - state_mean > 0, "negative", "positive")) %>% 
                        group_by(hospital.name, state, observed, nat_mean, trend)
                
                
                plot <- ggplot(plot_data, aes(hospital.name, amount - state_mean, fill = trend)) + geom_col() + 
                        geom_hline(aes(yintercept = nat_mean), col = "red", alpha = .5, size = 1.5) + 
                        geom_hline(aes(yintercept = 0), col = "blue", size = 1.5, alpha = 2/3) +
                        scale_fill_brewer(guide = FALSE, type = "qual", palette = 4)+
                        coord_flip() +
                        theme(axis.title.y = element_blank()) +
                        labs(caption = paste("Red line = national mean:", round(plot_data$nat_mean, 2),"\n", 
                                        "Blue line = state mean:", round(plot_data$state_mean, 2)), 
                             y = "Observed incidences - difference to state-mean",
                             title = paste("Complications with infections for:", input$state))
                
                
                #--- Simulate calculation step ---#
                Sys.sleep(time = 2)
                
                #--- Hide the spinner ---#
                material_spinner_hide(session, "map")
                
                plot
                        
        })
        
        
}
shinyApp(ui = ui, server = server)