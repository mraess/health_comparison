library(shiny)
library(shinymaterial)

health_data <- readRDS("./data/infections_hosp_tidy.rds")

# Wrap shinymaterial apps in material_page
ui <- material_page(
        title = "Comparison of hospital related infections",
        nav_bar_fixed = TRUE,
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
                material_card(
                        title = 
                                "Complications with infections by state",
                        plotOutput("complications_state"), 
                        depth = 2
                )
        ),
        material_side_nav_tab_content(
                side_nav_tab_id = "example_side_nav_tab_2",
                tags$h1("Second Side-Nav Tab Content")
        )
)

server <- function(input, output, session) {
        
        output$complications_state <- renderPlot({
                
                #--- Show the spinner ---#
                material_spinner_show(session, "complications_state")
                
                #--- Simulate calculation step ---#
                Sys.sleep(time = 5)
                
                #--- Hide the spinner ---#
                material_spinner_hide(session, "complications_state")
                
                health_data %>% group_by(state, observed) %>% summarise(n = sum(amount)) %>% 
                        
                        ggplot(aes(reorder(state, n), n, fill = observed)) + geom_col() + coord_flip()
                

        })
        
        
}
shinyApp(ui = ui, server = server)