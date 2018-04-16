library(shiny)
library(ggplot2)
library(dplyr)

survey <- readRDS("data/survey.rds")
survey_factors <- select(survey, Gender:obs_consequence)

# user interface ----
ui <- fluidPage(
    titlePanel("Mental Health in Tech Survey 2014"),
    
    fluidRow(
        column(width = 4,
               selectInput("col_1", 
                           label = "Variable to display",
                           choices = names(survey_factors))),
        column(width = 3, offset = 1,
               selectInput("twocol_1",
                           label = "Variable on x-axis",
                           choices = names(survey_factors))),
        column(width = 3,
               selectInput("twocol_2",
                           label = "Variable by colour",
                           choices = names(survey_factors)))
    ),
    
    fluidRow(
        column(width = 4,
               plotOutput("barplot")),
        
        column(width = 6, offset = 1,
               plotOutput("mosaicplot"))
    ),
    
    fluidRow(
      column(width = 3, offset = 1,
             tableOutput("table1d")),
      column(width = 6, offset = 2,
             tableOutput("table2d"))
    )
)

# server logic ----
server <- function(input, output){
    output$barplot <- renderPlot(
        ggplot(survey, aes_string(input$col_1)) +
            geom_bar()
    )
    
    output$mosaicplot <- renderPlot(
        ggplot(survey, aes_string(input$twocol_1)) +
            geom_bar(aes_string(fill = input$twocol_2))
    )
    
    output$table1d <- renderTable(
        table(survey[[input$col_1]])
    )
    
    output$table2d <- renderTable({
        table(survey[[input$twocol_1]],
              survey[[input$twocol_2]]) %>% 
            addmargins() %>%
            as.data.frame.matrix()},
        include.rownames = TRUE
    )
}

# Run app ----
shinyApp(ui, server)