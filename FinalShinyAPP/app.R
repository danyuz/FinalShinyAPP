library(shiny)
library(ggplot2)
library(devtools)

## packages required to use
library(tidyverse)
library(shinythemes)
library(plotly)
library(shinyjs)

data <- read.csv("ramen-ratings.csv", header = TRUE)

# Replacing unrated data with minimum of ratings
data$Stars[data$Stars == 'Unrated'] <- min(data$Stars)
data$Style[data$Stars == ''] <- mode(data$Style)

# preprocessing the top ten rated ramen
data$year <- substr(data$Top.Ten,1,4)
data$top_ten <- substr(data$Top.Ten, 5, 10)

for (i in 1:length(data$top_ten)) {
    if (length(data$top_ten[i]) == 4) {
        data$top_ten[i] = as.integer(substr(data$top_ten[i],3,5))
    } else if (length(data$top_ten[i] == 3)) {
        data$top_ten[i] = as.integer(substr(data$top_ten[i],3,4))
    }
}
data$top_ten[is.na(data$top_ten)] <- 0
data$year[data$year == ""] <- 0
data$year[data$year == "\n"] <- 0

data <- data[,-7]
data$year <- as.numeric(data$year)
data$top_ten <- as.numeric(data$top_ten)

# Define UI for application that draws a histogram
ui <- navbarPage("Final Shiny App",
                 tabPanel("Description of Ramen Ratings",
                          includeMarkdown("./Decription of Dataset.md")
                 ),
                 tabPanel("Summary of Ramen Ratings",
                          verbatimTextOutput("ramenSummary")
                 ),
                 tabPanel("Barplot of Countries and Style",
                          sidebarLayout(
                              sidebarPanel(
                                  h2("Barplot Plot"),
                                  radioButtons("barSelector", 
                                              choices = c("Countries", "Styles"), 
                                              label = h4("Select")                                  )
                              ), #  sidebarPanel
                              mainPanel(
                                  plotOutput(outputId = "plot1")
                              )
                          ), # sidebarLayout
                 ), # tabPanel
                 tabPanel("Boxplot of Ratings of Ramen according to Country and Style",
                          sidebarLayout(
                              sidebarPanel(
                                  h2("Box Plot"),
                                  selectInput("plotSelector", 
                                              choices = unique(data$Country), 
                                              label = h4("Select Country"),
                                              selected = sample(data$Country,1)
                                              ),
                                  selectInput("plotSelector2", 
                                              choices = unique(data$Style), 
                                              label = h4("Select Style"),
                                              selected = sample(data$Style,1)
                                  ),
                                        ), #  sidebarPanel
                              mainPanel(
                                  plotOutput(outputId = "plot2")
                              )
                          ), # sidebarLayout
                 ), # tabPanel
                 tabPanel("Top 10 Ramen according to Rating",
                          sidebarLayout(
                              sidebarPanel(
                                  h2("Top 10 Ramen"),
                                  checkboxGroupInput("toptenSelector", 
                                                    choices = unique(data$year[data$year != 0]), 
                                                    label = h4("Select year")
                                  ),
                              ), #  sidebarPanel
                              mainPanel(
                                  plotOutput(outputId = "plot3")
                              )
                          ), # sidebarLayout
                 )
                 tabPanel("Top 10 Ramen Details",
                          sidebarLayout(
                              sidebarPanel(
                                  h2("Decription of Top 10 Ramen"),
                                  numericInput("descripSelector", 
                                               value = unique(data$year[data$year != 0]), 
                                               label = h4("Select year")
                                  ),
                                  numericInput("descripSelector2", 
                                               label = h4("Select Top 10"),
                                               value = unique(data$top_ten[data$top_ten != 0]))
                              ), #  sidebarPanel
                              mainPanel(
                                  tableOutput(outputId = "descripTopten")
                              )
                          ), # sidebarLayout
                 ),
                 tabPanel("Regression Models for Ratings",
                          sidebarLayout(
                              sidebarPanel(
                                  h2("Variable to Predict Ratings"),
                                  numericInput("VarSelector", 
                                               value = colnames(data)[-6]), 
                                               label = h4("Select Variable")
                                  )
                              ), #  sidebarPanel
                              mainPanel(
                                  tableOutput(outputId = "Formula")
                              )
                          ) # sidebarLayout       
                 )
) # navbarPage



# Define server logic required to draw a histogram
server <- function(input, output) {
    output$ramenSummary <- renderTable(summary(data))
    
    output$plot1 <- renderPlot(
        data %>% filter(input$barSelector) %>% 
            ggplot(aes(x=Stars),) +
            geom_bar(color = "steelblue", fill = "steelblue") +
            theme_classic()   
    )
    
    output$plot2 <- renderPlot(
        data %>% filter(input$plotSelector) %>% 
            filter(input$plotSelector2) 
        
        
    )
        
    output$plot3 <- renderPlot(
        data %>% filter(toptenSelector) %>% 
            
    )
        

        
    output$descripTopten <- renderTable(
        for (year in 2012:2016) {
            for (topten in 1:10) {
                if(input$descripSelector == 2012) {
                    if (input$descripSelector2 == 1) {
                        data %>% subset(data$year==2012) %>% subset(data$top_ten==1)
                    }
                    else if (input$descripSelector2 == 2){
                        
                    }            
                    else if (input$descripSelector2 == 3){
                        
                    }
                    else if (input$descripSelector2 == 4){
                        
                    }
                    else if (input$descripSelector2 == 5){
                        
                    }
                    else if (input$descripSelector2 == 6){
                        
                    }
                    else if (input$descripSelector2 == 7){
                        
                    }
                    else if (input$descripSelector2 == 8){
                        
                    }
                    else if (input$descripSelector2 == 9){
                        
                    }
                    else if (input$descripSelector2 == 10){
                        
                    }
                }
                
            }
        }
    ) 
        plot_ly(x=~Cast, y=~Rating, title="Relationship between casts and rating")
    
    htmlwidgets::saveWidget(p,'plotly.html')
    
    output$Formula <- renderText()
    
}
# Run the application 
shinyApp(ui = ui, server = server)