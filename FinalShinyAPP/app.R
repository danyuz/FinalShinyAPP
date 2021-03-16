library(shiny)
library(ggplot2)
library(devtools)

## packages required to use
library(tidyverse)
library(shinythemes)
library(plotly)
library(shinyjs)

data <- read.csv("../ramen-ratings.csv", header = TRUE)

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

# Define UI for application that draws a histogram
ui <- navbarPage("Final Shiny App",
                 tabPanel("Description of Ramen Ratings",
                          includeMarkdown("Decription of Dataset.md")
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
                                  tableOutput(outputId = "descrip")
                              )
                          ), # sidebarLayout
                 ),
                 tabPanel("Top 10 Ramen according to Rating",
                          sidebarLayout(
                              sidebarPanel(
                                  h2("Top 10 Ramen"),
                                  checkboxGroupInput("toptenSelector", 
                                                    choices = data$year[data$year != 0], 
                                                    label = h4("Select year")
                                  ),
                              ), #  sidebarPanel
                              mainPanel(
                                  plotOutput(outputId = "topten")
                              )
                          ), # sidebarLayout
                 )
) # navbarPage



# Define server logic required to draw a histogram
server <- function(input, output) {
    
}
# Run the application 
shinyApp(ui = ui, server = server)