library(shiny)
library(shinyjs)
library(tidyverse)
library(ggplot2)
library(Stat2Data)

data(Film)

list_choices <-  unique(Film$Time_code)[!is.na(unique(Film$Time_code))]
names(list_choices) <- paste(unique(Film$Time_code)[!is.na(unique(Film$Time_code))],sep="")


# Define UI for application that draws a histogram
ui <- navbarPage("First Shiny App",
                 tabPanel("Description of Dataset",
                          includeMarkdown("./Description of Dataset Film.md")
                 ), #  tabPanel
                 tabPanel("Summary of Dataset Film",
                          verbatimTextOutput("FilmSummary")
                 ),
                 tabPanel("Histogram of Ratings by length",
                          fluidPage(
                              sidebarLayout(sidebarPanel(
                                  selectInput("select1", label = h3("Rating by Length of Film"), 
                                              choices = list_choices,
                                              selected = 1)
                              ), mainPanel(
                                  h3("Plot"),
                                  plotOutput(outputId = "histog", click = "plot_click"))
                              )
                          )),
                 tabPanel("Rating by Length of Film",
                          fluidPage(
                              sidebarLayout(sidebarPanel(
                                  selectInput("select2", label = h3("Rating by Length of Film"), 
                                              choices = list_choices,
                                              selected = 1)
                              ), mainPanel(
                                  h3("Plot"),
                                  plotOutput(outputId = "plot1", click = "plot_click"))
                              ))),
                 tabPanel("Rating by Number of Casts of Film",
                          fluidPage(plotOutput(outputId = "plot2")
                          ))
) # navbarPage

col_scale <- scale_colour_discrete(limits = list_choices)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$FilmSummary<-renderPrint(summary(Film))
    
    output$plot1 <- renderPlot({
        ggplot(Film %>% filter(Time_code == input$select2),
               aes(x=Rating)) +
            col_scale +
            geom_density()
    })
    
    output$plot2 <- renderPlot({
        ggplot(Film)+geom_point(aes(Cast, Rating))
    })
    
    output$histog <- renderPlot(hist(Film %>%filter(Time_code == input$select1) %>% dplyr::select(Rating) %>% unlist(),main="Histogram of Ratings"))
}
# Run the application 
shinyApp(ui = ui, server = server)