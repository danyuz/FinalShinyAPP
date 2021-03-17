library(shiny)
library(ggplot2)
library(devtools)
library(caret)
library(fastDummies)
library(randomForest)

## packages required to use
library(tidyverse)
library(shinythemes)
library(plotly)
library(shinyjs)

# trace errors
options(shiny.trace=TRUE)

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
data$year <- as.factor(data$year)
data$top_ten <- as.numeric(data$top_ten)
data$Stars <- as.numeric(data$Stars)


# Process for Machine Learning
data2 <- dummy_cols(data, select_columns = c('Style','Country'),
                      remove_selected_columns = TRUE)

data2 <- data2 %>% select(-c(Review.., Variety))

inTraining <- createDataPartition(data2$year, p = .75, list = FALSE)
training <- data2[ inTraining,]
testing  <- data2[-inTraining,]


# Define UI for application that draws a histogram
ui <- navbarPage("Ramen Ratings Shiny App",
                 theme = shinytheme("sandstone"),
                 tabPanel("Description of Ramen Ratings",
                          includeMarkdown("./Decription of Dataset.md"),
                          downloadButton("report", "Generate report")
                 ),
                 tabPanel("Summary of Ramen Ratings",
                          verbatimTextOutput("ramenSummary")
                 ),
                 tabPanel("Barplot of Countries and Style",
                          sidebarLayout(
                              sidebarPanel(
                                  h2("Barplot Plot"),
                                  radioButtons("barSelector", 
                                              choices = c("Country", "Style"), 
                                              label = h4("Select")                                  )
                              ), #  sidebarPanel
                              mainPanel(
                                  plotOutput(outputId = "plot1")
                              )
                          ) # sidebarLayout
                 ), # tabPanel
                 tabPanel("Boxplot of Ratings according to Country and Style",
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
                                  )
                                        ), #  sidebarPanel
                              mainPanel(
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Specified Box Plot",plotOutput("plot2")),
                                              tabPanel("Boxplot according to Countries", plotOutput("plot4")),
                                              tabPanel("Boxplot according to Styles", plotOutput("plot5"))
                                              
                              ))
                          ) # sidebarLayout
                 ), # tabPanel
                 tabPanel("Top 10 Ramen according to Rating",
                          sidebarLayout(
                              sidebarPanel(
                                  h2("Top 10 Ramen"),
                                  checkboxGroupInput("toptenSelector", 
                                                    choices = unique(data$year[data$year != 0]), 
                                                    label = h4("Select year"),
                                                    selected = 2012
                                  )
                              ), #  sidebarPanel
                              mainPanel(
                                  plotOutput(outputId = "plot3")
                              )
                          ) # sidebarLayout
                 ),
                 tabPanel("Top 10 Ramen Details",
                          sidebarLayout(
                              sidebarPanel(
                                  h2("Decription of Top 10 Ramen"),
                                  numericInput("descripSelector", 
                                               min = 2012,
                                               max = 2016,
                                               step = 1,
                                               value = 2012, 
                                               label = h4("Select year")
                                  ),
                                  numericInput("descripSelector2", 
                                               label = h4("Select Top 10"),
                                               min = 1,
                                               max = 10,
                                               step = 1,
                                               value = 1)
                              ), #  sidebarPanel
                              mainPanel(
                                  tableOutput(outputId = "descripTopten")
                              ) # sidebarLayout
                              )
                 ),
                 tabPanel("Regression Models for Ratings",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("ModelSelector", 
                                              choices = c("rf","gbm","nn"), 
                                              label = h4("Select Variable"),
                                              selected = "rf"),
                                  selectInput("MethodSelector", 
                                              choices = c("boot","cv","LOOCV","repeatedcv","none"), 
                                              label = h4("Select Tuning Method"),
                                              selected = "repeatedcv")
                                  
                              ), #  sidebarPanel
                              mainPanel(
                                  tabsetPanel(type = "tabs",
                                      tabPanel("Model", verbatimTextOutput("Model")),
                                      tabPanel("Prediction", verbatimTextOutput("Prediction"))                                  )
                              )# sidebarLayout   
                          )    
                 )
                 
) # navbarPage


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$ramenSummary <- renderPrint({summary(data)})
    
    output$plot1 <- renderPlot({
        if(input$barSelector=="Style"){
            ggplot(aggregate(Stars ~ Style, data=data, mean), aes(x=Style, y = Stars)) +
                geom_bar(color = "orange", fill = "orange", stat ="identity") +
                theme(axis.text.x = element_text(angle = 90))
        }
        else{
            ggplot(aggregate(Stars ~ Country, data=data, mean), aes(x=Country, y = Stars)) +
                geom_bar(color = "orange", fill = "orange", stat ="identity") +
                theme(axis.text.x = element_text(angle = 90))
            
        }
    })
    
    output$plot2 <- renderPlot({
        data %>% filter(Country == input$plotSelector) %>% 
            filter(Style == input$plotSelector2) %>% 
            plot_ly(y = ~Stars, alpha = 0.1, boxpoints = "suspectedoutliers") %>% 
            add_boxplot(x="Overall",color="orange")
    })
    
    output$plot3 <- renderPlot({
        data %>% filter(year == input$toptenSelector) %>% 
            ggplot(aes(y=Stars,x=top_ten)) + 
            geom_line(aes(color = year)) +
            geom_point(aes(color = year)) +
            ylim(0,5)+
            xlim(1,10)
        })
    
    output$descripTopten <- renderTable({
        data %>% filter(year == input$descripSelector) %>% 
            filter(top_ten == input$descripSelector2) %>% 
            select(-c(Review..,year,top_ten)) 
    })
    
    output$plot4 <- renderPlot({
        data %>% plot_ly(y = ~Stars, alpha = 0.1, boxpoints = "suspectedoutliers") %>% 
            add_boxplot(x= ~Country, color="orange")
    })
    
    output$plot5 <- renderPlot({
        data %>% plot_ly(y = ~Stars, alpha = 0.1, boxpoints = "suspectedoutliers") %>% 
            add_boxplot(x= ~Style, color="orange")
    })
    
    output$Model <- renderPrint({
        train(Stars ~ ., data = training, 
              method = input$ModelSelector, 
              trControl = trainControl(method = input$MethodSelector),
              verbose = FALSE)$finalModel
    })
    
    output$Prediction <- renderPrint({
        predict(train(Stars ~ ., data = training, 
                      method = input$ModelSelector, 
                      trControl = trainControl(method = input$MethodSelector),
                      verbose = FALSE),
                newdata = testing)
    })
    
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "Ramen_Report.html",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "Ramen_Report.Rmd")
            file.copy("Ramen_Report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(data = isolate(data))
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

