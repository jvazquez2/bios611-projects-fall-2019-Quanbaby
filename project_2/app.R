library(shiny)
library(tidyverse)

source("helper_functions.R",local = F)

ui <- navbarPage("Project 2",
    tabPanel("Background",
           fluidPage(
             titlePanel("Background"),
             sidebarLayout(
               sidebarPanel(
                 "About the project"
               ),
               mainPanel(
                 textOutput(outputId = "pre0"),
                 textOutput(outputId = "pre1"),
                 textOutput(outputId = "pre2"),
                 textOutput(outputId = "pre3"),
                 textOutput(outputId = "pre4")
               )
             )
           )
         ),
  

    tabPanel("Q1",
             fluidPage(
               titlePanel("Trend"),
               sidebarLayout(
                 sidebarPanel(
                 h3(textOutput(outputId = "text1")),
                 # Input
                 selectInput("trend", label = h5("Select a variable"), 
                             choices = list("People served" = "Food Provided for", "Food" = "Food Pounds", 
                                            "Clothes" = "Clothing Items", "Diapers" = "Diapers", 
                                            "Hygiene Kits" = "Hygiene Kits", "School Kits" = "School Kits",
                                            "Financial Support" = "Financial Support"), 
                             selected = 1),
                 # Output
                 textOutput("value1")
                 ),
                 mainPanel(
                   plotOutput(outputId = "plot1")
                 )
               )
             )
          ),
      
      tabPanel("Q2",
               fluidPage(
                 titlePanel("Relationship"),
                 sidebarLayout(
                   sidebarPanel(
                     h3(textOutput(outputId = "text2")),
                     # Input
                     selectInput("relation", label = h5("Select a variable"), 
                                 choices = list("Food" = "Food Pounds", 
                                                "Clothes" = "Clothing Items"), 
                                 selected = 1),
                     # Output
                     textOutput("value2")
                   ),
                   mainPanel(
                     plotOutput(outputId = "plot2")
                   )
                 )
               )
             ),
    
      tabPanel("Q3",
               fluidPage(
                 titlePanel("Average Amounts"),
                 sidebarLayout(
                   sidebarPanel(
                     h3(textOutput(outputId = "text3")),
                     # Input
                     selectInput("average", label = h5("Select a variable"), 
                                 choices = list("Food" = "Food Pounds", 
                                                "Clothes" = "Clothing Items"), 
                                 selected = 1),
                     # Output
                     textOutput("value3")
                   ),
                   mainPanel(
                     plotOutput(outputId = "plot3"),
                     textOutput(outputId = "text_avrg")
                   )
                 )
               )
             ),
       
       tabPanel("Q4",
                fluidPage(
                  titlePanel("Specific Analysis"),
                  sidebarLayout(
                    sidebarPanel(
                      h3(textOutput(outputId = "text4")),
                      # Input
                      numericInput("client", label = h5("Input a client file number"), 3502),
                      
                      # Output
                      textOutput(outputId = "value4")
                    ),
                    mainPanel(
                      plotOutput(outputId = "plot41"),
                      plotOutput(outputId = "plot42")
                    )
                  )
                )
              )
  
)


server <- function(input, output) {
  
  output$pre0 <- renderText("This project is a continuity of my project 1. I will focus on the four questions shown as below.")
  output$pre1 <- renderText("1. The trend of the change for different variables that people choose over time (from 2005 till now).")
  output$pre2 <- renderText("2. The relationships between number of people served and food pounds/clothing items. ")
  output$pre3 <- renderText("3. The average amounts of food pounds/clothing items that per person served per visit need. ")
  output$pre4 <- renderText("4. Specific analysis for individuals.")
  output$text1 <- renderText("1. Show the trends of different variables over time")
  
  output$value1 <- renderPrint({ 
    paste("You have selected", input$trend)
  })  
  
  output$plot1 <- renderPlot({
    trend(input$trend)
  })
  
  output$text2 <- renderText("2. Show the potential relationships between different variables.")
  
  output$value2 <- renderPrint({ 
    paste("You have selected", input$relation)
  }) 
  
  output$plot2 <- renderPlot({
    relation(input$relation)
  })
  
  output$text3 <- renderText("3. Show the average amount of food/clothes per person per visit getting.")
  
  output$value3 <- renderPrint({ 
    paste("You have selected", input$average)
  }) 
  
  output$plot3 <- renderPlot({
    average_plot(input$average)
  })

  output$text_avrg <- renderText(paste("The average amount of ", input$average, 
                                "from 2000 and from 2015 till now are", average_num(input$average)[1], 
                                ",", average_num(input$average)[2], ", respectively."))
  
  output$text4 <- renderText("4. Specific analysis for individuals.")
  
  output$value4 <- renderPrint({ 
    client_check(input$client)
  }) 
  
  
  output$plot41 <- renderPlot({
    client_plot1(input$client)
  })
  
  output$plot42 <- renderPlot({
    client_plot2(input$client)
  })
  
}


shinyApp(ui = ui, server = server)


