#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(readr)
library(dplyr)
library(shinycssloaders)

#source("prepareData.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   title = "Tawasul Stats.",
   
   # Application title
   titlePanel(HTML("<img src = 'tawasul-online-logo-en.png'/>")),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        # Select Ministry
        selectInput("ministryInp",
                    "Ministry",
                    choices = c("Select Ministry" = "", "All", levels(tawasul$MINISTRY))),
        
        # Date Range
        dateRangeInput("calInp",
                       "Select Date",
                       min = minDate, max = maxDate,
                       start = as.Date(maxDate - dyears(1) + ddays(1), tz = "UTC", "yyyy-mm-dd"), 
                       end = as.Date(maxDate, tz = "UTC", "yyyy-mm-dd"),
                       format = "dd/mm/yyyy"
        ),
        downloadButton('export', 'Export to pdf')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("Main",
                             HTML("<br>"),
                             htmlOutput("textSumm"),
                             HTML("<br><br>"),
                             withSpinner(plotOutput("channelsPlot")),
                             HTML("<br><br>"),
                             withSpinner(plotOutput("statusDetailsPlot")),
                             HTML("<br><br>"),
                             withSpinner(plotOutput("caseTypePlot")),
                             HTML("<br><br>"),
                             withSpinner(plotOutput("statusFeedbackPlot")),
                             HTML("<br><br>"),
                             withSpinner(plotOutput("userTypePlot"))),
                    tabPanel("Suggestions",
                             HTML("<br>"),
                             htmlOutput("textSugg"),
                             HTML("<br><br>"),
                             withSpinner(plotOutput("suggPlot")),
                             HTML("<br>"),
                             p(em("*Other contains: irrelevant, in-progress..etc"))),
                    tabPanel("SLA",
                             HTML("<br>"),
                             withSpinner(plotOutput("slaPlot")),
                             HTML("<br><br>"),
                             withSpinner(plotOutput("performancePlot"))),
                    tabPanel("Escalation",
                             HTML("<br>"),
                             withSpinner(plotOutput("ministerEscPlot")),
                             HTML("<br><br>"),
                             withSpinner(plotOutput("ofdpmEscPlot")))
                    )
        
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selectedData <- reactive({
    if (input$ministryInp == "All") {
      tawasul %>% filter(CREATE_DATE >= input$calInp[1] & 
                           CREATE_DATE <= input$calInp[2])
    } else {
      tawasul %>% filter(MINISTRY == input$ministryInp &
                           (CREATE_DATE >= input$calInp[1] & CREATE_DATE <= input$calInp[2]))
    }
    
  })
  
  suggestions <- reactive({
    selectedData() %>%
    filter(REQUEST_TYPE == "Suggestion")
  })
  
  vals <- reactiveValues(channelsPlot = NULL, statusDetailsPlot = NULL,
                         caseTypePlot = NULL, statusFeedbackPlot = NULL,
                         userTypePlot = NULL, slaPlot = NULL,
                         performancePlot = NULL, ministerEscPlot = NULL,
                         ofdpmEscPlot = NULL, suggPlot = NULL)
  
  ################ Main Tab Data ################
  # Summary
  output$textSumm <- renderUI({
    req(input$calInp[1], input$calInp[2], input$ministryInp)
    tags$h5("Total Number of cases From ", format(as.Date(input$calInp[1]), 
                                                  format = "%d/%m/%Y"), " To ", format(as.Date(input$calInp[2]), 
                                                                                       format = "%d/%m/%Y"), " is ",
            tags$b(nrow(selectedData())))
  })
  
  # Channels
  output$channelsPlot <- renderPlot({
    req(input$ministryInp)
    vals$channelsPlot <- ggplot(selectedData(), aes(x = COMPLAINT_CHANNEL, fill = COMPLAINT_CHANNEL)) +
      geom_bar() +
      coord_flip() +
      geom_text(stat = "count", aes(label = ..count..), #hjust = -0.2, vjust = 0.5,
                 size = 4) +
      labs(title = "Based on Channel", 
           x = "Channel", y = "Count") +
      tawasul_theme
    
    vals$channelsPlot
  })
  
  # Status details
  output$statusDetailsPlot <- renderPlot({
    req(input$ministryInp)
      vals$statusDetailsPlot <- ggplot(selectedData(), aes(x = STATUS_DESC, fill = STATUS_DESC)) +
        geom_bar() +
        coord_flip() +
        geom_text(stat = "count", aes(label = ..count..),
                  size = 4) +
        labs(title = "Based on Status Details", 
                            x = "Status", y = "Count") +
        tawasul_theme
      
      vals$statusDetailsPlot
    })
  
  # Case type
  output$caseTypePlot <- renderPlot({
    req(input$ministryInp)
      vals$caseTypePlot <- ggplot(selectedData(), aes(x = REQUEST_TYPE, fill = REQUEST_TYPE)) +
      geom_bar() +
      labs(title = "Based on Case Type", x = "Case", y = "Count") +
      geom_text(stat = "count", aes(label = ..count..),
                  vjust = -0.2, size = 4) +
        tawasul_theme
      vals$caseTypePlot
  })
  
  
  # Status based on feedback
  output$statusFeedbackPlot <- renderPlot({
    req(input$ministryInp)
    vals$statusFeedbackPlot <- selectedData() %>% 
      filter(STATUS_DESC %in% c("Closed - No Feedback", 
                                "Closed - Satisfied Feedback", 
                                "Closed - Not Satisfied")) %>% {
    ggplot(., aes(x = STATUS_DESC, fill = STATUS_DESC)) +
      geom_bar() +
      labs(title = paste0("Based on Feedback ", nrow(.)), 
           x = "Status", y = "Count") +
      geom_text(stat = "count", aes(label = ..count..),
                vjust = -0.2, size = 4) +
      tawasul_theme
                                }
    vals$statusFeedbackPlot
  })
  
  # User type
  output$userTypePlot <- renderPlot({
    req(input$ministryInp)
    vals$userTypePlot <- ggplot(selectedData(), aes(x = CUSTOMER_TYPE, fill = CUSTOMER_TYPE)) +
      geom_bar() +
      labs(title = "Based on Customer Type", x = "Customer Type", y = "Count") +
      geom_text(stat = "count", aes(label = ..count..),
                vjust = -0.2, size = 4) +
      tawasul_theme
    
    vals$userTypePlot
  })
  
  ################ SLA Tab Data ################
  # SLA
  output$slaPlot <- renderPlot({
    req(input$ministryInp)
    vals$slaPlot <- selectedData() %>%
     ggplot(., aes(x = SLA_DESC, fill = STATUS, color = STATUS)) +
        geom_bar(position = "dodge") +
        labs(title = paste0("SLA"), 
             x = "SLA Desc.", y = "Count") +
      geom_text(stat = "count", aes(label = ..count..),
                position = position_dodge(0.9), vjust = -.2) +
      tawasul_theme +
      theme(legend.position = c(0.05, 0.9), legend.title =  element_blank(),
            axis.text.x = element_text(angle = 60, vjust = .6))
    
    vals$slaPlot
  })
  
  # Performance cases
  output$performancePlot <- renderPlot({
    req(input$ministryInp)
    vals$performancePlot <- selectedData() %>% 
      ggplot(aes(x = PERFORMANCE, fill = PRIORITY, color = PRIORITY)) +
        geom_bar(position = "dodge") +
        labs(title = paste0("Performance"), 
             x = "Performance Desc.", y = "Count") +
        geom_text(stat = "count", aes(label = ..count..),
                  position = position_dodge(0.9), vjust = -.2) +
        tawasul_theme +
        theme(legend.position = c(0.05, 0.9), legend.title = element_blank(), 
              axis.text.x = element_text(angle = 60, vjust = .6))
    vals$performancePlot
    
  })
  
  ################ Escalation Tab Data ################
  # Escalation to minister
  output$ministerEscPlot <- renderPlot({
    req(input$ministryInp)
    vals$ministerEscPlot <- selectedData() %>%
      ggplot(., aes(x = ESCALATED_TO_MINISTER, fill = ESCALATED_TO_MINISTER)) +
      geom_bar(position = "dodge") +
      scale_x_discrete(breaks = 0:1, labels = c('Not Escalated', 'Escalated')) +
      labs(title = "Escalation to Minister", x = "", y = "Count") +
      geom_text(stat = "count", aes(label = ..count..),
                position = position_dodge(0.9), vjust = -.5) +
      tawasul_theme
    vals$ministerEscPlot
  })
  
  # Escalation to minister
  output$ofdpmEscPlot <- renderPlot({
    req(input$ministryInp)
    vals$ofdpmEscPlot <- selectedData() %>%
      ggplot(., aes(x = ESCALATED_TO_OFDPM, fill = ESCALATED_TO_OFDPM)) +
      geom_bar(position = "dodge") +
      scale_x_discrete(breaks = 0:1, labels = c('Not Escalated', 'Escalated')) +
      labs(title = "Escalation to OFDPM", x = "", y = "Count") +
      geom_text(stat = "count", aes(label = ..count..),
                position = position_dodge(0.9), vjust = -.5) +
      tawasul_theme
    vals$ofdpmEscPlot
  })
  
  # Number of suggestion
  output$textSugg <- renderUI({
    req(input$calInp[1], input$calInp[2], input$ministryInp)
    tags$h5("Number of suggestions ", tags$b(nrow(suggestions())))
  })
  
  # Suggestion details
  output$suggPlot <- renderPlot({
    req(input$ministryInp)
    vals$suggPlot <- ggplot(suggestions(), aes(x = SUGGESTION_STATUS, fill = CUSTOMER_TYPE, color = CUSTOMER_TYPE)) +
      geom_bar(position = "dodge") +
      geom_text(stat = "count", aes(label = ..count..),
                position = position_dodge(0.9), vjust = -.5) +
      labs(x = "Status", y = "Count") +
      tawasul_theme +
      theme(legend.position = c(0.05, 0.9), legend.title = element_blank())
    vals$suggPlot
  })
  
  output$export = downloadHandler(
    filename = function() {paste0(input$ministryInp, '.pdf')},
    content = function(file) {
      pdf(file, onefile = TRUE)
      
      print( vals$channelsPlot )
      print( vals$statusDetailsPlot )
      print( vals$caseTypePlot )
      print( vals$statusFeedbackPlot )
      print( vals$userTypePlot )
      print( vals$slaPlot )
      print( vals$performancePlot )
      print( vals$ministerEscPlot )
      print( vals$ofdpmEscPlot )
      print( vals$suggPlot )
      
      dev.off()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)