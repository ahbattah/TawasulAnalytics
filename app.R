#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
   # theme = shinytheme("cerulean"),
   
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
                       end = as.Date(maxDate, tz = "UTC", "yyyy-mm-dd")
        )
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Main",
                             HTML("<br>"),
                             htmlOutput("textSumm"),
                             HTML("<br><br>"),
                             plotOutput("channelsPlot"),
                             HTML("<br><br>"),
                             plotOutput("statusDetailsPlot"),
                             HTML("<br><br>"),
                             plotOutput("caseTypePlot"),
                             HTML("<br><br>"),
                             plotOutput("statusFeedbackPlot"),
                             HTML("<br><br>"),
                             plotOutput("userTypePlot")),
                    tabPanel("SLA",
                             HTML("<br>"),
                             plotOutput("slaPlot"),
                             HTML("<br><br>"),
                             plotOutput("performancePlot")),
                    tabPanel("Escalation",
                             HTML("<br>"),
                             plotOutput("escalationPlot"))
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
  
  ################ Main Tab Data ################
  # Summary
  output$textSumm <- renderUI({
    req(input$calInp[1], input$calInp[2], input$ministryInp)
    tags$h5("Total Number of cases From ", format(as.Date(input$calInp[1]), 
                                                  format = "%d/%m/%Y"), " To ", format(as.Date(input$calInp[2]), 
                                                                                       format = "%d/%m/%Y"), " is ",
            tags$b(nrow(selectedData())))
  })
  
  # Status details
  output$channelsPlot <- renderPlot({
    req(input$ministryInp)
    ggplot(selectedData(), aes(x = COMPLAINT_CHANNEL, fill = COMPLAINT_CHANNEL)) +
      geom_bar() +
      coord_flip() +
      geom_text(stat = "count", aes(label = ..count..), #hjust = -0.2, vjust = 0.5,
                 size = 4) +
      labs(title = "Based on Channel", 
           x = "Status", y = "Count") +
      tawasul_theme
  })
  
  # Status details
  output$statusDetailsPlot <- renderPlot({
    req(input$ministryInp)
      ggplot(selectedData(), aes(x = STATUS_DESC, fill = STATUS_DESC)) +
        geom_bar() +
        coord_flip() +
        geom_text(stat = "count", aes(label = ..count..),
                  size = 4) +
        labs(title = "Based on Status Details", 
                            x = "Status", y = "Count") +
        tawasul_theme
    })
  
  # Case type
  output$caseTypePlot <- renderPlot({
    req(input$ministryInp)
      ggplot(selectedData(), aes(x = REQUEST_TYPE, fill = REQUEST_TYPE)) +
      geom_bar() +
      labs(title = "Based on Case Type", x = "Case", y = "Count") +
      geom_text(stat = "count", aes(label = ..count..),
                  vjust = -0.5, size = 4) +
        tawasul_theme
  })
  
  
  # Status based on feedback
  output$statusFeedbackPlot <- renderPlot({
    req(input$ministryInp)
    selectedData() %>% 
      filter(STATUS_DESC %in% c("Closed - No Feedback", 
                                "Closed - Satisfied Feedback", 
                                "Closed - Not Satisfied")) %>% {
    ggplot(., aes(x = STATUS_DESC, fill = STATUS_DESC)) +
      geom_bar() +
      labs(title = paste0("Based on Feedback ", nrow(.)), 
           x = "Status", y = "Count") +
      geom_text(stat = "count", aes(label = ..count..),
                vjust = -0.5, size = 4) +
      tawasul_theme
    }
  })
  
  # User type
  output$userTypePlot <- renderPlot({
    req(input$ministryInp)
    ggplot(selectedData(), aes(x = CUSTOMER_TYPE, fill = CUSTOMER_TYPE)) +
      geom_bar() +
      labs(title = "Based on Customer Type", x = "Customer Type", y = "Count") +
      geom_text(stat = "count", aes(label = ..count..),
                vjust = -0.5, size = 4) +
      tawasul_theme
  })
  
  ################ SLA Tab Data ################
  # SLA
  output$slaPlot <- renderPlot({
    req(input$ministryInp)
    selectedData() %>%
      ggplot(., aes(x = SLA_DESC, fill = STATUS, color = STATUS)) +
        geom_bar(position = "dodge") +
        labs(title = paste0("SLA"), 
             x = "SLA Desc.", y = "Count") +
      geom_text(stat = "count", aes(label = ..count..),
                position = position_dodge(0.9), vjust = -.3) +
      tawasul_theme +
      theme(legend.position = "right", 
            axis.text.x = element_text(angle = 60, vjust = .6))
  })
  
  # Performance cases
  output$performancePlot <- renderPlot({
    req(input$ministryInp)
    selectedData() %>% 
      ggplot(aes(x = PERFORMANCE, fill = PRIORITY, color = PRIORITY)) +
        geom_bar(position = "dodge") +
        labs(title = paste0("Performance"), 
             x = "Performance Desc.", y = "Count") +
        geom_text(stat = "count", aes(label = ..count..),
                  position = position_dodge(0.9), vjust = -.5) +
        tawasul_theme +
        theme(legend.position = "right", 
              axis.text.x = element_text(angle = 60, vjust = .6))
    
  })
  
  ################ Escalation Tab Data ################
  # Escalation to minister
  output$escalationPlot <- renderPlot({
    req(input$ministryInp)
    selectedData() %>% 
      filter(IS_ESCALATED == "Escalated") %>% {
      ggplot(., aes(x = TYPE_OF_ESCALATION, fill = TYPE_OF_ESCALATION)) +
      geom_bar(position = "dodge") +
      labs(title = "Escalation", 
           x = "Type of Escalation", y = "Count") +
      geom_text(stat = "count", aes(label = ..count..),
                position = position_dodge(0.9), vjust = -.5) +
      tawasul_theme
      }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

