library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel(h2("Simple Web Scraping!\nWho Appears on 1st Page!!!")),
  
  
  sidebarPanel(
    textInput("searchTerm",
              "Search:",
              value = "ipquants",
              width = NULL,
              placeholder = NULL),
    submitButton("Update View", icon("refresh"))
    
  ),
  
  mainPanel(plotOutput("barPlot"),plotOutput("piePlot"))
))