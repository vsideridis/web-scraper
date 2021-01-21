library(shiny)

 
shinyUI(pageWithSidebar(
  
 
  headerPanel(h2("Job Statistics from Stack Overflow")),
  
  
  sidebarPanel(
    textInput("searchTerm",
              "Search:",
              value = "developer",
              width = NULL,
              placeholder = NULL),
    submitButton("Update View", icon("refresh"))
    
  ),
  
  mainPanel(textOutput("numOfJobs"),
            
            tabsetPanel(type = "tabs",
                        tabPanel("Company Size", plotOutput("plotCompanySize")),
                        tabPanel("Job Type", plotOutput("plotJobType")),
                        tabPanel("Industry", plotOutput("plotIndustry")),
                        tabPanel("Company Type", plotOutput("plotCompanyType")),
                        tabPanel("Experience Level", plotOutput("plotExperienceLevel")),
                        tabPanel("Role", plotOutput("plotRole"))
                        )
            )
))