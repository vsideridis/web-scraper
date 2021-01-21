library(shiny)
library(selectr)
library(xml2)
library(rvest)
library(stringr)
library(jsonlite)
library(tidyverse)
library(wesanderson)
library(ggplot2)
library(RColorBrewer)
library(rsconnect)
source('getInfo.R')


shinyServer(function(input, output) {
  
  numOfJobs = reactive({ getNumOfJobs(job = input$searchTerm) })
  details = reactive({ getJobDetails(job = input$searchTerm) })
  
  output$plotCompanySize <- renderPlot({
    companySize = details()
    companySize = companySize$Company_size
    companySize = na.omit(companySize)
    
    if(length(companySize)==0 | numOfJobs() == "0"){
      plot(1, type = 'n', bty='n', xaxt='n', yaxt='n', xlab="", ylab="")
      legend("top", legend = "Not Enough Data", cex=3, bty='n',text.col = "#173F5F")
    }else {
      data = table(companySize)
      data = (data/sum(data))*100
      
      y<-barplot(data,horiz=T,
                 yaxt="n",
                 main="",
                 xlab="Percentage (%)",
                 col="#3CAEA3",
                 border=NA,
                 cex.axis=2,
                 cex.lab = 1.8)
      x<-0
      text(x,y,names(data), col = "#173F5F", cex=1.5, pos = 4)
    }
    
    
  })
  
  
  
  output$plotJobType <- renderPlot({
    jobType = details()
    jobType = jobType$Jobtype
    jobType = na.omit(jobType)
    
    if(length(jobType)==0 | numOfJobs() == "0"){
      plot(1, type = 'n', bty='n', xaxt='n', yaxt='n', xlab="", ylab="")
      legend("top", legend = "Not Enough Data", cex=3, bty='n',text.col = "#173F5F")
    }else {
      data = table(jobType)
      data = (data/sum(data))*100
      
      y<-barplot(data,horiz=T,
                 yaxt="n",
                 main="",
                 xlab="Percentage (%)",
                 col="#3CAEA3",
                 border=NA,
                 cex.axis=2,
                 cex.lab = 1.8)
      x<-0
      text(x,y,names(data), col = "#173F5F", cex=1.5, pos = 4)
    }
    
  })
  
  
  
  output$plotExperienceLevel <- renderPlot({
    experienceLevel = details()
    experienceLevel = experienceLevel$Experience_level
    experienceLevel = na.omit(experienceLevel)
    
    if(length(experienceLevel)==0 | numOfJobs() == "0"){
      plot(1, type = 'n', bty='n', xaxt='n', yaxt='n', xlab="", ylab="")
      legend("top", legend = "Not Enough Data", cex=3, bty='n',text.col = "#173F5F")
    }else {
      data = table(experienceLevel)
      data = (data/sum(data))*100
      
      y<-barplot(data,horiz=T,
                 yaxt="n",
                 main="",
                 xlab="Percentage (%)",
                 col="#3CAEA3",
                 border=NA,
                 cex.axis=2,
                 cex.lab = 1.8)
      x<-0
      text(x,y,names(data), col = "#173F5F", cex=1.5, pos = 4)
    }
    
  })
  
  
  
  output$plotRole <- renderPlot({
    role = details()
    role = role$Role
    role = na.omit(role)
    
    if(length(role)==0 | numOfJobs() == "0"){
      plot(1, type = 'n', bty='n', xaxt='n', yaxt='n', xlab="", ylab="")
      legend("top", legend = "Not Enough Data", cex=3, bty='n',text.col = "#173F5F")
    }else {
      data = table(role)
      data = (data/sum(data))*100
      
      y<-barplot(data,horiz=T,
                 yaxt="n",
                 main="",
                 xlab="Percentage (%)",
                 col="#3CAEA3",
                 border=NA,
                 cex.axis=2,
                 cex.lab = 1.8)
      x<-0
      text(x,y,names(data), col = "#173F5F", cex=1.5, pos = 4)
    }
    
  })
  
  
  
  output$plotIndustry <- renderPlot({
    industry = details()
    industry = industry$Industry
    industry = na.omit(industry)
    
    if(length(industry)==0 | numOfJobs() == "0"){
      plot(1, type = 'n', bty='n', xaxt='n', yaxt='n', xlab="", ylab="")
      legend("top", legend = "Not Enough Data", cex=3, bty='n',text.col = "#173F5F")
    }else {
      data = table(industry)
      data = (data/sum(data))*100
      
      y<-barplot(data,horiz=T,
                 yaxt="n",
                 main="",
                 xlab="Percentage (%)",
                 col="#3CAEA3",
                 border=NA,
                 cex.axis=2,
                 cex.lab = 1.8)
      x<-0
      text(x,y,names(data), col = "#173F5F", cex=1.5, pos = 4)
    }
    
  })
  
  
  
  output$plotCompanyType <- renderPlot({
    companyType = details()
    companyType = companyType$Company_type
    companyType = na.omit(companyType)
    
    if(length(companyType)==0 | numOfJobs() == "0"){
      plot(1, type = 'n', bty='n', xaxt='n', yaxt='n', xlab="", ylab="")
      legend("top", legend = "Not Enough Data", cex=3, bty='n',text.col = "#173F5F")
    }else {
      data = table(companyType)
      data = (data/sum(data))*100
      
      y<-barplot(data,horiz=T,
                 yaxt="n",
                 main="",
                 xlab="Percentage (%)",
                 col="#3CAEA3",
                 border=NA,
                 cex.axis=2,
                 cex.lab = 1.8)
      x<-0
      text(x,y,names(data), col = "#173F5F", cex=1.5, pos = 4)
    }
    
  })
  
  
  
  output$numOfJobs <- renderText({ paste0(numOfJobs(), " jobs found with this description! Stats from the top 25 are shown below")  })
  
})