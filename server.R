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




# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  output$barPlot <- renderPlot({
    
    searchTerm = input$searchTerm
    url = paste0('https://www.google.com/search?q=',searchTerm)
    first_page <- read_html(url)
    titles <- html_nodes(first_page, xpath = "//div/div/div/a/div[not(div)]") %>%
      html_text()
    titles <- titles[titles != ">"]
    titles <- titles[titles != "View all"]
    titles <- titles[nzchar(titles)]
    
    n = length(titles)
    titles = sapply(seq(n), function(i) strsplit(titles[i], " ")[[1]][1])
    
    titles = titles[grepl(".", titles, fixed = TRUE)]
    
    data = table(titles)
    #data = sort(data)
    y<-barplot(data,horiz=T,
               yaxt="n",
               main=paste0("First Google Search Page for term '", input$searchTerm, "'"),
               xlab="Frequency",
               col="#3CAEA3",
               border=NA)
    x<-0.5*data
    text(x,y,names(data), col = "#173F5F", cex=1.5)
  })
  
  output$piePlot <- renderPlot({
    
    searchTerm = input$searchTerm
    url = paste0('https://www.google.com/search?q=',searchTerm)
    first_page <- read_html(url)
    titles <- html_nodes(first_page, xpath = "//div/div/div/a/div[not(div)]") %>%
      html_text()
    titles <- titles[titles != ">"]
    titles <- titles[titles != "View all"]
    titles <- titles[nzchar(titles)]
    
    n = length(titles)
    titles = sapply(seq(n), function(i) strsplit(titles[i], " ")[[1]][1])
    
    titles = titles[grepl(".", titles, fixed = TRUE)]
    nb.cols <- 10
    mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
    data = table(titles)
    df = data.frame("Names" = names(data), value = data)
    pie <- ggplot(df, aes(x="", y=value.Freq, fill=Names))+
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      scale_fill_manual(values = mycolors) +
      theme_minimal() +
      xlab("") +
      ylab("")
    print(pie)
  })
})