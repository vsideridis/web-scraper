library(shiny)
library(selectr)
library(xml2)
library(rvest)
library(stringr)
library(jsonlite)
library(tidyverse)


url = 'https://www.google.com/search?q=ipquants'
first_page <- read_html(url)
titles <- html_nodes(first_page, xpath = "//div/div/div/a/div[not(div)]") %>%
  html_text()
titles <- titles[titles != ">"]
titles <- titles[titles != "View all"]
titles <- titles[nzchar(titles)]

n = length(titles)
titles = sapply(seq(n), function(i) strsplit(titles[i], " ")[[1]][1])

titles = titles[grepl(".", titles, fixed = TRUE)]
titles

data = table(titles)
names(data)
barplot(prop.table(table(titles))*length(titles),
        ylab = "Times",
        col=rgb(0,0.1,0.7,0.7), border=NA)


data = sort(data)
y<-barplot(data,horiz=T,
           yaxt="n",
           main="First Google Search Page for term 'ipquants'",
           xlab="Frequency",
           col="#3CAEA3",
           border=NA)
x<-0.5*data
text(x,y,names(data), col = "#173F5F", cex=1.5)