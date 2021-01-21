library(shiny)
library(selectr)
library(xml2)
library(rvest)
library(stringr)
library(jsonlite)
library(tidyverse)
library(rsconnect)


library(robotstxt)

url = 'https://patents.google.com/?q=screen&oq=screen'
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












# Scrap job info from stack overflow
job = "data analyst"

job = paste(strsplit(job, " ")[[1]], collapse = '+') 

url = paste0('https://stackoverflow.com/jobs?q=', job)
page <- read_html(url)
numberOfJobs <- html_nodes(page, "#content .fc-light") %>%  html_text()

numberOfJobs = numberOfJobs %>%
  str_remove_all(" ") %>%
  str_remove_all("\r") %>%
  str_remove_all("\n") %>%
  str_remove_all("jobs")

numberOfJobs = as.numeric(numberOfJobs)

links <- html_nodes(page, ".stretched-link") %>% html_attr("href")

links = unlist(lapply(links, function(tmp) strsplit(tmp, "\\Q/\\E")[[1]][3]))

details = c()
for(tmpURL in links){
  details = rbind(details, getInfo(tmpURL))
}




getInfo = function(jobURL){
  url = paste0('https://stackoverflow.com/jobs/',jobURL,"/")
  job_page <- read_html(url)
  jobInfo <- html_nodes(job_page, ".mb24+ .mb32 .fc-medium") %>%  html_text()
  
  jobInfo = jobInfo %>%
    str_remove_all(" ") %>%
    str_remove_all("\r\r\r\r\r") %>%
    str_remove_all("\r\r\r") %>%
    str_remove_all("\n")
  
  results = c()
  counter = 1
  
  results = data.frame("Jobtype" = character(),
                       "Experience_level" = character(),
                       "Role" = character(),
                       "Industry" = character(),
                       "Company_size" = character(),
                       "Company_type" = character(),
                       stringsAsFactors = FALSE)
  
  terms = c("Jobtype:\r",
            "Experiencelevel:\r",
            "Role:\r",
            "Industry:\r",
            "Companysize:\r",
            "Companytype:\r")
  
  for (term in terms) {
    tmp = strsplit(jobInfo, term)[[1]]
    
    if(length(tmp)==2){
      results[1, counter] = strsplit(tmp[2], "\r")[[1]][1]
    }else{
      results[1, counter] = NA
    }
    
    counter = counter + 1
  }
  
  return(results)
}


plot( as.factor(str_remove_all(details$Company_size, "people"))) 










