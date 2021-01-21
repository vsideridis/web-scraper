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






getNumOfJobs = function(job){
  
  job = paste(strsplit(job, " ")[[1]], collapse = '+')
  
  url = paste0('https://stackoverflow.com/jobs?q=', job)
  page <- read_html(url)
  numberOfJobs <- html_nodes(page, "#content .fc-light") %>%  html_text()
  
  numberOfJobs = numberOfJobs %>%
    str_remove_all(" ") %>%
    str_remove_all("\r") %>%
    str_remove_all("\n") %>%
    str_remove_all("jobs")

  return(numberOfJobs)
}





getJobDetails = function(job){
  
  job = paste(strsplit(job, " ")[[1]], collapse = '+')
  url = paste0('https://stackoverflow.com/jobs?q=', job)
  page <- read_html(url)
  
  links <- html_nodes(page, ".stretched-link") %>% html_attr("href")
  links = unlist(lapply(links, function(tmp) strsplit(tmp, "\\Q/\\E")[[1]][3]))
  
  details = c()
  for(tmpURL in links){
    details = rbind(details, getInfo(tmpURL))
  }
  return(details)
}




