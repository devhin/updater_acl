library(readxl)
library(xlsx)
library(urltools)
library(rvest)
library(stringr)

setwd("C:/Users/olivier.bertrand/Documents/Projets_R/09_updater_acl")

# load data
annuaire <- as.data.frame(read_excel("annuaire.xlsx", sheet = 1))
url_root <- "https://www.google.com/search?q=site%3A+linkedin"

# create result data.frame
result <- data.frame(matrix(0, nrow(annuaire), 7),
                     stringsAsFactors=F)
colnames(result)=c("name",
           "surname",
           "last_company",
           "scraped_company",
           "scraped_position",
           "scraped_headline",
           "profile_link")

for (i in 1:nrow(annuaire)) {
  # get data
  name <- tolower(annuaire[i,1])
  surname <- tolower(annuaire[i,2])
  last_company <- tolower(annuaire[i,3])
  url <- (paste(url_root,
                       URLencode(name)
                       ,URLencode(surname)
                       ,URLencode(last_company)
                       ,sep = "+"))
  #print(url)
  google_search <- read_html(url)
  
  # get headline
  headlines <- google_search %>% html_nodes(xpath='//a') %>% html_text()
  for (hl in headlines) {
    if (grepl(paste0("LinkedInhttps://fr.linkedin.com > ", substr(name,1,2)), hl)) {
      scraped_headline <- hl
      position_index_start <- str_locate(scraped_headline, " - ")
      position_index_stop <- str_locate(scraped_headline, "LinkedInhttps")
      headline_truncated <- substr(scraped_headline, position_index_start+3, position_index_stop-4)
      position_index_mid <- str_locate(headline_truncated, " - ")
      position <- substr(headline_truncated, 1, position_index_mid-1)
      company <- substr(headline_truncated, position_index_mid+3, nchar(headline_truncated))
      print(scraped_headline)
      print(position)
      print(company)
      break
    }
  }
  
  # get profile link
  links <- html_attr(html_nodes(google_search, xpath='//a'), 'href')
  links <- gsub('/url\\?q=','',sapply(strsplit(links[as.vector(grep('url',links))],split='&'),'[',1))
  for (lk in links) {
    if(grepl("https://fr.linkedin.com/in", lk)) {
      link <- lk
      print(link)
      break
    }
  }
  
  # fill result data.frame
  result$name[i] <- name
  result$surname[i] <- surname
  result$last_company[i] <- last_company
  try(result$scraped_headline[i] <- scraped_headline)
  try(result$scraped_position[i] <- position)
  try(result$scraped_company[i] <- company)
  try(result$profile_link[i] <- link)
  
  # Sys.sleep(2)
}

write.xlsx2(result, file = "scraped_data.xlsx", sheetName="scraping",
            col.names=TRUE, row.names=TRUE, append=FALSE, password=NULL)