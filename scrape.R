library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(writexl)
library(httr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# medRxiv
## medRxiv lists covid related papers in an RSS feed at https://connect.medrxiv.org/relate/content/181
## the raw RSS feed is available at https://connect.medrxiv.org/relate/feed/181

# # download latest from RSS feed
# filename <- sprintf("medrxiv%s.txt", format(Sys.time(),'_%Y%m%d_%H%M'))
# download_xml("https://connect.medrxiv.org/relate/feed/181",file=filename)

# # or import previous download
filename <- "medrxiv_20200619_1120.txt"
# 
import.med <- read_xml(filename)
ns <- xml_children(import.med) # nodeset
# xml_find_all(ns[[3]],"//title")
# xml_find_all(ns,"//item")
xml_children(ns[[3]])

# can't get xml_find_all working as expected so going a different route via a list
med.list <- as_list(import.med)[[1]] 
ll <- length(med.list)-2
med.df <- data.frame(title=rep(NA,ll),first.author=rep(NA,ll),url=rep(NA,ll),abstract=rep(NA,ll),doi=rep(NA,ll))

for(i in 3:length(med.list)) { #first two items in list are not papers
  ind = i - 2
  med.df$title[ind] <- med.list[i]$item$title[[1]]
  med.df$first.author[ind] <- med.list[i]$item$creator[[1]] # not sure how to access other authors if we wanted to as all called creator
  med.df$url[ind] <- med.list[i]$item$link[[1]]
  med.df$abstract[ind] <- med.list[i]$item$description[[1]]
  med.df$doi[ind] <- med.list[i]$item$identifier[[1]]
}

# search for keywords and label rows
med.df$title.model <- grepl(med.df$title, pattern = "model*",ignore.case = T) # title contains "model" 
table(med.df$title.model) # 674
med.df$abstract.model <- grepl(med.df$abstract, pattern = "model*",ignore.case = T) # abstract contains "model" 
table(med.df$abstract.model) # 2197
med.df$abstract.uk <- grepl(med.df$abstract, pattern = "united kingdom | great britain | england",ignore.case = T) # abstract contains (“United Kingdom” or “Great Britain” or “England”)
table(med.df$abstract.uk) # 114
# abstract contains "model" AND  (“United Kingdom” or “Great Britain” or “England”)
with(med.df,table(abstract.uk,abstract.model))  # 61 contain both

# save excel file so can filter and review there
write_xlsx(med.df,"medrxiv_export.xlsx",col_names = TRUE)



# arXiv
## arXiv has an API for exporting metadata. See https://arxiv.org/help/api/index
## simplest thing to do to match above (because I know little about APIs) is pull the search results using http://export.arxiv.org/api/query?search_query=
## syntax described here https://arxiv.org/help/api/user-manual#Quickstart and https://arxiv.org/help/api/user-manual#query_details
## search query used in default is title=COVID-19; OR abstract=SARS-CoV-2; OR abstract=COVID-19; OR title=SARS-CoV-2; OR title=coronavirus; OR abstract=coronavirus
## set start and max results so can export in multiple bunches: We recommend to refine queries which return more than 1,000 results, or at least request smaller slices.
#

# searchURL <- "http://export.arxiv.org/api/query?search_query=ti:COVID-19+OR+abs:COVID-19+OR+ti:SARS-CoV-2+OR+abs:SARS-CoV-2+OR+ti:coronavirus+OR+abs:coronavirus&start=0&max_results=2000" # This should be 1,382 results based on HTML search page but API only returns 492 

searchURL <- "http://export.arxiv.org/api/query?search_query=ti:COVID+OR+abs:COVID+OR+ti:CoV+OR+abs:CoV+OR+ti:coronavirus+OR+abs:coronavirus&start=0&max_results=2000" # simplified COVID-19 to COVID and Sars-CoV-2 to CoV and this returns 1432 results. Perhaps come back to this issue. 

## download latest
# filenameA <- sprintf("arxiv%s.txt", format(Sys.time(),'_%Y%m%d_%H%M'))
# download_xml(searchURL,file=filenameA)

# # or import previous download
filenameA <- "medrxiv_20200619_1120.txt"

import.arx <- read_xml(filenameA)
nsA <- xml_children(import.arx)

## same strategy as above
arx.list <- as_list(import.arx)[[1]] 
ll <- length(arx.list)-7
arx.df <- data.frame(title=rep(NA,ll),first.author=rep(NA,ll),url=rep(NA,ll),abstract=rep(NA,ll),doi=rep(NA,ll))

# names of fields are different from above
for(i in 8:length(arx.list)) { #first seven items in list are not papers
  ind = i - 7
  arx.df$title[ind] <- arx.list[i]$entry$title[[1]]
  arx.df$first.author[ind] <- arx.list[i]$entry$author[[1]] #
  arx.df$url[ind] <- arx.list[i]$entry$id[[1]]
  arx.df$abstract[ind] <- arx.list[i]$entry$summary[[1]]
  # no DOI
}

# search for keywords and label rows
arx.df$title.model <- grepl(arx.df$title, pattern = "model*",ignore.case = T) # title contains "model" 
table(arx.df$title.model) # 287
arx.df$abstract.model <- grepl(arx.df$abstract, pattern = "model*",ignore.case = T) # abstract contains "model" 
table(arx.df$abstract.model) # 763
arx.df$abstract.uk <- grepl(arx.df$abstract, pattern = "united kingdom | great britain | england",ignore.case = T) # abstract contains (“United Kingdom” or “Great Britain” or “England”)
table(arx.df$abstract.uk) # 8
# abstract contains "model" AND  (“United Kingdom” or “Great Britain” or “England”)
with(arx.df,table(abstract.uk,abstract.model))  # 5 contain both

# save excel file so can filter and review there
write_xlsx(arx.df,"arxiv_export.xlsx",col_names = TRUE)

