library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(writexl)

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
med.df$abstract.uk <- grepl(med.df$abstract, pattern = "united kingdom | great britain | england",ignore.case = T) # abstract contains "model" 
table(med.df$abstract.uk) # 114
# abstract contains "model" AND  (“United Kingdom” or “Great Britain” or “England”)
with(med.df,table(abstract.uk,abstract.model))  # 61 contain both

# save excel file so can filter and review there
write_xlsx(med.df,"medrxiv_export.xlsx",col_names = TRUE)





# arXiv



# SMDM