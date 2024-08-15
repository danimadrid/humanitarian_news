#setwd("~/Documents/Humanitrain Corpus RA - Kate & Dani/Google News")
setwd("~/Documents/Humanitrain Corpus RA - Kate & Dani/Google News/Scraped files")

library(httr)
library(jsonlite)
library(tidyverse)
library(plyr)
library(lubridate)
#install.packages("newsanchor")
library(newsanchor)
library(openxlsx)


set_api_key(path = "~/.Renviron")


#### Function to scrape the data from the API
google_news_scrape <- function(x, y, z) {
  res <- get_everything_all(query = "humanitarian*", language = "en",
                            from = x, to = y)
  
  res_df <- res[[2]]
  #write.csv(res_df, file = paste0("google_", y, "_", x, ".csv"))
  write.xlsx(res_df, paste(z, ".xlsx", sep = ""))
  print(x)
  return(res_df)
}

### Construct date format
# From and to dates fro the function
z <- 1:12
from2017 <- paste0("2017", "-", z, "-", "01", sep = "")
from2018 <- paste0("2018", "-", z, "-", "01", sep = "")
from2019 <- paste0("2019", "-", z, "-", "01", sep = "")
from2020 <- paste0("2020", "-", z, "-", "01", sep = "")

from_all <- as.Date(c(from2017, from2018, from2019, from2020))
to_all <- from_all - 1
to_all <- to_all[-1]
from_all[10:40]
to_all[10:40]

## Months for file names
months_2017 <- paste0(month.name[10:12], "_2017")
months_2018 <- paste0(month.name, "_2018")
months_2019 <- paste0(month.name, "_2019")
months_2020 <- paste0(month.name[1:4], "_2020")

all_months <- c(months_2017, months_2018, months_2019, months_2020)

## Runs the final scrape
scrape_df <- mapply(google_news_scrape, from_all[10:40], to_all[10:40], all_months)
## NOTE: Some of the files returned are corrupted - saving to csv format does not fix this
## But saving to xlsx means that Excel will recover the corrupted data when opening
### After debugging the files it is the 'content' variable which is the problem
### Moving this variable allows the files to be opened without any problem


## Function to read xlsx files and return df
file_bind <- function (x) {
  df <- read.xlsx(x)
  print(x)
  return(df)
}
## Run function over dir() files
all_dfs <- lapply(dir()[18:48], file_bind)
length(all_dfs)

## Bind all dfs to a single df
all_cases_df <- do.call(rbind.fill, all_dfs)

## Write csv file for all cases
write.csv(all_cases_df, "google_news_all.csv", row.names = F)


##############################################################
##### Remove the duplicate cases #############################
##############################################################
all_df <- read.csv("google_news_all.csv", stringsAsFactors = F)
length(unique(all_df$url))

unique_locs <- match(unique(all_df$url), all_df$url) ## match returns the first case of the URL

all_df <- all_df[unique_locs, ] ## Create df from unique URL cases

write.csv(all_df, "google_news_all.csv", row.names = F)








