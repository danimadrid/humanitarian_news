library(httr)
library(jsonlite)
library(tidyverse)
library(plyr)
library(lubridate)

## Read in all SerpApi files
al_jaz_df <- read.csv("Al_Jazeera.csv", stringsAsFactors = F)
ap_df <- read.csv("APNews.csv", stringsAsFactors = F)
bbc_df <- read.csv("bbc.csv", stringsAsFactors = F)
cgtn_df <- read.csv("CGTN.csv", stringsAsFactors = F)
en_CCTV_df <- read.csv("enCCTV.csv", stringsAsFactors = F)
reuters_df <- read.csv("Reuters.csv", stringsAsFactors = F)
rt_df <- read.csv("RT.csv", stringsAsFactors = F)

## Remove variables from en_CCTV file
en_CCTV_df$position <- NULL
en_CCTV_df$related_pages_link <- NULL

## Attach a variable denoting the name
al_jaz_df$organisation <- "Al Jazeera"
ap_df$organisation <- "AP News"
bbc_df$organisation <- "BBC"
cgtn_df$organisation <- "CGTN"
en_CCTV_df$organisation <- "English CCTV"
reuters_df$organisation <- "Reuters"
rt_df$organisation <- "RT"

## Bind all the files
serp_api_all_df <- rbind.fill(al_jaz_df, ap_df, bbc_df, cgtn_df, 
                              en_CCTV_df, reuters_df, rt_df)
length(unique(serp_api_all_df$link))

## Remove any duplicate cases
serp_unique_locs <- match(unique(serp_api_all_df$link), serp_api_all_df$link)
serp_api_all_df <- serp_api_all_df[serp_unique_locs, ]

write.csv(serp_api_all_df, file = "SerpApi_all.csv", row.names = F)


#### Read in gdelt file and select unique cases
## gdelt file
gdelt_df <- read.csv("bq-results-20200501-190444-69tqqtgvzaw.csv", stringsAsFactors = F)
unique(gdelt_df$DocumentIdentifier)

## Location of unique cases & create df
gdelt_unique_locs <- match(unique(gdelt_df$DocumentIdentifier), gdelt_df$DocumentIdentifier)
gdelt_df <- gdelt_df[gdelt_unique_locs, ]

write.csv(gdelt_df, file = "gdelt_all.csv", row.names = F)

#####################################################################################
##### Create source files together ##################################################
#####################################################################################
###############
### SerpApi ###
###############
serpapi_df <- read.csv("SerpApi_all.csv", stringsAsFactors = F)
colnames(serpapi_df)

## Variable to show source
serpapi_df$source <- rep("serpapi", length(serpapi_df$link))

## Create df from three URL, name and source variables
serp_api_stripped_df <- serpapi_df[, c(2, 8, 9)]
write.csv(serp_api_stripped_df, file = 'serp_connect.csv', row.names = F)

################
### News Api ###
################
news_api_df <- read.csv("google_news_all.csv", stringsAsFactors = F)
news_api_df$source <- "newsapi"
colnames(news_api_df)

## Remove the content variable to fix csv formatting issues
news_api_df$content <- NULL
write.csv(news_api_df, file = "google_news_no_content.csv", row.names = F)

## Create new df from url, name and source
news_api_stripped <- news_api_df[, c(4, 9, 10)]
write.csv(news_api_stripped, file = "newsapi_connect.csv", row.names = F)

#############
### GDELT ###
#############
setwd("~/Documents/Humanitrain Corpus RA - Kate & Dani/GDELT")
dir()

gdelt_df <- read.csv("gdelt_all.csv", stringsAsFactors = F)
colnames(gdelt_df)

## Attach a name and source variable
gdelt_df$name <- NA
gdelt_df$source <- "gdelt"

gdelt_stripped <- gdelt_df[, c(1, 4, 5)]
write.csv(gdelt_stripped, file = "gdelt_connect.csv", row.names = F)


#######################################################################
### Bind the serpapi, newsapi and gdelt files and remove dulpicates ###
#######################################################################
serpapi_connect <- read.csv("serp_connect.csv", stringsAsFactors = F)
colnames(serpapi_connect) <- c("url", "name", "source")

newsapi_connect <- read.csv("newsapi_connect.csv", stringsAsFactors = F)
colnames(newsapi_connect) <- c("url", "name", "source")

gdelt_connect <- read.csv("gdelt_connect.csv", stringsAsFactors = F)
colnames(gdelt_connect) <- c("url", "name", "source")

## Bind all three dfs
all_connect <- rbind.fill(serpapi_connect, newsapi_connect, gdelt_connect)
length(unique(all_connect$url)) ## 67467 duplicates

## Pull the row numbers of the unique cases
connect_unique_locs <- match(unique(all_connect$url), all_connect$url)
## Create a new df from these locations
all_connect <- all_connect[connect_unique_locs, ]