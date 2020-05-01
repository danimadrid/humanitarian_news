library(httr)
library(jsonlite)
library(tidyverse)
library(plyr)
library(lubridate)

## API key for the account
secret_api_key <- "9750a4cd8b842f1ef20d1b01ac23d84f5a013bc313a034288cac5d88eb7d206a"

### To check how many searches left on account
GET(
  url = "https://serpapi.com/account",
  query = (list(api_key=secret_api_key))
) -> account
account


## Function to return the results from serpApi
## Using a date range of 10 days meant almost all cases could be dealt with this function
## The 5 cases that could were dealt with manually

get_all_links <- function(x) {
  ## Get call to return the first page of google results
  GET(        
    url = "https://serpapi.com/search",
    query = list(q=x,
                 api_key=secret_api_key, 
                 num="100",
                 start="0",
                 google_domain="google.com",
                 hl="en",
                 device="desktop",
                 filter="0")
  ) -> res
  
  data = fromJSON(rawToChar(res$content)) ## convert to JSON format
  df1 <- rbind.fill(data$organic_results) ## create df from data on first page
  df1$rich_snippet <- NULL  ## This removes a variable in some of the dataframes that rbind.fill can't handle
  
  if(length(df1$position) >= 99) {  ## Some of the results returned only 99 (rather than 100) results
                                    ## before paginating to page 2 - this 
   GET(
      url = "https://serpapi.com/search",
      query = list(q=x,
                   api_key=secret_api_key, 
                   num="100",
                   start="100",  ## Changing this operator to 100 pulls the data from the second page
                   device="desktop",
                   filter="0")
    ) -> res2
    data2 = fromJSON(rawToChar(res2$content))
    df2 <- rbind.fill(data2$organic_results)
    df2$rich_snippet <- NULL
    df_final <- rbind.fill(df1, df2) ## rbind df for pages 1 and 2
    
    
    print(paste(x, " ", "along with", length(df2$position))) ## This prints a message along with the number of cases on page 2
    return(df_final)                                         ## to the console so I know which cases have paginated
  } else {
    print(paste(x, " ", "with", length(df1$position)))  ## Prints the number of cases in each date range
    return(df1)
  }
}


## Build date variables for scrape date ranges
start_date <- mdy("12-31-15")
end_date <- mdy("4-30-20")
n_days <- interval(start_date,end_date)/days(1) ## Return number of days between the two dates

eleven_days <- seq(0,1581,11) ## 11 days (rather than 10) to account for the before/after overlap

before <- start_date + eleven_days
before <- c(before, end_date)
before <- before[-1]
before ## Vector before dates

after <- start_date + eleven_days
after[1] <- start_date
after ## Vector of after dates


###### Scrape for each of the sources
# AP News scrape
search_terms <- paste0("site:apnews.com humanitarian* after:", after," before:", before)
apnews <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
apnews_df <- do.call(rbind.fill, apnews) ## Binds each result in the apnews list in to a single df
apnews_df$position <- NULL
apnews_df$related_pages_link <- NULL
setwd("~/Documents/Humanitrain Corpus RA - Kate & Dani/SerpApi/AP News")
write.csv(apnews_df, 'APNews.csv', row.names = F)

# Reuters scrape
reuters_search_terms <- paste0("site:reuters.com humanitarian* after:", after," before:", before)
reuters <- lapply(reuters_search_terms, get_all_links)
reuters_df <- do.call(rbind.fill, reuters)
reuters_df <- rbind.fill(reuters_df, reuters_overflow_df)
reuters_df$position <- NULL
reuters_df$related_pages_link <- NULL
setwd("~/Documents/Humanitrain Corpus RA - Kate & Dani/SerpApi/Reuters")
write.csv(reuters_df, 'Reuters.csv', row.names = F)

# RT scrape
RT_search_terms <- paste0("site:rt.com humanitarian* after:", after," before:", before)
RT <- lapply(RT_search_terms, get_all_links)
RT_df <- do.call(rbind.fill, RT)
RT_df$position <- NULL
RT_df$related_pages_link <- NULL
setwd("~/Documents/Humanitrain Corpus RA - Kate & Dani/SerpApi/RT")
write.csv(RT_df, 'RT.csv', row.names = F)

# enCCTV scrape
enCCTV_terms <- paste0("site:english.cctv.com humanitarian* after:", after," before:", before)
enCCTV <- lapply(enCCTV_terms, get_all_links)
enCCTV_df <- do.call(rbind.fill, enCCTV)
enCCTV$position <- NULL
enCCTV$related_pages_link <- NULL
setwd("~/Documents/Humanitrain Corpus RA - Kate & Dani/SerpApi/English CCTV")
write.csv(enCCTV_df, 'enCCTV.csv', row.names = F)
### Last case needs checking in a lot of detail ###

# news.cgtn.com
CGTN_terms <- paste0("site:news.cgtn.com humanitarian* after:", after," before:", before)
CGTN <- lapply(CGTN_terms, get_all_links)
CGTN_df <- do.call(rbind.fill, CGTN)
CGTN_df$position <- NULL
CGTN_df$related_pages_link <- NULL
setwd("~/Documents/Humanitrain Corpus RA - Kate & Dani/SerpApi/CGTN")
write.csv(CGTN_df, 'CGTN.csv', row.names = F)

# aljazeera.com
al_jaz_terms <- paste0("site:aljazeera.com humanitarian* after:", after," before:", before)
al_jaz <- lapply(al_jaz_terms, get_all_links)
al_jaz_df <- do.call(rbind.fill, al_jaz)
al_jaz_df$position <- NULL
al_jaz_df$related_pages_link <- NULL
setwd("~/Documents/Humanitrain Corpus RA - Kate & Dani/SerpApi/Al Jazeera")
write.csv(al_jaz_df, 'Al_Jazeera.csv', row.names = F)

# bbc.com/news
bbc_terms <- paste0("site:bbc.com/news humanitarian* after:", after," before:", before)
bbc <- lapply(bbc_terms, get_all_links)
bbc_df <- do.call(rbind.fill, bbc)
bbc_df$position <- NULL
bbc_df$related_pages_link <- NULL
setwd("~/Documents/Humanitrain Corpus RA - Kate & Dani/SerpApi/BBC")
write.csv(bbc_df, 'bbc.csv', row.names = F)

