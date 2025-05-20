library(httr)
library(jsonlite)
library(tidyverse)
library(plyr)
library(lubridate)
library(data.table)

###### API key for the account ######
secret_api_key <- ""

###### To check how many searches left on account ######
GET(
  url = "https://serpapi.com/account",
  query = (list(api_key=secret_api_key))
) -> account
account

###### Function to return the results from serpApi ######
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
  df1$sitelinks <- NULL
  
  if(length(df1$position) >= 99) {  ## Some of the results returned only 99 (rather than 100) results
    ## before paginating to page 2 - this 
    GET(
      url = "https://serpapi.com/search",
      query = list(q=x,
                   api_key=secret_api_key, 
                   num="100",
                   start=length(df1$position)+1,  ## Changing this operator to 100 pulls the data from the second page
                   device="desktop",
                   filter="0")
    ) -> res2
    data2 = fromJSON(rawToChar(res2$content))
    df2 <- rbind.fill(data2$organic_results)
    df2$rich_snippet <- NULL
    df2$sitelinks <- NULL
    
    print(paste(x, " ", "along with", length(df2$position))) ## This prints a message along with the number of cases on page 2
  } else {
    print(paste(x, " ", "with", length(df1$position)))  ## Prints the number of cases in each date range
    return(df1)
  }
  
  if(length(df2$position) >= 99) {  ## Some of the results returned only 99 (rather than 100) results
    ## before paginating to page 3 - this 
    GET(
      url = "https://serpapi.com/search",
      query = list(q=x,
                   api_key=secret_api_key, 
                   num="100",
                   start=length(df1$position)+101,  ## Changing this operator to 100 pulls the data from the second page
                   device="desktop",
                   filter="0")
    ) -> res3
    data3 = fromJSON(rawToChar(res3$content))
    df3 <- rbind.fill(data3$organic_results)
    df3$rich_snippet <- NULL
    df3$sitelinks <- NULL
  
    print(paste(x, " ", "along with", length(df3$position))) ## This prints a message along with the number of cases on page 2
  } else {
    print(paste(x, " ", "with", length(df2$position)))  ## Prints the number of cases in each date range
    df_final <- rbind.fill(df1, df2) ## rbind df for pages 1 and 2
    return(df_final)
  }
  
  if(length(df3$position) >= 99) {  ## Some of the results returned only 99 (rather than 100) results
    ## before paginating to page 4 - this 
    GET(
      url = "https://serpapi.com/search",
      query = list(q=x,
                   api_key=secret_api_key, 
                   num="100",
                   start=length(df1$position)+201,  ## Changing this operator to 100 pulls the data from the second page
                   device="desktop",
                   filter="0")
    ) -> res4
    data4 = fromJSON(rawToChar(res4$content))
    df4 <- rbind.fill(data4$organic_results)
    df4$rich_snippet <- NULL
    df4$sitelinks <- NULL
    
    print(paste(x, " ", "along with", length(df4$position))) ## This prints a message along with the number of cases on page 2
  } else {
    print(paste(x, " ", "with", length(df3$position)))  ## Prints the number of cases in each date range
    df_final <- rbind.fill(df1, df2, df3) ## rbind df for pages 1 to 4
    return(df_final)
  }
  
  if(length(df4$position) >= 99) {  ## Some of the results returned only 99 (rather than 100) results
    ## before paginating to page 4 - this 
    GET(
      url = "https://serpapi.com/search",
      query = list(q=x,
                   api_key=secret_api_key, 
                   num="100",
                   start=length(df1$position)+301,  ## Changing this operator to 100 pulls the data from the second page
                   device="desktop",
                   filter="0")
    ) -> res5
    data5 = fromJSON(rawToChar(res5$content))
    df5 <- rbind.fill(data5$organic_results)
    df5$rich_snippet <- NULL
    df5$sitelinks <- NULL
    df_final <- rbind.fill(df1, df2, df3, df4, df5) ## rbind df for pages 1 to 5
    
    print(paste(x, " ", "along with", length(df5$position))) ## This prints a message along with the number of cases on page 2
    return(df_final)                                         ## to the console so I know which cases have paginated
  } else {
    print(paste(x, " ", "with", length(df4$position)))  ## Prints the number of cases in each date range
    df_final <- rbind.fill(df1, df2, df3, df4) ## rbind df for pages 1 and 2
    return(df_final)
  }
  
}

###### Build date variables for scrape date ranges ######
start_date <- mdy("12-31-09")
end_date <- mdy("6-1-20")
n_days <- interval(start_date,end_date)/days(1) ## Return number of days between the two dates

three_hundred_sixty_days <- seq(0,3805,361) ## 361 days (rather than 360) to account for the before/after overlap
before360 <- start_date + three_hundred_sixty_days
before360 <- c(before360, end_date)
before360 <- before360[-1]
after360 <- start_date + (three_hundred_sixty_days)-1
after360[1] <- start_date
before360
after360

one_hundred_eighty_days <- seq(0,3805,181) ## 181 days (rather than 180) to account for the before/after overlap
before180 <- start_date + one_hundred_eighty_days
before180 <- c(before180, end_date)
before180 <- before180[-1]
after180 <- start_date + (one_hundred_eighty_days)-1
after180[1] <- start_date
before180
after180

thirty_days <- seq(0,3805,31) ## 31 days (rather than 30) to account for the before/after overlap
before30 <- start_date + thirty_days
before30 <- c(before30, end_date)
before30 <- before30[-1]
after30 <- start_date + (thirty_days)-1
after30[1] <- start_date
before30 
after30

ninety_days <- seq(0,3805,91) ## 91 days (rather than 30) to account for the before/after overlap
before90 <- start_date + ninety_days
before90 <- c(before90, end_date)
before90 <- before90[-1]
after90 <- start_date + (ninety_days)-1
after90[1] <- start_date
before90 
after90

ten_days <- seq(0,3805,11) ## 11 days (rather than 10) to account for the before/after overlap
before10 <- start_date + ten_days
before10 <- c(before10, end_date)
before10 <- before10[-1]
after10 <- start_date + (ten_days)-1
after10[1] <- start_date
before10
after10 

rm(start_date,
   end_date,
   n_days,
   one_hundred_eighty_days,
   three_hundred_sixty_days,
   thirty_days,
   ten_days)

###### Scrape for each of the sources ######
###### NEW ZEALAND ######

# tvnz.co.nz/
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:tvnz.co.nz humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# newshub.co.nz
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:newshub.co.nz humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# stuff.co.nz
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:stuff.co.nz humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### AUSTRALIA ######
# The Monthly
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:themonthly.com.au humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# The New Daily
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:thenewdaily.com.au humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# 9 News
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:9news.com.au humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# Sky News AU
search_terms <- "site:skynews.com.au humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

#SBS
search_terms <- paste0("/XXXXXXXXXX", after30," before:", before30)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:sbs.com.au/news humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### CHINA ######
# ECNS
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:ecns.cn humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# CCTV
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:cctv.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# CGTN
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:cgtn.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# CCTV+
search_terms <- "site:cctvplus.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### HONG KONG ######
#RTHK
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:rthk.hk humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thestandard.com.hk
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:thestandard.com.hk humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### JAPAN ######
# NHK
search_terms <- "site:www3.nhk.or.jp/nhkworld/en/news/ humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### SOUTH KOREA ######
# hani.co.kr
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:english.hani.co.kr humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### TAIWAN ######
# Taiwan Times
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:taiwannews.com.tw humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# RTI Taiwan
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:en.rti.org.tw humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### MALAYSIA ###### 
# The Star
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:thestar.com.my humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# malaysiakini.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:malaysiakini.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### PHILIPPINES ###### 
# ABS-CBN
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:news.abs-cbn.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# Rappler
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:rappler.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# GMA
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:gmanetwork.com/news humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# PTV
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:ptvnews.ph humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### RUSSIA ###### 
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:rt.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### FRANCE ###### 
# France24
search_terms <- paste0("/XXXXXXXXXX", after10," before:", before10)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:france24.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### TURKEY ###### 

# TRT World
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:trtworld.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# hurriyetdailynews.com
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:hurriyetdailynews.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### CANADA ###### 

# globalnews.ca
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:globalnews.ca humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)


###### UK ###### 

# reuters.com
search_terms <- paste0("/XXXXXXXXXX", after10," before:", before10)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:reuters.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# bbc.co.uk
search_terms <- paste0("/XXXXXXXXXX", after10," before:", before10)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:bbc.com/news humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# The Economist
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:economist.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# Reuters Foundation
search_terms <- paste0("/XXXXXXXXXX", after30," before:", before30)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:news.trust.org humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)



###### CUBA ######

# Granma
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:en.granma.cu humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

#cubanews.acn.cu
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:cubanews.acn.cu humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### VENEZUELA ######

# Telesur
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:telesurenglish.net humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### US ######

# thenewhumanitarian.org
search_terms <- paste0("/XXXXXXXXXX", after10," before:", before10)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:thenewhumanitarian.org humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# bloomberg.com
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:bloomberg.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# time.com
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:time.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# ChicagoTribune.com
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:chicagotribune.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# apnews.com
search_terms <- paste0("/XXXXXXXXXX", after30," before:", before30)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:apnews.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# vice.com
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:vice.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# humanosphere.org
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:humanosphere.org humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### PAKISTAN ######

# geo.tv
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:geo.tv humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# dunyanews.tv
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:dunyanews.tv humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thenews.com.pk
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:thenews.com.pk humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### QATAR ######

# aljazeera.com
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### SWITZERLAND ######

# worldradio.ch
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thelocal.ch
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# swissinfo.ch
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### MALTA ######

# timesofmalta.com
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)


###### ANGOLA ######

# angop.ao
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### ALGERIA ######

# echoroukonline.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

#elmoudjahid.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

#aps.dz
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### BAHAMAS #####

# ewnews.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thenassauguardian.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# bahamaspress.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# bahamasnews.net
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)
  
###### BOTSWANA #####

# mmegi.bw
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thepatriot.co.bw
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# sundaystandard.info
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# weekendpost.co.bw 
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### BRAZIL #####

# agenciabrasil.ebc.com.br 
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# riotimesonline.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### EGYPT #####

# nileinternational.net/en
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# egyptindependent.com
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# egyptnews.net
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# madamasr.com/en
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# masress.com/en
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# english.ahram.org.eg
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# egypttoday.com
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### ETHIOPIA ######

# ena.et/en
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# FANABC
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# addisstandard.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)
###### THE GAMBIA ######

# standard.gm
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# fatunetwork.net
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# freedomnewspaper.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# gainako.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# kaironews.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thepoint.gm
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# foroyaa.net
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### GHANA ######

# thedailystatesman.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# ghonetv.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# 3news.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# businessworldghana.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# gbcghana.com
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# graphic.com.gh
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# ghanastar.com
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# pulse.com.gh
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### JAMAICA ######

# radiojamaicanewsonline.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# jamaicaobserver.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# jamaica-star.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# jamaicanews.net
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# jamaica-gleaner.com
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### KENYA ######

# citizentv.co.ke
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# standardmedia.co.ke
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# capitalfm.co.ke
search_terms <- paste0("/XXXXXXXXXX", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# tuko.co.ke
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# kenya-today.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# mediamaxnetwork.co.ke
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)


###### LIBERIA ######

# liberianobserver.com
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# kmtvliberia.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# themonroviatimes.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### LIBYA ######

# en.libyan-cna.net
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# lana-news.ly
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# libyanexpress.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# libyaobserver.ly
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# libyaherald.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)


###### MALAWI ######

# mbc.mw
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# malawi24.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# times.mw
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# zodiakmalawi.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# maravipost.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# nyasatimes.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# mwnation.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# manaonline.gov.mw
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### NAMIBIA ######

# economist.com.na
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# namibiansun.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# neweralive.na
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# namibian.com.na
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# nampa.org
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# nbc.na
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thevillager.com.na
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# southerntimesafrica.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thepatriot.com.na
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### RWANDA ######

# bwiza.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# ktpress.rw
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# nonaha.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# therwandan.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# rnanews.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# newtimes.co.rw
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### SOMALIA ######

# allsbc.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# sntv.so
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# hiiraan.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# goobjoog.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# ilwareed.info
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# mogtimes.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# sonna.so
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# hargeisapress.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# mareeg.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### SOUTH SUDAN ######

# nyamile.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# ssnewsnow.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# southsudannation.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# southsudannewsagency.org
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# jubamonitor.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# radiotamazuj.org
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# eyeradio.org
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# catholicradionetwork.org
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### NIGERIA ######
# ait.live
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# nannews.ng
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# channelstv.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# nta.ng
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# radionigeria.gov.ng
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# von.gov.ng
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# tvcnews.tv
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thenewsnigeria.com.ng
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# punchng.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### UGANDA ######

# nbs.ug
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# ntv.co.ug
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# ugandaradionetwork.net
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# monitor.co.ug
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# pmldaily.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# chimpreports.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# capitalradio.co.ug
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### ZAMBIA ######

# znbc.co.zm
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# muvitv.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# qfmzambia.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# daily-mail.co.zm
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# times.co.zm
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# zambiawatchdog.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# zambianeye.com
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# lusakatimes.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# mwebantu.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### SOUTH AFRICA ######

# capetalk.co.za
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# businesslive.co.za
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# news24.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# iol.co.za
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# sabcnews.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# enca.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# channelafrica.co.za
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)


###### ZIMBABWE ######

# thepatriot.co.zw
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# hararepost.co.zw
search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# nehandaradio.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# radiovop.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# zimeye.net
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thezimbabwemail.com
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# herald.co.zw
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# sundaymail.co.zw
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thestandard.co.zw
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# newsday.co.zw
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# theindependent.co.zw
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("/XXXXXXXXXX")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### INDIA ######

# justearthnews.com
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:justearthnews.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### GREECE ######

# en.protothema.gr
search_terms <- paste0("/XXXXXXXXXX", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:en.protothema.gr humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### ARGENTINA ######

# batimes.com.ar
search_terms <- paste0("/XXXXXXXXXX", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:batimes.com.ar humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("/XXXXXXXXXX","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### Download additional 2 months ######

###### Build date variables for scrape date ranges ######
start_date <- mdy("5-31-20")
end_date <- mdy("8-9-20")
n_days <- interval(start_date,end_date)/days(1) ## Return number of days between the two dates

thirty_days <- seq(0,70,31) ## 31 days (rather than 30) to account for the before/after overlap
before30 <- start_date + thirty_days
before30 <- c(before30, end_date)
before30 <- before30[-1]
after30 <- start_date + (thirty_days)-1
after30[1] <- start_date
before30 
after30

ten_days <- seq(0,70,11) ## 11 days (rather than 10) to account for the before/after overlap
before10 <- start_date + ten_days
before10 <- c(before10, end_date)
before10 <- before10[-1]
after10 <- start_date + (ten_days)-1
after10[1] <- start_date
before10
after10 

rm(start_date,
   end_date,
   n_days,
   thirty_days,
   ten_days)

###### Iterate through list of new sources to scrape ######

list_sources <- readLines("/XXXXXXXXXX")
list_countries <- readLines("/XXXXXXXXXX")

for(i in 1:length(list_sources)){
  search_terms <- paste0("/XXXXXXXXXX", eval(list_sources[i])," humanitarian* after:", after10," before:", before10)
  data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
  data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
  data.scrape_df$position <- NULL
  data.scrape_df$related_pages_link <- NULL
  name_file <- paste0(eval(list_countries[i]),"_", eval(list_sources[i]),"_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
  if(is.null(data.scrape_df) == FALSE)
    {fwrite(file = eval(name_file), x = data.scrape_df)
    }
}