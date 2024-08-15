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
search_terms <- paste0("site:tvnz.co.nz humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NZ_TVNZ_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:tvnz.co.nz humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NZ_TVNZ_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# newshub.co.nz
search_terms <- paste0("site:newshub.co.nz humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NZ_newshub_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:newshub.co.nz humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NZ_newshub_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# stuff.co.nz
search_terms <- paste0("site:stuff.co.nz humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NZ_stuffconz_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:stuff.co.nz humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NZ_stuffconz_bydate2_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:stuff.co.nz humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NZ_stuffconz_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### AUSTRALIA ######
# The Monthly
search_terms <- paste0("site:themonthly.com.au humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("AU_themontly_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:themonthly.com.au humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("AU_themontly_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# The New Daily
search_terms <- paste0("site:thenewdaily.com.au humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("AU_thenewdaily_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:thenewdaily.com.au humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("AU_thenewdaily_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# 9 News
search_terms <- paste0("site:9news.com.au humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("AU_ninenews_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:9news.com.au humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("AU_ninenews_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# Sky News AU
search_terms <- "site:skynews.com.au humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("AU_skynews_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

#SBS
search_terms <- paste0("site:sbs.com.au/news humanitarian* after:", after30," before:", before30)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("AU_sbs_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:sbs.com.au/news humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("AU_sbs_bydate2_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:sbs.com.au/news humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("AU_sbs_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### CHINA ######
# ECNS
search_terms <- paste0("site:ecns.cn humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CN_ecns_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:ecns.cn humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CN_ecns_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# CCTV
search_terms <- paste0("site:cctv.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CN_cctv_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:cctv.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CN_cctv_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# CGTN
search_terms <- paste0("site:cgtn.com humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CN_cgtn_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:cgtn.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CN_cgtn_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# CCTV+
search_terms <- "site:cctvplus.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CN_cctvplus_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### HONG KONG ######
#RTHK
search_terms <- paste0("site:rthk.hk humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("HK_rthk_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:rthk.hk humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("HK_rthk_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thestandard.com.hk
search_terms <- paste0("site:thestandard.com.hk humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("HK_thestandard_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:thestandard.com.hk humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("HK_thestandard_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### JAPAN ######
# NHK
search_terms <- "site:www3.nhk.or.jp/nhkworld/en/news/ humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("JP_nhk_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### SOUTH KOREA ######
# hani.co.kr
search_terms <- paste0("site:english.hani.co.kr humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("KR_hankyoreh_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:english.hani.co.kr humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("KR_hankyoreh_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### TAIWAN ######
# Taiwan Times
search_terms <- paste0("site:taiwannews.com.tw humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("TW_taiwannews_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:taiwannews.com.tw humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("TW_taiwannews_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# RTI Taiwan
search_terms <- paste0("site:en.rti.org.tw humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("TW_rti_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:en.rti.org.tw humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("TW_rti_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### MALAYSIA ###### 
# The Star
search_terms <- paste0("site:thestar.com.my humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MY_thestar_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:thestar.com.my humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MY_thestar_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# malaysiakini.com
search_terms <- paste0("site:malaysiakini.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MY_malaysiakini_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:malaysiakini.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MY_malaysiakini_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### PHILIPPINES ###### 
# ABS-CBN
search_terms <- paste0("site:news.abs-cbn.com humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("PH_abs_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:news.abs-cbn.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("PH_abs_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# Rappler
search_terms <- paste0("site:rappler.com humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("PH_rappler_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:rappler.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("PH_rappler_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# GMA
search_terms <- paste0("site:gmanetwork.com/news humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("PH_GMA_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:gmanetwork.com/news humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("PH_GMA_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# PTV
search_terms <- paste0("site:ptvnews.ph humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("PH_PTV_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:ptvnews.ph humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("PH_PTV_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### RUSSIA ###### 
search_terms <- paste0("site:rt.com humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("RU_rt_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:rt.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("RU_rt_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### FRANCE ###### 
# France24
search_terms <- paste0("site:france24.com humanitarian* after:", after10," before:", before10)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("FR_france24_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:france24.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("FR_france24_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### TURKEY ###### 

# TRT World
search_terms <- paste0("site:trtworld.com humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("TR_trt_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:trtworld.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("TR_trt_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# hurriyetdailynews.com
search_terms <- paste0("site:hurriyetdailynews.com humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("TR_hurriyet_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:hurriyetdailynews.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("TR_hurriyet_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### CANADA ###### 

# globalnews.ca
search_terms <- paste0("site:globalnews.ca humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CA_globalnews_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:globalnews.ca humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CA_globalnews_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)


###### UK ###### 

# reuters.com
search_terms <- paste0("site:reuters.com humanitarian* after:", after10," before:", before10)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UK_reuters_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:reuters.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UK_reuters_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# bbc.co.uk
search_terms <- paste0("site:bbc.com/news humanitarian* after:", after10," before:", before10)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UK_bbcnews_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:bbc.com/news humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UK_bbcnews_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# The Economist
search_terms <- paste0("site:economist.com humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UK_economist_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:economist.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UK_economist_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# Reuters Foundation
search_terms <- paste0("site:news.trust.org humanitarian* after:", after30," before:", before30)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UK_reutersfoundation_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:news.trust.org humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UK_reutersfoundation_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)



###### CUBA ######

# Granma
search_terms <- paste0("site:en.granma.cu humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CU_granma_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:en.granma.cu humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CU_granma_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

#cubanews.acn.cu
search_terms <- paste0("site:cubanews.acn.cu humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CU_ACN_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:cubanews.acn.cu humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CU_ACN_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### VENEZUELA ######

# Telesur
search_terms <- paste0("site:telesurenglish.net humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("VE_telesur_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:telesurenglish.net humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("VE_telesur_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### US ######

# thenewhumanitarian.org
search_terms <- paste0("site:thenewhumanitarian.org humanitarian* after:", after10," before:", before10)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("US_thenewhumanitarian_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:thenewhumanitarian.org humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("US_thenewhumanitarian_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# bloomberg.com
search_terms <- paste0("site:bloomberg.com humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("US_bloomberg_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:bloomberg.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("US_bloomberg_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# time.com
search_terms <- paste0("site:time.com humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("US_time_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:time.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("US_time_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# ChicagoTribune.com
search_terms <- paste0("site:chicagotribune.com humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("US_chitribune_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:chicagotribune.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("US_chitribune_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# apnews.com
search_terms <- paste0("site:apnews.com humanitarian* after:", after30," before:", before30)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("US_AP_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:apnews.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("US_AP_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# vice.com
search_terms <- paste0("site:vice.com humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("US_vice_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:vice.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("US_vice_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# humanosphere.org
search_terms <- paste0("site:humanosphere.org humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("US_humanosphere_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:humanosphere.org humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("US_humanosphere_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### PAKISTAN ######

# geo.tv
search_terms <- paste0("site:geo.tv humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("PK_geo_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:geo.tv humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("PK_geo_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# dunyanews.tv
search_terms <- paste0("site:dunyanews.tv humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("PK_dunya_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:dunyanews.tv humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("PK_dunya_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thenews.com.pk
search_terms <- paste0("site:thenews.com.pk humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("PK_thenews_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:thenews.com.pk humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("PK_thenews_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### QATAR ######

# aljazeera.com
search_terms <- paste0("site:aljazeera.com humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("QA_aje_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:aljazeera.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("QA_aje_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### SWITZERLAND ######

# worldradio.ch
search_terms <- paste0("site:worldradio.ch/news humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CH_worldradio_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:worldradio.ch/news humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CH_worldradio_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thelocal.ch
search_terms <- paste0("site:thelocal.ch humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CH_thelocal_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:thelocal.ch humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CH_thelocal_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# swissinfo.ch
search_terms <- paste0("site:swissinfo.ch/eng humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CH_swissinfo_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:swissinfo.ch/eng humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("CH_swissinfo_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### MALTA ######

# timesofmalta.com
search_terms <- paste0("site:timesofmalta.com humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MT_times_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:timesofmalta.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MT_times_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)


###### ANGOLA ######

# angop.ao
search_terms <- paste0("site:angop.ao humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("AO_ANGOP_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:angop.ao humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("AO_ANGOP_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### ALGERIA ######

# echoroukonline.com
search_terms <- paste0("site:echoroukonline.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("DZ_echoroukonline_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:echoroukonline.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("DZ_echoroukonline_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

#elmoudjahid.com
search_terms <- paste0("site:elmoudjahid.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("DZ_elmoudjahid_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:elmoudjahid.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("DZ_elmoudjahid_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

#aps.dz
search_terms <- paste0("site:aps.dz/en humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("DZ_aps_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:aps.dz/en humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("DZ_aps_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### BAHAMAS #####

# ewnews.com
search_terms <- paste0("site:ewnews.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("BS_ewnews_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:ewnews.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("BS_ewnews_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thenassauguardian.com
search_terms <- paste0("site:thenassauguardian.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("BS_thenassauguardian_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:thenassauguardian.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("BS_thenassauguardian_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# bahamaspress.com
search_terms <- paste0("site:bahamaspress.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("BS_bahamaspress_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# bahamasnews.net
search_terms <- paste0("site:bahamasnews.net humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("BS_bahamasnews_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)
  
###### BOTSWANA #####

# mmegi.bw
search_terms <- paste0("site:mmegi.bw humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("BW_mmegi_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:mmegi.bw humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("BW_mmegi_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thepatriot.co.bw
search_terms <- paste0("site:thepatriot.co.bw humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("BW_thepatriot_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# sundaystandard.info
search_terms <- paste0("site:sundaystandard.info humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("BW_sundaystandard_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# weekendpost.co.bw 
search_terms <- paste0("site:weekendpost.co.bw humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("BW_weekendpost_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### BRAZIL #####

# agenciabrasil.ebc.com.br 
search_terms <- paste0("site:agenciabrasil.ebc.com.br humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("BR_agenciabrasil_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# riotimesonline.com
search_terms <- paste0("site:riotimesonline.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("BR_riotimes_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### EGYPT #####

# nileinternational.net/en
search_terms <- paste0("site:nileinternational.net/en humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("EG_niletv_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:nileinternational.net/en humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("EG_niletv_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# egyptindependent.com
search_terms <- paste0("site:egyptindependent.com humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("EG_independent_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:egyptindependent.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("EG_independent_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# egyptnews.net
search_terms <- paste0("site:egyptnews.net humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("EG_egyptnews_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:egyptnews.net humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("EG_egyptnews_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# madamasr.com/en
search_terms <- paste0("site:madamasr.com/en humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("EG_madamasr_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:madamasr.com/en humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("EG_madamasr_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# masress.com/en
search_terms <- paste0("site:masress.com/en humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("EG_masress_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:masress.com/en humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("EG_masress_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# english.ahram.org.eg
search_terms <- paste0("site:english.ahram.org.eg humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("EG_ahram_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:english.ahram.org.eg humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("EG_ahram_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# egypttoday.com
search_terms <- paste0("site:egypttoday.com humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("EG_egypttoday_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:egypttoday.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("EG_egypttoday_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### ETHIOPIA ######

# ena.et/en
search_terms <- paste0("site:ena.et/en humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ET_ena_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:ena.et/en humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ET_ena_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# FANABC
search_terms <- paste0("site:fanabc.com/english humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ET_fanabc_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# addisstandard.com
search_terms <- paste0("site:addisstandard.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ET_addisstandard_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:addisstandard.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ET_addisstandard_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)
###### THE GAMBIA ######

# standard.gm
search_terms <- paste0("site:standard.gm humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GM_standard_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:standard.gm humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GM_standard_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# fatunetwork.net
search_terms <- paste0("site:fatunetwork.net humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GM_fatunetwork_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# freedomnewspaper.com
search_terms <- paste0("site:freedomnewspaper.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GM_freedomnewspaper_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# gainako.com
search_terms <- paste0("site:gainako.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GM_gainako_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# kaironews.com
search_terms <- paste0("site:kaironews.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GM_kaironews_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thepoint.gm
search_terms <- paste0("site:thepoint.gm humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GM_thepoint_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:thepoint.gm humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GM_thepoint_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# foroyaa.net
search_terms <- paste0("site:foroyaa.net humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GM_foroyaa_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:foroyaa.net humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GM_foroyaa_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### GHANA ######

# thedailystatesman.com
search_terms <- paste0("site:thedailystatesman.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GH_thedailystatesman_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# ghonetv.com
search_terms <- paste0("site:ghonetv.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GH_ghonetv_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# 3news.com
search_terms <- paste0("site:3news.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GH_3news_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# businessworldghana.com
search_terms <- paste0("site:businessworldghana.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GH_businessworldghana_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# gbcghana.com
search_terms <- paste0("site:gbcghana.com humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GH_gbc_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:gbcghana.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GH_gbc_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# graphic.com.gh
search_terms <- paste0("site:graphic.com.gh humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GH_graphic_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:graphic.com.gh humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GH_graphic_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# ghanastar.com
search_terms <- paste0("site:ghanastar.com humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GH_ghanastar_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:ghanastar.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GH_ghanastar_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# pulse.com.gh
search_terms <- paste0("site:pulse.com.gh humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GH_pulse_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:pulse.com.gh humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GH_pulse_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### JAMAICA ######

# radiojamaicanewsonline.com
search_terms <- paste0("site:radiojamaicanewsonline.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("JM_radiojamaicanewsonline_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# jamaicaobserver.com
search_terms <- paste0("site:jamaicaobserver.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("JM_jamaicaobserver_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# jamaica-star.com
search_terms <- paste0("site:jamaica-star.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("JM_jamaica-star_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# jamaicanews.net
search_terms <- paste0("site:jamaicanews.net humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("JM_jamaicanews_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:jamaicanews.net humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("JM_jamaicanews_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# jamaica-gleaner.com
search_terms <- paste0("site:jamaica-gleaner.com humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("JM_gleaner_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:jamaica-gleaner.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("JM_gleaner_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### KENYA ######

# citizentv.co.ke
search_terms <- paste0("site:citizentv.co.ke humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("KE_citizen_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:citizentv.co.ke humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("KE_citizen_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# standardmedia.co.ke
search_terms <- paste0("site:standardmedia.co.ke humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("KE_standardmedia_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:standardmedia.co.ke humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("KE_standardmedia_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# capitalfm.co.ke
search_terms <- paste0("site:capitalfm.co.ke humanitarian* after:", after180," before:", before180)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("KE_capitalfm_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:capitalfm.co.ke humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("KE_capitalfm_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# tuko.co.ke
search_terms <- paste0("site:tuko.co.ke humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("KE_tuko_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# kenya-today.com
search_terms <- paste0("site:kenya-today.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("KE_kenyatoday_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# mediamaxnetwork.co.ke
search_terms <- paste0("site:mediamaxnetwork.co.ke humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("KE_mediamaxnetwork_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)


###### LIBERIA ######

# liberianobserver.com
search_terms <- paste0("site:liberianobserver.com humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("LR_liberianobserver_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:liberianobserver.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("LR_liberianobserver_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# kmtvliberia.com
search_terms <- paste0("site:kmtvliberia.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("LR_kmtvliberia_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:kmtvliberia.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("LR_kmtvliberia_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# themonroviatimes.com
search_terms <- paste0("site:themonroviatimes.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("LR_themonroviatimes_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### LIBYA ######

# en.libyan-cna.net
search_terms <- paste0("site:en.libyan-cna.net humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("LY_libyancna_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:en.libyan-cna.net humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("LY_libyancna_bydate_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# lana-news.ly
search_terms <- paste0("site:lana-news.ly humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("LY_lana_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:lana-news.ly humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("LY_lana_bydate_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# libyanexpress.com
search_terms <- paste0("site:libyanexpress.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("LY_libyanexpress_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:libyanexpress.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("LY_libyanexpress_bydate_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# libyaobserver.ly
search_terms <- paste0("site:libyaobserver.ly humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("LY_libyaobserver_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:libyaobserver.ly humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("LY_libyaobserver_bydate_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# libyaherald.com
search_terms <- paste0("site:libyaherald.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("LY_libyaherald_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:libyaherald.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("LY_libyaherald_bydate_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)


###### MALAWI ######

# mbc.mw
search_terms <- paste0("site:mbc.mw humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MW_mbc_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# malawi24.com
search_terms <- paste0("site:malawi24.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MW_malawi24_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# times.mw
search_terms <- paste0("site:times.mw humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MW_times_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# zodiakmalawi.com
search_terms <- paste0("site:zodiakmalawi.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MW_zodiak_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:zodiakmalawi.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MW_zodiak_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# maravipost.com
search_terms <- paste0("site:maravipost.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MW_maravipost_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:maravipost.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MW_maravipost_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# nyasatimes.com
search_terms <- paste0("site:nyasatimes.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MW_nyasatimes_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:nyasatimes.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MW_nyasatimes_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# mwnation.com
search_terms <- paste0("site:nyasatimes.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MW_mwnation_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:nyasatimes.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MW_mwnation_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# manaonline.gov.mw
search_terms <- paste0("site:manaonline.gov.mw humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MW_manaonline_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:manaonline.gov.mw humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("MW_manaonline_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### NAMIBIA ######

# economist.com.na
search_terms <- paste0("site:economist.com.na humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NA_economist_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:economist.com.na humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NA_economist_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# namibiansun.com
search_terms <- paste0("site:namibiansun.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NA_sun_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:namibiansun.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NA_sun_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# neweralive.na
search_terms <- paste0("site:neweralive.na humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NA_newera_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:neweralive.na humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NA_newera_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# namibian.com.na
search_terms <- paste0("site:namibian.com.na humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NA_namibian_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:namibian.com.na humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NA_namibian_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# nampa.org
search_terms <- paste0("site:nampa.org humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NA_nampa_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:nampa.org humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NA_nampa_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# nbc.na
search_terms <- paste0("site:nbc.na humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NA_nbc_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thevillager.com.na
search_terms <- paste0("site:thevillager.com.na humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NA_villager_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# southerntimesafrica.com
search_terms <- paste0("site:southerntimesafrica.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NA_southerntimes_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thepatriot.com.na
search_terms <- paste0("site:thepatriot.com.na humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NA_patriot_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### RWANDA ######

# bwiza.com
search_terms <- paste0("site:bwiza.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("RW_bwiza_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# ktpress.rw
search_terms <- paste0("site:ktpress.rw humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("RW_ktpress_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# nonaha.com
search_terms <- paste0("site:nonaha.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("RW_nonaha_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# therwandan.com
search_terms <- paste0("site:therwandan.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("RW_therwandan_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# rnanews.com
search_terms <- paste0("site:rnanews.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("RW_rnanews_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# newtimes.co.rw
search_terms <- paste0("site:newtimes.co.rw humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("RW_newtimes_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:newtimes.co.rw humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("RW_newtimes_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### SOMALIA ######

# allsbc.com
search_terms <- paste0("site:allsbc.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SO_allsbc.com_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# sntv.so
search_terms <- paste0("site:sntv.so humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SO_sntv_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# hiiraan.com
search_terms <- paste0("site:hiiraan.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SO_hiiraan_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:hiiraan.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SO_hiiraan_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# goobjoog.com
search_terms <- paste0("site:goobjoog.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SO_goobjoog_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:goobjoog.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SO_goobjoog_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# ilwareed.info
search_terms <- paste0("site:ilwareed.info humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SO_ilwareed_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# mogtimes.com
search_terms <- paste0("site:mogtimes.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SO_mogtimes_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# sonna.so
search_terms <- paste0("site:sonna.so humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SO_sonna_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# hargeisapress.com
search_terms <- paste0("site:hargeisapress.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SO_hargeisapress_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# mareeg.com
search_terms <- paste0("site:mareeg.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SO_mareeg_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### SOUTH SUDAN ######

# nyamile.com
search_terms <- paste0("site:nyamile.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SS_nyamile_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:nyamile.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SS_nyamile_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# ssnewsnow.com
search_terms <- paste0("site:ssnewsnow.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SS_ssnewsnow_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# southsudannation.com
search_terms <- paste0("site:southsudannation.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SS_ssnation_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:southsudannation.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SS_ssnation_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# southsudannewsagency.org
search_terms <- paste0("site:southsudannewsagency.org humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SS_southsudannewsagency_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# jubamonitor.com
search_terms <- paste0("site:jubamonitor.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SS_jubamonitor_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:jubamonitor.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SS_jubamonitor_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# radiotamazuj.org
search_terms <- paste0("site:radiotamazuj.org humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SS_radiotamazuj_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:radiotamazuj.org humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SS_radiotamazuj_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# eyeradio.org
search_terms <- paste0("site:eyeradio.org humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SS_eyeradio_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:eyeradio.org humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SS_eyeradio_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# catholicradionetwork.org
search_terms <- paste0("site:catholicradionetwork.org humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SS_catholicradionetwork_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:catholicradionetwork.org humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("SS_catholicradionetwork_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### NIGERIA ######
# ait.live
search_terms <- paste0("site:ait.live humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NG_AIT_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# nannews.ng
search_terms <- paste0("site:nannews.ng humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NG_NAN_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# channelstv.com
search_terms <- paste0("site:channelstv.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NG_channelstv_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:channelstv.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NG_channelstv_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# nta.ng
search_terms <- paste0("site:nta.ng humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NG_NTA_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:nta.ng humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NG_NTA_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# radionigeria.gov.ng
search_terms <- paste0("site:radionigeria.gov.ng humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NG_radionigeria_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:radionigeria.gov.ng humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NG_radionigeria_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# von.gov.ng
search_terms <- paste0("site:von.gov.ng humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NG_von_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:von.gov.ng humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NG_von_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# tvcnews.tv
search_terms <- paste0("site:tvcnews.tv humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NG_TVC_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:tvcnews.tv humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NG_TVC_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thenewsnigeria.com.ng
search_terms <- paste0("site:thenewsnigeria.com.ng humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NG_thenewsnigeria_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:thenewsnigeria.com.ng humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NG_thenewsnigeria_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# punchng.com
search_terms <- paste0("site:punchng.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NG_punch_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:punchng.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("NG_punch_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### UGANDA ######

# nbs.ug
search_terms <- paste0("site:nbs.ug humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UG_nbs_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# ntv.co.ug
search_terms <- paste0("site:ntv.co.ug humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UG_ntv_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# ugandaradionetwork.net
search_terms <- paste0("site:ugandaradionetwork.net humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UG_ugandaradionetwork_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:ugandaradionetwork.net humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UG_ugandaradionetwork_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# monitor.co.ug
search_terms <- paste0("site:monitor.co.ug humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UG_monitor_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:monitor.co.ug humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UG_monitor_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# pmldaily.com
search_terms <- paste0("site:pmldaily.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UG_pmldaily_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:pmldaily.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UG_pmldaily_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# chimpreports.com
search_terms <- paste0("site:chimpreports.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UG_chimpreports_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:chimpreports.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UG_chimpreports_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# capitalradio.co.ug
search_terms <- paste0("site:capitalradio.co.ug humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("UG_capitalradio_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### ZAMBIA ######

# znbc.co.zm
search_terms <- paste0("site:znbc.co.zm humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZM_znbc_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# muvitv.com
search_terms <- paste0("site:muvitv.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZM_muvitv_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# qfmzambia.com
search_terms <- paste0("site:qfmzambia.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZM_qfmzambia_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# daily-mail.co.zm
search_terms <- paste0("site:daily-mail.co.zm humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZM_dailymail_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:daily-mail.co.zm humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZM_dailymail_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# times.co.zm
search_terms <- paste0("site:times.co.zm humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZM_timeszambia_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# zambiawatchdog.com
search_terms <- paste0("site:zambiawatchdog.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZM_zambiawatchdog_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# zambianeye.com
search_terms <- paste0("site:zambianeye.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZM_zambianeye_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# lusakatimes.com
search_terms <- paste0("site:lusakatimes.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZM_lusakatimes_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:lusakatimes.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZM_lusakatimes_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# mwebantu.com
search_terms <- paste0("site:mwebantu.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZM_mwebantu_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:mwebantu.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZM_mwebantu_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### SOUTH AFRICA ######

# capetalk.co.za
search_terms <- paste0("site:capetalk.co.za humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZA_capetalk_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:capetalk.co.za humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZA_capetalk_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# businesslive.co.za
search_terms <- paste0("site:businesslive.co.za humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZA_businesslive_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:businesslive.co.za humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZA_businesslive_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# news24.com
search_terms <- paste0("site:news24.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZA_news24_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:news24.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZA_news24_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# iol.co.za
search_terms <- paste0("site:iol.co.za humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZA_iol_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:iol.co.za humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZA_iol_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# sabcnews.com
search_terms <- paste0("site:sabcnews.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZA_SABC_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:sabcnews.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZA_SABC_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# enca.com
search_terms <- paste0("site:enca.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZA_eNCA_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:enca.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZA_eNCA_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# channelafrica.co.za
search_terms <- paste0("site:channelafrica.co.za humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZA_channelafrica_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:channelafrica.co.za humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZA_channelafrica_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)


###### ZIMBABWE ######

# thepatriot.co.zw
search_terms <- paste0("site:thepatriot.co.zw humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_thepatriot_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# hararepost.co.zw
search_terms <- paste0("site:hararepost.co.zw humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_hararepost_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# nehandaradio.com
search_terms <- paste0("site:nehandaradio.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_nehandaradio_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:nehandaradio.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_nehandaradio_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# radiovop.com
search_terms <- paste0("site:radiovop.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_radiovop_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:radiovop.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_radiovop_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# zimeye.net
search_terms <- paste0("site:zimeye.net humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_zimeye_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:zimeye.net humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_zimeye_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thezimbabwemail.com
search_terms <- paste0("site:thezimbabwemail.com humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_thezimbabwemail_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:thezimbabwemail.com humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_thezimbabwemail_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# herald.co.zw
search_terms <- paste0("site:herald.co.zw humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_heraldcozw_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:herald.co.zw humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_heraldcozw_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# sundaymail.co.zw
search_terms <- paste0("site:sundaymail.co.zw humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_sundaymailcozw_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:sundaymail.co.zw humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_sundaymailcozw_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# thestandard.co.zw
search_terms <- paste0("site:thestandard.co.zw humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_thestandardcozw_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:thestandard.co.zw humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_thestandardcozw_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# newsday.co.zw
search_terms <- paste0("site:newsday.co.zw humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_newsday_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:newsday.co.zw humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_newsday_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

# theindependent.co.zw
search_terms <- paste0("site:theindependent.co.zw humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_theindependentzm_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- paste0("site:theindependent.co.zw humanitarian*")
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("ZW_theindependentzm_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### INDIA ######

# justearthnews.com
search_terms <- paste0("site:justearthnews.com humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("IN_justearth_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:justearthnews.com humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("IN_justearth_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### GREECE ######

# en.protothema.gr
search_terms <- paste0("site:en.protothema.gr humanitarian* after:", after90," before:", before90)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GR_protothema_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:en.protothema.gr humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("GR_protothema_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

###### ARGENTINA ######

# batimes.com.ar
search_terms <- paste0("site:batimes.com.ar humanitarian* after:", after360," before:", before360)
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("AR_batimes_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
fwrite(file = eval(name_file), x = data.scrape_df)

search_terms <- "site:batimes.com.ar humanitarian*"
data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
data.scrape_df$position <- NULL
data.scrape_df$related_pages_link <- NULL
name_file <- paste0("AR_batimes_total_","nsize_",eval(nrow(data.scrape_df)),".cvs")
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

list_sources <- readLines("sources_to_scrape_round2.txt")
list_countries <- readLines("countries_sources_to_scrape_round2.txt")

for(i in 1:length(list_sources)){
  search_terms <- paste0("site:", eval(list_sources[i])," humanitarian* after:", after10," before:", before10)
  data.scrape <- lapply(search_terms, get_all_links) ## Runs the get_all_links function over all search terms
  data.scrape_df <- do.call(rbind.fill, data.scrape) ## Binds each result in the apnews list in to a single df
  data.scrape_df$position <- NULL
  data.scrape_df$related_pages_link <- NULL
  name_file <- paste0(eval(list_countries[i]),"_", eval(list_sources[i]),"_bydate_","nsize_",eval(nrow(data.scrape_df)),".cvs")
  if(is.null(data.scrape_df) == FALSE)
    {fwrite(file = eval(name_file), x = data.scrape_df)
    }
}
