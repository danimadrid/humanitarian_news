#install.packages("newsanchor")
library(pacman)
p_load(tidyverse, rvest, httr, tictoc, furrr, magrittr, reticulate, data.table, dplyr, R.utils,newsanchor,
       readxl, lubridate)

###### Step 1 - Retrieve data from Google News API ######
dates <- seq(as.Date("2018/6/1"), as.Date("2020/05/31"), "week")
dates2 <- seq(as.Date("2018/6/7"), as.Date("2020/05/31"), "week")
dates2 <- c(dates2, as.Date("2020/05/31"))
dates <- tibble(start_date = dates,
                end_date = dates2)

get_news_api_all <- function(date_from, date_to){
  df <- get_everything_all(query = "humanitarian*", 
                           language = "en", 
                           api_key = "[USE-OWN-API-KEY]",
                           from = date_from,
                           to = date_to)
  df <- df[["results_df"]]  
}

###### Step 2 - Load Google News API data ######
setwd("/Volumes/LaCieOrange/HumanitarianNewsData/")

df_googleapi <- fread("/Volumes/LaCieOrange/HumanitarianNewsData/GoogleAPI_all_human.csv")
df_googleapi$clean_urls <- str_replace(df_googleapi$url, "(.*?//.*?)/.*", "\\1")
df_googleapi$clean_urls <- str_replace(df_googleapi$clean_urls, "https://www3.|http://www.|https://www.|https://|http://","")

list_all_sources_urls <- readLines("/Users/danimadridmorales/Dropbox/Academe/Conferences&Papers/Humanitarianism&COVID19/list_ALL_sources_URLs.txt",warn = FALSE)
list_all_sources_urls <- str_replace(list_all_sources_urls, "(.*?//.*?)/.*", "\\1")
list_all_sources_urls <- str_replace(list_all_sources_urls, "https://www3.|http://www.|https://www.|https://|http://","")
list_all_sources_urls <- unique(list_all_sources_urls)
list_all_sources_urls <- tibble(clean_urls = list_all_sources_urls)

df_googleapi %<>%
  semi_join(list_all_sources_urls)

###### Step 3 - Scrape missing text ######

df_googleapi_missing <- df_googleapi %>%
  filter(content == "")

split_links_google <- sample(1:5, 
                      size = length(df_googleapi_missing$content), 
                      replace = TRUE, 
                      prob = c(0.2,0.2,0.2,0.2,0.2))

list_links1g <- df_googleapi_missing$url[split_links_google == 1]
list_links2g <- df_googleapi_missing$url[split_links_google == 2]
list_links3g <- df_googleapi_missing$url[split_links_google == 3]
list_links4g <- df_googleapi_missing$url[split_links_google == 4]
list_links5g <- df_googleapi_missing$url[split_links_google == 5]

setwd(dir = "/Users/danimadridmorales/Dropbox/Academe/Conferences&Papers/AfricaNews/")

get_text_from_py <- function(url_link){
  
  withTimeout({
    traf <- import("trafilatura")
    requests <- import("requests")
    rand <- import("requests_random_user_agent")
    goose <- import("goose3")
    
    r <- traf$fetch_url(url_link)
    web_text <- traf$extract(r)
    
    web_date <- traf$metadata$extract_metadata(r) %>%
      str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")
    
    g = goose$Goose({browser_user_agent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_2)'})
    
    article <- g$extract(url = url_link)
    web_text2 <- article$cleaned_text
    web_date2 <- as.character(article$publish_datetime_utc)
    
    if (is.null(web_date2) == TRUE){
      web_date2 <- ""
    }
    
    if (is.null(web_text2) == TRUE){
      web_text2 <- ""
    }
    
    g$close()
    
    if (length(web_text) == 0 & length(web_text2) == 0) {
      
      source_python("python_justext.py")
      web_text <- (py_capture_output(get_web_texts(url_link)))
      
      web_date <- as.character(Sys.Date())
      web_date2 <- as.character(Sys.time())
    }
    
    if (is.null(web_text) == TRUE){
      web_text <- ""
    }
    
    if (length(web_date) == 0){
      web_date <- ""
    }
    
    if (length(web_date2) == 0){
      web_date2 <- ""
    }
    
    tibble(web_url = url_link,
           web_date = web_date,
           web_date2 = web_date2,
           web_sysdate = Sys.Date(),
           web_text = web_text,
           web_text2 = web_text2)
  }, timeout = 10, onTimeout = "silent")
  
}
get_text_from_py <- safely(get_text_from_py)

tic()
scraped_text_safe1g <- future_map(list_links1g, get_text_from_py, .progress = TRUE)
scraped_text_error1g <- map(scraped_text_safe1g, "error")
scraped_text_safe1g %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2g <- future_map(list_links2g, get_text_from_py, .progress = TRUE)
scraped_text_error2g <- map(scraped_text_safe2g, "error")
scraped_text_safe2g %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3g <- future_map(list_links3g, get_text_from_py, .progress = TRUE)
scraped_text_error3g <- map(scraped_text_safe3g, "error")
scraped_text_safe3g %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4g <- future_map(list_links4g, get_text_from_py, .progress = TRUE)
scraped_text_error4g <- map(scraped_text_safe4g, "error")
scraped_text_safe4g %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5g <- future_map(list_links5g, get_text_from_py, .progress = TRUE)
scraped_text_error5g <- map(scraped_text_safe5g, "error")
scraped_text_safe5g %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safeg <- rbind(scraped_text_safe1g,
                           scraped_text_safe2g,
                           scraped_text_safe3g,
                           scraped_text_safe4g,
                           scraped_text_safe5g)

# Keep only one text per document
scraped_text_safeg %<>%
  mutate(chars_web_text = nchar(web_text),
         chars_web_text2 = nchar(web_text2),
         chars_web_text = replace_na(chars_web_text, 0),
         chars_web_text2 = replace_na(chars_web_text2, 0),
         url = web_url)

df1 <- scraped_text_safeg %>%
  filter(chars_web_text >= chars_web_text2) %>%
  select(url,
         web_text,
         web_date,
         web_date2) %>%
  filter(web_text != "")

df2 <- scraped_text_safeg %>%
  filter(chars_web_text < chars_web_text2) %>%
  select(url,
         web_text = web_text2,
         web_date,
         web_date2) %>%
  filter(web_text != "")

df <- rbind(df1,df2)
rm(df1,df2)
###### Step 4 - Prepare final Google News API Dataset ######
df$content <- df$web_text
df$web_text <- NULL

df_google <- df_googleapi %>%
  left_join(df, by="url")

# Keep only one text per document
df_google %<>%
  mutate(chars_web_contentx = nchar(content.x),
         chars_web_contenty = nchar(content.y),
         chars_web_contentx = replace_na(chars_web_contentx, 0),
         chars_web_contenty = replace_na(chars_web_contenty, 0))

df1 <- df_google %>%
  filter(chars_web_contentx >= chars_web_contenty) %>%
  select(title,
         publish_date = published_at,
         source = clean_urls,
         full_text = content.x,
         web_url = url)

df2 <- df_google %>%
  filter(chars_web_contentx < chars_web_contenty) %>%
  select(title,
         publish_date = published_at,
         source = clean_urls,
         full_text = content.y,
         web_url = url)

df <- rbind(df1,df2)
rm(df1,df2)

df %<>%
  distinct(web_url, .keep_all = TRUE) %>%
  tibble() %>%
  mutate(data_source = "google_news",
         id = paste0("GNAPI",seq(1,length(df$web_url))))

df$publish_date <- as.Date(df$publish_date)

sources <- readLines("~/Dropbox/Academe/Conferences&Papers/Humanitarianism&COVID19/list_ALL_sources.txt", warn = F) %>%
  map(function(x){str_extract(df$web_url, x)})
sources <- coalesce(!!!sources) %>%
  tibble(link = .)

df$source <- sources$link

fwrite(df, 
       "/Volumes/LaCieOrange/HumanitarianNewsData/_FinalData_GoogleAPI.csv",
       quote = TRUE,
       sep = "|")
