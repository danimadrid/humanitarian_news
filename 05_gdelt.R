library(pacman)
p_load(data.table, reader, tidyverse, magrittr, furrr, future,
       rvest, httr, tictoc, reticulate, lubridate, R.utils)

Sys.setenv(RETICULATE_PYTHON = "/XXXXXXXXXX/python")
reticulate::py_config()
plan(multisession(workers = 10))

###### Step 1 - Load GDELT data, merge files and keep matching source ######

setwd("/XXXXXXXXXX")
gdelt_files1 <- fread("GDELT_ThemesHUMANITARIAN_2016010120200531/bq-results-20200601-222427-s0l8og73v3wh.csv")
gdelt_files2 <- fread("GDELT_ThemesHUMANITARIAN_2020060120200808/bq-results-20200809-221041-ct3f03vtvpnm.csv")
gdelt_files3 <- fread("GDELT_V2ThemesHUMANITARIAN_2016010120200531/bq-results-20200601-221935-egx6wq9c9lwd.csv")
gdelt_files4 <- fread("GDELT_V2ThemesHUMANITARIAN_2020060120200808/bq-results-20200809-220939-wpp8yad4unvj.csv")

gdelt_files <- rbind(gdelt_files1,
                     gdelt_files2,
                     gdelt_files3,
                     gdelt_files4)

rm(gdelt_files1,gdelt_files2,gdelt_files3,gdelt_files4)

gdelt_files %<>%
  filter(TranslationInfo == "")

setwd("/XXXXXXXXXX")
sources_ls <- readLines("/XXXXXXXXXX", warn = F) %>%
  map(function(x){str_detect(gdelt_files$DocumentIdentifier, x)})
sources <- sources_ls %>%
  bind_cols()
sources_total <- rowSums(sources)

gdelt_files %<>%
  mutate(flag = sources_total)

fwrite(gdelt_files, "/XXXXXXXXXX/GDELT_filtered.csv")

gdelt_files <- fread("/XXXXXXXXXX/GDELT_filtered.csv")
gdelt_files %<>%
  filter(flag > 0) %>%
  select(-flag,
         -TranslationInfo)

rm(sources,
   sources_ls,
   sources_total)

###### Step 2 - Remove duplicates from scraper and SerpAPI ######
df_g <- fread("/XXXXXXXXXX/_FinalData_GoogleAPI.csv") %>%
  select(web_url)
df_s <- fread("/XXXXXXXXXX/_FinalData_serpAPI.csv") %>%
  select(web_url) %>%
  rbind(df_g)
rm(df_g)

df_gdelt <- gdelt_files %>%
  select(web_url = DocumentIdentifier,
         gdelt_date = GKGRECORDID) %>%
  distinct(web_url, .keep_all = TRUE) %>%
  anti_join(df_s)
rm(gdelt_files,
   df_s)

###### Step 3a - Scraping: Split links ######

split_links <- sample(1:20, 
                      size = length(df_gdelt$web_url), 
                      replace = TRUE, 
                      prob = c(0.05,0.05,0.05,0.05,0.05,
                               0.05,0.05,0.05,0.05,0.05,
                               0.05,0.05,0.05,0.05,0.05,
                               0.05,0.05,0.05,0.05,0.05))

list_links1 <- df_gdelt$web_url[split_links == 1]
list_links2 <- df_gdelt$web_url[split_links == 2]
list_links3 <- df_gdelt$web_url[split_links == 3]
list_links4 <- df_gdelt$web_url[split_links == 4]
list_links5 <- df_gdelt$web_url[split_links == 5]
list_links6 <- df_gdelt$web_url[split_links == 6]
list_links7 <- df_gdelt$web_url[split_links == 7]
list_links8 <- df_gdelt$web_url[split_links == 8]
list_links9 <- df_gdelt$web_url[split_links == 9]
list_links0 <- df_gdelt$web_url[split_links == 10]
list_links11 <- df_gdelt$web_url[split_links == 11]
list_links12 <- df_gdelt$web_url[split_links == 12]
list_links13 <- df_gdelt$web_url[split_links == 13]
list_links14 <- df_gdelt$web_url[split_links == 14]
list_links15 <- df_gdelt$web_url[split_links == 15]
list_links16 <- df_gdelt$web_url[split_links == 16]
list_links17 <- df_gdelt$web_url[split_links == 17]
list_links18 <- df_gdelt$web_url[split_links == 18]
list_links19 <- df_gdelt$web_url[split_links == 19]
list_links20 <- df_gdelt$web_url[split_links == 20]

###### Step 3b - Folds 1 & 2 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links1), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links1[splitting_urls == 1]
list_urls2 <- list_links1[splitting_urls == 2]
list_urls3 <- list_links1[splitting_urls == 3]
list_urls4 <- list_links1[splitting_urls == 4]
list_urls5 <- list_links1[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")
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
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links2), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links2[splitting_urls == 1]
list_urls7 <- list_links2[splitting_urls == 2]
list_urls8 <- list_links2[splitting_urls == 3]
list_urls9 <- list_links2[splitting_urls == 4]
list_urls0 <- list_links2[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links1) %>%
  rbind(tibble(web_url = list_links2)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links1,list_links2,scraped_text_missing,scraped_text_safe)

###### Step 3c - Folds 3 & 4 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links3), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links3[splitting_urls == 1]
list_urls2 <- list_links3[splitting_urls == 2]
list_urls3 <- list_links3[splitting_urls == 3]
list_urls4 <- list_links3[splitting_urls == 4]
list_urls5 <- list_links3[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")
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
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links4), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links4[splitting_urls == 1]
list_urls7 <- list_links4[splitting_urls == 2]
list_urls8 <- list_links4[splitting_urls == 3]
list_urls9 <- list_links4[splitting_urls == 4]
list_urls0 <- list_links4[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links3) %>%
  rbind(tibble(web_url = list_links4)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links3,list_links4,scraped_text_missing,scraped_text_safe,timestamp,splitting_urls)

###### Step 3d - Folds 5 & 6 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links5), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links5[splitting_urls == 1]
list_urls2 <- list_links5[splitting_urls == 2]
list_urls3 <- list_links5[splitting_urls == 3]
list_urls4 <- list_links5[splitting_urls == 4]
list_urls5 <- list_links5[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")
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
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links6), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links6[splitting_urls == 1]
list_urls7 <- list_links6[splitting_urls == 2]
list_urls8 <- list_links6[splitting_urls == 3]
list_urls9 <- list_links6[splitting_urls == 4]
list_urls0 <- list_links6[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links5) %>%
  rbind(tibble(web_url = list_links6)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links5,list_links6,scraped_text_missing,scraped_text_safe,timestamp,splitting_urls)
  
###### Step 3e - Folds 7 & 8 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links7), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links7[splitting_urls == 1]
list_urls2 <- list_links7[splitting_urls == 2]
list_urls3 <- list_links7[splitting_urls == 3]
list_urls4 <- list_links7[splitting_urls == 4]
list_urls5 <- list_links7[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")
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
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links8), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links8[splitting_urls == 1]
list_urls7 <- list_links8[splitting_urls == 2]
list_urls8 <- list_links8[splitting_urls == 3]
list_urls9 <- list_links8[splitting_urls == 4]
list_urls0 <- list_links8[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links7) %>%
  rbind(tibble(web_url = list_links8)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links7,list_links8,scraped_text_missing,scraped_text_safe,timestamp,splitting_urls)

###### Step 3f - Folds 9 & 10 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links9), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links9[splitting_urls == 1]
list_urls2 <- list_links9[splitting_urls == 2]
list_urls3 <- list_links9[splitting_urls == 3]
list_urls4 <- list_links9[splitting_urls == 4]
list_urls5 <- list_links9[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")
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
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links0), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links0[splitting_urls == 1]
list_urls7 <- list_links0[splitting_urls == 2]
list_urls8 <- list_links0[splitting_urls == 3]
list_urls9 <- list_links0[splitting_urls == 4]
list_urls0 <- list_links0[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links9) %>%
  rbind(tibble(web_url = list_links0)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links9,list_links0,scraped_text_missing,scraped_text_safe,timestamp,splitting_urls)

###### Step 3g - Transfer list of links  ######

list_links1 <- list_links11
list_links2 <- list_links12
list_links3 <- list_links13
list_links4 <- list_links14
list_links5 <- list_links15
list_links6 <- list_links16
list_links7 <- list_links17
list_links8 <- list_links18
list_links9 <- list_links19
list_links0 <- list_links20

rm(list_links11,list_links12,list_links13,list_links14,list_links15,
   list_links16,list_links17,list_links18,list_links19,list_links20)

###### Step 3h - Folds 11 & 12 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links1), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links1[splitting_urls == 1]
list_urls2 <- list_links1[splitting_urls == 2]
list_urls3 <- list_links1[splitting_urls == 3]
list_urls4 <- list_links1[splitting_urls == 4]
list_urls5 <- list_links1[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")
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
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links2), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links2[splitting_urls == 1]
list_urls7 <- list_links2[splitting_urls == 2]
list_urls8 <- list_links2[splitting_urls == 3]
list_urls9 <- list_links2[splitting_urls == 4]
list_urls0 <- list_links2[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links1) %>%
  rbind(tibble(web_url = list_links2)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links1,list_links2,scraped_text_missing,scraped_text_safe)

###### Step 3i - Folds 13 & 14 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links3), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links3[splitting_urls == 1]
list_urls2 <- list_links3[splitting_urls == 2]
list_urls3 <- list_links3[splitting_urls == 3]
list_urls4 <- list_links3[splitting_urls == 4]
list_urls5 <- list_links3[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")
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
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links4), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links4[splitting_urls == 1]
list_urls7 <- list_links4[splitting_urls == 2]
list_urls8 <- list_links4[splitting_urls == 3]
list_urls9 <- list_links4[splitting_urls == 4]
list_urls0 <- list_links4[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links3) %>%
  rbind(tibble(web_url = list_links4)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links3,list_links4,scraped_text_missing,scraped_text_safe,timestamp,splitting_urls)

###### Step 3j - Folds 15 & 16 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links5), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links5[splitting_urls == 1]
list_urls2 <- list_links5[splitting_urls == 2]
list_urls3 <- list_links5[splitting_urls == 3]
list_urls4 <- list_links5[splitting_urls == 4]
list_urls5 <- list_links5[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")
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
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links6), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links6[splitting_urls == 1]
list_urls7 <- list_links6[splitting_urls == 2]
list_urls8 <- list_links6[splitting_urls == 3]
list_urls9 <- list_links6[splitting_urls == 4]
list_urls0 <- list_links6[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links5) %>%
  rbind(tibble(web_url = list_links6)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links5,list_links6,scraped_text_missing,scraped_text_safe,timestamp,splitting_urls)

###### Step 3k - Folds 17 & 18 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links7), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links7[splitting_urls == 1]
list_urls2 <- list_links7[splitting_urls == 2]
list_urls3 <- list_links7[splitting_urls == 3]
list_urls4 <- list_links7[splitting_urls == 4]
list_urls5 <- list_links7[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")
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
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links8), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links8[splitting_urls == 1]
list_urls7 <- list_links8[splitting_urls == 2]
list_urls8 <- list_links8[splitting_urls == 3]
list_urls9 <- list_links8[splitting_urls == 4]
list_urls0 <- list_links8[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links7) %>%
  rbind(tibble(web_url = list_links8)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links7,list_links8,scraped_text_missing,scraped_text_safe,timestamp,splitting_urls)

###### Step 3l - Folds 19 & 20 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links9), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links9[splitting_urls == 1]
list_urls2 <- list_links9[splitting_urls == 2]
list_urls3 <- list_links9[splitting_urls == 3]
list_urls4 <- list_links9[splitting_urls == 4]
list_urls5 <- list_links9[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")
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
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links0), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links0[splitting_urls == 1]
list_urls7 <- list_links0[splitting_urls == 2]
list_urls8 <- list_links0[splitting_urls == 3]
list_urls9 <- list_links0[splitting_urls == 4]
list_urls0 <- list_links0[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links9) %>%
  rbind(tibble(web_url = list_links0)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links9,list_links0,scraped_text_missing,scraped_text_safe,timestamp,splitting_urls)

###### Step 4 - Retry failed 1 ######

missing1 <- readLines("/XXXXXXXXXX")
missing2 <- readLines("/XXXXXXXXXX")
missing3 <- readLines("/XXXXXXXXXX")
missing4 <- readLines("/XXXXXXXXXX")
missing5 <- readLines("/XXXXXXXXXX")
missing6 <- readLines("/XXXXXXXXXX")
missing7 <- readLines("/XXXXXXXXXX")
missing8 <- readLines("/XXXXXXXXXX")
missing9 <- readLines("/XXXXXXXXXX")
missing10 <- readLines("/XXXXXXXXXX")

missing_df <- tibble(web_link = c(missing1,missing2,missing3,missing4,missing5,
                                  missing6,missing7,missing8,missing9,missing10))
rm(missing1,
   missing2,
   missing3,
   missing4,
   missing5,
   missing6,
   missing7,
   missing8,
   missing9,
   missing10)

missing_df %<>%
  mutate(flag = str_detect(web_link, pattern = "english.wafa.ps|web_url|washingtonpost.com")) %>%
  filter(flag != TRUE) %>%
  select(-flag)

missing_df %<>%
  mutate(web_link = str_replace(web_link, "/XXXXXXXXXX", "www.philstar.com"),
         web_link = str_replace(web_link, "/XXXXXXXXXX", "news24.com/news24/"),
         web_link = str_replace(web_link, '/XXXXXXXXXX"', ""))

missing_df_n24 <- missing_df %>%
  filter(str_detect(web_link, "www.news24.com/")) %>%
  mutate(web_link = tolower(web_link))

missing_df <- missing_df %>%
  filter(!str_detect(web_link, "www.news24.com/")) %>%
  bind_rows(missing_df_n24)

rm(missing_df_n24)
###### Step 4a - Scraping: Split links ######

split_links <- sample(1:10, 
                      size = length(missing_df$web_link), 
                      replace = TRUE, 
                      prob = c(0.1,0.1,0.1,0.1,0.1,
                               0.1,0.1,0.1,0.1,0.1))

list_links1 <- missing_df$web_link[split_links == 1]
list_links2 <- missing_df$web_link[split_links == 2]
list_links3 <- missing_df$web_link[split_links == 3]
list_links4 <- missing_df$web_link[split_links == 4]
list_links5 <- missing_df$web_link[split_links == 5]
list_links6 <- missing_df$web_link[split_links == 6]
list_links7 <- missing_df$web_link[split_links == 7]
list_links8 <- missing_df$web_link[split_links == 8]
list_links9 <- missing_df$web_link[split_links == 9]
list_links0 <- missing_df$web_link[split_links == 10]


###### Step 4b - Folds 1 & 2 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links1), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links1[splitting_urls == 1]
list_urls2 <- list_links1[splitting_urls == 2]
list_urls3 <- list_links1[splitting_urls == 3]
list_urls4 <- list_links1[splitting_urls == 4]
list_urls5 <- list_links1[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")
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
  }, timeout = 25, onTimeout = "silent")
  
}
get_text_from_py <- safely(get_text_from_py)

tic()
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links2), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links2[splitting_urls == 1]
list_urls7 <- list_links2[splitting_urls == 2]
list_urls8 <- list_links2[splitting_urls == 3]
list_urls9 <- list_links2[splitting_urls == 4]
list_urls0 <- list_links2[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links1) %>%
  rbind(tibble(web_url = list_links2)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links1,list_links2,scraped_text_missing,scraped_text_safe)

###### Step 3c - Folds 3 & 4 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links3), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links3[splitting_urls == 1]
list_urls2 <- list_links3[splitting_urls == 2]
list_urls3 <- list_links3[splitting_urls == 3]
list_urls4 <- list_links3[splitting_urls == 4]
list_urls5 <- list_links3[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")
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
  }, timeout = 30, onTimeout = "silent")
  
}
get_text_from_py <- safely(get_text_from_py)

tic()
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links4), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links4[splitting_urls == 1]
list_urls7 <- list_links4[splitting_urls == 2]
list_urls8 <- list_links4[splitting_urls == 3]
list_urls9 <- list_links4[splitting_urls == 4]
list_urls0 <- list_links4[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links3) %>%
  rbind(tibble(web_url = list_links4)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links3,list_links4,scraped_text_missing,scraped_text_safe,timestamp,splitting_urls)

###### Step 3d - Folds 5 & 6 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links5), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links5[splitting_urls == 1]
list_urls2 <- list_links5[splitting_urls == 2]
list_urls3 <- list_links5[splitting_urls == 3]
list_urls4 <- list_links5[splitting_urls == 4]
list_urls5 <- list_links5[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")
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
  }, timeout = 30, onTimeout = "silent")
  
}
get_text_from_py <- safely(get_text_from_py)

tic()
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links6), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links6[splitting_urls == 1]
list_urls7 <- list_links6[splitting_urls == 2]
list_urls8 <- list_links6[splitting_urls == 3]
list_urls9 <- list_links6[splitting_urls == 4]
list_urls0 <- list_links6[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links5) %>%
  rbind(tibble(web_url = list_links6)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links5,list_links6,scraped_text_missing,scraped_text_safe,timestamp,splitting_urls)

###### Step 3e - Folds 7 & 8 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links7), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links7[splitting_urls == 1]
list_urls2 <- list_links7[splitting_urls == 2]
list_urls3 <- list_links7[splitting_urls == 3]
list_urls4 <- list_links7[splitting_urls == 4]
list_urls5 <- list_links7[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")
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
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links8), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links8[splitting_urls == 1]
list_urls7 <- list_links8[splitting_urls == 2]
list_urls8 <- list_links8[splitting_urls == 3]
list_urls9 <- list_links8[splitting_urls == 4]
list_urls0 <- list_links8[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links7) %>%
  rbind(tibble(web_url = list_links8)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links7,list_links8,scraped_text_missing,scraped_text_safe,timestamp,splitting_urls)

###### Step 3f - Folds 9 & 10 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links9), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links9[splitting_urls == 1]
list_urls2 <- list_links9[splitting_urls == 2]
list_urls3 <- list_links9[splitting_urls == 3]
list_urls4 <- list_links9[splitting_urls == 4]
list_urls5 <- list_links9[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")
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
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links0), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links0[splitting_urls == 1]
list_urls7 <- list_links0[splitting_urls == 2]
list_urls8 <- list_links0[splitting_urls == 3]
list_urls9 <- list_links0[splitting_urls == 4]
list_urls0 <- list_links0[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links9) %>%
  rbind(tibble(web_url = list_links0)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links9,list_links0,scraped_text_missing,scraped_text_safe,timestamp,splitting_urls)

###### Step 5 - Retry failed 2 ######

missing1 <- readLines("/XXXXXXXXXX")
missing2 <- readLines("/XXXXXXXXXX")
missing3 <- readLines("/XXXXXXXXXX")
missing4 <- readLines("/XXXXXXXXXX")
missing5 <- readLines("/XXXXXXXXXX")

missing_df <- tibble(web_link = c(missing1,missing2,missing3,missing4,missing5))
rm(missing1,
   missing2,
   missing3,
   missing4,
   missing5)

missing_df %<>%
  mutate(flag = str_detect(web_link, pattern = "web_url")) %>%
  filter(flag != TRUE) %>%
  select(-flag)

missing_df %<>%
  mutate(flag = str_detect(web_link, pattern = '"')) %>%
  filter(flag != TRUE) %>%
  select(-flag)

###### Step 4a - Scraping: Split links ######

split_links <- sample(1:10, 
                      size = length(missing_df$web_link), 
                      replace = TRUE, 
                      prob = c(0.1,0.1,0.1,0.1,0.1,
                               0.1,0.1,0.1,0.1,0.1))

list_links1 <- missing_df$web_link[split_links == 1]
list_links2 <- missing_df$web_link[split_links == 2]
list_links3 <- missing_df$web_link[split_links == 3]
list_links4 <- missing_df$web_link[split_links == 4]
list_links5 <- missing_df$web_link[split_links == 5]
list_links6 <- missing_df$web_link[split_links == 6]
list_links7 <- missing_df$web_link[split_links == 7]
list_links8 <- missing_df$web_link[split_links == 8]
list_links9 <- missing_df$web_link[split_links == 9]
list_links0 <- missing_df$web_link[split_links == 10]

###### Step 4b - Folds 1 & 2 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links1), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links1[splitting_urls == 1]
list_urls2 <- list_links1[splitting_urls == 2]
list_urls3 <- list_links1[splitting_urls == 3]
list_urls4 <- list_links1[splitting_urls == 4]
list_urls5 <- list_links1[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")
get_text_from_py <- function(url_link){
  
  withTimeout({
    traf <- import("trafilatura")
    
    r <- traf$fetch_url(url_link)
    web_text <- traf$extract(r)
    
    web_date <- traf$metadata$extract_metadata(r) %>%
      str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")
    
    web_text2 <- " "
    web_date2 <- " "

    if (length(web_text) == 0 | is.null(web_text) == TRUE) {
      
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
  }, timeout = 30, onTimeout = "silent")
  
}
get_text_from_py <- safely(get_text_from_py)

tic()
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links2), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links2[splitting_urls == 1]
list_urls7 <- list_links2[splitting_urls == 2]
list_urls8 <- list_links2[splitting_urls == 3]
list_urls9 <- list_links2[splitting_urls == 4]
list_urls0 <- list_links2[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links1) %>%
  rbind(tibble(web_url = list_links2)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links1,list_links2,scraped_text_missing,scraped_text_safe)

###### Step 4c - Folds 3 & 4 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links3), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links3[splitting_urls == 1]
list_urls2 <- list_links3[splitting_urls == 2]
list_urls3 <- list_links3[splitting_urls == 3]
list_urls4 <- list_links3[splitting_urls == 4]
list_urls5 <- list_links3[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")

tic()
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links4), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links4[splitting_urls == 1]
list_urls7 <- list_links4[splitting_urls == 2]
list_urls8 <- list_links4[splitting_urls == 3]
list_urls9 <- list_links4[splitting_urls == 4]
list_urls0 <- list_links4[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links3) %>%
  rbind(tibble(web_url = list_links4)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links3,list_links4,scraped_text_missing,scraped_text_safe,timestamp,splitting_urls)

###### Step 4d - Folds 5 & 6 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links5), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links5[splitting_urls == 1]
list_urls2 <- list_links5[splitting_urls == 2]
list_urls3 <- list_links5[splitting_urls == 3]
list_urls4 <- list_links5[splitting_urls == 4]
list_urls5 <- list_links5[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")

tic()
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links6), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links6[splitting_urls == 1]
list_urls7 <- list_links6[splitting_urls == 2]
list_urls8 <- list_links6[splitting_urls == 3]
list_urls9 <- list_links6[splitting_urls == 4]
list_urls0 <- list_links6[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links5) %>%
  rbind(tibble(web_url = list_links6)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links5,list_links6,scraped_text_missing,scraped_text_safe,timestamp,splitting_urls)

###### Step 4e - Folds 7 & 8 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links7), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links7[splitting_urls == 1]
list_urls2 <- list_links7[splitting_urls == 2]
list_urls3 <- list_links7[splitting_urls == 3]
list_urls4 <- list_links7[splitting_urls == 4]
list_urls5 <- list_links7[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")

tic()
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links8), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links8[splitting_urls == 1]
list_urls7 <- list_links8[splitting_urls == 2]
list_urls8 <- list_links8[splitting_urls == 3]
list_urls9 <- list_links8[splitting_urls == 4]
list_urls0 <- list_links8[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links7) %>%
  rbind(tibble(web_url = list_links8)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links7,list_links8,scraped_text_missing,scraped_text_safe,timestamp,splitting_urls)

###### Step 4f - Folds 9 & 10 of 20 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links9), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links9[splitting_urls == 1]
list_urls2 <- list_links9[splitting_urls == 2]
list_urls3 <- list_links9[splitting_urls == 3]
list_urls4 <- list_links9[splitting_urls == 4]
list_urls5 <- list_links9[splitting_urls == 5]

setwd(dir = "/XXXXXXXXXX")

tic()
scraped_text_safe1 <- future_map(list_urls1, get_text_from_py, .progress = TRUE)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls2, get_text_from_py, .progress = TRUE)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls3, get_text_from_py, .progress = TRUE)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls4, get_text_from_py, .progress = TRUE)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls5, get_text_from_py, .progress = TRUE)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

splitting_urls <- sample(1:5, 
                         size = length(list_links0), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls6 <- list_links0[splitting_urls == 1]
list_urls7 <- list_links0[splitting_urls == 2]
list_urls8 <- list_links0[splitting_urls == 3]
list_urls9 <- list_links0[splitting_urls == 4]
list_urls0 <- list_links0[splitting_urls == 5]

tic()
scraped_text_safe6 <- future_map(list_urls6, get_text_from_py, .progress = TRUE)
scraped_text_error6 <- map(scraped_text_safe6, "error")
scraped_text_safe6 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe7 <- future_map(list_urls7, get_text_from_py, .progress = TRUE)
scraped_text_error7 <- map(scraped_text_safe7, "error")
scraped_text_safe7 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe8 <- future_map(list_urls8, get_text_from_py, .progress = TRUE)
scraped_text_error8 <- map(scraped_text_safe8, "error")
scraped_text_safe8 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe9 <- future_map(list_urls9, get_text_from_py, .progress = TRUE)
scraped_text_error9 <- map(scraped_text_safe9, "error")
scraped_text_safe9 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe0 <- future_map(list_urls0, get_text_from_py, .progress = TRUE)
scraped_text_error0 <- map(scraped_text_safe0, "error")
scraped_text_safe0 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5,
                           scraped_text_safe6,
                           scraped_text_safe7,
                           scraped_text_safe8,
                           scraped_text_safe9,
                           scraped_text_safe0)

scraped_text_missing <- tibble(web_url = list_links9) %>%
  rbind(tibble(web_url = list_links0)) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/XXXXXXXXXX",timestamp,".csv",sep = ""))

rm(list_urls0,
   list_urls1,
   list_urls2,
   list_urls3,
   list_urls4,
   list_urls5,
   list_urls6,
   list_urls7,
   list_urls8,
   list_urls9,
   scraped_text_error0,scraped_text_error1,scraped_text_error2,scraped_text_error3,scraped_text_error4,
   scraped_text_error5,scraped_text_error6,scraped_text_error7,scraped_text_error8,scraped_text_error9,
   scraped_text_safe0,scraped_text_safe1,scraped_text_safe2,scraped_text_safe3,scraped_text_safe4,
   scraped_text_safe5,scraped_text_safe6,scraped_text_safe7,scraped_text_safe8,scraped_text_safe9,
   list_links9,list_links0,scraped_text_missing,scraped_text_safe,timestamp,splitting_urls)



###### Step 6 - Merge ######
 
setwd("/XXXXXXXXXX")

full_gdelt <- fread("/XXXXXXXXXX/GDELT_filtered.csv") %>%
  filter(flag == TRUE) %>%
  mutate(web_gdeltdate = str_match(GKGRECORDID, "[0-9]{8}"),
         web_gdeltdate = parse_date_time(web_gdeltdate, orders = "Ymd")) %>%
  select(web_url = DocumentIdentifier,
         web_gdeltdate) %>%
  distinct(web_url,.keep_all = TRUE)
glimpse(full_gdelt)

scraped_gdelt_data <- list.files("/XXXXXXXXXX", pattern = "fulltext") %>%
  map(function(x){
    temp <- fread(x) %>%
      tibble() %>%
      mutate(web_date = as.Date(web_date),
             web_sysdate = as.Date(web_sysdate))}) %>%
  bind_rows()

scraped_gdelt_data %<>%
  mutate(chars_web_text = nchar(web_text),
         chars_web_text2 = nchar(web_text2),
         chars_web_text = replace_na(chars_web_text, 0),
         chars_web_text2 = replace_na(chars_web_text2, 0)) %>%
  left_join(full_gdelt)

df1 <- scraped_gdelt_data %>%
  filter(chars_web_text >= chars_web_text2) %>%
  select(web_url,
         web_text,
         web_date,
         web_date2,
         web_gdeltdate)
df2 <- scraped_gdelt_data %>%
  filter(chars_web_text < chars_web_text2) %>%
  select(web_url,
         web_text = web_text2,
         web_date,
         web_date2,
         web_gdeltdate)

df <- rbind(df1,df2)
rm(df1,df2,scraped_gdelt_data, full_gdelt)

df %<>%
  mutate(title = " ",
         web_date = as.Date(web_date),
         web_date2 = as.Date(web_date2),
         web_gdeltdate = as.Date(web_gdeltdate),
         publish_date = coalesce(web_date,web_date2,web_gdeltdate))

df$source <- str_replace(df$web_url, "(.*?//.*?)/.*", "\\1")
df$source <- str_replace(df$source, "https://www3.|http://www.|https://www.|https://|http://","")

df %<>%
  mutate(data_source = "GDELT",
         id = paste0("/XXXXXXXXXX",seq(1,length(df$web_url)))) %>%
  select(title,
         publish_date,
         source,
         full_text = web_text,
         web_url,
         data_source,
         id)

fwrite(df, 
       "/XXXXXXXXXX/_FinalData_GDELT.csv",
       quote = TRUE,
       sep = "|")

rm(df)