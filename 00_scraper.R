library(pacman)
p_load(tidyverse, rvest, httr, tictoc, furrr, magrittr, reticulate, data.table, dplyr, R.utils, lubridate)

Sys.setenv(RETICULATE_PYTHON = "/Users/danimadridmorales/Dropbox/Academe/Conferences&Papers/AfricaNews/python/bin/python")

reticulate::py_config()
plan(multisession(workers = 7))

### First round of scraping ###

###### Step 1 - Get list of links from file to download ######
list_files_scrape1 <- list.files("/Volumes/LaCieOrange/HumanitarianNewsData/_ScrapeURLs", full.names = TRUE)

get_cvs_scrape <- function(x){
  df <- read_csv(x) %>%
    tibble() %>%
    mutate(file = x,
           file = str_replace(file, "/Volumes/LaCieOrange/HumanitarianNewsData/_ScrapeURLs/",""),
           country = str_extract(file, "[A-Z]{2}"),
           source = str_extract(file, "(?<=_)(.*?)(?=_by)|(?<=_)(.*?)(?=_total)"))
}

df <- map_df(list_files_scrape1, get_cvs_scrape)

df %<>%
  distinct(df$link, .keep_all = TRUE)


###### Step 2 - Split list of links into 10 parts ######
split_links <- sample(1:10, 
                      size = length(df$displayed_link), 
                      replace = TRUE, 
                      prob = c(0.1,0.1,0.1,0.1,0.1,
                               0.1,0.1,0.1,0.1,0.1))

list_links1 <- df$link[split_links == 1]
list_links2 <- df$link[split_links == 2]
list_links3 <- df$link[split_links == 3]
list_links4 <- df$link[split_links == 4]
list_links5 <- df$link[split_links == 5]
list_links6 <- df$link[split_links == 6]
list_links7 <- df$link[split_links == 7]
list_links8 <- df$link[split_links == 8]
list_links9 <- df$link[split_links == 9]
list_links0 <- df$link[split_links == 10]

###### Step 3 - Folds 1 & 2 of 10 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links1), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links1[splitting_urls == 1]
list_urls2 <- list_links1[splitting_urls == 2]
list_urls3 <- list_links1[splitting_urls == 3]
list_urls4 <- list_links1[splitting_urls == 4]
list_urls5 <- list_links1[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 4 - Folds 3 & 4 of 10 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links3), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links3[splitting_urls == 1]
list_urls2 <- list_links3[splitting_urls == 2]
list_urls3 <- list_links3[splitting_urls == 3]
list_urls4 <- list_links3[splitting_urls == 4]
list_urls5 <- list_links3[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 5 - Folds 5 & 6 of 10 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links5), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links5[splitting_urls == 1]
list_urls2 <- list_links5[splitting_urls == 2]
list_urls3 <- list_links5[splitting_urls == 3]
list_urls4 <- list_links5[splitting_urls == 4]
list_urls5 <- list_links5[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 6 - Folds 7 & 8 of 10 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links7), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links7[splitting_urls == 1]
list_urls2 <- list_links7[splitting_urls == 2]
list_urls3 <- list_links7[splitting_urls == 3]
list_urls4 <- list_links7[splitting_urls == 4]
list_urls5 <- list_links7[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 7 - Folds 9 & 10 of 10 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links9), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links9[splitting_urls == 1]
list_urls2 <- list_links9[splitting_urls == 2]
list_urls3 <- list_links9[splitting_urls == 3]
list_urls4 <- list_links9[splitting_urls == 4]
list_urls5 <- list_links9[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 8a - Retry failed links ######

missing1 <- readLines("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200804215343.csv")
missing2 <- readLines("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200805001515.csv")
missing3 <- readLines("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200805025034.csv")
missing4 <- readLines("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200805052119.csv")
missing5 <- readLines("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200805080329.csv")

missing_df <- tibble(web_link = c(missing1,missing2,missing3,missing4,missing5))
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

###### Step 8b - Folds 1 & 2 for retry ######
splitting_urls <- sample(1:5, 
                         size = length(list_links1), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links1[splitting_urls == 1]
list_urls2 <- list_links1[splitting_urls == 2]
list_urls3 <- list_links1[splitting_urls == 3]
list_urls4 <- list_links1[splitting_urls == 4]
list_urls5 <- list_links1[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 8c - Folds 3 & 4 for retry ######

splitting_urls <- sample(1:5, 
                         size = length(list_links3), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links3[splitting_urls == 1]
list_urls2 <- list_links3[splitting_urls == 2]
list_urls3 <- list_links3[splitting_urls == 3]
list_urls4 <- list_links3[splitting_urls == 4]
list_urls5 <- list_links3[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 8d - Folds 5 & 6 for retry ######

splitting_urls <- sample(1:5, 
                         size = length(list_links5), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links5[splitting_urls == 1]
list_urls2 <- list_links5[splitting_urls == 2]
list_urls3 <- list_links5[splitting_urls == 3]
list_urls4 <- list_links5[splitting_urls == 4]
list_urls5 <- list_links5[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 8e - Folds 7 & 8 for retry ######

splitting_urls <- sample(1:5, 
                         size = length(list_links7), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links7[splitting_urls == 1]
list_urls2 <- list_links7[splitting_urls == 2]
list_urls3 <- list_links7[splitting_urls == 3]
list_urls4 <- list_links7[splitting_urls == 4]
list_urls5 <- list_links7[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 8f - Folds 9 & 10 for retry ######

splitting_urls <- sample(1:5, 
                         size = length(list_links9), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links9[splitting_urls == 1]
list_urls2 <- list_links9[splitting_urls == 2]
list_urls3 <- list_links9[splitting_urls == 3]
list_urls4 <- list_links9[splitting_urls == 4]
list_urls5 <- list_links9[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))
###### Step 9 - Retry failed links 2 ######

missing1 <- readLines("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200805111835.csv")
missing2 <- readLines("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200805121112.csv")
missing3 <- readLines("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200805130732.csv")
missing4 <- readLines("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200805135846.csv")
missing5 <- readLines("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200805145018.csv")

missing_df <- tibble(web_link = c(missing1,missing2,missing3,missing4,missing5))
missing_df$web_link <- str_replace_all(missing_df$web_link, "\"\"\"", "")
missing_df$web_link <- str_replace_all(missing_df$web_link, '\"', "")
missing_df$web_link <- str_remove_all(missing_df$web_link, "web_url")

splitting_urls <- sample(1:5, 
                         size = length(missing_df$web_link), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- missing_df$web_link[splitting_urls == 1]
list_urls2 <- missing_df$web_link[splitting_urls == 2]
list_urls3 <- missing_df$web_link[splitting_urls == 3]
list_urls4 <- missing_df$web_link[splitting_urls == 4]
list_urls5 <- missing_df$web_link[splitting_urls == 5]

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

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5)

scraped_text_missing <- tibble(web_url = missing_df$web_link) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 10 - Retry failed links 3 ######

missing_df <- tibble(fread("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200806002225.csv"))

splitting_urls <- sample(1:2, 
                         size = length(missing_df$web_url), 
                         replace = TRUE, 
                         prob = c(0.5,0.5))

list_urls1 <- missing_df$web_url[splitting_urls == 1]
list_urls2 <- missing_df$web_url[splitting_urls == 2]

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
  }, timeout = 20, onTimeout = "silent")
  
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

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2)

scraped_text_missing <- tibble(web_url = missing_df$web_link) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))


##### Second round of scraping #####

###### Step 1 - Get list of links from file to download ######
list_files_scrape1 <- list.files("/Volumes/LaCieOrange/HumanitarianNewsData/_ScrapeURLs2", 
                                 full.names = TRUE,pattern = ".cvs")

get_cvs_scrape <- function(x){
  df <- read_csv(x) %>%
    tibble() %>%
    mutate(file = x)
}

df <- map_df(list_files_scrape1, get_cvs_scrape)

df %<>%
  distinct(df$link, .keep_all = TRUE)

df$file <- str_replace(df$file, "/Volumes/LaCieOrange/HumanitarianNewsData/_ScrapeURLs2/", "")
df$country <- str_extract(df$file, "[A-Z]{2}")
df$source = str_extract(df$file, "(?<=_)(.*?)(?=_by)|(?<=_)(.*?)(?=_total)")

###### Step 2 - Split list of links into 10 parts ######
split_links <- sample(1:10, 
                      size = length(df$displayed_link), 
                      replace = TRUE, 
                      prob = c(0.1,0.1,0.1,0.1,0.1,
                               0.1,0.1,0.1,0.1,0.1))

list_links1 <- df$link[split_links == 1]
list_links2 <- df$link[split_links == 2]
list_links3 <- df$link[split_links == 3]
list_links4 <- df$link[split_links == 4]
list_links5 <- df$link[split_links == 5]
list_links6 <- df$link[split_links == 6]
list_links7 <- df$link[split_links == 7]
list_links8 <- df$link[split_links == 8]
list_links9 <- df$link[split_links == 9]
list_links0 <- df$link[split_links == 10]

###### Step 3 - Folds 1 & 2 of 10 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links1), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links1[splitting_urls == 1]
list_urls2 <- list_links1[splitting_urls == 2]
list_urls3 <- list_links1[splitting_urls == 3]
list_urls4 <- list_links1[splitting_urls == 4]
list_urls5 <- list_links1[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 4 - Folds 3 & 4 of 10 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links3), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links3[splitting_urls == 1]
list_urls2 <- list_links3[splitting_urls == 2]
list_urls3 <- list_links3[splitting_urls == 3]
list_urls4 <- list_links3[splitting_urls == 4]
list_urls5 <- list_links3[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 5 - Folds 5 & 6 of 10 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links5), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links5[splitting_urls == 1]
list_urls2 <- list_links5[splitting_urls == 2]
list_urls3 <- list_links5[splitting_urls == 3]
list_urls4 <- list_links5[splitting_urls == 4]
list_urls5 <- list_links5[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 6 - Folds 7 & 8 of 10 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links7), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links7[splitting_urls == 1]
list_urls2 <- list_links7[splitting_urls == 2]
list_urls3 <- list_links7[splitting_urls == 3]
list_urls4 <- list_links7[splitting_urls == 4]
list_urls5 <- list_links7[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 7 - Folds 9 & 10 of 10 of links to retrieve  ######

splitting_urls <- sample(1:5, 
                         size = length(list_links9), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links9[splitting_urls == 1]
list_urls2 <- list_links9[splitting_urls == 2]
list_urls3 <- list_links9[splitting_urls == 3]
list_urls4 <- list_links9[splitting_urls == 4]
list_urls5 <- list_links9[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 8a - Retry failed links ######

missing1 <- readLines("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200810164655.csv")
missing2 <- readLines("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200810195245.csv")
missing3 <- readLines("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200810211657.csv")
missing4 <- readLines("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200810225835.csv")
missing5 <- readLines("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200811000011.csv")

missing_df <- tibble(web_link = c(missing1,missing2,missing3,missing4,missing5))
rm(missing1,
   missing2,
   missing3,
   missing4,
   missing5)
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

###### Step 8b - Folds 1 & 2 for retry ######
splitting_urls <- sample(1:5, 
                         size = length(list_links1), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links1[splitting_urls == 1]
list_urls2 <- list_links1[splitting_urls == 2]
list_urls3 <- list_links1[splitting_urls == 3]
list_urls4 <- list_links1[splitting_urls == 4]
list_urls5 <- list_links1[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 8c - Folds 3 & 4 for retry ######

splitting_urls <- sample(1:5, 
                         size = length(list_links3), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links3[splitting_urls == 1]
list_urls2 <- list_links3[splitting_urls == 2]
list_urls3 <- list_links3[splitting_urls == 3]
list_urls4 <- list_links3[splitting_urls == 4]
list_urls5 <- list_links3[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 8d - Folds 5 & 6 for retry ######

splitting_urls <- sample(1:5, 
                         size = length(list_links5), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links5[splitting_urls == 1]
list_urls2 <- list_links5[splitting_urls == 2]
list_urls3 <- list_links5[splitting_urls == 3]
list_urls4 <- list_links5[splitting_urls == 4]
list_urls5 <- list_links5[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 8e - Folds 7 & 8 for retry ######

splitting_urls <- sample(1:5, 
                         size = length(list_links7), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links7[splitting_urls == 1]
list_urls2 <- list_links7[splitting_urls == 2]
list_urls3 <- list_links7[splitting_urls == 3]
list_urls4 <- list_links7[splitting_urls == 4]
list_urls5 <- list_links7[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 8f - Folds 9 & 10 for retry ######

splitting_urls <- sample(1:5, 
                         size = length(list_links9), 
                         replace = TRUE, 
                         prob = c(0.2,0.2,0.2,0.2,0.2))

list_urls1 <- list_links9[splitting_urls == 1]
list_urls2 <- list_links9[splitting_urls == 2]
list_urls3 <- list_links9[splitting_urls == 3]
list_urls4 <- list_links9[splitting_urls == 4]
list_urls5 <- list_links9[splitting_urls == 5]

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
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))
###### Step 9 - Retry failed links 2 ######

missing1 <- readLines("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200811004949.csv")
missing_df <- tibble(web_link = missing1)
missing_df$web_link <- str_replace_all(missing_df$web_link, "\"\"\"", "")
missing_df$web_link <- str_replace_all(missing_df$web_link, '\"', "")
missing_df$web_link <- str_remove_all(missing_df$web_link, "web_url")

list_urls1 <- missing_df$web_link

get_text_from_py <- function(url_link){
  
  withTimeout({
    requests <- import("requests")
    rand <- import("requests_random_user_agent")
    goose <- import("goose3")

    g = goose$Goose({http_proxy = 'http://19c7b717bd1e4f7c93a8d6e4eb0c1c3c:@proxy.crawlera:8010'})
    
    article <- g$extract(url = url_link)
    web_text2 <- article$cleaned_text
    web_date2 <- as.character(article$publish_datetime_utc)
    
    if (is.null(web_date2) == TRUE){
      web_date2 <- ""
    }
    
    if (is.null(web_text2) == TRUE){
      web_text2 <- ""
    }
    
    tibble(web_url = url_link,
           web_date = "",
           web_date2 = web_date2,
           web_sysdate = Sys.Date(),
           web_text = "",
           web_text2 = web_text2)
  }, timeout = 10, onTimeout = "silent")
  
}
get_text_from_py <- safely(get_text_from_py)

tic()
scraped_text_safe1 <- future_map(list_urls1[1:50], get_text_from_py)
scraped_text_error1 <- map(scraped_text_safe1, "error")
scraped_text_safe1 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe2 <- future_map(list_urls1[51:150], get_text_from_py)
scraped_text_error2 <- map(scraped_text_safe2, "error")
scraped_text_safe2 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe3 <- future_map(list_urls1[151:250], get_text_from_py)
scraped_text_error3 <- map(scraped_text_safe3, "error")
scraped_text_safe3 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe4 <- future_map(list_urls1[251:300], get_text_from_py)
scraped_text_error4 <- map(scraped_text_safe4, "error")
scraped_text_safe4 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

tic()
scraped_text_safe5 <- future_map(list_urls1[301:358], get_text_from_py)
scraped_text_error5 <- map(scraped_text_safe5, "error")
scraped_text_safe5 %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2,
                           scraped_text_safe3,
                           scraped_text_safe4,
                           scraped_text_safe5)

scraped_text_missing <- tibble(web_url = missing_df$web_link) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 10 - Retry failed links 3 ######

missing_df <- tibble(fread("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200811134843.csv"))
missing_df %<>%
  mutate(web_url = str_replace(web_url, "(?<=news24.com/)(.*)(?=/)", "News24"))

splitting_urls <- sample(1:2, 
                         size = length(missing_df$web_url), 
                         replace = TRUE, 
                         prob = c(0.5,0.5))

list_urls1 <- missing_df$web_url[splitting_urls == 1]
list_urls2 <- missing_df$web_url[splitting_urls == 2]

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
      
      source_python("requests_proxy_crawlera.py")
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
  }, timeout = 20, onTimeout = "silent")
  
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

scraped_text_safe <- rbind(scraped_text_safe1,
                           scraped_text_safe2)

scraped_text_missing <- tibble(web_url = missing_df$web_url) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 11 - Retry failed links 4 ######
missing_df <- tibble(fread("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_20200811141850.csv"))
missing_df %<>%
  mutate(web_url = str_replace(web_url, "News24", "news24"))

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
      
      source_python("requests_proxy_crawlera.py")
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
  }, timeout = 20, onTimeout = "silent")
  
}
get_text_from_py <- safely(get_text_from_py)

tic()
scraped_text_safe <- future_map(missing_df$web_url, get_text_from_py, .progress = TRUE)
scraped_text_error <- map(scraped_text_safe, "error")
scraped_text_safe %<>% 
  map("result") %>% 
  compact() %>% 
  discard(~nrow(.) == 0) %>%
  reduce(bind_rows)
toc()

scraped_text_missing <- tibble(web_url = missing_df$web_url) %>%
  anti_join(tibble(web_url = scraped_text_safe$web_url))

timestamp <- str_replace_all(Sys.time(), ":|-| ", "")
fwrite(scraped_text_safe, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/fulltext_",timestamp,".csv",sep = ""))

fwrite(scraped_text_missing, 
       paste0("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped/missingtext_",timestamp,".csv",sep = ""))

###### Step 11 - Merge text, metadata, and select ######

# Load scraped text data
list_files_scrape1 <- list.files("/Volumes/LaCieOrange/HumanitarianNewsData/_Scraped", 
                                 full.names = TRUE,
                                 pattern = "fulltext_")

get_cvs_scrape <- function(x){
  df <- read_csv(x) %>%
    tibble()
}
df <- map_df(list_files_scrape1, get_cvs_scrape)

# Load original list of links and merge metadata with texts
list_files_scrape2a <- list.files("/Volumes/LaCieOrange/HumanitarianNewsData/_ScrapeURLs", full.names = TRUE)
list_files_scrape2b <- list.files("/Volumes/LaCieOrange/HumanitarianNewsData/_ScrapeURLs2/", full.names = TRUE)
list_files_scrape2 <- c(list_files_scrape2a, list_files_scrape2b)

get_cvs_scrape <- function(x){
  df <- read_csv(x) %>%
    tibble() %>%
    mutate(file = x,
           file = str_replace(file, "/Volumes/LaCieOrange/HumanitarianNewsData/_ScrapeURLs/",""),
           country = str_extract(file, "[A-Z]{2}"))
}

df_metadata <- map_df(list_files_scrape2, get_cvs_scrape)
df_metadata %<>%
  distinct(link, .keep_all = TRUE) %>%
  select(title,
         web_url = link,
         web_date_google = date,
         country)
df %<>%
  left_join(df_metadata) %>%
  mutate(source = str_replace(web_url, "(.*?//.*?)/.*", "\\1"),
         source = str_replace(source, "https://www3.|http://www.|https://www.|https://|http://",""))

rm(df_metadata,
   list_files_scrape1,
   list_files_scrape2,
   list_files_scrape2a,
   list_files_scrape2b)

# Keep only one text per document
df %<>%
  mutate(chars_web_text = nchar(web_text),
         chars_web_text2 = nchar(web_text2),
         chars_web_text = replace_na(chars_web_text, 0),
         chars_web_text2 = replace_na(chars_web_text2, 0))

df1 <- df %>%
  filter(chars_web_text >= chars_web_text2) %>%
  select(web_url,
         title,
         country,
         source,
         web_text,
         web_date_google,
         web_date,
         web_date2)
df2 <- df %>%
  filter(chars_web_text < chars_web_text2) %>%
  select(web_url,
         title,
         country,
         source,
         web_text = web_text2,
         web_date_google,
         web_date,
         web_date2)

df <- rbind(df1,df2)
rm(df1,df2)


df %<>%
  mutate(web_date_google = parse_date_time(web_date_google, "%b %d, %Y"),
         web_date_alternative = str_extract(web_url, "[0-9]{4}/[0-9]{2}/[0-9]{2}"),
         web_date_alternative = parse_date_time(web_date_alternative, "%Y/%m/%d"),
         publish_date = coalesce(web_date,web_date2,web_date_google,web_date_alternative))

df %<>%
  mutate(data_source = "serpAPI",
         id = paste0("SERP",seq(1,length(df$web_url)))) %>%
  select(title,
         publish_date,
         source,
         full_text = web_text,
         web_url,
         data_source,
         id)

sources <- readLines("~/Dropbox/Academe/Conferences&Papers/Humanitarianism&COVID19/list_ALL_sources.txt", warn = F) %>%
  map(function(x){str_extract(df$web_url, x)})
sources <- coalesce(!!!sources) %>%
  tibble(link = .)

df$source <- sources$link

# Final Cleaning of data
df %<>%
  filter(source != "nampa.org", # Remove nampa (subscription based)
         source != "nileinternational.net") # Site down

fwrite(df, 
       "/Volumes/LaCieOrange/HumanitarianNewsData/_FinalData_serpAPI.csv",
       quote = TRUE,
       sep = "|")
