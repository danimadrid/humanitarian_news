library(pacman)
p_load(data.table, reader, tidyverse, magrittr, furrr, future, ggplot2, hrbrthemes,quanteda,
       openxlsx,textreuse, tm, NLP,readxl,zip, RNewsflow, lubridate, httr, jsonlite, plyr, lubridate)


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
setwd("/XXXXXXXXXX")
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

###### Merge all Nexis ######

df_gdelt <- fread("/XXXXXXXXXX/_FinalData_GDELT.csv")
df_google <- fread("/XXXXXXXXXX/_FinalData_GoogleAPI.csv")
df_factiva <- fread("/XXXXXXXXXX/_FinalData_Factiva.csv")
df_nexis <- fread("/XXXXXXXXXX/_FinalData_NEXIS.csv")
df_serpapi <- fread("/XXXXXXXXXX/_FinalData_serpAPI.csv")

df_gdelt$publish_date <- as.Date(df_gdelt$publish_date)
df_gdelt$source.country <- " "
df_google$publish_date <- as.Date(df_google$publish_date)
df_google$source.country <- " "
df_nexis$publish_date <- as.Date(df_nexis$publish_date)
df_serpapi$publish_date <- as.Date(df_serpapi$publish_date)
df_serpapi$source.country <- " "
df_factiva$publish_date <- as.Date(df_factiva$publish_date)
df_factiva$source <- str_replace(df_factiva$source, "The Guardian", "The Guardian NG")
df_factiva %<>%
  mutate(
    source = case_when(
      source.country == "KE" & source == "The Standard" ~ "The Standard KE",
      source.country == "HK" & source == "The Standard" ~ "The Standard HK",
      source.country == "JO" & source == "The Star" ~ "The Star JO",
      TRUE ~ as.character(source)
    )
  )




df <- bind_rows(df_gdelt,
            df_google,
            df_nexis,
            df_serpapi,
            df_factiva)

rm(df_gdelt,
   df_google,
   df_nexis,
   df_serpapi,
   df_factiva)

###### Homogenize names of sources and countries ######
# Export list of source for further processing
#df %>%
#  group_by(source, source.country) %>%
#  count(source) %>%
#  fwrite("list_sources_merged.csv")

# Load list of processed sources and merge
df_sources_cleaning <- read_excel("list_sources_merged_processed.xlsx")
df %<>%
  full_join(df_sources_cleaning, by = c("source")) %>%
  filter(is.keep == TRUE,
         source != "NA") %>%
  mutate(source = new.source,
         source.country = new.country) %>%
  select(-new.source,
         -new.country,
         -is.keep)

rm(df_sources_cleaning)

###### Remove Duplicates ######
df %<>%
  filter(is.na(publish_date) != TRUE) %>%
  tibble()

# Count final list of sources
sources_data <- df %>%
  group_by(source, source.country) %>%
  count(source) %>%
  unique
#write.xlsx(sources_data, "CountItemsPerSourceRaw.xlsx")

setwd("/XXXXXXXXXX")

remove_save_delete <- function(name.source,name.country){
  df_id <- df %>%
    filter(source == eval(name.source) & source.country == eval(name.country)) %>% # subset by country
    distinct(id, .keep_all = TRUE) %>%
    corpus(text_field = "full_text", # create corpus
           docid_field = "id") %>% # assign unique ID as doc name
    dfm() %>%
    textstat_simil(method = "jaccard") %>% # run textstat
    as.data.frame() %>%
    filter(jaccard > 0.8) %>% # discard duplicates with jaccard higher than 0.8
    mutate(document1 = as.character(document1),
           document2 = as.character(document2)) %>%
    group_by(grp = paste(pmax(document1, document2), pmin(document1, document2), sep = "_")) %>%
    slice(1) %>%
    ungroup() %>%
    select(-grp) %>%
    select(id = document2) %>%
    unique()
  
  df_sample <- df %>%
    filter(source == eval(name.source) & source.country == eval(name.country)) %>%
    anti_join(df_id) %>%
    mutate(publish_date = str_remove_all(as.character(publish_date), "-"),
           source = str_remove_all(source, " "))
  
  invisible(lapply(1:nrow(df_sample), 
                   function(i) write.table(df_sample[i,4],
                                           file = paste0(df_sample[i,2],"_", 
                                                         df_sample[i,8],"_",
                                                         str_replace_all(df_sample[i,3], "[[:punct:]]", " "), "_",
                                                         df_sample[i,7], ".txt"),
                                           row.names = FALSE, 
                                           col.names = FALSE,
                                           quote = FALSE)))
  
  zip(zipfile = paste0(unique(df_sample$source.country), "_",str_replace_all(unique(df_sample$source), "[[:punct:]]", " "),".zip"), 
      files = list.files(path = getwd(), pattern = ".txt"))
  
  txt_remove <- dir(path=getwd(), pattern=".txt")
  file.remove(txt_remove)
  
  df_sample
}

sources1 <- sources_data %>%
  ungroup %>%
  filter(n < 1000) %>%
  select(-n)
final_df1 <- map2(.x = sources1$source,
     .y = sources1$source.country, 
     .f = remove_save_delete)

df_final1 <- final_df1 %>%
  compact()%>%
  bind_rows()
fwrite(df_final1, "/XXXXXXXXXX/_FinalData_Full1.csv", quote = TRUE, sep = "|")

rm(sources1,
   final_df1,
   remove_save_delete, 
   df_final1)

remove_save_delete_big <- function(name.source,name.country){
  df_id <- df %>%
    filter(source == eval(name.source) & source.country == eval(name.country)) %>% # subset by country
    distinct(id, .keep_all = TRUE)
  
  df_id$publish_date <- as.Date(df_id$publish_date)
  df_id$ym <- floor_date(df_id$publish_date, "halfyear")
  available_dates <- unique(df_id$ym)
  
  extract_by_date <- function(availabledates)
    {
    df_id %<>%
      filter(ym == availabledates) %>%  
      corpus(text_field = "full_text", # create corpus
             docid_field = "id") %>% # assign unique ID as doc name
      dfm() %>%
      textstat_simil(method = "jaccard") %>% # run textstat
      as.data.frame() %>%
      filter(jaccard > 0.8) %>% # discard duplicates with jaccard higher than 0.8
      mutate(document1 = as.character(document1),
             document2 = as.character(document2)) %>%
      group_by(grp = paste(pmax(document1, document2), pmin(document1, document2), sep = "_")) %>%
      slice(1) %>%
      ungroup() %>%
      select(-grp) %>%
      select(id = document2) %>%
      unique()
  }
  
  df_id <- map_df(available_dates, extract_by_date) %>%
    compact() %>%
    bind_rows()
  
  if(nrow(df_id) == 0){
    df_sample <- df %>%
      filter(source == eval(name.source) & source.country == eval(name.country)) %>%
      mutate(publish_date = str_remove_all(as.character(publish_date), "-"),
             source = str_remove_all(source, " "))
  }
  
  if(nrow(df_id) != 0){
    df_sample <- df %>%
      filter(source == eval(name.source) & source.country == eval(name.country)) %>%
      anti_join(df_id) %>%
      mutate(publish_date = str_remove_all(as.character(publish_date), "-"),
             source = str_remove_all(source, " "))
  }
  
  invisible(lapply(1:nrow(df_sample), 
                   function(i) write.table(df_sample[i,4],
                                           file = paste0(df_sample[i,2],"_", 
                                                         df_sample[i,8],"_",
                                                         str_replace_all(df_sample[i,3], "[[:punct:]]", " "), "_",
                                                         df_sample[i,7], ".txt"),
                                           row.names = FALSE, 
                                           col.names = FALSE,
                                           quote = FALSE)))
  
  zip(zipfile = paste0(unique(df_sample$source.country), "_",str_replace_all(unique(df_sample$source), "[[:punct:]]", " "),".zip"), 
      files = list.files(path = getwd(), pattern = ".txt"))
  
  txt_remove <- dir(path=getwd(), pattern=".txt")
  file.remove(txt_remove)
  
  df_sample
}

sources2 <- sources_data %>%
  ungroup %>%
  filter(n < 5000 & n >= 1000) %>%
  select(-n)

final_df2 <- map2(.x = sources2$source,
                  .y = sources2$source.country, 
                  .f = remove_save_delete_big)

df_final2 <- final_df2 %>%
  compact()%>%
  bind_rows()
fwrite(df_final2, "/XXXXXXXXXX/_FinalData_Full2.csv", quote = TRUE, sep = "|")

rm(df_final2,
   sources2)

sources3 <- sources_data %>%
  ungroup %>%
  filter(n < 10000 & n >= 5000) %>%
  select(-n)

final_df3 <- map2(.x = sources3$source,
                  .y = sources3$source.country, 
                  .f = remove_save_delete_big)

df_final3 <- final_df3 %>%
  compact()%>%
  bind_rows()
fwrite(df_final3, "/XXXXXXXXXX/_FinalData_Full3.csv", quote = TRUE, sep = "|")

rm(df_final3,
   sources3, 
   final_df3)

remove_save_delete_very_big <- function(name.source,name.country){
  df_id <- df %>%
    filter(source == eval(name.source) & source.country == eval(name.country)) %>% # subset by country
    distinct(id, .keep_all = TRUE)
  
  df_id$publish_date <- as.Date(df_id$publish_date)
  df_id$ym <- floor_date(df_id$publish_date, "month")
  available_dates <- unique(df_id$ym)
  
  extract_by_date <- function(availabledates)
  {
    df_id %<>%
      filter(ym == availabledates) %>%  
      corpus(text_field = "full_text", # create corpus
             docid_field = "id") %>% # assign unique ID as doc name
      dfm() %>%
      textstat_simil(method = "jaccard") %>% # run textstat
      as.data.frame() %>%
      filter(jaccard > 0.8) %>% # discard duplicates with jaccard higher than 0.8
      mutate(document1 = as.character(document1),
             document2 = as.character(document2)) %>%
      group_by(grp = paste(pmax(document1, document2), pmin(document1, document2), sep = "_")) %>%
      slice(1) %>%
      ungroup() %>%
      select(-grp) %>%
      select(id = document2) %>%
      unique()
  }
  
  df_id <- map_df(available_dates, extract_by_date) %>%
    compact() %>%
    bind_rows()
  
  if(nrow(df_id) == 0){
    df_sample <- df %>%
      filter(source == eval(name.source) & source.country == eval(name.country)) %>%
      mutate(publish_date = str_remove_all(as.character(publish_date), "-"),
             source = str_remove_all(source, " "))
  }
  
  if(nrow(df_id) != 0){
    df_sample <- df %>%
      filter(source == eval(name.source) & source.country == eval(name.country)) %>%
      anti_join(df_id) %>%
      mutate(publish_date = str_remove_all(as.character(publish_date), "-"),
             source = str_remove_all(source, " "))
  }
  
  invisible(lapply(1:nrow(df_sample), 
                   function(i) write.table(df_sample[i,4],
                                           file = paste0(df_sample[i,2],"_", 
                                                         df_sample[i,8],"_",
                                                         str_replace_all(df_sample[i,3], "[[:punct:]]", " "), "_",
                                                         df_sample[i,7], ".txt"),
                                           row.names = FALSE, 
                                           col.names = FALSE,
                                           quote = FALSE)))
  
  zip(zipfile = paste0(unique(df_sample$source.country), "_",str_replace_all(unique(df_sample$source), "[[:punct:]]", " "),".zip"), 
      files = list.files(path = getwd(), pattern = ".txt"))
  
  txt_remove <- dir(path=getwd(), pattern=".txt")
  file.remove(txt_remove)
  
  df_sample
}

sources4 <- sources_data %>%
  ungroup %>%
  filter(n < 90000 & n >= 10000)

final_df4 <- map2(.x = sources4$source,
                  .y = sources4$source.country, 
                  .f = remove_save_delete_very_big)

df_final4 <- final_df4 %>%
  compact()%>%
  bind_rows()
fwrite(df_final4, "/XXXXXXXXXX/_FinalData_Full4.csv", quote = TRUE, sep = "|")

rm(df_final4,
   sources4, 
   final_df4, remove_save_delete_very_big)


df_reuters <- df %>%
  filter(source == "Reuters" | source == "Reuters Foundation") %>% 
  distinct(id, .keep_all = TRUE)

rm(df,sources_data)

df_reuters$publish_date <- as.Date(df_reuters$publish_date)
df_reuters$ym <- floor_date(df_reuters$publish_date, "month")
available_dates <- unique(df_reuters$ym)
available_dates <- available_dates[9:136]

remove_save_delete_reuters <- function(availabledates){
    df <- df_reuters %>%
      filter(ym == availabledates) %>%  
      corpus(text_field = "full_text", # create corpus
             docid_field = "id") %>% # assign unique ID as doc name
      dfm() %>%
      textstat_simil(method = "jaccard") %>% # run textstat
      as.data.frame() %>%
      filter(jaccard > 0.8) %>% # discard duplicates with jaccard higher than 0.8
      mutate(document1 = as.character(document1),
             document2 = as.character(document2)) %>%
      group_by(grp = paste(pmax(document1, document2), pmin(document1, document2), sep = "_")) %>%
      slice(1) %>%
      ungroup() %>%
      select(-grp) %>%
      select(id = document2) %>%
      unique()

  if(nrow(df) == 0){
    df_sample <- df_reuters %>%
      filter(ym == availabledates) %>%
      mutate(publish_date = str_remove_all(as.character(publish_date), "-"),
             source = str_remove_all(source, " "))
  }
  
  if(nrow(df) != 0){
    df_sample <- df_reuters %>%
      filter(ym == availabledates) %>%
      anti_join(df) %>%
      mutate(publish_date = str_remove_all(as.character(publish_date), "-"),
             source = str_remove_all(source, " "))
  }
  
  invisible(lapply(1:nrow(df_sample), 
                   function(i) write.table(df_sample[i,4],
                                           file = paste0(df_sample[i,2],"_", 
                                                         df_sample[i,8],"_",
                                                         str_replace_all(df_sample[i,3], "[[:punct:]]", " "), "_",
                                                         df_sample[i,7], ".txt"),
                                           row.names = FALSE, 
                                           col.names = FALSE,
                                           quote = FALSE)))
  
  zip(zipfile = paste0(unique(df_sample$source.country), "_", eval(availabledates), "_",
                       "Reuters_ReutersFoundation",".zip"), 
      files = list.files(path = getwd(), pattern = ".txt"))
  
  txt_remove <- dir(path=getwd(), pattern=".txt")
  file.remove(txt_remove)
  
  df_sample
}

final_df5 <- map(available_dates, remove_save_delete_reuters)

df_final5 <- final_df5 %>%
  compact()%>%
  bind_rows()
fwrite(df_final5, "/XXXXXXXXXX/_FinalData_Full5.csv", quote = TRUE, sep = "|")


df1 <- fread("/XXXXXXXXXX/_FinalData_Full1.csv")
df2 <- fread("/XXXXXXXXXX/_FinalData_Full2.csv")
df3 <- fread("/XXXXXXXXXX/_FinalData_Full3.csv")
df4 <- fread("/XXXXXXXXXX/_FinalData_Full4.csv")
df5 <- fread("/XXXXXXXXXX/_FinalData_Full5.csv")
df6 <- fread("/XXXXXXXXXX/_FinalData_Full_Awsat.csv")
df5$ym <- NULL
df6$ym <- NULL

df <- rbind(df1,
            df2,
            df3,
            df4,
            df5,
            df6)

saveRDS(object = df, file = "/XXXXXXXXXX/_FinalData20210913.rds")

###### Basic Plots ######
df %>%
  mutate(publish_date = ymd(df$publish_date)) %>%
  group_by(month = floor_date(publish_date, unit = "quarter")) %>%
  count(month) %>%
  filter(month >=as.Date("2010-01-01") & month <= as.Date("2020-08-15")) %>%
  ggplot(aes(x=month, y=n)) + 
  geom_bar(stat = "identity")

df %>%
  mutate(publish_date = ymd(df$publish_date)) %>%
  group_by(month = floor_date(publish_date, unit = "quarter"),source.country) %>%
  count(month) %>%
  filter(month >=as.Date("2010-01-01") & month <= as.Date("2020-08-15")) %>%
  ggplot(aes(x=month, y=n)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ source.country, scales = "free")

df$covid <- str_count(df$full_text, "COVID|Covid|covid|coronavirus")

df %<>%
  mutate(publish_date = ymd(df$publish_date)) %>%
  filter(publish_date >=as.Date("2010-01-01") & publish_date <= as.Date("2020-08-15"))

df %>%
  filter(covid > 0) %>%
  group_by(source.country) %>%
  count(source.country) %>%
  ggplot(aes(x=source.country, y=n)) + 
  geom_bar(stat = "identity")

df %>%
  filter(covid > 0 & source != "Reuters") %>%
  group_by(source.country) %>%
  count(source.country) %>%
  ggplot(aes(x=source.country, y=n)) + 
  geom_bar(stat = "identity")

df %>%
  filter(covid > 0) %>%
  group_by(source.country) %>%
  count(source.country) %>%
  write.xlsx("Counts_mention_COVID_by_country.xlsx")

df %>%
  filter(covid > 0) %>%
  group_by(source.country) %>%
  count(source.country) %>%
  write.xlsx("Counts_total_by_country.xlsx")

df %>%
  select(-title,
         -full_text,
         -web_url) %>%
  fwrite("Metadata.csv")
  