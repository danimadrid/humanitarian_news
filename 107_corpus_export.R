library(pacman)
p_load(tidyverse, quanteda, data.table, quanteda.dictionaries, tsibble, readr, fst,
       stringr,textreadr,dplyr,magrittr,tidyverse,lubridate,furrr,data.table,readxl, striprtf)

# Data from the whole Humanitarian news corpus
news_corpus <- read_fst("../Humanitarianism&COVID19/humanitarian/data/raw/_FinalData20210913.fst", 
                        as.data.table = T)

# countries with digital and non digital journalism sources 
list.cntries.both <- c("AE", "AM", "AU", "CA", "CN", "DE", "FR", "GB", "GH", "IE", "IN", 
                       "JO", "KE", "KR", "MY", "NG", "NZ", "PH", "PK", "PS", "QA", "RU", 
                       "TR", "US", "ZA")

news_corpus %<>%
  filter(source.country %in% list.cntries.both)

srces <- tibble(source = news_corpus$source,
                country = news_corpus$source.country) %>%
  mutate(source = case_when(source == "BusinessDay" & country == "NG" ~ "BusinessDayNG",
                            source == "BusinessDay" & country == "ZA" ~ "BusinessDayZA",
                            source == "DailyMail" & country == "GB" ~ "DailyMailUK",
                            source == "SundayTimes" & country == "LK" ~ "SundayTimesLK",
                            source == "SundayTimes" & country == "ZA" ~ "SundayTimesZA",
                            source == "TheSouthernTimes" ~ "SouthernTimes",
                            source == "TheSunHerald" ~ "HeraldSun",
                            source == "Tubo" ~ "Tuko",
                            source == "TheStandard" & country == "KE" ~ "TheStandardKE",
                            source == "TheStandard" & country == "HK" ~ "TheStandardHK",
                            source == "TheStar" & country == "KE" ~ "TheStarKE",
                            source == "TheStar" & country == "ZA" ~ "TheStarZA",
                            source == "DailyMirror" & country == "LK" ~ "DailyMirrorLK",
                            source == "DailyMirror" & country == "GB" ~ "DailyMirrorGB",
                            source == "DailyNews" & country == "LK" ~ "DailyNewsLK",
                            source == "TheGuardian" & country == "NG" ~ "TheGuardianNG",
                            source == "TheGuardian" & country == "GB" ~ "TheGuardianUK",
                            .default = as.character(source)))

corpus_meta <- readxl::read_xlsx("../Humanitarianism&COVID19/humanitarian/data/raw/FullCorpusMetadata_revised.xlsx") %>%
  select(source = news_source_name,
         media = media_type_sampled) %>%
  distinct(source, .keep_all = TRUE)

srces <- tibble(source = srces$source,
                country = srces$country) %>%
  left_join(corpus_meta, by = "source") %>%
  mutate(media = case_when(media == "news site" ~ "digital",
                           .default = "not digital"),
         country.source = str_c(country, " ", media))

country.sources.counts <- as.data.frame(table(srces$country.source)) %>%
  rename(seqvar = Var1,
         n = Freq)

news_corpus$media.type <- srces$media
news_corpus$country.source <- srces$country.source

rm(srces,
   list.cntries.both,
   corpus_meta,
   country.sources.counts)

news_corpus$mentions.reuters <- str_detect(news_corpus$full_text, "Reuters")
news_corpus$mentions.ap <- str_detect(news_corpus$full_text, "AP")
news_corpus$mentions.xinhua <- str_detect(news_corpus$full_text, "Xinhua")
news_corpus$mentions.afp <- str_detect(news_corpus$full_text, "AFP")

round(prop.table(table(news_corpus$media.type, news_corpus$mentions.reuters), 1)*100, 2)
round(prop.table(table(news_corpus$media.type, news_corpus$mentions.ap), 1)*100, 2)
round(prop.table(table(news_corpus$media.type, news_corpus$mentions.afp), 1)*100, 2)
round(prop.table(table(news_corpus$media.type, news_corpus$mentions.xinhua), 1)*100, 2)

# Export as ".txt"
news_corpus$year <- str_extract(news_corpus$publish_date, "[0-9]{4}")

news_corpus$full_text <- str_replace(string = news_corpus$full_text, 
                                     pattern = "Please credit the Thomson Reuters Foundation, the charitable arm of Thomson Reuters, that covers humanitarian news, women's rights, trafficking, property rights and climate change", 
                                     replacement = "")

news_corpus %<>%
  mutate(date = as.character(publish_date),
         date = as.Date(date, format = "%Y%m%d")) %>%
  filter(date >= "2010-01-01")

list_iteration <- news_corpus %>%
  distinct(country.source, year, media.type)

list_iteration_digital <- list_iteration %>%
  filter(media.type == "digital")

list_iteration_notdigital <- list_iteration %>%
  filter(media.type != "digital")

export_txt_digital <- function(year_pub, country_pub){
  tmp <- news_corpus %>% 
    filter(country.source == country_pub & year == year_pub) %>%
    select(full_text)
  write_lines(tmp, 
              paste0("DJ Submission/digital_corpus/digital/",eval(country_pub),"_",eval(year_pub),".txt"))
}
export_txt_notdigital <- function(year_pub, country_pub){
  tmp <- news_corpus %>% 
    filter(country.source == country_pub & year == year_pub) %>%
    select(full_text)
  write_lines(tmp, 
              paste0("DJ Submission/digital_corpus/notdigital/",eval(country_pub),"_",eval(year_pub),".txt"))
}

walk2(.x = list_iteration_digital$year, 
      .y = list_iteration_digital$country.source, 
      .f = export_txt_digital)

walk2(.x = list_iteration_notdigital$year, 
      .y = list_iteration_notdigital$country.source, 
      .f = export_txt_notdigital)