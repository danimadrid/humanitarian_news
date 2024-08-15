library(pacman)
p_load(tidyverse, 
       magrittr, 
       openxlsx, 
       RNewsflow, 
       lubridate, 
       quanteda, 
       ISOcodes, 
       GGally, 
       network,
       sna,
       gridExtra,
       ggnet2)

####### Computing similarities digital vs. non digital #######
dfm_digital <- readRDS("data/RR/dfmat_digital.RDS")
dfm_notdigital <- readRDS("data/RR/dfmat_nodigital.RDS")

dfmat <- rbind(dfm_digital,
               dfm_notdigital)

# generate vector with countries to retain
list.cntries.both <- c("AE","AU","CA","CN","DE",
                       "FR","GB","IE","IN","KR",
                       "MY","NG","NZ","PH","PK",
                       "RU","TR","US","ZA","QA")
dfmat2 <- dfm_subset(dfmat, source.country %in% list.cntries.both)
dfmat <- dfmat2
rm(dfmat2)

# add relevant meta data to the dfm
corpus_meta <- readxl::read_xlsx("data/raw/FullCorpusMetadata_revised.xlsx") %>%
  select(source = news_source_name,
         media = media_type_sampled) %>%
  distinct(source, .keep_all = TRUE)

dfmat_meta <- docvars(dfmat) %>%
  mutate(source = case_when(source == "BusinessDay" & source.country == "ZA" ~ "BusinessDayZA",
                            source == "BusinessDay" & source.country == "NG" ~ "BusinessDayNG",
                            source == "TheGuardian" & source.country == "NG" ~ "TheGuardianNG",
                            source == "DailyMail" & source.country == "GB" ~ "DailyMailUK",
                            source == "SundayTimes" & source.country == "ZA" ~ "SundayTimesZA",
                            source == "TheSunHerald" ~ "HeraldSun",
                            source == "TheStar" & source.country == "ZA" ~ "TheStarZA",
                            source == "DailyMirror" & source.country == "GB" ~ "DailyMirrorUK",
                            source == "TheGuardian" & source.country == "GB" ~ "TheGuardianUK",
                            .default = as.character(source))) %>%
  left_join(corpus_meta, by = "source")

dfmat$media <- dfmat_meta$media

# create dfms with & without news agency content
dfm_agencies <- dfm_subset(dfmat, media %in% c("newswire stories", "newswire copy"))
dfm_rest <- dfm_subset(dfmat, !media %in% c("newswire stories", "newswire copy"))

# add time variables
dfm_agencies$date <- paste0(dfm_agencies$date, " 09:00:00")
dfm_agencies$date <- as.character(dfm_agencies$date)
dfm_agencies$date <- ymd_hms(dfm_agencies$date)
dfm_agencies$date <- as.POSIXct(dfm_agencies$date)

dfm_rest$date <- paste0(dfm_rest$date, " 09:00:00")
dfm_rest$date <- as.character(dfm_rest$date)
dfm_rest$date <- ymd_hms(dfm_rest$date)
dfm_rest$date <- as.POSIXct(dfm_rest$date)

# merge dfms & add ids
dfm_digital <- rbind(dfm_agencies,
                     dfm_rest)
docnames(dfm_digital) <- sprintf("ID_%06d", seq(1,nrow(dfmat_meta)))

# compute document level similarities
g_digital <- newsflow_compare(dfm_digital, 
                         date_var = "date",
                         hour_window = c(0,48), 
                         min_similarity = 0.97)

agg_perdoc = network_aggregate(g_digital, 
                               by_from='name', 
                               by_to='media', 
                               edge_attribute='weight', 
                               agg_FUN=max,
                               return_df=TRUE)


# extract edgelist with limited news agencies
agg_year <- network_aggregate(g_digital, 
                              by_from='source', 
                              by_to='label', 
                              edge_attribute='hourdiff', 
                              agg_FUN=median, 
                              return_df=TRUE)

tmp <- agg_year[agg_year$from.source %in% c("Reuters", "AFP", 
                                            "Xinhua", "Interfax", 
                                            "DPA", 
                                            "AssociatedPress",
                                            "Yonhap", "AnadoluAgency", "ITAR-TASS"),  
                c('from.source', 'to.label', 'to.Vprop')]
tmp$to.Vprop <- tmp$to.Vprop*100
tmp$from.source <- str_replace(tmp$from.source, "AssociatedPress", "AP")
tmp$from.source <- str_replace(tmp$from.source, "AnadoluAgency", "Anadolu")
tmp %<>%
  arrange(desc(to.Vprop))
write.xlsx(tmp, "data/output/similarity_analysis.xlsx")

# plot a network of text re-use
edgelist_netwwork <- tmp %>%
  filter(to.Vprop > 0.3) %>%
  rename(from = from.source,
         to = to.label,
         weight = to.Vprop) %>%
  mutate(weight = weight/9)

g <- network(edgelist_netwwork)
ggnet2(g, label = TRUE)
x = network.vertex.names(g)
x = ifelse(x %in% c("Reuters", "AFP", 
                    "Xinhua", "Interfax", "DPA", "AP", "Anadolu","ITAR-TASS",
                    "Yonhap"), "news agency", "country & type of media")
g %v% "Source" = x
set.seed(145)
ggnet2(g, 
       color = "Source", 
       label = TRUE,
       edge.size = "weight",
       #edge.label = "weight",
       edge.label.size = 1,
       palette = c("news agency" = "gold", "country & type of media" = "grey"))

#ggsave("DJ Submission/network_similarities.png", width = 12, height = 6)

# create edgelist from all sources to all groups (country + type of media)
g_agg <- network_aggregate(g_digital, 
                           by_from='source',
                           by_to='label',
                           edge_attribute='hourdiff', 
                           agg_FUN=median)

e <- as_data_frame(g_agg, 'edges') %>%
  mutate(to.Vprop = to.Vprop*100) %>%
  select(from,
         to,
         weight = to.Vprop)

write.xlsx(e, "data/output/all_similarities.xlsx")

# generate a list of counts of sources
write.xlsx(dfmat_meta %>%
             group_by(source, source.country, label, media) %>%
             tally(), "data/output/sources_corpus.xlsx")

rm(list = ls())

####### Computing similarities - full corpus #######

# load dfms for analysis
dfm_digital <- readRDS("data/RR/dfmat_digital.RDS")
dfm_notdigital <- readRDS("data/RR/dfmat_nodigital.RDS")

dfmat <- rbind(dfm_digital,
               dfm_notdigital)

rm(dfm_digital,
   dfm_notdigital)

corpus_meta <- readxl::read_xlsx("data/raw/FullCorpusMetadata_revised.xlsx") %>%
  select(source = news_source_name,
         media = media_type_sampled) %>%
  distinct(source, .keep_all = TRUE)

dfmat_meta <- docvars(dfmat) %>%
  mutate(source = case_when(source == "BusinessDay" & source.country == "NG" ~ "BusinessDayNG",
                            source == "BusinessDay" & source.country == "ZA" ~ "BusinessDayZA",
                            source == "DailyMail" & source.country == "GB" ~ "DailyMailUK",
                            source == "SundayTimes" & source.country == "LK" ~ "SundayTimesLK",
                            source == "SundayTimes" & source.country == "ZA" ~ "SundayTimesZA",
                            source == "TheSouthernTimes" ~ "SouthernTimes",
                            source == "TheSunHerald" ~ "HeraldSun",
                            source == "Tubo" ~ "Tuko",
                            source == "TheStandard" & source.country == "KE" ~ "TheStandardKE",
                            source == "TheStandard" & source.country == "HK" ~ "TheStandardHK",
                            source == "TheStar" & source.country == "KE" ~ "TheStarKE",
                            source == "TheStar" & source.country == "ZA" ~ "TheStarZA",
                            source == "DailyMirror" & source.country == "LK" ~ "DailyMirrorLK",
                            source == "DailyMirror" & source.country == "GB" ~ "DailyMirrorUK",
                            source == "DailyNews" & source.country == "LK" ~ "DailyNewsLK",
                            source == "TheGuardian" & source.country == "NG" ~ "TheGuardianNG",
                            source == "TheGuardian" & source.country == "GB" ~ "TheGuardianUK",
                            .default = as.character(source))) %>%
  left_join(corpus_meta, by = "source")

dfmat$media <- dfmat_meta$media

# create dfms with & without news agency content
dfm_agencies <- dfm_subset(dfmat, media %in% c("newswire stories", "newswire copy"))
dfm_rest <- dfm_subset(dfmat, !media %in% c("newswire stories", "newswire copy"))

# add time variables
dfm_agencies$date <- paste0(dfm_agencies$date, " 09:00:00")
dfm_agencies$date <- as.character(dfm_agencies$date)
dfm_agencies$date <- ymd_hms(dfm_agencies$date)
dfm_agencies$date <- as.POSIXct(dfm_agencies$date)

dfm_rest$date <- paste0(dfm_rest$date, " 09:00:00")
dfm_rest$date <- as.character(dfm_rest$date)
dfm_rest$date <- ymd_hms(dfm_rest$date)
dfm_rest$date <- as.POSIXct(dfm_rest$date)

# merge dfms & add ids
dfm_digital <- rbind(dfm_agencies,
                     dfm_rest)
docnames(dfm_digital) <- sprintf("ID_%06d", seq(1,nrow(dfmat_meta)))  

rm(corpus_meta,
   dfm_agencies,
   dfm_rest,
   dfmat, 
   dfmat_meta)

# compute document level similarities
g_digital <- newsflow_compare(dfm_digital, 
                              date_var = "date",
                              hour_window = c(0,48), 
                              min_similarity = 0.97)

g_agg <- network_aggregate(g_digital, 
                           by_from='source',
                           by_to='label',
                           edge_attribute='hourdiff', 
                           agg_FUN=median)

e <- as_data_frame(g_agg, 'edges') %>%
  mutate(to.Vprop = to.Vprop*100) %>%
  select(from,
         to,
         weight = to.Vprop)
write.xlsx(e, "data/output/similarities_agencies_25countries.xlsx")