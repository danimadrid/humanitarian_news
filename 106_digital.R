library(tidyverse)
library(quanteda)
library(text2vec)
library(conText)
library(fst)
library(zoo)
library(tsibble)
library(lubridate)
library(ggthemes)
library(tidylog)
library(ggplot2)
library(ggthemes)
library(readr)
library(magrittr)
library(countrycode)
library(pacman)
library(quanteda.textstats)
library(quanteda.textplots)

p_load(cluster, factoextra, tidyverse, NbClust)

source("utils.R")

##### Load data & pre-process #####
# load full corpus
humdat <- read_fst("data/raw/_FinalData20210913.fst", as.data.table = T)

# read in local embedding
local_glove <- readRDS(file = "data/embedding/humdat_local_glove.rds")
# read in local transformation matrix (A matrix)
local_transform <- readRDS(file = "data/embedding/humdat_local_transform.rds")

# load metadata
corpus_meta <- readxl::read_xlsx("data/raw/FullCorpusMetadata_revised.xlsx") %>%
  select(source = news_source_name,
         media = media_type_sampled) %>%
  distinct(source, .keep_all = TRUE)

# format date
humdat$date <- as.character(humdat$publish_date)
humdat$date <- as.Date(humdat$date, format = "%Y%m%d")

# remove articles before 2010
humdat <- humdat %>%
  filter(date>="2010-01-01")

# fix wrong country codes in corpus
humdat %<>%
  mutate(source.country = str_replace(source.country, "EQ", "EC"),
         source.country = str_replace(source.country, "NB", "NA"))

# countries with digital and non digital journalism sources 
list.cntries.both <- c("AE", "AM", "AU", "CA", "CN", "DE", "FR", "GB", "GH", "IE", "IN", 
                       "JO", "KE", "KR", "MY", "NG", "NZ", "PH", "PK", "PS", "QA", "RU", 
                       "TR", "US", "ZA")

humdat %<>%
  filter(source.country %in% list.cntries.both)

srces <- tibble(source = humdat$source,
                country = humdat$source.country) %>%
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

srces <- tibble(source = srces$source,
                country = srces$country) %>%
  left_join(corpus_meta, by = "source") %>%
  mutate(media = case_when(media == "news site" ~ "digital",
                           .default = "not digital"),
         country.source = str_c(country, " ", media))

country.sources.counts <- as.data.frame(table(srces$country.source)) %>%
  rename(seqvar = Var1,
         n = Freq)


##### Keyness Analysis #####
humdat$digital <- srces$media
humdat$label <- srces$country.source

# break down corpus to fit in memory
humdat_digital <- humdat %>%
  filter(digital == "digital")

humdat_nodigital <- humdat %>%
  filter(digital != "digital") 

humdat_nodigital_uk <- humdat_nodigital %>%
  filter(source.country == "GB") 

humdat_nodigital_us <- humdat_nodigital %>%
  filter(source.country == "US") 

humdat_nodigital_other <- humdat_nodigital %>%
  filter(!source.country %in% c("US", "GB"))

rm(humdat)
rm(humdat_nodigital)

# tokenize corpus by group & add metadata - digital sources
toks_digital <- tokens(humdat_digital$full_text, 
                       remove_punct=T, 
                       remove_symbols=T, 
                       remove_numbers=T, 
                       remove_separators=T)
toks_digital$date <- humdat_digital$date
toks_digital$source <- humdat_digital$source
toks_digital$source.country <- humdat_digital$source.country
toks_digital$digital <- humdat_digital$digital
toks_digital$label <- humdat_digital$label
rm(humdat_digital)
saveRDS(toks_digital, "data/RR/toks_digital.RDS")

# tokenize corpus by group & add metadata - no digital sources UK
toks_nodigital_uk <- tokens(humdat_nodigital_uk$full_text, 
                       remove_punct=T, 
                       remove_symbols=T, 
                       remove_numbers=T, 
                       remove_separators=T)
toks_nodigital_uk$date <- humdat_nodigital_uk$date
toks_nodigital_uk$source <- humdat_nodigital_uk$source
toks_nodigital_uk$source.country <- humdat_nodigital_uk$source.country
toks_nodigital_uk$digital <- humdat_nodigital_uk$digital
toks_nodigital_uk$label <- humdat_nodigital_uk$label
rm(humdat_nodigital_uk)
saveRDS(toks_nodigital_uk, "data/RR/toks_nodigital_uk.RDS")

# tokenize corpus by group & add metadata - no digital sources US
toks_nodigital_us <- tokens(humdat_nodigital_us$full_text, 
                            remove_punct=T, 
                            remove_symbols=T, 
                            remove_numbers=T, 
                            remove_separators=T)
toks_nodigital_us$date <- humdat_nodigital_us$date
toks_nodigital_us$source <- humdat_nodigital_us$source
toks_nodigital_us$source.country <- humdat_nodigital_us$source.country
toks_nodigital_us$digital <- humdat_nodigital_us$digital
toks_nodigital_us$label <- humdat_nodigital_us$label
rm(humdat_nodigital_us)
saveRDS(toks_nodigital_us, "data/RR/toks_nodigital_us.RDS")

# tokenize corpus by group & add metadata - no digital sources Other
toks_nodigital_other <- tokens(humdat_nodigital_other$full_text, 
                            remove_punct=T, 
                            remove_symbols=T, 
                            remove_numbers=T, 
                            remove_separators=T)
toks_nodigital_other$date <- humdat_nodigital_other$date
toks_nodigital_other$source <- humdat_nodigital_other$source
toks_nodigital_other$source.country <- humdat_nodigital_other$source.country
toks_nodigital_other$digital <- humdat_nodigital_other$digital
toks_nodigital_other$label <- humdat_nodigital_other$label
rm(humdat_nodigital_other)
saveRDS(toks_nodigital_other, "data/RR/toks_nodigital_other.RDS")

toks_digital <- readRDS("data/RR/toks_digital.RDS")

toks_digital <- toks_digital %>%
  tokens_keep(min_nchar = 3) %>%
  tokens_remove(stopwords("en"))

dfmat_digital <- dfm(x = toks_digital)
rm(toks_digital)
saveRDS(dfmat_digital, "data/RR/dfmat_digital.RDS")

toks_nodigital_us <- readRDS("data/RR/toks_nodigital_us.RDS")
dfmat_nodigital_us <- dfm(x = toks_nodigital_us) %>%
  dfm_remove(stopwords("en")) %>%
  dfm_keep(min_nchar = 3)
rm(toks_nodigital_us)

toks_nodigital_uk <- readRDS("data/RR/toks_nodigital_uk.RDS")
dfmat_nodigital_uk <- dfm(x = toks_nodigital_uk) %>%
  dfm_remove(stopwords("en")) %>%
  dfm_keep(min_nchar = 3)
rm(toks_nodigital_uk)

toks_nodigital_other <- readRDS("data/RR/toks_nodigital_other.RDS")
dfmat_nodigital_other <- dfm(x = toks_nodigital_other) %>%
  dfm_remove(stopwords("en")) %>%
  dfm_keep(min_nchar = 3)
rm(toks_nodigital_other)

dfm_nodigital <- rbind(dfmat_nodigital_us,
                       dfmat_nodigital_uk,
                       dfmat_nodigital_other)
saveRDS(dfm_nodigital, "data/RR/dfmat_nodigital.RDS")
rm(dfmat_nodigital_us,
   dfmat_nodigital_uk,
   dfmat_nodigital_other)

dfmat <- rbind(dfm_nodigital, dfmat_digital)

rm(dfm_nodigital, 
   dfmat_digital)

tstat_key <- textstat_keyness(dfmat, 
                              target = dfmat$digital == "not digital")
textplot_keyness(tstat_key)

##### Compute document embeddings & plots Q1 #####

# set target and candidate words(s)

target_word <- "humanitarian"
candidate_words <- readRDS("data/qualtrics/Q1seed.rds")

# get vector of countries/type of media needed as seqvar for get_seq_cos_sim()
srces_media <- srces$country.source

cos_simsdf <-
  get_seq_cos_sim(
    x = news_corpus$full_text,
    seqvar = srces_media,
    target = target_word,
    candidates = candidate_words,
    pre_trained = local_glove,
    transform_matrix = local_transform
  )

saveRDS(cos_simsdf, "data/output/cos_simsQ1_digital.rds")
cos_simsdf <- readRDS("data/output/cos_simsQ1_digital.rds")

#take row average of different words in seed word set
cos_simsdf$sw_vec <- Matrix::rowMeans(cos_simsdf[1:2])

# plot overall country & type of media comparison
cos_simsdf %>%
  mutate(seqvar = str_replace(seqvar, "not digital", "not_digital")) %>%
  separate_wider_delim(seqvar, " ", names = c("country", "media")) %>%
  ggplot() +
  geom_point(aes(x = reorder(media, -sw_vec), y = sw_vec)) +
  labs(x = "country/type of media", y = "cosine similarity, humanitarian:crisis/disaster vector") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + coord_flip() +
  facet_grid(vars(country))

ggsave("plots/cos_sim_crisis_digital.png",
       width=300, height = 200, 
       dpi=300, units="mm", bg = "white")

# boxplots for digital vs. non digital
cos_simsdf %>%
  mutate(seqvar = str_replace(seqvar, "not digital", "not_digital")) %>%
  separate_wider_delim(seqvar, " ", names = c("country", "media")) %>%
  ggplot +
  geom_boxplot(aes(media, sw_vec)) +
  labs(x = "Type of media",
       y = "cosine similarity, humanitarian:crisis/disaster vector")

ggsave("plots/cos_sim_crisis_digitalnotdigital.png",
       width=300, height = 200,
       dpi=300, units="mm", bg = "white")

##### Compute document embeddings & plots Q2 #####

candidate_words2 <- readRDS("data/qualtrics/Q2seed.rds")

cos_simsdf2 <-
  get_seq_cos_sim(
    x = news_corpus$full_text,
    seqvar = srces_media,
    target = target_word,
    candidates = candidate_words2,
    pre_trained = local_glove,
    transform_matrix = local_transform
  )

saveRDS(cos_simsdf2, "data/output/cos_simsQ2_digital.rds")
cos_simsdf2 <- readRDS("data/output/cos_simsQ2_digital.rds")

#take row average of different words in seed word set
cos_simsdf2$sw_vec <- Matrix::rowMeans(cos_simsdf2[1:3])

# plot overall country & type of media comparison
cos_simsdf2 %>%
  mutate(seqvar = str_replace(seqvar, "not digital", "not_digital")) %>%
  separate_wider_delim(seqvar, " ", names = c("country", "media")) %>%
  ggplot() +
  geom_point(aes(x = reorder(media, -sw_vec), y = sw_vec)) +
  labs(x = "country/type of media", y = "cosine similarity, humanitarian:now/sudden/urgent vector") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + coord_flip() +
  facet_grid(vars(country))

ggsave("plots/cos_sim_urgent_digital.png",
       width=300, height = 200, 
       dpi=300, units="mm", bg = "white")

# boxplots for digital vs. non digital
cos_simsdf2 %>%
  mutate(seqvar = str_replace(seqvar, "not digital", "not_digital")) %>%
  separate_wider_delim(seqvar, " ", names = c("country", "media")) %>%
  ggplot +
  geom_boxplot(aes(media, sw_vec)) +
  labs(x = "Type of media",
       y = "cosine similarity, humanitarian:now/sudden/urgent vector")

ggsave("plots/cos_sim_urgent_digitalnotdigital.png",
       width=300, height = 200,
       dpi=300, units="mm", bg = "white")


##### Compute document embeddings & plots Q3 #####

candidate_words3 <- readRDS("data/qualtrics/Q3seed.rds")

cos_simsdf3 <-
  get_seq_cos_sim(
    x = news_corpus$full_text,
    seqvar = srces_media,
    target = target_word,
    candidates = candidate_words3,
    pre_trained = local_glove,
    transform_matrix = local_transform
  )

saveRDS(cos_simsdf3, "data/output/cos_simsQ3_digital.rds")
cos_simsdf3 <- readRDS("data/output/cos_simsQ3_digital.rds")

#take row average of different words in seed word set
cos_simsdf3$sw_vec <- Matrix::rowMeans(cos_simsdf3[1:3])

# plot overall country & type of media comparison
cos_simsdf3 %>%
  mutate(seqvar = str_replace(seqvar, "not digital", "not_digital")) %>%
  separate_wider_delim(seqvar, " ", names = c("country", "media")) %>%
  ggplot() +
  geom_point(aes(x = reorder(media, -sw_vec), y = sw_vec)) +
  labs(x = "country/type of media", y = "cosine similarity, humanitarian:help/rescue/save vector") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + coord_flip() +
  facet_grid(vars(country))

ggsave("plots/cos_sim_rescue_digital.png",
       width=300, height = 200, 
       dpi=300, units="mm", bg = "white")

# boxplots for digital vs. non digital
cos_simsdf3 %>%
  mutate(seqvar = str_replace(seqvar, "not digital", "not_digital")) %>%
  separate_wider_delim(seqvar, " ", names = c("country", "media")) %>%
  ggplot +
  geom_boxplot(aes(media, sw_vec)) +
  labs(x = "Type of media",
       y = "cosine similarity, humanitarian:help/rescue/save vector")

ggsave("plots/cos_sim_rescue_digitalnotdigital.png",
       width=300, height = 200,
       dpi=300, units="mm", bg = "white")

##### Data loading and filtering #####

# Load embeddings for each humanitarian imaginary dimensions
set.seed(1)
df1 <- readRDS("data/output/cos_simsQ1_digital.rds")
df2 <- readRDS("data/output/cos_simsQ2_digital.rds")
df3 <- readRDS("data/output/cos_simsQ3_digital.rds")

# Filter out countries with less than 10k articles in corpus
df <- left_join(df1, df2, by = "seqvar") %>%
  left_join(df3, by = "seqvar") %>%
  left_join(country.sources.counts, by = "seqvar") %>%
  mutate(country = str_extract(seqvar, "[A-Z]{2}")) %>%
  filter(!country %in% c("AM", "GH", "KE", "PS", "JO")) %>%
  mutate(seqvar = str_replace(seqvar, "not digital", ""),
         seqvar = str_replace(seqvar, "digital", "dgt"))
rownames(df) <- df$seqvar

# Scale and center values
df <- df %>%
  select(-seqvar,
         -country) %>%
  scale()

# Determine ideal k for k means
NbClust(df, method = 'complete', index = 'all')$Best.nc

k3 <- kmeans(df, centers = 3, nstart = 100, algorithm = "Forgy", iter.max = 100)

# Recode the values using a simple mapping function
clusters <- k3$cluster

recode_clusters <- sapply(clusters, function(x) {
  if (x == 3) {
    return(1)
  } else if (x == 1) {
    return(3)
  } else {
    return(x)  # if there are any other values, keep them as they are
  }
})

# Assign the recoded values back to the k3 list
k3$cluster <- recode_clusters

fviz_cluster(k3, data = df, geom = "text",
             main = "",
             label.rectangle = TRUE,
             labelsize = 30,
             legend = "bottom",
             legend.title = "Cluster",
             font.legend = c(22, "plain", "black"),
             font.x = c(22, "plain", "black"),
             font.y = c(22, "plain", "black"),
             #ylim = c(-3.5, 3.5),
             #xlim = c(-3.5, 3.5),
             ggtheme = theme_minimal())  + scale_x_reverse() 

ggsave("plots/kmeans3_digital2.png",
       width=755, height = 775,
       dpi=300, units="mm", bg = "white")