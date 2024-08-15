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

source("utils.R")

##### Load data & pre-process #####

# read in local embedding
local_glove <- readRDS(file = "data/embedding/humdat_local_glove.rds")
# read in local transformation matrix (A matrix)
local_transform <- readRDS(file = "data/embedding/humdat_local_transform.rds")

# # (GloVe) pre-trained embeddings
# local_glove <- readRDS("data/embedding/pretrained/glove.rds")
# # transformation matrix
# local_transform <- readRDS("data/embedding/pretrained/khodakA.rds")
# read in newspaper corpus
# news_corpus <- readRDS("data/output/sample_79k.rds")

# read in newspaper corpus
news_corpus <- read_fst("data/raw/_FinalData20210913.fst", as.data.table = T,
                        columns = c("full_text", 
                                    "source.country", 
                                    "publish_date"))

# format date
news_corpus$date <- as.character(news_corpus$publish_date)
news_corpus$date <- as.Date(news_corpus$date, format = "%Y%m%d")

# remove articles before 2010
news_corpus <- news_corpus %>%
  filter(date>="2010-01-01")

# fix wrong country codes in corpus
news_corpus %<>%
  mutate(source.country = str_replace(source.country, "EQ", "EC"),
         source.country = str_replace(source.country, "NB", "NA"))

##### Compute document embeddings & plots Q1 #####

# set target and candidate words(s)

target_word <- "humanitarian"

candidate_words <- readRDS("data/qualtrics/Q1seed.rds")

# NOTE have removed propagation step as it doesn't add any useful new terms
# # propagate labels (switch graphics to T for click-button interface)
# nns_selected <- label_propagate(candidate_words, local_glove, 
#                                 graphics = F, N= 10, norm = "l2")
# 
# #  1, 2
# 
# #repeat procedure
# nns_selected <- label_propagate(nns_selected, local_glove, 
#                                 graphics = F, N= 50, norm = "l2")
# 
# #  1, 2, 22

# saveRDS(nns_selected, "data/qualtrics/Q1seeds.rds")
# Q1seeds <- readRDS("data/qualtrics/Q1seeds.rds")

# get vector of countries needed as seqvar for get_seq_cos_sim()
cntries <- news_corpus$source.country

cos_simsdf <-
  get_seq_cos_sim(
    x = news_corpus$full_text,
    seqvar = cntries,
    target = target_word,
    candidates = candidate_words,
    pre_trained = local_glove,
    transform_matrix = local_transform
  )

saveRDS(cos_simsdf, "data/output/cos_simsQ1_allcntries.rds")
cos_simsdf <- readRDS("data/output/cos_simsQ1_allcntries.rds")

# get names from country codes

cntcodes <- readRDS("data/output/cntcodes.rds")
cntcodes <- cntcodes %>%
  rename(seqvar = source.country)

cos_simsdf <- cos_simsdf %>%
  left_join(cntcodes, by = "seqvar")

cos_countries <- cos_simsdf %>%
  select(country_name, seqvar, iso3c)

write_csv(cos_countries, "data/output/cos_countries.csv")

#take row average of different words in seed word set
cos_simsdf$sw_vec <- Matrix::rowMeans(cos_simsdf[1:2])

# plot
cos_simsdf %>%
  ggplot() +
  geom_point(aes(x = reorder(country_name, -sw_vec), y = sw_vec)) +
  labs(x = "country", y = "cosine similarity, humanitarian:crisis/disaster vector") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + coord_flip()

ggsave("plots/cos_sim_crisis.png",
       width=300, height = 200, 
       dpi=300, units="mm", bg = "white")

# group countries by region
regions <- read.csv("data/output/regions.csv", sep = "") %>%
  mutate(regions_abr = case_when(region == "Europe & Central Asia" ~ "Europe &\nCentral Asia",
                                 region == "Middle East & North Africa" ~ "MENA",
                                 region == "South Asia" ~ "South Asia",
                                 region == "Latin America & Caribbean" ~ "LatAm &\nCaribbean",
                                 region == "Sub-Saharan Africa" ~ "Sub-Saharan\nAfrica",
                                 region == "East Asia & Pacific" ~ "East Asia &\nPacific",
                                 region == "North America" ~ "North America"))

## splitting by region:
cos_simsdf %>%
  left_join(regions, by = "iso3c") %>%
  ggplot +
  geom_boxplot(aes(x=reorder(region,sw_vec, median), sw_vec)) + 
  labs(x = "Region",
       y = "cosine similarity, humanitarian:crisis/disaster vector")

ggsave("plots/cos_sim_crisis_region.png",
       width=300, height = 200,
       dpi=300, units="mm", bg = "white")

## US/UK comparison

cos_simsdf %>%
  left_join(regions, by = "iso3c") %>%
  mutate(US_UK = case_when(iso3c=="GBR"|iso3c=="USA" ~ "US_UK",
                   TRUE ~ "other")) %>%
  ggplot +
  geom_boxplot(aes(US_UK, sw_vec)) +
  labs(x = "Region",
       y = "cosine similarity, humanitarian:crisis/disaster vector")

ggsave("plots/cos_sim_crisis_usuk.png",
       width=300, height = 200,
       dpi=300, units="mm", bg = "white")

# overlay distributions US-UK versus other

sw_vec <- as.data.frame(cos_simsdf$sw_vec)
colnames(sw_vec) <- "sw_vec"

cos_simsdf %>%
  left_join(regions, by = "iso3c") %>%
  mutate(US_UK = case_when(iso3c=="GBR"|iso3c=="USA" ~ "US_UK",
                           TRUE ~ "other")) %>%
  ggplot +
  geom_density(aes(sw_vec, group = US_UK, fill = US_UK),
               alpha = .5) +
  scale_fill_grey() +
  xlim(.15, .25) +
  theme_tufte(base_family = "Helvetica")

#compare cosine similarities between US/UK and non-DAC countries
# Brazil, Russia, India, China, S Africa, Kuwait, Qatar, Saudi, UAE, Turkey
cos_simsdf %>%
  left_join(regions, by = "iso3c") %>%
  mutate(US_UK_NO_DAC = case_when(iso3c=="GBR"|iso3c=="USA" ~ "US_UK",
                                  iso3c=="BRA"|iso3c=="RUS"|iso3c=="IND"|iso3c=="CHN"|iso3c=="ZAF"|iso3c=="KWT"||iso3c=="QAT"|iso3c=="SAU"|iso3c=="ARE"|iso3c=="TUR" ~ "NO_DAC",
                                  TRUE ~ "other")) %>%
  filter(US_UK_NO_DAC != "other") %>%
  ggplot +
  geom_boxplot(aes(US_UK_NO_DAC, sw_vec)) +
  labs(x = "Region",
       y = "cosine similarity, humanitarian:crisis/disaster vector")

ggsave("plots/cos_sim_crisis_usuknodac.png",
       width=300, height = 200,
       dpi=300, units="mm", bg = "white")


#compare distributions across UN regions.
data(codelist) # extract list of regions from countrycode package
un_regions <- tibble(un_subregion = codelist$un.regionsub.name,
                     iso3c = codelist$iso3c)

cos_simsdf %>%
  left_join(un_regions, by = "iso3c") %>%
  mutate(un_subregion = replace_na(un_subregion, "Eastern Asia")) %>%
  ggplot +
  geom_boxplot(aes(x=reorder(un_subregion,sw_vec, median), sw_vec)) + 
  labs(x = "Region",
       y = "cosine similarity, humanitarian:crisis/disaster vector")

ggsave("plots/cos_sim_crisis_un_region.png",
       width=300, height = 200,
       dpi=300, units="mm", bg = "white")


# filter out countries low data (keep over 5000 articles)
countries_large <- news_corpus %>% 
  group_by(source.country) %>% 
  count() %>%
  filter(n > 5000) %>%
  rename(iso2c = source.country)

# plot with selected countries and regions plot
cos_simsdf %>%
  left_join(regions, by = "iso3c") %>%
  filter(iso2c %in% countries_large$iso2c) %>%
  #filter(iso3c %in% c("USA","CAN","GBR","RUS","TKM","PRK","VNM","SGP","SAU","TUR","CHN" , "QAT")) %>%
  mutate(regions_abr = replace_na(regions_abr, "East Asia &\nPacific"),
         country = replace_na(country, "North Korea")) %>%
  ggplot() +
  geom_point(aes(x = reorder(country, -sw_vec), y = sw_vec)) +
  labs(x = "country", y = "cosine similarity, humanitarian:crisis/disaster vector") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  coord_flip() +
  facet_grid(regions_abr ~ ., scales = "free_y")

##### Compute document embeddings & plots Q2 #####

candidate_words2 <- readRDS("data/qualtrics/Q2seed.rds")

cos_simsdf2 <-
  get_seq_cos_sim(
    x = news_corpus$full_text,
    seqvar = cntries,
    target = target_word,
    candidates = candidate_words2,
    pre_trained = local_glove,
    transform_matrix = local_transform
  )

saveRDS(cos_simsdf2, "data/output/cos_simsQ2_allcntries.rds")
cos_simsdf2 <- readRDS("data/output/cos_simsQ2_allcntries.rds")

cos_simsdf2 <- cos_simsdf2 %>%
  left_join(cntcodes, by = "seqvar")

#take row average of different words in seed word set
cos_simsdf2$sw_vec <- Matrix::rowMeans(cos_simsdf2[1:3])

# plot
cos_simsdf2 %>%
  ggplot() +
  geom_point(aes(x = reorder(country_name, -sw_vec), y = sw_vec)) +
  labs(x = "country", y = "cosine similarity, humanitarian:now/sudden/urgent vector") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip()

ggsave("plots/cos_sim_urgent.png",
       width=300, height = 200, 
       dpi=300, units="mm", bg = "white")

## splitting by region:
cos_simsdf2 %>%
  left_join(regions, by = "iso3c") %>%
  ggplot +
  geom_boxplot(aes(x=reorder(region,sw_vec, median), sw_vec)) + 
  labs(x = "Region",
       y = "cosine similarity, humanitarian:now/sudden/urgent vector")

ggsave("plots/cos_sim_urgent_region.png",
       width=300, height = 200,
       dpi=300, units="mm", bg = "white")

## US/UK comparison

cos_simsdf2 %>%
  left_join(regions, by = "iso3c") %>%
  mutate(US_UK = case_when(iso3c=="GBR"|iso3c=="USA" ~ "US_UK",
                           TRUE ~ "other")) %>%
  ggplot +
  geom_boxplot(aes(US_UK, sw_vec)) +
  labs(x = "Region",
       y = "cosine similarity, humanitarian:now/sudden/urgent vector")

ggsave("plots/cos_sim_urgent_usuk.png",
       width=300, height = 200,
       dpi=300, units="mm", bg = "white")

#compare cosine similarities between US/UK and non-DAC countries
# Brazil, Russia, India, China, S Africa, Kuwait, Qatar, Saudi, UAE, Turkey
cos_simsdf2 %>%
  left_join(regions, by = "iso3c") %>%
  mutate(US_UK_NO_DAC = case_when(iso3c=="GBR"|iso3c=="USA" ~ "US_UK",
                                  iso3c=="BRA"|iso3c=="RUS"|iso3c=="IND"|iso3c=="CHN"|iso3c=="ZAF"|iso3c=="KWT"||iso3c=="QAT"|iso3c=="SAU"|iso3c=="ARE"|iso3c=="TUR" ~ "NO_DAC",
                                  TRUE ~ "other")) %>%
  filter(US_UK_NO_DAC != "other") %>%
  ggplot +
  geom_boxplot(aes(US_UK_NO_DAC, sw_vec)) +
  labs(x = "Region",
       y = "cosine similarity, humanitarian:now/sudden/urgent vector")

ggsave("plots/cos_sim_urgent_usuknodac.png",
       width=300, height = 200,
       dpi=300, units="mm", bg = "white")

# compare cosine similarities among UN sub-regions
cos_simsdf2 %>%
  left_join(un_regions, by = "iso3c") %>%
  mutate(un_subregion = replace_na(un_subregion, "Eastern Asia")) %>%
  ggplot +
  geom_boxplot(aes(x=reorder(un_subregion,sw_vec, median), sw_vec)) + 
  labs(x = "Region",
       y = "cosine similarity, humanitarian:now/sudden/urgent vector")

ggsave("plots/cos_sim_urgent_un_region.png",
       width=300, height = 200,
       dpi=300, units="mm", bg = "white")

##### Compute document embeddings & plots Q3 #####

candidate_words3 <- readRDS("data/qualtrics/Q3seed.rds")

cos_simsdf3 <-
  get_seq_cos_sim(
    x = news_corpus$full_text,
    seqvar = cntries,
    target = target_word,
    candidates = candidate_words3,
    pre_trained = local_glove,
    transform_matrix = local_transform
  )

saveRDS(cos_simsdf3, "data/output/cos_simsQ3_allcntries.rds")
cos_simsdf3 <- readRDS("data/output/cos_simsQ3_allcntries.rds")

cos_simsdf3 <- cos_simsdf3 %>%
  left_join(cntcodes, by = "seqvar")

#take row average of different words in seed word set
cos_simsdf3$sw_vec <- Matrix::rowMeans(cos_simsdf3[1:3])

# plot
cos_simsdf3 %>%
  ggplot() +
  geom_point(aes(x = reorder(country_name, -sw_vec), y = sw_vec)) +
  labs(x = "country", y = "cosine similarity, humanitarian:help/rescue/save vector") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip()

ggsave("plots/cos_sim_rescue.png",
       width=300, height = 200, 
       dpi=300, units="mm", bg = "white")

## splitting by region:
cos_simsdf3 %>%
  left_join(regions, by = "iso3c") %>%
  ggplot +
  geom_boxplot(aes(x=reorder(region,sw_vec, median), sw_vec)) + 
  labs(x = "Region",
       y = "cosine similarity, humanitarian:help/rescue/save vector")

ggsave("plots/cos_sim_rescue_region.png",
       width=300, height = 200,
       dpi=300, units="mm", bg = "white")

## US/UK comparison

cos_simsdf3 %>%
  left_join(regions, by = "iso3c") %>%
  mutate(US_UK = case_when(iso3c=="GBR"|iso3c=="USA" ~ "US_UK",
                           TRUE ~ "other")) %>%
  ggplot +
  geom_boxplot(aes(US_UK, sw_vec)) +
  labs(x = "Region",
       y = "cosine similarity, humanitarian:help/rescue/save vector")

ggsave("plots/cos_sim_rescue_usuk.png",
       width=300, height = 200,
       dpi=300, units="mm", bg = "white")

#compare cosine similarities between US/UK and non-DAC countries
# Brazil, Russia, India, China, S Africa, Kuwait, Qatar, Saudi, UAE, Turkey
cos_simsdf3 %>%
  left_join(regions, by = "iso3c") %>%
  mutate(US_UK_NO_DAC = case_when(iso3c=="GBR"|iso3c=="USA" ~ "US_UK",
                                  iso3c=="BRA"|iso3c=="RUS"|iso3c=="IND"|iso3c=="CHN"|iso3c=="ZAF"|iso3c=="KWT"||iso3c=="QAT"|iso3c=="SAU"|iso3c=="ARE"|iso3c=="TUR" ~ "NO_DAC",
                                  TRUE ~ "other")) %>%
  filter(US_UK_NO_DAC != "other") %>%
  ggplot +
  geom_boxplot(aes(US_UK_NO_DAC, sw_vec)) +
  labs(x = "Region",
       y = "cosine similarity, humanitarian:help/rescue/save vector")

ggsave("plots/cos_sim_rescue_usuknodac.png",
       width=300, height = 200,
       dpi=300, units="mm", bg = "white")

# compare cosine similarities among UN sub-regions
cos_simsdf3 %>%
  left_join(un_regions, by = "iso3c") %>%
  mutate(un_subregion = replace_na(un_subregion, "Eastern Asia")) %>%
  ggplot +
  geom_boxplot(aes(x=reorder(un_subregion,sw_vec, median), sw_vec)) + 
  labs(x = "Region",
       y = "cosine similarity, humanitarian:help/rescue/save vector")

ggsave("plots/cos_sim_rescue_un_region.png",
       width=300, height = 200,
       dpi=300, units="mm", bg = "white")