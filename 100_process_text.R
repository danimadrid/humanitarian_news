library(dplyr)
library(ggplot2)
library(ggthemes)
library(textstem)
library(fst)
library(countrycode)
library(tm)

set.seed(123L)

humdat <- readRDS("data/raw/_FinalData20210913.rds")
#generate fst file for faster import
write_fst(humdat, "data/raw/_FinalData20210913.fst")

# get country codes with full names
cntcodes <- codelist %>%
  select(iso2c, iso3c, iso.name.en) %>%
  rename(source.country = iso2c,
         country_name = iso.name.en)
saveRDS(cntcodes, "data/output/cntcodes.rds")

# check data and plot
## plot of count of articles by country, filtering out countries with < 1000 articles

humdat <- read_fst("data/raw/_FinalData20210913.fst", as.data.table = T)

cntcodes <- readRDS("data/output/cntcodes.rds")

humdat %>%
  group_by(source.country) %>%
  summarise(count = n()) %>%
  filter(count >=1000) %>%
  left_join(cntcodes, by = "source.country") %>%
  ggplot() +
  geom_bar(aes(reorder( country_name, count), y = count), stat="identity") +
  labs(y = "Country", x = "# articles") +
  coord_flip()

ggsave("plots/countrycounts.png",
       width=300, height = 300, 
       dpi=300, units="mm", bg = "white")

# sample from full dataset

# get 10k sample
humdatsamp <- humdat %>%
  sample_n(10000)

#format date
humdatsamp$date <- as.character(humdatsamp$publish_date)
humdatsamp$date <- as.Date(humdatsamp$date, format = "%Y%m%d")

saveRDS(humdatsamp, "data/output/sample_10k.rds")

# sample 1000 from each country 

humdatsamp <- humdat %>%
  group_by(source.country) %>%
  mutate(count = n()) %>%
  filter(count >=1000) %>%
  sample_n(1000)

#79k observations, keeping only countries with >=1000 articles at least total, 
#and taking 1000 from each country

#format date
humdatsamp$date <- as.character(humdatsamp$publish_date)
humdatsamp$date <- as.Date(humdatsamp$date, format = "%Y%m%d")

saveRDS(humdatsamp, "data/output/sample_79k.rds")

# get totals by year for emergency word trend check

humdatotals <- humdatsamp %>%
  filter(date >= "2010-01-01") %>%
  group_by(date) %>%
  summarise(count = n())

target_word <- "emergency"

humdatsamp %>%
  mutate(obs=1) %>%
  filter(grepl(target_word,full_text, ignore.case = T)) %>%
  group_by(date) %>%
  summarise(sum_words = sum(obs)) %>%
  filter(date >= "2010-01-01") %>%
  full_join(humdatotals,  by="date") %>%
  mutate(sum_words= ifelse(is.na(sum_words), 0, sum_words),
         pctword = sum_words/count) %>%
  ggplot() +
  geom_bar(aes(date, pctword), 
           stat = "identity") +
  geom_smooth(aes(date, pctword), 
              method = "loess", se =F, col = "red", span =.1) +
  theme_tufte(base_family = "Helvetica") +
  labs(x = "", y = "normalized word freq.") +
  ggtitle(paste("word:", target_word)) 

# not much variation over time; initial check against temporal confounding

ggsave("plots/normemergfreq.png",
       width=300, height = 200, 
       dpi=300, units="mm", bg = "white")