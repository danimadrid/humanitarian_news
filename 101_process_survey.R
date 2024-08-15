library(dplyr)
library(ggplot2)
library(tidytext)

## get survey responses for Q1

responses <- read.csv("data/qualtrics/emerg_imag_form_December 19, 2021_05.57.csv")

responses <- responses[3:8,]
responses <- responses[-6,]

cnames <- colnames(responses[,18:27])
words_all <- data.frame()
for (i in seq_along(cnames)) {
  cname <- cnames[i]
  words <- as.data.frame(recode(responses[,cname], 
                                "1" = "Crisis", "2" = "Disaster", "3" = "Famine",
                                "4" = "Flood", "5" = "Hurricane", "6" = "Earthquake",
                                "7" = "Typhoon", "8" = "Drought", "9" = "Conflict",
                                "10" = "Violence"))
  colnames(words) <- cname
  
  words_all <- merge(words_all, words, 
                     by = 0, all = TRUE)[-1]
}

wordsdf <- as.data.frame(do.call(paste, words_all))
colnames(wordsdf) <- "words"

words <- wordsdf %>% 
  unnest_tokens(word, words)

table(words)

Q1seed <- c("crisis", "disaster")
saveRDS(Q1seed, "data/qualtrics/Q1seed.rds")


## get survey responses for Q2

cnames <- colnames(responses[,33:42])
words_all <- data.frame()
for (i in seq_along(cnames)) {
  cname <- cnames[i]
  words <- as.data.frame(recode(responses[,cname], 
                                "1" = "Now", "2" = "Sudden", "3" = "Urgent",
                                "4" = "Desperate", "5" = "Immediate", "6" = "Crucial",
                                "7" = "Critical", "8" = "Serious", "9" = "Pressing",
                                "10" = "Vital"))
  colnames(words) <- cname
  
  words_all <- merge(words_all, words, 
                     by = 0, all = TRUE)[-1]
}

wordsdf <- as.data.frame(do.call(paste, words_all))
colnames(wordsdf) <- "words"

words <- wordsdf %>% 
  unnest_tokens(word, words)

table(words)

Q2seed <- c("now", "sudden", "urgent")
saveRDS(Q2seed, "data/qualtrics/Q2seed.rds")

## get survey responses for Q3

cnames <- colnames(responses[,48:57])
words_all <- data.frame()
for (i in seq_along(cnames)) {
  cname <- cnames[i]
  words <- as.data.frame(recode(responses[,cname], 
                                "1" = "Save", "2" = "Help", "3" = "Rescue",
                                "4" = "Give", "5" = "Donate", "6" = "Assist",
                                "7" = "Aid", "8" = "Respond", "9" = "Appeal",
                                "10" = "Rescue"))
  colnames(words) <- cname
  
  words_all <- merge(words_all, words, 
                     by = 0, all = TRUE)[-1]
}

wordsdf <- as.data.frame(do.call(paste, words_all))
colnames(wordsdf) <- "words"

words <- wordsdf %>% 
  unnest_tokens(word, words)

table(words)

Q3seed <- c("help", "rescue", "save")
saveRDS(Q3seed, "data/qualtrics/Q3seed.rds")