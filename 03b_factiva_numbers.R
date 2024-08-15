library(plyr)

## Used the csv option on the Factiva results page
## Only returns the top 100 cases
## The total number of the source count returned in these csv files is 530,729
## The number of Factiva results returned for the 1/1/2016-30/4/2020 period was 773,987

## This separates the publication and count columns from the rest of the data
pubs_count <- function(csv) {
  df <- read.csv(csv, stringsAsFactors = F)
  return(df[,1:2])
}

all_2016 <- do.call(rbind.fill, lapply(dir(), pubs_count))
all_2016 <- all_2016[-which(is.na(all_2016$Document.Count) == TRUE), ] ## Removes some NA cases that appeared
write.csv(all_2016, file = "all_2016.csv", row.names = F)
sum(all_2016$Document.Count)

all_2017 <- do.call(rbind.fill, lapply(dir(), pubs_count))
sum(all_2017$Document.Count)
write.csv(all_2017, file = "all_2017.csv", row.names = F)

all_2018 <- do.call(rbind.fill, lapply(dir(), pubs_count))
sum(all_2018$Document.Count)
write.csv(all_2018, file = "all_2018.csv", row.names = F)

all_2019 <- do.call(rbind.fill, lapply(dir(), pubs_count))
all_2019 <- all_2019[-which(is.na(all_2019$Document.Count) == TRUE), ]
sum(all_2019$Document.Count)
write.csv(all_2019, file = "all_2019.csv", row.names = F)

all_2020 <- do.call(rbind.fill, lapply(dir(), pubs_count))
all_2020 <- all_2020[-which(is.na(all_2020$Document.Count) == TRUE), ]
sum(all_2020$Document.Count)
write.csv(all_2020, file = "all_2020.csv", row.names = F)


### Create the totals files for each year
#all_2016 <- read.csv("all_2016.csv", stringsAsFactors = F)
#all_2017 <- read.csv("all_2017.csv", stringsAsFactors = F)
#all_2018 <- read.csv("all_2018.csv", stringsAsFactors = F)
#all_2019 <- read.csv("all_2019.csv", stringsAsFactors = F)
all_2020 <- read.csv("all_2020.csv", stringsAsFactors = F)
colnames(all_2020) <- c("publication", "n")

## Returns a count for each unique publication from the 'all' files and writes a 'total' file
article_number <- function (unique) { 
  df <- all_2020
  loc <- which(df[,1] == unique)
  print(unique)
  return(sum(df$n[loc]))
}

unlist(lapply(unique(all_2020$publication), article_number))

totals_2020_df <- cbind.data.frame(unique(all_2020$publication), unlist(lapply(unique(all_2020$publication), article_number)))

## Change col names
colnames(totals_2020_df) <- c('Publication', 'n')
head(totals_2020_df)

## Write file
write.csv(totals_2020_df, file = 'total_2020.csv', row.names = F)


## Get unique list of publications across all total files
## Create a df with unique pubs, 2016, 2017, 2018, 2019, 2020 <- asign year columns a value of x
## Use match function across each of the year files to pull values (what to do with any NAs???)

pub_names <- function (x) {
  
  y <- read.csv(x, stringsAsFactors = F)
  return(y$Publication)
}
all_pubs <- unlist(lapply(dir()[6:10], pub_names))
publications <- unique(all_pubs)
publications

## Create matrix
news_matrix <- cbind.data.frame(publications, rep("Factiva", length(publications)), rep(0, length(publications)), 
                                rep(0, length(publications)), rep(0, length(publications)), rep(0, length(publications)),
                                rep(0, length(publications)))
## Change column names
colnames(news_matrix) <- c('News.source.name', 'Database', 'N Articles 2016', 'N Articles 2017', 'N Articles 2018', 'N Articles 2019', 'N Articles 2020')
write.csv(news_matrix, file = 'all_total.csv', row.names = F)
news_matrix <- read.csv('all_total.csv', stringsAsFactors = F)


total_2016 <- read.csv("total_2016.csv", stringsAsFactors = F)
total_2016 <- rbind.data.frame(total_2016, c("no_match", 0)) ## Bind to nomatch case
news_matrix$N.Articles.2016 <- as.numeric(total_2016$n[match(news_matrix$News.source.name, total_2016$Publication, nomatch = length(total_2016$Publication))])
sum(news_matrix$N.Articles.2016)

total_2017 <- read.csv("total_2017.csv", stringsAsFactors = F)
total_2017 <- rbind.data.frame(total_2017, c("no_match", 0))
news_matrix$N.Articles.2017 <- as.numeric(total_2017$n[match(news_matrix$News.source.name, total_2017$Publication, nomatch = length(total_2017$Publication))])
sum(news_matrix$N.Articles.2017)

total_2018 <- read.csv("total_2018.csv", stringsAsFactors = F)
total_2018 <- rbind.data.frame(total_2018, c("no_match", 0))
news_matrix$N.Articles.2018 <- as.numeric(total_2018$n[match(news_matrix$News.source.name, total_2018$Publication, nomatch = length(total_2018$Publication))])
sum(news_matrix$N.Articles.2018)

total_2019 <- read.csv("total_2019.csv", stringsAsFactors = F)
total_2019 <- rbind.data.frame(total_2019, c("no_match", 0))
news_matrix$N.Articles.2019 <- as.numeric(total_2019$n[match(news_matrix$News.source.name, total_2019$Publication, nomatch = length(total_2019$Publication))])
sum(news_matrix$N.Articles.2019)

total_2020 <- read.csv("total_2020.csv", stringsAsFactors = F)
total_2020 <- rbind.data.frame(total_2020, c("no_match", 0))
news_matrix$N.Articles.2020 <- as.numeric(total_2020$n[match(news_matrix$News.source.name, total_2020$Publication, nomatch = length(total_2020$Publication))])
sum(news_matrix$N.Articles.2020)

str(news_matrix)

news_matrix$source.total <- rowSums(news_matrix[,3:7])
write.csv(news_matrix, file = 'all_total.csv', row.names = F)

news_matrix <- read.csv('all_total.csv', stringsAsFactors = F)
