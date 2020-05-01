### I used the inbuilt Google Chrome web scraper to pull sources and their count for two month periods
### The csv files they produce are very messy so require a little bit of cleaning

## Function to clean up scraped data
pubs_scrape_clean <- function (x) {
  y <- read.csv(x, stringsAsFactors = F)
  
  y$pubs <- gsub(",", "", y$pubs) ## Remove all the commas from the numbers (e.g. 1,000)
  
  y$n <- trimws(str_extract(y$pubs, "(?<=\n).*$"))  ## Extracts the numbers from the string
  
  y$publication <- trimws(gsub("\\n.*", "", y$pubs))  ## Cleans up the publication name
  
  y_2 <- y[, c(5,4)]  ## Reorder the dataframe
  
  write.csv(y_2, file = paste0("clean_", x), row.names = F)
}

sapply(dir(), pubs_scrape_clean)

########################################################
## Binding the files to a single file ##################
########################################################
a <- read.csv("clean_ja_2017.csv", stringsAsFactors = F)
b <- read.csv("clean_jf_2017.csv", stringsAsFactors = F)
c <- read.csv("clean_ma_2017.csv", stringsAsFactors = F)
d <- read.csv("clean_mj_2017.csv", stringsAsFactors = F)
e <- read.csv("clean_nd_2017.csv", stringsAsFactors = F)
f <- read.csv("clean_so_2017.csv", stringsAsFactors = F)
all_df <- rbind.data.frame(a,b,c,d,e,f)
unique(all_df$publication)
write.csv(all_df, file = "all_2017.csv", row.names = F)


## Function for creating files containing totals
create_all_files <- function (x) {
  
  all_file <- read.csv(x, stringsAsFactors = F)
  n <- as.vector(mapply(article_number, x, unique(all_file$publication)))
  
  totals_file <- cbind.data.frame(unique(all_file$publication), n)
  colnames(totals_file) <- c('Publication', 'n')
  
  write.csv(totals_file, file = paste0("totals_", x), row.names = F)
  print(paste0(x, " ", "is done", sep = " "))
}
lapply(dir()[3:5], create_all_files)


## Quality check of the number of articles
## Consistently 3k under the Factiva totals
number_check <- function (x) {
  
  y <- read.csv(x, stringsAsFactors = F)
  print(sum(y$n, na.rm=T))
}
lapply(dir()[6:10], number_check)


## Get unique list of publications across all total files
## Create a df with unique pubs, 2016, 2017, 2018, 2019, 2020 <- asign year columns a value of x
## Use match function across each of the year files to pull values (what to do with any NAs???)

pub_names <- function (x) {
  
  y <- read.csv(x, stringsAsFactors = F)
  return(y$Publication)
}
all_pubs <- unlist(lapply(dir()[6:10], pub_names))
publications <- unique(all_pubs)


## Create matrix
news_matrix <- cbind.data.frame(publications, rep("Lexis", length(publications)), rep(0, length(publications)), 
                                rep(0, length(publications)), rep(0, length(publications)), rep(0, length(publications)),
                                rep(0, length(publications)))
## Change column names
colnames(news_matrix) <- c('News source name', 'Database', 'N Articles 2016', 'N Articles 2017', 'N Articles 2018', 'N Articles 2019', 'N Articles 2020')
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
