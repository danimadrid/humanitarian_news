library(pacman)
p_load(stringr,textreadr,dplyr,magrittr,tidyverse,lubridate,furrr,data.table,tictoc, pdftools,readxl)
plan(multisession(workers = 10))

###### Part A - Nexis Data ######
list_countries <- list.files(path = "/XXXXXXXXXX", 
                             pattern = "[A-Z]_Nexis",
                             recursive = FALSE, 
                             full.names = FALSE) %>%
  str_replace(.,"_Nexis","")

get_lexis_articles <- function(filename){
  full_text <- read_rtf(filename)
  file.docx <- str_extract(filename, "(?<=HumanitarianNewsData/)(.*?)(?=/)")
  title.docx <- as.character(full_text[1])
  source.docx <- as.character(full_text[2])
  date.docx <- full_text[3]
  pos.length <- grep('Length:', full_text)
  length.docx <- full_text[pos.length]
  pos2 = grep('Classification', full_text)
  pos1 = grep('Body', full_text)
  if(length(pos1) < 1){
    pos1 <- pos2-1
  }
  text.docx <- full_text[(pos1+1):(pos2-1)]
  text.docx <- paste(gsub("[\r\n]", "", text.docx),sep = " ", collapse = " ")
  pos.language <- grep('Language:', full_text)
  language.docx <- full_text[pos.language]
  if(length(pos.language) < 1){
    language.docx <- " "
  }
  pos.pubtype <- grep('Publication-Type:', full_text)
  pubtype.docx <- full_text[pos.pubtype]
  if(length(pos.pubtype) < 1){
    pubtype.docx <- " "
  }
  pos.subject <- grep('Subject:', full_text)
  subject.docx <- full_text[pos.subject]
  if(length(pos.subject) < 1){
    subject.docx <- " "
  }
  pos.geo <- grep('Geographic:', full_text)
  geo.docx <- full_text[pos.geo]
  if(length(pos.geo) < 1){
    geo.docx <- " "
  }
  docx <- data.frame(title.docx, source.docx, pubtype.docx, date.docx, length.docx,
                     text.docx,language.docx,subject.docx,geo.docx, file.docx)
  
  docx %<>% 
    mutate(length.docx = gsub("([0-9]+).*$", "\\1", length.docx),
           length = as.numeric(str_trim(gsub("Length:", "", length.docx))),
           subject = str_trim(gsub("Subject:", "", subject.docx)),
           geo = str_trim(gsub("Geographic:", "", geo.docx)),
           text = str_squish(as.character(text.docx)),
           pub.type = as.character(str_trim(gsub("Publication-Type:", "", pubtype.docx))),
           title = as.character(title.docx),
           medium = as.character(source.docx),
           date.month = str_trim(str_extract(date.docx, "([a-zA-Z]*\\s)")),
           date.day = str_extract(date.docx, "(\\s[0-9]*),"),
           date.day = str_trim(str_replace(date.day, ",", "")),
           date.day = replace_na(date.day, replace = 1),
           date.year = str_trim(str_extract(date.docx, "(\\s[0-9]{4})")), 
           date.docx = paste(date.day, date.month, date.year, sep = "-"),
           pub.date = as.Date(parse_date_time(date.docx, "%e-%B-%Y", exact = TRUE))) %>%
    select(title,
           medium,
           pub.type,
           source.country = file.docx,
           date.month,
           date.day,
           date.year,
           pub.date,
           wordcount = length,
           text = text,
           topic = subject,
           geo = geo)
}

get_lexis_articles_docx <- function(filename){
  full_text <- read_docx(filename)
  file.docx <- str_extract(filename, "(?<=HumanitarianNewsData/)(.*?)(?=/)")
  title.docx <- as.character(full_text[1])
  source.docx <- as.character(full_text[2])
  date.docx <- full_text[3]
  pos.length <- grep('Length:', full_text)
  length.docx <- full_text[pos.length]
  pos2 = grep('Classification', full_text)
  pos1 = grep('Body', full_text)
  if(length(pos1) < 1){
    pos1 <- pos2-1
  }
  text.docx <- full_text[(pos1+1):(pos2-1)]
  text.docx <- paste(gsub("[\r\n]", "", text.docx),sep = " ", collapse = " ")
  pos.language <- grep('Language:', full_text)
  language.docx <- full_text[pos.language]
  if(length(pos.language) < 1){
    language.docx <- " "
  }
  pos.pubtype <- grep('Publication-Type:', full_text)
  pubtype.docx <- full_text[pos.pubtype]
  if(length(pos.pubtype) < 1){
    pubtype.docx <- " "
  }
  pos.subject <- grep('Subject:', full_text)
  subject.docx <- full_text[pos.subject]
  if(length(pos.subject) < 1){
    subject.docx <- " "
  }
  pos.geo <- grep('Geographic:', full_text)
  geo.docx <- full_text[pos.geo]
  if(length(pos.geo) < 1){
    geo.docx <- " "
  }
  docx <- data.frame(title.docx, source.docx, pubtype.docx, date.docx, length.docx,
                     text.docx,language.docx,subject.docx,geo.docx, file.docx)
  
  docx %<>% 
    mutate(length.docx = gsub("([0-9]+).*$", "\\1", length.docx),
           length = as.numeric(str_trim(gsub("Length:", "", length.docx))),
           subject = str_trim(gsub("Subject:", "", subject.docx)),
           geo = str_trim(gsub("Geographic:", "", geo.docx)),
           text = str_squish(as.character(text.docx)),
           pub.type = as.character(str_trim(gsub("Publication-Type:", "", pubtype.docx))),
           title = as.character(title.docx),
           medium = as.character(source.docx),
           date.month = str_trim(str_extract(date.docx, "([a-zA-Z]*\\s)")),
           date.day = str_extract(date.docx, "(\\s[0-9]*),"),
           date.day = str_trim(str_replace(date.day, ",", "")),
           date.day = replace_na(date.day, replace = 1),
           date.year = str_trim(str_extract(date.docx, "(\\s[0-9]{4})")), 
           date.docx = paste(date.day, date.month, date.year, sep = "-"),
           pub.date = as.Date(parse_date_time(date.docx, "%e-%B-%Y", exact = TRUE))) %>%
    select(title,
           medium,
           pub.type,
           source.country = file.docx,
           date.month,
           date.day,
           date.year,
           pub.date,
           wordcount = length,
           text = text,
           topic = subject,
           geo = geo)
}

get_lexis_articles_pdf <- function(filename,sourcelist){
  full_text <- pdftools::pdf_text(pdf = filename) %>%
    readr::read_lines()
  file.docx <- str_extract(filename, "(?<=HumanitarianNewsData/)(.*?)(?=/)")
  title.docx <- str_squish(as.character(full_text[1]))
  source.docx <- str_extract(paste0(full_text, collapse = " "),sourcelist)
  date.docx <- str_extract(paste0(full_text, collapse = " "),
                           "(January|February|March|April|May|June|July|August|September|October|November|December) [0-9]{1,2}, [0-9]{4}")
  pos.length <- grep('Length:', full_text)
  length.docx <- full_text[pos.length]
  pos2 = grep('Classification', full_text)
  pos1 = grep('Body', full_text)
  if(length(pos1) < 1){
    pos1 <- pos2-1
  }
  text.docx <- full_text[(pos1+1):(pos2-1)]
  text.docx <- paste(gsub("[\r\n]", "", text.docx),sep = " ", collapse = " ")
  pos.language <- grep('Language:', full_text)
  language.docx <- full_text[pos.language]
  if(length(pos.language) < 1){
    language.docx <- " "
  }
  pos.pubtype <- grep('Publication-Type:', full_text)
  pubtype.docx <- full_text[pos.pubtype]
  if(length(pos.pubtype) < 1){
    pubtype.docx <- " "
  }
  pos.subject <- grep('Subject:', full_text)
  subject.docx <- full_text[pos.subject]
  if(length(pos.subject) < 1){
    subject.docx <- " "
  }
  pos.geo <- grep('Geographic:', full_text)
  geo.docx <- full_text[pos.geo]
  if(length(pos.geo) < 1){
    geo.docx <- " "
  }
  docx <- data.frame(title.docx, source.docx, pubtype.docx, date.docx, length.docx,
                     text.docx,language.docx,subject.docx,geo.docx, file.docx)
  
  docx %<>% 
    mutate(length.docx = gsub("([0-9]+).*$", "\\1", length.docx),
           length = as.numeric(str_trim(gsub("Length:", "", length.docx))),
           subject = str_trim(gsub("Subject:", "", subject.docx)),
           geo = str_trim(gsub("Geographic:", "", geo.docx)),
           text = str_squish(as.character(text.docx)),
           pub.type = as.character(str_trim(gsub("Publication-Type:", "", pubtype.docx))),
           title = as.character(title.docx),
           medium = as.character(source.docx),
           date.month = str_trim(str_extract(date.docx, "([a-zA-Z]*\\s)")),
           date.day = str_extract(date.docx, "(\\s[0-9]*),"),
           date.day = str_trim(str_replace(date.day, ",", "")),
           date.day = replace_na(date.day, replace = 1),
           date.year = str_trim(str_extract(date.docx, "(\\s[0-9]{4})")), 
           date.docx = paste(date.day, date.month, date.year, sep = "-"),
           pub.date = as.Date(parse_date_time(date.docx, "%e-%B-%Y", exact = TRUE))) %>%
    select(title,
           medium,
           pub.type,
           source.country = file.docx,
           date.month,
           date.day,
           date.year,
           pub.date,
           wordcount = length,
           text = text,
           topic = subject,
           geo = geo)
}

get_lexis_articles_TW <- function(filename){
  full_text <- read_rtf(filename)
  file.docx <- str_extract(filename, "(?<=HumanitarianNewsData/)(.*?)(?=/)")
  title.docx <- as.character(full_text[1])
  source.docx <- as.character(full_text[2])
  date.docx <- full_text[3]
  pos.length <- grep('Length:', full_text)
  length.docx <- full_text[pos.length]
  pos2 = grep('Classification', full_text)
  if(length(pos2) == 0){
    pos2 = grep('End of Document', full_text)
  }
  pos1 = grep('Body', full_text)
  if(length(pos1) < 1){
    pos1 <- pos2-1
  }
  text.docx <- full_text[(pos1+1):(pos2-1)]
  text.docx <- paste(gsub("[\r\n]", "", text.docx),sep = " ", collapse = " ")
  pos.language <- grep('Language:', full_text)
  language.docx <- full_text[pos.language]
  if(length(pos.language) < 1){
    language.docx <- " "
  }
  pos.pubtype <- grep('Publication-Type:', full_text)
  pubtype.docx <- full_text[pos.pubtype]
  if(length(pos.pubtype) < 1){
    pubtype.docx <- " "
  }
  pos.subject <- grep('Subject:', full_text)
  subject.docx <- full_text[pos.subject]
  if(length(pos.subject) < 1){
    subject.docx <- " "
  }
  pos.geo <- grep('Geographic:', full_text)
  geo.docx <- full_text[pos.geo]
  if(length(pos.geo) < 1){
    geo.docx <- " "
  }
  docx <- data.frame(title.docx, source.docx, pubtype.docx, date.docx, length.docx,
                     text.docx,language.docx,subject.docx,geo.docx, file.docx)
  
  docx %<>% 
    mutate(length.docx = gsub("([0-9]+).*$", "\\1", length.docx),
           length = as.numeric(str_trim(gsub("Length:", "", length.docx))),
           subject = str_trim(gsub("Subject:", "", subject.docx)),
           geo = str_trim(gsub("Geographic:", "", geo.docx)),
           text = str_squish(as.character(text.docx)),
           pub.type = as.character(str_trim(gsub("Publication-Type:", "", pubtype.docx))),
           title = as.character(title.docx),
           medium = as.character(source.docx),
           date.month = str_trim(str_extract(date.docx, "([a-zA-Z]*\\s)")),
           date.day = str_extract(date.docx, "(\\s[0-9]*),"),
           date.day = str_trim(str_replace(date.day, ",", "")),
           date.day = replace_na(date.day, replace = 1),
           date.year = str_trim(str_extract(date.docx, "(\\s[0-9]{4})")), 
           date.docx = paste(date.day, date.month, date.year, sep = "-"),
           pub.date = as.Date(parse_date_time(date.docx, "%e-%B-%Y", exact = TRUE))) %>%
    select(title,
           medium,
           pub.type,
           source.country = file.docx,
           date.month,
           date.day,
           date.year,
           pub.date,
           wordcount = length,
           text = text,
           topic = subject,
           geo = geo)
}

###### Data Countries from A to C ######
tic()
AE <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

AE$source.country <- "AE"
fwrite(AE,
       file = "/XXXXXXXXXX/AE.csv",
       quote = TRUE,
       sep = "|")
rm(AE)

tic()
AF <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

AF$source.country <- "AF"
fwrite(AF,
       file = "/XXXXXXXXXX/AF.csv",
       quote = TRUE,
       sep = "|")
rm(AF)

tic()
AM <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

AM$source.country <- "AM"
fwrite(AM,
       file = "/XXXXXXXXXX/AM.csv",
       quote = TRUE,
       sep = "|")
rm(AM)

tic()
AO <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

AO$source.country <- "AO"
fwrite(AO,
       file = "/XXXXXXXXXX/AO.csv",
       quote = TRUE,
       sep = "|")
rm(AO)

tic()
AU <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

AU$source.country <- "AU"
fwrite(AU,
       file = "/XXXXXXXXXX/AU.csv",
       quote = TRUE,
       sep = "|")
rm(AU)

tic()
BB <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

BB$source.country <- "BB"
fwrite(BB,
       file = "/XXXXXXXXXX/BB.csv",
       quote = TRUE,
       sep = "|")
rm(BB)

tic()
BD <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

BD$source.country <- "BD"
fwrite(BD,
       file = "/XXXXXXXXXX/BD.csv",
       quote = TRUE,
       sep = "|")
rm(BD)

tic()
BH <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

BH$source.country <- "BH"
fwrite(BH,
       file = "/XXXXXXXXXX/BH.csv",
       quote = TRUE,
       sep = "|")
rm(BH)

tic()
BW <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

BW$source.country <- "BW"
fwrite(BW,
       file = "/XXXXXXXXXX/BW.csv",
       quote = TRUE,
       sep = "|")
rm(BW)

tic()
CA <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

CA$source.country <- "CA"
fwrite(CA,
       file = "/XXXXXXXXXX/CA.csv",
       quote = TRUE,
       sep = "|")
rm(CA)

tic()
CN <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

CN$source.country <- "CN"
fwrite(CN,
       file = "/XXXXXXXXXX/CN.csv",
       quote = TRUE,
       sep = "|")
rm(CN)



###### Data Countries from D to H ######
tic()
DE <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

DE$source.country <- "DE"
fwrite(DE,
       file = "/XXXXXXXXXX/DE.csv",
       quote = TRUE,
       sep = "|")
rm(DE)

tic()
EG <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

EG$source.country <- "EG"
fwrite(EG,
       file = "/XXXXXXXXXX/EG.csv",
       quote = TRUE,
       sep = "|")
rm(EG)

tic()
EQ <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

EQ$source.country <- "EQ"
fwrite(EQ,
       file = "/XXXXXXXXXX/EQ.csv",
       quote = TRUE,
       sep = "|")
rm(EQ)

tic()
ES <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

ES$source.country <- "ES"
fwrite(ES,
       file = "/XXXXXXXXXX/ES.csv",
       quote = TRUE,
       sep = "|")
rm(ES)

tic()
ET <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

ET$source.country <- "ET"
fwrite(ET,
       file = "/XXXXXXXXXX/ET.csv",
       quote = TRUE,
       sep = "|")
rm(ET)

tic()
FR <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

FR$source.country <- "FR"
fwrite(FR,
       file = "/XXXXXXXXXX/FR.csv",
       quote = TRUE,
       sep = "|")
rm(FR)

tic()
GB <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

GB$source.country <- "GB"
fwrite(GB,
       file = "/XXXXXXXXXX/GB.csv",
       quote = TRUE,
       sep = "|")
rm(GB)

tic()
GH <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

GH$source.country <- "GH"
fwrite(GH,
       file = "/XXXXXXXXXX/GH.csv",
       quote = TRUE,
       sep = "|")
rm(GH)

tic()
GM <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

GM$source.country <- "GM"
fwrite(GM,
       file = "/XXXXXXXXXX/GM.csv",
       quote = TRUE,
       sep = "|")
rm(GM)

tic()
HK <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

HK$source.country <- "HK"
fwrite(HK,
       file = "/XXXXXXXXXX/HK.csv",
       quote = TRUE,
       sep = "|")
rm(HK)

###### Data Countries from I to K ######
tic()
ID <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

ID$source.country <- "ID"
fwrite(ID,
       file = "/XXXXXXXXXX/ID.csv",
       quote = TRUE,
       sep = "|")
rm(ID)

tic()
IE <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

IE$source.country <- "IE"
fwrite(IE,
       file = "/XXXXXXXXXX/IE.csv",
       quote = TRUE,
       sep = "|")
rm(IE)

tic()
IL <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

IL$source.country <- "IL"
fwrite(IL,
       file = "/XXXXXXXXXX/IL.csv",
       quote = TRUE,
       sep = "|")
rm(IL)

tic()
IN <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

IN$source.country <- "IN"
fwrite(IN,
       file = "/XXXXXXXXXX/IN.csv",
       quote = TRUE,
       sep = "|")
rm(IN)

tic()
IQ <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

IQ$source.country <- "IQ"
fwrite(IQ,
       file = "/XXXXXXXXXX/IQ.csv",
       quote = TRUE,
       sep = "|")
rm(IQ)

tic()
IR <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE, pattern = ".rtf") %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()

IR2 <- list.files(path = "/XXXXXXXXXX",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE, pattern = ".pdf")
sl <- "FARS News Agency|Iran Daily|Iranian Students News Agency|Iran News|Mehr News Agency|Press TV"
IR2 <- future_map2(.x = IR2,.y = sl,
                   .f = safely(get_lexis_articles_pdf)) %>%
  map("result") %>%
  bind_rows()
toc()

IR <- rbind(IR, IR2)

IR$source.country <- "IR"
fwrite(IR,
       file = "/XXXXXXXXXX/IR.csv",
       quote = TRUE,
       sep = "|")
rm(IR,IR2)

tic()
IT <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

IT$source.country <- "IT"
fwrite(IT,
       file = "/XXXXXXXXXX/IT.csv",
       quote = TRUE,
       sep = "|")
rm(IT)

tic()
JO <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

JO$source.country <- "JO"
fwrite(JO,
       file = "/XXXXXXXXXX/JO.csv",
       quote = TRUE,
       sep = "|")
rm(JO)

tic()
JP <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

JP$source.country <- "JP"
fwrite(JP,
       file = "/XXXXXXXXXX/JP.csv",
       quote = TRUE,
       sep = "|")
rm(JP)

tic()
KE <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

KE$source.country <- "KE"
fwrite(KE,
       file = "/XXXXXXXXXX/KE.csv",
       quote = TRUE,
       sep = "|")
rm(KE)

tic()
KG <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

KG$source.country <- "KG"
fwrite(KG,
       file = "/XXXXXXXXXX/KG.csv",
       quote = TRUE,
       sep = "|")
rm(KG)

tic()
KR <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

KR$source.country <- "KR"
fwrite(KR,
       file = "/XXXXXXXXXX/KR.csv",
       quote = TRUE,
       sep = "|")
rm(KR)

tic()
KW <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

KW$source.country <- "KW"
fwrite(KW,
       file = "/XXXXXXXXXX/KW.csv",
       quote = TRUE,
       sep = "|")
rm(KW)

###### Data Countries from L to O ######
tic()
LK <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

LK$source.country <- "LK"
fwrite(LK,
       file = "/XXXXXXXXXX/LK.csv",
       quote = TRUE,
       sep = "|")
rm(LK)


tic()
LR <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

LR$source.country <- "LR"
fwrite(LR,
       file = "/XXXXXXXXXX/LR.csv",
       quote = TRUE,
       sep = "|")
rm(LR)

tic()
LY <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

LY$source.country <- "LY"
fwrite(LY,
       file = "/XXXXXXXXXX/LY.csv",
       quote = TRUE,
       sep = "|")
rm(LY)

tic()
MM <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

MM$source.country <- "MM"
fwrite(MM,
       file = "/XXXXXXXXXX/MM.csv",
       quote = TRUE,
       sep = "|")
rm(MM)

tic()
MT <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

MT$source.country <- "MT"
fwrite(MT,
       file = "/XXXXXXXXXX/MT.csv",
       quote = TRUE,
       sep = "|")
rm(MT)

tic()
MW <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

MW$source.country <- "MW"
fwrite(MW,
       file = "/XXXXXXXXXX/MW.csv",
       quote = TRUE,
       sep = "|")
rm(MW)

tic()
MY <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

MY$source.country <- "MY"
fwrite(MY,
       file = "/XXXXXXXXXX/MY.csv",
       quote = TRUE,
       sep = "|")
rm(MY)

tic()
NB <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

NB$source.country <- "NB"
fwrite(NB,
       file = "/XXXXXXXXXX/NB.csv",
       quote = TRUE,
       sep = "|")
rm(NB)

tic()
NG <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

NG$source.country <- "NG"
fwrite(NG,
       file = "/XXXXXXXXXX/NG.csv",
       quote = TRUE,
       sep = "|")
rm(NG)

tic()
NP <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

NP$source.country <- "NP"
fwrite(NP,
       file = "/XXXXXXXXXX/NP.csv",
       quote = TRUE,
       sep = "|")
rm(NP)

tic()
NZ <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE, 
                 pattern = ".rf") %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()


NZ$source.country <- "NZ"
fwrite(NZ,
       file = "/XXXXXXXXXX/NZ.csv",
       quote = TRUE,
       sep = "|")
rm(NZ)

tic()
OM <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

OM$source.country <- "OM"
fwrite(OM,
       file = "/XXXXXXXXXX/OM.csv",
       quote = TRUE,
       sep = "|")
rm(OM)

###### Data Countries from P to S ######
PH <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

PH$source.country <- "PH"
fwrite(PH,
       file = "/XXXXXXXXXX/PH.csv",
       quote = TRUE,
       sep = "|")
rm(PH)

tic()
PK <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

PK$source.country <- "PK"
fwrite(PK,
       file = "/XXXXXXXXXX/PK.csv",
       quote = TRUE,
       sep = "|")
rm(PK)

tic()
PS <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

PS$source.country <- "PS"
fwrite(PS,
       file = "/XXXXXXXXXX/PS.csv",
       quote = TRUE,
       sep = "|")
rm(PS)

tic()
QA <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

QA$source.country <- "QA"
fwrite(QA,
       file = "/XXXXXXXXXX/QA.csv",
       quote = TRUE,
       sep = "|")
rm(QA)

tic()
RU <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

RU$source.country <- "RU"
fwrite(RU,
       file = "/XXXXXXXXXX/RU.csv",
       quote = TRUE,
       sep = "|")
rm(RU)

tic()
RW <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

RW$source.country <- "RW"
fwrite(RW,
       file = "/XXXXXXXXXX/RW.csv",
       quote = TRUE,
       sep = "|")
rm(RW)

tic()
SA <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

SA$source.country <- "SA"
fwrite(SA,
       file = "/XXXXXXXXXX/SA.csv",
       quote = TRUE,
       sep = "|")
rm(SA)

tic()
SG <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

SG$source.country <- "SG"
fwrite(SG,
       file = "/XXXXXXXXXX/SG.csv",
       quote = TRUE,
       sep = "|")
rm(SG)

tic()
SO <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

SO$source.country <- "SO"
fwrite(SO,
       file = "/XXXXXXXXXX/SO.csv",
       quote = TRUE,
       sep = "|")
rm(SO)

tic()
SS <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

SS$source.country <- "SS"
fwrite(SS,
       file = "/XXXXXXXXXX/SS.csv",
       quote = TRUE,
       sep = "|")
rm(SS)

tic()
SY <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

SY$source.country <- "SY"
fwrite(SY,
       file = "/XXXXXXXXXX/SY.csv",
       quote = TRUE,
       sep = "|")
rm(SY)

###### Data Countries from T to Z ######
TH <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

TH$source.country <- "TH"
fwrite(TH,
       file = "/XXXXXXXXXX/TH.csv",
       quote = TRUE,
       sep = "|")
rm(TH)

tic()
TM <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

TM$source.country <- "TM"
fwrite(TM,
       file = "/XXXXXXXXXX/TM.csv",
       quote = TRUE,
       sep = "|")
rm(TM)

tic()
TR <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

TR$source.country <- "TR"
fwrite(TR,
       file = "/XXXXXXXXXX/TR.csv",
       quote = TRUE,
       sep = "|")
rm(TR)

tic()
TW <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

TW$source.country <- "TW"
TW$pub.type <- "Newspaper"
fwrite(TW,
       file = "/XXXXXXXXXX/TW.csv",
       quote = TRUE,
       sep = "|")
rm(TW)

tic()
UA <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

UA$source.country <- "UA"
fwrite(UA,
       file = "/XXXXXXXXXX/UA.csv",
       quote = TRUE,
       sep = "|")
rm(UA)

tic()
UG <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

UG$source.country <- "UG"
fwrite(UG,
       file = "/XXXXXXXXXX/UG.csv",
       quote = TRUE,
       sep = "|")
rm(UG)



tic()
UZ <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

UZ$source.country <- "UZ"
fwrite(UZ,
       file = "/XXXXXXXXXX/UZ.csv",
       quote = TRUE,
       sep = "|")
rm(UZ)

tic()
YE <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

YE$source.country <- "YE"
fwrite(YE,
       file = "/XXXXXXXXXX/YE.csv",
       quote = TRUE,
       sep = "|")
rm(YE)

tic()
ZA <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

ZA$source.country <- "ZA"
fwrite(ZA,
       file = "/XXXXXXXXXX/ZA.csv",
       quote = TRUE,
       sep = "|")
rm(ZA)

tic()
ZM <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

ZM$source.country <- "ZM"
fwrite(ZM,
       file = "/XXXXXXXXXX/ZM.csv",
       quote = TRUE,
       sep = "|")
rm(ZM)

tic()
ZW <- list.files(path = "/XXXXXXXXXX",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

ZW$source.country <- "ZW"
fwrite(ZW,
       file = "/XXXXXXXXXX/ZW.csv",
       quote = TRUE,
       sep = "|")
rm(ZW)
###### Data US ######

tic()
US1 <- list.files(path = "/XXXXXXXXXX",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()

US2 <- list.files(path = "/XXXXXXXXXX",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()

US3 <- list.files(path = "/XXXXXXXXXX",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()

US4 <- list.files(path = "/XXXXXXXXXX",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()

US5 <- list.files(path = "/XXXXXXXXXX",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles),.progress = TRUE) %>%
  map("result") %>%
  bind_rows()

US6 <- list.files(path = "/XXXXXXXXXX",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles),.progress = TRUE) %>%
  map("result") %>%
  bind_rows()

US7 <- list.files(path = "/XXXXXXXXXX",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW),.progress = TRUE) %>%
  map("result") %>%
  bind_rows()

US8 <- list.files(path = "/XXXXXXXXXX",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles),.progress = TRUE) %>%
  map("result") %>%
  bind_rows()

US9 <- list.files(path = "/XXXXXXXXXX",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles),.progress = TRUE) %>%
  map("result") %>%
  bind_rows()

toc()

US <- rbind(US1, US2, US3, US4, US5, US6, US7, US8, US9)

US$source.country <- "US"
fwrite(US,
       file = "/XXXXXXXXXX/US.csv",
       quote = TRUE,
       sep = "|")


###### Merge all files Nexis ######

df_nexis <- list.files("/XXXXXXXXXX",
                       full.names = TRUE) %>%
  map(function(x){
    temp <- fread(x) %>%
      tibble()}) %>%
  bind_rows()

# Export data about medium for processing
df_nexis %>%
  group_by(source.country, medium) %>%
  count(medium) %>%
  fwrite("list_nexis_medium_merged.csv")

# Import medium data, merge, and recode medium name
df_sources_cleaning <- read_excel("list_nexis_medium_merged_processeed.xlsx")
df_nexis %<>%
  full_join(df_sources_cleaning, by = c("medium", "source.country")) %>%
  filter(is.keep == TRUE) %>%
  mutate(source.country = source.country2,
         medium = new.medium) %>%
  select(-new.medium,
         -source.country2,
         -is.keep)

t <- df_nexis %>%
  group_by(source.country, medium) %>%
  count(medium)

df <- df_nexis %>%
  mutate(web_url = " ",
         data_source = "NEXIS",
         id = paste0("/XXXXXXXXXX",seq(1,length(df_nexis$title)))) %>%
  select(title,
         publish_date = pub.date,
         source = medium,
         source.country,
         full_text = text,
         web_url,
         data_source,
         id)

fwrite(df, 
       "/XXXXXXXXXX/_FinalData_NEXIS.csv",
       quote = TRUE,
       sep = "|")