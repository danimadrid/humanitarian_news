library(pacman)
p_load(stringr,textreadr,dplyr,magrittr,tidyverse,lubridate,furrr,data.table,tictoc, pdftools,readxl)
plan(multisession(workers = 10))

###### Part A - Nexis Data ######
list_countries <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/", 
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
AE <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/AE_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

AE$source.country <- "AE"
fwrite(AE,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/AE.csv",
       quote = TRUE,
       sep = "|")
rm(AE)

tic()
AF <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/AF_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

AF$source.country <- "AF"
fwrite(AF,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/AF.csv",
       quote = TRUE,
       sep = "|")
rm(AF)

tic()
AM <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/AM_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

AM$source.country <- "AM"
fwrite(AM,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/AM.csv",
       quote = TRUE,
       sep = "|")
rm(AM)

tic()
AO <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/AO_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

AO$source.country <- "AO"
fwrite(AO,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/AO.csv",
       quote = TRUE,
       sep = "|")
rm(AO)

tic()
AU <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/AU_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

AU$source.country <- "AU"
fwrite(AU,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/AU.csv",
       quote = TRUE,
       sep = "|")
rm(AU)

tic()
BB <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/BB_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

BB$source.country <- "BB"
fwrite(BB,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/BB.csv",
       quote = TRUE,
       sep = "|")
rm(BB)

tic()
BD <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/BD_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

BD$source.country <- "BD"
fwrite(BD,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/BD.csv",
       quote = TRUE,
       sep = "|")
rm(BD)

tic()
BH <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/BH_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

BH$source.country <- "BH"
fwrite(BH,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/BH.csv",
       quote = TRUE,
       sep = "|")
rm(BH)

tic()
BW <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/BW_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

BW$source.country <- "BW"
fwrite(BW,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/BW.csv",
       quote = TRUE,
       sep = "|")
rm(BW)

tic()
CA <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/CA_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

CA$source.country <- "CA"
fwrite(CA,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/CA.csv",
       quote = TRUE,
       sep = "|")
rm(CA)

tic()
CN <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/CN_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

CN$source.country <- "CN"
fwrite(CN,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/CN.csv",
       quote = TRUE,
       sep = "|")
rm(CN)



###### Data Countries from D to H ######
tic()
DE <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/DE_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

DE$source.country <- "DE"
fwrite(DE,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/DE.csv",
       quote = TRUE,
       sep = "|")
rm(DE)

tic()
EG <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/EG_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

EG$source.country <- "EG"
fwrite(EG,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/EG.csv",
       quote = TRUE,
       sep = "|")
rm(EG)

tic()
EQ <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/EQ_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

EQ$source.country <- "EQ"
fwrite(EQ,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/EQ.csv",
       quote = TRUE,
       sep = "|")
rm(EQ)

tic()
ES <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/ES_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

ES$source.country <- "ES"
fwrite(ES,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/ES.csv",
       quote = TRUE,
       sep = "|")
rm(ES)

tic()
ET <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/ET_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

ET$source.country <- "ET"
fwrite(ET,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/ET.csv",
       quote = TRUE,
       sep = "|")
rm(ET)

tic()
FR <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/FR_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

FR$source.country <- "FR"
fwrite(FR,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/FR.csv",
       quote = TRUE,
       sep = "|")
rm(FR)

tic()
GB <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/GB_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

GB$source.country <- "GB"
fwrite(GB,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/GB.csv",
       quote = TRUE,
       sep = "|")
rm(GB)

tic()
GH <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/GH_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

GH$source.country <- "GH"
fwrite(GH,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/GH.csv",
       quote = TRUE,
       sep = "|")
rm(GH)

tic()
GM <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/GM_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

GM$source.country <- "GM"
fwrite(GM,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/GM.csv",
       quote = TRUE,
       sep = "|")
rm(GM)

tic()
HK <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/HK_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

HK$source.country <- "HK"
fwrite(HK,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/HK.csv",
       quote = TRUE,
       sep = "|")
rm(HK)

###### Data Countries from I to K ######
tic()
ID <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/ID_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

ID$source.country <- "ID"
fwrite(ID,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/ID.csv",
       quote = TRUE,
       sep = "|")
rm(ID)

tic()
IE <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/IE_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

IE$source.country <- "IE"
fwrite(IE,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/IE.csv",
       quote = TRUE,
       sep = "|")
rm(IE)

tic()
IL <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/IL_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

IL$source.country <- "IL"
fwrite(IL,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/IL.csv",
       quote = TRUE,
       sep = "|")
rm(IL)

tic()
IN <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/IN_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

IN$source.country <- "IN"
fwrite(IN,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/IN.csv",
       quote = TRUE,
       sep = "|")
rm(IN)

tic()
IQ <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/IQ_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

IQ$source.country <- "IQ"
fwrite(IQ,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/IQ.csv",
       quote = TRUE,
       sep = "|")
rm(IQ)

tic()
IR <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/IR_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE, pattern = ".rtf") %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()

IR2 <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/IR_Nexis/",
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
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/IR.csv",
       quote = TRUE,
       sep = "|")
rm(IR,IR2)

tic()
IT <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/IT_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

IT$source.country <- "IT"
fwrite(IT,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/IT.csv",
       quote = TRUE,
       sep = "|")
rm(IT)

tic()
JO <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/JO_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

JO$source.country <- "JO"
fwrite(JO,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/JO.csv",
       quote = TRUE,
       sep = "|")
rm(JO)

tic()
JP <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/JP_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

JP$source.country <- "JP"
fwrite(JP,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/JP.csv",
       quote = TRUE,
       sep = "|")
rm(JP)

tic()
KE <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/KE_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

KE$source.country <- "KE"
fwrite(KE,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/KE.csv",
       quote = TRUE,
       sep = "|")
rm(KE)

tic()
KG <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/KG_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

KG$source.country <- "KG"
fwrite(KG,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/KG.csv",
       quote = TRUE,
       sep = "|")
rm(KG)

tic()
KR <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/KR_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

KR$source.country <- "KR"
fwrite(KR,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/KR.csv",
       quote = TRUE,
       sep = "|")
rm(KR)

tic()
KW <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/KW_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

KW$source.country <- "KW"
fwrite(KW,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/KW.csv",
       quote = TRUE,
       sep = "|")
rm(KW)

###### Data Countries from L to O ######
tic()
LK <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/LK_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

LK$source.country <- "LK"
fwrite(LK,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/LK.csv",
       quote = TRUE,
       sep = "|")
rm(LK)


tic()
LR <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/LR_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

LR$source.country <- "LR"
fwrite(LR,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/LR.csv",
       quote = TRUE,
       sep = "|")
rm(LR)

tic()
LY <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/LY_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

LY$source.country <- "LY"
fwrite(LY,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/LY.csv",
       quote = TRUE,
       sep = "|")
rm(LY)

tic()
MM <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/MM_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

MM$source.country <- "MM"
fwrite(MM,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/MM.csv",
       quote = TRUE,
       sep = "|")
rm(MM)

tic()
MT <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/MT_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

MT$source.country <- "MT"
fwrite(MT,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/MT.csv",
       quote = TRUE,
       sep = "|")
rm(MT)

tic()
MW <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/MW_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

MW$source.country <- "MW"
fwrite(MW,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/MW.csv",
       quote = TRUE,
       sep = "|")
rm(MW)

tic()
MY <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/MY_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

MY$source.country <- "MY"
fwrite(MY,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/MY.csv",
       quote = TRUE,
       sep = "|")
rm(MY)

tic()
NB <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/NB_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

NB$source.country <- "NB"
fwrite(NB,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/NB.csv",
       quote = TRUE,
       sep = "|")
rm(NB)

tic()
NG <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/NG_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

NG$source.country <- "NG"
fwrite(NG,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/NG.csv",
       quote = TRUE,
       sep = "|")
rm(NG)

tic()
NP <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/NP_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

NP$source.country <- "NP"
fwrite(NP,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/NP.csv",
       quote = TRUE,
       sep = "|")
rm(NP)

tic()
NZ <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/NZ_Nexis/",
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
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/NZ.csv",
       quote = TRUE,
       sep = "|")
rm(NZ)

tic()
OM <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/OM_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

OM$source.country <- "OM"
fwrite(OM,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/OM.csv",
       quote = TRUE,
       sep = "|")
rm(OM)

###### Data Countries from P to S ######
PH <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/PH_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

PH$source.country <- "PH"
fwrite(PH,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/PH.csv",
       quote = TRUE,
       sep = "|")
rm(PH)

tic()
PK <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/PK_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

PK$source.country <- "PK"
fwrite(PK,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/PK.csv",
       quote = TRUE,
       sep = "|")
rm(PK)

tic()
PS <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/PS_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

PS$source.country <- "PS"
fwrite(PS,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/PS.csv",
       quote = TRUE,
       sep = "|")
rm(PS)

tic()
QA <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/QA_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

QA$source.country <- "QA"
fwrite(QA,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/QA.csv",
       quote = TRUE,
       sep = "|")
rm(QA)

tic()
RU <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/RU_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

RU$source.country <- "RU"
fwrite(RU,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/RU.csv",
       quote = TRUE,
       sep = "|")
rm(RU)

tic()
RW <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/RW_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

RW$source.country <- "RW"
fwrite(RW,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/RW.csv",
       quote = TRUE,
       sep = "|")
rm(RW)

tic()
SA <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/SA_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

SA$source.country <- "SA"
fwrite(SA,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/SA.csv",
       quote = TRUE,
       sep = "|")
rm(SA)

tic()
SG <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/SG_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW)) %>%
  map("result") %>%
  bind_rows()
toc()

SG$source.country <- "SG"
fwrite(SG,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/SG.csv",
       quote = TRUE,
       sep = "|")
rm(SG)

tic()
SO <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/SO_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

SO$source.country <- "SO"
fwrite(SO,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/SO.csv",
       quote = TRUE,
       sep = "|")
rm(SO)

tic()
SS <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/SS_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

SS$source.country <- "SS"
fwrite(SS,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/SS.csv",
       quote = TRUE,
       sep = "|")
rm(SS)

tic()
SY <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/SY_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

SY$source.country <- "SY"
fwrite(SY,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/SY.csv",
       quote = TRUE,
       sep = "|")
rm(SY)

###### Data Countries from T to Z ######
TH <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/TH_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

TH$source.country <- "TH"
fwrite(TH,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/TH.csv",
       quote = TRUE,
       sep = "|")
rm(TH)

tic()
TM <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/TM_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

TM$source.country <- "TM"
fwrite(TM,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/TM.csv",
       quote = TRUE,
       sep = "|")
rm(TM)

tic()
TR <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/TR_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

TR$source.country <- "TR"
fwrite(TR,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/TR.csv",
       quote = TRUE,
       sep = "|")
rm(TR)

tic()
TW <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/TW_Nexis/",
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
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/TW.csv",
       quote = TRUE,
       sep = "|")
rm(TW)

tic()
UA <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/UA_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

UA$source.country <- "UA"
fwrite(UA,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/UA.csv",
       quote = TRUE,
       sep = "|")
rm(UA)

tic()
UG <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/UG_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

UG$source.country <- "UG"
fwrite(UG,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/UG.csv",
       quote = TRUE,
       sep = "|")
rm(UG)



tic()
UZ <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/UZ_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

UZ$source.country <- "UZ"
fwrite(UZ,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/UZ.csv",
       quote = TRUE,
       sep = "|")
rm(UZ)

tic()
YE <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/YE_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

YE$source.country <- "YE"
fwrite(YE,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/YE.csv",
       quote = TRUE,
       sep = "|")
rm(YE)

tic()
ZA <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/ZA_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

ZA$source.country <- "ZA"
fwrite(ZA,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/ZA.csv",
       quote = TRUE,
       sep = "|")
rm(ZA)

tic()
ZM <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/ZM_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

ZM$source.country <- "ZM"
fwrite(ZM,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/ZM.csv",
       quote = TRUE,
       sep = "|")
rm(ZM)

tic()
ZW <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/ZW_Nexis/",
                 recursive = TRUE, 
                 full.names = TRUE,
                 include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()
toc()

ZW$source.country <- "ZW"
fwrite(ZW,
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/ZW.csv",
       quote = TRUE,
       sep = "|")
rm(ZW)
###### Data US ######

tic()
US1 <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/US_Nexis/ABCNews/",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()

US2 <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/US_Nexis/AtlanticCBS/",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()

US3 <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/US_Nexis/CNN/",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()

US4 <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/US_Nexis/MSNBC/",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles)) %>%
  map("result") %>%
  bind_rows()

US5 <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/US_Nexis/Slate+/",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles),.progress = TRUE) %>%
  map("result") %>%
  bind_rows()

US6 <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/US_Nexis/UPI/",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles),.progress = TRUE) %>%
  map("result") %>%
  bind_rows()

US7 <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/US_Nexis/AP/",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles_TW),.progress = TRUE) %>%
  map("result") %>%
  bind_rows()

US8 <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/US_Nexis/Multiple/",
                  recursive = TRUE, 
                  full.names = TRUE,
                  include.dirs = FALSE) %>%
  future_map(safely(get_lexis_articles),.progress = TRUE) %>%
  map("result") %>%
  bind_rows()

US9 <- list.files(path = "/Volumes/LaCieOrange/HumanitarianNewsData/US_Nexis/VOA/",
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
       file = "/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis/US.csv",
       quote = TRUE,
       sep = "|")


###### Merge all files Nexis ######

df_nexis <- list.files("/Volumes/LaCieOrange/HumanitarianNewsData/_Nexis",
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
         id = paste0("NEXIS",seq(1,length(df_nexis$title)))) %>%
  select(title,
         publish_date = pub.date,
         source = medium,
         source.country,
         full_text = text,
         web_url,
         data_source,
         id)

fwrite(df, 
       "/Volumes/LaCieOrange/HumanitarianNewsData/_FinalData_NEXIS.csv",
       quote = TRUE,
       sep = "|")