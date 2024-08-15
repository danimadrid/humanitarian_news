library(pacman)
p_load(readtext,stringr,textreadr, tidyverse, magrittr,lubridate, stringi,striprtf, data.table, readxl)


####### Merge files from Factiva 1 #######
get_factiva_v1 <- function(filename){
  file_text <- read_lines(filename, skip_empty_rows = TRUE)
  file_text <- str_replace_all(string = file_text, pattern = "Document [[:alnum:]]{25}", replacement = "###")
  file_text <- paste0(file_text, collapse = " ")
  file_text <- strsplit(file_text, "###")
  df <- tibble(matrix(unlist(file_text), nrow=lengths(file_text), byrow=T))
  df <- df[1:(nrow(df)-1),]
  colnames(df) <- "text" # Name this new column "text"
  
  df %<>%
    tibble() %<>%
    mutate(title = str_extract(text, pattern = "(?<=)(.*?)(?= ([0-9]{2,3} words)|([0-9]{1,2},[0-9]{3} words) )"),
           text = str_remove(text, pattern = "(?<= )(.*?)(?= ([0-9]{2,3} words)|([0-9]{1,2},[0-9]{3} words) )"),
           text = str_remove(text, pattern = "([0-9]{2,3} words)|([0-9]{1,2},[0-9]{3} words)"),
           text = str_remove(text, pattern = "[0-9]{2}:[0-9]{2} GMT"),
           pub.date = str_extract(text,
                                  "[0-9]{1,2} (January|February|March|April|May|June|July|August|September|October|November|December) [0-9]{4}"),
           text = str_remove(text,
                             "[0-9]{1,2} (January|February|March|April|May|June|July|August|September|October|November|December) [0-9]{4}"),
           pub.date = as.Date(parse_date_time(pub.date, "%e %B %Y", exact = TRUE)),
           source = str_replace(filename, "[0-9]{2}.txt", ""),
           source = str_replace(source, paste0(eval(pathfiles), "/"), ""),
           text = str_squish(text),
           text = str_replace(text, "^.*?(?=Copyright|©|\\(c\\))", ""),
           country = str_extract(filename, "(?<=HumanitarianNewsData/)(.*?)(?=/)"),
           country = str_replace(country, "_Factiva", "")) %>%
    select(title,
           publish_date = pub.date,
           source,
           country,
           text)
}

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/AE_Factiva"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
AE <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/AT_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
AT <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/BG_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
BG <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/BY_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
BY <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/CA_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
CA <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/CZ_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
CZ <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/DE_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
DE <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/EE_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
EE <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/FR_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
FR <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/GB_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
GB <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows() %>%
  mutate(source = "Reuters")

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/GR_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
GR <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/HR_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
HR <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/IE_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
IE <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/IL_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
IL <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/IN_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
IN <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/JO_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
JO <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/KG_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
KG <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/KR_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
KR <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/KZ_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
KZ <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/LT_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
LT <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/LV_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
LV <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/NP_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
NP <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/QA_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
QA <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows() %>%
  mutate(source = "AlJazeera")

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/RO_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
RO <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/RU_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
RU <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/SK_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
SK <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/SV_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
SV <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/TM_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
TM <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/UA_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
UA <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/US_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
US <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

pathfiles <- "/Volumes/LaCieOrange/HumanitarianNewsData/VN_Factiva/"
list_of_files <- list.files(pathfiles,full.names = TRUE,pattern = "[0-9].txt")
VN <- map(list_of_files, safely(get_factiva_v1)) %>%
  map("result") %>%
  bind_rows()

factiva1 <- rbind(AE,AT,BG,BY,CA,CZ,DE,EE,FR,GB,GR,HR,IE,IL,IN,JO,KG,KR,KZ,LT,LV,NP,QA,RO,RU,SK,SV,TM,UA,US,VN)
t <- factiva1 %>%
  group_by(country) %>%
  count(country)

fwrite(factiva1, "/Volumes/LaCieOrange/HumanitarianNewsData/_Factiva/_factiva_merged1.csv",quote = TRUE, sep = "|")


####### Merge files from Factiva 2 #######
rm(list = ls())

filename <- list_of_files[358]

get_factiva_v2 <- function(filename){
  file_text <- read_lines(filename, skip_empty_rows = TRUE)
  file_text <- str_replace_all(string = file_text, pattern = "Document [[:alnum:]]{25}", replacement = "###")
  file_text <- paste0(file_text, collapse = " ")
  file_text <- strsplit(file_text, "###")
  df <- tibble(matrix(unlist(file_text), nrow=lengths(file_text), byrow=T))
  df <- df[1:(nrow(df)-1),]
  colnames(df) <- "text" # Name this new column "text"
  
  df %<>%
    mutate(title = str_squish(str_extract(text, "(?<=\\sHD\\s)(.*)(?=\\sWC\\s)")),
           source = str_squish(str_extract(text, "(?<=\\sSN\\s)(.*?)(?=\\sSC\\s)")),
           pub.date = str_squish(str_extract(text, "(?<=\\sPD\\s)(.*?)(?=\\sSN\\s)")),
           pub.date = str_squish(str_replace(pub.date, "ET [0-9][0-9]:[0-9][0-9]", "")),
           pub.date = as.Date(parse_date_time(pub.date, "%e %B %Y", exact = TRUE)),
           text = gsub("(SE\\s).*(LP\\s)",replacement = "", x = text),
           text = gsub("(HD\\s).*(LP\\s)",replacement = "", x = text),
           text = gsub("(^[0-9]{2,4}).*(LP\\s)",replacement = "", x = text),
           text = gsub(x = text, pattern = "(\\sNS\\s).*(\\sPUB\\s)", replacement = ""),
           text = gsub(x = text, pattern = "(\\sCO\\s).*(\\sPUB\\s)", replacement = ""),
           text = gsub(x = text, pattern = "(\\sIN\\s).*(\\sPUB\\s)", replacement = ""),
           text = gsub(x = text, pattern = "(\\sRE\\s).*(\\sPUB\\s)", replacement = ""),
           text = str_squish(text),
           country = str_extract(filename, "(?<=HumanitarianNewsData/_Factiva)(.*?)(?=.txt)"),
           country = str_extract(country, "[A-Z]{2}")) %>%
    select(title,
           publish_date = pub.date,
           source,
           country,
           text)
}

list_of_files <- list.files("/Volumes/LaCieOrange/HumanitarianNewsData/_Factiva", 
                            recursive = FALSE, 
                            full.names = TRUE, 
                            pattern = ".txt")
factiva2 <- map(list_of_files, safely(get_factiva_v2)) %>%
  map("result") %>%
  bind_rows()

fwrite(factiva2, "/Volumes/LaCieOrange/HumanitarianNewsData/_Factiva/_factiva_merged2.csv",quote = TRUE, sep = "|")

####### Merge files and export #######

factiva1 <- fread("/Volumes/LaCieOrange/HumanitarianNewsData/_Factiva/_factiva_merged1.csv")
factiva2 <- fread("/Volumes/LaCieOrange/HumanitarianNewsData/_Factiva/_factiva_merged2.csv")

df_factiva <- rbind(factiva1, factiva2) 

df <- df_factiva %>%
  mutate(web_url = " ",
         data_source = "FACTIVA",
         id = paste0("FACTIVA",seq(1,length(df_factiva$title)))) %>%
  select(title,
         publish_date,
         source,
         source.country = country,
         full_text = text,
         web_url,
         data_source,
         id)

# Export data about medium for processing
df %>%
  group_by(source.country, source) %>%
  count(source) %>%
  fwrite("list_factiva_medium_merged.csv")

df_sources_cleaning <- read_excel("list_factiva_medium_merged_processeed.xlsx")
df %<>%
  full_join(df_sources_cleaning, by = c("source", "source.country")) %>%
  filter(is.keep == TRUE) %>%
  mutate(source.country = new.country,
         source = new.source) %>%
  select(-new.source,
         -new.country,
         -is.keep)

fwrite(df, "/Volumes/LaCieOrange/HumanitarianNewsData/_FinalData_Factiva.csv", quote = TRUE, sep = "|")
