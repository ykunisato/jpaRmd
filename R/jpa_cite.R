rm(list = ls())
library(tidyverse)
setwd("develop/")


## help functions
extract_values <- function(string) {
  content <- string %>%
    ### delete escape-sequence, \"
    stringr::str_replace_all(pattern = '\\\"', replacement = "") %>%
    ### delete curly-bracket
    stringr::str_replace_all(pattern = "\\{", replacement = "") %>%
    stringr::str_replace_all(pattern = "\\}", replacement = "") %>%
    str_trim()
  ### if the last character is , then delete
  for (i in 1:length(content)) {
    Ln <- stringr::str_length(content[i])
    lastChar <- stringr::str_sub(content[i], start = Ln, end = Ln)
    if (!is.na(lastChar) & lastChar == ",") {
      content[i] <- str_sub(content[i], start = 1, end = Ln - 1)
    }
  }
  return(content)
}



name_spliter <- function(dat) {
  dat %>%
    stringr::str_split(pattern = " and ") %>%
    unlist() %>%
    data.frame(Names = .) %>%
    dplyr::mutate(authors_name_split = purrr::map(.x = Names, ~ humaniformat::format_reverse(.x))) %>%
    dplyr::mutate(
      first_name = purrr::map(authors_name_split, ~ humaniformat::first_name(.x)),
      middle_name = purrr::map(authors_name_split, ~ humaniformat::middle_name(.x)),
      last_name = purrr::map(authors_name_split, ~ humaniformat::last_name(.x)),
      initial_first = purrr::map(first_name, ~ str_sub(.x, start = 1, end = 1) %>% str_to_upper()),
      initial_middle = purrr::map(middle_name, ~ str_sub(.x, start = 1, end = 1) %>% str_to_upper()),
    ) %>%
    return()
}



# main --------------------------------------------------------------------

tmp <- readLines("testRMD2.Rmd", warn = F) %>% as_tibble()
# Bibfile name(from YMAL header)
bibfile <- tmp$value %>%
  stringr::str_extract(".*\\.bib") %>%
  as.vector() %>%
  na.omit() %>%
  stringr::str_replace(pattern = "bibliography:", "") %>%
  stringr::str_trim()

# reference pick-up
refAll <- tmp %>%
  dplyr::mutate(refs = stringr::str_extract(.$value, "\\@.*")) %>%
  na.omit()

# bibfile <- 'reference.bib'
bibfile <- "sample.bib"
# readBib file as tibble
bib <- readLines(bibfile, warn = F) %>%
  str_trim()

## Delete commented out records
for (i in 1:length(bib)) {
  if (stringr::str_sub(bib[[i]], 1, 1) == "%") {
    bib[[i]] <- ""
  }
}
## Delete record which has no containts
bib <- bib[-which(sapply(bib, function(x) x == ""))]

## Lines which don't have the `key` of records(if the line has the `key`,
## the line must have '=' or '@') are continuation of the previous line.
for (i in 1:length(bib)) {
  ## check the key
  if (stringr::str_detect(bib[[i]], pattern = "=|@")) {
    flg <- i
  }
  ## paste to previous liens
  if (i != flg) {
    bib[[flg]] <- paste(bib[[flg]], bib[[i]])
  }
}
## ommit the line which have no keys
bib <- bib[which(stringr::str_detect(bib, pattern = "=|@"))]

##### Thanks to bib2df
## LineID to read from
from <- which(str_extract(bib, "[:graph:]") == "@")
## LineID to read stop
to <- c(from[-1] - 1, length(bib))




## data list
ls <- mapply(
  function(x, y) {
    return(bib[x:y])
  },
  x = from,
  y = to,
  SIMPLIFY = T
)

## get reference KEY and fields,categories
keys <- lapply(
  ls,
  function(x) {
    stringr::str_extract(x[1], "(?<=\\{)[^,]+")
  }
)
fields <- lapply(ls, function(x) {
  stringr::str_extract(x[1], "(?<=@)[^\\{]+") %>% stringr::str_to_upper()
})

categories <- lapply(
  ls,
  function(x) {
    stringr::str_extract(x, "[[:alnum:]_-]+") %>% stringr::str_to_upper()
  }
)

values <- lapply(
  ls,
  ## delete first record which has Key and Category
  function(x) {
    stringr::str_extract(x, "(?<==).*") %>%
      extract_values() %>%
      stringr::str_trim()
  }
)

items <- mapply(cbind, categories, values, SIMPLIFY = FALSE)
items <- lapply(
  items,
  function(x) {
    x <- cbind(stringr::str_to_upper(x[, 1]), x[, 2])
  }
)

items <- lapply(
  items,
  function(x) {
    x[complete.cases(x), ]
  }
)

items <- mapply(function(x, y) {
  rbind(x, c("CATEGORY", y))
},
x = items, y = fields, SIMPLIFY = FALSE
)

items <- lapply(items, t)
items <- lapply(
  items,
  function(x) {
    colnames(x) <- x[1, ]
    x <- x[-1, ]
    return(x)
  }
)
items <- lapply(
  items,
  function(x) {
    x <- t(x)
    x <- data.frame(x, stringsAsFactors = FALSE)
    return(x)
  }
)

empty <- data.frame(
  CATEGORY = character(0L),
  BIBTEXKEY = character(0L),
  ADDRESS = character(0L),
  ANNOTE = character(0L),
  AUTHOR = character(0L),
  BOOKTITLE = character(0L),
  CHAPTER = character(0L),
  CROSSREF = character(0L),
  EDITION = character(0L),
  EDITOR = character(0L),
  HOWPUBLISHED = character(0L),
  INSTITUTION = character(0L),
  JOURNAL = character(0L),
  KEY = character(0L),
  MONTH = character(0L),
  NOTE = character(0L),
  NUMBER = character(0L),
  ORGANIZATION = character(0L),
  PAGES = character(0L),
  PUBLISHER = character(0L),
  SCHOOL = character(0L),
  SERIES = character(0L),
  TITLE = character(0L),
  TYPE = character(0L),
  VOLUME = character(0L),
  YEAR = character(0L),
  stringsAsFactors = FALSE
)

bib.df <- bind_rows(c(list(empty), items)) %>%
  as_tibble() %>%
  rowid_to_column("ID") %>%
  group_by(ID)
bib.df$BIBTEXKEY <- unlist(keys)

bib.df <- bib.df %>%
  ## Split name into First,Middle,Last Name
  dplyr::mutate(
    AUTHORs = purrr::map(AUTHOR, ~ name_spliter(.x)),
    EDITORs = purrr::map(EDITOR, ~ name_spliter(.x)),
    JAUTHORs = purrr::map(JAUTHOR, ~ name_spliter(.x))
  ) %>%
  ## detect Languate --- not Complete
  dplyr::mutate(
    langFLG = purrr::map_lgl(AUTHOR, ~ stringi::stri_enc_isascii(.x))
  )


## Filtering to only actually cited
refKey <- bib.df$BIBTEXKEY %>% paste0("@", .)
refFLG <- vector(length = length(refKey))
for (i in 1:NROW(refAll)) {
  refFLG <- refFLG | refAll[i, ]$refs %>% str_detect(pattern = refKey)
}
bib.df <- bib.df[refFLG, ]



# 引用型式を出力する(事実上のスタイルファイル） -------------------------------------------------

## 刊行年には（　）．を付ける。
bib.df %>% dplyr::mutate(pYEAR = purrr::map(.x = YEAR, ~ paste0("(", .x, ").")))
### 著者名は，姓を先に書き，カンマ（，）をおき，ファースト・ネーム，ミドル・ネームのイニシャルの順で書く。
### イニシャルのあとにはピリオド（.）を付ける。もし同姓で，イニシャルも同じ著者があるときは，名も略さずに書く。
### 著者名の表記法は，原著者のそれに従う。
bib.df$AUTHORs[[1]] %>% print_Name() %>% print

print_Name <- function(str){
  str %>% 
  dplyr::mutate(
    initial_first = purrr::map(initial_first, ~ paste0(.x, ".")),
    initial_middle = purrr::map(initial_middle, ~ paste0(.x, ".")),
    initial_name = purrr::map2(
      .x = initial_first, .y = initial_middle,
      ~ paste0(.x, if_else(.y == "NA.", "", .y))
    )
  ) %>%
  dplyr::mutate(
    pName = purrr::map2(last_name,initial_name,
                        ~paste0(.x,",",.y))
  ) -> tmp
  #もし同姓で，イニシャルも同じ著者があるときは，名も略さずに書く。
  pName.tmp <- tmp$pName %>%  unlist
  duplicated.name <- which(table(pName.tmp)>1) %>% names()
  for(i in 1:NROW(tmp)){
    if(str_detect(tmp$pName[i],pattern=duplicated.name) %>% sum){
      tmp$pName[i] <- paste0(tmp$last_name[i],",",tmp$first_name[i])
    }
  }
  nameList <- tmp$pName %>% unlist
  if(length(nameList)==1){
    ## 単著
    pName <- nameList
  }else{
    ## 共著（著者が8名以上）
    if(NROW(tmp)>7){
      ## 著者が8名以上の場合は，第1から第6著者まで書き，
      ## 途中の著者は“...”で省略表記し，最後の著者を書く。
      pName <- paste0(nameList[1:6] %>% str_flatten(collapse = ", "),
                      "...",
                      nameList[length(nameList)])
    }else{
      #すべての著者を書き，最後の著者の前にカンマ（，）と＆をおく。
      # andと綴らぬこと。
      pName <- stringr::str_flatten(nameList[1:(length(nameList)-1)],collapse=", ")
      pName <- paste(pName,"&",nameList[length(nameList)])
    }
  }
}

