rm(list = ls())
library(tidyverse)
setwd("develop/")
source("functions.R")





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
  )



## Filtering to only actually cited
refKey <- bib.df$BIBTEXKEY %>% paste0("@", .)
refFLG <- vector(length = length(refKey))
for (i in 1:NROW(refAll)) {
  refFLG <- refFLG | refAll[i, ]$refs %>% str_detect(pattern = refKey)
}
#bib.df <- bib.df[refFLG, ]


# 引用型式を出力する(事実上のスタイルファイル） -------------------------------------------------

for (i in 1:NROW(bib.df)) {
  tmp <- bib.df[i, ]
  # AUTHORが日本人か，翻訳などJTITLEフィールドがある場合
  langFLG = (stringi::stri_enc_isascii(tmp$AUTHOR) && is.na(tmp$JTITLE))
  tmp$pYear <- paste0("(", tmp$YEAR, ").")
  if (langFLG) {
    tmp$pName <- print_EName(tmp$AUTHORs)
  } else {
    tmp$pName <- print_JName(tmp$AUTHORs)
  }
  pBib <- case_when(
    langFLG==TRUE && tmp$CATEGORY == "BOOK" ~ print_English_book(tmp),
    langFLG==FALSE && tmp$CATEGORY == "BOOK" ~ print_Japanese_book(tmp),
    langFLG==TRUE && tmp$CATEGORY == "ARTICLE" ~ print_English_article(tmp),
    langFLG==FALSE && tmp$CATEGORY == "ARTICLE" ~ print_Japanese_article(tmp),
    # iv ）編集書中の特定章
    tmp$CATEGORY == "INBOOK" ~ print_inbook(tmp),
    tmp$CATEGORY == "INCOLLECTION" ~ print_incollection(tmp)
  )
  print(pBib)
}
