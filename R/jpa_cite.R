rm(list = ls())
library(tidyverse)
setwd("/Users/napier/Dropbox/Git/jpaRmd/develop")
tmp <- readLines("skeleton.Rmd", warn = F) %>% as_tibble()
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
  str_trim() %>%
  print()
## Delete commented out records
for (i in 1:length(bib)) {
  if (stringr::str_sub(bib[[i]], 1, 1) == "%") {
    bib[[i]] <- ""
  }
}
## Delete record which has no containts
bib <- bib[-which(sapply(bib, function(x) x == ""))]

## Lies which don't have the `key` of records(if the line has the `key`,
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
## prepared function
text_between_brackets <- function(string) {
  min <- min(gregexpr(pattern = "\\{|\\\"", string)[[1]])
  max <- max(gregexpr(pattern = "\\}|\\\"", string)[[1]])
  content <- substring(string, min + 1, max - 1)
  return(content)
}

## data list
ls <- mapply(
  function(x, y) {
    return(bib[x:y])
  },
  x = from,
  y = to - 1,
  SIMPLIFY = T
)

## get reference KEY and fields,categories
keys <- lapply(
  ls,
  function(x) {
    str_extract(x[1], "(?<=\\{)[^,]+")
  }
)
fields <- lapply(ls, function(x) {
  str_extract(x[1], "(?<=@)[^\\{]+")
})
fields <- lapply(fields, toupper)

categories <- lapply(
  ls,
  function(x) {
    str_extract(x, "[[:alnum:]_-]+")
  }
)

values <- lapply(
  ls,
  function(x) {
    str_extract(x, "(?<==).*")
  }
)

values <- lapply(
  values,
  function(x) {
    sapply(x, text_between_brackets, simplify = TRUE, USE.NAMES = FALSE)
  }
)

values <- lapply(values, trimws)

items <- mapply(cbind, categories, values, SIMPLIFY = FALSE)
items <- lapply(
  items,
  function(x) {
    x <- cbind(toupper(x[, 1]), x[, 2])
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
dat <- bind_rows(c(list(empty), items))
dat <- as_tibble(dat)
dat$BIBTEXKEY <- unlist(keys)
dat
