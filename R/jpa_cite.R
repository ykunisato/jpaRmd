library(tidyverse)
setwd("/Users/napier/Dropbox/Git/jpaRmd/develop")
tmp <- readLines("skeleton.Rmd",warn = F) %>% as_tibble
# Bibfile name(from YMAL header)
bibfile <- tmp$value %>% str_extract(".*\\.bib") %>% 
  as.vector %>% na.omit %>% 
  str_replace(pattern="bibliography:","") %>% 
  str_trim()

# reference pick-up
refAll  <- tmp %>% dplyr::mutate(refs = str_extract(.$value,"\\@.*")) %>% na.omit()

# readBib file as tibble
bib <- readLines(bibfile,warn = F) %>% str_trim()
##### Thanks to bib2df
## LineID to read from
from <- which(str_extract(bib,"[:graph:]") == "@")
## LineID to read stop
to  <- c(from[-1] - 1, length(bib))

itemslist <- mapply(
  function(x, y) return(bib[x:y]),
  x = from,
  y = to - 1,
  SIMPLIFY = FALSE
)

keys <- lapply(itemslist,
               function(x) {
                 str_extract(x[1], "(?<=\\{)[^,]+")
               }
)
fields <- lapply(itemslist,
                 function(x) {
                   str_extract(x[1], "(?<=@)[^\\{]+")
                 }
)
fields <- lapply(fields, toupper)

categories <- lapply(itemslist,
                     function(x) {
                       str_extract(x, "[[:alnum:]_-]+")
                     }
)


values <- lapply(itemslist,
                 function(x) {
                   str_extract(x, "(?<==).*")
                 }
)

values <- lapply(values,
                 function(x) {
                   sapply(x, text_between_curly_brackets, simplify = TRUE, USE.NAMES = FALSE)
                 }
)

values <- lapply(values, trimws)

items <- mapply(cbind, categories, values, SIMPLIFY = FALSE)
items <- lapply(items,
                function(x) {
                  x <- cbind(toupper(x[, 1]), x[, 2])
                }
)
items <- lapply(items,
                function(x) {
                  x[complete.cases(x), ]
                }
)
items <- mapply(function(x, y) {
  rbind(x, c("CATEGORY", y))
},
x = items, y = fields, SIMPLIFY = FALSE)

items <- lapply(items, t)
items <- lapply(items,
                function(x) {
                  colnames(x) <- x[1, ]
                  x <- x[-1, ]
                  return(x)
                }
)
items <- lapply(items,
                function(x) {
                  x <- t(x)
                  x <- data.frame(x, stringsAsFactors = FALSE)
                  return(x)
                }
)
