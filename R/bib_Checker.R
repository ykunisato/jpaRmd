#' @title bib_Checker function
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract_all
#' @param Rmd_file file name of R Markdown file
#' @param Bib_file file name of Bib file
#' @examples
#' # bib_Checker(Rmd_file = "RmdFileName",Bib_file = "BibFileName")
#' @export
#' 
bib_Checker <- function(Rmd_file = "RmdFileName", Bib_file = "BibFileName") {
  bib.df <- bib_to_DF(Rmd_file, Bib_file)
  ## citation check
  ## get rmd file
  tmpfile <- readLines(Rmd_file, warn = F)
  citation <- c()
  for (i in 1:length(tmpfile)) {
    st <- tmpfile[i]
    checkFLG <- str_detect(st, pattern = "@")
    if (checkFLG) {
      item <- st %>% str_extract_all(pattern = "@[\\[a-zA-Z0-9-_\\.\\p{Hiragana}\\p{Katakana}\\p{Han}]*")
      citation <- c(citation, unlist(item))
    }
  }
  citation_tmp <- str_replace_all(citation, pattern = "@", replacement = "")
  corresp <- citation_tmp %in% bib.df$BIBTEXKEY
  if (length(citation[!corresp]) != 0) {
    print("The following citation is not in the Bib file")
    print(citation[!corresp])
  }
  ## NA check
  NR <- NROW(bib.df)
  for (i in 1:NR) {
    FLG <- str_detect(bib.df[i, ]$pBib, pattern = "NA")
    if (FLG) {
      print(paste0("There is missing information in the citation @", bib.df[i, ]$BIBTEXKEY, "."))
      word <- bib.df[i, ]$pBib %>%
        str_replace(pattern = "emph", replacement = "") %>%
        str_replace_all(pattern = "\\\\", replacement = "") %>%
        str_replace_all(pattern = "\\{", replacement = "") %>%
        str_replace_all(pattern = "\\}", replacement = "")
      print(word)
    }
  }
}

