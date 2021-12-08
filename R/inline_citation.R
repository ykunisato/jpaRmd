#' @title inLine Citation
#' @importFrom rlang .data
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_locate
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_sub
#' @importFrom dplyr left_join
#' @param st A line of text
#' @param bib.df Bib.df object created by bib_to_DF function
#' @return replace statement
#' @examples
#' # bib_to_DF(Rmd_file = "RmdFileName",Bib_file = "BibFileName")
#' @export
#'
#'
inLineCitation <- function(st, bib.df) {
  item <- st %>% str_extract(pattern = "@[\\[a-zA-Z0-9-_\\.\\p{Hiragana}\\p{Katakana}\\p{Han}]*")
  ### citation cheker
  checksum <- bib.df$BIBTEXKEY %in% str_replace(item,pattern = "@",replacement = "") %>% sum
  if(checksum == 0 ){
    stop(paste("Citation key",item," does not exsist on your bib file."))
  }
  
  loc <- st %>% str_locate(item)
  loc <- loc[1] - 1
  tp <- FALSE
  if (loc > 0) {
    tp <- str_sub(st, loc, loc) %>% str_detect(pattern = "\\[")
  }
  tmp.df <- data.frame()
  if (tp) {
    ### citation on the end of line
    ### retake citation
    item <- st %>% str_extract(pattern = "\\[@.*?\\]")
    ##### citaton data frame
    tmp.df <- item %>%
      str_extract_all(pattern = "@[a-zA-Z0-9-_\\.\\p{Hiragana}\\p{Katakana}\\p{Han}]*", simplify = T) %>%
      t() %>%
      as.data.frame() %>%
      mutate(KEY = str_replace(.data$V1, pattern = "@", replacement = "")) %>%
      ### join with bib.df
      left_join(bib.df, by = c("KEY" = "BIBTEXKEY")) %>%
      ### get the citation name
      select(.data$V1, KEY, .data$citeName1, .data$citeName2, .data$ListYear, .data$count,.data$langFLG,.data$JYEAR) %>%
      mutate(ListYear = str_extract(.data$ListYear, "[a-z0-9]{4,5}")) %>%
      mutate(citeName = if_else(.data$count > 0, .data$citeName2, .data$citeName1)) %>% 
      mutate(citation = if_else(.data$langFLG!="Tr",paste0(.data$citeName, ",\\ ", .data$ListYear),
                                paste0(.data$citeName, "\\ ", .data$JYEAR)))
    KEY <- tmp.df$KEY
    word <- tmp.df$citation %>% paste0(collapse = "; ")
    word <- paste0("(", word, ")")
    ### reform for regular expression
    item <- str_replace(item, pattern = "\\[", replacement = "\\\\[") %>%
      str_replace(pattern = "\\]", replacement = "\\\\]")
  } else {
    ### citation in the line
    KEY <- str_replace(item, pattern = "@", replacement = "")
    ref.df <- bib.df[bib.df$BIBTEXKEY == KEY, ] %>%
      mutate(ListYear = str_sub(.data$ListYear, 1, str_length(.data$ListYear) - 1))
    ### translated book's printed Year
    if(ref.df$langFLG == "Tr"){
      ref.df$ListYear <- paste0(" ",ref.df$JYEAR,")")
    }
    ###
    if (bib.df[bib.df$BIBTEXKEY == KEY, ]$count == 0) {
      word <- paste0(ref.df$citeName1, ref.df$ListYear)
    } else {
      # more
      word <- paste0(ref.df$citeName2, ref.df$ListYear)
    }
  }


  return(list(item = item, word = word, key = KEY))
}
