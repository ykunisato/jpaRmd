#' @title Extractor function
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom stringr str_length
#' @importFrom stringr str_sub
#' @param string string which contains \{ or \"
#' @export
value_extractor <- function(string) {
  content <- string %>%
    ### delete escape-sequence, \"
    str_replace_all(pattern = '\\\"', replacement = "") %>%
    ### delete curly-bracket
    str_replace(pattern = "\\{", replacement = "") %>%
    str_replace(pattern = "\\}", replacement = "") %>%
    str_trim()
  ### if the last character is , then delete
  for (i in 1:length(content)) {
    Ln <- str_length(content[i])
    lastChar <- str_sub(content[i], start = Ln, end = Ln)
    if (!is.na(lastChar) & lastChar == ",") {
      content[i] <- str_sub(content[i], start = 1, end = Ln - 1)
    }
  }
  return(content)
}

#' @title Add citation with JPA format
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr if_else
#' @importFrom dplyr n
#' @importFrom dplyr row_number
#' @importFrom dplyr left_join
#' @importFrom tidyr unnest
#' @importFrom tidyr nest
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
#' @importFrom stringr str_trim
#' @importFrom stringr str_sub
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_locate
#' @importFrom stringr str_extract_all
#' @importFrom stringi stri_enc_isascii
#' @importFrom stringi stri_escape_unicode
#' @importFrom stringi stri_unescape_unicode
#' @importFrom purrr map
#' @importFrom stats complete.cases na.omit
#' @param Rmd_file file name of R Markdown file
#' @param Bib_file file name of Bib file
#' @return Make reference list and add it to R Markdown file
#' @examples
#' # jpa_cite(Rmd_file = "RmdFileName",Bib_file = "BibFileName")
#' @export
jpa_cite <- function(Rmd_file, Bib_file, use.et.al.first = T) {
  bib.df <- bib_to_DF(Rmd_file, Bib_file, list_ampersand = T, cite_ampersand = T, underline = F)
  # Rewrite citation in the text. -------------------------------------------------------------------
  ## get original file
  tmpfile <- readLines(Rmd_file, warn = F)
  ## count how many times cited
  if (NROW(bib.df) != 0) {
    bib.df$count <- 0
    ## et al. notation from the beginning
    if (use.et.al.first = TRUE) {
      bib.df$count <-1
    }
  }
  ## open temporary file
  Ftmp <- file(paste0("tmp_", Rmd_file), "w")
  ## check the file in each line
  for (i in 1:length(tmpfile)) {
    st <- tmpfile[i]
    checkFLG <- str_detect(st, pattern = "@")
    ## Not-Check comment out line
    if (str_detect(st, pattern = "<!--.*-->")) {
      checkFLG <- FALSE
    }
    refFLG <- str_detect(st, pattern = "<insert_reference>")
    if (checkFLG) {
      # Replacement of main text
      while (str_detect(st, pattern = "@")) {
        replacement <- inLineCitation(st, bib.df)
        st <- str_replace(st, pattern = replacement$item, replacement = replacement$word)
        bib.df[bib.df$BIBTEXKEY %in% replacement$key, ]$count <- 1
      }
    }
    writeLines(st, Ftmp)
    ## output reference
    if (refFLG && NROW(bib.df) != 0) {
      writeLines("\n", Ftmp)
      for (i in 1:NROW(bib.df)) {
        writeLines(bib.df[i, ]$pBib, Ftmp)
        writeLines("\n", Ftmp)
      }
    }
  }
  close(Ftmp)
}


#' Add citation with JPR format
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr if_else
#' @importFrom dplyr n
#' @importFrom dplyr row_number
#' @importFrom dplyr left_join
#' @importFrom tidyr unnest
#' @importFrom tidyr nest
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
#' @importFrom stringr str_trim
#' @importFrom stringr str_sub
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_locate
#' @importFrom stringr str_extract_all
#' @importFrom stringi stri_enc_isascii
#' @importFrom stringi stri_escape_unicode
#' @importFrom stringi stri_unescape_unicode
#' @importFrom purrr map
#' @importFrom stats complete.cases na.omit
#' @param Rmd_file file name of R Markdown file
#' @param Bib_file file name of Bib file
#' @return Make reference list and add it to R Markdown file
#' @examples
#' # jpr_cite(Rmd_file = "RmdFileName",Bib_file = "BibFileName")
#' @export

# function developed
jpr_cite <- function(Rmd_file, Bib_file) {
  bib.df <- bib_to_DF(Rmd_file, Bib_file, list_ampersand = F, cite_ampersand = F, underline = T)
  # Output the citation type (substantively a Style file) -------------------------------------------------

  # Rewrite citation in the text. -------------------------------------------------------------------
  ## get original file
  tmpfile <- readLines(Rmd_file, warn = F)
  ## count how many times cited
  if (NROW(bib.df) != 0) {
    bib.df$count <- 0
  }
  ## open temporary file
  Ftmp <- file(paste0("tmp_author_", Rmd_file), "w")
  Ftmp2 <- file(paste0("tmp_", Rmd_file), "w")
  Ftmp3 <- file(paste0("tmp_abst_author_", Rmd_file), "w")
  Ftmp4 <- file(paste0("tmp_abst_", Rmd_file), "w")
  ## check the file in each line
  for (i in 1:length(tmpfile)) {
    st <- tmpfile[i]
    checkFLG <- str_detect(st, pattern = "@")
    ## Not-Check comment out line
    if (str_detect(st, pattern = "<!--.*-->")) {
      checkFLG <- FALSE
    }
    refFLG <- str_detect(st, pattern = "<insert_reference>")
    if (checkFLG) {
      # Replacement of main text
      while (str_detect(st, pattern = "@")) {
        replacement <- inLineCitation(st, bib.df)
        st <- str_replace(st, pattern = replacement$item, replacement = replacement$word)
        bib.df[bib.df$BIBTEXKEY %in% replacement$key, ]$count <- 1
      }
    }
    # FLG of author info
    if (i == 1) {
      authorInfoOn <- FALSE
    }
    if (str_detect(st, pattern = "author-info-start")) {
      authorInfoOn <- TRUE
    }
    if (str_detect(st, pattern = "author-info-end")) {
      authorInfoOn <- FALSE
    }
    # FLG of end of abstract
    if (i == 1) {
      abstEnd <- FALSE
    }
    if (str_detect(st, pattern = "abstract-end")) {
      abstEnd <- TRUE
    }

    # write paper with author info
    writeLines(st, Ftmp)
    ## include reference
    if (refFLG && NROW(bib.df) != 0) {
      writeLines("\n", Ftmp)
      for (i in 1:NROW(bib.df)) {
        writeLines(bib.df[i, ]$pBib, Ftmp)
        writeLines("\n", Ftmp)
      }
    }

    # write paper without author info
    if (authorInfoOn == FALSE) {
      writeLines(st, Ftmp2)
      ## include reference
      if (refFLG) {
        writeLines("\n", Ftmp2)
        for (i in 1:NROW(bib.df)) {
          writeLines(bib.df[i, ]$pBib, Ftmp2)
          writeLines("\n", Ftmp2)
        }
      }
    }

    # write abstract
    if (abstEnd == FALSE) {
      # write abstract with author info
      writeLines(st, Ftmp3)
      ## include reference
      if (refFLG) {
        writeLines("\n", Ftmp3)
        for (i in 1:NROW(bib.df)) {
          writeLines(bib.df[i, ]$pBib, Ftmp3)
          writeLines("\n", Ftmp3)
        }
      }
      # write abstract without author info
      if (authorInfoOn == FALSE) {
        writeLines(st, Ftmp4)
        ## include reference
        if (refFLG) {
          writeLines("\n", Ftmp)
          for (i in 1:NROW(bib.df)) {
            writeLines(bib.df[i, ]$pBib, Ftmp4)
            writeLines("\n", Ftmp4)
          }
        }
      }
    }
  }
  close(Ftmp)
  close(Ftmp2)
  close(Ftmp3)
  close(Ftmp4)
}
