#' Extractor function
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
    str_replace_all(pattern = "\\{", replacement = "") %>%
    str_replace_all(pattern = "\\}", replacement = "") %>%
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

#' Add citation function
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
#' @importFrom stringr str_trim
#' @importFrom stringr str_sub
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_upper
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

jpa_cite <- function(Rmd_file, Bib_file) {
  # check argument
  if (missing(Rmd_file)) {
    stop("Please set the name of RMarkdown file")
  }
  # check Bib file
  if (missing(Bib_file)) {
    stop("Please set the name of Bib file")
  }

  # reference pick-up
  refAll <- readLines(Rmd_file, warn = F) %>%
    as_tibble() %>%
    mutate(refs = str_extract(.$value, "\\@.*")) %>%
    na.omit()

  # readBib file as tibble
  bib <- readLines(Bib_file, warn = F) %>%
    str_trim()

  ## Delete commented out records
  for (i in 1:length(bib)) {
    if (str_sub(bib[[i]], 1, 1) == "%") {
      bib[[i]] <- ""
    }
  }
  ## Delete record which has no containts
  bib <- bib[-which(sapply(bib, function(x) x == ""))]

  ## Lines which don't have the `key` of records(if the line has the `key`,
  ## the line must have '=' or '@') are continuation of the previous line.
  for (i in 1:length(bib)) {
    ## check the key
    if (str_detect(bib[[i]], pattern = "=|@")) {
      flg <- i
    }
    ## paste to previous liens
    if (i != flg) {
      bib[[flg]] <- paste(bib[[flg]], bib[[i]])
    }
  }
  ## ommit the line which have no keys
  bib <- bib[which(str_detect(bib, pattern = "=|@"))]

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
      str_extract(x[1], "(?<=\\{)[^,]+")
    }
  )
  fields <- lapply(ls, function(x) {
    str_extract(x[1], "(?<=@)[^\\{]+") %>% str_to_upper()
  })

  categories <- lapply(
    ls,
    function(x) {
      str_extract(x, "[[:alnum:]_-]+") %>% str_to_upper()
    }
  )

  values <- lapply(
    ls,
    ## delete first record which has Key and Category
    function(x) {
      str_extract(x, "(?<==).*") %>%
        value_extractor() %>%
        str_trim()
    }
  )

  items <- mapply(cbind, categories, values, SIMPLIFY = FALSE)
  items <- lapply(
    items,
    function(x) {
      x <- cbind(str_to_upper(x[, 1]), x[, 2])
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

  ## Write down all possible records here
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
    YOMI = character(0L),
    JTITLE = character(0L),
    JAUTHOR = character(0L),
    JKANYAKU = character(0L),
    TRANSAUTHOR = character(0L),
    TRANSWORK = character(0L),
    TRANSINFO = character(0L),
    stringsAsFactors = FALSE
  )

  bib.df <- bind_rows(c(list(empty), items)) %>%
    as_tibble() %>%
    rowid_to_column("ID") %>%
    group_by(ID)
  bib.df$BIBTEXKEY <- unlist(keys)

  bib.df <- bib.df %>%
    ## Split name into First,Middle,Last Name
    mutate(
      AUTHORs = map(AUTHOR, ~ name_spliter(.x)),
      EDITORs = map(EDITOR, ~ name_spliter(.x)),
      JAUTHORs = map(JAUTHOR, ~ name_spliter(.x)),
      JKANYAKUs = map(JKANYAKU, ~ name_spliter(.x)),
      TRANSAUTHORs = map(TRANSAUTHOR, ~ name_spliter(.x))
    )

  ## Filtering to only actually cited
  refKey <- bib.df$BIBTEXKEY %>% paste0("@", .)
  refFLG <- vector(length = length(refKey))
  for (i in 1:NROW(refAll)) {
    refFLG <- refFLG | refAll[i, ]$refs %>% str_detect(pattern = refKey)
  }
  bib.df <- bib.df[refFLG, ]

  # Output the citation type (substantively a Style file) -------------------------------------------------

  ## Sort by NAME whether in Japanese or English
  bib.df <- bib.df %>%
    mutate(sortRecord = if_else(is.na(YOMI), AUTHOR, YOMI)) %>%
    ## In the case which the same author has some papers in the same year, assign an alphabet
    ## str(YAER) is character, make Numeric one
    mutate(YEARn = as.numeric(YEAR)) %>%
    ## sort by Author and Year
    arrange(sortRecord, YEARn) %>%
    ## group by Author and Year
    group_by(sortRecord, YEARn) %>%
    ## count the papers with group
    mutate(n = n()) %>%
    mutate(num = row_number()) %>%
    ## Add a string if it needs
    mutate(addletter = if_else(n > 1, letters[num], "")) %>%
    ### Retrun
    mutate(YEAR = paste0(YEAR, addletter)) %>%
    ### Language type check
    mutate(langFLG = !str_detect(paste0(AUTHOR, TITLE, JTITLE, JOURNAL), pattern = "\\p{Hiragana}|\\p{Katakana}|\\p{Han}")) %>%
    ### printed name and year in ref.list
    mutate(
      pName = if_else(langFLG, print_EName(AUTHORs), print_JName(AUTHORs)),
      pYear = paste0("(", YEAR, ").")
    ) %>%
    mutate(dplFLG = 0) %>%
    # make items for List
    group_by(ID) %>%
    nest() %>%
    mutate(
      pBib = purrr::map(.x = data, .f = ~ pBibMaker(.x)),
      prefix = purrr::map(.x = data, .f = ~ prefixMaker(.x))
    ) %>%
    # make items for citating
    mutate(cite.tmp = purrr::map(.x = data, .f = ~ citationMaker(.x))) %>%
    # Differnt Authors, but same family name,same year --for the case of confusion
    unnest(cols = c(data, pBib, prefix, cite.tmp)) %>%
    group_by(citeCheckFLG) %>%
    mutate(dplFLG = n()) %>%
    ungroup(citeCheckFLG) %>%
    select(-citeName1, -citeName2, -citeCheckFLG) %>%
    group_by(ID) %>%
    nest() %>%
    mutate(cite = purrr::map(.x = data, .f = ~ citationMaker(.x))) %>%
    unnest(cols = c(data, cite)) %>%
    select(-citeCheckFLG)

  # Rewrite citation in the text. -------------------------------------------------------------------
  ## get original file
  tmpfile <- readLines(Rmd_file, warn = F)
  ## count how many times cited
  bib.df$count <- 0
  ## open temporary file
  Ftmp <- file(paste0(Rmd_file, ".tmp"), "w")
  ## check the file in each line
  for (i in 1:length(tmpfile)) {
    st <- tmpfile[i]
    checkFLG <- str_detect(st, pattern = "@")
    refFLG <- str_detect(st,pattern="# \\u5f15\\u7528\\u6587\\u732e")
    if (checkFLG) {
      # Replacement
      while (str_detect(st, pattern = "@")) {
        replacement.item <- st %>% str_extract(pattern = "@[\\[a-zA-Z0-9-_\\.\\p{Hiragana}\\p{Katakana}\\p{Han}]*")
        loc <- st %>% str_locate(replacement.item)
        loc <- loc[1] - 1
        tp <- FALSE
        if (loc > 0) {
          tp <- str_sub(st, loc, loc) %>% str_detect(pattern = "\\[")
        }

        if (tp) {
          ### citation on the end of line
          ##### retake citation key
          replacement.item <- st %>% str_extract(pattern = "\\[.*?\\]")
          ##### citaton data frame
          replacement.df <- replacement.item %>%
            str_extract_all(pattern = "@[a-zA-Z0-9-_\\.\\p{Hiragana}\\p{Katakana}\\p{Han}]*", simplify = T) %>%
            t() %>%
            as.data.frame() %>%
            mutate(KEY = str_replace(V1, pattern = "@", replacement = "")) %>%
            ### join with bib.df
            left_join(bib.df, by = c("KEY" = "BIBTEXKEY")) %>%
            ### get the citation name
            select(V1, KEY, citeName1, citeName2, pYear, count) %>%
            mutate(pYear = str_extract(pYear, "[a-z0-9]{4,5}")) %>%
            mutate(citeName = if_else(count > 0, citeName2, citeName1)) %>%
            mutate(citation = paste0(citeName, ",", pYear))
          
          replacement.word <- replacement.df$citation %>% paste0(collapse = "; ")
          replacement.word <- paste0("(", replacement.word, ")")
          ### reform for regular expression
          replacement.item <- str_replace(replacement.item, pattern = "\\[", replacement = "\\\\[") %>%
            str_replace(pattern = "\\]", replacement = "\\\\]")
          ### replacement!!
          st <- str_replace(st, pattern = replacement.item, replacement = replacement.word)
          ### count up
          bib.df[bib.df$BIBTEXKEY %in% replacement.df$KEY, ]$count <- 1
          
        } else {
          
          ### citation in the line
          KEY <- str_replace(replacement.item, pattern = "@", replacement = "")
          ref.df <- bib.df[bib.df$BIBTEXKEY == KEY, ] %>%
            mutate(pYear = str_sub(pYear, 1, str_length(pYear) - 1))
          if (bib.df[bib.df$BIBTEXKEY == KEY, ]$count == 0) {
            # First time
            st <- str_replace(st, pattern = replacement.item, replacement = paste0(ref.df$citeName1, ref.df$pYear))
          } else {
            # more
            st <- str_replace(st, pattern = replacement.item, replacement = paste0(ref.df$citeName2, ref.df$pYear))
          }
          ### count up
          bib.df[bib.df$BIBTEXKEY == KEY, ]$count <- 1
        }
        
      }
    }
    writeLines(st, Ftmp)
    ## include reference
    if(refFLG){
      writeLines("\n",Ftmp)
      for(i in 1:NROW(bib.df)){
        writeLines(bib.df[i, ]$pBib,Ftmp)
      }  
    }
    
  }
  close(Ftmp)
}
