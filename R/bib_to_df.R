#' @title bib_to_DF
#' @importFrom rlang .data
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
#' @param list_ampersand combinle the last author with & or not in Bibliography
#' @param cite_ampersand combinle the last author with & or not in in-line citation
#' @param underline Whether to underline emphasis or not. if FALSE, emph as Italic.
#' @return Prepare for citation database
#' @examples
#' # bib_to_DF(Rmd_file = "RmdFileName",Bib_file = "BibFileName")
#' @export
bib_to_DF <- function(Rmd_file, Bib_file, list_ampersand = T, cite_ampersand = F, underline = F) {
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
    mutate(refs = str_extract(.data$value, "\\@.*")) %>%
    # Omit commented-out LINE
    mutate(refs = ifelse(str_detect(.data$value, "<!--.*-->"), NA, refs)) %>%
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
      ### except only closing bracket
      if (bib[[i]] != "}") {
        bib[[flg]] <- paste(bib[[flg]], bib[[i]])
      }
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

  items <- mapply(
    function(x, y) {
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
    JYEAR = character(0L),
    JAUTHOR = character(0L),
    JKANYAKU = character(0L),
    TRANSAUTHOR = character(0L),
    GENCHOKANA = character(0L),
    TRANSWORK = character(0L),
    TRANSINFO = character(0L),
    DOI = character(0L),
    INFO = character(0L),
    stringsAsFactors = FALSE
  )

  bib.df <- bind_rows(c(list(empty), items)) %>%
    as_tibble() %>%
    rowid_to_column("ID") %>%
    group_by(.data$ID)
  bib.df$BIBTEXKEY <- unlist(keys)

  bib.df <- bib.df %>%
    ## Split name into First,Middle,Last Name
    mutate(
      AUTHORs = map(.data$AUTHOR, ~ name_spliter(.x)),
      EDITORs = map(.data$EDITOR, ~ name_spliter(.x)),
      JAUTHORs = map(.data$JAUTHOR, ~ name_spliter(.x)),
      JKANYAKUs = map(.data$JKANYAKU, ~ name_spliter(.x)),
      GENCHOKANAs = map(.data$GENCHOKANA, ~ name_spliter(.x)),
      TRANSAUTHORs = map(.data$TRANSAUTHOR, ~ name_spliter(.x))
    )

  ## Filtering to only actually cited
  refKey <- paste0("@", bib.df$BIBTEXKEY)
  refFLG <- vector(length = length(refKey))
  for (i in 1:NROW(refAll)) {
    refFLG <- refFLG | refAll[i, ]$refs %>% str_detect(pattern = refKey)
  }

  if (length(refFLG) != 0) {
    bib.df <- bib.df[refFLG, ]

    bib.df <- bib.df %>%
      ## In the case which the same author has some papers in the same year, assign an alphabet
      ### sorting Order; in JPA, the sorting follows the reading order of Japanese-YOMI or English-AUTHOR
      mutate(sortRecord = if_else(is.na(.data$YOMI), .data$AUTHOR, .data$YOMI)) %>%
      ## cut curly brackets of the organization-name
      mutate(sortRecord = str_replace(sortRecord, pattern = "\\{", replacement = "")) %>%
      ## str(YAER) is character, make Numeric one
      mutate(YEARn = as.numeric(YEAR)) %>%
      ## sort by Author and Year
      arrange(.data$sortRecord, YEARn) %>%
      ## group by Author and Year
      group_by(.data$sortRecord, YEARn) %>%
      ## count the papers with group
      mutate(n = n()) %>%
      mutate(num = row_number()) %>%
      ## Add a string if it needs
      mutate(addletter = if_else(n > 1, letters[.data$num], "")) %>%
      ### Retrun
      mutate(YEAR = paste0(.data$YEAR, .data$addletter)) %>%
      ### Language type check
      mutate(langFLG = if_else(str_detect(paste0(AUTHOR, TITLE, JOURNAL), pattern = "\\p{Hiragana}|\\p{Katakana}|\\p{Han}"), "J",
        if_else(!is.na(JTITLE), "Tr", "E")
      )) %>%
      ### delete unnecessary variables
      ungroup() %>%
      select(-c(.data$sortRecord, .data$YEARn, n, .data$num, .data$addletter))

    ## List and Citation Name
    bib.df <- bib.df %>%
      rowwise() %>%
      ################################## bib list
      mutate(
        ListName = if_else(langFLG == "J", print_JName(.data$AUTHORs), print_EName(.data$AUTHORs, ampersand = list_ampersand)),
      ) %>%
      ## Duplicate check
      group_by(ListName, YEAR) %>%
      mutate(dplFLG = n(), add_one = 1, postfix_id = cumsum(add_one)) %>%
      select(-(add_one)) %>%
      mutate(ListYear = ifelse(dplFLG > 1, paste0("(", .data$YEAR, letters[postfix_id], ")."), paste0("(", .data$YEAR, ")."))) %>%
      ungroup() %>%
      ## prepare for confusion
      mutate(confusionCase = 0) %>%
      # make items for List
      group_by(.data$ID) %>%
      nest() %>%
      mutate(
        pBib = purrr::map2(.x = .data$data, .y = underline, .f = ~ pBibMaker(.x, .y)) %>% unlist(),
        prefix = purrr::map(.x = .data$data, .f = ~ prefixMaker(.x))
      ) %>%
      ################################### inline citation
      # make items for citating
      mutate(cite = purrr::map2(.x = .data$data, .y = cite_ampersand, .f = ~ citationMaker(.x, .y))) %>%
      unnest(cols = c(.data$data, .data$cite)) %>%
      ## check for confusion Case
      group_by(citeCheckFLG) %>%
      mutate(confusionCase = n()) %>%
      ungroup()

    #### Case for risk of confusion due to the citation of a reference
    ###   by a different author with the same surname and the same year
    if (NROW(bib.df[bib.df$confusionCase > 1, ]) != 0) {
      bib.df[bib.df$confusionCase > 1, ] <-
        bib.df[bib.df$confusionCase > 1, ] %>%
        select(-citeName1, -citeName2, -citeCheckFLG) %>%
        group_by(ID) %>%
        nest() %>%
        mutate(cite = purrr::map2(.x = .data$data, .y = cite_ampersand, .f = ~ citationMaker(.x, .y))) %>%
        unnest(cols = c(.data$data, .data$cite)) %>%
        ungroup()
    }

    return(bib.df)
  } else {
    bib.df <- NULL
  }
}
