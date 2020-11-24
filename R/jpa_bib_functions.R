#' Name spliter function
#' @importFrom magrittr %>%
#' @importFrom stringr str_split
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom humaniformat format_reverse
#' @importFrom humaniformat first_name
#' @importFrom humaniformat middle_name
#' @importFrom humaniformat last_name
#' @importFrom stringr str_sub
#' @importFrom stringr str_to_upper
#' @param dat elements of data.frame contains NAME
#' @export
name_spliter <- function(dat) {
  dat %>%
    str_split(pattern = " and ") %>%
    unlist() %>%
    data.frame(Names = .) %>%
    mutate(authors_name_split = map(.x = Names, ~ format_reverse(.x))) %>%
    mutate(
      first_name = map(authors_name_split, ~ first_name(.x)),
      middle_name = map(authors_name_split, ~ middle_name(.x)),
      last_name = map(authors_name_split, ~ last_name(.x)),
      initial_first = map(first_name, ~ str_sub(.x, start = 1, end = 1) %>% str_to_upper()),
      initial_middle = map(middle_name, ~ str_sub(.x, start = 1, end = 1) %>% str_to_upper()),
    ) %>%
    return()
}

#' Print name function(English)
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom stringr str_flatten
#' @param st Strings of name
#' @export
print_EName <- function(st) {
  st <- as.data.frame(st)
  initial_first <- NULL
  initial_middle <- NULL
  initial_name <- NULL
  st %>%
    mutate(
      initial_first = map(initial_first, ~ paste0(.x, ".")),
      initial_middle = map(initial_middle, ~ paste0(.x, ".")),
      initial_name = map2(
        .x = initial_first, .y = initial_middle,
        ~ paste0(.x, if_else(.y == "NA.", "", .y))
      )
    ) %>%
    mutate(
      pName = map2(
        last_name, initial_name,
        ~ paste0(.x, ",", .y)
      )
    ) -> tmp
  # If authors have same name, don't abbreviate first name.
  pName.tmp <- tmp$pName %>% unlist()
  duplicated.name <- which(table(pName.tmp) > 1) %>% names()
  for (i in 1:NROW(tmp)) {
    if (str_detect(tmp$pName[i], pattern = duplicated.name) %>% sum()) {
      tmp$pName[i] <- paste0(tmp$last_name[i], ",", tmp$first_name[i])
    }
  }
  nameList <- tmp$pName %>% unlist()
  if (length(nameList) == 1) {
    ## single author
    pName <- nameList
  } else {
    ## co-authors(over 8)
    if (NROW(tmp) > 7) {
      ## if number of co-authors is over 8, write down first to 6th author's name and
      ## add "..." and last author's name.
      pName <- paste0(
        nameList[1:6] %>% str_flatten(collapse = ", "),
        "...",
        nameList[length(nameList)]
      )
    } else {
      # wirte down all author's name and add "," befor last author's name.
      # use & not and
      pName <- str_flatten(nameList[1:(length(nameList) - 1)], collapse = ", ")
      binder <- paste0("\\", "&")
      pName <- paste(pName, binder, nameList[length(nameList)])
    }
  }
  return(unlist(pName))
}

#' Print name function(Jpanese)
#' @param st Strings of Japanese name
#' @export
print_JName <- function(st) {
  st <- as.data.frame(st)
  pName <- paste(st[1, ]$last_name, st[1, ]$first_name)
  if (NROW(st) > 1) {
    if (NROW(st) < 8) {
      # ii) if number of co-author is under 7, write down all author's name and add "・"
      for (i in 2:NROW(st)) {
        pName <- paste0(pName, "・", st[i, ]$last_name, st[i, ]$first_name)
      }
    } else {
      # iii) if number of co-author is over 8，write down first to 6th author's name and
      ## add "..." and last author's name.
      for (i in 2:6) {
        pName <- paste0(pName, "・", st[i, ]$last_name, st[i, ]$first_name)
      }
      pName <- paste0(
        pName, "…",
        st[NROW(st), ]$last_name, st[NROW(st), ]$first_name
      )
    }
  }
  # iv ) The books and articles in the name of a group, such as government, government offices,
  # research institutions, academic associations, and general private organizations,
  # the official name should be written without abbreviating it,
  # and they should be arranged in the same alphabetical order as for the names of individual authors.
  # v ) If there are no authors, list them in alphabetical order according to their titles.
  return(pName)
}

#' Print bib info function(English book)
#' @param df Strings of Bib info
#' @export
print_English_book <- function(df) {
  name.tmp <- print_EName(df$AUTHORs)
  title.tmp <- paste0("\\emph{", df$TITLE, "}.")
  # i ) General examples (author), (year of publication), (book title), (place of publication: publisher)
  # ii) New editions: Always indicate the number of editions except for the first edition.
  # Editions should be abbreviated to ed.
  if (!is.na(df$EDITION)) {
    title.tmp <- paste0(title.tmp, "(", df$EDITION, "ed.)")
  }
  # iii)In the case of multiple editors, it should be abbreviated to Eds.
  if (!is.na(df$EDITOR)) {
    if (NROW(df$AUTHORs) == 1) {
      name.postfix <- "(Ed.)"
    } else {
      name.postfix <- "(Eds.)"
    }
    name.tmp <- paste0(name.tmp, name.postfix)
  }
  # v) Books in several volumes (author), (year of publication), (book title), (Vols),
  # (place of publication: publisher)
  if (!is.na(df$VOLUME)) {
    title.tmp <- paste0(title.tmp, "(Vols.", df$VOLUME, ")")
  }
  # vi)One specific volume of a book spanning several volumes,
  # vii) Translations, and  viii) reprints are handled by the Bib files
  # (e.g., put it in the title; see Google Scholar)
  pBib <- paste(name.tmp, df$pYear, title.tmp, ",", df$ADDRESS, ":", df$PUBLISHER)
  return(pBib)
}

#' Print bib info function(Japanese book)
#' @param df Strings of Bib info
#' @export
print_Japanese_book <- function(df) {
  name.tmp <- print_JName(df$AUTHORs)
  title.tmp <- df$TITLE
  # iii）Editorial and Supervisory Book
  if (!is.na(df$EDITOR)) {
    name.tmp <- paste0(name.tmp, "(編)")
  }
  # v ）Books in several volumes (including thematic series, collections, etc.)
  if (!is.na(df$VOLUME)) {
    title.tmp <- paste0(title.tmp, "(全", df$VOLUME, "巻)")
  }
  # ii) New edition, iii) reprints, and vi)  the book in several volumes
  # are handled by the Bib files (e.g., put it in the title; see Google Scholar)
  # vii）翻訳書
  if (!is.na(df$JTITLE)) {
    E.part <- print_English_book(df)
    ## 監訳
    if (!is.na(df$JKANYAKU)) {
      Jname <- print_JName(df$JKANYAKU)
      Jname <- paste0(Jname, "(監訳)")
    } else {
      Jname <- print_JName(df$JAUTHORs)
      Jname <- paste0(Jname, "(訳)")
    }
    J.part <- paste(df$GENCHOKANA, Jname, df$pYear, df$JTITLE, df$JPUBLISHER)
    pBib <- paste(E.part, "(", J.part, ")")
  } else {
    pBib <- paste(df$pName, df$pYear, df$TITLE, df$PUBLISHER)
  }

  return(pBib)
}

#' Print bib info function(English article)
#' @param df Strings of Bib info
#' @export
print_English_article <- function(df) {
  # (author's name), (year of publication), (title), (journal title), (number of copies), (page citations)
  TITLE.tmp <- paste0(df$TITLE,".")
  JOURNAL.tmp <- paste0("\\emph{",df$JOURNAL,"},")
  if (!is.na(df$NUMBER)) {
    Vol_and_Num.tmp <- paste0(df$VOLUME, "(", df$NUMBER, "),")
  } else {
    Vol_and_Num.tmp <- paste0(df$VOLUME, ",")
  }
  PAGES.tmp <- paste0(df$PAGES, ".")
  pBib <- paste(df$pName, df$pYear, TITLE.tmp, JOURNAL.tmp, Vol_and_Num.tmp, PAGES.tmp)
  return(pBib)
}

#' Print bib info function(Jaopanese article)
#' @param df Strings of Bib info
#' @export
print_Japanese_article <- function(df) {
  # (Author's name), (Year of publication), (Title), (Title), (Number of copies), (Citation page)
  JOURNAL.tmp <- paste0(df$JOURNAL, ",")
  if (!is.na(df$NUMBER)) {
    Vol_and_Num.tmp <- paste0("\\emph{", df$VOLUME, "}", "(", df$NUMBER, "),")
  } else {
    Vol_and_Num.tmp <- paste0("\\emph{", df$VOLUME, "},")
  }
  PAGES.tmp <- paste0(df$PAGES, ".")
  pBib <- paste(df$pName, df$pYear, df$TITLE, JOURNAL.tmp, Vol_and_Num.tmp, PAGES.tmp)
  return(pBib)
}

#' Print bib info function(in collection)
#' @param df Strings of Bib info
#' @export
print_incollection <- function(df) {
  return("incollectionはまだ")
}

#' Print bib info function(other)
#' @param df Strings of Bib info
#' @export
print_others <- function(df) {
  return("その他はまだ")
}

#' Print bib info function(in book)
#' @param df Strings of Bib info
#' @export
print_inbook <- function(df) {
  return("inBookはまだ")
}
