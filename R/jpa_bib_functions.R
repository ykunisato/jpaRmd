#' Extractor function
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom stringr str_length
#' @importFrom stringr str_sub
#' @export
extract_values <- function(string) {
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
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom stringr str_flatten
#' @export
print_EName <- function(st) {
  st <- as.data.frame(st)
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
      pName <- paste(pName, "&", nameList[length(nameList)])
    }
  }
  return(unlist(pName))
}

#' Print name function(Jpanese)
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
#' @export
print_English_book <- function(df) {
  name.tmp <- df$pName
  title.tmp <- paste0("{\\emph ",df$TITLE,"}")
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
  pBib <- paste(df$pName, df$pYear, df$TITLE, ",", df$ADDRESS, ":", df$PUBLISHER)
  return(pBib)
}

#' Print bib info function(Japanese book)
#' @export
print_Japanese_book <- function(df) {
  name.tmp <- df$pName
  title.tmp <- df$TITLE
  # iii）Editorial and Supervisory Book
  if(!is.na(df$EDITOR)){
    name.tmp <- paste0(name.tmp,"(編)")
  }
  # v ）Books in several volumes (including thematic series, collections, etc.)
  if (!is.na(df$VOLUME)) {
    title.tmp <- paste0(title.tmp, "(全", df$VOLUME, "巻)")
  }
  # ii) New edition, iii) reprints, and vi)  the book in several volumes 
  # are handled by the Bib files (e.g., put it in the title; see Google Scholar)
  # vii）翻訳書
  if(!is.na(df$JTITLE)){
    E.part = print_English_book(df)
    J.part = paste(df$GENCHOKANA,df$pYear,df$JTITLE,df$JPUBLISHER)
    #Jauthor か Jeditor
    pBib <- paste0(E.part,"(",J.part,")")
  }else{
    pBib <- paste0(df$pName, df$pYear, df$TITLE, df$PUBLISHER)
  }
  
  return(pBib)
}

#' Print bib info function(English article)
#' @export
print_English_article <- function(df) {
  # (author's name), (year of publication), (title), (journal title), (number of copies), (page citations)
  TITLE.tmp <-  title.tmp <- paste0("{\\emph ",df$TITLE,"},")
  JOURNAL.tmp <- paste0(df$JOURNAL,",")
  if(!is.na(df$NUMBER)){
    Vol_and_Num.tmp <- paste0(df$VOLUME,"(",df$NUMBER,"),")
  }else{
    Vol_and_Num.tmp <- paste0(df$VOLUME,",")
  }
  PAGES.tmp <- paste0(df$PAGES,".")
  pBib <- paste(df$pName,df$pYear,TITLE.tmp,JOURNAL.tmp,Vol_and_Num.tmp,PAGES.tmp)
  return(pBib)
}

#' Print bib info function(Jaopanese article)
#' @export
print_Japanese_article <- function(df) {
  # (Author's name), (Year of publication), (Title), (Title), (Number of copies), (Citation page)
  JOURNAL.tmp <- paste0(df$JOURNAL,",")
  if(!is.na(df$NUMBER)){
    Vol_and_Num.tmp <- paste0("{\\emph ",df$VOLUME,"}","(",df$NUMBER,"),")
  }else{
    Vol_and_Num.tmp <- paste0("{\\emph ",df$VOLUME,"},")
  }  
  PAGES.tmp <- paste0(df$PAGES,".")
  pBib <- paste(df$pName,df$pYear,df$TITLE,JOURNAL.tmp,Vol_and_Num.tmp,PAGES.tmp)
  return(pBib)
}

#' Print bib info function(in collection)
#' @export
print_incollection <- function(df) {
  return("incollectionはまだ")
}

#' Print bib info function(other)
#' @export
print_others <- function(df) {
  return("その他はまだ")
}

#' Print bib info function(in book)
#' @export
print_inbook <- function(df) {
  return("inBookはまだ")
}