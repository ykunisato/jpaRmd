# extractor ---------------------------------------------------------------

extract_values <- function(string) {
  content <- string %>%
    ### delete escape-sequence, \"
    stringr::str_replace_all(pattern = '\\\"', replacement = "") %>%
    ### delete curly-bracket
    stringr::str_replace_all(pattern = "\\{", replacement = "") %>%
    stringr::str_replace_all(pattern = "\\}", replacement = "") %>%
    str_trim()
  ### if the last character is , then delete
  for (i in 1:length(content)) {
    Ln <- stringr::str_length(content[i])
    lastChar <- stringr::str_sub(content[i], start = Ln, end = Ln)
    if (!is.na(lastChar) & lastChar == ",") {
      content[i] <- str_sub(content[i], start = 1, end = Ln - 1)
    }
  }
  return(content)
}



# name spliter ------------------------------------------------------------

name_spliter <- function(dat) {
  dat %>%
    stringr::str_split(pattern = " and ") %>%
    unlist() %>%
    data.frame(Names = .) %>%
    dplyr::mutate(authors_name_split = purrr::map(.x = Names, ~ humaniformat::format_reverse(.x))) %>%
    dplyr::mutate(
      first_name = purrr::map(authors_name_split, ~ humaniformat::first_name(.x)),
      middle_name = purrr::map(authors_name_split, ~ humaniformat::middle_name(.x)),
      last_name = purrr::map(authors_name_split, ~ humaniformat::last_name(.x)),
      initial_first = purrr::map(first_name, ~ str_sub(.x, start = 1, end = 1) %>% str_to_upper()),
      initial_middle = purrr::map(middle_name, ~ str_sub(.x, start = 1, end = 1) %>% str_to_upper()),
    ) %>%
    return()
}



# print Name --------------------------------------------------------------

### 著者名は，姓を先に書き，カンマ（，）をおき，ファースト・ネーム，ミドル・ネームのイニシャルの順で書く。
### イニシャルのあとにはピリオド（.）を付ける。もし同姓で，イニシャルも同じ著者があるときは，名も略さずに書く。
### 著者名の表記法は，原著者のそれに従う。

print_EName <- function(st) {
  st <- as.data.frame(st)
  st %>%
    dplyr::mutate(
      initial_first = purrr::map(initial_first, ~ paste0(.x, ".")),
      initial_middle = purrr::map(initial_middle, ~ paste0(.x, ".")),
      initial_name = purrr::map2(
        .x = initial_first, .y = initial_middle,
        ~ paste0(.x, if_else(.y == "NA.", "", .y))
      )
    ) %>%
    dplyr::mutate(
      pName = purrr::map2(
        last_name, initial_name,
        ~ paste0(.x, ",", .y)
      )
    ) -> tmp
  # もし同姓で，イニシャルも同じ著者があるときは，名も略さずに書く。
  pName.tmp <- tmp$pName %>% unlist()
  duplicated.name <- which(table(pName.tmp) > 1) %>% names()
  for (i in 1:NROW(tmp)) {
    if (str_detect(tmp$pName[i], pattern = duplicated.name) %>% sum()) {
      tmp$pName[i] <- paste0(tmp$last_name[i], ",", tmp$first_name[i])
    }
  }
  nameList <- tmp$pName %>% unlist()
  if (length(nameList) == 1) {
    ## 単著
    pName <- nameList
  } else {
    ## 共著（著者が8名以上）
    if (NROW(tmp) > 7) {
      ## 著者が8名以上の場合は，第1から第6著者まで書き，
      ## 途中の著者は“...”で省略表記し，最後の著者を書く。
      pName <- paste0(
        nameList[1:6] %>% str_flatten(collapse = ", "),
        "...",
        nameList[length(nameList)]
      )
    } else {
      # すべての著者を書き，最後の著者の前にカンマ（，）と＆をおく。
      # andと綴らぬこと。
      pName <- stringr::str_flatten(nameList[1:(length(nameList) - 1)], collapse = ", ")
      pName <- paste(pName, "&", nameList[length(nameList)])
    }
  }

  return(unlist(pName))
}

print_JName <- function(st) {
  st <- as.data.frame(st)
  pName <- paste(st[1, ]$last_name, st[1, ]$first_name)
  if (NROW(st) > 1) {
    if (NROW(st) < 8) {
      # ii ）共著（著者が7名以下）の場合には，各著者の間に中黒（・）を入れて結ぶ。
      for (i in 2:NROW(st)) {
        pName <- paste0(pName, "・", st[i, ]$last_name, st[i, ]$first_name)
      }
    } else {
      # iii）共著（著者が8名以上）の場合には，第1から第6著者まで書き，途中の著者は「…」で省略表記し，最後の著者を書く。
      for (i in 2:6) {
        pName <- paste0(pName, "・", st[i, ]$last_name, st[i, ]$first_name)
      }
      pName <- paste0(
        pName, "…",
        st[NROW(st), ]$last_name, st[NROW(st), ]$first_name
      )
    }
  }
  # iv ）政府・官公庁・研究機関・学協会組織・一般民間組織など団体名義の著作物は，正式の名称を略さずに書き，個人著者名の場合と同様に，アルファベット順に並べる。
  # v ）著者名がない文献の場合は，表題によってアルファベット順に入れる。
  return(pName)
}


# print Bib infromation ---------------------------------------------------

print_English_book <- function(df) {
  name.tmp <- df$pName
  title.tmp <- df$TITLE
  # i ）一般的な例  （著者名），（刊行年），（書籍名），（出版地：出版社）
  # ii ）新版：初版以外は必ず版数を明記しておく。版（edition）はed. と省略表記する。
  if (!is.na(df$EDITION)) {
    title.tmp <- paste0(title.tmp, "(", df$EDITION, "ed.)")
  }
  # iii）編集書：編集者（editor）はEd. と省略表記し，編集者が複数のときはEds. と省略表記する。
  if (!is.na(df$EDITOR)) {
    if (NROW(df$AUTHORs) == 1) {
      name.postfix <- "(Ed.)"
    } else {
      name.postfix <- "(Eds.)"
    }
    name.tmp <- paste0(name.tmp, name.postfix)
  }
  # v ）数巻にわたる書籍  （著者名），（刊行年），（書籍名）（Vols. 巻数），（出版地：出版社）
  if (!is.na(df$VOLUME)) {
    title.tmp <- paste0(title.tmp, "(Vols.", df$VOLUME, ")")
  }
  # vi ）数巻にわたる書籍の特定の1巻  # vii）翻訳書  # viii）再版については
  # Bibファイルの方で対応(タイトルに入れるなど。Google Scholar参照)

  pBib <- paste(df$pName, df$pYear, df$TITLE, ",", df$ADDRESS, ":", df$PUBLISHER)
  return(pBib)
}

print_Japanese_book <- function(df) {
  name.tmp <- df$pName
  title.tmp <- df$TITLE
  # iii）編集書・監修書
  if(!is.na(df$EDITOR)){
    name.tmp <- paste0(name.tmp,"(編)")
  }
  # v ）数巻にわたる書籍（主題を持つ叢書・集書等を含む）
  if (!is.na(df$VOLUME)) {
    title.tmp <- paste0(title.tmp, "(全", df$VOLUME, "巻)")
  }
  # ii ）新・改訂版 viii）再版　  # vi ）数巻にわたる書籍の特定の1巻　については
  # Bibファイルの方で対応(タイトルに入れるなど。Google Scholar参照)
  
  # vii）翻訳書
  if(!is.na(df$JTITLE)){
    print("やくほん")
    E.part = print_English_book(df)
    J.part = paste(df$GENCHOKANA,df$pYear,df$JTITLE,df$JPUBLISHER)
    #Jauthor か Jeditor
    pBib <- paste(E.part,"(",J.part,")")
  }else{
    pBib <- paste(df$pName, df$pYear, df$TITLE, df$PUBLISHER)
  }
  
  return(pBib)
}

print_article <- function(df) {
  return("論文はまだ")
}

print_incollection <- function(df) {
  return("部分はまだ")
}

print_others <- function(df) {
  return("その他はまだ")
}

print_inbook <- function(df) {
  return("inBookはまだ")
}
