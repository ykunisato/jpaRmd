#' @title Add old content of manuscript to crrection table
#' @param old_text old content of manuscript
#' @export
old_m <- function(old_text) {
  if (exists("old_manuscript") == FALSE) {
    old_manuscript <- NULL
  }
  old_manuscript <- c(old_manuscript, old_text)
  assign("old_manuscript", old_manuscript, envir = globalenv())
}

#' Add new content of manuscript to crrection table
#' @param new_text new content of manuscript
#' @export
new_m <- function(new_text) {
  if (exists("new_manuscript") == FALSE) {
    new_manuscript <- NULL
  }
  new_manuscript <- c(new_manuscript, new_text)
  assign("new_manuscript", new_manuscript, envir = globalenv())
}


#' @title Make crrection table
#' @param old_manuscript vector of old content of manuscript
#' @param new_manuscript vector of new content of manuscript
#' @importFrom knitr kable
#' @importFrom magrittr %>%
#' @importFrom kableExtra kable_styling
#' @export
make_correction_table <- function(old_manuscript, new_manuscript) {
  correction_data <- data.frame(old_manuscript, new_manuscript)
  correction_table <- kable(correction_data,
    format = "latex",
    longtable = T,
    booktabs = T,
    linesep = c("\\addlinespace[16pt]"),
    col.names = c("\u65e7\u8ad6\u6587", "\u8a02\u6b63\u8ad6\u6587")
  ) %>%
    kable_styling(latex_options = c("repeat_header"), full_width = T)
  return(correction_table)
}
