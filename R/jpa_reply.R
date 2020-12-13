#' Add old content of manuscript to crrection table
#' @param old_text old content of manuscript
#' @export

old_m <- function(old_text){
  if(exists("old_manuscript")==FALSE){
    old_manuscript <- NULL
  }
  old_manuscript = c(old_manuscript,old_text)
  assign( "old_manuscript", old_manuscript, envir = globalenv() )
}

#' Add new content of manuscript to crrection table
#' @param new_text new content of manuscript
#' @export

new_m <- function(new_text){
  if(exists("new_manuscript")==FALSE){
    new_manuscript <- NULL
  }
  new_manuscript = c(new_manuscript,new_text)
  assign( "new_manuscript", new_manuscript, envir = globalenv() )
}
