#' R Markdown template for paper of the Japanese Journal of Psychology
#' @export
jpa_jjp <- function(){
  template_tex_file <- system.file("rmarkdown/templates/jpa_jjp/resources/jpa_jjp.tex",
                             package = 'jpaRmd')
  format_pdf <- rmarkdown::pdf_document(latex_engine = "xelatex",
                                        template = template_tex_file,
                                        keep_tex = TRUE,
                                        toc = TRUE,
                                        toc_depth = 3,
                                        highlight = 'tango')
  format_pdf$inherits <- "pdf_document"
  format_pdf
}

#' R Markdown template for paper of the Japanese Journal of Psychology ver2
#' @export
jpa_jjp2 <- function(){
  template_tex_file <- system.file("rmarkdown/templates/jpa_jjp2/resources/jpa_jjp2.tex",
                                   package = 'jpaRmd')
  format_pdf <- rmarkdown::pdf_document(latex_engine = "xelatex",
                                        template = template_tex_file,
                                        keep_tex = TRUE,
                                        toc = TRUE,
                                        toc_depth = 3,
                                        highlight = 'tango')
  format_pdf$inherits <- "pdf_document"
  format_pdf
}