#' render function for paper of Japanese Journal of Psychology
#' 
#' @importFrom rmarkdown render
#' @importFrom rmarkdown pdf_document
#' @param Rmd_file file name of R Markdown file
#' @param Bib_file file name of Bib file
#' @export
render_jjp <- function(Rmd_file, Bib_file){
  jpa_cite(Rmd_file, Bib_file)
  tmp_rmd <- paste0("tmp_",Rmd_file)
  template_tex_file <- system.file("rmarkdown/templates/jpa_jjp/resources/jpa_jjp.tex",
                                   package = 'jpaRmd')
  format_pdf <- pdf_document(latex_engine = "xelatex",
                                        template = template_tex_file,
                                        keep_tex = TRUE,
                                        toc = TRUE,
                                        toc_depth = 3,
                                        highlight = 'tango')
  format_pdf$inherits <- "pdf_document"
  output_file <- strsplit(Rmd_file, ".Rmd")[[1]]
  render(tmp_rmd,format_pdf,output_file)
}

#' render function for paper of Japanese Journal of Behavioral and Cognitive Therapies
#' 
#' @importFrom rmarkdown render
#' @importFrom rmarkdown pdf_document
#' @param Rmd_file file name of R Markdown file
#' @param Bib_file file name of Bib file
#' @export
render_jjbct <- function(Rmd_file, Bib_file){
  jpa_cite(Rmd_file, Bib_file)
  tmp_rmd <- paste0("tmp_",Rmd_file)
  template_tex_file <- system.file("rmarkdown/templates/jjbct/resources/jjbct.tex",
                                   package = 'jpaRmd')
  format_pdf <- pdf_document(latex_engine = "xelatex",
                             template = template_tex_file,
                             keep_tex = TRUE,
                             toc = TRUE,
                             toc_depth = 3,
                             highlight = 'tango')
  format_pdf$inherits <- "pdf_document"
  output_file <- strsplit(Rmd_file, ".Rmd")[[1]]
  render(tmp_rmd,format_pdf,output_file)
}


#' R Markdown template for Reply to reviewers
#' 
#' @export
#' @importFrom rmarkdown pdf_document

reply <- function(){
  template_tex_file <- system.file("rmarkdown/templates/reply/resources/reply.tex",
                                   package = 'jpaRmd')
  format_pdf <- pdf_document(latex_engine = "xelatex",
                             template = template_tex_file,
                             keep_tex = TRUE,
                             toc = TRUE,
                             toc_depth = 3,
                             highlight = 'tango')
  format_pdf$inherits <- "pdf_document"
  format_pdf
}