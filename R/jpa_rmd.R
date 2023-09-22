#' @title render function for paper of Japanese Journal of Psychology
#' @importFrom rmarkdown render
#' @importFrom rmarkdown pdf_document
#' @param Rmd_file file name of R Markdown file
#' @param Bib_file file name of Bib file
#' @export
render_jjp <- function(Rmd_file, Bib_file, use.et.al.first = T) {
  jpa_cite(Rmd_file, Bib_file, use.et.al.first)
  tmp_rmd <- paste0("tmp_", Rmd_file)
  template_tex_file <- system.file("rmarkdown/templates/jpa_jjp/resources/jpa_jjp.tex",
    package = "jpaRmd"
  )
  base <- pdf_document(
    latex_engine = "xelatex",
    template = template_tex_file,
    keep_tex = TRUE,
    toc = TRUE,
    toc_depth = 3,
    highlight = "tango"
  )
  base$inherits <- "pdf_document"
  output_file <- strsplit(Rmd_file, ".Rmd")[[1]]
  render(tmp_rmd, base, output_file)
}

#' @title render function for paper of Japanese Journal of Behavioral and Cognitive Therapies
#' @importFrom rmarkdown render
#' @importFrom rmarkdown pdf_document
#' @param Rmd_file file name of R Markdown file
#' @param Bib_file file name of Bib file
#' @export
render_jjbct <- function(Rmd_file, Bib_file, use.et.al.first = T) {
  jpa_cite(Rmd_file, Bib_file, use.et.al.first)
  tmp_rmd <- paste0("tmp_", Rmd_file)
  template_tex_file <- system.file("rmarkdown/templates/jjbct/resources/jjbct.tex",
    package = "jpaRmd"
  )
  format_pdf <- pdf_document(
    latex_engine = "xelatex",
    template = template_tex_file,
    keep_tex = TRUE,
    toc = TRUE,
    toc_depth = 3,
    highlight = "tango"
  )
  format_pdf$inherits <- "rmarkdown_output_format"
  output_file <- strsplit(Rmd_file, ".Rmd")[[1]]
  render(tmp_rmd, format_pdf, output_file)
}

#' @title render function for paper of Japanese Psychological Review
#' @importFrom rmarkdown render
#' @importFrom rmarkdown pdf_document
#' @param Rmd_file file name of R Markdown file
#' @param Bib_file file name of Bib file
#' @export
render_jpr <- function(Rmd_file, Bib_file, use.et.al.first = T) {
  jpr_cite(Rmd_file, Bib_file, use.et.al.first)
  tmp_rmd1 <- paste0("tmp_author_", Rmd_file)
  tmp_rmd2 <- paste0("tmp_", Rmd_file)
  tmp_rmd3 <- paste0("tmp_abst_author_", Rmd_file)
  tmp_rmd4 <- paste0("tmp_abst_", Rmd_file)

  template_tex_file <- system.file("rmarkdown/templates/jpr/resources/jpr.tex",
    package = "jpaRmd"
  )
  format_pdf <- pdf_document(
    latex_engine = "xelatex",
    template = template_tex_file,
    keep_tex = TRUE,
    toc = TRUE,
    toc_depth = 3,
    highlight = "tango"
  )
  format_pdf$inherits <- "rmarkdown_output_format"
  output_file1 <- paste0(strsplit(Rmd_file, ".Rmd")[[1]], "_authorInfo")
  output_file2 <- paste0(strsplit(Rmd_file, ".Rmd")[[1]])
  output_file3 <- paste0(strsplit(Rmd_file, ".Rmd")[[1]], "_abst_authorInfo")
  output_file4 <- paste0(strsplit(Rmd_file, ".Rmd")[[1]], "_abst")
  render(tmp_rmd1, format_pdf, output_file1)
  render(tmp_rmd2, format_pdf, output_file2)
  render(tmp_rmd3, format_pdf, output_file3)
  render(tmp_rmd4, format_pdf, output_file4)
}

#' @title R Markdown template for Reply to reviewers
#' @export
#' @importFrom rmarkdown pdf_document
reply <- function() {
  template_tex_file <- system.file("rmarkdown/templates/reply/resources/reply.tex",
    package = "jpaRmd"
  )
  format_pdf <- pdf_document(
    latex_engine = "xelatex",
    template = template_tex_file,
    keep_tex = TRUE,
    toc = TRUE,
    toc_depth = 3,
    highlight = "tango"
  )
  format_pdf$inherits <- "rmarkdown_output_format"
  format_pdf
}
