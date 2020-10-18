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

#' Set Research Compendium for JPA
#'
#' @param file_name file name and directory name of RMarkdown
#' @return Make directories for Research Compendium of JPA and R Markdown file
#' @examples # set_rc_jpa("rmarkdown_for_reproducibility")
#' @export

set_rc_jpa <- function (file_name = "paper"){
  path = getwd()
  # make Directory & README
  if(!dir.exists(file.path(path, file_name))){
    dir.create(file.path(path, file_name), showWarnings = FALSE)
    file.create(file.path(path, file_name, "README.md"), showWarnings = FALSE)
    writeLines("## README\n\n- analysis:\n- data:\n- function:\n- materials:", file.path(path, file_name, "README.md"))
  }
  # make RMarkdown file and directory
  if(!file.exists(file.path(path, file_name, "paper.Rmd"))){
    rmarkdown::draft(file.path(path, file_name, "paper.Rmd"), template = "jpa_jjp", package = "jpaRmd", edit = FALSE)
  }
  # make analysis directory
  if(!dir.exists(file.path(path, file_name, "analysis"))){
    dir.create(file.path(path, file_name, "analysis"), showWarnings = FALSE)
    file.create(file.path(path, file_name, "analysis/README_analysis.md"), showWarnings = FALSE)
    writeLines("README about analysis", file.path(path, file_name, "analysis/README_analysis.md"))
  }

  # make data directory
  if(!dir.exists(file.path(path, file_name, "data"))){
    dir.create(file.path(path, file_name, "data"), showWarnings = FALSE)
    file.create(file.path(path, file_name, "data/README_data.md"), showWarnings = FALSE)
    writeLines("README about data", file.path(path, file_name, "data/README_data.md"))
  }

  # make function directory
  if(!dir.exists(file.path(path, file_name, "function"))){
    dir.create(file.path(path, file_name, "function"), showWarnings = FALSE)
    file.create(file.path(path, file_name, "function/README_function.md"), showWarnings = FALSE)
    writeLines("README about function", file.path(path, file_name, "function/README_function.md"))
  }

  # make materials directory
  if(!dir.exists(file.path(path, file_name, "materials"))){
    dir.create(file.path(path, file_name, "materials"), showWarnings = FALSE)
    file.create(file.path(path, file_name, "materials/README_materials.md"), showWarnings = FALSE)
    writeLines("README about materials", file.path(path, file_name, "materials/README_materials.md"))
  }
}
