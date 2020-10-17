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
#' @examples # set_rc_jpa("yoshihiko_kunisato")
#' @export

set_rc_jpa <- function (file_name = "paper"){
  path = getwd()
  # make README
  if(!file.exists(file.path(path, "README.md"))){
    file.create(file.path(path, "README.md"), showWarnings = FALSE)
    writeLines("## README\n\n- analysis:\n- data:\n- function:\n- materials:", "README.md")
  }

  # make RMarkdown file and directory
  if(!file.exists(file.path(path, paste0(file_name,".Rmd")))){
    rmarkdown::draft(paste0(file_name,".Rmd"), template = "jpa_jjp", package = "jpaRmd", edit = FALSE)
  }
  # make analysis directory
  if(!dir.exists(file.path(path, "analysis"))){
    dir.create(file.path(path, "analysis"), showWarnings = FALSE)
    file.create(file.path(path, "analysis/README_analysis.md"), showWarnings = FALSE)
    writeLines("README about analysis", "analysis/README_analysis.md")
  }

  # make data directory
  if(!dir.exists(file.path(path, "data"))){
    dir.create(file.path(path, "data"), showWarnings = FALSE)
    file.create(file.path(path, "data/README_data.md"), showWarnings = FALSE)
    writeLines("README about data", "data/README_data.md")
  }

  # make function directory
  if(!dir.exists(file.path(path, "function"))){
    dir.create(file.path(path, "function"), showWarnings = FALSE)
    file.create(file.path(path, "function/README_function.md"), showWarnings = FALSE)
    writeLines("README about function", "function/README_function.md")
  }

  # make materials directory
  if(!dir.exists(file.path(path, "materials"))){
    dir.create(file.path(path, "materials"), showWarnings = FALSE)
    file.create(file.path(path, "materials/README_materials.md"), showWarnings = FALSE)
    writeLines("README about materials", "materials/README_materials.md")
  }
}
