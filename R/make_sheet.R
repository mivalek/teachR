
#' Convert .Rmd to course-formatted HTML document or R Notebook
#'
#' Lab sheets and lecture materials should be written in R Markdown with title, subtitle, and author in YAML header and saved on a local drive (due to Pandoc's issues with network drives). make.sheets() converts a .Rmd file into a HTML/R Notebook document applying the theme for given course.
#'
#' @param file character. Path to .Rmd file to convert.
#' @param course character. Course the sheet is for: one of "dapR_1", "daprR_2", "dapR_3", "usmr", "msmr", "other".
#' @param meta_folder character. path to folder with sheets.css and sheets.js for formatting HTML. By default a subdirectory of directory where file= is located.
#' @param ntb logical. TRUE to render document as R Notebook. FALSE by default.
#' @param toc logical. Should table of content be included
#' @param toc_depth logical. Depth of headers to include in table of contents.
#' @param toc_float TRUE to float the table of contents to the left of the main document content. Rather than TRUE you may also pass a list of options that control the behavior of the floating table of contents. For more details, see ?rmarkdown::html_document.
#' @param fig_width,fig_depth numeric. Default width and height (in inches) for figures.
#' @param ... Other arguments to be passed to rmarkdown:html_document or rmarkdown:html_notebook.
#' @return Function does not return anything but outputs a .html file.
#' @examples
#' # .Rmd and meta.folder must be on a local drive!
#' make.sheet("C:/Users/mvalasek/slides/dapR_1_demo.Rmd", "dapR_1", "C:/Users/mvalasek/meta")


make.sheet <- function(file, course, meta_folder, ntb = FALSE, toc = T, toc_depth = 2, toc_float = T,
                       fig_width = 5, fig_height = 3.5, ...) {
  require(rmarkdown)
  if (!file.exists(file)) stop("The file does not exist.")
  if (!tolower(course) %in% c("dapr_1", "dapr_2", "dapr_3", "usmr", "msmr", "other"))
    stop("course= must be one of c(\"dapr_1\", \"dapr_2\", \"dapr_3\", \"usmr\", \"msmr\", \"other\").")
  if (missing(meta_folder)) {
    meta_folder <- file.path(
      gsub(paste0("(.*)", .Platform$file.sep, ".*"), "\\1", file), "meta")
    if (!dir.exists(meta_folder)) {
      meta_folder <- file.path(getwd(), "meta")
      if (!dir.exists(meta_folder))
        stop("Cannot find the folder including .css, .js, and _header.html files.")
    }
  }
  course <- gsub("r_", "R_", tolower(course))
  x <- readLines(file)
  x <- gsub("^(\\s*?)>\\s*?-", "\\1-", x) # get rid of incremental bulletpoints if used ( > - ...)
  yaml <- grep("---", x)
  title <- grep("^\\s*?title:", x, value = T)
  subtitle <- grep("^\\s*?subtitle:", x, value = T)
  author <- grep("^\\s*?author:", x, value = T)
  x <- x[-(1:yaml[2])]
  x <- gsub("^#[^#]", "## ", x)
  h <- c(
    "---",
    title, subtitle, author,
    paste0("date: \"[Click for slides](",
           gsub("(.*)\\.[rR]md", "./\\1_slides.html",
                rev(unlist(strsplit(file, .Platform$file.sep)))[1]), ")\""),
    "---",
    " ",
    "```{r, echo=F, results='asis'}",
    "cat(\"",
    "<style>",
      ":root {",
        paste0("--theme-col: var(--", course, "-col);"),
      "}",
    "</style>",
    "\")",
    "```",
    "",
    "```{r, rsetup, include=F}",
    "knitr::opts_chunk$set(comment=NULL, collapse=T, strip.white=F, echo=T)",
    # "                      fig.height = 3.5,",
    # "                      fig.width = 5)",
    "```",
    " ",
    "```{r task_fun, echo=FALSE}",
    "t <- 1 # Task counter",
    "task <- function(x = t) {",
    "  t <<- x + 1",
    "  return(paste0(\"**Task \", x, \": **\"))",
    "}",
    "```"
  )
  css <- file.path(meta_folder, "css", "sheets.css")
  js <- file.path(meta_folder, "js", "sheets.js")
  x <- c(h, x)
  out_file <- gsub("\\.[Rr]md", "_temp.Rmd", file)
  writeLines(x, out_file)

  pres_file <- gsub("\\.[Rr]md$", ".html", file)

  if (ntb) {
    render(
      input = out_file,
      output_format = html_notebook(
        toc = toc, toc_depth = toc_depth, toc_float = toc_float,
        fig_width = fig_width, fig_height = fig_height,
        includes = includes(after_body = c(css, js)),
        ...
      ),
      output_file = pres_file)
  }
  else {
    render(
      input = out_file,
      output_format = html_document(
        toc = toc, toc_depth = toc_depth, toc_float = toc_float,
        fig_width = fig_width, fig_height = fig_height,
        includes = includes(after_body = c(css, js)),
        ...
    ),
    output_file = pres_file)
  }

  file.remove(out_file)
}
