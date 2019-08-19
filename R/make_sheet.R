
#' Convert .Rmd to course-formatted HTML document or R Notebook
#'
#' Lab sheets and lecture materials should be written in R Markdown with title, subtitle, and author in YAML header and saved on a local drive (due to Pandoc's issues with network drives). make.sheets() converts a .Rmd file into a HTML/R Notebook document applying the theme for given course.
#'
#' @param file character. Path to .Rmd file to convert.
#' @param course character. Course the sheet is for: one of "dapR_1", "daprR_2", "dapR_3", "usmr", "msmr", "other".
#' @param handout logical. TRUE adds "Click for slides" link under author. To be used only for slide handout HTML files. FALSE by default.
#' @param ntb logical. TRUE to render document as R Notebook. FALSE by default.
#' @param toc logical. Should table of content be included
#' @param toc_depth logical. Depth of headers to include in table of contents.
#' @param toc_float TRUE to float the table of contents to the left of the main document content. Rather than TRUE you may also pass a list of options that control the behavior of the floating table of contents. For more details, see ?rmarkdown::html_document.
#' @param fig_width,fig_depth numeric. Default width and height (in inches) for figures.
#' @param highlight character. Syntax highlighting style. See ?rmarkdown::html_document.
#' @param ... Other arguments to be passed to rmarkdown:html_document or rmarkdown:html_notebook.
#' @details Function requires a .css and .js files for correct formatting of lab sheets/handouts. These files sit on the stats website in the [root]/sheet_files folder and the path is hard-coded into the function. Look for css and js objects in function body.
#' @return TRUE if output .html file was successfully created.
#' @seealso handout()
#' @examples
#' # .Rmd must be on a local drive!
#' make.sheet("C:/Users/mvalasek/slides/dapR_1_handout_demo.Rmd", "dapR_1")


make.sheet <- function(file, course, handout = FALSE, ntb = FALSE, toc = T, toc_depth = 2, toc_float = T,
                       fig_width = 5, fig_height = 3.5, highlight = "tango", ...) {
  if (!file.exists(file)) stop("The file does not exist.")
  if (!tolower(course) %in% c("dapr_1", "dapr_2", "dapr_3", "usmr", "msmr", "other"))
    stop("course= must be one of c(\"dapr_1\", \"dapr_2\", \"dapr_3\", \"usmr\", \"msmr\", \"other\").")
  course <- gsub("r_", "R_", tolower(course))
  x <- readLines(file)
  x <- gsub("^(\\s*?)>\\s*?-", "\\1-", x) # get rid of incremental bulletpoints if used ( > - ...)
  x <- gsub("#\\s*?inc\\s*?$", "", x) # get rid of #inc
  yaml <- grep("---", x)
  title <- grep("^\\s*?title:", x, value = T)[1]
  subtitle <- grep("^\\s*?subtitle:", x, value = T)[1]
  author <- grep("^\\s*?author:", x, value = T)[1]
  x <- x[-(1:yaml[2])]
  x <- gsub("^#[^#]", "## ", x)
  h <- c(
    "---",
    title, subtitle, author,
    if (handout) paste0("date: \"[Click for slides](",
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
  css <- "https://mivalek.github.io/sheet_files/sheets.css"
  js <- "https://mivalek.github.io/sheet_files/sheets.js"
  x <- c(h, x)
  out_file <- gsub("\\.[Rr]md", "_temp.Rmd", file)
  writeLines(x, out_file)

  pres_file <- gsub("\\.[Rr]md$", ".html", file)

  if (ntb) {
    render(
      input = out_file,
      output_format = html_notebook(
        toc = toc, toc_depth = toc_depth, toc_float = toc_float,
        fig_width = fig_width, fig_height = fig_height, highlight = highlight,
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
        fig_width = fig_width, fig_height = fig_height, highlight = highlight,
        includes = includes(after_body = c(css, js)),
        ...
    ),
    output_file = pres_file)
  }

  file.remove(out_file)
}
