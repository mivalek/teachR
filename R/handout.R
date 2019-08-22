
#' Convert .Rmd to course-formatted slide handout HTML document or R Notebook
#'
#' Lecture materials should be written in R Markdown with title, subtitle, and author in YAML header. handout() is a convenience function equivalent to make.sheet(handout = TRUE, ...). It converts a .Rmd file into a HTML/R Notebook document applying the theme for given course.
#'
#' @param file character. Path to .Rmd file to convert.
#' @param course character. Course the sheet is for: one of "dapR_1", "daprR_2", "dapR_3", "usmr", "msmr", "other".
#' @param ntb logical. TRUE to render document as R Notebook. FALSE by default.
#' @param toc logical. Should table of content be included
#' @param toc_depth logical. Depth of headers to include in table of contents.
#' @param toc_float TRUE to float the table of contents to the left of the main document content. Rather than TRUE you may also pass a list of options that control the behavior of the floating table of contents. For more details, see ?rmarkdown::html_document.
#' @param fig_width,fig_depth numeric. Default width and height (in inches) for figures.
#' @param highlight character. Syntax highlighting style. See ?rmarkdown::html_document.
#' @param ... Other arguments to be passed to rmarkdown:html_document or rmarkdown:html_notebook.
#' @details Function requires a .css and .js files for correct formatting of lab sheets/handouts. These files sit on the stats website in the [root]/sheet_files folder and the path is hard-coded into the function. Look for css and js objects in function body.
#' @return TRUE if output .html file was successfully created.
#' @seealso make.sheet()
#' @examples
#' handout("C:/Users/mvalasek/slides/dapR_1_handout_demo.Rmd", "dapR_1")


handout <- function(file, course, ntb = FALSE, toc = T, toc_depth = 2, toc_float = T,
                       fig_width = 5, fig_height = 3.5, highlight = "tango", ...) {
  make.sheet(file = file, course = course, handout = TRUE, ntb = ntb, toc = toc, toc_depth = toc_depth,
             toc_float = toc_float, fig_width = fig_width, fig_height = fig_height, highlight = highlight, ...)
}
