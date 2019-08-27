
#' Convert .Rmd to course-formatted slide handout HTML document or R Notebook
#'
#' Lecture materials should be written in R Markdown with title, subtitle, and author in YAML header. {handout()} is a convenience function equivalent to \code{make.sheet(handout = TRUE, ...)}. It converts a .Rmd file into a HTML/R Notebook document applying the theme for given course.
#'
#' @param file \code{character}. Path to .Rmd file to convert.
#' @param course \code{character}. Course the sheet is for: one of \code{"dapR_1", "daprR_2", "dapR_3", "usmr", "msmr", "other"}.
#' @param ntb \code{logical}. \code{TRUE} to render document as R Notebook. \code{FALSE} by default.
#' @param toc \code{logical}. Should table of content be included
#' @param toc_depth \code{logical}. Depth of headers to include in table of contents.
#' @param toc_float \code{TRUE} to float the table of contents to the left of the main document content. Rather than TRUE you may also pass a list of options that control the behavior of the floating table of contents. For more details, see \code{\link[rmarkdown]{html_document}}.
#' @param fig_width,fig_depth \code{numeric}. Default width and height (in inches) for figures.
#' @param highlight \code{character}. Syntax highlighting style. See \code{\link[rmarkdown]{html_document}}.
#' @param ... Other arguments to be passed to \code{rmarkdown:html_document} or \code{rmarkdown:html_notebook}.
#' @details Function requires a .css and .js files for correct formatting of lab sheets/handouts. These files sit on the stats website in the [root]/sheet_files folder and the path is hard-coded into the function. Look for css and js objects in function body.
#' @return \code{TRUE} if output .html file was successfully created.
#' @seealso \code{\link{make.sheet}()}
#' @examples
#' handout("C:/Users/mvalasek/slides/dapR_1_handout_demo.Rmd", "dapR_1")


handout <- function(file, course, ntb = FALSE, toc = T, toc_depth = 2, toc_float = T,
                       fig_width = 5, fig_height = 3.5, highlight = "tango", ...) {
  make.sheet(file = file, course = course, handout = TRUE, ntb = ntb, toc = toc, toc_depth = toc_depth,
             toc_float = toc_float, fig_width = fig_width, fig_height = fig_height, highlight = highlight, ...)
}
