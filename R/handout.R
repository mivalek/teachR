
#' Convert .Rmd to course-formatted slide handout HTML document or R Notebook
#'
#' Lecture materials should be written in R Markdown with title, subtitle, and author in YAML header. {handout()} is a convenience function equivalent to \code{make.sheet(handout = TRUE, ...)}. It converts a .Rmd file into a HTML/R Notebook document applying the theme for given course.
#'
#' @param file \code{character}. Path to .Rmd file to convert.
#' @param course \code{character}. Course the sheet is for: one of \code{"dapR_1", "daprR_2", "dapR_3", "usmr", "msmr", "other"}.
#' @param tasks_to_headings \code{logical}. Should individual tasks and subtaks be rendered as level 2 and 3 (respectively) headings? \code{FALSE} by default.
#' @param notes \code{logical}. If \code{handout=TRUE}, then if \code{TRUE} a writable box for notes will appear at the bottom of each slide.
#' @param ntb \code{logical}. \code{TRUE} to render document as R Notebook. \code{FALSE} by default.
#' @param color \code{character}. Either a single valid colour (hex code or any of the values in \code{colours()}) or any valid value of the \code{course=} argument. If provided, it will be used to set colour scheme instead of \code{course=}.
#' @param toc \code{logical}. Should table of content be included
#' @param toc_depth \code{logical}. Depth of headers to include in table of contents.
#' @param toc_float \code{TRUE} to float the table of contents to the left of the main document content. Rather than TRUE you may also pass a list of options that control the behavior of the floating table of contents. For more details, see \code{\link[rmarkdown]{html_document}}.
#' @param fig_width,fig_depth \code{numeric}. Default width and height (in inches) for figures.
#' @param highlight \code{character}. Syntax highlighting style. See \code{\link[rmarkdown]{html_document}}.
#' @param keep_temp_Rmd \code{logical}. Should temporary Rmd file be kept? Useful for post-hoc edits and debugging. 
#' @param ... Other arguments to be passed to \code{rmarkdown:html_document} or \code{rmarkdown:html_notebook}.
#' @details Function requires a .css and .js files for correct formatting of lab sheets/handouts. These files sit on the stats website in the [root]/sheet_files folder and the path is hard-coded into the function. Look for css and js objects in function body.
#' @return \code{TRUE} if output .html file was successfully created.
#' @seealso \code{\link{make.sheet}()}
#' @examples
#' handout("C:/Users/mvalasek/slides/dapR_1_handout_demo.Rmd", "dapR_1")


handout <- function(file, course, tasks_to_headings = FALSE, ntb = FALSE, color = NULL, toc = T, toc_depth = 2, toc_float = T,
                       fig_width = 5, fig_height = 3.5, highlight = "tango", colour = color, keep_temp_Rmd = F, ...) {
  make.sheet(file = file, course = course, handout = TRUE, tasks_to_headings = tasks_to_headings, ntb = ntb, color = color,
             toc = toc, toc_depth = toc_depth, toc_float = toc_float,
             fig_width = fig_width, fig_height = fig_height, highlight = highlight, colour = colour, keep_temp_Rmd = keep_temp_Rmd, ...)
}
