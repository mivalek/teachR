
#' Write console output to clipboard
#'
#' Captures console output of a command (\code{show()}) and copies it to clipboard as a HTML code block.
#'
#' @param x Valid R command whose output you want to copy.
#' @param style \code{logical}. Should the code block be styled. Default is \code{FALSE}.
#' @param global_style \code{logical}. If \code{TRUE}, the style gets applied to all code blocks on the page, otherwise just this one. Default is \code{FALSE}.
#' @param css \code{character}. A string with valid CSS to set the style. 
#' 
#' @importFrom xtable sanitize
#' @examples
#' x <- rnorm(100)
#' y <- 4 + .7 * x + rnrom(100)
#' get_output(summary(lm(y ~ x)))
#' @export
#' 

get_output = function(x, style = FALSE, global_style = FALSE, css = "background:#002140;color:white;width:max-content;margin:auto") {
  id = paste(sample(c(letters, LETTERS), 10), collapse = "")
  out <- capture.output(show(x))
  out <- paste(xtable::sanitize(out, "html"), collapse = "\n")
  styleTag <- ""
  if (style) {
    styleTag <- ifelse(global_style,
                       paste0("<style>pre{", css, "}</style>"),
                       paste0("<style>#", id, "{", css, "}</style>"))
  }
  out <- paste0(styleTag, "<pre id=\"", id, "\"><code>", out, "</pre></code>")
  writeClipboard(out)
}
