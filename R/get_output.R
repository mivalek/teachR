
#' Write console output to clipboard
#'
#' Captures console output of a command (\code{show()}) and copies it to clipboard as a HTML code block.
#'
#' @param x Valid R command whose output you want to copy.
#' @importFrom xtable sanitize
#' @examples
#' x <- rnorm(100)
#' y <- 4 + .7 * x + rnrom(100)
#' get_output(summary(lm(y ~ x)))
#' @export
#' 

get_output = function(x) {
  out <- capture.output(show(x))
  out <- paste(xtable::sanitize(out, "html"), collapse = "\n")
  out <- paste0("<pre><code>", out, "</pre></code>")
  writeClipboard(out)
}
