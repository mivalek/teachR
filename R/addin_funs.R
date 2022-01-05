#' RStudio addins for inserting teaching material divs and quiz chunks
#'
#' @export
#' 

insertSol <- function() {
  pos <- rstudioapi::getActiveDocumentContext()$selection[[1]]$range$start[1]
  rstudioapi::insertText("\n:::{.sol}\n\n:::\n")
  rstudioapi::setCursorPosition(c(pos + 2, 1))
}

insertSolChunk <- function() {
  pos <- rstudioapi::getActiveDocumentContext()$selection[[1]]$range$start[1]
  rstudioapi::insertText("\n```{r, sol=TRUE}\n\n```\n")
  rstudioapi::setCursorPosition(c(pos + 2, 1))
}

insertBox <- function() {
  pos <- rstudioapi::getActiveDocumentContext()$selection[[1]]$range$start[1]
  rstudioapi::insertText("\n:::{.box}\n\n:::\n")
  rstudioapi::setCursorPosition(c(pos + 2, 1))
}

insertExtraBox <- function() {
  pos <- rstudioapi::getActiveDocumentContext()$selection[[1]]$range$start[1]
  rstudioapi::insertText("\n:::{.extra}\n\n:::\n")
  rstudioapi::setCursorPosition(c(pos + 2, 1))
}

insertMoreBox <- function() {
  pos <- rstudioapi::getActiveDocumentContext()$selection[[1]]$range$start[1]
  rstudioapi::insertText("\n:::{.more}\n\n:::\n")
  rstudioapi::setCursorPosition(c(pos + 2, 1))
}

insertRbox <- function() {
  pos <- rstudioapi::getActiveDocumentContext()$selection[[1]]$range$start[1]
  rstudioapi::insertText("\n:::{.r-box}\n\n:::\n")
  rstudioapi::setCursorPosition(c(pos + 2, 1))
}

insertHint <- function() {
  pos <- rstudioapi::getActiveDocumentContext()$selection[[1]]$range$start[1]
  rstudioapi::insertText("\n:::{.hint}\n\n:::\n")
  rstudioapi::setCursorPosition(c(pos + 2, 1))
}

insertWarn <- function() {
  pos <- rstudioapi::getActiveDocumentContext()$selection[[1]]$range$start[1]
  rstudioapi::insertText("\n:::{.warn}\n\n:::\n")
  rstudioapi::setCursorPosition(c(pos + 2, 1))
}

insertTask <- function() {
  rstudioapi::insertText("`r task()`")
}

insertSubtask <- function() {
  rstudioapi::insertText("`r subtask()`")
}

insertQuiz <- function() {
  pos <- rstudioapi::getActiveDocumentContext()$selection[[1]]$range$start[1]
  rstudioapi::insertText("\n```{r, quiz=TRUE}\nmcq(\n\tq = \"\",\n\toptions = c(), # list possible response options,\n\t# default options (can be omitted):\n\tcorrect = 1, # 1st option is correct\n\tshuffle = TRUE, # randomise order of options\n\tcorrect_msg = \"Correct!\",\n\twrong_msg = \"That's not right...\",\n\tnumbered = TRUE #should question have a number?\n)\n\nsaq(\n\tq = \"\",\n\tcorrect = , # provide correct response (numeric or string)\n\t# default options:\n\tnumeric = TRUE, # should user input be interpreted as number or string?\n\tcorrect_msg = \"Correct!\",\n\twrong_msg = \"That's not right...\",\n\tnumbered = TRUE\n)\n```\n")
  rstudioapi::setCursorPosition(c(pos + 3, 7))
}

insertAside <- function() {
  pos <- rstudioapi::getActiveDocumentContext()$selection[[1]]$range$start[1]
  rstudioapi::insertText("\n<aside>\n\n</aside>\n")
  rstudioapi::setCursorPosition(c(pos + 2, 1))
}

