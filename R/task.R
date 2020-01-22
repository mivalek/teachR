
#' Functions used within \code{\link{make.sheet}()}
#' 
#' These are not to be used by user.
#' 

task <- function(x = t, headings = tasks_to_headings) {
  t <<- x + 1
  s <<- 1
  return(paste0("\\ \n\n", ifelse(headings, "## ", "**"), "Task ", x, ifelse(headings, "\n\n", ": ")))
}

subtask <- function(x = t-1, y = s, headings = tasks_to_headings) {
  s <<- y + 1
  return(paste0("\\ \n\n", ifelse(headings, "### ", "**"), "Task ", x, ".", y, ifelse(headings, "\n\n", ": ")))
}
