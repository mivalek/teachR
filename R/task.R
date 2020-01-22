
#' Functions used within \code{\link{make.sheet}()}
#' 
#' These are not to be used by user.
#' 

task <- function(x = tsk, headings = tasks_to_headings) {
  tsk <<- x + 1
  s_tsk <<- 1
  return(paste0("\\ \n\n", ifelse(headings, "## ", "**"), "Task ", x, ifelse(headings, "\n\n", ": ")))
}

subtask <- function(x = tsk - 1, y = s_tsk, headings = tasks_to_headings) {
  s_tsk <<- y + 1
  return(paste0("\\ \n\n", ifelse(headings, "### ", "**"), "Task ", x, ".", y, ifelse(headings, "\n\n", ": ")))
}
