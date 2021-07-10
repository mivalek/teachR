
### TASKS
# task counters
tsk <- s_tsk <- 1

# functions

task <- function(difficulty, x = tsk, heading_level = ifelse(exists("task_heading_level"), task_heading_level, 3)) {
  tsk <<- x + 1
  s_tsk <<- 1
  return(paste0(
    "\n\n",
    paste(rep("#", heading_level), collapse=""),
    " Task ", x,
    ifelse(missing(difficulty),
           "",
           paste0("{.", as.character(substitute(difficulty)), "}")),
    "\n\n")
  )
}

subtask <- function(difficulty, x = tsk - 1, y = s_tsk, heading_level = ifelse(exists("task_heading_level"), task_heading_level + 1, 4)) {
  s_tsk <<- y + 1
  return(paste0(
    "\n\n",
    paste(rep("#", heading_level), collapse=""),
    " Task ", x, ".", y,
    ifelse(missing(difficulty),
           "",
           paste0("{.", as.character(substitute(difficulty)), "}")),
    "\n\n")
  )
}

### QUIZZES
# counter
quiz_qu_no <- 1

# quiz functions
mcq <- function(q, options, correct = 1, shuffle = TRUE, code = FALSE,
                correct_msg = "Correct!", wrong_msg = "That's not right...", numbered = TRUE) {
  corrAns <- gsub("--", "-\\\\-", options[correct])
  cat(
    "<div class=\"question mcq", if (shuffle) " shuffle", if (code) " code","\">\n",
    if (numbered) paste0("#### QUESTION ", quiz_qu_no, "\n\n"),
    q,
    "\n\n:::{.corrAns}\n",
    corrAns,
    "\n:::\n<div class=\"submit\"><p class=\"opts", if (shuffle) paste(" shuffle") ,"\">", paste0("<input class=\"quizSubmit\" type=\"submit\" value=\"", options, "\">\n", collapse = ""),
    "</p><p class=\"feedback\"><span class=\"correct\">", correct_msg, "</span><span class=\"incorrect\">", wrong_msg, "</span></p></div></div>\n",
    sep = ""
  )
  if (numbered)  quiz_qu_no <<- quiz_qu_no + 1 
}

saq <- function(q, correct, numeric = TRUE, code = FALSE,
                correct_msg = "Correct!", wrong_msg = "That's not right...", numbered=TRUE) {
  cat(
    "<div class=\"question saq", if (code) " code", "\">\n",
    if (numbered) paste0("#### QUESTION ", quiz_qu_no, "\n\n"),
    q,
    "\n\n:::{.corrAns", ifelse(numeric, "", " .str"),"}\n",
    paste(gsub("--", "-\\\\-", correct), collapse = " | "),
    "\n:::\n:::{.submit}\n",
    "<input class=\"quizInput\" value=\"\">",
    "<input class=\"quizSubmit\" type=\"submit\" value=\"Submit\"><span class=\"correct\">", correct_msg, "</span><span class=\"incorrect\">", wrong_msg, "</span>\n:::\n</div>\n",
    sep = ""
  )
  if (numbered) quiz_qu_no <<- quiz_qu_no + 1 
}
