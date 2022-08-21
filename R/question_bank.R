
#' Create a question bank from .Rmd exam item files.
#'
#' Produces a data frame that can be used as first argument to \code{\link{make.paper}()}.
#'
#' @param question_files \code{character}. Paths to Rmd files with items. Best to have one file per author if there are multiple authors of exam items. See mcq_example.Rmd.Rmd in package folder for the correct markdown.
#' @param authors \code{character}. Requires one author per Rmd file.
#' @return Function returns a \code{data.frame} with following columns:
#'  \itemize{
#'    \item \code{q_num} Original item number in the Rmd file
#'    \item \code{week} Week of course/module the item pertains to
#'    \item \code{diff} Difficulty (Simple, Easy, Medium, Hard)
#'    \item \code{author} Author of the item
#'    \item \code{stem} Stem/lead-in
#'    \item \code{opts} Response options
#'    \item \code{correct} Correct answer
#'    \item \code{group} If item is a part of a group tht share a group stem, ID number identifying all items from the same group
#'    \item \code{group_stem} Stem shared by a group of items.
#'  }
#'     
#'Week, and difficulty are extracted from the question tag, e.g. {02S} for week 2, simple. See mcq_example.Rmd in package folder for the correct markdown.
#' @importFrom tibble tibble
#' @examples
#' question.bank("exam/and_qu_MV.Rmd", "MV")
#' @export
#' 


question.bank <- function(question_files, authors) {
  
  all_questions <- all_groups <- groups <- list()
  group_counter <- first_gr_couter <-  0
  for (i in seq_along(question_files)) {
    first_gr_couter <- first_gr_couter + 1
    questions <- readLines(question_files[i])
    questions <- questions[grep("^\\s*$", questions, invert = T)]
    
    
    group_ind <-  cbind(
      grep("^\\s*\\[multi\\]", questions),
      grep("^\\}", questions)
    )
    
    if (length(group_ind) > 0) {
      group_ind <- split(
        group_ind,
        rep(1:nrow(group_ind), 2))
      
      groups <- lapply(group_ind, function(ind) questions[seq(ind[1], ind[2])])
      group_qus <- lapply(groups, function(x) x[(grep("\\{\\s*$", groups[[1]]) + 1):(length(x) - 1)])
      
      questions <- questions[-unname(
        unlist(
          lapply(group_ind, function(x) seq(x[1], x[2]))
        )
      )]
    }
    
    temp <- list()
    if (length(questions) > 0)
      temp <- teachR:::get_questions(questions, author = authors[i])
    
    if (length(group_ind) > 0) {
      for (j in 1:length(groups)) {
        group_counter <- group_counter + 1
        temp <- append(temp, teachR:::get_questions(groups[[j]], group_counter, authors[i]))
      }
    }
    all_questions <- append(all_questions, temp)
    all_groups <- append(all_groups, groups)
  }
  names(all_questions) <- seq_along(all_questions)
  names(all_groups) <- seq_along(all_groups)
  
  group_stems <- unname(unlist(
    lapply(all_groups,
           function(x) paste(x[grep("^\\s*\\[multi\\]", x):grep("\\{\\s*$", x)], collapse="\\n\\n")
    )
  ))
  
  group_stems <- gsub("^\\s*\\[multi\\]\\s*|\\s*\\{\\s*$", "", group_stems)
  
  all_q_tib <- tibble::tibble(
    q_num = unlist(lapply(all_questions, function(x) x$q_num)),
    week = unlist(lapply(all_questions, function(x) x$week)),
    diff = factor(unlist(lapply(all_questions, function(x) x$diff)),
                  levels = c("S", "E", "M", "D"),
                  labels = c("Simple", "Easy", "Medium", "Difficult")),
    author = unlist(lapply(all_questions, function(x) x$author)),
    stem = unlist(lapply(all_questions, function(x) x$stem)),
    opts = unlist(lapply(all_questions, function(x) x$opts), recursive = F),
    correct = unlist(lapply(all_questions, function(x) x$correct)),
    group =  unlist(lapply(all_questions, function(x) x$group)),
    group_stem = group_stems[group],
    stringsAsFactors = F
  )
  
  return(all_q_tib)
}
