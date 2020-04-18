
#' Generate boilerplate title page for Psychology@Sussex exams.
#'
#' Produces a title page and yaml front matter text to be used in \code{\link{make.paper}()}.
#'
#' @param question_files \code{character}. Paths to Rmd files with items. Best to have one file per author if there are multiple authors of exam items. See example_items.Rmd in package folder for the correct markdown.
#' @param module @param module_code \code{character}. Name and code of the module.
#' @param exam_diet \code{character}. Examinatiopn diet (e.g., sit - A2, resit - A3...). See example.
#' @param time_allowed \code{character}. Total time allowed for completion.
#' @param n_sections \code{numeric}. Number of sections in the paper.
#' @param marks_per_q \code{numeric}. Number of marks per item.
#' @param section_weights \code{character}. Line to include with information about section weighting.
#' @param sample \code{logical}. Should this be a sample paper title page? \code{FALSE} by default.
#' @return Function returns a \code{data.frame} with following columns:
#'   \itemize {
#'     \item \code{title_page} Text of the title page
#'     \item \code{yaml} YAML front matter
#'   }
#'     
#'Week, and difficulty are extracted from the question tag, e.g. {02S} for week 2, simple. See example_items.Rmd in package folder for the correct markdown.
#' @examples
#' boiler <- sussex.boilerplate(module = "Analysing Data",
#'                              module_code = "C8891",
#'                              exam_diet = "BSc FIRST YEAR EXAMINATION May/June 2020 (A2)",
#'                              time_allowed = "2 hours",
#'                              n_sections = 1,
#'                              marks_per_q = 2,
#'                              section_weights = NULL,
#'                              sample = F)
#' @export
#' 

sussex.boilerplate <- function(module, module_code, exam_diet, time_allowed, n_sections, marks_per_q, section_weights = NULL, sample = F) {
  instruction <- c(
    "INSTRUCTIONS",
    "Do not write your name on this question paper.",
    "",
    "Do not tear off any part of this question paper.",
    "",
    "Do not, under any circumstances, remove this question paper, used or unused, from the examination room. It must be left on your desk when you leave the examination.",
    "",
    paste("**Time allowed:", time_allowed, "**"),
    "",
    "Answer **ALL** questions.",
    "",
    paste0("There ",
           ifelse(n_sections > 1,
                  paste("are ", n_sections, " sections"),
                  " is one section"),
           " in this exam paper",
           if (n_sections > 1) paste0(" (", paste(LETTERS[1:n_sections], collapse = ", "), ")"),
           ". All questions are worth ",  marks_per_q, " marks. ",
           if (n_sections > 1) section_weights),
    ifelse(sample, "###### NOTE: The real exam has more questions", ""), "", "",
    ifelse(sample, "Write your answers on a sheet of paper",
           "Mark your answers on the answer sheet provided."),
    if (sample) "###### The real exam will use MCQ an answer sheet",
    "",
    "**IF THERE ARE ANY RELEVANT TABLES THAT YOU NEED TO ANSWER THE QUESTIONS, THEY ARE PROVIDED AT THE BACK OF THIS EXAM PAPER.**"
  )
  
  title_page <- c(
    "### The University of Sussex",
    "\n\n\\ \n\n",
    paste0("#### **", exam_diet, "**"),
    "\n\n\\ \n\n",
    paste("##", module),
    "\n\n\\ \n\n",
    if (sample) c("## Sample Paper", "\n\n\\ \n\n"),
    "---------------------------------------------------------------------------",
    "\n\n\\ \n\n",
    "# Do not turn over until instructed to by the chief invigilator",
    "\n\n\\ \n\n",
    gsub("^#### #", "#", paste("####", instruction)),
    "\n##### page break"
  )
  
  
  yaml_header <- c(
    "---",
    "title: 'Candidate Number'",
    "subtitle: '\\ '",
    "author: '\\ '",
    paste0("date: '", module_code, "'"),
    "---"
  )
  
  return(list(title_page = title_page,
              yaml = yaml_header))
}
