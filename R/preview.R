
#' Preview marked HTML report in RStudio Viewer
#'
#' Wrapper for \code{teachR::mark()} producing a temporary preview of commented HTML file in the RStudio Viewer pane.
#' 
#' @param file Either path to .Rmd file to mark or, if working directory is set to the folder containing the file, ID number.
#' @param rubric_url \code{character}. A named list containing the rubric criteria (c1, c2, etc). Each criterion should be a list with \code{name}, \code{col}, and \code{text} elements containing the name, colour, and description of the criterion, respectively.
#' @param fdbck_boiler_text \code{character}. Optional path to boilerplate text to be used in the Feedback sections. See \code{teachR::mark()} 
#' @param rubric_btn_url \code{cahracter}. URL to a webpage containing markng rubric/criteria/guidelines for markers' quick reference.
#' @param flowchart_url \code{cahracter}. URL to a webpage containing report flowchart students were supposed to follow. Also for for markers' quick reference. Can be either a .html file or image.
#' @param quick_comment_url \code{cahracter}. URL to a (Google?) spreadsheet containing quick comments for convenient marking. 
#' @details Function knits the file in the temp directory (\code{tempdir()} and opens it in RStudio's Viewer pane. Neither gubric grades nor marks are rendered. It requires all packages used in the Rmd report to be installed beforehand.
#' @examples
#' preview(file = "marking/AnD/2019-20/48323.Rmd")
#' @export
#' 

preview <- function(file, rubric_url = "https://raw.githubusercontent.com/SussexPsychMethods/and_pub/master/marking/sussex_rubric.R",
                    fdbck_boiler_text = "https://raw.githubusercontent.com/SussexPsychMethods/and_pub/master/marking/fdbck_boilerplate.txt",
                    install_missing_pkgs = F,
                    rubric_btn_url = "https://mivalek.github.io/adata/marking/rubric_and_criteria_faq.html",
                    flowchart_url = "https://raw.githubusercontent.com/mivalek/mivalek.github.io/master/adata/marking/AnD_report_flowchart.png",
                    quick_comment_url = "https://mivalek.github.io/adata/marking/quick_comments.html",
                    button_urls = "https://mivalek.github.io/adata/marking",
                    height = "maximize") {
  
  # cand no must be in global env for knitting
  file <- ifelse(grepl("\\.rmd$", file, ignore.case = T), file, 
                paste0(id, ".Rmd"))
  if (!file.exists(file)) 
    stop(paste0("Can't find ", file, ". Did you set working directory?"))
  
  fname <- rev(unlist(strsplit(file, "[\\/]")))[1]
  file.copy(file, file.path(tempdir(), fname))
  wd_restore <- getwd()
  setwd(tempdir())
  on.exit(setwd(wd_restore))
  
  knitr::purl(fname, "temp.R")
  r_code <- readLines("temp.R")
  file.remove("temp.R") # tidy up
  
  green <- grep("adata::green_data()", r_code, fixed = T)
  red <- grep("adata::red_data()", r_code, fixed = T)
  
  if (length(red) != 0) {
    study <- ifelse(
      length(green) != 0 && isTRUE(green > red),
      "green",
      "red")
  } else if (length(green) != 0) study <- "green"
  
  
  eval(parse(text = grep("candidate_number", r_code, value = T)), envir = globalenv())
  on.exit(rm("candidate_number", "rubric", envir = globalenv()), add = T)
  
  html <- sub("\\.rmd$", "_marked.html", fname, ignore.case = T)
   
  source(rubric_url)
  
  teachR::mark(file = fname, study=study, mark = NULL,
       rubric_grades = factor(rep(NA, 5)), rubric = rubric, include_rubric_desc = T,
       include_results = T, results_obj = teachR:::check_results(study, NULL)$correct_res,
       feedback = T,
       fdbck_boiler_text = fdbck_boiler_text,
       install_missing_pkgs = F, collapse_chunks = T,
       rubric_url = rubric_btn_url,
       flowchart_url = flowchart_url,
       quick_comment_url = quick_comment_url,
       preview = T)
  
  knitted <- readLines(html)
  urls <- grep(paste0(button_urls, ".*?\\.html"), knitted, value = T)
  urls <- gsub(".*(https.*?\\.html).*", "\\1", urls)
  
  dir.create("doc")
  sapply(urls, function(x) download.file(x, file.path("doc", gsub(".*/", "", x))))
  knitted <- gsub(paste0("<iframe src=\"", button_urls), "<iframe src=\"doc/", 
                  knitted)
  writeLines(knitted, html)
  rstudioapi::viewer(html, height = height)
}
