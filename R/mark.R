
#' Mark coursework assignment Rmd
#'
#' For Rmd submissions of reports and other coursework. Function checks word count and inserts space for feedback.
#'
#' @param file \code{character}. Path to .Rmd file to mark.
#' @param feedback \code{logical}. Should feedback box with marker's comments appear at the bottom of document? \code{FALSE} by default.
#' @param count_words \code{logical}. \code{TRUE} counts body text words and if word count exceeds \code{limit}, inserts a warning line in HTML. \code{!feedback} by default.
#' @param limit \code{numeric}. Word count limit
#' @param color \code{character}. Single valid colour string (hex code or any of the values in \code{colours()}). 
#' @details Function run with \code{feedback = FALSE} will overwrite the original Rmd file and knit it into HTML. This should be done before marking in order to automate word count. The overwritten Rmd should then be used for comments and feedback. Once done, \code{mark(feedback = TRUE)} should be run on the edited Rmd. This will output a ..._marked.html file that can be returned to students.
#' 
#' In-text comments can be inserted into .Rmd file on a lew line surrounded by the <fb></fb> HTML tag.
#' @return \code{TRUE} if output .Rmd and .html file were successfully created.
#' @examples
#' # first run
#' mark("C:/work/201000.Rmd")
#' # then
#' mark("C:/work/201000.Rmd", T)

mark <- function(file, feedback = F, count_words = !feedback, limit, color = "#b38ed2") {
  ff <- readLines(file)
  out_file <- ifelse(feedback, sub("\\.Rmd$", "_marked.Rmd", file), file)
  
  # word limit reached line
  insert <- '\n\\ \n\n<div><p style="color:#cc0000;font-size:2em;text-align:center">*** WORD LIMIT REACHED ***</p></div>\n\n\\ \n'
  
  if (count_words && !any(grepl(insert, ff, fixed = T))) {
    
    ff_edit <- ff
    
    # name lines to identify limit-th line later
    names(ff_edit) <- paste0(1:length(ff_edit), "_")
    
    # remove code chunks
    code_chunks <- matrix(grep("^\\s*```", ff_edit), ncol = 2, byrow = T)
    ff_edit <- ff_edit[-unlist(apply(code_chunks, 1, function(x) x[1]:x[2]))]
    
    # remove inserted pics
    ff_edit <- grep("^!\\[", ff_edit, invert = T, value = T)
    
    # remove inline code
    ff_edit <- gsub("`.*?`", "", ff_edit)
    
    # remove YAML header
    ff_edit <- ff_edit[-c(1:grep("^\\s*---", ff_edit)[2])]
    
    words <- unlist(strsplit(ff_edit, "\\s+"))
    words <- grep("[A-Za-z]", words, value = T)
    
    # identify line that includes limit-th word
    cutoff <- as.numeric(unlist(strsplit(names(words[limit]), "_")))[1]
    
    feedback <- c('\n\\ \n',
                  '<div class="feedback">',
                  '<!-- THE GOOD -->',
                  '\n\n',
                  '<!-- THE BAD -->',
                  '\n\n',
                  '<!-- RECOMMENDATIONS -->',
                  '\n\n',
                  '</div>\n\n')
    out <- if (is.na(cutoff)) {
      c(ff, feedback)
    } else {
      c(ff[1:cutoff], insert, ff[(cutoff + 1):length(ff)], feedback)
    }
      
    writeLines(out, out_file)
    
    render(input = out_file, output_format = html_document(toc = F))
    
  } else if (feedback) {
    
    good_text <- c(
      "\n\\ \n\n",
      "First of all, it is important to highlight the strengths of your report:",
      ""
    )
    
    bad_text <- c(
      "\n\\ \n\n",
      "There were, however, also things that lowered the overal quality of your work:",
      ""
    )
    
    recom_text <- c(
      "\n\\ \n\n",
      "These are some recommendations that will help you produce a better piece of writing:",
      ""
    )
    
    # code chunk that defines --theme-col for CSS
    color_chunk <-  c("```{r, echo=F, results='asis'}",
    "cat(\"",
    "<style>",
    ":root {",
    paste0("--theme-col: ", paste(col2rgb(color), collapse=", "), ";"),
    "}",
    "</style>",
    "\")",
    "```")
    
    # insert chunk
    ff <- c(
      ff[1:grep("^\\s*---", ff)[2]],
      "", color_chunk, "",
      ff[(grep("^\\s*---", ff)[2] + 1):length(ff)]
    )
    
    
    ff[grep("<!-- THE GOOD", ff)] <- paste(good_text, collapse = "\n")
    ff[grep("<!-- THE BAD", ff)] <- paste(bad_text, collapse = "\n")
    ff[grep("<!-- RECOMMEND", ff)] <- paste(recom_text, collapse = "\n")
    
    writeLines(ff, out_file)
    
    render(input = out_file, output_format = html_document(
      toc = F, includes = includes(after_body = paste0(path.package("teachR"), "/feedback.css")))
    )
  }  
}
