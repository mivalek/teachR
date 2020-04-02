
#' Mark coursework assignment Rmd
#'
#' For Rmd submissions of reports and other coursework. Function checks word count and inserts space for feedback.
#'
#' @param file \code{character}. Either path to .Rmd file to mark or URL from which to read the file.
#' @param file_name \code{character}. If \code{file=} is a URL, name of file to write to.
#' @param mark \code{numeric}. Total mark/grade for given assignment.
#' @param feedback \code{logical}. Should feedback box with marker's comments appear at the bottom of document? \code{FALSE} by default.
#' @param count_words \code{logical}. \code{TRUE} counts body text words and if word count exceeds \code{limit}, inserts a warning line in HTML. \code{!feedback} by default.
#' @param limit \code{numeric}. Word count limit
#' @param include_results \code{logical}. If \code{TRUE}, object provided to \code{results_obj=} will get inserted into a formatted box in knitted HTML. \code{FALSE} by default.
#' @param results_obj See \code{include_results}.
#' @param remove_results \code{logical}. If \code{TRUE}, correct results will not be displayed in final marked HTML document. \code{FALSE} by default.
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

mark <- function(file, file_name = file, mark = NULL, feedback = F, count_words = !feedback, limit = 2000, include_results = F, results_obj = NULL, remove_results = F, color = "#b38ed2") {
  ff <- readLines(file)
  if (grepl("^https://", file_name)) {
    stop("Plesae provide value to file_name= when reading file from URL.")
  } else if (!grepl("\\.rmd$", tolower(file_name)))
    file_name <- paste0(file_name, ".Rmd")
  
  if (include_results && missing(results_obj))
    stop("Please provide results_obj= if include_results = TRUE.")
  
  out_file <- ifelse(feedback, sub("\\.Rmd$", "_marked.Rmd", file_name), file_name)
  
  # word limit reached line
  insert <- '\n\\ \n\n<div>
  <p style="color:#cc0000;font-size:2em;text-align:center">*** WORD LIMIT REACHED ***</p>
  <p style="color:#cc0000;text-align:center">(Scroll down for feedback)</p>
  </div>\n\n\\ \n'
  
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
    words <- grep("[[:alnum:]]", words, value = T)
    
    # identify line that includes limit-th word
    cutoff <- as.numeric(unlist(strsplit(names(words[limit]), "_")))
    cutoff_line <- cutoff[1]
    cutoff_word <- cutoff[2]
    
    feedback <- c('\n\n\\ \n',
                  '<div class="feedback">',
                  '<!-- THE GOOD -->',
                  '\n\n',
                  '<!-- THE BAD -->',
                  '\n\n',
                  '<!-- RECOMMENDATIONS -->',
                  '\n\n',
                  '</div>\n\n')
    if (is.na(cutoff)[1]) {
      out <- c(ff, feedback)
    } else {
      words <- words[grep(paste0("^", cutoff_line, "_"), names(words))]
      words_sane <- gsub(" ", "", gsub("([\\(\\[\\{\\*\\$\\.\\^\\#\\)\\}]|\\])", "\\\\ \\1", words))
      ptrn <- paste(words_sane[1:cutoff_word], collapse = "[[:punct:] ]*?")
      # introduce line break after limit has been reached
      ff[cutoff_line] <- sub(paste0("(", ptrn, ")"), "\\1\n", ff[cutoff_line])
      # split by \n again
      ff <- unlist(strsplit(paste(ff, collapse = "\n"), "\n"))
      out <- c(ff[1:cutoff_line], insert, ff[(cutoff_line + 1):length(ff)], feedback)
    }
    # out <- gsub("candidate_number\\s*<-", "candidate_number <<-", out)
    
    writeLines(out, out_file)
    
    rmarkdown::render(input = out_file,
                      output_format = rmarkdown::html_document(toc = F),
                      envir = new.env())
    
    out <- readLines(sub("[Rr]md$", "html", out_file))
    if (include_results) {
      if (is_green) {
        chisq_res <- list()
        for (i in c("car", "cleaner", "dishwasher")) {
          chisq_res[i] <- paste0(
            "<p><strong>", i, ": </strong></p>",
            "<p>&chi;^2^(",
            results_obj[[i]]$parameter,
            ") = ", round(results_obj[[i]]$statistic, 2),
            ", <em>p</em> ", ifelse(results_obj[[i]]$p.value < .001, "&lt; ", "= "),
            pround(results_obj[[i]]$p.value), "</p>")
        }
        test_res <- paste(unlist(chisq_res), collapse = "<br>")
      } else if (is_red) {
        test_res <- paste0(
          "<p><em>t</em>(",
          round(results_obj$result$parameter, 2),
          ") = ", round(results_obj$result$statistic, 2),
          ", <em>p</em> ", ifelse(results_obj$result$p.value < .001, "&lt; ", "= "),
          pround(results_obj$resul$p.value), "</p>")
      }
      
      res <- c(
        "<div class=\"sidebar1\">",
        "<div class=\"results-container\">",
        "<div class=\"results\">",
        "<div class=\"results-item item1\">",
        "<p><strong><em>N</em> removed NAs:</strong> ", results_obj$rem_age_na, "</p>",
        "<p><strong><em>N</em> removed &lt;18:</strong> ", results_obj$rem_age_young, "</p>",
        "<p><strong><em>N</em> total clean:</strong> ", results_obj$n_clean, "</p>",
        "<p><strong><em>N</em> total clean:</strong> ", results_obj$n_clean, "</p>",
        "</div>",
        "<div class=\"results-item item2\">",
        if (is_green) 
          kableExtra::kable_styling(
            kableExtra::kable(results_obj$gen_desc[ , c(1:3,7)]),
            full_width = F,
            position = "left"
            ), "",
        kableExtra::kable_styling(
          kableExtra::kable(results_obj$cond_desc[ , c(1:3,7)]),
          full_width = F,
          position = "left"
          ),
        "</div>",
        "<div class=\"results-item item3\">",
        "<p><strong>Main analysis results</strong></p>",
        test_res,
        "</div>",
        "</div>",
        "</div>",
        "</div>"
      )
    
      start_row <- grep("<div class=\"container-fluid main-container\">", out)
      end_row <- rev(grep("</div>", out))[1]
      out <- c(
        out[1:start_row],
        "<div class=\"col-md-12\">",
        "<div class=\"inner\">",
        res,
        "<div class=\"main-content\">", 
        out[(start_row+1):end_row],
        "<div class=\"sidebar2\" style=\"display: none\">",
        "<div class=\"mark-container\">",
        "<div class=\"mark\">",
        "</div>",
        "</div>",
        "</div>",
        "</div>",
        "</div>",
        out[(end_row+1):length(out)]
      )
      
      writeLines(out, sub("[Rr]md$", "html", out_file))
    }
    
  } else if (feedback) {
    
    good_text <- c(
      "\n\\ \n\n",
      readLines(paste0(path.package("teachR"), "/fb_gd.txt")),
      ""
    )
    
    bad_text <- c(
      "\n\\ \n\n",
      readLines(paste0(path.package("teachR"), "/fb_bd.txt")),
      ""
    )
    
    recom_text <- c(
      "\n\\ \n\n",
      readLines(paste0(path.package("teachR"), "/fb_rec.txt")),
      ""
    )
    
    # code chunk that defines --theme-col for CSS
    color_chunk <-  c("```{r, echo=F, include=T, eval=T, results='asis'}",
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
    
    
    if (missing(mark)) {
      warning("mark= is missing: Final mark will not be displayed.")
    } else {
      ff <- sub("<div class=\"sidebar2\" style=\"display: none\">",
                "<div class=\"sidebar2\">", ff)
      ff <- sub("<div class=\"mark\">",
                paste0("<div class=\"mark\"><p>", mark, "</p>"), ff)
    }
    
    ff[grep("<!-- THE GOOD", ff)] <- paste(good_text, collapse = "\n")
    ff[grep("<!-- THE BAD", ff)] <- paste(bad_text, collapse = "\n")
    ff[grep("<!-- RECOMMEND", ff)] <- paste(recom_text, collapse = "\n")
    
    writeLines(ff, out_file)
    
    rmarkdown::render(input = out_file,
                      output_format = rmarkdown::html_document(
      toc = F,
      includes = rmarkdown::includes(after_body = paste0(path.package("teachR"), "/feedback.css")))
    )
  } 
  return(T)
}

