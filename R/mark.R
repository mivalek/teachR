
#' Mark coursework assignment Rmd
#'
#' For Rmd submissions of reports and other coursework. Function checks word count and inserts space for feedback.
#'
#' @param file \code{character}. Either path to .Rmd file to mark or URL from which to read the file.
#' @param file_name \code{character}. If \code{file=} is a URL, name of file to write to.
#' @param study \code{character}. Which study to get correct results for? Currently either \code{"red"} or \code{"green"}.
#' @param mark \code{numeric}. Total mark/grade for given assignment.
#' @param rubric_grades. Mark for each rubric criterion. Can be a factor with levels sorted from lowest mark to highest mark. See \code{rubric} and \code{include_rubric_desc} for more info.
#' @param rubric A named list containing the rubric criteria (c1, c2, etc). Each criterion should be a list with \code{name}, \code{col}, and \code{text} elements containing the name, colour, and description of the criterion, respectively.
#' @param include_rubric_desc \code{logical}. If \code{TRUE} and if \code{rubric_grades} is a factor, grade descriptors for each marking criterion will be included in sidebar boxes.
#' @param feedback \code{logical}. Should feedback box with marker's comments appear at the bottom of document? \code{FALSE} by default.
#' @param count_words \code{logical}. \code{TRUE} counts body text words and if word count exceeds \code{limit}, inserts a warning line in HTML. \code{!feedback} by default.
#' @param limit \code{numeric}. Word count limit
#' @param rubric A named list containing the rubric criteria (c1, c2, etc). Each criterion should be a list with \code{name}, \code{col}, and \code{text} elements containing the name, colour, and description of the criterion, respectively.
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

mark <- function(file, file_name = file, study = NULL, mark = NULL, rubric_grades = NULL, rubric = NULL,
                 include_rubric_desc = F, feedback = F, count_words = !feedback, limit = 2000,
                 include_results = F, results_obj = NULL, remove_results = F, color = "#b38ed2") {
  
  
  if (grepl("^https://", file_name)) {
    stop("Plesae provide value to file_name= when reading file from URL.")
  } else if (!grepl("\\.rmd$", tolower(file_name)))
    file_name <- paste0(file_name, ".Rmd")
  
  if (include_results && is.null(results_obj))
    stop("Please provide results_obj= if include_results = TRUE.")
  
  if (include_rubric_desc && !is.factor(rubric_grades))
    stop("rubric_grades= must be a factor if include_rubric_desc=TRUE. See ?mark.")
  
  ff <- readLines(file)
  # remove author
  ff <- ff[!grepl("^author:", ff)]
  # comment out install.packages()
  ff <- gsub("(^\\s*install\\.packages\\(.*$)", "# \\1", ff)

  
  out_file <- ifelse(feedback, sub("\\.Rmd$", "_marked.Rmd", file_name), file_name)
  
  # word limit reached line
  insert <- '\n\\ \n\n<div>
  <p style="color:#cc0000;font-size:2em;text-align:center">*** WORD LIMIT REACHED ***</p>
  <p style="color:#cc0000;text-align:center">(Scroll down for feedback)</p>
  </div>\n\n\\ \n'
  
  if (count_words && !any(grepl(insert, ff, fixed = T))) {
    
    ### remove comments from code chunks
    chunk_limits <- matrix(grep("```", ff), ncol = 2, byrow= T)
    
    chunk_ind <- unlist(apply(chunk_limits, 1, function(x) seq(x[1], x[2])))
    ff[chunk_ind] <- gsub("^\\s*#.*$", "", ff[chunk_ind])
    
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
    
    fdbck <- c('</div>\n\n',
                  '\n\n\\ \n',
                  '<a name="feedback"></a>',
                  '<div class="feedback">',
                  '<!-- THE GOOD -->',
                  '\n\n',
                  '<!-- THE BAD -->',
                  '\n\n',
                  '<!-- RECOMMENDATIONS -->',
                  '\n\n')
    if (is.na(cutoff)[1]) {
      out <- c(ff, fdbck)
    } else {
      words <- words[grep(paste0("^", cutoff_line, "_"), names(words))]
      words_sane <- gsub(" ", "", gsub("([\\(\\[\\{\\*\\$\\.\\^\\#\\)\\}]|\\])", "\\\\ \\1", words))
      ptrn <- paste(words_sane[1:cutoff_word], collapse = "[[:punct:] ]*?")
      # introduce line break after limit has been reached
      ff[cutoff_line] <- sub(paste0("(", ptrn, ")"), "\\1\n", ff[cutoff_line])
      # split by \n again
      ff <- unlist(strsplit(paste(ff, collapse = "\n"), "\n"))
      out <- c(ff[1:cutoff_line], insert, ff[(cutoff_line + 1):length(ff)], fdbck)
    }
    # out <- gsub("candidate_number\\s*<-", "candidate_number <<-", out)
    
    writeLines(out, out_file)
    
    rmarkdown::render(input = out_file,
                      output_format = rmarkdown::html_document(
                        toc = F,
                        includes = rmarkdown::includes(
                          after_body = paste0(path.package("teachR"), "/feedback.css"))),
                      envir = new.env())
    
  } else if (feedback) {
    ### EDIT CHUNK OPTS
    # if chunk opts set to include=F
    if (any(grepl("opts_chunk\\$set\\(.*?include\\s*=\\s*F", ff))) {
      # change all include=T to results='markup'
      ff <- sub("(include\\s*=\\s*)TRUE|(include\\s*=\\s*)T", "\\1\\2T, results='markup'", ff)
      # identify chunks with BOTH results='markup' and results='asis' due to line above
      results_conflict <- base::intersect(grep("results='markup'", ff), grep("results\\s*=\\s*['\"]asis", ff))
      if (length(results_conflict) > 0) {
        # delete results='markup' from those
        ff[results_conflict] <- gsub(", results='markup'", "", ff[results_conflict])
      }
    }
    
    # change all echo=F to echo=T
    ff <- sub("(echo\\s*=\\s*)FALSE|(echo\\s*=\\s*)F", "\\1\\2T", ff)
    # change all include=F to include=T, results='hidden'
    ff <- sub("(include\\s*=\\s*)FALSE|(include\\s*=\\s*)F", "\\1\\2T, results='hide'", ff)
    
    
    # replace comment mkd with html tags
    ff <- gsub("<--([a-z0-9]*)\\s(.*?)-->", "<\\1>\\2</\\1>", ff)
    
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
    
    
    if (is.null(mark)) {
      warning("mark= is missing: Final mark will not be displayed.")
    }
    
    ff[grep("<!-- THE GOOD", ff)] <- paste(good_text, collapse = "\n")
    ff[grep("<!-- THE BAD", ff)] <- paste(bad_text, collapse = "\n")
    ff[grep("<!-- RECOMMEND", ff)] <- paste(recom_text, collapse = "\n")
    
    writeLines(ff, out_file)
    
    rmarkdown::render(input = out_file,
                      output_format = rmarkdown::html_document(
      toc = F,
      code_folding = "hide",
      includes = rmarkdown::includes(after_body = paste0(path.package("teachR"), "/feedback.css"))),
      envir = new.env()
    )
  }
  
  ### add HTML magic
  out <- readLines(sub("[Rr]md$", "html", out_file))
  if (include_results) {
    if (study == "green") {
      chisq_res <- list()
      for (i in c("car", "cleaner", "dishwasher")) {
        chisq_res[i] <- paste0(
          "<p><strong>", i, ": </strong><br>",
          "&chi;<sup>2</sup>(",
          results_obj[[i]]$parameter,
          ") = ", round(results_obj[[i]]$statistic, 2),
          ", <em>p</em> ", ifelse(results_obj[[i]]$p.value < .001, "&lt; ", "= "),
          pround(results_obj[[i]]$p.value), "</p>")
      }
      test_res <- paste(unlist(chisq_res), collapse = "")
    } else if (study == "red") {
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
      results_obj$cond_desc_tab,
      "</div>",
      if (study == "green") {
        c("<div class=\"results-item item3\">",
          results_obj$gen_desc_tab,
          "</div>",
          "<div class=\"results-item item4\">")
      } else if (study == "red") {
        "<div class=\"results-item item3\">"
      },
      "<p><strong>Main analysis results</strong></p>",
      test_res,
      "</div>",
      "</div>",
      "</div>",
      "</div>"
    )
    
    rub_desc <- rub_grades <- rubric_vars <- c()
    for (i in seq_along(names(rubric))) {
      # add bold prefix with first 3 letters of rubric criterion name to comments
      comment_prefix <- paste(unlist(strsplit(rubric[[i]]$name, ""))[1:3], collapse = "")
      out <- gsub(
        paste0("<c", i, ">"),
        paste0("<c", i, "><strong>", comment_prefix, ":</strong> "),
        out)
      
      for (j in names(rubric[[i]]))
        rubric_vars <- c(rubric_vars,
                         paste0("  --", names(rubric)[i], "-", j, ": ",
                                ifelse(j == "col", paste(col2rgb(rubric[[i]][[j]]), collapse = ", "),
                                       paste0("\"", rubric[[i]][[j]], "\"")), ";")
        )
      rub_grades <- c(rub_grades,
                      paste0("<details class=\"c", i, "\"><summary>", rubric_grades[i],
                             "</summary>",
                             # grade descriptor if include_rubric_desc, rubric text otherwise
                             ifelse(include_rubric_desc, rubric[[i]]$grade[rubric_grades[i]], rubric[[i]]$text),
                             "</details>"))
    }
    
    style_end <- grep("</style>", out)[1]
    out <- c(out[1:(style_end - 1)],
             ":root {",
             paste0("  --res-width: ",
                    if (study == "green") {
                      745
                    } else if (study == "red") {
                      605
                    }, "px;"),
             paste0("  --res-offset: ",
                    if (study == "green") {
                      280
                    } else if (study == "red") {
                      220
                    }, "px;"),
             paste0("  --theme-col: ", paste(col2rgb(color), collapse=", "), ";"),
             paste0("  --warn-col: var(--", ifelse(study == "green", "green", "red"), "-col);"),
             rubric_vars,
             "}",
             out[style_end:length(out)])
    start_row <- grep("<div class=\"container-fluid main-container\">", out)
    end_row <- rev(grep("</div>", out))[1]
    fb_row <- grep('<div class="feedback"', out, fixed = T) - 1
    out <- c(
      out[1:start_row],
      "<div class=\"col-md-12\">",
      "<div class=\"inner\">",
      res,
      "<div class=\"sidebar2\">",
      paste0("<div class=\"mark-container\"", ifelse(is.null(mark), "style=\"display: none\"", ""), ">"),
      paste0("<a href=\"#feedback\" class=\"mark\">", ifelse(is.null(mark), "NA", mark)),
      "</a>",
      "</div>",
      "</div>",
      "<div class=\"main-content\">", 
      out[(start_row+1):fb_row],
      if (feedback && !is.null(rubric_grades))
        c("<div class=\"grade\">", rub_grades, "</div>"),
      out[(fb_row + 1):end_row],
      "</div>",
      "</div>",
      "</div>",
      out[(end_row+1):length(out)]
    )
    
    if (feedback) {
      ### remove #s from comments
      code_limits <- cbind(grep('<pre class="r"><code>', out), grep('</code></pre>', out))
      
      code_ind <- unlist(apply(code_limits, 1, function(x) seq(x[1], x[2])))
      out[code_ind] <- gsub('^(<pre class="r"><code>)?#+\\s*([^<]*)(</code></pre>.*)?',
                            '\\1<span class="hljs-comment">\\2</span>\\3', out[code_ind])
    }
      
    writeLines(out, sub("[Rr]md$", "html", out_file))
  }
  
  return(T)
}

