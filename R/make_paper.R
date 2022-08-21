
#' Create formatted exam paper in Word, PDF, or HTML
#'
#' Requires a tibble of exam items, such as the one returned by \code{\link{question.bank}()} and can take one or more .Rmd files with code chunks with code required to generate item data/tables/figures etc.
#'
#' @param items. Data frame of items for a given exam paper. It is recommended to first generate a question bank with \code{\link{question.bank}()} and then select individual paper items.
#' @param code_file \code{character}. Path to .Rmd file(s) with code chunks.
#' @param output_format \code{character}. Output format for paper: one of \code{"word", "pdf", "html"}.
#' @param out_file_name \code{character}. Name of the output file without extension.
#' @param yaml_header, @param title_page \code{character}. Vectors including lines for title page and YAML front matter. It is recommended to generate these using a function similar to \code{\link{sussex.boilerplate}()}.
#' @param ref_file \code{character}. Path to a reference docx file with formatting and headers. One can be found in the package folder.
#' @param pre_preocessor Pre-processor function to include \code{title_page} in the final document. Currently uses \code{teachR:::sus_pre_processor()} that gets called by \code{teachR:::exam_paper_document()} passed to \code{rmarkdown::render()}. Using alternatives not yet implemented.
#' @param randomise_qs \code{logical}. Should order of items be randomised (multi-items that share a single stem/paradigm will be appended to the end of the exam in either case). \code{TRUE} by default.
#' @param randomise_response_opts \code{logical}. Should order of response options be randomised. \code{TRUE} by default.
#' @param sample \code{TRUE} to generate a sample paper with correct responses at the end of the paper. \code{FALSE} by default.
#' @details Function requires a .css and .js files for correct formatting of lab sheets/handouts. These files sit on the stats website in the [root]/sheet_files folder and the path is hard-coded into the function. Look for css and js objects in function body.
#' @return Function returns a message if successful and, if \code{sample=FALSE}, produces two versions of paper; one with correct answers (..._WITH_KEY in file name) and one without. A scoring key CSV file gets also generated. If \code{sample=TRUE},, only the sample paper gets generated.
#' @importFrom tibble tibble
#' @examples
#' paper_items <- teachR::question.bank("exam_items.Rmd", authors = "MV")
#' make.paper(paper_items, "exam_code.Rmd", out_file_name = "AnD_MCQ_paper_19_20",
#'            yaml_header = boilerplate$yaml, title_page = boiler_plate$title_page,
#'            ref_file = file.path(path.package("teachR"), "mcq_format_ref.docx"), pre_processor = teachR:::sus_pre_processor)
#' @export
#' 

make.paper <- function(items, code_file = NULL, output_format = "word", out_file_name, yaml_header,
                       title_page, ref_file = NULL, pre_preocessor = NULL, randomise_qs = TRUE,
                       randomise_response_opts = TRUE, sample = FALSE) {
  
  if (!output_format %in% c("word", "pdf", "html"))
    stop('output_format= must be one of c("word", "pdf", "html")', call. = F)
  
  if (!all(file.exists(code_file)))
    stop("Can't find all the files in code_file=.", call. = F)
  
  if (is.null(ref_file)) {
    ref_file <- file.path(path.package("teachR"), "mcq_format_ref.docx")
  } else {
    if (!file.exists(ref_file)) {
      stop("Can't find ref_file=.", call. = F)
    } else ref_file <- normalizePath(ref_file)
  }
  
  out_file_name <- sub("\\..*$", "", out_file_name)
  out_rmd <- normalizePath(paste0(out_file_name, ifelse(sample, ".Rmd", "_WITH_KEY.Rmd")), mustWork = F)
  output_con <- file(out_rmd, "w")
  on.exit(close(output_con))
  
  key_file <-  normalizePath(paste0(out_file_name, "_scoring_key.csv"), mustWork = F)
  rendered_file <- sub("Rmd$", paste0(ifelse(output_format == "word", "docx", output_format)), out_rmd)
  rendered_no_key <- sub("_WITH_KEY", "", rendered_file)
  if (output_format %in% c("word", "pdf")) {
    rendered_con <- try(
      suppressWarnings(file(rendered_file, "w")),
      silent = T)
    if (class(rendered_con)[1] == "try-error") stop(paste(rendered_file, "is open. Please close it and try again"), call. = F)
    close(rendered_con)
    if (!sample) {
      no_key_con <- try(
        suppressWarnings(file(rendered_no_key, "w")),
        silent = T)
      if (class(no_key_con)[1] == "try-error") stop(paste(rendered_no_key, "is open. Please close it and try again"), call. = F)
      close(no_key_con)
      key_con <- try(
        suppressWarnings(file(key_file, "w")),
        silent = T)
      if (class(key_con)[1] == "try-error") stop(paste(key_file, "is open. Please close it and try again"), call. = F)
      close(key_con)
    }
  }
  
  ### set options
  # save chunk default options
  default_chunk_opts <- knitr::opts_chunk$get()[c("echo", "error", "message", "warning", "eval", "comment")]
  options(knitr.kable.NA = '',
          knitr.table.format = "pandoc")
  knitr::opts_chunk$set(echo=F, error=T, include=T, message=F, warning=F, comment=NA)
  
  pretty_inline_num <-  c(
    "```{r setup}",
    if (output_format == 'pdf') {
      c(
        "hook_inline = knitr::knit_hooks$get('inline')",
        "  knitr::knit_hooks$set(",
        "    inline = function(x) {",
        "      res = hook_inline(x)",
        "      if (is.numeric(x)) gsub('(-.*)', '$\\1$', prettyNum(format(x, scientific=FALSE), big.mark=',')) else res",
        "    }",
        "  )"
      )
    } else {
      c(
        "hook_inline = knitr::knit_hooks$get('inline')",
        "  knitr::knit_hooks$set(",
        "    inline = function(x) {",
        "      res = hook_inline(x)",
        "      if (is.numeric(x)) gsub('-', '&minus;', prettyNum(format(x, scientific=FALSE), big.mark=',')) else res",
        "    }",
        "  )"
      )
    },
    "```", ""
  )
  
  paper_qu <- items[is.na(items$group), ]
  
  # randomise
  if (randomise_qs) paper_qu <- paper_qu[sample(nrow(paper_qu)), ]
  
  
  paper <- c(yaml_header, "", pretty_inline_num)
  
  # insert code code files before questions
  for (i in seq_along(code_file)) {
    if (file.exists(code_file[i]))
      paper <- c(paper, paste0("<!-- ", code_file[i], " code starts here -->"),
                 readLines(code_file[i]), "")
  }
  
  # format sinlge items
  scoring_key <- tibble(q_num = rep(NA_character_, nrow(items)), correct = rep(NA_character_, nrow(items)))
  for (i in 1:nrow(paper_qu)) {
    item <- teachR:::format_q(paper_qu[i, ], i, key = !sample, randomise_opts = randomise_response_opts)
    paper <- c(paper, "", item$item)
    scoring_key[i, ] <- rbind(item$key)
  }
  
  # format multi-items
  group_stems <- unique(items$group_stem[!is.na(items$group)])
  for (g in seq_along(group_stems)) {
    gr_stem <- unlist(strsplit(group_stems[g], "\\n", fixed = T))
    chunk_lims <- grep("```", gr_stem) + c(-1, 1)
    # insert empty lines outside of code chunks
    if (length(chunk_lims) > 0) {
      ind <- c(2:chunk_lims[1], chunk_lims[2]:length(gr_stem))
      gr_stem[ind] <- sub("^$", "\\\\n\\\\n\\\\ \\\\n\\\\n", gr_stem[ind])
      gr_stem <- unlist(strsplit(gr_stem, "\\n", fixed = T))
    } else {
      gr_stem <- sub("^$", "\\\\n\\\\n\\\\ \\\\n\\\\n", gr_stem)
      gr_stem <- unlist(strsplit(gr_stem, "\\n", fixed = T))
    }
    paper <- c(paper, "", "\\ ", "", gr_stem, "", "\\ ", "")
    temp <- items[which(items$group == g), ]
    for (q in 1:nrow(temp)) {
      i <- i + 1
      item <- teachR:::format_q(temp[q, ], i, key = !sample, randomise_opts = randomise_response_opts)
      paper <- c(paper, item$item)
      scoring_key[i, ] <- rbind(item$key)
    }
  }
  
  if (sample) {
    paper <- c(
      paper,
      "##### \\ ", # page_break
      "",
      "### Correct answers",
      "", "\\ ", "", "\\ ", "", "\\ ", "",
      "```{r}",
      "knitr::kable(scoring_key, col.names = c('Question', 'Correct'), align = 'cc')",
      "```",
      ""
    )
  } else {
    # produce scoring key CSV
    write.csv(scoring_key, key_file)
  }
  
  writeLines(paper, output_con)
  close(output_con)
  
  title_page <<- title_page
  on.exit(rm(title_page))
  rmarkdown::render(out_rmd,
                    output_format = teachR:::exam_paper_document(
                      output_format = output_format,
                      reference_docx = ref_file,
                      # df_print = "kable",
                      fig_height = 4,
                      fig_width = 5))
  
  paper <- gsub(" **(correct)**", "", paper, fixed = T)
  
  if (!sample) {
    rendered_no_key <- sub("\\..+$", ".Rmd", rendered_no_key)
    no_key_rmd <- file(rendered_no_key)
    writeLines(paper, no_key_rmd)
    close(no_key_rmd)
    on.exit(file.remove(rendered_no_key))
    
    rmarkdown::render(rendered_no_key,
                      output_format = teachR:::exam_paper_document(
                        output_format = output_format,
                        reference_docx = ref_file,
                        # df_print = "kable",
                        fig_height = 4,
                        fig_width = 5))
  }
  
  
  # restore default chunk opts
  knitr::opts_chunk$set(default_chunk_opts)
  options(knitr.kable.NA = 'NA',
          knitr.table.format = "html")
  
  bottom_dashes <- options()$width
  top_dashes <- paste(rep("-", floor((bottom_dashes - 10) /2)), collapse = "")
  bottom_dashes <- paste(rep("-", bottom_dashes), collapse = "")
  out_msg <- if (sample) {
    paste0("\n", top_dashes, " SUCCESS! ", top_dashes,
           "\nSample exam paper with correct answers created in:\n\n\t",
           rendered_file,
           "\n\nRmd file also available in the folder.\n\n",
           bottom_dashes)
  } else {
    paste0("\n", top_dashes, " SUCCESS! ", top_dashes,
           "\nExam paper with and without key created in:\n\n\t",
           rendered_file, "\n\t", sub("_WITH_KEY", "", rendered_file),
           "\n\nRmd file with key and a _scoring_key.csv also available in the folder.\n\n",
           bottom_dashes)
  }
  return(message(out_msg))
}
