
#' Convert .Rmd to course-formatted HTML document or R Notebook
#'
#' Lab sheets and lecture materials should be written in R Markdown with title, subtitle, and author in YAML header. \code{make.sheets()} converts a .Rmd file into a HTML/R Notebook document applying the theme for given course.
#'
#' @param file \code{character}. Path to .Rmd file to convert.
#' @param course \code{character}. Course the sheet is for: one of \code{"dapR_1", "daprR_2", "dapR_3", "usmr", "msmr", "other"}.
#' @param solution \code{logical}. Should solutions to taks be rendered? \code{FALSE} by default.
#' @param handout \code{logical}. \code{TRUE} adds "Click for slides" link under author. To be used only for slide handout HTML files. \code{FALSE} by default.
#' @param tasks_to_headings \code{logical}. Should individual tasks and subtaks be rendered as level 2 and 3 (respectively) headings? \code{TRUE} by default.
#' @param notes \code{logical}. If \code{handout=TRUE}, then if \code{TRUE} a writable box for notes will appear at the bottom of each slide.
#' @param ntb \code{logical}. \code{TRUE} to render document as R Notebook. \code{FALSE} by default.
#' @param color \code{character}. Either a single valid colour (hex code or any of the values in \code{colours()}) or any valid value of the \code{course=} argument. If provided, it will be used to set colour scheme instead of \code{course=}.
#' @param toc \code{logical}. Should table of content be included.
#' @param toc_depth \code{numeric}. Depth of headers to include in table of contents. 3 by default.
#' @param toc_float \code{TRUE} to float the table of contents to the left of the main document content. Rather than TRUE you may also pass a list of options that control the behavior of the floating table of contents. For more details, see \code{\link[rmarkdown]{html_document}}.
#' @param fig_width,fig_depth \code{numeric}. Default width and height (in inches) for figures.
#' @param highlight \code{character}. Syntax highlighting style. See \code{\link[rmarkdown]{html_document}}.
#' @param keep_temp_Rmd \code{logical}. Should temporary Rmd file be kept? Useful for post-hoc edits and debugging. 
#' @param ... Other arguments to be passed to \code{rmarkdown:html_document} or \code{rmarkdown:html_notebook}.
#' @details Function requires a .css and .js files for correct formatting of lab sheets/handouts. These files sit on the stats website in the [root]/sheet_files folder and the path is hard-coded into the function. Look for css and js objects in function body.
#' @return \code{TRUE} if output .html file was successfully created.
#' @seealso \code{\link{handout}()}
#' @examples
#' make.sheet("C:/Users/mvalasek/slides/dapR_1_handout_demo.Rmd", "dapR_1")


make.sheet <- function(file, course, solution = FALSE, handout = FALSE, tasks_to_headings = TRUE, notes = TRUE, ntb = FALSE,
                       color = NULL, toc = TRUE, toc_depth = 3, toc_float = TRUE, fig_width = 5, fig_height = 3.5,
                       highlight = "tango", colour = color, keep_temp_Rmd = FALSE, ...) {
  if (!file.exists(file)) stop("The file does not exist.")
  if (!grepl("\\.[rR]md$", file)) stop("file= needs to be an .Rmd file.")
  if (!tolower(course) %in% c("dapr_1", "dapr_2", "dapr_3", "usmr", "msmr", "and", "ad", "adata", "fun_ind", "other"))
    stop("course= must be one of c(\"dapr_1\", \"dapr_2\", \"dapr_3\", \"usmr\", \"msmr\", \"and\", \"fun_ind\", \"other\").")
  if (course %in% c("ad", "adata")) course <- "and"
  if (!is.null(colour)) color <- colour
  if (!is.null(color)) {
    if (length(color) != 1) stop("Please provide exactly one value to color=.")
    if (color %in% c("dapr_1", "dapr_2", "dapr_3", "usmr", "msmr", "and", "ad", "adata", "other")) {
      course <- color
      color <- NULL
    } else {
    if (!grepl("^#[[:xdigit:]]{3,6}$", color) && !(color %in% colours()))
      stop("Invalid color provided.")
    }
  }
  course <- gsub("r_", "R_", tolower(course))
  
  color_list <- list(
    dapR_1 = "#6bcded",
    dapR_2 = "#b38ed2",
    dapR_3 =  "#85a6ea",
    usmr = "#eda46f",
    msmr = "#d8d768",
    and = "#b38ed2",
    other = "#77bd9d",
    fun_ind = "#013035" # Canvas colour
  )
  
  theme_col <- ifelse(is.null(color), color_list[[course]], color)
  
  sol <- function(x) {
    if (solution) {
      paste0('<div class="solBody">
    <p class="solHeader>', x, '</p>')
    } else paste('<br><br><br><br><br><br>
<div style="display:none;">')
  }
  
  oldwd <- getwd()
  file <- gsub("\\", "/", normalizePath(file, "/", T), fixed = T)
  outwd <- gsub("(.*)/.*$", "\\1", file)
  file <- gsub(".*/(.*?)", "\\1", file)
  setwd(outwd)
  on.exit(setwd(oldwd))
  
  x <- readLines(file)
  x <- gsub("^(\\s*?)>\\s*?-", "\\1-", x) # get rid of incremental bulletpoints if used ( > - ...)
  x <- gsub("#\\s*?inc\\s*?$", "", x) # get rid of #inc
  yaml <- grep("---", x)
  title <- grep("^\\s*?title:", x[1:yaml[2]], value = T)[1]
  if (solution) title <- sub("\"$", "<br>...with solutions\"", title)
  subtitle <- grep("^\\s*?subtitle:", x[1:yaml[2]], value = T)[1]
  author <- grep("^\\s*?author:", x[1:yaml[2]], value = T)[1]
  x <- x[-(1:yaml[2])]
  x <- gsub("^\\s*(#+.*?)\\s*$", "\\1", x) # remove leading/trailing white spaces from headings
  if (handout) {
    x <- gsub("^#\\s*([^#])", "## \\1", x) # turn # in to ##
    x[(1:length(x))[duplicated(x)]] <-
      gsub("^#+.*", "", x[(1:length(x))[duplicated(x)]]) # get rid of duplicated headings
  } else {
    ## show/hide <!-- solution or write-up
    begin_comment2 <- grep("<!--", x)
    end_comment <- grep("-->", x)
    
    begin_comment <- setdiff(begin_comment2, end_comment)
    end_comment <- setdiff(end_comment, begin_comment2)
    
    if (length(begin_comment) != length(end_comment)) stop("There seems to be something wrong with the way you used '<!--' comments to designate solution body text.")
    
    ### answer toggle formatting
    toggle <- grep("<!--\\s*toggle", x[begin_comment])
    begin_toggle <- begin_comment[toggle]
    end_toggle <- end_comment[toggle]
    
    x[begin_toggle] <- "<details><summary>Solution</summary>"
    x[end_toggle] <- gsub("\\s*-->\\s*$", "</details>", x[end_toggle])
    
    sol <- grep("<!--\\s*solution|<!--\\s*write.?up", x[begin_comment])
    begin_comment <- begin_comment[sol]
    end_comment <- end_comment[sol]
    
    for (i in seq_along(begin_comment)) {
      if (solution) {
        x[begin_comment[i]] <- ifelse(
          grepl("<!--\\s*write.?up", x[begin_comment[i]]),
          '<div class="writeUp">',
          '<div class="solText">')
        strip_end <- sub("\\s*-->\\s*", "", x[end_comment[i]])
        x[end_comment[i]] <- "</div>"
        if (strip_end != "") x[end_comment[i]] <- paste0(strip_end, "\n", x[end_comment[i]])
      } else if (grepl("<!--\\s*write.?up", x[begin_comment[i]])){
        height_text <- ceiling(ceiling(nchar(x[(begin_comment[i] + 1):end_comment[i]]) / 82) * 1.3)
        x[begin_comment[i]] <- paste0(
          '<div class="noteBox writeUp" contenteditable="true"></div>\n<!--')
      }
    }
  }

  h <- c(
    "---",
    title, subtitle, author,
    if (handout) paste0("date: \"[Click for slides](", sub("\\.[Rr]md$", "_slides.html", file), ")\""),
    "---",
    " ",
    "```{r, echo=F, results='asis'}",
    "cat(\"",
    "<style>",
      ":root {",
    paste0("--theme-col: ", paste(col2rgb(theme_col), collapse=", "), ";"),
    paste0("--font-col: ", paste(col2rgb(ifelse(course == "fun_ind", theme_col, "#434b75")), collapse=", "), ";"),
      "}",
    "</style>",
    "\")",
    "```",
    "",
    "```{r, rsetup, include=F}",
    "knitr::opts_chunk$set(comment=NULL, collapse=T, strip.white=F, echo=T,\n    message = F, warning = F, prompt = F, comment = NA, split = F, toggle = F, fig.align = \"center\")",
    "knitr::knit_hooks$set(sol = function(before, options, envir){",
    "  if (before){",
    "    paste0('<div class=\"solution\">')",
    "  } else {",
    "    paste0('</div>')",
    "  }",
    "},",
    "                     toggle = function(before, options, envir){",
    "  if (options$toggle) {",
    "    if (before){",
    "      paste0('<details><summary>Solution</summary>')",
    "    } else {",
    "      paste0('</details>')",
    "    }",
    "  }",
    "})",
    "hook_inline = knitr::knit_hooks$get('inline')",
    "knitr::knit_hooks$set(",
    "  inline = function(x) {",
    "    res = hook_inline(x)",
    "    if (is.numeric(x)) prettyNum(format(x, scientific=FALSE), big.mark=',') else res",
    "  })",
    "```",
    " ",
    "```{r task_fun, echo=FALSE}",
    "tsk <- s_tsk <- 1 # Task counter",
    readLines(paste0(path.package("teachR"), "/task.txt")),
    "```",
    " ",
    "```{r plot_theme, include=FALSE}",
    "library(ggplot2)",
    "bg_col <- '#fdfdfd'",
    "default_col <- '#434b75'",
    paste("theme_col <-", theme_col),
    "complement_col <- colortools::complementary(theme_col, F)[2]",
    "point_col <- paste0(default_col, '88')",
    " ",
    "my_theme <- cowplot::theme_cowplot() +",
    "  theme(line = element_line(colour = default_col),",
    "        plot.background = element_rect(fill = bg_col),",
    "        panel.background = element_rect(fill = bg_col),",
    "        text = element_text(colour = default_col),",
    "        title = element_text(colour = default_col),",
    "        axis.line = element_line(colour = default_col),",
    "        axis.ticks = element_line(colour = default_col),",
    "        axis.text = element_text(colour = default_col),",
    "        axis.title = element_text(colour = default_col),",
    "        strip.background = element_rect(fill = default_col, colour = default_col),",
    "        strip.text = element_text(colour = bg_col)",
    "  )",
    "update_geom_defaults(\"bar\", list(fill = bg_col, colour = default_col))",
    "ggplot2::theme_set(my_theme)",
    "```",
    " "
  )
  

  
  h <- as.vector(na.omit(h))
  x <- c(h, x)
  if (handout) x <- c(
    x,
    " ", "\ ", " ", "\ ", " ", "<div class=\"warn\">",
    "**Don't forget to save this page on your computer, otherwise you will lose all notes!**",
    "</div>", " ", "\ ", " ", " ", "\ ", " ")
  
  css <- "https://mivalek.github.io/sheet_files/sheets.css"
  js <- "https://mivalek.github.io/sheet_files/sheets.js"
  
  temp_rmd <- sub("\\.[Rr]md$", "_temp.Rmd", file)
  writeLines(x, temp_rmd)
  if (!keep_temp_Rmd) on.exit(file.remove(temp_rmd), add = T, after = F)

  out_html <- sub("\\.[Rr]md$", ifelse(solution, "_sol.html", ".html"), file)
  
  

  if (ntb) {
    render(
      input = temp_rmd,
      output_format = html_notebook(
        toc = toc, toc_depth = toc_depth, toc_float = toc_float,
        fig_width = fig_width, fig_height = fig_height, highlight = highlight,
        includes = includes(after_body = c(css, js)),# md_extensions = "+fenced_code_attributes",
        ...
      ),
      intermediates_dir = tempdir())
  }
  else {
    render(
      input = temp_rmd,
      output_format = html_document(
        toc = toc, toc_depth = toc_depth, toc_float = toc_float,
        fig_width = fig_width, fig_height = fig_height, highlight = highlight,
        includes = includes(after_body = c(css, js)),# md_extensions = "+fenced_code_attributes",
        ...
    ),
    intermediates_dir = tempdir())
  }
  file.rename(sub("\\.Rmd$", ".html", temp_rmd), out_html)
  
  if (handout && notes) {
    x <- readLines(out_html)
    ind <- grep("^\\s*<h[2-9]>", x)[-1]
    x[ind] <- gsub("^\\s*(<h[2-9]>.*)",
                   "<div class=\"noteBox\" contenteditable=\"true\"></div>\n\\1", x[ind])
    writeLines(x, out_html)
  }
}
