
#' Convert .Rmd to reveal.js slides
#'
#' Lecture slides should be written in R Markdown with title, subtitle, and author in YAML header and saved on a local drive (due to Pandoc's issues with network drives). slidify() converts lecture .Rmd file into a reveal.js html presentation applying the theme for given course.
#'
#' @param file character. Path to .Rmd file to convert.
#' @param course character. Course the lecture is for: one of "dapR_1", "daprR_2", "dapR_3", "usmr", "msmr", "other".
#' @param header_text character. Text to be displayed in the top right corner of slides. Course-specific text by default.
#' @param incremental logical. TRUE to render slide bullets incrementally on click. FALSE by default.
#' @return Function does not return anything but outputs a .html file called [file]_slides.html and a corresponding folder with figures.
#' @param fig_width,fig_height numeric. numeric. Default width and height (in inches) for figures.
#' @param transition,background_transition character. Slide transition animation. "fade" by default. See reveal.js documentation for more options.
#' @param plugins character. which plugind to include. By default c("notes", "search", "chalkboard").
#' @details Function requires a .css and .js files for correct formatting of lab sheets/handouts. These files sit on the stats website in the [root]/slides_files folder and the path is hard-coded into the function. Look for css and js objects in function body.
#' @examples
#' # .Rmd must be on a local drive!
#' slidify("C:/Users/mvalasek/slides/dapR_1_handout_demo.Rmd", "dapR_1")


slidify <- function(file, course, header_text = "default", meta_folder, incremental = FALSE,
                    fig_width = 5, fig_height = 3.5,
                    transition = "fade", background_transition = transition,
                    plugins = c("notes", "search", "chalkboard")) {
  if (!file.exists(file)) stop("The file does not exist.")
  if (!tolower(course) %in% c("dapr_1", "dapr_2", "dapr_3", "usmr", "msmr", "other"))
    stop("course= must be one of c(\"dapr_1\", \"dapr_2\", \"dapr_3\", \"usmr\", \"msmr\", \"other\").")
  course <- gsub("r_", "R_", tolower(course))
  x <- readLines(file)
  inc_lines <- grep("#\\s*?inc\\s*?$", x) # identify lines with #inc
  dist_mat <- as.matrix(dist(inc_lines)) # distances between #inc lines
  distance <- dist_mat[row(dist_mat) == col(dist_mat) + 1]
  gaps <- sort(c(inc_lines[c(T, distance != 1)] - 1, # where to insert empty lines
                 inc_lines[c(distance != 1, T)]))
  y <- c(x, rep(" ", length(gaps))) # put all empty lines at the end
  index <- c(seq_along(x), gaps + .5) # gaps get half-rank
  x <- y[order(index)] # half-ranks get inserted in the right places
  x <- gsub("^(\\s*?)(.*?)#\\s*?inc", "\\1> \\2", x) # make lines with #inc render incrementally
  yaml <- grep("---", x)
  title <- grep("^\\s*?title:", x, value = T)
  subtitle <- grep("^\\s*?subtitle:", x, value = T)
  author <- grep("^\\s*?author:", x, value = T)
  x <- x[-(1:yaml[2])]
  x <- gsub("^#[^#]", "## ", x)
  h <- c(
    "---",
    title, subtitle, author,
    paste0("date: \"[Click for handout](",
           gsub("(.*)[rR]md", "./\\1html",
                rev(unlist(strsplit(file, .Platform$file.sep)))[1]), ")\""),
    "---",
    " ",
    "```{r, echo=F, results='asis'}",
    "cat(\"",
    "<style>",
      ":root {",
        paste0("--theme-col: var(--", course, "-col1);"),
        paste0("--hover-col: var(--", course, "-col2);"),
      "}",
    "</style>",
    "\")",
    "```",
    "",
    "```{r, revealjs-setup, include=F}",
    "knitr::opts_chunk$set(comment=NULL, collapse=T, strip.white=F,",
    "                      fig.height = 3.5,",
    "                      fig.width = 5)",
    "def.chunk.hook  <- knitr::knit_hooks$get(\"chunk\")",
    "knitr::knit_hooks$set(chunk = function(x, options) {",
    "  x <- def.chunk.hook(x, options)",
    "  ifelse(options$size != \"normalsize\", paste0(\"\\\\\", options$size,\"\\n\\n\", x, \"\\n\\n \\\\normalsize\"), x)",
    "})",
    "```"
  )
  css <- "https://mivalek.github.io/slides_files/css/slides.css"
  js <- "https://mivalek.github.io/slides_files/js/slides.js"
  if (header_text == "default") {
  header_text <-
    if (grepl("dapR", course)) {
      paste0(sub("_", "<strong>", course), "</strong>")
    } else if (course == "usmr") {
      "Univariate statistics<strong>in R</strong>"
    } else if (course == "msmr") {
      "Multivariate statistics<strong>in R</strong>"
    } else ""
  }
  header_file <- gsub("\\.[Rr]md", "_header.html", file)
  writeLines(
    paste0("<div class=\"banner\"><div class = \"",
           ifelse(course %in% c("usmr", "msmr"), "header msc", "header"),
           "\"><a href=\"/\">", header_text, "</a></div></div>"),
    header_file)
  x <- c(h, x)
  out_file <- gsub("\\.[Rr]md", "_temp.Rmd", file)
  writeLines(x, out_file)

  pres_file <- gsub("\\.[Rr]md$", "_slides.html", file)
  render(
    input = out_file,
    output_format = revealjs::revealjs_presentation(
      fig_width = fig_width, fig_height = fig_height, self_contained = F,
      transition = transition, background_transition = background_transition, incremental = incremental,
      reveal_options = list(
        slideNumber = "c/t", controls = F, width = 1000, height = 750, margin = 0),
      reveal_plugins = plugins,
      highlight = "tango", includes = includes(before_body = header_file, after_body = js),
      css = css
    ),
    output_file = pres_file)
  file.remove(out_file)
  file.remove(header_file)

  x <- readLines(pres_file)
  ind <- grep("_slides_files", x)
  ind <- ind[grep("img src", x[ind], invert = T)]
  x[ind] <-  gsub("^(\\s*?<link rel=\"stylesheet\" href=\").*?(slides_files.*)$", "\\1/\\2", x[ind])
  x[ind] <-  gsub("^(\\s*?<link href=\").*?(slides_files.*)$", "\\1/\\2", x[ind])
  x[ind] <-  gsub("^(\\s*?<script src=\").*?(slides_files.*)$", "\\1/\\2", x[ind])
  x[ind] <-  gsub("^(\\s*?\\{ src: ').*?(slides_files.*)$", "\\1/\\2", x[ind])

  writeLines(x, pres_file)
  files_dirs <- list.dirs(sub("\\.html", "_files", pres_file), recursive = F)
  for (i in grep("figure-reveal", files_dirs, invert = T, value = T)) unlink(i, recursive = T)
}
