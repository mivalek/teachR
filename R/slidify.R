
#' Convert .Rmd to reveal.js slides
#'
#' Lecture slides should be written in R Markdown with output set as html_document and saved on a local drive (due to Pandoc's issues with network drives). slidify() converts lecture .Rmd file into a reveal.js html presentation applying the theme for given course.
#'
#' @param file character. Path to .Rmd file to convert.
#' @param course character. Course the lecture is for: one of "dapR_1", "daprR_2", "dapR_3", "usmr", "msmr".
#' @param meta.folder character. path to folder with .css, .js, and _header.html files for generating reveal.js slides. By default a subdirectory of directory where file= is located.
#' @return Function does not return anything but outputs a .html file called [file]_slides.html and a corresponding folder with figures.
#' @examples
#' # .Rmd and meta.folde must be on a local drive!
#' slidify("C:/Users/mvalasek/slides/dapR_1_demo.Rmd", "dapR_1", "C:/Users/mvalasek/meta")


slidify <- function(file, course, meta_folder) {
  require(rmarkdown)
  require(revealjs)
  if (!file.exists(file)) stop("The file does not exist.")
  if (!tolower(course) %in% c("dapr_1", "dapr_2", "dapr_3", "usmr", "msmr"))
    stop("course= must be one of c(\"dapr_1\", \"dapr_2\", \"dapr_3\", \"usmr\", \"msmr\").")
  if (missing(meta_folder)) {
    meta_folder <- file.path(
      gsub(paste0("(.*)", .Platform$file.sep, ".*"), "\\1", file), "meta")
    if (!dir.exists(meta_folder)) {
      meta_folder <- file.path(getwd(), "meta")
      if (!dir.exists(meta_folder))
        stop("Cannot find the folder including .css, .js, and _header.html files.")
    }
  }
  course <- gsub("r_", "R_", tolower(course))
  x <- readLines(file)
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
  css <- file.path(meta_folder, "css", paste0(course, ".css"))
  js <- file.path(meta_folder, "js", "slides.js")
  header <- file.path(meta_folder, paste0(course, "_header.html"))
  x <- c(h, x)
  out_file <- gsub("\\.[Rr]md", "_temp.Rmd", file)
  writeLines(x, out_file)

  pres_file <- gsub("\\.[Rr]md$", "_slides.html", file)
  render(
    input = out_file,
    output_format = revealjs::revealjs_presentation(
      fig_width = 5, fig_height = 3.5, self_contained = F, transition = "fade", background_transition = "fade",
      reveal_options = list(
        slideNumber = "c/t", controls = F, width = 1000, height = 750, margin = 0),
      reveal_plugins = c("notes", "search", "chalkboard"),
      highlight = "tango", includes = includes(before_body = header, after_body = js), css = css
    ),
    output_file = pres_file)
  file.remove(out_file)

  x <- readLines(pres_file)
  ind <- grep("_slides_files", x)
  ind <- ind[grep("img src", x[ind], invert = T)]
  x[ind] <-  gsub("^(\\s*?<link rel=\"stylesheet\" href=\").*?(slides_files.*)$", "\\1/\\2", x[ind])
  x[ind] <-  gsub("^(\\s*?<link href=\").*?(slides_files.*)$", "\\1/\\2", x[ind])
  x[ind] <-  gsub("^(\\s*?<script src=\").*?(slides_files.*)$", "\\1/\\2", x[ind])
  x[ind] <-  gsub("^(\\s*?\\{ src: ').*?(slides_files.*)$", "\\1/\\2", x[ind])

  x[grepl(paste0(course, ".css"), x)] <-
    paste0("    <link rel=\"stylesheet\" href=\"/slides_files/css/",
           course, ".css\"/>")
  writeLines(x, pres_file)
  files_dirs <- list.dirs(sub("\\.html", "_files", pres_file), recursive = F)
  for (i in grep("figure-reveal", files_dirs, invert = T, value = T)) unlink(i, recursive = T)
}
