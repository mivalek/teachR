
#' View submitted Rmd and augmented HTML in RStudio
#'
#' Opens up an Rmd file in source editor and displays the corresponding HTML file in the Viewer pane. This allosw for more convenient marking with source and knitted files side-by-side.
#'
#' @param id \code{character}. Either path to .Rmd file to mark or, if working directory has been set to file containing Rmd submission, id number of the submissions to display.
#' @param module \code{character}. Module name. Defaults to \code{"and"} (Analysing Data)
#' @param height \code{numeric} height of RStudio viewer window or \code{"maximize"} (default).
#' @param online \code{logical}. Should knitted file be pulled from the web? \code{TRUE} by default.
#' @param url \code{cahracter}. URL to a webpage containing knitted HTML files. Ignored if \code{online=FALSE}. See details.
#' @param offline_html_path \code{cahracter}. If \code{online=FALSE}, path to knitted HTML files. Ignored if \code{online=TRUE}.
#' @details String passed to \code{url} may contain \code{"..."}. If it does, it will get replace by value passed to \code{module}. This allows for easier use accross different modules, provided the file structure of the hosting website is identical except for the module name.
#' @return Function does not return a value. It opens an .Rmd file in the source editor and a HTML file in the Viewer.
#' @examples
#' # not setting working directory
#' show.me("marking/and/2019_20/1234.Rmd")
#' # not setting wd, offline HTML
#' show.me("marking/and/2019_20/1234.Rmd", offline=F, offline_thml_path = "marking/and/2019_20/knitted)
#' # setting wd
#' setwd("marking/and/2019_20")
#' show.me("1234")
#' @export
#' 

show.me <- function(id, module = "and", height = "maximize", online = T, url = "https://mivalek.github.io/.../marking/knitted/", offline_html_path = NULL) {
  module <- ifelse(module == "and", "adata", module)
  if (online) {
    url <- sub("...", module, url, fixed = T)
  } else {
    offline_html_path <- ifelse(is.null(offline_html_path), "", offline_html_path)
  }
  
  rmd <- ifelse(grepl("\\.rmd$", id, ignore.case = T), id, paste0(id, ".Rmd"))
  html <- gsub("rmd$", "html", rev(unlist(strsplit(rmd, "[\\/]")))[1], ignore.case = T)
  local_html <- file.path(tempdir(), html)
  if (!file.exists(rmd)) stop(paste0("Can't find ", rmd, ". Did you set working directory?"))
  
  if (online) {
  download.file(paste0(url, html),
            local_html)
  } else {
    file.copy(file.path(offline_html_path, html), local_html)
  }
  
  rstudioapi::navigateToFile(rmd)
  rstudioapi::viewer(local_html, height = height)
}

