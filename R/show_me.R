
#' View submitted Rmd and augmented HTML in RStudio
#'
#' Opens up an Rmd file in source editor and displays the corresponding HTML file in the Viewer pane. This allosw for more convenient marking with source and knitted files side-by-side.
#'
#' @param id \code{character}. Either path to .Rmd file to mark or, if working directory has been set to file containing Rmd submission, id number of the submissions to display.
#' @param module \code{character}. Module name. Defaults to \code{"and"} (Analysing Data)
#' @param height \code{numeric} height of RStudio viewer window or \code{"maximize"} (default).
#' @param online \code{logical}. Should knitted file be pulled from the web? \code{FALSE} by default.
#' @param url \code{cahracter}. URL to a webpage containing knitted HTML files. Ignored if \code{online=FALSE}. See details.
#' @param offline_html_path \code{cahracter}. If \code{online=FALSE}, path to knitted HTML files relative to \code{id=}. Ignored if \code{online=TRUE}.
#' @details String passed to \code{url} may contain \code{"..."}. If it does, it will get replace by value passed to \code{module}. This allows for easier use accross different modules, provided the file structure of the hosting website is identical except for the module name.
#' @return Function does not return a value. It opens an .Rmd file in the source editor and a HTML file in the Viewer.
#' @examples
#' # not setting working directory
#' show.me("marking/and/2019_20/1234.Rmd")
#' # not setting wd, offline HTML
#' show.me("marking/and/2019_20/1234.Rmd", offline=F, offline_thml_path = "/knitted")
#' # setting wd
#' setwd("marking/and/2019_20")
#' show.me("1234")
#' @export
#' 

show.me <- function(id, module = "and", height = "maximize", online = F, url = "https://mivalek.github.io/.../marking/knitted/",
                    offline_html_path = "../knitted") {
  module <- ifelse(module == "and", "adata", module)
  if (online) {
    url <- sub("...", module, url, fixed = T)
  } else {
    offline_html_path <- ifelse(is.null(offline_html_path), "", offline_html_path)
  }
  
  wd_restore <- getwd()
  on.exit(setwd(wd_restore))
  
  path <- sub("^(.*)/.*$", "\\1", id)
  if (path != id) {
    setwd(path)
    id <- sub("^.*/", "", id)
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
  
  file <- readLines(local_html)
  # extract limits for <div class="sidebar1"></div>
  sidebar1_ind <- grep("class=\"sidebar[12]\"", file) - 0:1
  sidebar1 <- file[sidebar1_ind[1]:sidebar1_ind[2]]
  iframes <- grep("<iframe src=\".*?\\.html", sidebar1, value = T)
  urls <- gsub(".*?\\\"(.*?)\\\".*", "\\1", iframes)
  dir.create(file.path(tempdir(), "doc"))
  sapply(grep("^http", urls, value = T), function(x) download.file(x, file.path(tempdir(), "doc", gsub(".*/", "", x))))
  local_files <- grep("^http", urls, value = T, invert = T)
  file.copy(local_files, file.path(tempdir(), gsub(".*/", "doc/", local_files)))
  for (i in urls) {
    file[sidebar1_ind[1]:sidebar1_ind[2]] <- sub(i, sub(".*/", "doc/", i), file[sidebar1_ind[1]:sidebar1_ind[2]])
  }
  
  writeLines(file, local_html)
  
  rstudioapi::navigateToFile(rmd)
  rstudioapi::viewer(local_html, height = height)
}


