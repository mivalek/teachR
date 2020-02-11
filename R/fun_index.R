
#' Create html index of used functions
#'
#' Function creates an index files of all functions inside all code chunks within .Rmd files in a given folder (optionally only those conforming to a pattern, see Arguments). The index file is a HTML file with links to individual lab sheets/practicals.
#' 
#' @param folder \code{character}. Path to folder containing lab sheet/practical .Rmd files.
#' @param output \code{character}. Name of output HTML file without extention. "fun_index" by default
#' @param course \code{character}. Course name to govern colour scheme. One of \code{"dapR_1", "daprR_2", "dapR_3", "usmr", "msmr", "other"}. \code{"other"} by default.
#' @param url \code{character}. URL paths (absolute or relative) to find reference HTML practicals/sheets. Either length 1 or length equal to number of files indexed. By default \code{""}.
#' @param pattern \code{character}. Pattern to match when searching.  
#' @param recursive \code{logical}. Should subfolders of \code{folder=} be searched? \code{TRUE} by default.
#' @details It's best to keep website structure flat. Put all practicals/lab sheets in a single folder and leave \code{url=} at default. fun_index.html should be placed in the same folder. If this is not the case, \code{url=} must be change otherwise links will be broken.
#' @examples
#' fun.index("N:/teaching/analysing_data", , pattern = "practical\\d+\\.Rmd")

fun.index <- function(folder, output = "fun_index", course = "other", url = "", pattern = ".Rmd", recursive = T) {

  if(!dir.exists(folder))
    stop("This folder does not exist.")
  resetwd <- getwd()
  on.exit(setwd(resetwd))
  setwd(folder)
  
  files <- list.files(pattern = pattern, recursive = recursive)
  if (length(files) == 0)
    stop(paste("There are no files matching pattern= in", folder))
  
  if (!grepl("\\.Rmd$", output))
    output <- paste0(gsub("\\.html$", "", output), ".Rmd")
  
  if (length(url) == 1) url <- rep(url, length(files))
  if (length(url) != length(files))
    stop("length(url) must be either 1 or equal to number of matched .Rmd files")
  
  url <- ifelse(url != "" & !grepl("/$", url), paste0(url, "/"), url)
  fun_list <- list()
  warn <- c()
  
  on.exit(file.remove("temp.R"), T, F)
  on.exit(file.remove("temp.Rmd"), T, F)
  for (i in files) {
    tmp <- readLines(i)
    start <- grep("```\\{r setup", tmp)
    x <- grep("```", tmp)
    
    end <- x[which(x %in% start) + 1]
    cut <- unlist(apply(cbind(start, end), 1, function(x) x[1]:x[2]))
    writeLines(tmp[-cut], "temp.Rmd")
    
    knitr::purl("temp.Rmd", "temp.R")
    tmp <- try(parse("temp.R", keep.source=TRUE))
    if (class(tmp) == "try-error") {
      warn <- c(warn, i)
    } else {
      tmp %>%
        utils::getParseData() %>%
        dplyr::filter(token == "SYMBOL_FUNCTION_CALL") %>%
        dplyr::pull(text) %>%
        unique() ->
        fun_list[[i]]
    }
  }

  index <- tibble::tibble(fun = sort(unique(unlist(fun_list))))
  fun_in_file <- list()
  for (i in seq_along(index$fun))
    fun_in_file[[i]] <- names(
      which(
        unlist(
          lapply(fun_list, function(x) index$fun[i] %in% x)
          )))
  
  fun_in_file <- lapply(
    fun_in_file, function(x) x[order(as.numeric(gsub(".*?(\\d+).*", "\\1", x)))])
  
  index$practical <- unlist(
    lapply(
      fun_in_file, function(x) paste(paste0(
        "[", gsub(".*?(\\d+).*", "\\1", x), "](",
        url[grep(x, files)], # pick URL prefixes
        gsub("\\.Rmd", ".html", x), ")"), collapse = ", ")
    )
  )
  
  index_letter <- unique(tolower(gsub("^(.).*", "\\1", index$fun)))
  
  out <- c(
    "---",
    "title: 'Index of functions used in practicals'",
    "---",
    "",
    "Below is the idexed list of all the functions we have used in the practicals.",
    "It tells you which practicals make use of any given function.",
    "You can click on the number of the practical to open the worksheet and then search (<kbd>Ctrl</kbd> + <kbd>F</kbd> on Windows and <kbd>&#8984;\\ Command</kbd> + <kbd>F</kbd> on Mac OS) for the function to see how it is used.",
    "",
    "")
  
  for (i in index_letter) {
    out <- c(
      out,
      "",
      paste("###", toupper(i)),
      "```{r, echo=F}",
      paste0("index %>% filter(gsub('^(.).*', '\\\\1', fun) == '", i,
             "') %>% kable(col.names = c('Function', 'Practical')) %>% ",
            "kable_styling(full_width = F, position = 'left') %>% ",
            "column_spec(1, '4in')"),
      "```",
      "",
      "\ "
    )
  }
  
  on.exit(rm(index,envir = .GlobalEnv), T)
  index <<- index
  
  writeLines(out, output)
  on.exit(file.remove(output), T, F)
  make.sheet("fun_index.Rmd", course = course, toc_depth = 3)
  
  if (length(warn) != 0)
    message(paste("Warning: The following .Rmd files could not be parsed and were not indexed. They likely include chunks with error=T option.\n", warn, collapse="\n"))
}
