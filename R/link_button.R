
#' Link a button on course content page to URL
#'
#' Function links a given button (all weeks by default) to URLs specified in \code{links=}.
#'
#' IMPORTANT: Set working directory to website folder!.
#'
#' @param course \code{character}. Course page to modify: one of "dapR_1", "daprR_2", "dapR_3", "usmr", or "msmr".
#' @param sem \code{numeric}. Semester to modify: either 1 or 2. Only applicable for 2-semester courses, currently dapR 1 and 2.
#' @param weeks Weeks in which to link buttons. Defaults to \code{"all"}, otherwise \code{numeric} vector.
#' @param label \code{character}. Name of the button to link.
#' @param links \code{character} vector. URLs for the buttons. Either length 1 or \code{length(weeks)}.
#' @return Function does not return anything of use.
#' @examples
#' # Link Explainer button for each week to 10 different URLs
#' link.button(course = "dapR_1", sem = 2, label = "Explainer",
#' link = paste0("/explainers/topic_", sprintf("%02d", 1:10), ".html"))

link.button <- function(course, sem = NULL, weeks = "all", label, links) {
  if (!course %in% c(paste0("dapR_", 1:3L), "usmr", "msmr"))
    stop('course must be one of c("dapR_1", "daprR_2", "dapR_3", "usmr", "msmr")')
  if (!isFALSE(course %in% c("dapR_1", "dapR_2") && !sem %in% 1:2L))
    stop(paste('sem= must be either 1 or 2 for', course))
  if (missing(label)) stop("Button label not provided.")
  if (missing(label)) stop("Links need to be provided..")
  if (!(length(weeks) == 1 && weeks[1] == "all") && !class(weeks) %in% c("integer", "numeric"))
    stop("weeks= must be either \"all\" or numeric.")
  if (class(try(readLines("index.html"))) == "try-error")
    stop("Cannot access website files. Have you set working directory?")


  label <- sub("^(.)", "\\U\\1", tolower(label), perl = T)
  sem <- paste0("sem_", sem)

  ff <- ifelse(is.null(sem),
               paste(course,  "index.html", sep = "/"),
               paste(course,  sem, "index.html", sep = "/"))
  x <- readLines(ff)
  bttns <- grep(paste0("<li><a href=.*?class=\"button.*?big\">",
                       label,"</a></li>"), x)
  if (weeks[1] == "all") weeks <- 1:length(bttns)
  if (length(bttns) < max(weeks)) {
    weeks <- weeks[weeks <= length(bttns)]
    warning("More weeks than buttons; ignoring excess weeks.")
  }
  bttns <- bttns[weeks]
  for (i in seq_along(links))
    links[i] <- gsub("^/?(.*?)/?$",
                     ifelse(grepl("\\.", links[i]), "/\\1", # add leading / if links to a file
                            "/\\1/"), # add leading and trailing / if links to folder
                     links[i])
  for (i in 1:length(bttns))
    x[bttns[i]] <- sub("(.*?href=\\\").*?(\\\" class.*?)",
                       paste0("\\1", links[i], "\\2"),
                       x[bttns[i]])
  writeLines(x, ff)
}
