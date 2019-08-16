
#' Add or remove a button on course content page
#'
#' A button gets added/removed for each week of a given course/semester.
#'
#' IMPORTANT: Set working directory to website folder!.
#'
#' @param course character. Course page to modify: one of "dapR_1", "daprR_2", "dapR_3", "usmr", or "msmr".
#' @param sem numeric. Semester to modify: either 1 or 2. Only applicable for 2-semester courses, currently dapR 1 and 2.
#' @param label character. Button text. Keep it short, e.g., "Lecture", "Lab", "Explainer"...
#' @param link character. Single URL for the buttons. Defaults to /course/sem. Probably better to use link.button().
#' @param remove logical. If FALSE (default), button gets added. If TRUE it gets removed
#' @return Function does not return anything of use.
#' @examples
#' # add Explainer button for each week
#' week.button(course = "dapR_1", sem = 2, label = "Explainer")
#'
#' # remove Lecture button
#' week.button("msmr", label = "Lecture", rmeove = T)

week.button <- function(course, sem = NULL, label, link, remove = FALSE) {
  if (!course %in% c(paste0("dapR_", 1:3L), "usmr", "msmr"))
    stop('course= must be one of c("dapR_1", "daprR_2", "dapR_3", "usmr", "msmr")')
  if (!isFALSE(course %in% c("dapR_1", "dapR_2") && !sem %in% 1:2L))
    stop(paste('sem= must be either 1 or 2 for', course))
  if(missing(label)) stop("Button label not provided.")
  if (!is.logical(remove)) stop("remove= must be logical.")
  label <- sub("^(.)", "\\U\\1", tolower(label), perl = T)
  if (!is.null(sem)) sem <- paste0("sem_", sem)
  ff <- ifelse(is.null(sem),
               paste(course,  "index.html", sep = "/"),
               paste(course,  sem, "index.html", sep = "/"))
  x <- readLines(ff)
  if (!remove) {
    if (missing(link)) {
      link <- ifelse(is.null(sem),
                     course,
                     paste(course,  sem, sep = "/"))
      warning(paste("link= not specified; linking to", link))
    }
    line <- grep("</ul>", x)
    line <- line[grepl("<li><a href=.*?class=\"button.*?big\">.*?</a></li>", x[line-1])] - 1
    for (i in 1L:length(line)) {
      x <- c(
        x[1L:(line[i] + i-1L)],
        paste0("                                    <li><a href=\"/",
               link, "/\" class=\"",
               ifelse(grepl("special", x[line[i] + i-1L]),
                      "button big\">", "button special big\">"),
               label, "</a></li>"),
        x[(line[i] + i):length(x)])
    }
  } else {
    x <- x[!grepl(paste0("button.*?big\">", label, "</a></li>"), x)]
    # make buttons alternate styles after removal of one
    edit <- grep("<li><a href=.*?class=\"button.*?big\">.*?</a></li>", x) # lines with buttons
    mat <- matrix(edit, nrow = 10, byrow = T) # arrange in matrix of 10 rows (10 weeks)
    normal <- as.vector(mat[ , seq(1L, ncol(mat), by = 2)]) # odd columns are "button"
    special <- as.vector(mat[ , seq(2L, ncol(mat), by = 2)]) # odd columns are "button special"
    x[normal] <- gsub("class=\"button special", "class=\"button", x[normal])
    x[special] <- gsub("class=\"button", "class=\"button special", x[special])
  }
  writeLines(x, ff)
}
