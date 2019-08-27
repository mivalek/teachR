
#' Update People section of a course page
#'
#' Function reqires an up-to-date website_people.csv file with all info. For more details about the file, see internal guide.
#'
#' IMPORTANT: Set working directory to website folder!.
#'
#' @param surname \code{character}. Either \code{"all"} (default) or surname of person to add to course.
#' @param course \code{haracter}. Course to modify: either \code{"all"} (default) or one of \code{"dapR_1", "daprR_2", "dapR_3", "usmr", "msmr"}.
#' @param file \code{character}. Name of .csv file containing details. By default \code{"//chss.datastore.ed.ac.uk/chss/ppls/shared/courses/DAP/website_people.csv"}. Not required if \code{remove = TRUE}. See details for MacOS and Linux users.
#' @param remove \code{logical}. \code{TRUE} removes people from page, \code{FALSE} adds them.
#' @param inside \code{logical}. Suppresses dialog windows. \code{TRUE} mainly for use within functions.
#' @details The function will by default try to access //chss.datastore.ed.ac.uk/chss... This only works on Windows. If it cannot access the path, it will try /Volumes/chss... instead. This should work on MacOS. For Linux users, you're on your own but I bet you're used to that by now.
#' @return Function does not return anything of use.
#' @examples
#' # update all people on all courses
#' update.people()
#'
#' # remove single person from all courses
#' update.people("valasek", remove = T)

update.people <- function(surname = "all", course = "all",
                          file = "default", remove = F, inside = F) {
  if (class(surname) != "character") stop("surname= must be a character vector.")
  if (!all(course %in% c(paste0("dapR_", 1:3L), "usmr", "msmr")) && course != "all")
    stop('course= must be "all" or one of c("dapR_1", "daprR_2", "dapR_3", "usmr", "msmr").')
  if (!is.logical(remove)) stop("remove= must be logical.")
  if (class(try(readLines("index.html"))) == "try-error")
    stop("Cannot access website files. Have you set working directory?")
  surname <- tolower(surname)
  all_peeps <- surname[1] == "all"
  all_courses <- course[1] == "all"
  if (remove) {
    if (all_courses && !inside) {
      if (all_peeps) {
        continue <- askYesNo("Do you want to remove all people from all courses?")
      } else {
        names <- paste(surname, collapse = ", ")
        names <- sub("(.*), (.*)", "\\1 and \\2", names)
        continue <- askYesNo(paste("Do you want to remove ", names, "from all courses?"))
      }
    } else if (all_peeps && !inside) {
      names <- paste(course, collapse = ", ")
      names <- sub("(.*), (.*)", "\\1 and \\2", names)
      continue <- askYesNo(paste0("Do you want to remove all people from ", names, "?"))
    } else continue = T

    if (!isTRUE(continue)) {
      for (i in course) {
        # read file
        x <- readLines(paste0(i, "/people.html"))
        if (all_peeps) {
          begin <- grep("<section id=\"two\" class=\"spotlights\">", x) + 1
          footer <- grep("<footer id=\"footer\".*?>", x)
          end_section <- rev(grep("</section>", x))
          end_section <- end_section[end_section < footer]
          end <- end_section[1]
          x <- x[-(begin:end)]
        } else {
          # find begining of person's <section>
          begin <- grep(paste(paste0("<section id=\".*_", surname, "\""), collapse = "|"), x)
          end_section <- grep("</section>", x)
          # find corresponding </section>
          end <- sapply(begin, function(x) end_section[x < end_section][1])
          # get all rows in between (even in case of multiple people)
          cut_rows <- as.vector(
            apply(cbind(begin, end), 1, function(x) x[1]:x[2]))
          # remove rows
          x <- x[-cut_rows]
        }
        # write file
        writeLines(x, paste0(i, "/people.html"))
      }
    }

  } else {
    if (file == "default") {
      file <- "//chss.datastore.ed.ac.uk/chss/ppls/shared/courses/DAP/website_people.csv"
      if (!file.exists(file)) {
        file <- sub("/chss.datastore.ed.ac.uk", "Volumes", file)
        if (!file.exists(file)) stop("Cannot access the shared drive. Please set path manually.")
      }
    } else
      if (!file.exists(file)) stop("The file you linked to does not exist.")
    if (all_courses) course <- c(paste0("dapR_", 1:3L), "usmr", "msmr")
    df <- read.csv(file, na.strings = "", stringsAsFactors = F)
    df$role <- ordered(df$role, levels = c("CO", "L", "TC", "TA"))
    df <- df[ , c(1:8, grep(paste(course, collapse = "|"), names(df)))]
    df$pic[is.na(df$pic)] <- "no_photo.png"
    for (i in course) {
      df_i <- df[!is.na(df[[i]]), ]
      df_i <- df_i[order(df_i$role, df_i$surname), ]
      # first remova all current people (easier that way)
      update.people("all", i, remove = T, inside = T)
      # read in html
      x <- readLines(paste0(i, "/people.html"))
      header <- x[1:grep("<section id=\"two\" class=\"spotlights\">", x)]
      footer <-
        x[(grep("<footer id=\"footer\".*?>", x) - 1):length(x)]
      x <- " "
      for (j in 1:nrow(df_i)) {
        x <- c(
          x,
          paste0(
            "\t<section id=\"",
            paste(tolower(df_i[j, c("forename", "surname")]), collapse = "_"),
            "\">"
          ),
          paste0(
            "\t\t<a href=\"", df_i$profile[j], "\" class=\"image\" target=\"_blank\">"
          ),
          paste0(
            "\t\t\t<img src=\"../img/", df_i$pic[j], "\" alt=\"\" data-position=\"center center\" />"
          ),
          "\t\t</a>\n\t\t<div class=\"content\">",
          "\t\t\t<div class=\"inner\">",
          "\t\t\t\t<header class=\"major\">",
          paste0(
            "\t\t\t\t\t<h3>",
            paste(df_i[j, c("title", "forename", "display_surname")], collapse = " "), "</h3>"
          ),
          "\t\t\t\t</header>",
          if(!is.na(df_i$blurb[j])) paste0("\t\t\t\t<p>", df_i$blurb[j], "</p>"),
          "\t\t\t\t<ul class=\"actions\">",
          paste0(
            "\t\t\t\t\t<li><a href=\"", df_i$profile[j],
            "\" target=\"_blank\" class=\"button big\">See profile</a></li>"),
          "\t\t\t\t</ul>","\t\t\t</div>","\t\t</div>","\t</section>"," "
        )
      }
      x <- c(header, x, "</section>", " ", footer)
      writeLines(x, paste0(i, "/people.html"))
    }
  }
}
