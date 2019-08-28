
#' Update Course details section of a course page
#'
#' Function reqires an up-to-date website_courses.csv and website_people.csv files (both can be renamed) with all info within. For more details about the files, see internal guide.
#'
#' IMPORTANT: Set working directory to website folder!.
#'
#' @param details.file \code{character}. Name of .csv file containing course details. By default \code{"//chss.datastore.ed.ac.uk/chss/ppls/shared/courses/DAP/website_courses.csv"}. See details for MacOS and Linux users.
#' @param people.file \code{character}. Name of .csv file containing people details. By default, replaces \code{"_courses.csv"} in string given to \code{details.file=} with \code{"_people.csv"}.
#' @details The function will by default try to access //chss.datastore.ed.ac.uk/chss... This only works on Windows. If it cannot access the path, it will try /Volumes/chss... instead. This should work on MacOS. For Linux users, you're on your own but I bet you're used to that by now.
#' @return Function does not return anything of use.
#' @examples
#' # update course details for all courses
#' update.course.details()

update.course.details <- function(details.file = "default",
                                  people.file = "default") {
  if (details.file == "default") {
    details.file <- "//chss.datastore.ed.ac.uk/chss/ppls/shared/courses/DAP/website_courses.csv"
    if (!file.exists(details.file)) {
      details.file <- sub("/chss.datastore.ed.ac.uk", "Volumes", details.file)
      if (!file.exists(details.file)) stop("Cannot access the shared drive. Please set path manually.")
    }
  } else
    if (!file.exists(details.file)) stop("The course details file you linked to does not exist.")
  if (people.file == "default")
    people.file <- sub("_courses.csv", "_people.csv", details.file)
  if (!file.exists(people.file)) stop("The people details file you linked to does not exist.")
  if (class(try(readLines("index.html"))) == "try-error")
    stop("Cannot access website files. Have you set working directory?")
  
  ff <- list.files(pattern = "info.html", recursive = T)
  people <- read.csv(people.file, stringsAsFactors = F)
  info <- read.csv(details.file, stringsAsFactors = F, na.strings = "")
  info <- info[rowSums(is.na(info)) != ncol(info), ]
  # remove leading and trailing white spaces
  info <- as.data.frame(lapply(
    info, function(y) gsub("^\\s+|\\s+$", "", y)), stringsAsFactors = F)

  for (i in ff) {
    course <- gsub("/?(.*?)/.*", "\\1", i) # extract course from path
    html <- readLines(i)
    if (course %in% c("dapR_1", "dapR_2")) {
      sem1_section <- grep("<!-- Semester 1 -->", html)
      end_sem1_section <- grep("<!-- END Semester 1 -->", html)
      end_sem2_section <- grep("<!-- END Semester 2 -->", html)
      if (length(end_sem2_section) > 0) { # delete semester 2 section if exists
        html <- c(
          html[c(1:end_sem1_section, (end_sem2_section + 1):length(html))]
        )
        
      } else html <- gsub("sem-show", "sem-hide", html)
    }
    
    # x <- readLines(i)
    details <- info[ , c("course", grep(course, names(info), value = T))]
    diff_sem2 <- ncol(details) == 3 && !all(is.na(details[[paste0(course, "_sem2")]]))
    
    if (diff_sem2) {
      lec_drop <- grep("lecturer|drop_in", details$course)
      replace <- is.na(details[lec_drop, 3])
      details[lec_drop, 3][replace] <- details[lec_drop, 2][replace]
    }
    details <- as.data.frame(t(details), stringsAsFactors = F)
    names(details) <- as.character(details[1, ])
    details <- details[-1, ]

    # format rooms - replace ", " with <br>
    details[ , grep("_room", names(details))] <-
      lapply(details[ , grep("_room", names(details))],
             function(y) gsub(", ", "<br>", y))
    # replace space after days with en-space
    details[ , grep("_time", names(details))] <-
      lapply(details[ , grep("_time", names(details))],
             function(y) gsub("(days?) ", "\\1&ensp;", y))
    # replace "-" with en-dash
    details[ , grep("_time", names(details))] <-
      lapply(details[ , grep("_time", names(details))],
             function(y) gsub("-", " &ndash; ", y))
    

    
    html <- update.html(html, details[1, ], people = people)
    
    if (diff_sem2) {
      html <- gsub("sem-hide", "sem-show", html)
      end_sem1_section <- grep("<!-- END Semester 1 -->", html)
      sem2_html <- update.html(html, details[2, ], people = people)
      
      sem2_html <- gsub("Semester 1", "Semester 2", sem2_html)
      sem2_section <- grep("<!-- Semester 2 -->", sem2_html)
      end_sem2_section <- grep("<!-- END Semester 2 -->", sem2_html)
      sem2_html <- sem2_html[sem2_section:end_sem2_section]
      html <- c(html[1:end_sem1_section], sem2_html, html[(end_sem1_section + 1):length(html)])
    } else {
      html <- gsub("sem-show", "sem-hide", html)
    }

    writeLines(html, i)
  }
}
