
#' Update Course details section of a course page
#'
#' Function reqires an up-to-date website_courses.csv and website_people.csv files (both can be renamed) with all info within. For more details about the files, see internal guide.
#'
#' IMPORTANT: Set working directory to website folder!.
#'
#' @param details.file character. Name of .csv file containing course details (with path).
#' @param people.file character. Name of .csv file containing people details (with path). By default, replaces "_courses.csv" in string given to details.file= with "_people.csv".
#' @return Function does not return anything of use.
#' @examples
#' # update course details for all courses
#' update.course.details(details.file = "M:/teaching/admin/website_courses.csv")

update.course.details <- function(details.file,
                                  people.file = sub("_courses.csv", "_people.csv", details.file)) {
  if (!file.exists(details.file)) stop("The course details file you linked to does not exist.")
  if (!file.exists(people.file)) stop("The people details file you linked to does not exist.")
  ff <- list.files(pattern = "info.html", recursive = T)
  people <- read.csv(people.file, stringsAsFactors = F)
  info <- read.csv(details.file, stringsAsFactors = F, na.strings = "")
  info <- info[rowSums(is.na(info)) != ncol(info), ]
  # remove leading and trailing white spaces
  info <- as.data.frame(lapply(
    info, function(y) gsub("^\\s+|\\s+$", "", y)), stringsAsFactors = F)

  for (i in ff) {
    course <- gsub("/?(.*?)/.*", "\\1", i) # extract course from path
    x <- readLines(i)
    details <- info[ , c("course", course)]
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
    attach(details)

    one_lecture_course <- is.na(lec_day2)
    same_room_labs <- all(is.na(c(lab_room2, lab_room3, lab_room4))) |
      length(table(c(lab_room1, lab_room2, lab_room3, lab_room4), useNA = "always")) == 1

    # get relevant lines of info.html
    indices <- sapply(c("co", "tc", "cs", paste0("lecturer", 1:2), paste0("lec_day", 1:2),
                        paste0("lec_room", 1:2), paste0("lec_time", 1:2),
                        paste0("lab_room", 1:4), paste0("lab_time", 1:4),
                        "drop_in_person", "drop_in_room", "drop_in_time"),
                      function(y) grep(paste0("id=\"", y), x), simplify = F)

    # comment out / uncomment appropriate sections
    if (one_lecture_course) {
      x[grep("BEGIN ONE LECTURE", x)] <- "\t\t\t\t\t\t\t\t<!-- BEGIN ONE LECTURE COURSE-->"
      x[grep("END ONE LECTURE", x)] <- "\t\t\t\t\t\t\t\t<!-- END ONE LECTURE COURSE -->"

      x[grep("BEGIN TWO LECTURE", x)] <- "\t\t\t\t\t\t\t\t<!-- BEGIN TWO LECTURE COURSE"
      x[grep("END TWO LECTURE", x)] <- "\t\t\t\t\t\t\t\tEND TWO LECTURE COURSE -->"
    } else {
      x[grep("BEGIN ONE LECTURE", x)] <- "\t\t\t\t\t\t\t\t<!-- BEGIN ONE LECTURE COURSE"
      x[grep("END ONE LECTURE", x)] <- "\t\t\t\t\t\t\t\tEND ONE LECTURE COURSE -->"

      x[grep("BEGIN TWO LECTURE", x)] <- "\t\t\t\t\t\t\t\t<!-- BEGIN TWO LECTURE COURSE -->"
      x[grep("END TWO LECTURE", x)] <- "\t\t\t\t\t\t\t\t<!-- END TWO LECTURE COURSE -->"
    }

    if (same_room_labs) {
      x[grep("BEGIN SAME ROOM", x)] <- "\t\t\t\t\t\t\t\t<!-- BEGIN SAME ROOM LABS-->"
      x[grep("END SAME ROOM", x)] <- "\t\t\t\t\t\t\t\t<!-- END SAME ROOM LABS -->"

      x[grep("BEGIN DIFFERENT ROOM", x)] <- "\t\t\t\t\t\t\t\t<!-- BEGIN DIFFERENT ROOM LABS"
      x[grep("END DIFFERENT ROOM", x)] <- "\t\t\t\t\t\t\t\tEND DIFFERENT ROOM LABS -->"
    } else {
      x[grep("BEGIN SAME ROOM", x)] <- "\t\t\t\t\t\t\t\t<!-- BEGIN SAME ROOM LABS"
      x[grep("END SAME ROOM", x)] <- "\t\t\t\t\t\t\t\tEND SAME ROOM LABS -->"

      x[grep("BEGIN DIFFERENT ROOM", x)] <- "\t\t\t\t\t\t\t\t<!-- BEGIN DIFFERENT ROOM LABS -->"
      x[grep("END DIFFERENT ROOM", x)] <- "\t\t\t\t\t\t\t\t<!-- END DIFFERENT ROOM LABS -->"
    }

    ### update people
    # get display names / slugs / profile URLs of people
    get.name <- function(x, get = "all") {
      if (is.na(x)) {
        out <- list(name = NA, slug = NA, profile = NA)
      } else {
        name <- unlist(strsplit(x, " "))
        index <- people$forename == name[1] &
          people$surname == name[2]
        out <- list(name = paste(
          people[index,
                 c("title", "forename", "display_surname")],
          collapse = " "),
          slug = paste(tolower(name), collapse = "_"),
          profile = people$profile[index])
      }
      if (get == "all") get <- 1:3
      return(out[get])
    }

    # identify <div class="contact-method"> just BEFORE lecturer1 line
    div_ind <- rev(grep("class=\"contact-method", x))
    div_ind <- div_ind[div_ind < indices$lecturer1][1]

    if (!is.na(lecturer2)) {
      # two lecturer case
      if (length(indices$lecturer2) == 0) # no lecturer 2 line in html
        # insert lecturer 2 line
        x <- c(
          x[1:indices$lecturer1],
          c("<br>", sub("lecturer1", "lecturer2", x[indices$lecturer1])),
          x[(indices$lecturer1 + 1):length(x)]
        )
      # add id="multiline" for styling of bottom margin
      x[div_ind] <- "\t\t\t\t\t\t\t\t<div id=\"multiline\" class=\"contact-method\">"

    } else if (length(indices$lecturer2) != 0) {# 1 lecturer case with l2 line in html
      x <- x[-(indices$lecturer2 - 1:0)] # delete lecturer 2 line
      # remove id="multiline" for styling of bottom margin
      x[div_ind] <- "\t\t\t\t\t\t\t\t<div class=\"contact-method\">"
    }
    # update indices
    indices <- sapply(c("co", "tc", "cs", paste0("lecturer", 1:2), paste0("lec_day", 1:2),
                        paste0("lec_room", 1:2), paste0("lec_time", 1:2),
                        paste0("lab_room", 1:4), paste0("lab_time", 1:4),
                        "drop_in_person", "drop_in_room", "drop_in_time"),
                      function(y) grep(paste0("id=\"", y), x), simplify = F)

    # update people
    for (j in c("co", "tc", "lecturer1", "lecturer2", "drop_in_person")) {
      x[indices[[j]]] <- paste0(
        "\t\t\t\t\t\t\t\t\t<a id=\"", j, "\" href=\"/dapR_1/people.html#",
        get.name(details[[j]], 2), "\">",
        get.name(details[[j]], 1), "</a>"
      )
    }

    x[indices$cs] <- paste0(
      "\t\t\t\t\t\t\t\t\t<a id=\"", j, "\" href=\"",
      get.name(details$cs, 3), "\">",
      get.name(details$cs, 1), "</a>")

    ### update rooms
    for (j in c(paste0("lec_room", 1:2), paste0("lab_room", 1:4), "drop_in_room")) {
      x[indices[[j]]] <- paste0(
        "\t\t\t\t\t\t\t\t\t<a id=\"", j, "\" href=\"https://www.ed.ac.uk/maps/maps?building=",
        details[[paste0(j, "_link")]],
        "\" target=\"_blank\">",
        details[[j]],
        "</a>")
    }

    ### update times
    ## lectures
    if (one_lecture_course) {
      x[indices$lec_time1] <- paste0(
        "\t\t\t\t\t\t\t\t\t<span id=\"lec_time1\">", lec_day1, "&ensp;",
        sub("-", " &ndash; ", lec_time1), "</span>")
    } else {
      x[indices$lec_day1] <- paste0("\t\t\t<h4 id=\"lec_day1\">", lec_day1, "</h4>")
      x[indices$lec_time1] <- paste0(
        "\t\t\t\t\t\t\t\t\t<span id=\"lec_time1\">", lec_time1, "</span>")

      x[indices$lec_day2] <- paste0("\t\t\t<h4 id=\"lec_day2\">", lec_day2, "</h4>")
      x[indices$lec_time2] <- paste0(
        "\t\t\t\t\t\t\t\t\t<span id=\"lec_time2\">", lec_time2, "</span>")
    }

    ## labs
    if (same_room_labs) {
      # get lab times in a vector
      labs <- as.vector(details[ , grep("lab_time", names(details))])
      x[indices$lab_time1] <- paste0(
        "\t\t\t\t\t\t\t\t\t<span id=\"lab_time1\">",
        paste(labs[!is.na(labs)], collapse = "<br>"),
        "</span>"
      )
    } else{
      for (j in paste0("lab_time", 1:4)) {
        x[indices[[j]]] <- paste0(
          "\t\t\t\t\t\t\t\t\t<span id=\"", j, "\">", details[[j]], "</span>")
      }
    }

    # drop-ins
    x[indices$drop_in_time] <- paste0(
      "\t\t\t\t\t\t\t\t\t<span id=\"drop_in_time\">", details$drop_in_time, "</span>")

    writeLines(x, i)
  }
}
