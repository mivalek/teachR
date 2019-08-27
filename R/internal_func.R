
#' Functions used within \code{\link{update.course.details}()}
#' 
#' These are not to be used by user.
#' 

# get relevant lines of info.html
get.indices <- function(x) {
  sapply(c("co", "tc1", "tc2", "cs", paste0("lecturer", 1:2), paste0("lec_day", 1:2),
           paste0("lec_room", 1:2), paste0("lec_time", 1:2),
           paste0("lab_room", 1:4), paste0("lab_time", 1:4),
           "drop_in_person", "drop_in_room", "drop_in_time"),
         function(y) grep(paste0("id=\"", y), x), simplify = F)
}

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

update.html <- function(x, sem_details) {
  attach(sem_details, warn.conflicts = F)
  one_lecture_course <- is.na(lec_day2)
  same_room_labs <- all(is.na(c(lab_room2, lab_room3, lab_room4))) |
    length(table(c(lab_room1, lab_room2, lab_room3, lab_room4), useNA = "always")) == 1
  
  # get relevant lines of info.html
  indices <- get.indices(x)
  
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
  
  if (!is.na(tc2)) {
    if (length(indices$tc2) == 0) {
      x <- c(
        x[1:indices$tc1],
        c("\t\t\t\t\t\t\t\t\t<br>", sub("tc1", "tc2", x[indices$tc1])),
        x[(indices$tc1 + 1):length(x)]
      )
    }
  } else if (length(indices$tc2) != 0) { # 1 teaching co-ordinator case with tc2 line in html
    x <- x[-(indices$tc2 - 1:0)] # delete tc2 line
  }
  
  # update indices
  indices <- get.indices(x)
  
  # identify <div class="contact-method"> just BEFORE lecturer1 line
  div_ind <- rev(grep("class=\"contact-method", x))
  div_ind <- div_ind[div_ind < indices$lecturer1][1]
  
  if (!is.na(lecturer2)) {
    # two lecturer case
    if (length(indices$lecturer2) == 0) # no lecturer 2 line in html
      # insert lecturer 2 line
      x <- c(
        x[1:indices$lecturer1],
        c("\t\t\t\t\t\t\t\t\t<br>", sub("lecturer1", "lecturer2", x[indices$lecturer1])),
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
  indices <- get.indices(x)
  
  # update people
  for (j in c("co", "tc1", "tc2", "lecturer1", "lecturer2", "drop_in_person")) {
    x[indices[[j]]] <- paste0(
      "\t\t\t\t\t\t\t\t\t<a id=\"", j, "\" href=\"/dapR_1/people.html#",
      get.name(sem_details[[j]], 2), "\">",
      get.name(sem_details[[j]], 1), "</a>"
    )
  }
  
  x[indices$cs] <- paste0(
    "\t\t\t\t\t\t\t\t\t<a id=\"cs\" href=\"",
    get.name(sem_details$cs, 3), "\">",
    get.name(sem_details$cs, 1), "</a>")
  
  ### update rooms
  for (j in c(paste0("lec_room", 1:2), paste0("lab_room", 1:4), "drop_in_room")) {
    x[indices[[j]]] <- paste0(
      "\t\t\t\t\t\t\t\t\t<a id=\"", j, "\" href=\"https://www.ed.ac.uk/maps/maps?building=",
      sem_details[[paste0(j, "_link")]],
      "\" target=\"_blank\">",
      sem_details[[j]],
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
    labs <- as.vector(sem_details[ , grep("lab_time", names(sem_details))])
    x[indices$lab_time1] <- paste0(
      "\t\t\t\t\t\t\t\t\t<span id=\"lab_time1\">",
      paste(labs[!is.na(labs)], collapse = "<br>"),
      "</span>"
    )
  } else{
    for (j in paste0("lab_time", 1:4)) {
      x[indices[[j]]] <- paste0(
        "\t\t\t\t\t\t\t\t\t<span id=\"", j, "\">", sem_details[[j]], "</span>")
    }
  }
  
  # drop-ins
  x[indices$drop_in_time] <- paste0(
    "\t\t\t\t\t\t\t\t\t<span id=\"drop_in_time\">", sem_details$drop_in_time, "</span>")
  
  return(x)
}
