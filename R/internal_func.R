

#' Functions used for within \code{\link{make.paper}()} and \code{\link{question.bank}()}.
#' 

parse_qus <- function(x, group = F, author) {
  parsed <- gsub("^\\s*(\\d*)\\.?\\s+(.*?)\\s*\\{(\\d{2})([SEMD])\\}", "\\1~***~\\2~***~\\3~***~\\4", x[1])
  stem_elements <- strsplit(parsed, "~***~", fixed = T)
  opts_lines <- grep("^\\s*-", x)
  if (opts_lines[1] != 2) {
    complex_stem <- x[2:(opts_lines[1] - 1)]
    stem_elements[[1]][2] <- paste(stem_elements[[1]][2],
                                   paste(complex_stem, collapse = "\n\n"), "", sep = "\n\n")
  }
  opts <- gsub("^\\s*-\\s*", "", x[opts_lines])
  opts <- opts[order(grepl("\\(correct\\)", opts), decreasing = T)]
  opts <- gsub("\\s*\\(correct\\)\\s*$", "", opts)
  correct <- opts[1]
  
  out <- list(q_num = stem_elements[[1]][1],
              stem = stem_elements[[1]][2],
              opts = list(opts),
              correct = correct,
              week = stem_elements[[1]][3],
              diff = stem_elements[[1]][4],
              group = group,
              author = author
  )
  
  return(out)
}

#' @describeIn parse_qus 
get_questions <- function(x, group = NA, author) {
  qu_ind <- cbind(
    grep("^\\d{2}\\.", x),
    c(grep("^\\d{2}\\.", x)[-1] - 1, length(x))
  )
  
  qu_ind <- split(
    qu_ind,
    rep(1:nrow(qu_ind), 2))
  
  single_qus <- lapply(qu_ind, function(ind) x[seq(ind[1], ind[2])])
  
  out <- lapply(single_qus, teachR:::parse_qus, group, author)
  return(out)
}

#' @describeIn parse_qus 
format_q <- function(x, num, key = F, randomise_opts = T) {
  stem <- x$stem
  
  stem <- unname(unlist(strsplit(stem, "\n", fixed = T)))
  stem[1] <- paste0(num, ". ", stem[1])
  stem[-1] <- sub("\\s*(\\S+)", "> \\1", stem[-1])
  
  opts <- unname(unlist(x$opts))
  backtick <- grep("<code>", opts, fixed = T)
  if (length(backtick) != 0) {
    if (output == "pdf") {
      opts[backtick] <- gsub("<code>", "\\texttt{", opts[backtick], fixed = T)
      opts[backtick] <- gsub("</code>", "}", opts[backtick], fixed = T)
      opts[backtick] <- Hmisc::latexTranslate(gsub("`", "\\textasciigrave ", opts[backtick], fixed = T))
    } else {
      opts[backtick] <- gsub("`", "&#96;", opts[backtick])
      opts[backtick] <- gsub("<code>((&\\#96;)*)(r)", "\\1\\3 `", opts[backtick])
      opts[backtick] <- gsub("<code>((&\\#96;)*)", "\\1`", opts[backtick])
      opts[backtick] <- gsub("((&\\#96;)*)</code>", "`\\1", opts[backtick])
    }
  }
  if (key)
    opts[1] <- paste(opts[1], "**(correct)**")
  
  if (randomise_opts) {
    opts_order <- sample(length(opts))
    opts <- paste(paste0("(a) ", opts[opts_order]), collapse = "\n")
  } else {
    opts_order <- 1:4
    opts <- paste(paste0("(a) ", opts), collapse = "\n")
  }
  opts[length(opts)] <- paste0(opts[length(opts)], "\\.")
  
  
  out <- list(item = c(stem, "", "\\ ", "", opts, "", "\\ ","", "\\ ", ""),
              key = c(num, LETTERS[which(opts_order == 1)]))
  return(out)
}

#' Functions used for marking and within \code{\link{mark}()}
#' 

unlibrary <- function(x = default_pkgs) {
  loaded_pkgs <- .packages()
  
  unload <- setdiff(loaded_pkgs, x)
  if (length(unload) > 0)
    lapply(paste0("package:", unload), function(x) try(detach(x, unload = T, char = T, force = T), silent = T))
}

#' @describeIn unlibrary HTML formatting for p-values

pround <- function(x) {
  if (x < .001) {
    return(".001")
  } else {
    return(sub("^0", "",
               format(round(x, 3), nsmall = 3))) 
  }
}

check_results <- function(study, student_res) {
  correct_results <- teachR:::res(study)
  results_ok <- F
  if (!is.null(student_res)) {
    if (study == "green") {
      student_stats <- unlist(lapply(student_res, function(x) x$statistic))
      correct_stats <- unlist(lapply(correct_results[c("car", "cleaner", "dishwasher")],
                                     function(x) x$statistic))
      if (length(student_res) != 3) {
        results_ok <- "FALSE - check number of tests"
      } else if (all(student_stats %in% correct_stats))
        results_ok <- T
      
    } else if (study == "red") {
      # are all htest object equal?
      equal_htest <- all(
        unlist(lapply(student_res, function(x) all.equal(x, student_res[[1]]))) == T
      )
      
      if (length(student_res) != 1 && !equal_htest) {
        results_ok <- "FALSE - check number of tests"
      } else if (student_res[[1]]$statistic == correct_results$result$statistic) {
        results_ok <- T
      }
    }
  }
  return(
    list(results_ok = results_ok,
         correct_res = correct_results)
  )
}

vsub <- Vectorize(function(x, y, ...) sub(x, "", y, ...), SIMPLIFY = T)


res <- function(study = NA, cand_no = candidate_number) {
  `%>%` <- magrittr::`%>%`
  if (study == "red") {
    data <- adata::red_data(cand_no, mark = T)
    
    groups <- c("control", "experimental")
    typo_cond <- grep(paste(groups, collapse = "|"), unique(data$condition),
                      invert = T, value = T)
    
    out <- list(
      rem_age_na = sum(is.na(data$age)),
      rem_age_typo = 1,
      rem_age_young = sum(as.numeric(data$age) < 18, na.rm = T)
    )
    data$condition[data$condition == typo_cond] <- groups[which.min(adist(typo_cond, groups))]
    
    
    
    data <- data %>%
      dplyr::mutate(condition = droplevels(condition),
                    age = as.numeric(age),
                    rating = (item_1 + item_2 + item_3)/3) %>%
      dplyr::filter(!is.na(age) & age > 17)
    
    out$n_clean = nrow(data)
    
    out$cond_desc <- data %>%
      dplyr::group_by(condition) %>%
      dplyr::summarise(n = dplyr::n(),
                       age_m = mean(age),
                       sd_age = sd(age),
                       min_age = min(age),
                       max_age = max(age),
                       rating_m = mean(rating),
                       rating_sd = sd(rating)) %>%
      dplyr::mutate_if(is.numeric, round, 2)
    out$cond_desc_tab <- out$cond_desc[ , c(1:3, 7)] %>%
      dplyr::mutate(condition = dplyr::recode(
        condition, "control" = "Control", "experimental" = "Experim")) %>%
      knitr::kable(col.names = c("Group", "<em>N</em>",
                                 "<em>M</em><sub>age</sub>", "<em>M</em><sub>rating</sub>"),
                   escape = F) %>%
      kableExtra::kable_styling(full_width = F, position = "left")
    out$result <- t.test(rating ~ condition, data, var.equal = T)
  } else if (study == "green") {
    
    data <- adata::green_data(cand_no, mark = T)
    
    groups <- c("control", "experimental")
    typo_cond <- grep(paste(groups, collapse = "|"), unique(data$condition),
                      invert = T, value = T)
    typo_age <- grep("[A-z]", data$age, value = T)
    correct_age <- na.omit(
      unique(as.numeric(
        data$age[data$id == data$id[which(data$age == typo_age)]]
      ))
    )
    
    out <- list(
      rem_age_na = sum(is.na(data$age)/3)
    )
    data$condition[data$condition == typo_cond] <- groups[which.min(adist(typo_cond, groups))]
    data$age[which(data$age == typo_age)] <- correct_age
    
    out$rem_age_young = sum(as.numeric(data$age) < 18, na.rm = T)/3
    
    data <- data %>%
      dplyr::mutate(condition = droplevels(condition),
                    age = as.numeric(age)) %>%
      dplyr::filter(!is.na(age) & age > 17)
    
    out$n_clean = nrow(data)/3
    out$gen_desc <- data %>%
      dplyr::group_by(gender) %>%
      dplyr::summarise(n = dplyr::n()/3,
                       age_m = mean(age),
                       sd_age = sd(age),
                       min_age = min(age),
                       max_age = max(age),
                       perc_green = round(prop.table(table(selection))["green"] * 100, 2)) %>%
      dplyr::mutate_if(is.numeric, round, 2)
    out$cond_desc <- data %>%
      dplyr::group_by(condition) %>%
      dplyr::summarise(n = dplyr::n()/3,
                       age_m = mean(age),
                       sd_age = sd(age),
                       min_age = min(age),
                       max_age = max(age),
                       perc_green = round(prop.table(table(selection))["green"] * 100, 2)) %>%
      dplyr::mutate_if(is.numeric, round, 2)
    
    out$gen_desc_tab <- out$gen_desc[ , c(1:3, 7)] %>%
      dplyr::mutate(gender = stringr::str_to_sentence(gender)) %>%
      knitr::kable(col.names = c("Gender", "<em>N</em>",
                                 "<em>M</em><sub>age</sub>", "% green"),
                   escape = F) %>%
      kableExtra::kable_styling(full_width = F, position = "left")
    
    out$cond_desc_tab <- out$cond_desc[ , c(1:3, 7)] %>%
      dplyr::mutate(condition = dplyr::recode(
        condition, "control" = "Control", "experimental" = "Experim")) %>%
      knitr::kable(col.names = c("Group", "<em>N</em>",
                                 "<em>M</em><sub>age</sub>", "% green"),
                   escape = F) %>%
      kableExtra::kable_styling(full_width = F, position = "left")
    for (i in levels(data$product)) {
      temp <- data[data$product == i, ]
      out[[i]] <- chisq.test(table(temp$selection, temp$condition))
    }
  }
  
  return(out)
}



#' Functions used within \code{\link{update.course.details}()}
#' 
#' These are not to be used by user.
#' 
#'  get relevant lines of info.html
get.indices <- function(x) {
  sapply(c("co", "tc1", "tc2", "cs", paste0("lecturer", 1:2), paste0("lec_day", 1:2),
           paste0("lec_room", 1:2), paste0("lec_time", 1:2),
           paste0("lab_room", 1:4), paste0("lab_time", 1:4),
           "drop_in_person", "drop_in_room", "drop_in_time"),
         function(y) grep(paste0("id=\"", y), x), simplify = F)
}


#' @describeIn get.indices Get display names / slugs / profile URLs of people
get.name <- function(x, get = "all", people) {
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

#' @describeIn get.indices
update.html <- function(x, sem_details, people) {
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
      get.name(sem_details[[j]], 2, people), "\">",
      get.name(sem_details[[j]], 1, people), "</a>"
    )
  }
  
  x[indices$cs] <- paste0(
    "\t\t\t\t\t\t\t\t\t<a id=\"cs\" href=\"",
    get.name(sem_details$cs, 3, people), "\">",
    get.name(sem_details$cs, 1, people), "</a>")
  
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
