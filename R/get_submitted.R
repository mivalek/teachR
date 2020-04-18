
#' Wrapper for \code{rcanvas::get_submissions}
#'
#' Retrieves assignment submissions from Canvas from only those students who  have submitted .
#'
#' @param token \code{character}. Canvas access token.
#' @param domain \code{character}. Canvas domain.
#' @param course_id \code{numeric}. ID of Canvas course/module from which to retrieve assignment
#' @param assign_id \code{numeric}. ID code of Canvas assignment to retrieve.
#' @import rcanvas
#' @examples
#' submitted <- get.submitted("1204923CwEH1wk2GKcJqbXF3Nja3QUg7G2Ksr6dBscIN9, course_id = 1234, assign_id = 1324)
#' @export
#' 

# get submitted reports from Canvas
get.submitted <- function(token, domain = "https://canvas.sussex.ac.uk", course_id, assign_id) {
  rcanvas::set_canvas_token(token)
  rcanvas::set_canvas_domain(domain)
  submissions <- rcanvas::get_submissions(course_id, "assignments", assign_id)
  submissions[submissions$workflow_state == "submitted", ]
}
