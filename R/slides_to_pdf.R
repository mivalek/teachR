#' Generate PDF lecture slides for a HUGO tachR website
#'
#' Requires a local install of \href{https://github.com/astefanutti/decktape}{decktape}. Function converts one or more knitted teachR::xaringan_slides files to a PDF version with slides next to a "notepad" sheet (\href{https://and.netlify.app/lectures/01/AnD_lecture01_slides.pdf}{example}).
#'
#' @param lecture_number Number of the lecture to convert to PDF. May be a vector.
#' @param module \code{character}. Module name to use as a prefix in PDF file name. Defaults to \code{"AnD"}.
#' @param port Port number on which to locally serve the Hugo website hosting lecture slides.
#'
#' @details Function will first check if \code{\link[blogdown:serve_site]{blogdown::serve_site()}} has been called. If it hasn't it will call it before running decktape on lectures hosted at \code{"http://localhost:[port]/lectures/[lecture_number]/slides/?handout=true"}.
#' 
#' Ouptu PDF gets saved to \code{"content/lectures/[lecture_number]/[module]_lecture[lecture_number]_slides.pdf"}.
#'  
#' @examples
#' slides_to_pdf(1:3)
#' 
#' @export

slides_to_pdf <- function(lecture_number, module="AnD", port=4321) {
  needsServer <- try(readLines(paste0("http://localhost:", port), n = 1)) |> inherits("try-error")
  if (needsServer) blogdown::serve_site(port = port)
  num <-sprintf("%02d", lecture_numer)
  
  for (i in num) system2(
    "decktape",
    c("-s 600x400 --chrome-arg=--disable-web-security",
      paste0("http://localhost:", port, "/lectures/",i,"/slides/?handout=true"),
      paste0("content/lectures/", i, "/", module, "_lecture", i, "_slides.pdf")
    )
  )
  if (needsServer) blogdown::stop_server()
}
