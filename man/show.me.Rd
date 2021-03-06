% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/show_me.R
\name{show.me}
\alias{show.me}
\title{View submitted Rmd and augmented HTML in RStudio}
\usage{
show.me(
  id,
  module = "and",
  height = "maximize",
  online = F,
  url = "https://mivalek.github.io/.../marking/knitted/",
  offline_html_path = "../knitted"
)
}
\arguments{
\item{id}{\code{character}. Either path to .Rmd file to mark or, if working directory has been set to file containing Rmd submission, id number of the submissions to display.}

\item{module}{\code{character}. Module name. Defaults to \code{"and"} (Analysing Data)}

\item{height}{\code{numeric} height of RStudio viewer window or \code{"maximize"} (default).}

\item{online}{\code{logical}. Should knitted file be pulled from the web? \code{FALSE} by default.}

\item{url}{\code{cahracter}. URL to a webpage containing knitted HTML files. Ignored if \code{online=FALSE}. See details.}

\item{offline_html_path}{\code{cahracter}. If \code{online=FALSE}, path to knitted HTML files relative to \code{id=}. Ignored if \code{online=TRUE}.}
}
\value{
Function does not return a value. It opens an .Rmd file in the source editor and a HTML file in the Viewer.
}
\description{
Opens up an Rmd file in source editor and displays the corresponding HTML file in the Viewer pane. This allosw for more convenient marking with source and knitted files side-by-side.
}
\details{
String passed to \code{url} may contain \code{"..."}. If it does, it will get replace by value passed to \code{module}. This allows for easier use accross different modules, provided the file structure of the hosting website is identical except for the module name.
}
\examples{
# not setting working directory
show.me("marking/and/2019_20/1234.Rmd")
# not setting wd, offline HTML
show.me("marking/and/2019_20/1234.Rmd", offline=F, offline_thml_path = "/knitted")
# setting wd
setwd("marking/and/2019_20")
show.me("1234")
}
