
#' Mark coursework assignment Rmd
#'
#' For Rmd submissions of reports and other coursework. Function checks word count and inserts space for feedback.
#'
#' @param csv_path \code{character}. Path or URL of a csv file from which to extract quick comments.
#' @param rubric \code{character}. Sourceable file that produces a rubric list object from which to get colours of buttons so that they match colours of rubric criteria from \code{mark()}. See URL in example.
#' @param output_dir \code{character}. Which study to get correct results for? Currently either \code{"red"} or \code{"green"}.
#' @param warn_col \code{numeric}. Hex code of colour for general comments.
#' @param code_col \code{numeric}. Hex code of colour for comments in code.
#' @param tabbed \code{logical}. Should sections be tabs (\code{TRUE}; default) or FAQ-style buttons (HTML \code{<details>})?
#' @param keep_rmd \code{logical}. Should the source .Rmd file be kept (in working directory)? \code{FALSE} by default.
#' @return Function produces a HTML document saved in \code{output_dir}.
#' @examples
#' get.quick.com(csv_path = "https://docs.google.com/spreadsheets/d/19CXCZk28CQzX4MzQ86a5U-ijYQvP47x0mfX7pQk6KKo/export?gid=588770828&format=csv",
#' rubric = "https://raw.githubusercontent.com/SussexPsychMethods/and_pub/master/marking/sussex_rubric.R",
#' output_dir = "../mivalek_io/adata/marking/")
#' @export get.quick.com
#' @usage get.quick.com(csv_path, rubric, output_dir, warn_col = "#cc0000", code_col = "#b38ed2", tabbed = T, keep_rmd = F)
#' 


get.quick.com <- function(csv_path, rubric, output_dir, warn_col = "#cc0000", code_col = "#b38ed2", tabbed = T, keep_rmd = F) {
  q_com <- read.csv(csv_path, stringsAsFactors = F)
  if (grepl('^http', csv_path)) {
    is.url <- T
    if (grepl('google.com/spreadsheets', csv_path))
      csv_path <- sub('export\\?(.*?)&format=csv', 'edit#\\1', csv_path)
  }
  source(rubric, local = T)
  on.exit(rm(rubric))
  colors <- lapply(rubric, function(x)  paste(col2rgb(x$col), collapse=", "))
  colors <- c(
    paste0("--", names(colors), "-col: ", colors, ";"),
    paste0("--warn-col: ", paste(col2rgb(warn_col), collapse=", "), ";"),
    paste0("--code-col: ", paste(col2rgb(code_col), collapse=", "), ";")
  )
  
  q_com <- q_com[nzchar(q_com$description), ]
  criteria <- q_com$description[!nzchar((q_com$text))]
  q_com <- q_com %>% dplyr::filter(nzchar(text)) %>%
    dplyr::mutate(text = gsub("&", "&amp;", text),
           markdown = gsub("&", "&amp;", markdown),
           button =  paste0('<button class="btn ', tag,
                            '" data-clipboard-text="',
                            gsub('\"', '\'', markdown),
                            '">Markdown</button>')) %>%
    dplyr::select(-markdown) %>%
    tidyr::nest(data = c(description, text, button))
  tabs <- q_com %>%
    purrr::pmap(function(tag, data)
      data %>%
        kableExtra::kable(col.names = c("Description", "Text", ""), escape = F, align = "llr", padding = "2px") %>%
        kableExtra::column_spec(1, width = "150px") %>%
        kableExtra::kable_styling(full_width = T)
    )
  
  styles <- c()
  for (i in 1:nrow(q_com)) {
    styles <- c(
      styles,
      if (tabbed) {
        c(
          paste0('.section.', q_com$tag[i], ' {'),
          paste0('  border-color: rgb(var(--', q_com$tag[i], '-col));'),
          '}',
          '',
          paste0('.nav-pills > li:nth-child(', i, ') {'),
          paste0('  border-color: rgb(var(--', q_com$tag[i], '-col));'),
          paste0('  background-color: rgb(var(--', q_com$tag[i], '-col));'),
          '}',
          '',
          paste0('.nav-pills > li:nth-child(', i, '):hover {'),
          paste0('  background-color: rgba(var(--', q_com$tag[i], '-col), .3);'),
          '}',
          paste0('.nav-pills > li:nth-child(', i, ').active > a,'),
          paste0('.nav-pills > li:nth-child(', i, ') > a:hover,'),
          paste0('.nav-pills > li:nth-child(', i, ').active > a:hover {'),
          paste0('  color: rgb(var(--', q_com$tag[i], '-col));'),
          '}',
          '',
          paste0('.btn.', q_com$tag[i], ' {'),
          paste0('  background-color: rgb(var(--', q_com$tag[i], '-col));'),
          '}'
        )
      } else {
        c(
          paste0('details.', q_com$tag[i], ' {'),
          paste0('  background-color: rgb(var(--', q_com$tag[i], '-col));'),
          '}',
          '',
          paste0('details.', q_com$tag[i], '[open] > summary {'),
          paste0('  color: rgb(var(--', q_com$tag[i], '-col));'),
          '}',
          '',
          paste0('details.', q_com$tag[i], '[open] {'),
          paste0('  border-color: rgb(var(--', q_com$tag[i], '-col));'),
          'background-color: white;',
          '}',
          '',
          paste0('.btn.', q_com$tag[i], ' {'),
          paste0('  background-color: rgb(var(--', q_com$tag[i], '-col));'),
          '}'
        )
      }
    )
  }
  
  text <- c(
    '---',
    'pagetitle: "Quick Comments - AnD"',
    'output: html_document',
    '---',
    '',
    '<style>',
    ':root {',
    paste("    ", colors),
    '}',
    '',
    
    '',
    styles,
    '</style>',
    '',
    '\\ ',
    '',
    '\\ ',
    '',
    if (is.url) 
      paste0('Please feel free to add to quick comments [here](', csv_path, ').'),
    '',
    paste0('(Synced with `teachR::get.quick.com()` on ', sub("^0", "", format(Sys.Date(), "%d %B %Y")), '.)'),
    '',
    '\\ ',
    '',
    if (tabbed) {
      c(
        '## \\ {.tabset .tabset-pills}',
        '',
        '```{r, echo = F, results = "asis"}',
        'for (i in 1:nrow(q_com)) {',
        '  cat("### ", criteria[i], " {.", q_com$tag[i], "}",','"\n\n",',
        '  tabs[[i]],',
        '  "\n\n\\ \n\n", sep = "")',
        '}',
        '```',
        ''
      )
    } else {
      c(
        '```{r, echo = F, results = "asis"}',
        'for (i in 1:nrow(q_com)) {',
        '  cat("<details class=\\"", q_com$tag[i],"\\"><summary>",',
        '      criteria[i],',
        '     "</summary>\n",', 
        '      tabs[[i]],',
        '      "\n</details>\n\n\\ \n\n", sep = "")',
        '}',
        '```'
      )
      },
    '',
    '\\ ',
    ''
  )
  
  writeLines(text, "quick_comments.Rmd")
  if (!keep_rmd) on.exit(file.remove("quick_comments.Rmd"))
  
  rmarkdown::render("quick_comments.Rmd",
                    output_format = rmarkdown::html_document(
                      css = file.path(path.package("teachR"), "quick_com.css"),
                      includes = rmarkdown::includes(
                        before_body = file.path(path.package("teachR"), "copyClipboard.js"))),
                    output_dir = output_dir)
}

