
#' Mark coursework assignment Rmd
#'
#' For Rmd submissions of reports and other coursework. Function checks word count and inserts space for feedback.
#'
#' @param csv_path \code{character}. Path or URL of a csv file from which to extract quick comments.
#' @param rubric \code{character}. Sourceable file that produces a rubric list object from which to get colours of buttons so that they match colours of rubric criteria from \code{mark()}. See URL in example.
#' @param output_dir \code{character}. Which study to get correct results for? Currently either \code{"red"} or \code{"green"}.
#' @param warn_col \code{numeric}. Hex code of colour for general comments.
#' @param code_col \code{numeric}. Hex code of colour for comments in code.
#' @param tabbed \code{logical}. Should sections be tabs (\code{TRUE}; default) or FAq-style buttons (\code{<details>})?
#' @return Function produces a HTML document saved in \code{output_dir}.
#' @examples
#' get.quick.com(csv_path = "https://docs.google.com/spreadsheets/d/19CXCZk28CQzX4MzQ86a5U-ijYQvP47x0mfX7pQk6KKo/export?gid=588770828&format=csv",
#' rubric = "https://raw.githubusercontent.com/SussexPsychMethods/and_pub/master/marking/sussex_rubric.R",
#' output_dir = "../mivalek_io/adata/marking/")
#' @export get.quick.com
#' @usage get.quick.com(csv_path, rubric, output_dir, warn_col = "#f00000", code_col = "#b38ed2")
#' 


get.quick.com <- function(csv_path, rubric, output_dir, warn_col = "#f00000", code_col = "#b38ed2", tabbed = T) {
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
                            '">Copy text</button>')) %>%
    dplyr::select(-markdown) %>%
    tidyr::nest(data = c(description, text, button))
  tabs <- q_com %>%
    purrr::pmap(function(tag, data)
      data %>%
        kableExtra::kable(col.names = c("Description", "Text", ""), escape = F, align = "llr", padding = "2px") %>%
        kableExtra::column_spec(1, width = "150px") %>%
        kableExtra::kable_styling(full_width = T)
    )
  text <- c(
    '---',
    'output: html_document',
    '---',
    '',
    paste0('<script src="', path.package("teachR"), '/clipboard.js-master/dist/clipboard.min.js"></script>'),
    '<script>new ClipboardJS(".btn");</script>',
    '<style>',
    ':root {',
    paste("    ", colors),
    '}',
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
  # on.exit(file.remove("quick_comments.Rmd"))
  
  rmarkdown::render("quick_comments.Rmd",
                    output_format = rmarkdown::html_document(css = paste0(path.package("teachR"), "/quick_com.css")),
                    output_dir = output_dir)
}

