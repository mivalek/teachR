
sus_pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir, from) {
  
  # Identify YAML front matter delimiters
  input_text <- readLines(input_file, encoding = "UTF-8")
  yaml_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_text)
  
  if(length(yaml_delimiters) >= 2 &&
     (yaml_delimiters[2] - yaml_delimiters[1] > 1) &&
     grepl("^---\\s*$", input_text[yaml_delimiters[1]])) {
    yaml_params <- yaml::yaml.load(paste(input_text[(yaml_delimiters[1] + 1):(yaml_delimiters[2] - 1)], collapse = "\n"))
  } else yaml_params <- NULL
  
  ## Add modified title page components after YAML front matter
  # title_page must be created before pre_processor is used
  augmented_input_text <- c(title_page, input_text[(yaml_delimiters[2] + 1):length(input_text)])
  
  # Add modifications to input file
  augmented_input_text <- c("---", yaml::as.yaml(yaml_params), "---", augmented_input_text)
  input_file_connection <- file(input_file, encoding = "UTF-8")
  writeLines(augmented_input_text, input_file_connection)
  close(input_file_connection)
  
  NULL
}

exam_paper_document <- function(output_format, ...) {
  config <- switch(
    output_format,
    "word" = rmarkdown::word_document(...),
    "pdf" = rmarkdown::pdf_document(...),
    "html" = rmarkdown::html_document(...)
  )
  
  # Preprocessor functions are adaptations from the RMarkdown package
  # (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir, from = .from) {
    # save files dir (for generating intermediates)
    saved_files_dir <<- files_dir
    
    args <- teachR:::sus_pre_processor(metadata, input_file, runtime, knit_meta, files_dir, output_dir, from)
    args
  }
  
  config$pre_processor <- pre_processor
  config
}

