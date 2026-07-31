replace_file_text <- function(
    file_path,
    pattern,
    replacement,
    fixed = FALSE
) {
  
  if (!file.exists(file_path)) {
    stop(
      "The file was not found: ",
      file_path
    )
  }
  
  contents <- readLines(
    file_path,
    warn = FALSE,
    encoding = "UTF-8"
  ) |>
    paste(collapse = "\n")
  
  pattern_found <- grepl(
    pattern = pattern,
    x = contents,
    fixed = fixed,
    perl = !fixed
  )
  
  if (!pattern_found) {
    stop(
      "Could not find the expected text in: ",
      file_path
    )
  }
  
  updated_contents <- sub(
    pattern = pattern,
    replacement = replacement,
    x = contents,
    fixed = fixed,
    perl = !fixed
  )
  
  writeLines(
    updated_contents,
    file_path,
    useBytes = TRUE
  )
  
  invisible(TRUE)
}