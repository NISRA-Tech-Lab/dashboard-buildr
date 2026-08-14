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

detect_dashboard_project_root <- function(
    path = getwd()
) {
  
  path <- normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )
  
  if (!dir.exists(path)) {
    return(NULL)
  }
  
  rproj_files <- list.files(
    path,
    pattern = "\\.Rproj$",
    full.names = TRUE
  )
  
  required_files <- c(
    "index.html",
    file.path(
      "src",
      "config",
      "config.js"
    ),
    file.path(
      "src",
      "utils",
      "page-layout.js"
    )
  )
  
  has_rproj <-
    length(rproj_files) > 0
  
  has_dashboard_files <- all(
    file.exists(
      file.path(
        path,
        required_files
      )
    )
  )
  
  if (
    isTRUE(has_rproj) &&
    isTRUE(has_dashboard_files)
  ) {
    return(path)
  }
  
  NULL
}