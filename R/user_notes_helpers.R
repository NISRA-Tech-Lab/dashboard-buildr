read_user_notes_html <- function(
    project_root
) {
  
  html_path <- file.path(
    project_root,
    "user-notes.html"
  )
  
  if (!file.exists(html_path)) {
    stop("user-notes.html was not found.")
  }
  
  html_lines <- readLines(
    html_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  region_start <- grep(
    "^\\s*<!--\\s*BuildR user notes start\\s*-->\\s*$",
    html_lines
  )
  
  region_end <- grep(
    "^\\s*<!--\\s*BuildR user notes end\\s*-->\\s*$",
    html_lines
  )
  
  if (
    length(region_start) != 1 ||
    length(region_end) != 1 ||
    region_end <= region_start
  ) {
    stop(
      "Could not uniquely identify the user notes HTML region."
    )
  }
  
  if (region_end == region_start + 1) {
    return("")
  }
  
  content <- html_lines[
    seq.int(
      region_start + 1,
      region_end - 1
    )
  ]
  
  #
  # Remove the template indentation while retaining
  # indentation within the user's HTML.
  #
  content <- sub(
    "^        ",
    "",
    content
  )
  
  paste(
    content,
    collapse = "\n"
  )
}

update_user_notes_html <- function(
    project_root,
    user_html
) {
  
  html_path <- file.path(
    project_root,
    "user-notes.html"
  )
  
  if (!file.exists(html_path)) {
    stop("user-notes.html was not found.")
  }
  
  html_lines <- readLines(
    html_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  region_start <- grep(
    "^\\s*<!--\\s*BuildR user notes start\\s*-->\\s*$",
    html_lines
  )
  
  region_end <- grep(
    "^\\s*<!--\\s*BuildR user notes end\\s*-->\\s*$",
    html_lines
  )
  
  if (
    length(region_start) != 1 ||
    length(region_end) != 1 ||
    region_end <= region_start
  ) {
    stop(
      "Could not uniquely identify the user notes HTML region."
    )
  }
  
  user_html <- as.character(
    user_html
  )[1]
  
  if (
    is.na(user_html) ||
    !nzchar(trimws(user_html))
  ) {
    content_lines <- character()
  } else {
    
    content_lines <- strsplit(
      user_html,
      "\n",
      fixed = TRUE
    )[[1]]
    
    content_lines <- paste0(
      "        ",
      content_lines
    )
  }
  
  updated_html <- c(
    html_lines[
      seq_len(region_start)
    ],
    
    content_lines,
    
    html_lines[
      region_end:length(html_lines)
    ]
  )
  
  writeLines(
    updated_html,
    html_path,
    useBytes = TRUE
  )
  
  invisible(html_path)
}