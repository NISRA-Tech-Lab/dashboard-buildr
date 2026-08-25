suggest_custom_dataset_name <- function(
    project_root,
    config
) {
  
  existing_names <- character()
  
  if (
    !is.null(config$custom) &&
    length(config$custom) > 0
  ) {
    existing_names <- c(
      existing_names,
      as.character(
        unlist(
          config$custom,
          use.names = FALSE
        )
      )
    )
  }
  
  data_dir <- file.path(
    project_root,
    "public",
    "data"
  )
  
  if (dir.exists(data_dir)) {
    
    csv_files <- list.files(
      data_dir,
      pattern = "\\.csv$",
      ignore.case = TRUE
    )
    
    csv_names <- sub(
      "\\.csv$",
      "",
      csv_files,
      ignore.case = TRUE
    )
    
    existing_names <- c(
      existing_names,
      csv_names
    )
  }
  
  existing_names <- unique(
    toupper(
      trimws(
        existing_names
      )
    )
  )
  
  number <- 1L
  
  repeat {
    
    candidate <- sprintf(
      "MYDATA%02d",
      number
    )
    
    if (!candidate %in% existing_names) {
      return(candidate)
    }
    
    number <- number + 1L
  }
}

validate_custom_dataset_name <- function(
    dataset_name,
    project_root,
    config
) {
  
  dataset_name <- toupper(
    trimws(
      as.character(dataset_name)[1]
    )
  )
  
  if (!nzchar(dataset_name)) {
    return(
      list(
        valid = FALSE,
        message = "Enter a short dataset name."
      )
    )
  }
  
  if (nchar(dataset_name) > 10) {
    return(
      list(
        valid = FALSE,
        message = "The dataset name must be no more than 10 characters."
      )
    )
  }
  
  if (!grepl(
    "^[A-Z0-9]+$",
    dataset_name
  )) {
    return(
      list(
        valid = FALSE,
        message = paste(
          "The dataset name must contain only",
          "uppercase letters and numbers, with no spaces."
        )
      )
    )
  }
  
  existing_names <- character()
  
  if (
    !is.null(config$custom) &&
    length(config$custom) > 0
  ) {
    existing_names <- c(
      existing_names,
      as.character(
        unlist(
          config$custom,
          use.names = FALSE
        )
      )
    )
  }
  
  csv_path <- file.path(
    project_root,
    "public",
    "data",
    paste0(
      dataset_name,
      ".csv"
    )
  )
  
  if (
    dataset_name %in% toupper(existing_names) ||
    file.exists(csv_path)
  ) {
    return(
      list(
        valid = FALSE,
        message = "That dataset name is already in use."
      )
    )
  }
  
  list(
    valid = TRUE,
    name = dataset_name
  )
}