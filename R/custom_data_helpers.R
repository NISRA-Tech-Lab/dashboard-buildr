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

read_custom_csv <- function(
    path
) {
  
  if (!file.exists(path)) {
    stop("The CSV file was not found.")
  }
  
  #
  # Inspect the first part of the file as raw bytes.
  #
  connection <- file(
    path,
    open = "rb"
  )
  
  first_bytes <- readBin(
    connection,
    what = "raw",
    n = 200
  )
  
  close(connection)
  
  byte_values <- as.integer(
    first_bytes
  )
  
  file_encoding <- "UTF-8"
  
  #
  # UTF-16 LE BOM
  #
  if (
    length(byte_values) >= 2 &&
    identical(
      byte_values[1:2],
      c(255L, 254L)
    )
  ) {
    
    file_encoding <- "UTF-16LE"
    
    #
    # UTF-16 BE BOM
    #
  } else if (
    length(byte_values) >= 2 &&
    identical(
      byte_values[1:2],
      c(254L, 255L)
    )
  ) {
    
    file_encoding <- "UTF-16BE"
    
  } else if (
    length(byte_values) >= 4
  ) {
    
    even_positions <- byte_values[
      seq(
        2,
        length(byte_values),
        by = 2
      )
    ]
    
    odd_positions <- byte_values[
      seq(
        1,
        length(byte_values),
        by = 2
      )
    ]
    
    even_zero_rate <- mean(
      even_positions == 0
    )
    
    odd_zero_rate <- mean(
      odd_positions == 0
    )
    
    if (
      is.finite(even_zero_rate) &&
      even_zero_rate > 0.3
    ) {
      
      file_encoding <- "UTF-16LE"
      
    } else if (
      is.finite(odd_zero_rate) &&
      odd_zero_rate > 0.3
    ) {
      
      file_encoding <- "UTF-16BE"
    }
  }
  
  #
  # UTF-16 is safer if decoded to character text first,
  # then passed to the CSV parser.
  #
  if (
    file_encoding %in% c(
      "UTF-16LE",
      "UTF-16BE"
    )
  ) {
    
    text_lines <- readLines(
      path,
      encoding = file_encoding,
      warn = FALSE,
      skipNul = TRUE
    )
    
    text_lines <- enc2utf8(
      text_lines
    )
    
    csv_connection <- textConnection(
      text_lines
    )
    
    on.exit(
      close(csv_connection),
      add = TRUE
    )
    
    data <- utils::read.csv(
      csv_connection,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
  } else {
    
    data <- utils::read.csv(
      path,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8"
    )
  }
  
  names(data) <- trimws(
    names(data)
  )
  
  data
}

suggest_custom_variable_type <- function(
    values,
    variable_name = ""
) {
  
  variable_name_lower <- tolower(
    variable_name
  )
  
  #
  # Date/time variable names take priority over the
  # underlying R data type. This prevents numeric years
  # from being automatically classified as Numeric.
  #
  if (
    grepl(
      "\\byear\\b",
      variable_name_lower
    )
  ) {
    return("date")
  }
  
  if (
    grepl(
      "\\bquarter\\b",
      variable_name_lower
    )
  ) {
    return("date")
  }
  
  if (
    grepl(
      "\\bmonth\\b",
      variable_name_lower
    )
  ) {
    return("date")
  }
  
  if (
    grepl(
      "\\bweek\\b",
      variable_name_lower
    )
  ) {
    return("date")
  }
  
  non_missing <- values[
    !is.na(values)
  ]
  
  if (length(non_missing) == 0) {
    return("categorical")
  }
  
  if (
    is.numeric(values) ||
    is.integer(values)
  ) {
    return("numeric")
  }
  
  "categorical"
}

suggest_custom_date_frequency <- function(
    variable_name
) {
  
  variable_name_lower <- tolower(
    trimws(
      as.character(variable_name)
    )
  )
  
  if (
    grepl(
      "\\byear\\b",
      variable_name_lower
    )
  ) {
    return("yearly")
  }
  
  if (
    grepl(
      "\\bquarter\\b",
      variable_name_lower
    )
  ) {
    return("quarterly")
  }
  
  if (
    grepl(
      "\\bmonth\\b",
      variable_name_lower
    )
  ) {
    return("monthly")
  }
  
  if (
    grepl(
      "\\bweek\\b",
      variable_name_lower
    )
  ) {
    return("weekly")
  }
  
  #
  # If the column was manually classified as Date / time
  # and its name gives us no clue, default to Yearly.
  # The user can change this in the configuration modal.
  #
  "yearly"
}

read_custom_geography_lookup <- function() {
  
  lookup_path <- system.file(
    "extdata",
    "geography_lookup.csv",
    package = "dashboardBuildR"
  )
  
  if (!nzchar(lookup_path)) {
    stop(
      "The geography lookup file could not be found."
    )
  }
  
  lookup <- utils::read.csv(
    lookup_path,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  required_columns <- c(
    "geography_type",
    "geography_code",
    "geography_name"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(lookup)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste(
        "The geography lookup is missing:",
        paste(
          missing_columns,
          collapse = ", "
        )
      )
    )
  }
  
  lookup$geography_type <- trimws(
    as.character(
      lookup$geography_type
    )
  )
  
  lookup$geography_code <- trimws(
    as.character(
      lookup$geography_code
    )
  )
  
  lookup$geography_name <- trimws(
    as.character(
      lookup$geography_name
    )
  )
  
  lookup
}

custom_geography_choices <- function() {
  
  c(
    "Assembly Area" = "AA",
    "Assembly Area (2024)" = "AA2024",
    "Local Government District" = "LGD2014",
    "Health and Social Care Trust" = "HSCT"
  )
}