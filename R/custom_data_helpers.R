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

suggest_custom_geography_type <- function(
    values
) {
  
  lookup <- read_custom_geography_lookup()
  
  values <- unique(
    trimws(
      as.character(values)
    )
  )
  
  values <- values[
    !is.na(values) &
      nzchar(values)
  ]
  
  #
  # Northern Ireland is valid alongside any supported
  # geography type, so exclude it when inferring type.
  #
  ni_codes <- lookup$geography_code[
    lookup$geography_type == "NI"
  ]
  
  values_to_match <- setdiff(
    values,
    ni_codes
  )
  
  if (length(values_to_match) == 0) {
    return("")
  }
  
  geography_types <- c(
    "AA",
    "AA2024",
    "LGD2014",
    "HSCT"
  )
  
  match_counts <- vapply(
    geography_types,
    function(geography_type) {
      
      valid_codes <- lookup$geography_code[
        lookup$geography_type == geography_type
      ]
      
      sum(
        values_to_match %in%
          valid_codes
      )
    },
    integer(1)
  )
  
  best_match <- geography_types[
    which.max(
      match_counts
    )
  ]
  
  #
  # Only suggest a type if every non-NI geography code
  # belongs to that geography type.
  #
  if (
    max(match_counts) ==
    length(values_to_match)
  ) {
    return(best_match)
  }
  
  ""
}

build_custom_dataset_metadata <- function(
    import_data
) {
  
  if (is.null(import_data$dataset_name)) {
    stop("The custom dataset name is missing.")
  }
  
  if (is.null(import_data$dataset_title)) {
    stop("The custom dataset title is missing.")
  }
  
  if (is.null(import_data$updated)) {
    stop("The custom dataset updated date is missing.")
  }
  
  if (is.null(import_data$variable_types)) {
    stop("The custom dataset variables have not been classified.")
  }
  
  csv_data <- import_data$data
  variable_types <- import_data$variable_types
  
  variables <- list()
  
  #
  # Categorical variables
  #
  categorical_columns <- names(
    variable_types[
      variable_types == "categorical"
    ]
  )
  
  for (column_name in categorical_columns) {
    
    category_values <- import_data$category_orders[[
      column_name
    ]]
    
    value_codes <- as.character(
      seq_along(category_values)
    )
    
    values <- stats::setNames(
      as.list(category_values),
      value_codes
    )
    
    variable_code <- toupper(
      gsub(
        "[^A-Za-z0-9]",
        "",
        column_name
      )
    )
    
    variables[[length(variables) + 1L]] <- list(
      code = variable_code,
      name = column_name,
      type = "categorical",
      values = values
    )
  }
  
  #
  # Date / time variables
  #
  date_columns <- names(
    variable_types[
      variable_types == "date"
    ]
  )
  
  frequency_codes <- c(
    yearly = "TLIST(A1)",
    quarterly = "TLIST(Q1)",
    monthly = "TLIST(M1)",
    weekly = "TLIST(W1)"
  )
  
  for (column_name in date_columns) {
    
    frequency <- import_data$date_frequencies[[
      column_name
    ]]
    
    variable_code <- unname(
      frequency_codes[[
        frequency
      ]]
    )
    
    date_values <- unique(
      as.character(
        csv_data[[
          column_name
        ]]
      )
    )
    
    date_values <- date_values[
      !is.na(date_values) &
        nzchar(trimws(date_values))
    ]
    
    values <- stats::setNames(
      as.list(date_values),
      date_values
    )
    
    variables[[length(variables) + 1L]] <- list(
      code = variable_code,
      name = column_name,
      type = "date",
      values = values
    )
  }
  
  #
  # Geography variables
  #
  geography_columns <- names(
    variable_types[
      variable_types == "geography"
    ]
  )
  
  if (length(geography_columns) > 0) {
    
    geography_lookup <- read_custom_geography_lookup()
    
    for (column_name in geography_columns) {
      
      geography_type <- import_data$geography_types[[
        column_name
      ]]
      
      geography_codes <- unique(
        trimws(
          as.character(
            csv_data[[
              column_name
            ]]
          )
        )
      )
      
      geography_codes <- geography_codes[
        !is.na(geography_codes) &
          nzchar(geography_codes)
      ]
      
      relevant_lookup <- geography_lookup[
        geography_lookup$geography_type %in%
          c(
            geography_type,
            "NI"
          ) &
          geography_lookup$geography_code %in%
          geography_codes,
        ,
        drop = FALSE
      ]
      
      geography_names <- stats::setNames(
        relevant_lookup$geography_name,
        relevant_lookup$geography_code
      )
      
      values <- stats::setNames(
        as.list(
          unname(
            geography_names[
              geography_codes
            ]
          )
        ),
        geography_codes
      )
      
      variables[[length(variables) + 1L]] <- list(
        code = geography_type,
        name = column_name,
        type = "geography",
        values = values
      )
    }
  }
  
  #
  # All numeric CSV columns are represented by one
  # synthetic pivot variable called Values.
  #
  numeric_columns <- names(
    variable_types[
      variable_types == "numeric"
    ]
  )
  
  if (length(numeric_columns) == 0) {
    stop(
      "At least one Numeric variable is required."
    )
  }
  
  numeric_codes <- as.character(
    seq_along(
      numeric_columns
    )
  )
  
  variables[[length(variables) + 1L]] <- list(
    code = "VALUES",
    name = "Values",
    type = "numeric",
    values = stats::setNames(
      as.list(
        numeric_columns
      ),
      numeric_codes
    )
  )
  
  list(
    source = "custom",
    label = import_data$dataset_title,
    updated = import_data$updated,
    variables = variables
  )
}