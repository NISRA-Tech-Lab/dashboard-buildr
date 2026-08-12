read_card_calculation_data <- function(
    project_root,
    matrix
) {
  
  csv_path <- file.path(
    project_root,
    "public",
    "data",
    paste0(matrix, ".csv")
  )
  
  metadata_path <- file.path(
    project_root,
    "public",
    "data",
    "data.json"
  )
  
  if (!file.exists(csv_path)) {
    stop(
      paste0(
        "The CSV file was not found: ",
        basename(csv_path)
      )
    )
  }
  
  if (!file.exists(metadata_path)) {
    stop("public/data/data.json was not found.")
  }
  
  metadata_text <- paste(
    readLines(
      metadata_path,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
  
  if (!nzchar(trimws(metadata_text))) {
    stop("public/data/data.json is empty.")
  }
  
  metadata <- jsonlite::fromJSON(
    metadata_text,
    simplifyVector = FALSE
  )
  
  matrix_metadata <- metadata[[matrix]]
  
  if (is.null(matrix_metadata)) {
    stop(
      paste0(
        "No metadata was found for matrix ",
        matrix,
        "."
      )
    )
  }
  
  variables <- matrix_metadata$variables
  
  if (
    is.null(variables) ||
    length(variables) == 0
  ) {
    stop(
      paste0(
        "No variable metadata was found for ",
        matrix,
        "."
      )
    )
  }
  
  table_data <- read.csv(
    csv_path,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    encoding = "UTF-8"
  )
  
  if (ncol(table_data) == 0) {
    stop("The selected CSV contains no columns.")
  }
  
  # The final variable is pivoted into several CSV columns.
  pivot_variable <- variables[[length(variables)]]
  
  pivot_labels <- unname(
    unlist(
      pivot_variable$values,
      use.names = FALSE
    )
  )
  
  pivot_labels <- as.character(
    pivot_labels
  )
  
  pivot_columns <- intersect(
    pivot_labels,
    names(table_data)
  )
  
  if (length(pivot_columns) == 0) {
    stop(
      paste0(
        "None of the values for the final variable '",
        pivot_variable$name,
        "' were found as columns in the CSV."
      )
    )
  }
  
  row_variables <- if (length(variables) > 1) {
    variables[
      seq_len(length(variables) - 1)
    ]
  } else {
    list()
  }
  
  row_filters <- list()
  
  for (variable_number in seq_along(row_variables)) {
    
    variable <- row_variables[[variable_number]]
    column_name <- as.character(variable$name)[1]
    
    if (!column_name %in% names(table_data)) {
      next
    }
    
    column_values <- table_data[[column_name]]
    
    distinct_values <- unique(
      as.character(column_values)
    )
    
    distinct_values <- distinct_values[
      !is.na(distinct_values) &
        nzchar(trimws(distinct_values))
    ]
    
    # Do not create a filter where every row has the same value.
    if (length(distinct_values) <= 1) {
      next
    }
    
    variable_code <- as.character(
      variable$code
    )[1]
    
    is_year_variable <- grepl(
      "^TLIST\\((A1|Q1|M1|W1)\\)$",
      variable_code
    )
    
    filter_choices <- distinct_values
    
    if (is_year_variable) {
      special_year_choices <- c(
        "Latest year" = "__LATEST_YEAR__",
        "Previous year" = "__PREVIOUS_YEAR__",
        "Earliest year" = "__EARLIEST_YEAR__"
      )
      
      ordinary_year_choices <- stats::setNames(
        distinct_values,
        distinct_values
      )
      
      filter_choices <- c(
        special_year_choices,
        ordinary_year_choices
      )
    }
    
    row_filters[[length(row_filters) + 1]] <- list(
      column = column_name,
      label = column_name,
      code = variable_code,
      is_year = is_year_variable,
      choices = filter_choices,
      input_id = paste0(
        "calculation_filter_",
        length(row_filters) + 1
      )
    )
  }
  
  list(
    matrix = matrix,
    label = as.character(matrix_metadata$label)[1],
    data = table_data,
    row_filters = row_filters,
    pivot_label = as.character(pivot_variable$name)[1],
    pivot_columns = pivot_columns
  )
}


filter_card_calculation_data <- function(
    calculation_data,
    selections
) {
  filtered_data <- calculation_data$data
  
  for (filter_definition in calculation_data$row_filters) {
    selected_values <- selections[[
      filter_definition$input_id
    ]]
    
    if (is.null(selected_values)) {
      next
    }
    
    selected_values <- as.character(
      selected_values
    )
    
    # Empty selection means this variable is not filtered.
    if (length(selected_values) == 0) {
      next
    }
    
    if (isTRUE(filter_definition$is_year)) {
      selected_values <- resolve_year_filter_values(
        column_values = filtered_data[[
          filter_definition$column
        ]],
        selected_values = selected_values
      )
    }
    
    filtered_data <- filtered_data[
      as.character(
        filtered_data[[
          filter_definition$column
        ]]
      ) %in% selected_values,
      ,
      drop = FALSE
    ]
  }
  
  filtered_data
}


calculate_card_value <- function(
    calculation_data,
    filtered_data,
    selected_columns
) {
  
  selected_columns <- as.character(
    selected_columns
  )
  
  selected_columns <- intersect(
    selected_columns,
    calculation_data$pivot_columns
  )
  
  if (length(selected_columns) == 0) {
    return(NA_real_)
  }
  
  if (nrow(filtered_data) == 0) {
    return(NA_real_)
  }
  
  selected_data <- filtered_data[
    ,
    selected_columns,
    drop = FALSE
  ]
  
  numeric_values <- lapply(
    selected_data,
    function(column) {
      
      if (is.numeric(column)) {
        return(column)
      }
      
      suppressWarnings(
        as.numeric(
          gsub(
            ",",
            "",
            as.character(column),
            fixed = TRUE
          )
        )
      )
    }
  )
  
  numeric_values <- unlist(
    numeric_values,
    use.names = FALSE
  )
  
  if (
    length(numeric_values) == 0 ||
    all(is.na(numeric_values))
  ) {
    return(NA_real_)
  }
  
  sum(
    numeric_values,
    na.rm = TRUE
  )
}


format_card_calculation_value <- function(
    value,
    decimal_places = 0,
    comma_separator = FALSE
) {
  
  if (
    length(value) != 1 ||
    is.na(value) ||
    !is.finite(value)
  ) {
    return("")
  }
  
  decimal_places <- as.integer(
    decimal_places
  )
  
  if (
    is.na(decimal_places) ||
    decimal_places < 0
  ) {
    decimal_places <- 0L
  }
  
  formatC(
    value,
    format = "f",
    digits = decimal_places,
    big.mark = if (isTRUE(comma_separator)) {
      ","
    } else {
      ""
    }
  )
}

resolve_year_filter_values <- function(
    column_values,
    selected_values
) {
  selected_values <- as.character(
    selected_values
  )
  
  special_tokens <- c(
    "__LATEST_YEAR__",
    "__PREVIOUS_YEAR__",
    "__EARLIEST_YEAR__"
  )
  
  if (!any(selected_values %in% special_tokens)) {
    return(selected_values)
  }
  
  available_values <- unique(
    as.character(column_values)
  )
  
  available_values <- available_values[
    !is.na(available_values) &
      nzchar(trimws(available_values))
  ]
  
  if (length(available_values) == 0) {
    return(
      setdiff(
        selected_values,
        special_tokens
      )
    )
  }
  
  numeric_years <- suppressWarnings(
    as.numeric(available_values)
  )
  
  if (all(!is.na(numeric_years))) {
    ordered_values <- available_values[
      order(
        numeric_years,
        decreasing = FALSE
      )
    ]
  } else {
    ordered_values <- sort(
      available_values,
      decreasing = FALSE
    )
  }
  
  earliest_year <- ordered_values[1]
  latest_year <- ordered_values[
    length(ordered_values)
  ]
  
  previous_year <- if (length(ordered_values) >= 2) {
    ordered_values[
      length(ordered_values) - 1
    ]
  } else {
    latest_year
  }
  
  resolved_values <- setdiff(
    selected_values,
    special_tokens
  )
  
  if ("__LATEST_YEAR__" %in% selected_values) {
    resolved_values <- c(
      resolved_values,
      latest_year
    )
  }
  
  if ("__PREVIOUS_YEAR__" %in% selected_values) {
    resolved_values <- c(
      resolved_values,
      previous_year
    )
  }
  
  if ("__EARLIEST_YEAR__" %in% selected_values) {
    resolved_values <- c(
      resolved_values,
      earliest_year
    )
  }
  
  unique(resolved_values)
}

matrix_year_values <- function(
    calculation_data
) {
  
  row_filters <- calculation_data$row_filters
  
  year_definitions <- Filter(
    function(filter_definition) {
      isTRUE(filter_definition$is_year)
    },
    row_filters
  )
  
  if (length(year_definitions) > 0) {
    
    year_column <- year_definitions[[1]]$column
    
  } else {
    
    year_candidates <- names(
      calculation_data$data
    )[
      grepl(
        "year",
        names(calculation_data$data),
        ignore.case = TRUE
      )
    ]
    
    if (length(year_candidates) == 0) {
      return(NULL)
    }
    
    year_column <- year_candidates[[1]]
  }
  
  years <- unique(
    calculation_data$data[[
      year_column
    ]]
  )
  
  years <- years[
    !is.na(years)
  ]
  
  numeric_years <- suppressWarnings(
    as.numeric(years)
  )
  
  if (
    length(years) > 0 &&
    all(!is.na(numeric_years))
  ) {
    years <- years[
      order(numeric_years)
    ]
  } else {
    years <- sort(
      as.character(years)
    )
  }
  
  list(
    column = year_column,
    years = years,
    first = years[[1]],
    latest = years[[length(years)]],
    last = if (length(years) >= 2) {
      years[[length(years) - 1]]
    } else {
      years[[1]]
    }
  )
}

build_matrix_year_variables_js <- function(
    matrix
) {
  
  c(
    paste0(
      "    // BuildR ",
      matrix,
      " year variables start"
    ),
    
    paste0(
      "    const ",
      matrix,
      "_year_column = ",
      matrix,
      "_meta.variables"
    ),
    '        .filter(x => x["code"].includes("TLIST"))',
    '        .map(x => x["name"])[0];',
    "",
    
    paste0(
      "    let ",
      matrix,
      "_years = ",
      matrix,
      "_data"
    ),
    paste0(
      "        .sort((a, b) => a[",
      matrix,
      "_year_column] - b[",
      matrix,
      "_year_column])"
    ),
    paste0(
      "        .map(row => row[",
      matrix,
      "_year_column]);"
    ),
    "",
    
    paste0(
      "    ",
      matrix,
      "_years = [...new Set(",
      matrix,
      "_years)];"
    ),
    "",
    
    paste0(
      "    const ",
      matrix,
      "_first_year = ",
      matrix,
      "_years[0];"
    ),
    
    paste0(
      "    const ",
      matrix,
      "_latest_year = ",
      matrix,
      "_years[",
      matrix,
      "_years.length - 1];"
    ),
    
    paste0(
      "    const ",
      matrix,
      "_last_year = ",
      matrix,
      "_years.length >= 2 ? ",
      matrix,
      "_years[",
      matrix,
      "_years.length - 2] : ",
      matrix,
      "_latest_year;"
    ),
    
    paste0(
      "    // BuildR ",
      matrix,
      " year variables end"
    ),
    ""
  )
}

javascript_dynamic_year_value <- function(
    value,
    year_prefix = NULL
) {
  
  prefix <- if (
    is.null(year_prefix) ||
    !nzchar(year_prefix)
  ) {
    ""
  } else {
    paste0(
      year_prefix,
      "_"
    )
  }
  
  if (identical(value, "__LATEST_YEAR__")) {
    return(
      paste0(
        prefix,
        "latest_year"
      )
    )
  }
  
  if (identical(value, "__PREVIOUS_YEAR__")) {
    return(
      paste0(
        prefix,
        "last_year"
      )
    )
  }
  
  if (identical(value, "__EARLIEST_YEAR__")) {
    return(
      paste0(
        prefix,
        "first_year"
      )
    )
  }
  
  NULL
}

page_update_year_spans_matrix <- function(
    js_lines
) {
  
  matches <- grep(
    paste0(
      "^\\s*updateYearSpans\\s*\\(",
      "\\s*([A-Za-z0-9_]+)_data\\s*,",
      "\\s*\\1_meta\\s*\\)\\s*;?\\s*$"
    ),
    js_lines,
    value = TRUE,
    perl = TRUE
  )
  
  if (length(matches) == 0) {
    return(NULL)
  }
  
  sub(
    paste0(
      "^\\s*updateYearSpans\\s*\\(",
      "\\s*([A-Za-z0-9_]+)_data.*$"
    ),
    "\\1",
    matches[[1]],
    perl = TRUE
  )
}

matrix_needs_own_year_variables <- function(
    project_root,
    matrix,
    js_lines
) {
  
  owner_matrix <- page_update_year_spans_matrix(
    js_lines
  )
  
  #
  # No owner yet, or this matrix is the owner:
  # use the normal global year variables.
  #
  if (
    is.null(owner_matrix) ||
    !nzchar(owner_matrix) ||
    identical(owner_matrix, matrix)
  ) {
    return(FALSE)
  }
  
  owner_data <- tryCatch(
    read_card_calculation_data(
      project_root = project_root,
      matrix = owner_matrix
    ),
    error = function(e) NULL
  )
  
  current_data <- tryCatch(
    read_card_calculation_data(
      project_root = project_root,
      matrix = matrix
    ),
    error = function(e) NULL
  )
  
  if (
    is.null(owner_data) ||
    is.null(current_data)
  ) {
    return(FALSE)
  }
  
  owner_years <- matrix_year_values(
    owner_data
  )
  
  current_years <- matrix_year_values(
    current_data
  )
  
  if (
    is.null(owner_years) ||
    is.null(current_years)
  ) {
    return(FALSE)
  }
  
  !identical(
    as.character(owner_years$latest),
    as.character(current_years$latest)
  )
}