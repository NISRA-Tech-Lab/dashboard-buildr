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
    
    row_filters[[length(row_filters) + 1]] <- list(
      column = column_name,
      label = column_name,
      choices = distinct_values,
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
    
    if (length(selected_values) == 0) {
      return(
        filtered_data[0, , drop = FALSE]
      )
    }
    
    filtered_data <- filtered_data[
      as.character(
        filtered_data[[filter_definition$column]]
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