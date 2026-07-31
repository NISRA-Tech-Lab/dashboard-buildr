verify_matrix <- function(matrix_name) {
  
  matrix_name <- toupper(trimws(matrix_name))
  
  if (!nzchar(matrix_name)) {
    return(
      list(
        valid = FALSE,
        matrix = matrix_name,
        message = "Enter a matrix name."
      )
    )
  }
  
  url <- paste0(
    "https://ws-data.nisra.gov.uk/public/api.restful/",
    "PxStat.Data.Cube_API.ReadDataset/",
    utils::URLencode(matrix_name, reserved = TRUE),
    "/JSON-stat/2.0/en"
  )
  
  result <- tryCatch({
    
    response <- httr2::request(url) |>
      httr2::req_timeout(seconds = 15) |>
      httr2::req_error(is_error = function(response) FALSE) |>
      httr2::req_perform()
    
    status <- httr2::resp_status(response)
    body <- httr2::resp_body_string(response)
    
    if (status < 200 || status >= 300) {
      return(
        list(
          valid = FALSE,
          matrix = matrix_name,
          message = paste0(
            "Matrix '",
            matrix_name,
            "' was not found on the Data Portal."
          )
        )
      )
    }
    
    response_text <- httr2::resp_body_string(response)
    
    dataset <- tryCatch(
      jsonlite::fromJSON(
        response_text,
        simplifyVector = FALSE
      ),
      error = function(error) NULL
    )
    
    if (is.null(dataset)) {
      return(
        list(
          valid = FALSE,
          matrix = matrix_name,
          message = "The Data Portal returned an invalid response."
        )
      )
    }
    
    dataset_class <- dataset$class
    
    if (is.list(dataset_class)) {
      dataset_class <- unlist(
        dataset_class,
        use.names = FALSE
      )
    }
    
    returned_matrix <- dataset$extension$matrix
    
    if (is.null(returned_matrix)) {
      returned_matrix <- ""
    }
    
    returned_matrix <- toupper(
      as.character(returned_matrix)[1]
    )
    
    is_dataset <- "dataset" %in% as.character(dataset_class)
    
    if (
      !is_dataset ||
      !identical(returned_matrix, matrix_name)
    ) {
      return(
        list(
          valid = FALSE,
          matrix = matrix_name,
          message = paste0(
            "Matrix '",
            matrix_name,
            "' was not found on the Data Portal."
          )
        )
      )
    }
    
    list(
      valid = TRUE,
      matrix = matrix_name,
      message = paste0(
        "Matrix '",
        matrix_name,
        "' was verified."
      )
    )
    
  }, error = function(error) {
    
    list(
      valid = FALSE,
      matrix = matrix_name,
      message = paste0(
        "The Data Portal could not be reached: ",
        conditionMessage(error)
      )
    )
  })
  
  result
}