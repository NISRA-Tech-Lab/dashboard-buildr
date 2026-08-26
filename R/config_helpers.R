format_navigation_js <- function(navigation) {
  
  json <- jsonlite::toJSON(
    navigation[, c("href", "text"), drop = FALSE],
    dataframe = "rows",
    auto_unbox = TRUE,
    pretty = TRUE
  )
  
  paste0('"navigation": ', json)
}


replace_navigation_in_config <- function(config_text, navigation) {
  
  pattern <- paste0(
    '["\']navigation["\']\\s*:\\s*',
    '\\[(?:[^\\[\\]]|\\[[^\\[\\]]*\\])*\\]'
  )
  
  match <- regexpr(
    pattern,
    config_text,
    perl = TRUE
  )
  
  if (match[1] == -1) {
    stop("Could not locate the navigation array in config.js.")
  }
  
  start <- as.integer(match[1])
  match_length <- as.integer(attr(match, "match.length"))
  end <- start + match_length - 1
  
  replacement <- format_navigation_js(navigation)
  
  paste0(
    substr(config_text, 1, start - 1),
    replacement,
    substr(config_text, end + 1, nchar(config_text))
  )
}

format_matrix_js <- function(matrices) {
  
  matrices <- trimws(as.character(matrices))
  matrices <- matrices[nzchar(matrices)]
  matrices <- unique(matrices)
  
  json <- jsonlite::toJSON(
    matrices,
    auto_unbox = FALSE,
    pretty = TRUE
  )
  
  paste0('"matrix": ', json)
}


replace_matrix_in_config <- function(config_text, matrices) {
  
  pattern <- paste0(
    '["\']matrix["\']\\s*:\\s*',
    '(?:',
    '\\[(?:[^\\[\\]]|\\[[^\\[\\]]*\\])*\\]',
    '|',
    '"(?:\\\\.|[^"\\\\])*"',
    ')'
  )
  
  match <- regexpr(
    pattern,
    config_text,
    perl = TRUE
  )
  
  if (match[1] == -1) {
    stop("Could not locate the matrix property in config.js.")
  }
  
  start <- as.integer(match[1])
  match_length <- as.integer(attr(match, "match.length"))
  end <- start + match_length - 1
  
  replacement <- format_matrix_js(matrices)
  
  paste0(
    substr(config_text, 1, start - 1),
    replacement,
    substr(config_text, end + 1, nchar(config_text))
  )
}

format_custom_js <- function(custom_tables) {
  
  custom_tables <- trimws(
    as.character(custom_tables)
  )
  
  custom_tables <- custom_tables[
    nzchar(custom_tables)
  ]
  
  custom_tables <- unique(
    custom_tables
  )
  
  json <- jsonlite::toJSON(
    custom_tables,
    auto_unbox = FALSE,
    pretty = TRUE
  )
  
  paste0(
    '"custom": ',
    json
  )
}


replace_custom_in_config <- function(
    config_text,
    custom_tables
) {
  
  pattern <- paste0(
    '["\']custom["\']\\s*:\\s*',
    '(?:',
    '\\[(?:[^\\[\\]]|\\[[^\\[\\]]*\\])*\\]',
    '|',
    '"(?:\\\\.|[^"\\\\])*"',
    ')'
  )
  
  match <- regexpr(
    pattern,
    config_text,
    perl = TRUE
  )
  
  replacement <- format_custom_js(
    custom_tables
  )
  
  #
  # Older dashboards may not yet contain a custom property.
  # In that case insert it immediately after matrix.
  #
  if (match[1] == -1) {
    
    matrix_pattern <- paste0(
      '(["\']matrix["\']\\s*:\\s*',
      '(?:',
      '\\[(?:[^\\[\\]]|\\[[^\\[\\]]*\\])*\\]',
      '|',
      '"(?:\\\\.|[^"\\\\])*"',
      ')',
      ')'
    )
    
    matrix_match <- regexpr(
      matrix_pattern,
      config_text,
      perl = TRUE
    )
    
    if (matrix_match[1] == -1) {
      stop(
        "Could not locate the matrix property in config.js."
      )
    }
    
    start <- as.integer(
      matrix_match[1]
    )
    
    match_length <- as.integer(
      attr(
        matrix_match,
        "match.length"
      )
    )
    
    end <- start + match_length - 1
    
    return(
      paste0(
        substr(
          config_text,
          1,
          end
        ),
        ",\n\n    ",
        replacement,
        substr(
          config_text,
          end + 1,
          nchar(config_text)
        )
      )
    )
  }
  
  start <- as.integer(
    match[1]
  )
  
  match_length <- as.integer(
    attr(
      match,
      "match.length"
    )
  )
  
  end <- start + match_length - 1
  
  paste0(
    substr(
      config_text,
      1,
      start - 1
    ),
    replacement,
    substr(
      config_text,
      end + 1,
      nchar(config_text)
    )
  )
}