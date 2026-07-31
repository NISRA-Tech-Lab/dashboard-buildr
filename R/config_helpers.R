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