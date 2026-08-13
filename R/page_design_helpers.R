page_design_paths <- function(
    project_root,
    page_href
) {
  
  page_href <- basename(
    as.character(page_href)[1]
  )
  
  if (
    !nzchar(page_href) ||
    !grepl("\\.html$", page_href, ignore.case = TRUE)
  ) {
    stop("A valid page HTML file is required.")
  }
  
  html_path <- file.path(
    project_root,
    page_href
  )
  
  js_filename <- sub(
    "\\.html$",
    ".js",
    page_href,
    ignore.case = TRUE
  )
  
  js_path <- file.path(
    project_root,
    "src",
    js_filename
  )
  
  list(
    html = html_path,
    js = js_path,
    href = page_href,
    js_filename = js_filename
  )
}


read_page_strapline <- function(
    project_root,
    page_href
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$html)) {
    return("")
  }
  
  lines <- readLines(
    paths$html,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  start_line <- grep(
    'class=["\'][^"\']*\\bcurrent-page-strapline\\b',
    lines,
    perl = TRUE
  )
  
  if (length(start_line) != 1) {
    return("")
  }
  
  end_line <- find_closing_div(
    lines,
    start_line
  )
  
  fragment <- paste(
    lines[start_line:end_line],
    collapse = "\n"
  )
  
  fragment <- sub(
    "^.*?<div\\b[^>]*>",
    "",
    fragment,
    perl = TRUE
  )
  
  fragment <- sub(
    "</div>.*$",
    "",
    fragment,
    perl = TRUE
  )
  
  fragment <- trimws(fragment)
  
  if (!nzchar(fragment)) {
    return("")
  }
  
  trimws(
    xml2::xml_text(
      xml2::read_html(
        paste0(
          "<div>",
          fragment,
          "</div>"
        )
      )
    )
  )
}


update_page_strapline <- function(
    project_root,
    page_href,
    strapline
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$html)) {
    stop(
      paste0(
        paths$href,
        " was not found."
      )
    )
  }
  
  strapline <- trimws(
    as.character(strapline)
  )
  
  if (!nzchar(strapline)) {
    stop("The page strapline cannot be empty.")
  }
  
  lines <- readLines(
    paths$html,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  start_line <- grep(
    'class=["\'][^"\']*\\bcurrent-page-strapline\\b',
    lines,
    perl = TRUE
  )
  
  if (length(start_line) != 1) {
    stop(
      "Could not uniquely identify the .current-page-strapline element."
    )
  }
  
  end_line <- find_closing_div(
    lines,
    start_line
  )
  
  opening_tag <- sub(
    "^(.*?<div\\b[^>]*>).*",
    "\\1",
    lines[start_line],
    perl = TRUE
  )
  
  escaped <- as.character(
    htmltools::htmlEscape(
      strapline,
      attribute = FALSE
    )
  )
  
  replacement <- paste0(
    opening_tag,
    escaped,
    "</div>"
  )
  
  updated <- c(
    if (start_line > 1) {
      lines[seq_len(start_line - 1)]
    } else {
      character()
    },
    replacement,
    if (end_line < length(lines)) {
      lines[seq.int(end_line + 1, length(lines))]
    } else {
      character()
    }
  )
  
  writeLines(
    updated,
    paths$html,
    useBytes = TRUE
  )
  
  invisible(paths$html)
}


find_page_cards_row <- function(lines) {
  
  row_candidates <- grep(
    paste0(
      '<div\\b[^>]*class\\s*=\\s*["\']',
      '[^"\']*\\brow\\b',
      '[^"\']*\\btext-center\\b',
      '[^"\']*["\']'
    ),
    lines,
    perl = TRUE
  )
  
  if (length(row_candidates) == 0) {
    stop("Could not identify the page cards row.")
  }
  
  for (row_start in row_candidates) {
    
    row_end <- find_closing_div(
      lines,
      row_start
    )
    
    card_starts <- grep(
      paste0(
        '<div\\b[^>]*class\\s*=\\s*["\']',
        '[^"\']*\\bcol-6\\b',
        '[^"\']*\\bcol-xl\\b',
        '[^"\']*\\bpy-2\\b',
        '[^"\']*["\']'
      ),
      lines,
      perl = TRUE
    )
    
    card_starts <- card_starts[
      card_starts > row_start &
        card_starts < row_end
    ]
    
    if (length(card_starts) > 0) {
      return(
        list(
          row_start = row_start,
          row_end = row_end,
          card_starts = card_starts
        )
      )
    }
  }
  
  stop("Could not identify the page cards row.")
}


blank_page_card <- function(
    card_number,
    background = "blue"
) {
  
  background_class <- if (
    identical(background, "navy")
  ) {
    "navy-bg"
  } else {
    "blue-bg"
  }
  
  c(
    '            <div class="col-6 col-xl py-2">',
    paste0(
      '                <div class="card h-100 ',
      background_class,
      '">'
    ),
    '                    <div class="card-body d-flex flex-column justify-content-center">',
    '                    </div>',
    '                </div>',
    '            </div>'
  )
}


count_page_cards <- function(
    project_root,
    page_href
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$html)) {
    stop(
      paste0(
        paths$href,
        " was not found."
      )
    )
  }
  
  lines <- readLines(
    paths$html,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  length(
    find_page_cards_row(lines)$card_starts
  )
}


read_page_card_display_values <- function(
    project_root,
    page_href
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$js)) {
    return(character())
  }
  
  lines <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  markers <- grep(
    "^\\s*//\\s*Content for card\\s+[0-9]+\\s*$",
    lines
  )
  
  if (length(markers) == 0) {
    return(character())
  }
  
  closing <- grep(
    "^\\s*\\}\\)\\s*;?\\s*$",
    lines
  )
  
  if (length(closing) == 0) {
    return(character())
  }
  
  closing <- tail(
    closing,
    1
  )
  
  values <- character(
    length(markers)
  )
  
  for (i in seq_along(markers)) {
    
    section_end <- if (i < length(markers)) {
      markers[i + 1] - 1
    } else {
      closing - 1
    }
    
    section <- lines[
      markers[i]:section_end
    ]
    
    value_line <- grep(
      "^\\s*//\\s*BuildR display value:",
      section,
      value = TRUE
    )
    
    if (length(value_line) == 1) {
      values[i] <- trimws(
        sub(
          "^\\s*//\\s*BuildR display value:\\s*",
          "",
          value_line
        )
      )
    }
  }
  
  values
}


read_page_card_values <- function(
    project_root,
    page_href
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$html)) {
    stop(
      paste0(
        paths$href,
        " was not found."
      )
    )
  }
  
  lines <- readLines(
    paths$html,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  card_section <- find_page_cards_row(
    lines
  )
  
  display_values <- read_page_card_display_values(
    project_root,
    page_href
  )
  
  lapply(
    seq_along(card_section$card_starts),
    function(card_number) {
      
      card_start <- card_section$card_starts[
        card_number
      ]
      
      card_end <- find_closing_div(
        lines,
        card_start
      )
      
      fragment <- paste(
        lines[card_start:card_end],
        collapse = "\n"
      )
      
      document <- xml2::read_html(
        paste0(
          "<html><body>",
          fragment,
          "</body></html>"
        )
      )
      
      body <- xml2::xml_find_first(
        document,
        paste0(
          "//div[contains(concat(' ', ",
          "normalize-space(@class), ' '), ",
          "' card-body ')]"
        )
      )
      
      card <- xml2::xml_find_first(
        document,
        paste0(
          "//div[contains(concat(' ', ",
          "normalize-space(@class), ' '), ",
          "' card ')]"
        )
      )
      
      paragraph <- xml2::xml_find_first(
        body,
        ".//p"
      )
      
      value_span <- xml2::xml_find_first(
        body,
        paste0(
          ".//span[@id='card-",
          card_number,
          "-value']"
        )
      )
      
      unit_span <- xml2::xml_find_first(
        body,
        paste0(
          ".//span[contains(concat(' ', ",
          "normalize-space(@class), ' '), ",
          "' unit ')]"
        )
      )
      
      top_line <- ""
      bottom_line <- ""
      
      if (
        !inherits(paragraph, "xml_missing") &&
        !inherits(value_span, "xml_missing")
      ) {
        
        paragraph_html <- as.character(
          paragraph
        )
        
        value_html <- as.character(
          value_span
        )
        
        position <- regexpr(
          value_html,
          paragraph_html,
          fixed = TRUE
        )
        
        if (position[1] != -1) {
          
          before_value <- substr(
            paragraph_html,
            1,
            position[1] - 1
          )
          
          after_value <- substr(
            paragraph_html,
            position[1] +
              attr(position, "match.length"),
            nchar(paragraph_html)
          )
          
          before_value <- sub(
            "^<p[^>]*>",
            "",
            before_value,
            perl = TRUE
          )
          
          before_value <- sub(
            "<br\\s*/?>\\s*$",
            "",
            before_value,
            perl = TRUE
          )
          
          if (!inherits(unit_span, "xml_missing")) {
            after_value <- sub(
              pattern = as.character(unit_span),
              replacement = "",
              x = after_value,
              fixed = TRUE
            )
          }
          
          after_value <- sub(
            "^\\s*<br\\s*/?>",
            "",
            after_value,
            perl = TRUE
          )
          
          after_value <- sub(
            "</p>\\s*$",
            "",
            after_value,
            perl = TRUE
          )
          
          top_line <- html_fragment_to_text(
            before_value
          )
          
          bottom_line <- html_fragment_to_text(
            after_value
          )
        }
      }
      
      unit <- if (
        inherits(unit_span, "xml_missing")
      ) {
        ""
      } else {
        trimws(
          xml2::xml_text(unit_span)
        )
      }
      
      card_class <- if (
        inherits(card, "xml_missing")
      ) {
        ""
      } else {
        xml2::xml_attr(
          card,
          "class"
        )
      }
      
      background <- if (
        grepl(
          "\\bnavy-bg\\b",
          card_class
        )
      ) {
        "navy"
      } else {
        "blue"
      }
      
      list(
        top_line = top_line,
        value = if (
          card_number <= length(display_values)
        ) {
          display_values[card_number]
        } else {
          ""
        },
        unit = unit,
        bottom_line = bottom_line,
        background = background
      )
    }
  )
}


update_page_card_count <- function(
    project_root,
    page_href,
    card_count
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  card_count <- as.integer(
    card_count
  )
  
  if (
    length(card_count) != 1 ||
    is.na(card_count) ||
    card_count < 4 ||
    card_count > 6
  ) {
    stop("The number of page cards must be between 4 and 6.")
  }
  
  html_lines <- readLines(
    paths$html,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  js_lines <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  section <- find_page_cards_row(
    html_lines
  )
  
  existing_cards <- lapply(
    section$card_starts,
    function(card_start) {
      
      card_end <- find_closing_div(
        html_lines,
        card_start
      )
      
      html_lines[
        card_start:card_end
      ]
    }
  )
  
  keep_count <- min(
    length(existing_cards),
    card_count
  )
  
  updated_cards <- existing_cards[
    seq_len(keep_count)
  ]
  
  if (card_count > keep_count) {
    
    for (
      card_number in
      seq.int(keep_count + 1, card_count)
    ) {
      updated_cards[[card_number]] <- blank_page_card(
        card_number,
        background = "blue"
      )
    }
  }
  
  replacement <- c(
    html_lines[section$row_start],
    unlist(
      updated_cards,
      use.names = FALSE
    ),
    html_lines[section$row_end]
  )
  
  updated_html <- c(
    if (section$row_start > 1) {
      html_lines[
        seq_len(section$row_start - 1)
      ]
    } else {
      character()
    },
    replacement,
    if (section$row_end < length(html_lines)) {
      html_lines[
        seq.int(
          section$row_end + 1,
          length(html_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  tryCatch(
    {
      writeLines(
        updated_html,
        paths$html,
        useBytes = TRUE
      )
      
      update_page_js_card_sections(
        project_root = project_root,
        page_href = page_href,
        card_count = card_count,
        preserve_existing = TRUE
      )
    },
    error = function(error) {
      
      writeLines(
        html_lines,
        paths$html,
        useBytes = TRUE
      )
      
      writeLines(
        js_lines,
        paths$js,
        useBytes = TRUE
      )
      
      stop(
        paste(
          "Page card changes were restored after an error:",
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
  
  invisible(paths$html)
}


update_page_js_card_sections <- function(
    project_root,
    page_href,
    card_count,
    preserve_existing = TRUE
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  lines <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  anchor <- grep(
    "^\\s*//\\s*Insert values into page cards below\\s*$",
    lines
  )
  
  if (length(anchor) != 1) {
    stop(
      paste0(
        "Could not find ",
        "'// Insert values into page cards below' in ",
        paths$js_filename,
        "."
      )
    )
  }
  
  end_marker <- grep(
    "^\\s*//\\s*End page card content\\s*$",
    lines
  )
  
  if (length(end_marker) != 1) {
    stop(
      paste0(
        "Could not uniquely identify ",
        "'// End page card content' in ",
        paths$js_filename,
        "."
      )
    )
  }
  
  if (end_marker <= anchor) {
    stop(
      "The page-card end marker occurs before the start marker."
    )
  }
  
  existing_region <- if (
    end_marker > anchor + 1
  ) {
    lines[
      seq.int(anchor + 1, end_marker - 1)
    ]
  } else {
    character()
  }
  
  sections <- list()
  
  if (isTRUE(preserve_existing)) {
    
    comment_lines <- grep(
      "^\\s*//\\s*Content for card\\s+[0-9]+\\s*$",
      existing_region
    )
    
    if (length(comment_lines) > 0) {
      
      for (i in seq_along(comment_lines)) {
        
        section_end <- if (
          i < length(comment_lines)
        ) {
          comment_lines[i + 1] - 1
        } else {
          length(existing_region)
        }
        
        sections[[i]] <- existing_region[
          comment_lines[i]:section_end
        ]
      }
    }
  }
  
  new_region <- character()
  
  for (i in seq_len(card_count)) {
    
    if (
      isTRUE(preserve_existing) &&
      i <= length(sections)
    ) {
      
      card_section <- sections[[i]]
      
      card_section[1] <- paste0(
        "    // Content for card ",
        i
      )
      
      while (
        length(card_section) > 1 &&
        !nzchar(trimws(tail(card_section, 1)))
      ) {
        card_section <- head(
          card_section,
          -1
        )
      }
      
    } else {
      card_section <- paste0(
        "    // Content for card ",
        i
      )
    }
    
    new_region <- c(
      new_region,
      card_section,
      "",
      ""
    )
  }
  
  updated <- c(
    lines[seq_len(anchor)],
    "",
    new_region,
    lines[end_marker:length(lines)]
  )
  
  writeLines(
    updated,
    paths$js,
    useBytes = TRUE
  )
  
  invisible(paths$js)
}


update_page_card_body <- function(
    project_root,
    page_href,
    card_number,
    top_line = "",
    unit = "",
    bottom_line = "",
    background = "blue"
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  lines <- readLines(
    paths$html,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  section <- find_page_cards_row(
    lines
  )
  
  card_number <- as.integer(
    card_number
  )
  
  if (
    is.na(card_number) ||
    card_number < 1 ||
    card_number > length(section$card_starts)
  ) {
    stop("The selected page card was not found.")
  }
  
  card_start <- section$card_starts[
    card_number
  ]
  
  card_end <- find_closing_div(
    lines,
    card_start
  )
  
  body_start <- grep(
    'class=["\'][^"\']*\\bcard-body\\b',
    lines,
    perl = TRUE
  )
  
  body_start <- body_start[
    body_start >= card_start &
      body_start <= card_end
  ]
  
  if (length(body_start) != 1) {
    stop(
      paste0(
        "Could not identify the body for page card ",
        card_number,
        "."
      )
    )
  }
  
  body_end <- find_closing_div(
    lines,
    body_start
  )
  
  card_container <- grep(
    paste0(
      '<div\\b[^>]*class=["\']',
      '[^"\']*\\bcard\\b',
      '[^"\']*\\bh-100\\b'
    ),
    lines,
    perl = TRUE
  )
  
  card_container <- card_container[
    card_container >= card_start &
      card_container <= card_end
  ]
  
  if (length(card_container) != 1) {
    stop(
      paste0(
        "Could not identify the container for page card ",
        card_number,
        "."
      )
    )
  }
  
  background_class <- if (
    identical(background, "navy")
  ) {
    "navy-bg"
  } else {
    "blue-bg"
  }
  
  card_line <- lines[
    card_container
  ]
  
  card_line <- gsub(
    "\\s+(blue-bg|navy-bg)\\b",
    "",
    card_line,
    perl = TRUE
  )
  
  card_line <- sub(
    '(class=["\'][^"\']*)',
    paste0(
      "\\1 ",
      background_class
    ),
    card_line,
    perl = TRUE
  )
  
  lines[
    card_container
  ] <- card_line
  
  parsed_top <- parse_homepage_year_tags(
    top_line
  )
  
  parsed_bottom <- parse_homepage_year_tags(
    bottom_line
  )
  
  escaped_unit <- as.character(
    htmltools::htmlEscape(
      trimws(unit),
      attribute = FALSE
    )
  )
  
  body_indent <- sub(
    "^(\\s*).*",
    "\\1",
    lines[body_start]
  )
  
  content_indent <- paste0(
    body_indent,
    "    "
  )
  
  value_span <- paste0(
    '<span id="card-',
    card_number,
    '-value" class="display-6"></span>'
  )
  
  unit_span <- if (nzchar(trimws(unit))) {
    paste0(
      '<span class="unit">',
      escaped_unit,
      "</span>"
    )
  } else {
    ""
  }
  
  replacement <- c(
    sub(
      "^(.*?<div\\b[^>]*>).*",
      "\\1",
      lines[body_start],
      perl = TRUE
    ),
    paste0(
      content_indent,
      "<p>",
      parsed_top
    ),
    paste0(
      content_indent,
      "    <br>",
      value_span,
      unit_span
    ),
    paste0(
      content_indent,
      "    <br>",
      parsed_bottom
    ),
    paste0(
      content_indent,
      "</p>"
    ),
    paste0(
      body_indent,
      "</div>"
    )
  )
  
  updated <- c(
    if (body_start > 1) {
      lines[seq_len(body_start - 1)]
    } else {
      character()
    },
    replacement,
    if (body_end < length(lines)) {
      lines[
        seq.int(body_end + 1, length(lines))
      ]
    } else {
      character()
    }
  )
  
  writeLines(
    updated,
    paths$html,
    useBytes = TRUE
  )
  
  invisible(paths$html)
}

update_page_card_value_js <- function(
    project_root,
    page_href,
    card_number,
    calculation,
    year_prefix = NULL,
    use_matrix_years = FALSE
) {
  
  paths <- page_design_paths(
    project_root = project_root,
    page_href = page_href
  )
  
  js_path <- paths$js
  
  if (!file.exists(js_path)) {
    stop(
      paste0(
        paths$js_filename,
        " was not found."
      )
    )
  }
  
  card_number <- as.integer(
    card_number
  )
  
  if (
    length(card_number) != 1 ||
    is.na(card_number) ||
    card_number < 1 ||
    card_number > 6
  ) {
    stop(
      "A valid page card number is required."
    )
  }
  
  if (
    is.null(calculation) ||
    is.null(calculation$matrix) ||
    !nzchar(
      as.character(
        calculation$matrix
      )[1]
    )
  ) {
    stop(
      "No stored calculation was supplied."
    )
  }
  
  matrix <- as.character(
    calculation$matrix
  )[1]
  
  selected_columns <- as.character(
    calculation$selected_columns
  )
  
  if (length(selected_columns) == 0) {
    stop(
      "No value column was selected."
    )
  }
  
  decimal_places <- as.integer(
    calculation$decimal_places
  )
  
  if (
    length(decimal_places) != 1 ||
    is.na(decimal_places) ||
    decimal_places < 0
  ) {
    decimal_places <- 0L
  }
  
  comma_separator <- isTRUE(
    calculation$comma_separator
  )
  
  display_value <- format_card_calculation_value(
    value = calculation$raw_value,
    decimal_places = decimal_places,
    comma_separator = comma_separator
  )
  
  js_filters <- calculation$js_filters
  
  if (is.null(js_filters)) {
    js_filters <- list()
  }
  
  js_lines <- readLines(
    js_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  #
  # First locate the current card before changing any line numbers.
  #
  
  start_marker <- grep(
    "^\\s*//\\s*Insert values into page cards below\\s*$",
    js_lines
  )
  
  if (length(start_marker) != 1) {
    stop(
      paste0(
        "Could not uniquely identify ",
        "'// Insert values into page cards below' in ",
        paths$js_filename,
        "."
      )
    )
  }
  
  end_marker <- grep(
    "^\\s*//\\s*End page card content\\s*$",
    js_lines
  )
  
  if (length(end_marker) != 1) {
    stop(
      paste0(
        "Could not uniquely identify ",
        "'// End page card content' in ",
        paths$js_filename,
        "."
      )
    )
  }
  
  if (end_marker <= start_marker) {
    stop(
      "The page-card end marker occurs before the start marker."
    )
  }
  
  current_marker_pattern <- paste0(
    "^\\s*//\\s*Content for card\\s+",
    card_number,
    "\\s*$"
  )
  
  current_marker <- grep(
    current_marker_pattern,
    js_lines,
    perl = TRUE
  )
  
  current_marker <- current_marker[
    current_marker > start_marker &
      current_marker < end_marker
  ]
  
  if (length(current_marker) != 1) {
    stop(
      paste0(
        "Could not identify the JavaScript section for page card ",
        card_number,
        "."
      )
    )
  }
  
  #
  # Find every readData declaration for this matrix in the
  # entire JS module.
  #
  
  matrix_load_pattern <- paste0(
    "^\\s*const\\s+\\[\\s*",
    matrix,
    "_data\\s*,\\s*",
    matrix,
    "_meta\\s*\\]\\s*=\\s*await\\s+readData\\(",
    "[\"']",
    matrix,
    "[\"']",
    "\\);\\s*$"
  )
  
  existing_matrix_loads <- grep(
    matrix_load_pattern,
    js_lines,
    perl = TRUE
  )
  
  #
  # If a declaration already exists before this card, keep the
  # earliest one there. Otherwise this card becomes the owner
  # of the declaration.
  #
  
  earlier_matrix_loads <- existing_matrix_loads[
    existing_matrix_loads < current_marker
  ]
  
  matrix_loaded_earlier <-
    length(earlier_matrix_loads) > 0
  
  keep_matrix_load <- if (
    matrix_loaded_earlier
  ) {
    min(earlier_matrix_loads)
  } else {
    NA_integer_
  }
  
  #
  # Remove all other declarations for this matrix.
  #
  # This includes declarations in chart sections below the
  # cards and any accidental duplicates elsewhere.
  #
  
  matrix_loads_to_remove <- if (
    matrix_loaded_earlier
  ) {
    setdiff(
      existing_matrix_loads,
      keep_matrix_load
    )
  } else {
    existing_matrix_loads
  }
  
  if (length(matrix_loads_to_remove) > 0) {
    js_lines <- js_lines[
      -matrix_loads_to_remove
    ]
  }
  
  #
  # Move ownership of updateYearSpans() with the matrix load.
  #
  # If this card is becoming the first owner of the matrix,
  # remove any existing call further down the script, such
  # as one previously written inside a chart block.
  #
  if (!matrix_loaded_earlier) {
    js_lines <- remove_matrix_update_year_spans(
      lines = js_lines,
      matrix = matrix
    )
  }
  
  #
  # Recalculate every marker because removing readData lines
  # may have changed line numbers.
  #
  
  start_marker <- grep(
    "^\\s*//\\s*Insert values into page cards below\\s*$",
    js_lines
  )
  
  end_marker <- grep(
    "^\\s*//\\s*End page card content\\s*$",
    js_lines
  )
  
  if (
    length(start_marker) != 1 ||
    length(end_marker) != 1
  ) {
    stop(
      "Could not identify the page-card JavaScript region after removing duplicate data declarations."
    )
  }
  
  current_marker <- grep(
    current_marker_pattern,
    js_lines,
    perl = TRUE
  )
  
  current_marker <- current_marker[
    current_marker > start_marker &
      current_marker < end_marker
  ]
  
  if (length(current_marker) != 1) {
    stop(
      paste0(
        "Could not identify the JavaScript section for page card ",
        card_number,
        " after removing duplicate data declarations."
      )
    )
  }
  
  all_card_markers <- grep(
    "^\\s*//\\s*Content for card\\s+[0-9]+\\s*$",
    js_lines,
    perl = TRUE
  )
  
  all_card_markers <- all_card_markers[
    all_card_markers > start_marker &
      all_card_markers < end_marker
  ]
  
  next_markers <- all_card_markers[
    all_card_markers > current_marker
  ]
  
  section_end <- if (
    length(next_markers) > 0
  ) {
    min(next_markers) - 1
  } else {
    end_marker - 1
  }
  
  data_variable <- paste0(
    matrix,
    "_data"
  )
  
  metadata_variable <- paste0(
    matrix,
    "_meta"
  )
  
  raw_variable <- paste0(
    "card_",
    card_number,
    "_raw"
  )
  
  value_variable <- paste0(
    "card_",
    card_number,
    "_value"
  )
  
  element_id <- paste0(
    "card-",
    card_number,
    "-value"
  )
  
  #
  # Add readData here only when an earlier card does not
  # already own the declaration.
  #
  
  load_lines <- if (
    matrix_loaded_earlier
  ) {
    character()
  } else {
    c(
      paste0(
        "    const [",
        data_variable,
        ", ",
        metadata_variable,
        '] = await readData("',
        escape_javascript_string(matrix),
        '");'
      ),
      
      if (card_number == 1) {
        paste0(
          "    updateYearSpans(",
          data_variable,
          ", ",
          metadata_variable,
          ");"
        )
      },
      
      ""
    )
  }
  
  #
  # Build row filters.
  #
  
  filter_lines <- character()
  
  if (length(js_filters) > 0) {
    
    filter_conditions <- character()
    
    for (
      column_name in
      names(js_filters)
    ) {
      
      filter_definition <- js_filters[[
        column_name
      ]]
      
      if (
        is.list(filter_definition) &&
        !is.null(filter_definition$values)
      ) {
        
        selected_values <- as.character(
          filter_definition$values
        )
        
        is_year_filter <- isTRUE(
          filter_definition$is_year
        )
        
      } else {
        
        selected_values <- as.character(
          filter_definition
        )
        
        is_year_filter <- FALSE
      }
      
      js_values <- vapply(
        selected_values,
        javascript_filter_value,
        character(1),
        is_year = is_year_filter
      )
      
      if (length(js_values) == 1) {
        
        condition <- paste0(
          "row[",
          javascript_string(
            column_name
          ),
          "] == ",
          js_values[1]
        )
        
      } else {
        
        condition <- paste0(
          "[",
          paste(
            js_values,
            collapse = ", "
          ),
          "].includes(row[",
          javascript_string(
            column_name
          ),
          "])"
        )
      }
      
      filter_conditions <- c(
        filter_conditions,
        condition
      )
    }
    
    combined_condition <- paste(
      filter_conditions,
      collapse = " &&\n"
    )
    
    filter_lines <- paste0(
      "        .filter(row => ",
      combined_condition,
      ")"
    )
  }
  
  selected_column_js <- paste(
    vapply(
      selected_columns,
      javascript_string,
      character(1)
    ),
    collapse = ", "
  )
  
  single_row <- all(
    vapply(
      js_filters,
      function(filter_definition) {
        
        if (
          is.list(filter_definition) &&
          !is.null(filter_definition$values)
        ) {
          length(
            filter_definition$values
          )
        } else {
          length(
            filter_definition
          )
        }
      },
      integer(1)
    ) == 1L
  )
  
  #
  # Build the raw card value.
  #
  
  if (
    length(selected_columns) == 1 &&
    single_row
  ) {
    
    selected_column <- javascript_string(
      selected_columns[[1]]
    )
    
    value_lines <- c(
      paste0(
        "    const ",
        raw_variable,
        " = ",
        data_variable
      ),
      filter_lines,
      paste0(
        "        .map(col => col[",
        selected_column,
        "])[0];"
      ),
      ""
    )
    
  } else if (
    length(selected_columns) == 1
  ) {
    
    selected_column <- javascript_string(
      selected_columns[[1]]
    )
    
    value_lines <- c(
      paste0(
        "    const ",
        raw_variable,
        " = ",
        data_variable
      ),
      filter_lines,
      paste0(
        "        .map(col => col[",
        selected_column,
        "])"
      ),
      paste0(
        "        .reduce((sum, value) => ",
        "sum + (Number(value) || 0), 0);"
      ),
      ""
    )
    
  } else {
    
    value_lines <- c(
      paste0(
        "    const ",
        raw_variable,
        " = ",
        data_variable
      ),
      filter_lines,
      paste0(
        "        .flatMap(row => [",
        selected_column_js,
        "].map(column => row[column]))"
      ),
      paste0(
        "        .reduce((sum, value) => ",
        "sum + (Number(value) || 0), 0);"
      ),
      ""
    )
  }
  
  #
  # Apply display formatting.
  #
  
  if (comma_separator) {
    
    formatting_lines <- c(
      paste0(
        "    const ",
        value_variable,
        " = ",
        raw_variable,
        '.toLocaleString("en-GB", {'
      ),
      paste0(
        "        minimumFractionDigits: ",
        decimal_places,
        ","
      ),
      paste0(
        "        maximumFractionDigits: ",
        decimal_places
      ),
      "    });"
    )
    
  } else if (
    decimal_places > 0
  ) {
    
    formatting_lines <- paste0(
      "    const ",
      value_variable,
      " = ",
      raw_variable,
      ".toFixed(",
      decimal_places,
      ");"
    )
    
  } else {
    
    formatting_lines <- paste0(
      "    const ",
      value_variable,
      " = ",
      raw_variable,
      ";"
    )
  }
  
  insertion_lines <- c(
    paste0(
      "    // BuildR display value: ",
      display_value
    ),
    formatting_lines,
    paste0(
      '    insertValue("',
      element_id,
      '", ',
      value_variable,
      ");"
    ),
    "",
    ""
  )
  
  replacement <- c(
    js_lines[current_marker],
    "",
    load_lines,
    value_lines,
    insertion_lines
  )
  
  updated_js <- c(
    if (current_marker > 1) {
      js_lines[
        seq_len(
          current_marker - 1
        )
      ]
    } else {
      character()
    },
    
    replacement,
    
    if (
      section_end <
      length(js_lines)
    ) {
      js_lines[
        seq.int(
          section_end + 1,
          length(js_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  writeLines(
    updated_js,
    js_path,
    useBytes = TRUE
  )
  
  invisible(
    js_path
  )
}

clear_page_js_chart_content <- function(
    js_lines,
    js_filename,
    chart_count
) {
  
  chart_start <- grep(
    "^\\s*//\\s*Insert chart content below\\s*$",
    js_lines
  )
  
  if (length(chart_start) != 1) {
    stop(
      paste0(
        "Could not uniquely identify ",
        "'// Insert chart content below' in ",
        js_filename,
        "."
      )
    )
  }
  
  chart_end <- grep(
    "^\\s*//\\s*End chart content\\s*$",
    js_lines
  )
  
  if (length(chart_end) != 1) {
    stop(
      paste0(
        "Could not uniquely identify ",
        "'// End chart content' in ",
        js_filename,
        "."
      )
    )
  }
  
  if (chart_end <= chart_start) {
    stop(
      "The chart-content end marker occurs before its start marker."
    )
  }
  
  chart_count <- as.integer(chart_count)
  
  if (
    length(chart_count) != 1 ||
    is.na(chart_count) ||
    chart_count < 0 ||
    chart_count > 3
  ) {
    stop(
      "The number of charts must be between 0 and 3."
    )
  }
  
  empty_chart_sections <- character()
  
  if (chart_count > 0) {
    
    for (chart_number in seq_len(chart_count)) {
      
      empty_chart_sections <- c(
        empty_chart_sections,
        paste0(
          "    // Content for chart ",
          chart_number
        ),
        "",
        ""
      )
    }
    
    # Keep formatting tidy before the end marker.
    while (
      length(empty_chart_sections) > 0 &&
      identical(
        tail(
          empty_chart_sections,
          1
        ),
        ""
      )
    ) {
      empty_chart_sections <- head(
        empty_chart_sections,
        -1
      )
    }
  }
  
  c(
    js_lines[
      seq_len(chart_start)
    ],
    "",
    empty_chart_sections,
    "",
    js_lines[
      chart_end:length(js_lines)
    ]
  )
}

find_page_chart_cards_region <- function(html_lines) {
  
  marker_start <- grep(
    "^\\s*<!--\\s*Chart cards start\\s*-->\\s*$",
    html_lines
  )
  
  marker_end <- grep(
    "^\\s*<!--\\s*Chart cards end\\s*-->\\s*$",
    html_lines
  )
  
  if (length(marker_start) != 1) {
    stop(
      "Could not uniquely identify '<!-- Chart cards start -->'."
    )
  }
  
  if (length(marker_end) != 1) {
    stop(
      "Could not uniquely identify '<!-- Chart cards end -->'."
    )
  }
  
  if (marker_end <= marker_start) {
    stop(
      "The chart-card end marker occurs before its start marker."
    )
  }
  
  row_candidates <- grep(
    paste0(
      '<div\\b[^>]*class\\s*=\\s*["\']',
      '[^"\']*\\brow\\b',
      '[^"\']*["\']'
    ),
    html_lines,
    perl = TRUE
  )
  
  row_candidates <- row_candidates[
    row_candidates > marker_start &
      row_candidates < marker_end
  ]
  
  if (length(row_candidates) != 1) {
    stop(
      "Could not uniquely identify the chart-card row."
    )
  }
  
  row_start <- row_candidates[[1]]
  
  row_end <- find_closing_div(
    html_lines,
    row_start
  )
  
  if (row_end >= marker_end) {
    stop(
      "The chart-card row extends beyond its end marker."
    )
  }
  
  chart_starts <- grep(
    paste0(
      '<div\\b[^>]*class\\s*=\\s*["\']',
      '[^"\']*\\bcol-12\\b',
      '[^"\']*\\bcol-xl-(4|6|12)\\b',
      '[^"\']*\\bpy-2\\b',
      '[^"\']*["\']'
    ),
    html_lines,
    perl = TRUE
  )
  
  chart_starts <- chart_starts[
    chart_starts > row_start &
      chart_starts < row_end
  ]
  
  list(
    marker_start = marker_start,
    marker_end = marker_end,
    row_start = row_start,
    row_end = row_end,
    chart_starts = chart_starts
  )
}


page_chart_column_class <- function(chart_count) {
  
  chart_count <- as.integer(chart_count)
  
  switch(
    as.character(chart_count),
    "1" = "col-xl-12",
    "2" = "col-xl-6",
    "3" = "col-xl-4",
    stop("The number of charts must be between 1 and 3.")
  )
}


set_page_chart_column_width <- function(
    chart_lines,
    chart_count
) {
  
  column_class <- page_chart_column_class(
    chart_count
  )
  
  opening_line <- chart_lines[[1]]
  
  if (!grepl("\\bcol-12\\b", opening_line)) {
    stop(
      "Could not identify the chart column opening line."
    )
  }
  
  opening_line <- gsub(
    "\\bcol-xl-(4|6|12)\\b",
    column_class,
    opening_line,
    perl = TRUE
  )
  
  # Support older blank chart cards that may lack a col-xl-* class.
  if (!grepl("\\bcol-xl-(4|6|12)\\b", opening_line)) {
    
    opening_line <- sub(
      "\\bcol-12\\b",
      paste(
        "col-12",
        column_class
      ),
      opening_line
    )
  }
  
  chart_lines[[1]] <- opening_line
  
  chart_lines
}


blank_page_chart_card <- function(
    chart_number,
    chart_count
) {
  
  column_class <- page_chart_column_class(
    chart_count
  )
  
  c(
    paste0(
      '            <div class="col-12 ',
      column_class,
      ' py-2">'
    ),
    '                <div class="card h-100">',
    paste0(
      '                    <div id="chart-',
      chart_number,
      '-capture">'
    ),
    '                        <div class="card-header"></div>',
    '                        <div class="card-body d-flex flex-column justify-content-center"></div>',
    '                    </div>',
    '                    <div class="card-footer"></div>',
    '                </div>',
    '            </div>'
  )
}


count_page_charts <- function(
    project_root,
    page_href
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$html)) {
    stop(
      paste0(
        paths$href,
        " was not found."
      )
    )
  }
  
  html_lines <- readLines(
    paths$html,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  length(
    find_page_chart_cards_region(
      html_lines
    )$chart_starts
  )
}

read_page_chart_titles <- function(
    project_root,
    page_href
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$html)) {
    stop(
      paste0(
        paths$href,
        " was not found."
      )
    )
  }
  
  html_lines <- readLines(
    paths$html,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  chart_region <- find_page_chart_cards_region(
    html_lines
  )
  
  lapply(
    seq_along(chart_region$chart_starts),
    function(chart_number) {
      
      chart_start <- chart_region$chart_starts[
        chart_number
      ]
      
      chart_end <- find_closing_div(
        html_lines,
        chart_start
      )
      
      header_start <- grep(
        'class=["\'][^"\']*\\bcard-header\\b',
        html_lines,
        perl = TRUE
      )
      
      header_start <- header_start[
        header_start >= chart_start &
          header_start <= chart_end
      ]
      
      if (length(header_start) == 0) {
        return("")
      }
      
      if (length(header_start) != 1) {
        stop(
          paste0(
            "Could not uniquely identify the header for chart ",
            chart_number,
            "."
          )
        )
      }
      
      header_end <- find_closing_div(
        html_lines,
        header_start
      )
      
      header_html <- paste(
        html_lines[header_start:header_end],
        collapse = "\n"
      )
      
      header_html <- sub(
        "^.*?<div\\b[^>]*>",
        "",
        header_html,
        perl = TRUE
      )
      
      header_html <- sub(
        "</div>\\s*$",
        "",
        header_html,
        perl = TRUE
      )
      
      # This converts year spans back to:
      # <<latest-year>>, <<last-year>>, <<first-year>>
      html_fragment_to_text(
        header_html
      )
    }
  )
}


update_page_chart_title <- function(
    project_root,
    page_href,
    chart_number,
    chart_title,
    year_prefix = NULL
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$html)) {
    stop(
      paste0(
        paths$href,
        " was not found."
      )
    )
  }
  
  chart_number <- as.integer(
    chart_number
  )
  
  if (
    length(chart_number) != 1 ||
    is.na(chart_number) ||
    chart_number < 1 ||
    chart_number > 3
  ) {
    stop("A valid chart number is required.")
  }
  
  html_lines <- readLines(
    paths$html,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  chart_region <- find_page_chart_cards_region(
    html_lines
  )
  
  if (chart_number > length(chart_region$chart_starts)) {
    stop(
      paste0(
        "Chart ",
        chart_number,
        " was not found."
      )
    )
  }
  
  chart_start <- chart_region$chart_starts[
    chart_number
  ]
  
  chart_end <- find_closing_div(
    html_lines,
    chart_start
  )
  
  header_start <- grep(
    'class=["\'][^"\']*\\bcard-header\\b',
    html_lines,
    perl = TRUE
  )
  
  header_start <- header_start[
    header_start >= chart_start &
      header_start <= chart_end
  ]
  
  if (length(header_start) != 1) {
    stop(
      paste0(
        "Could not uniquely identify the header for chart ",
        chart_number,
        "."
      )
    )
  }
  
  header_end <- find_closing_div(
    html_lines,
    header_start
  )
  
  parsed_title <- parse_homepage_year_tags(
    chart_title,
    year_prefix = year_prefix
  )
  
  opening_line <- sub(
    "^(.*?<div\\b[^>]*>).*",
    "\\1",
    html_lines[header_start],
    perl = TRUE
  )
  
  header_indent <- sub(
    "^(\\s*).*",
    "\\1",
    html_lines[header_start]
  )
  
  content_indent <- paste0(
    header_indent,
    "    "
  )
  
  replacement <- if (nzchar(trimws(chart_title))) {
    c(
      opening_line,
      paste0(
        content_indent,
        parsed_title
      ),
      paste0(
        header_indent,
        "</div>"
      )
    )
  } else {
    paste0(
      opening_line,
      "</div>"
    )
  }
  
  updated_html <- c(
    if (header_start > 1) {
      html_lines[
        seq_len(header_start - 1)
      ]
    } else {
      character()
    },
    
    replacement,
    
    if (header_end < length(html_lines)) {
      html_lines[
        seq.int(
          header_end + 1,
          length(html_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  writeLines(
    updated_html,
    paths$html,
    useBytes = TRUE
  )
  
  invisible(paths$html)
}

clear_page_design_files <- function(
    project_root,
    page_href
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$html)) {
    stop(
      paste0(
        paths$href,
        " was not found."
      )
    )
  }
  
  if (!file.exists(paths$js)) {
    stop(
      paste0(
        paths$js_filename,
        " was not found."
      )
    )
  }
  
  original_html <- readLines(
    paths$html,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  original_js <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  preserved_info_boxes <- extract_page_info_boxes_js(
    original_js
  )
  
  updated_html <- original_html
  
  chart_count <- length(
    find_page_chart_cards_region(
      original_html
    )$chart_starts
  )
  
  # Clear the page strapline.
  current_page <- grep(
    'class=["\'][^"\']*\\bcurrent-page-strapline\\b',
    updated_html,
    perl = TRUE
  )
  
  if (length(current_page) != 1) {
    stop(
      "Could not uniquely identify the page strapline."
    )
  }
  
  updated_html <- clear_div_contents_by_line(
    updated_html,
    current_page
  )
  
  # Clear the headline-card bodies.
  card_section <- find_page_cards_row(
    updated_html
  )
  
  card_body_starts <- grep(
    'class=["\'][^"\']*\\bcard-body\\b',
    updated_html,
    perl = TRUE
  )
  
  card_body_starts <- card_body_starts[
    card_body_starts > card_section$row_start &
      card_body_starts < card_section$row_end
  ]
  
  if (length(card_body_starts) == 0) {
    stop(
      "No page headline-card bodies were found."
    )
  }
  
  for (
    body_start in sort(
      card_body_starts,
      decreasing = TRUE
    )
  ) {
    updated_html <- clear_div_contents_by_line(
      updated_html,
      body_start
    )
  }
  
  # Locate the explicitly marked chart-card region.
  chart_html_start <- grep(
    "^\\s*<!--\\s*Chart cards start\\s*-->\\s*$",
    updated_html
  )
  
  chart_html_end <- grep(
    "^\\s*<!--\\s*Chart cards end\\s*-->\\s*$",
    updated_html
  )
  
  if (length(chart_html_start) != 1) {
    stop(
      paste(
        "Could not uniquely identify",
        "'<!-- Chart cards start -->'."
      )
    )
  }
  
  if (length(chart_html_end) != 1) {
    stop(
      paste(
        "Could not uniquely identify",
        "'<!-- Chart cards end -->'."
      )
    )
  }
  
  if (chart_html_end <= chart_html_start) {
    stop(
      "The chart-card end marker occurs before its start marker."
    )
  }
  
  # Clear all chart card headers.
  chart_header_starts <- grep(
    'class=["\'][^"\']*\\bcard-header\\b',
    updated_html,
    perl = TRUE
  )
  
  chart_header_starts <- chart_header_starts[
    chart_header_starts > chart_html_start &
      chart_header_starts < chart_html_end
  ]
  
  # Clear all chart card bodies.
  chart_body_starts <- grep(
    'class=["\'][^"\']*\\bcard-body\\b',
    updated_html,
    perl = TRUE
  )
  
  chart_body_starts <- chart_body_starts[
    chart_body_starts > chart_html_start &
      chart_body_starts < chart_html_end
  ]
  
  elements_to_clear <- sort(
    c(
      chart_header_starts,
      chart_body_starts
    ),
    decreasing = TRUE
  )
  
  if (length(elements_to_clear) == 0) {
    stop(
      "No chart card headers or bodies were found."
    )
  }
  
  # Work from the bottom upward so earlier line numbers stay valid.
  for (element_start in elements_to_clear) {
    updated_html <- clear_div_contents_by_line(
      updated_html,
      element_start
    )
  }
  
  # Clear all generated headline-card JavaScript.
  card_count <- length(
    find_page_cards_row(
      updated_html
    )$card_starts
  )
  
  updated_html <- normalise_page_chart_capture_ids(
    updated_html
  )
  
  tryCatch(
    {
      writeLines(
        updated_html,
        paths$html,
        useBytes = TRUE
      )
      
      update_page_js_card_sections(
        project_root = project_root,
        page_href = page_href,
        card_count = card_count,
        preserve_existing = FALSE
      )
      
      updated_js <- readLines(
        paths$js,
        warn = FALSE,
        encoding = "UTF-8"
      )
      
      updated_js <- clear_page_js_chart_content(
        js_lines = updated_js,
        js_filename = paths$js_filename,
        chart_count = chart_count
      )
      
      updated_js <- updated_js[
        !grepl(
          "^\\s*import\\b.*[\"']\\./charts/",
          updated_js,
          perl = TRUE
        )
      ]
      
      #
      # Restore the existing info-box content after
      # clearing cards and charts.
      #
      updated_js <- restore_page_info_boxes_js(
        js_lines = updated_js,
        info_box_lines = preserved_info_boxes
      )
      
      writeLines(
        updated_js,
        paths$js,
        useBytes = TRUE
      )
    },
    error = function(error) {
      
      writeLines(
        original_html,
        paths$html,
        useBytes = TRUE
      )
      
      writeLines(
        original_js,
        paths$js,
        useBytes = TRUE
      )
      
      stop(
        paste(
          "Page files were restored after an error:",
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
  
  invisible(paths)
}


clear_div_contents_by_line <- function(
    lines,
    start_line
) {
  
  end_line <- find_closing_div(
    lines,
    start_line
  )
  
  opening <- sub(
    "^(.*?<div\\b[^>]*>).*",
    "\\1",
    lines[start_line],
    perl = TRUE
  )
  
  indent <- sub(
    "^(\\s*).*",
    "\\1",
    lines[start_line]
  )
  
  replacement <- paste0(
    opening,
    "</div>"
  )
  
  c(
    if (start_line > 1) {
      lines[seq_len(start_line - 1)]
    } else {
      character()
    },
    replacement,
    if (end_line < length(lines)) {
      lines[seq.int(end_line + 1, length(lines))]
    } else {
      character()
    }
  )
}

update_page_js_chart_sections <- function(
    project_root,
    page_href,
    chart_count,
    preserve_existing = TRUE
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$js)) {
    stop(
      paste0(
        paths$js_filename,
        " was not found."
      )
    )
  }
  
  chart_count <- as.integer(chart_count)
  
  if (
    length(chart_count) != 1 ||
    is.na(chart_count) ||
    chart_count < 0 ||
    chart_count > 3
  ) {
    stop("The number of charts must be between 0 and 3.")
  }
  
  js_lines <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  region_start <- grep(
    "^\\s*//\\s*Insert chart content below\\s*$",
    js_lines
  )
  
  region_end <- grep(
    "^\\s*//\\s*End chart content\\s*$",
    js_lines
  )
  
  if (length(region_start) != 1) {
    stop(
      paste0(
        "Could not uniquely identify ",
        "'// Insert chart content below' in ",
        paths$js_filename,
        "."
      )
    )
  }
  
  if (length(region_end) != 1) {
    stop(
      paste0(
        "Could not uniquely identify ",
        "'// End chart content' in ",
        paths$js_filename,
        "."
      )
    )
  }
  
  if (region_end <= region_start) {
    stop(
      "The chart-content end marker occurs before its start marker."
    )
  }
  
  existing_region <- if (
    region_end > region_start + 1
  ) {
    js_lines[
      seq.int(
        region_start + 1,
        region_end - 1
      )
    ]
  } else {
    character()
  }
  
  existing_sections <- list()
  
  if (isTRUE(preserve_existing)) {
    
    chart_markers <- grep(
      "^\\s*//\\s*Content for chart\\s+[0-9]+\\s*$",
      existing_region
    )
    
    if (length(chart_markers) > 0) {
      
      for (i in seq_along(chart_markers)) {
        
        section_start <- chart_markers[[i]]
        
        section_end <- if (
          i < length(chart_markers)
        ) {
          chart_markers[[i + 1]] - 1
        } else {
          length(existing_region)
        }
        
        section <- existing_region[
          section_start:section_end
        ]
        
        while (
          length(section) > 1 &&
          !nzchar(trimws(tail(section, 1)))
        ) {
          section <- head(
            section,
            -1
          )
        }
        
        existing_sections[[i]] <- section
      }
    }
  }
  
  updated_region <- character()
  
  if (chart_count > 0) {
    
    for (chart_number in seq_len(chart_count)) {
      
      if (
        isTRUE(preserve_existing) &&
        chart_number <= length(existing_sections)
      ) {
        
        section <- existing_sections[[
          chart_number
        ]]
        
        section[[1]] <- paste0(
          "    // Content for chart ",
          chart_number
        )
        
      } else {
        
        section <- paste0(
          "    // Content for chart ",
          chart_number
        )
      }
      
      updated_region <- c(
        updated_region,
        section,
        "",
        ""
      )
    }
  }
  
  # Avoid an excessive group of blank lines before the end marker.
  while (
    length(updated_region) > 0 &&
    identical(tail(updated_region, 1), "")
  ) {
    updated_region <- head(
      updated_region,
      -1
    )
  }
  
  updated_js <- c(
    js_lines[
      seq_len(region_start)
    ],
    "",
    updated_region,
    "",
    js_lines[
      region_end:length(js_lines)
    ]
  )
  
  writeLines(
    updated_js,
    paths$js,
    useBytes = TRUE
  )
  
  invisible(paths$js)
}

update_page_chart_count <- function(
    project_root,
    page_href,
    chart_count
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$html)) {
    stop(
      paste0(
        paths$href,
        " was not found."
      )
    )
  }
  
  if (!file.exists(paths$js)) {
    stop(
      paste0(
        paths$js_filename,
        " was not found."
      )
    )
  }
  
  chart_count <- as.integer(chart_count)
  
  if (
    length(chart_count) != 1 ||
    is.na(chart_count) ||
    chart_count < 0 ||
    chart_count > 3
  ) {
    stop("The number of charts must be between 0 and 3.")
  }
  
  original_html <- readLines(
    paths$html,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  original_js <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  chart_region <- find_page_chart_cards_region(
    original_html
  )
  
  existing_charts <- lapply(
    chart_region$chart_starts,
    function(chart_start) {
      
      chart_end <- find_closing_div(
        original_html,
        chart_start
      )
      
      original_html[
        chart_start:chart_end
      ]
    }
  )
  
  charts_to_keep <- min(
    length(existing_charts),
    chart_count
  )
  
  updated_charts <- if (charts_to_keep > 0) {
    existing_charts[
      seq_len(charts_to_keep)
    ]
  } else {
    list()
  }
  
  if (chart_count > charts_to_keep) {
    
    for (
      chart_number in
      seq.int(
        charts_to_keep + 1,
        chart_count
      )
    ) {
      
      updated_charts[[chart_number]] <-
        blank_page_chart_card(
          chart_number = chart_number,
          chart_count = chart_count
        )
    }
  }
  
  # Every retained chart also needs the width appropriate
  # to the newly selected total.
  if (chart_count > 0) {
    
    updated_charts <- lapply(
      updated_charts,
      set_page_chart_column_width,
      chart_count = chart_count
    )
  }
  
  row_indent <- sub(
    "^(\\s*).*",
    "\\1",
    original_html[
      chart_region$row_start
    ]
  )
  
  row_opening <- original_html[
    chart_region$row_start
  ]
  
  row_closing <- original_html[
    chart_region$row_end
  ]
  
  replacement_row <- c(
    row_opening,
    if (chart_count > 0) {
      unlist(
        updated_charts,
        use.names = FALSE
      )
    } else {
      character()
    },
    row_closing
  )
  
  updated_html <- c(
    if (chart_region$row_start > 1) {
      original_html[
        seq_len(
          chart_region$row_start - 1
        )
      ]
    } else {
      character()
    },
    
    replacement_row,
    
    if (
      chart_region$row_end <
      length(original_html)
    ) {
      original_html[
        seq.int(
          chart_region$row_end + 1,
          length(original_html)
        )
      ]
    } else {
      character()
    }
  )
  
  updated_html <- normalise_page_chart_capture_ids(
    updated_html
  )
  
  tryCatch(
    {
      writeLines(
        updated_html,
        paths$html,
        useBytes = TRUE
      )
      
      update_page_js_chart_sections(
        project_root = project_root,
        page_href = page_href,
        chart_count = chart_count,
        preserve_existing = TRUE
      )
    },
    error = function(error) {
      
      writeLines(
        original_html,
        paths$html,
        useBytes = TRUE
      )
      
      writeLines(
        original_js,
        paths$js,
        useBytes = TRUE
      )
      
      stop(
        paste(
          "Page chart changes were restored after an error:",
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
  
  invisible(paths)
}

page_chart_type_definitions <- function() {
  list(
    bar = list(
      label = "Bar chart",
      script = "bar-chart.js",
      function_name = "barChart"
    ),
    table = list(
      label = "Table",
      script = "insert-table.js",
      function_name = "insertTable"
    ),
    line = list(
      label = "Line chart",
      script = "line-chart.js",
      function_name = "lineChart"
    ),
    pie = list(
      label = "Pie chart",
      script = "pie-chart.js",
      function_name = "pieChart"
    ),
    map = list(
      label = "Map",
      script = "plot-map.js",
      function_name = "plotMap"
    ),
    pyramid = list(
      label = "Population pyramid",
      script = "pyramid-chart.js",
      function_name = "pyramidChart"
    ),
    treemap = list(
      label = "Treemap",
      script = "treemap-chart.js",
      function_name = "treemapChart"
    )
  )
}


page_chart_type_choices <- function() {
  definitions <- page_chart_type_definitions()
  
  stats::setNames(
    names(definitions),
    vapply(
      definitions,
      function(definition) {
        definition$label
      },
      character(1)
    )
  )
}

page_bar_series_source_choices <- function() {
  c(
    "Value columns" = "value_columns",
    "Category values" = "category_values"
  )
}


page_bar_label_format_choices <- function() {
  c(
    "Number" = "",
    "Comma separator" = ",",
    "Percentage" = "%"
  )
}


page_bar_alignment_choices <- function() {
  c(
    "Vertical" = "vertical",
    "Horizontal" = "horizontal"
  )
}

read_page_chart_types <- function(
    project_root,
    page_href
) {
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$js)) {
    return(character())
  }
  
  js_lines <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  region_start <- grep(
    "^\\s*//\\s*Insert chart content below\\s*$",
    js_lines
  )
  
  region_end <- grep(
    "^\\s*//\\s*End chart content\\s*$",
    js_lines
  )
  
  if (
    length(region_start) != 1 ||
    length(region_end) != 1 ||
    region_end <= region_start
  ) {
    return(character())
  }
  
  chart_markers <- grep(
    "^\\s*//\\s*Content for chart\\s+[0-9]+\\s*$",
    js_lines
  )
  
  chart_markers <- chart_markers[
    chart_markers > region_start &
      chart_markers < region_end
  ]
  
  if (length(chart_markers) == 0) {
    return(character())
  }
  
  chart_types <- character(
    length(chart_markers)
  )
  
  for (i in seq_along(chart_markers)) {
    section_end <- if (i < length(chart_markers)) {
      chart_markers[i + 1] - 1
    } else {
      region_end - 1
    }
    
    section <- js_lines[
      chart_markers[i]:section_end
    ]
    
    type_line <- grep(
      "^\\s*//\\s*BuildR chart type:\\s*[a-z]+\\s*$",
      section,
      value = TRUE
    )
    
    if (length(type_line) == 1) {
      chart_types[i] <- trimws(
        sub(
          "^\\s*//\\s*BuildR chart type:\\s*",
          "",
          type_line
        )
      )
    } else {
      chart_types[i] <- ""
    }
  }
  
  chart_types
}
normalise_page_chart_imports <- function(js_lines) {
  definitions <- page_chart_type_definitions()
  
  type_lines <- grep(
    "^\\s*//\\s*BuildR chart type:\\s*[a-z]+\\s*$",
    js_lines,
    value = TRUE
  )
  
  chart_types <- trimws(
    sub(
      "^\\s*//\\s*BuildR chart type:\\s*",
      "",
      type_lines
    )
  )
  
  chart_types <- unique(
    chart_types[
      chart_types %in% names(definitions)
    ]
  )
  
  # Remove all existing imports from ./charts/.
  js_lines <- js_lines[
    !grepl(
      "^\\s*import\\b.*[\"']\\./charts/",
      js_lines,
      perl = TRUE
    )
  ]
  
  required_imports <- character()
  
  if (length(chart_types) > 0) {
    required_imports <- vapply(
      chart_types,
      function(chart_type) {
        definition <- definitions[[
          chart_type
        ]]
        
        paste0(
          "import { ",
          definition$function_name,
          ' } from "./charts/',
          definition$script,
          '";'
        )
      },
      character(1)
    )
  }
  
  listener_line <- grep(
    paste0(
      "^\\s*window\\.addEventListener\\(",
      "[\"']DOMContentLoaded[\"']"
    ),
    js_lines,
    perl = TRUE
  )
  
  if (length(listener_line) != 1) {
    stop(
      paste(
        "Could not uniquely identify the",
        "DOMContentLoaded event listener."
      )
    )
  }
  
  insertion_point <- listener_line - 1
  
  # Remove blank lines directly before the event listener so the
  # imports can be reconstructed consistently.
  while (
    insertion_point > 0 &&
    !nzchar(trimws(js_lines[insertion_point]))
  ) {
    insertion_point <- insertion_point - 1
  }
  
  c(
    if (insertion_point > 0) {
      js_lines[seq_len(insertion_point)]
    } else {
      character()
    },
    required_imports,
    "",
    js_lines[listener_line:length(js_lines)]
  )
}

update_page_chart_type <- function(
    project_root,
    page_href,
    chart_number,
    chart_type
) {
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$js)) {
    stop(
      paste0(
        paths$js_filename,
        " was not found."
      )
    )
  }
  
  chart_number <- as.integer(
    chart_number
  )
  
  if (
    length(chart_number) != 1 ||
    is.na(chart_number) ||
    chart_number < 1 ||
    chart_number > 3
  ) {
    stop("A valid chart number is required.")
  }
  
  chart_type <- trimws(
    as.character(chart_type)[1]
  )
  
  definitions <- page_chart_type_definitions()
  
  if (
    nzchar(chart_type) &&
    !chart_type %in% names(definitions)
  ) {
    stop("The selected chart type is not valid.")
  }
  
  original_js <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  js_lines <- original_js
  
  region_start <- grep(
    "^\\s*//\\s*Insert chart content below\\s*$",
    js_lines
  )
  
  region_end <- grep(
    "^\\s*//\\s*End chart content\\s*$",
    js_lines
  )
  
  if (
    length(region_start) != 1 ||
    length(region_end) != 1
  ) {
    stop(
      "Could not uniquely identify the chart JavaScript region."
    )
  }
  
  marker_pattern <- paste0(
    "^\\s*//\\s*Content for chart\\s+",
    chart_number,
    "\\s*$"
  )
  
  chart_marker <- grep(
    marker_pattern,
    js_lines
  )
  
  chart_marker <- chart_marker[
    chart_marker > region_start &
      chart_marker < region_end
  ]
  
  if (length(chart_marker) != 1) {
    stop(
      paste0(
        "Could not identify the JavaScript section for chart ",
        chart_number,
        "."
      )
    )
  }
  
  all_markers <- grep(
    "^\\s*//\\s*Content for chart\\s+[0-9]+\\s*$",
    js_lines
  )
  
  all_markers <- all_markers[
    all_markers > region_start &
      all_markers < region_end
  ]
  
  next_markers <- all_markers[
    all_markers > chart_marker
  ]
  
  section_end <- if (length(next_markers) > 0) {
    min(next_markers) - 1
  } else {
    region_end - 1
  }
  
  existing_section <- js_lines[
    chart_marker:section_end
  ]
  
  existing_section <- existing_section[
    !grepl(
      "^\\s*//\\s*BuildR chart type:",
      existing_section
    )
  ]
  
  replacement_section <- c(
    existing_section[1],
    if (nzchar(chart_type)) {
      paste0(
        "    // BuildR chart type: ",
        chart_type
      )
    } else {
      character()
    },
    if (length(existing_section) > 1) {
      existing_section[-1]
    } else {
      character()
    }
  )
  
  updated_js <- c(
    if (chart_marker > 1) {
      js_lines[
        seq_len(chart_marker - 1)
      ]
    } else {
      character()
    },
    replacement_section,
    if (section_end < length(js_lines)) {
      js_lines[
        seq.int(
          section_end + 1,
          length(js_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  updated_js <- normalise_page_chart_imports(
    updated_js
  )
  
  tryCatch(
    {
      writeLines(
        updated_js,
        paths$js,
        useBytes = TRUE
      )
    },
    error = function(error) {
      writeLines(
        original_js,
        paths$js,
        useBytes = TRUE
      )
      
      stop(
        paste(
          "The chart JavaScript was restored after an error:",
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
  
  invisible(paths$js)
}

read_page_chart_matrices <- function(
    project_root,
    page_href
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$js)) {
    return(character())
  }
  
  js_lines <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  region_start <- grep(
    "^\\s*//\\s*Insert chart content below\\s*$",
    js_lines
  )
  
  region_end <- grep(
    "^\\s*//\\s*End chart content\\s*$",
    js_lines
  )
  
  if (
    length(region_start) != 1 ||
    length(region_end) != 1 ||
    region_end <= region_start
  ) {
    return(character())
  }
  
  chart_markers <- grep(
    "^\\s*//\\s*Content for chart\\s+[0-9]+\\s*$",
    js_lines
  )
  
  chart_markers <- chart_markers[
    chart_markers > region_start &
      chart_markers < region_end
  ]
  
  if (length(chart_markers) == 0) {
    return(character())
  }
  
  matrices <- character(
    length(chart_markers)
  )
  
  for (i in seq_along(chart_markers)) {
    
    section_end <- if (
      i < length(chart_markers)
    ) {
      chart_markers[i + 1] - 1
    } else {
      region_end - 1
    }
    
    section <- js_lines[
      chart_markers[i]:section_end
    ]
    
    matrix_line <- grep(
      "^\\s*//\\s*BuildR matrix:\\s*[A-Za-z0-9_-]+\\s*$",
      section,
      value = TRUE
    )
    
    if (length(matrix_line) == 1) {
      
      matrices[i] <- trimws(
        sub(
          "^\\s*//\\s*BuildR matrix:\\s*",
          "",
          matrix_line
        )
      )
      
    } else {
      
      matrices[i] <- ""
    }
  }
  
  matrices
}

update_page_chart_matrix <- function(
    project_root,
    page_href,
    chart_number,
    matrix
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$js)) {
    stop(
      paste0(
        paths$js_filename,
        " was not found."
      )
    )
  }
  
  chart_number <- as.integer(
    chart_number
  )
  
  if (
    length(chart_number) != 1 ||
    is.na(chart_number) ||
    chart_number < 1 ||
    chart_number > 3
  ) {
    stop("A valid chart number is required.")
  }
  
  matrix <- trimws(
    as.character(matrix)[1]
  )
  
  original_js <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  js_lines <- original_js
  
  region_start <- grep(
    "^\\s*//\\s*Insert chart content below\\s*$",
    js_lines
  )
  
  region_end <- grep(
    "^\\s*//\\s*End chart content\\s*$",
    js_lines
  )
  
  if (
    length(region_start) != 1 ||
    length(region_end) != 1 ||
    region_end <= region_start
  ) {
    stop(
      "Could not uniquely identify the chart JavaScript region."
    )
  }
  
  chart_marker <- grep(
    paste0(
      "^\\s*//\\s*Content for chart\\s+",
      chart_number,
      "\\s*$"
    ),
    js_lines
  )
  
  chart_marker <- chart_marker[
    chart_marker > region_start &
      chart_marker < region_end
  ]
  
  if (length(chart_marker) != 1) {
    stop(
      paste0(
        "Could not identify the JavaScript section for chart ",
        chart_number,
        "."
      )
    )
  }
  
  all_markers <- grep(
    "^\\s*//\\s*Content for chart\\s+[0-9]+\\s*$",
    js_lines
  )
  
  all_markers <- all_markers[
    all_markers > region_start &
      all_markers < region_end
  ]
  
  next_markers <- all_markers[
    all_markers > chart_marker
  ]
  
  section_end <- if (length(next_markers) > 0) {
    min(next_markers) - 1
  } else {
    region_end - 1
  }
  
  section <- js_lines[
    chart_marker:section_end
  ]
  
  # Replace the chart's stored matrix comment.
  section <- section[
    !grepl(
      "^\\s*//\\s*BuildR matrix:",
      section
    )
  ]
  
  metadata_position <- grep(
    "^\\s*//\\s*BuildR chart type:",
    section
  )
  
  insertion_position <- if (
    length(metadata_position) == 1
  ) {
    metadata_position
  } else {
    1L
  }
  
  if (nzchar(matrix)) {
    
    section <- append(
      section,
      paste0(
        "    // BuildR matrix: ",
        matrix
      ),
      after = insertion_position
    )
  }
  
  updated_js <- c(
    if (chart_marker > 1) {
      js_lines[
        seq_len(chart_marker - 1)
      ]
    } else {
      character()
    },
    
    section,
    
    if (section_end < length(js_lines)) {
      js_lines[
        seq.int(
          section_end + 1,
          length(js_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  if (nzchar(matrix)) {
    
    data_variable <- paste0(
      matrix,
      "_data"
    )
    
    metadata_variable <- paste0(
      matrix,
      "_meta"
    )
    
    escaped_matrix <- escape_javascript_string(
      matrix
    )
    
    declaration_pattern <- paste0(
      "^\\s*const\\s+\\[\\s*",
      matrix,
      "_data\\s*,\\s*",
      matrix,
      "_meta\\s*\\]\\s*=\\s*await\\s+readData\\(",
      "[\"']",
      matrix,
      "[\"']",
      "\\);\\s*$"
    )
    
    matrix_already_loaded <- any(
      grepl(
        declaration_pattern,
        updated_js,
        perl = TRUE
      )
    )
    
    if (!matrix_already_loaded) {
      
      region_start <- grep(
        "^\\s*//\\s*Insert chart content below\\s*$",
        updated_js
      )
      
      chart_marker <- grep(
        paste0(
          "^\\s*//\\s*Content for chart\\s+",
          chart_number,
          "\\s*$"
        ),
        updated_js
      )
      
      chart_marker <- chart_marker[
        chart_marker > region_start
      ]
      
      matrix_comment <- grep(
        paste0(
          "^\\s*//\\s*BuildR matrix:\\s*",
          matrix,
          "\\s*$"
        ),
        updated_js
      )
      
      matrix_comment <- matrix_comment[
        matrix_comment > chart_marker
      ]
      
      if (length(matrix_comment) != 1) {
        stop(
          "Could not identify where to insert the chart dataset."
        )
      }
      
      load_lines <- c(
        paste0(
          "    const [",
          data_variable,
          ", ",
          metadata_variable,
          '] = await readData("',
          escaped_matrix,
          '");'
        ),
        ""
      )
      
      updated_js <- append(
        updated_js,
        load_lines,
        after = matrix_comment
      )
    }
  }
  
  tryCatch(
    {
      writeLines(
        updated_js,
        paths$js,
        useBytes = TRUE
      )
    },
    error = function(error) {
      
      writeLines(
        original_js,
        paths$js,
        useBytes = TRUE
      )
      
      stop(
        paste(
          "The chart dataset change was restored after an error:",
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
  
  invisible(paths$js)
}

find_line_chart_year_definition <- function(
    project_root,
    matrix
) {
  calculation_data <- read_card_calculation_data(
    project_root = project_root,
    matrix = matrix
  )
  
  year_filters <- Filter(
    function(filter_definition) {
      isTRUE(filter_definition$is_year)
    },
    calculation_data$row_filters
  )
  
  if (length(year_filters) == 1) {
    year_column <- year_filters[[1]]$column
  } else {
    metadata_path <- file.path(
      project_root,
      "public",
      "data",
      "data.json"
    )
    
    metadata_text <- paste(
      readLines(
        metadata_path,
        warn = FALSE,
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )
    
    metadata <- jsonlite::fromJSON(
      metadata_text,
      simplifyVector = FALSE
    )
    
    variables <- metadata[[matrix]]$variables
    
    year_variables <- Filter(
      function(variable) {
        grepl(
          "^TLIST\\((A1|Q1|M1|W1)\\)$",
          as.character(variable$code)[1]
        )
      },
      variables
    )
    
    if (length(year_variables) != 1) {
      stop(
        paste0(
          "Could not uniquely identify the time variable for ",
          matrix,
          "."
        )
      )
    }
    
    year_column <- as.character(
      year_variables[[1]]$name
    )[1]
  }
  
  if (!year_column %in% names(calculation_data$data)) {
    stop(
      paste0(
        "The time column '",
        year_column,
        "' was not found in ",
        matrix,
        ".csv."
      )
    )
  }
  
  year_values <- unique(
    as.character(
      calculation_data$data[[year_column]]
    )
  )
  
  year_values <- year_values[
    !is.na(year_values) &
      nzchar(trimws(year_values))
  ]
  
  numeric_values <- suppressWarnings(
    as.numeric(year_values)
  )
  
  if (all(!is.na(numeric_values))) {
    year_values <- year_values[
      order(numeric_values)
    ]
  } else {
    year_values <- sort(year_values)
  }
  
  list(
    column = year_column,
    values = year_values,
    calculation_data = calculation_data
  )
}

select_line_chart_years <- function(
    available_years,
    mode = "all",
    recent_years = 5L
) {
  available_years <- as.character(
    available_years
  )
  
  if (identical(mode, "all")) {
    return(available_years)
  }
  
  recent_years <- as.integer(
    recent_years
  )
  
  if (
    length(recent_years) != 1 ||
    is.na(recent_years) ||
    recent_years < 1
  ) {
    stop(
      "The number of recent years must be at least 1."
    )
  }
  
  number_to_keep <- min(
    recent_years,
    length(available_years)
  )
  
  tail(
    available_years,
    number_to_keep
  )
}

javascript_array_value <- function(value) {
  value <- as.character(value)[1]
  
  numeric_value <- suppressWarnings(
    as.numeric(value)
  )
  
  if (
    !is.na(numeric_value) &&
    grepl(
      "^-?[0-9]+(?:\\.[0-9]+)?$",
      value
    )
  ) {
    return(value)
  }
  
  javascript_string(value)
}

build_chart_filter_condition <- function(
    column_name,
    selected_values,
    year_prefix = NULL
) {
  if (
    is.null(selected_values) ||
    length(selected_values) == 0
  ) {
    return(character())
  }
  
  js_value <- function(value) {
    
    dynamic_year_value <- javascript_dynamic_year_value(
      value = value,
      year_prefix = year_prefix
    )
    
    if (!is.null(dynamic_year_value)) {
      return(dynamic_year_value)
    }
    
    javascript_string(value)
  }
  
  if (length(selected_values) == 1) {
    return(
      paste0(
        "row[",
        javascript_string(column_name),
        "] == ",
        js_value(selected_values[[1]])
      )
    )
  }
  
  paste0(
    "[",
    paste(
      vapply(
        selected_values,
        js_value,
        character(1)
      ),
      collapse = ", "
    ),
    "].includes(row[",
    javascript_string(column_name),
    "])"
  )
}

build_page_line_chart_js <- function(
    chart_number,
    matrix,
    year_column,
    pivot_label,
    year_mode,
    recent_years,
    lines,
    unit,
    show_points,
    year_prefix = NULL
) {
  
  data_variable <- paste0(
    matrix,
    "_data"
  )
  
  years_variable <- paste0(
    "line_chart_",
    chart_number,
    "_years"
  )
  
  #
  # Build years dynamically from this matrix's
  # actual year column.
  #
  years_lines <- c(
    paste0(
      "    let ",
      years_variable,
      " = ",
      data_variable
    ),
    paste0(
      "        .map(col => col[",
      javascript_string(year_column),
      "]);"
    ),
    "",
    paste0(
      "    ",
      years_variable,
      " = [...new Set(",
      years_variable,
      ")];"
    )
  )
  
  #
  # Restrict to the most recent n years when requested.
  #
  if (identical(year_mode, "recent")) {
    
    recent_years <- as.integer(
      recent_years
    )
    
    if (
      length(recent_years) != 1 ||
      is.na(recent_years) ||
      recent_years < 1
    ) {
      stop(
        "The number of recent years must be at least 1."
      )
    }
    
    years_lines <- c(
      years_lines,
      "",
      paste0(
        "    ",
        years_variable,
        " = ",
        years_variable,
        ".slice(-",
        recent_years,
        ");"
      )
    )
  }
  
  if (
    is.null(lines) ||
    length(lines) == 0
  ) {
    stop(
      "At least one line must be configured."
    )
  }
  
  #
  # Build each configured line.
  #
  series_lines <- vapply(
    lines,
    function(line_definition) {
      
      if (
        is.null(line_definition$column) ||
        !nzchar(line_definition$column)
      ) {
        stop(
          "Every line must have a value column."
        )
      }
      
      #
      # Keep only rows whose year appears in the
      # selected line-chart year array.
      #
      filter_conditions <- c(
        paste0(
          years_variable,
          ".includes(row[",
          javascript_string(year_column),
          "])"
        )
      )
      
      line_filters <- line_definition$filters
      
      if (
        !is.null(line_filters) &&
        length(line_filters) > 0
      ) {
        
        for (
          column_name in
          names(line_filters)
        ) {
          
          filter_conditions <- c(
            filter_conditions,
            build_chart_filter_condition(
              column_name = column_name,
              selected_values =
                line_filters[[
                  column_name
                ]],
              year_prefix = year_prefix
            )
          )
        }
      }
      
      paste0(
        "        ",
        data_variable,
        "\n",
        "            .filter(row => ",
        paste(
          filter_conditions,
          collapse = " &&\n                "
        ),
        ")\n",
        "            .map(col => col[",
        javascript_string(
          line_definition$column
        ),
        "])"
      )
    },
    character(1)
  )
  
  line_labels <- vapply(
    lines,
    function(line_definition) {
      javascript_string(
        line_definition$label
      )
    },
    character(1)
  )
  
  labels_block <- paste0(
    "    const line_chart_",
    chart_number,
    "_labels = [",
    paste(
      line_labels,
      collapse = ", "
    ),
    "];"
  )
  
  unit <- as.character(unit)[1]
  
  if (is.na(unit)) {
    unit <- ""
  }
  
  show_points_js <- if (
    isTRUE(show_points)
  ) {
    "true"
  } else {
    "false"
  }
  
  chart_call <- c(
    "",
    "    lineChart({",
    paste0(
      "        years: line_chart_",
      chart_number,
      "_years,"
    ),
    paste0(
      "        lines: line_chart_",
      chart_number,
      "_lines,"
    ),
    paste0(
      "        labels: line_chart_",
      chart_number,
      "_labels,"
    ),
    paste0(
      "        unit: ",
      javascript_string(unit),
      ","
    ),
    paste0(
      '        canvas_id: "line-canvas-',
      chart_number,
      '",'
    ),
    paste0(
      '        expanded_canvas_id: "line-canvas-',
      chart_number,
      '-expanded",'
    ),
    paste0(
      "        showPoints: ",
      show_points_js
    ),
    "    });"
  )
  
  query_variable <- paste0(
    "line_chart_",
    chart_number,
    "_query"
  )
  
  #
  # Work out which ordinary row-filter dimensions
  # are restricted consistently across every line.
  #
  filter_names <- unique(
    unlist(
      lapply(
        lines,
        function(line_definition) {
          names(
            line_definition$filters
          )
        }
      ),
      use.names = FALSE
    )
  )
  
  query_entries <- character()
  
  #
  # Only restrict the time dimension in the download
  # query when "Most recent n years" is selected.
  #
  if (identical(year_mode, "recent")) {
    
    query_entries <- c(
      query_entries,
      paste0(
        "        ",
        javascript_string(year_column),
        ": line_chart_",
        chart_number,
        "_years"
      )
    )
  }
  
  #
  # Add ordinary filter dimensions.
  #
  # If any configured line leaves a dimension
  # unrestricted, omit that dimension from the
  # download query.
  #
  for (filter_name in filter_names) {
    
    line_has_filter <- vapply(
      lines,
      function(line_definition) {
        !is.null(
          line_definition$filters[[
            filter_name
          ]]
        ) &&
          length(
            line_definition$filters[[
              filter_name
            ]]
          ) > 0
      },
      logical(1)
    )
    
    if (!all(line_has_filter)) {
      next
    }
    
    selected_values <- unique(
      unlist(
        lapply(
          lines,
          function(line_definition) {
            as.character(
              line_definition$filters[[
                filter_name
              ]]
            )
          }
        ),
        use.names = FALSE
      )
    )
    
    if (length(selected_values) == 1) {
      
      query_entries <- c(
        query_entries,
        paste0(
          "        ",
          javascript_string(filter_name),
          ": ",
          javascript_query_value(
            selected_values[[1]],
            year_prefix = year_prefix
          )
        )
      )
      
    } else {
      
      query_entries <- c(
        query_entries,
        paste0(
          "        ",
          javascript_string(filter_name),
          ": [",
          paste(
            vapply(
              selected_values,
              function(value) {
                javascript_query_value(
                  value,
                  year_prefix = year_prefix
                )
              },
              character(1)
            ),
            collapse = ", "
          ),
          "]"
        )
      )
    }
  }
  
  #
  # The pivot variable is represented by the selected
  # CSV value columns, so add all unique line columns
  # under the corresponding metadata variable name.
  #
  pivot_values <- unique(
    vapply(
      lines,
      function(line_definition) {
        as.character(
          line_definition$column
        )[1]
      },
      character(1)
    )
  )
  
  if (
    !is.null(pivot_label) &&
    nzchar(pivot_label) &&
    length(pivot_values) > 0
  ) {
    
    if (length(pivot_values) == 1) {
      
      query_entries <- c(
        query_entries,
        paste0(
          "        ",
          javascript_string(pivot_label),
          ": ",
          javascript_string(
            pivot_values
          )
        )
      )
      
    } else {
      
      query_entries <- c(
        query_entries,
        paste0(
          "        ",
          javascript_string(pivot_label),
          ": [",
          paste(
            vapply(
              pivot_values,
              javascript_string,
              character(1)
            ),
            collapse = ", "
          ),
          "]"
        )
      )
    }
  }
  
  query_block <- if (
    length(query_entries) == 0
  ) {
    
    paste0(
      "    const ",
      query_variable,
      " = {};"
    )
    
  } else {
    
    c(
      paste0(
        "    const ",
        query_variable,
        " = {"
      ),
      paste0(
        query_entries,
        collapse = ",\n"
      ),
      "    };"
    )
  }
  
  download_call <- c(
    "",
    "    downloadButton(",
    paste0(
      '        "chart-',
      chart_number,
      '-capture",'
    ),
    paste0(
      "        ",
      javascript_string(matrix),
      ","
    ),
    paste0(
      "        dateFormat(",
      matrix,
      "_meta.updated),"
    ),
    paste0(
      "        ",
      query_variable
    ),
    "    );"
  )
  
  lines_block <- c(
    paste0(
      "    const line_chart_",
      chart_number,
      "_lines = ["
    ),
    paste0(
      series_lines,
      collapse = ",\n\n"
    ),
    "    ];"
  )
  
  c(
    years_lines,
    "",
    lines_block,
    "",
    labels_block,
    chart_call,
    "",
    query_block,
    download_call
  )
}

update_page_line_chart_js <- function(
    project_root,
    page_href,
    chart_number,
    matrix,
    line_chart_js,
    use_matrix_years = FALSE
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$js)) {
    stop(
      paste0(
        paths$js_filename,
        " was not found."
      )
    )
  }
  
  chart_number <- as.integer(
    chart_number
  )
  
  if (
    length(chart_number) != 1 ||
    is.na(chart_number) ||
    chart_number < 1 ||
    chart_number > 3
  ) {
    stop("A valid chart number is required.")
  }
  
  js_lines <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  region_start <- grep(
    "^\\s*//\\s*Insert chart content below\\s*$",
    js_lines
  )
  
  region_end <- grep(
    "^\\s*//\\s*End chart content\\s*$",
    js_lines
  )
  
  if (
    length(region_start) != 1 ||
    length(region_end) != 1 ||
    region_end <= region_start
  ) {
    stop(
      "Could not uniquely identify the chart JavaScript region."
    )
  }
  
  chart_marker <- grep(
    paste0(
      "^\\s*//\\s*Content for chart\\s+",
      chart_number,
      "\\s*$"
    ),
    js_lines
  )
  
  chart_marker <- chart_marker[
    chart_marker > region_start &
      chart_marker < region_end
  ]
  
  if (length(chart_marker) != 1) {
    stop(
      paste0(
        "Could not identify the JavaScript section for chart ",
        chart_number,
        "."
      )
    )
  }
  
  needs_update_year_spans <- !page_js_has_update_year_spans_before(
    js_lines = js_lines,
    line_number = chart_marker
  )
  
  year_update_lines <- if (
    isTRUE(needs_update_year_spans)
  ) {
    c(
      paste0(
        "    updateYearSpans(",
        matrix,
        "_data, ",
        matrix,
        "_meta);"
      ),
      ""
    )
  } else {
    character()
  }
  
  matrix_year_lines <- if (
    isTRUE(use_matrix_years) &&
    !isTRUE(needs_update_year_spans)
  ) {
    build_matrix_year_variables_js(
      matrix = matrix
    )
  } else {
    character()
  }
  
  matrix_year_span_lines <- if (
    isTRUE(use_matrix_years) &&
    !isTRUE(needs_update_year_spans)
  ) {
    build_matrix_year_span_js(
      matrix = matrix
    )
  } else {
    character()
  }
  
  all_chart_markers <- grep(
    "^\\s*//\\s*Content for chart\\s+[0-9]+\\s*$",
    js_lines
  )
  
  all_chart_markers <- all_chart_markers[
    all_chart_markers > region_start &
      all_chart_markers < region_end
  ]
  
  later_markers <- all_chart_markers[
    all_chart_markers > chart_marker
  ]
  
  section_end <- if (
    length(later_markers) > 0
  ) {
    min(later_markers) - 1
  } else {
    region_end - 1
  }
  
  section <- js_lines[
    chart_marker:section_end
  ]
  
  section <- remove_page_chart_config_blocks(
    section
  )
  
  while (
    length(section) > 0 &&
    !nzchar(trimws(section[[length(section)]]))
  ) {
    section <- section[-length(section)]
  }
  
  replacement_section <- c(
    section,
    "",
    "    // BuildR line chart config start",
    year_update_lines,
    matrix_year_lines,
    matrix_year_span_lines,
    line_chart_js,
    "    // BuildR line chart config end",
    ""
  )
  
  updated_js <- c(
    if (chart_marker > 1) {
      js_lines[
        seq_len(chart_marker - 1)
      ]
    } else {
      character()
    },
    
    replacement_section,
    
    if (section_end < length(js_lines)) {
      js_lines[
        seq.int(
          section_end + 1,
          length(js_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  writeLines(
    updated_js,
    paths$js,
    useBytes = TRUE
  )
  
  invisible(paths$js)
}

normalise_page_chart_capture_ids <- function(
    html_lines
) {
  
  chart_region <- find_page_chart_cards_region(
    html_lines
  )
  
  if (length(chart_region$chart_starts) == 0) {
    return(html_lines)
  }
  
  for (
    chart_number in
    seq_along(chart_region$chart_starts)
  ) {
    
    chart_start <- chart_region$chart_starts[
      chart_number
    ]
    
    chart_end <- find_closing_div(
      html_lines,
      chart_start
    )
    
    capture_candidates <- grep(
      '<div\\b[^>]*id=["\'][^"\']+["\']',
      html_lines,
      perl = TRUE
    )
    
    capture_candidates <- capture_candidates[
      capture_candidates > chart_start &
        capture_candidates < chart_end
    ]
    
    if (length(capture_candidates) == 0) {
      stop(
        paste0(
          "Could not identify the capture wrapper for chart ",
          chart_number,
          "."
        )
      )
    }
    
    capture_line <- capture_candidates[[1]]
    
    html_lines[capture_line] <- sub(
      '(id\\s*=\\s*["\'])[^"\']+(["\'])',
      paste0(
        "\\1chart-",
        chart_number,
        "-capture\\2"
      ),
      html_lines[capture_line],
      perl = TRUE
    )
  }
  
  html_lines
}

javascript_query_value <- function(
  value,
  year_prefix = NULL
  
) {
  
  dynamic_year_value <- javascript_dynamic_year_value(
    value = value,
    year_prefix = year_prefix
  )
  
  if (!is.null(dynamic_year_value)) {
    return(dynamic_year_value)
  }
  
  if (identical(value, "__LATEST_YEAR__")) {
    return("latest_year")
  }
  
  if (identical(value, "__FIRST_YEAR__")) {
    return("first_year")
  }
  
  javascript_string(value)
}

build_page_bar_chart_js <- function(
    chart_number,
    matrix,
    settings,
    year_prefix = NULL
) {
  data_variable <- paste0(
    matrix,
    "_data"
  )
  
  bar_data_variable <- paste0(
    "bar_chart_",
    chart_number,
    "_data"
  )
  
  query_variable <- paste0(
    "bar_chart_",
    chart_number,
    "_query"
  )
  
  filter_conditions <- character()
  
  #
  # Ordinary filters
  #
  if (
    !is.null(settings$filters) &&
    length(settings$filters) > 0
  ) {
    for (
      column_name in
      names(settings$filters)
    ) {
      filter_conditions <- c(
        filter_conditions,
        build_chart_filter_condition(
          column_name = column_name,
          selected_values = settings$filters[[column_name]],
          year_prefix = year_prefix
        )
      )
    }
  }
  
  #
  # Restrict category values if selected.
  #
  if (
    !is.null(settings$category_values) &&
    length(settings$category_values) > 0
  ) {
    filter_conditions <- c(
      filter_conditions,
      build_chart_filter_condition(
        column_name = column_name,
        selected_values = settings$filters[[column_name]],
        year_prefix = year_prefix
      )
    )
  }
  
  #
  # Restrict series-category values.
  #
  if (
    identical(
      settings$series_source,
      "category_values"
    ) &&
    !is.null(settings$bar_values) &&
    length(settings$bar_values) > 0
  ) {
    filter_conditions <- c(
      filter_conditions,
      build_chart_filter_condition(
        column_name = column_name,
        selected_values = settings$filters[[column_name]],
        year_prefix = year_prefix
      )
    )
  }
  
  data_block <- if (
    length(filter_conditions) == 0
  ) {
    paste0(
      "    const ",
      bar_data_variable,
      " = ",
      data_variable,
      ";"
    )
  } else {
    c(
      paste0(
        "    const ",
        bar_data_variable,
        " = ",
        data_variable
      ),
      paste0(
        "        .filter(row => ",
        paste(
          filter_conditions,
          collapse = " &&\n                       "
        ),
        ");"
      )
    )
  }
  
  #
  # value and bars arguments
  #
  if (
    identical(
      settings$series_source,
      "value_columns"
    )
  ) {
    value_js <- paste0(
      "[",
      paste(
        vapply(
          settings$values,
          javascript_string,
          character(1)
        ),
        collapse = ", "
      ),
      "]"
    )
    
    bars_js <- "null"
    
  } else {
    value_js <- javascript_string(
      settings$values[[1]]
    )
    
    bars_js <- javascript_string(
      settings$bars
    )
  }
  
  label_format <- if (
    is.null(settings$label_format)
  ) {
    ""
  } else {
    settings$label_format
  }
  
  align <- if (
    is.null(settings$align) ||
    !settings$align %in% c(
      "vertical",
      "horizontal"
    )
  ) {
    "vertical"
  } else {
    settings$align
  }
  
  y_label <- if (
    is.null(settings$y_label)
  ) {
    ""
  } else {
    settings$y_label
  }
  
  chart_call <- c(
    "",
    "    barChart({",
    paste0(
      "        data: ",
      bar_data_variable,
      ","
    ),
    paste0(
      "        value: ",
      value_js,
      ","
    ),
    paste0(
      "        bars: ",
      bars_js,
      ","
    ),
    paste0(
      "        categories: ",
      javascript_string(
        settings$categories
      ),
      ","
    ),
    paste0(
      '        canvas_id: "bar-canvas-',
      chart_number,
      '",'
    ),
    paste0(
      '        expanded_canvas_id: "bar-canvas-',
      chart_number,
      '-expanded",'
    ),
    paste0(
      "        label_format: ",
      javascript_string(
        label_format
      ),
      ","
    ),
    paste0(
      "        stacked: ",
      if (isTRUE(settings$stacked)) {
        "true"
      } else {
        "false"
      },
      ","
    ),
    paste0(
      "        align: ",
      javascript_string(
        align
      ),
      ","
    ),
    paste0(
      "        y_label: ",
      javascript_string(
        y_label
      )
    ),
    "    });"
  )
  
  #
  # Download query
  #
  query_entries <- character()
  
  if (
    !is.null(settings$filters) &&
    length(settings$filters) > 0
  ) {
    for (
      column_name in
      names(settings$filters)
    ) {
      values <- settings$filters[[
        column_name
      ]]
      
      value_js_query <- if (
        length(values) == 1
      ) {
        javascript_query_value(
          values[[1]],
          year_prefix = year_prefix
        )
      } else {
        paste0(
          "[",
          paste(
            vapply(
              values,
              function(value) {
                javascript_query_value(
                  value,
                  year_prefix = year_prefix
                )
              },
              character(1)
            ),
            collapse = ", "
          ),
          "]"
        )
      }
      
      query_entries <- c(
        query_entries,
        paste0(
          "        ",
          javascript_string(
            column_name
          ),
          ": ",
          value_js_query
        )
      )
    }
  }
  
  if (
    !is.null(settings$category_values) &&
    length(settings$category_values) > 0
  ) {
    category_query_value <- if (
      length(settings$category_values) == 1
    ) {
      javascript_string(
        settings$category_values[[1]]
      )
    } else {
      paste0(
        "[",
        paste(
          vapply(
            settings$category_values,
            javascript_string,
            character(1)
          ),
          collapse = ", "
        ),
        "]"
      )
    }
    
    query_entries <- c(
      query_entries,
      paste0(
        "        ",
        javascript_string(
          settings$categories
        ),
        ": ",
        category_query_value
      )
    )
  }
  
  if (
    identical(
      settings$series_source,
      "category_values"
    ) &&
    length(settings$bar_values) > 0
  ) {
    bar_query_value <- if (
      length(settings$bar_values) == 1
    ) {
      javascript_string(
        settings$bar_values[[1]]
      )
    } else {
      paste0(
        "[",
        paste(
          vapply(
            settings$bar_values,
            javascript_string,
            character(1)
          ),
          collapse = ", "
        ),
        "]"
      )
    }
    
    query_entries <- c(
      query_entries,
      paste0(
        "        ",
        javascript_string(
          settings$bars
        ),
        ": ",
        bar_query_value
      )
    )
  }
  
  #
  # Pivoted value dimension
  #
  if (
    !is.null(settings$pivot_label) &&
    nzchar(settings$pivot_label)
  ) {
    pivot_values <- settings$values
    
    pivot_query_value <- if (
      length(pivot_values) == 1
    ) {
      javascript_string(
        pivot_values[[1]]
      )
    } else {
      paste0(
        "[",
        paste(
          vapply(
            pivot_values,
            javascript_string,
            character(1)
          ),
          collapse = ", "
        ),
        "]"
      )
    }
    
    query_entries <- c(
      query_entries,
      paste0(
        "        ",
        javascript_string(
          settings$pivot_label
        ),
        ": ",
        pivot_query_value
      )
    )
  }
  
  query_block <- if (
    length(query_entries) == 0
  ) {
    paste0(
      "    const ",
      query_variable,
      " = {};"
    )
  } else {
    c(
      paste0(
        "    const ",
        query_variable,
        " = {"
      ),
      paste0(
        query_entries,
        collapse = ",\n"
      ),
      "    };"
    )
  }
  
  download_call <- c(
    "",
    "    downloadButton(",
    paste0(
      '        "chart-',
      chart_number,
      '-capture",'
    ),
    paste0(
      "        ",
      javascript_string(
        matrix
      ),
      ","
    ),
    paste0(
      "        dateFormat(",
      matrix,
      "_meta.updated),"
    ),
    paste0(
      "        ",
      query_variable
    ),
    "    );"
  )
  
  c(
    data_block,
    chart_call,
    "",
    query_block,
    download_call
  )
}

update_page_bar_chart_js <- function(
    project_root,
    page_href,
    chart_number,
    matrix,
    bar_chart_js,
    use_matrix_years = FALSE
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  js_lines <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  region_start <- grep(
    "^\\s*//\\s*Insert chart content below\\s*$",
    js_lines
  )
  
  region_end <- grep(
    "^\\s*//\\s*End chart content\\s*$",
    js_lines
  )
  
  if (
    length(region_start) != 1 ||
    length(region_end) != 1 ||
    region_end <= region_start
  ) {
    stop(
      "Could not uniquely identify the chart JavaScript region."
    )
  }
  
  chart_marker <- grep(
    paste0(
      "^\\s*//\\s*Content for chart\\s+",
      chart_number,
      "\\s*$"
    ),
    js_lines
  )
  
  chart_marker <- chart_marker[
    chart_marker > region_start &
      chart_marker < region_end
  ]
  
  if (length(chart_marker) != 1) {
    stop(
      paste0(
        "Could not identify the JavaScript section for chart ",
        chart_number,
        "."
      )
    )
  }
  
  needs_update_year_spans <- !page_js_has_update_year_spans_before(
    js_lines = js_lines,
    line_number = chart_marker
  )
  
  year_update_lines <- if (
    isTRUE(needs_update_year_spans)
  ) {
    c(
      paste0(
        "    updateYearSpans(",
        matrix,
        "_data, ",
        matrix,
        "_meta);"
      ),
      ""
    )
  } else {
    character()
  }
  
  matrix_year_lines <- if (
    isTRUE(use_matrix_years) &&
    !isTRUE(needs_update_year_spans)
  ) {
    build_matrix_year_variables_js(
      matrix = matrix
    )
  } else {
    character()
  }
  
  matrix_year_span_lines <- if (
    isTRUE(use_matrix_years) &&
    !isTRUE(needs_update_year_spans)
  ) {
    build_matrix_year_span_js(
      matrix = matrix
    )
  } else {
    character()
  }
  
  all_chart_markers <- grep(
    "^\\s*//\\s*Content for chart\\s+[0-9]+\\s*$",
    js_lines
  )
  
  all_chart_markers <- all_chart_markers[
    all_chart_markers > region_start &
      all_chart_markers < region_end
  ]
  
  later_markers <- all_chart_markers[
    all_chart_markers > chart_marker
  ]
  
  section_end <- if (
    length(later_markers) > 0
  ) {
    min(later_markers) - 1
  } else {
    region_end - 1
  }
  
  section <- js_lines[
    chart_marker:section_end
  ]
  
  section <- remove_page_chart_config_blocks(
    section
  )
  
  while (
    length(section) > 0 &&
    !nzchar(trimws(section[[length(section)]]))
  ) {
    section <- section[-length(section)]
  }
  
  replacement_section <- c(
    section,
    "",
    "    // BuildR bar chart config start",
    year_update_lines,
    matrix_year_lines,
    matrix_year_span_lines,
    bar_chart_js,
    "    // BuildR bar chart config end",
    ""
  )
  
  updated_js <- c(
    if (chart_marker > 1) {
      js_lines[
        seq_len(
          chart_marker - 1
        )
      ]
    } else {
      character()
    },
    
    replacement_section,
    
    if (section_end < length(js_lines)) {
      js_lines[
        seq.int(
          section_end + 1,
          length(js_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  writeLines(
    updated_js,
    paths$js,
    useBytes = TRUE
  )
  
  invisible(
    paths$js
  )
}

page_pie_type_choices <- function() {
  c(
    "Pie" = "pie",
    "Doughnut" = "doughnut"
  )
}

build_page_pie_chart_js <- function(
    chart_number,
    matrix,
    settings,
    year_prefix = NULL
) {
  
  data_variable <- paste0(
    matrix,
    "_data"
  )
  
  pie_data_variable <- paste0(
    "pie_chart_",
    chart_number,
    "_data"
  )
  
  query_variable <- paste0(
    "pie_chart_",
    chart_number,
    "_query"
  )
  
  slice_source <- if (
    !is.null(settings$slice_source) &&
    settings$slice_source %in% c(
      "pivot",
      "row"
    )
  ) {
    settings$slice_source
  } else {
    "pivot"
  }
  
  filter_conditions <- character()
  
  if (
    !is.null(settings$filters) &&
    length(settings$filters) > 0
  ) {
    
    for (
      column_name in
      names(settings$filters)
    ) {
      
      filter_conditions <- c(
        filter_conditions,
        build_chart_filter_condition(
          column_name = column_name,
          selected_values =
            settings$filters[[
              column_name
            ]],
          year_prefix = year_prefix
        )
      )
    }
  }
  
  #
  # MODE 1:
  # Pivoted variable provides the slices.
  #
  # Example:
  #
  #   Slice variable = Farm size
  #   Values = Very small, Small, Medium, Large
  #
  # One filtered row is converted into one object.
  #
  if (identical(
    slice_source,
    "pivot"
  )) {
    
    mapped_values <- vapply(
      settings$values,
      function(value_name) {
        
        paste0(
          "            ",
          javascript_string(value_name),
          ": col[",
          javascript_string(value_name),
          "]"
        )
      },
      character(1)
    )
    
    if (length(filter_conditions) > 0) {
      
      data_block <- c(
        paste0(
          "    const ",
          pie_data_variable,
          " = ",
          data_variable
        ),
        paste0(
          "        .filter(row => ",
          paste(
            filter_conditions,
            collapse = " &&\n                       "
          ),
          ")"
        ),
        "        .map(col => ({",
        paste0(
          mapped_values,
          collapse = ",\n"
        ),
        "        }))[0];"
      )
      
    } else {
      
      data_block <- c(
        paste0(
          "    const ",
          pie_data_variable,
          " = ",
          data_variable
        ),
        "        .map(col => ({",
        paste0(
          mapped_values,
          collapse = ",\n"
        ),
        "        }))[0];"
      )
    }
    
  } else {
    
    #
    # MODE 2:
    # Row-variable values provide the slices.
    #
    # Example:
    #
    #   Slice variable = Equality group
    #   Values = Sex - Female, Sex - Male
    #   Value column = All farms
    #
    
    if (
      is.null(settings$slice_variable) ||
      !nzchar(settings$slice_variable)
    ) {
      stop(
        "A row-based pie chart requires a slice variable."
      )
    }
    
    if (
      is.null(settings$value_column) ||
      !nzchar(settings$value_column)
    ) {
      stop(
        "A row-based pie chart requires a value column."
      )
    }
    
    slice_filter_condition <- build_chart_filter_condition(
      column_name = settings$slice_variable,
      selected_values = settings$values,
      year_prefix = year_prefix
    )
    
    all_conditions <- c(
      filter_conditions,
      slice_filter_condition
    )
    
    #
    # Build an object for pieChart(), preserving the
    # user's selected slice order.
    #
    #
    # Example output:
    #
    # const pie_chart_1_data = {
    #     "Sex - Female": FARMSIZEEQ_data
    #         .filter(...)[0]["All farms"],
    #     "Sex - Male": FARMSIZEEQ_data
    #         .filter(...)[0]["All farms"]
    # };
    #
    
    mapped_values <- vapply(
      settings$values,
      function(slice_value) {
        
        slice_specific_conditions <- c(
          filter_conditions,
          build_chart_filter_condition(
            column_name =
              settings$slice_variable,
            selected_values =
              slice_value,
            year_prefix =
              year_prefix
          )
        )
        
        paste0(
          "        ",
          javascript_string(slice_value),
          ": ",
          data_variable,
          "\n",
          "            .filter(row => ",
          paste(
            slice_specific_conditions,
            collapse =
              " &&\n                           "
          ),
          ")\n",
          "            .map(col => col[",
          javascript_string(
            settings$value_column
          ),
          "])[0]"
        )
      },
      character(1)
    )
    
    data_block <- c(
      paste0(
        "    const ",
        pie_data_variable,
        " = {"
      ),
      paste0(
        mapped_values,
        collapse = ",\n"
      ),
      "    };"
    )
  }
  
  pie_type <- if (
    !is.null(settings$type) &&
    settings$type %in% c(
      "pie",
      "doughnut"
    )
  ) {
    settings$type
  } else {
    "pie"
  }
  
  chart_call <- c(
    "",
    "    pieChart({",
    paste0(
      "        data: ",
      pie_data_variable,
      ","
    ),
    paste0(
      '        canvas_id: "pie-canvas-',
      chart_number,
      '",'
    ),
    paste0(
      '        expanded_canvas_id: "pie-canvas-',
      chart_number,
      '-expanded",'
    ),
    paste0(
      "        type: ",
      javascript_string(
        pie_type
      )
    ),
    "    });"
  )
  
  #
  # Download query.
  #
  query_entries <- character()
  
  if (
    !is.null(settings$filters) &&
    length(settings$filters) > 0
  ) {
    
    for (
      column_name in
      names(settings$filters)
    ) {
      
      values <- settings$filters[[
        column_name
      ]]
      
      query_value <- if (
        length(values) == 1
      ) {
        
        javascript_query_value(
          values[[1]],
          year_prefix = year_prefix
        )
        
      } else {
        
        paste0(
          "[",
          paste(
            vapply(
              values,
              function(value) {
                javascript_query_value(
                  value,
                  year_prefix =
                    year_prefix
                )
              },
              character(1)
            ),
            collapse = ", "
          ),
          "]"
        )
      }
      
      query_entries <- c(
        query_entries,
        paste0(
          "        ",
          javascript_string(
            column_name
          ),
          ": ",
          query_value
        )
      )
    }
  }
  
  #
  # MODE 1 query:
  # Selected slice values belong to the pivoted
  # metadata variable.
  #
  if (
    identical(
      slice_source,
      "pivot"
    ) &&
    !is.null(settings$pivot_label) &&
    nzchar(settings$pivot_label)
  ) {
    
    pivot_query_value <- if (
      length(settings$values) == 1
    ) {
      
      javascript_string(
        settings$values[[1]]
      )
      
    } else {
      
      paste0(
        "[",
        paste(
          vapply(
            settings$values,
            javascript_string,
            character(1)
          ),
          collapse = ", "
        ),
        "]"
      )
    }
    
    query_entries <- c(
      query_entries,
      paste0(
        "        ",
        javascript_string(
          settings$pivot_label
        ),
        ": ",
        pivot_query_value
      )
    )
  }
  
  #
  # MODE 2 query:
  #
  # Selected slice labels belong to a row variable,
  # while value_column identifies one selected value
  # of the pivoted metadata variable.
  #
  if (identical(
    slice_source,
    "row"
  )) {
    
    slice_query_value <- if (
      length(settings$values) == 1
    ) {
      
      javascript_string(
        settings$values[[1]]
      )
      
    } else {
      
      paste0(
        "[",
        paste(
          vapply(
            settings$values,
            javascript_string,
            character(1)
          ),
          collapse = ", "
        ),
        "]"
      )
    }
    
    query_entries <- c(
      query_entries,
      paste0(
        "        ",
        javascript_string(
          settings$slice_variable
        ),
        ": ",
        slice_query_value
      )
    )
    
    if (
      !is.null(settings$pivot_label) &&
      nzchar(settings$pivot_label) &&
      !is.null(settings$value_column) &&
      nzchar(settings$value_column)
    ) {
      
      query_entries <- c(
        query_entries,
        paste0(
          "        ",
          javascript_string(
            settings$pivot_label
          ),
          ": ",
          javascript_string(
            settings$value_column
          )
        )
      )
    }
  }
  
  query_block <- if (
    length(query_entries) == 0
  ) {
    
    paste0(
      "    const ",
      query_variable,
      " = {};"
    )
    
  } else {
    
    c(
      paste0(
        "    const ",
        query_variable,
        " = {"
      ),
      paste0(
        query_entries,
        collapse = ",\n"
      ),
      "    };"
    )
  }
  
  download_call <- c(
    "",
    "    downloadButton(",
    paste0(
      '        "chart-',
      chart_number,
      '-capture",'
    ),
    paste0(
      "        ",
      javascript_string(
        matrix
      ),
      ","
    ),
    paste0(
      "        dateFormat(",
      matrix,
      "_meta.updated),"
    ),
    paste0(
      "        ",
      query_variable
    ),
    "    );"
  )
  
  c(
    data_block,
    chart_call,
    "",
    query_block,
    download_call
  )
}

update_page_pie_chart_js <- function(
    project_root,
    page_href,
    chart_number,
    matrix,
    pie_chart_js,
    use_matrix_years = FALSE
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$js)) {
    stop(
      paste0(
        paths$js_filename,
        " was not found."
      )
    )
  }
  
  js_lines <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  region_start <- grep(
    "^\\s*//\\s*Insert chart content below\\s*$",
    js_lines
  )
  
  region_end <- grep(
    "^\\s*//\\s*End chart content\\s*$",
    js_lines
  )
  
  if (
    length(region_start) != 1 ||
    length(region_end) != 1 ||
    region_end <= region_start
  ) {
    stop(
      "Could not uniquely identify the chart JavaScript region."
    )
  }
  
  chart_marker <- grep(
    paste0(
      "^\\s*//\\s*Content for chart\\s+",
      chart_number,
      "\\s*$"
    ),
    js_lines
  )
  
  chart_marker <- chart_marker[
    chart_marker > region_start &
      chart_marker < region_end
  ]
  
  if (length(chart_marker) != 1) {
    stop(
      paste0(
        "Could not identify the JavaScript section for chart ",
        chart_number,
        "."
      )
    )
  }
  
  needs_update_year_spans <- !page_js_has_update_year_spans_before(
    js_lines = js_lines,
    line_number = chart_marker
  )
  
  year_update_lines <- if (
    isTRUE(needs_update_year_spans)
  ) {
    c(
      paste0(
        "    updateYearSpans(",
        matrix,
        "_data, ",
        matrix,
        "_meta);"
      ),
      ""
    )
  } else {
    character()
  }
  
  matrix_year_lines <- if (
    isTRUE(use_matrix_years) &&
    !isTRUE(needs_update_year_spans)
  ) {
    build_matrix_year_variables_js(
      matrix = matrix
    )
  } else {
    character()
  }
  
  matrix_year_span_lines <- if (
    isTRUE(use_matrix_years) &&
    !isTRUE(needs_update_year_spans)
  ) {
    build_matrix_year_span_js(
      matrix = matrix
    )
  } else {
    character()
  }
  
  all_chart_markers <- grep(
    "^\\s*//\\s*Content for chart\\s+[0-9]+\\s*$",
    js_lines
  )
  
  all_chart_markers <- all_chart_markers[
    all_chart_markers > region_start &
      all_chart_markers < region_end
  ]
  
  later_markers <- all_chart_markers[
    all_chart_markers > chart_marker
  ]
  
  section_end <- if (
    length(later_markers) > 0
  ) {
    min(later_markers) - 1
  } else {
    region_end - 1
  }
  
  section <- js_lines[
    chart_marker:section_end
  ]
  
  section <- remove_page_chart_config_blocks(
    section
  )
  
  while (
    length(section) > 0 &&
    !nzchar(trimws(section[[length(section)]]))
  ) {
    section <- section[-length(section)]
  }
  
  replacement_section <- c(
    section,
    "",
    "    // BuildR pie chart config start",
    year_update_lines,
    matrix_year_lines,
    matrix_year_span_lines,
    pie_chart_js,
    "    // BuildR pie chart config end",
    ""
  )
  
  updated_js <- c(
    if (chart_marker > 1) {
      js_lines[
        seq_len(
          chart_marker - 1
        )
      ]
    } else {
      character()
    },
    
    replacement_section,
    
    if (section_end < length(js_lines)) {
      js_lines[
        seq.int(
          section_end + 1,
          length(js_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  writeLines(
    updated_js,
    paths$js,
    useBytes = TRUE
  )
  
  invisible(
    paths$js
  )
}

pie_slice_variable_choices <- function(
    calculation_data
) {
  
  variables <- calculation_data$variables
  
  if (
    is.null(variables) ||
    length(variables) == 0
  ) {
    return(character())
  }
  
  valid_variables <- Filter(
    function(variable) {
      
      variable_name <- as.character(
        variable$name
      )[1]
      
      variable_code <- as.character(
        variable$code
      )[1]
      
      if (
        is.na(variable_name) ||
        !nzchar(trimws(variable_name))
      ) {
        return(FALSE)
      }
      
      is_time_variable <- grepl(
        "^TLIST",
        variable_code
      )
      
      is_statistic_variable <- identical(
        tolower(trimws(variable_name)),
        "statistic"
      )
      
      !is_time_variable &&
        !is_statistic_variable
    },
    variables
  )
  
  if (length(valid_variables) == 0) {
    return(character())
  }
  
  variable_names <- vapply(
    valid_variables,
    function(variable) {
      as.character(
        variable$name
      )[1]
    },
    character(1)
  )
  
  stats::setNames(
    variable_names,
    variable_names
  )
}


update_page_chart_canvas_html <- function(
    project_root,
    page_href,
    chart_number,
    canvas_prefix
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$html)) {
    stop(
      paste0(
        paths$href,
        " was not found."
      )
    )
  }
  
  chart_number <- as.integer(
    chart_number
  )
  
  if (
    length(chart_number) != 1 ||
    is.na(chart_number) ||
    chart_number < 1 ||
    chart_number > 3
  ) {
    stop(
      "A valid chart number is required."
    )
  }
  
  canvas_prefix <- as.character(
    canvas_prefix
  )[1]
  
  if (
    is.na(canvas_prefix) ||
    !nzchar(canvas_prefix)
  ) {
    stop(
      "A canvas prefix is required."
    )
  }
  
  html_lines <- readLines(
    paths$html,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  chart_region <- find_page_chart_cards_region(
    html_lines
  )
  
  if (
    chart_number >
    length(chart_region$chart_starts)
  ) {
    stop(
      paste0(
        "Chart ",
        chart_number,
        " was not found."
      )
    )
  }
  
  chart_start <- chart_region$chart_starts[
    chart_number
  ]
  
  chart_end <- find_closing_div(
    html_lines,
    chart_start
  )
  
  body_starts <- grep(
    'class=["\'][^"\']*\\bcard-body\\b',
    html_lines,
    perl = TRUE
  )
  
  body_starts <- body_starts[
    body_starts > chart_start &
      body_starts < chart_end
  ]
  
  if (length(body_starts) == 0) {
    stop(
      paste0(
        "Could not identify the card body for chart ",
        chart_number,
        "."
      )
    )
  }
  
  body_start <- body_starts[[1]]
  
  body_end <- find_closing_div(
    html_lines,
    body_start
  )
  
  indent <- sub(
    "^(\\s*).*",
    "\\1",
    html_lines[
      body_start
    ]
  )
  
  opening_tag <- sub(
    "^(\\s*<div\\b[^>]*>).*",
    "\\1",
    html_lines[
      body_start
    ],
    perl = TRUE
  )
  
  canvas_id <- paste0(
    canvas_prefix,
    "-canvas-",
    chart_number
  )
  
  replacement <- c(
    opening_tag,
    paste0(
      indent,
      "    ",
      '<canvas id="',
      canvas_id,
      '" class="chart-canvas"></canvas>'
    ),
    paste0(
      indent,
      "</div>"
    )
  )
  
  updated_html <- c(
    if (body_start > 1) {
      html_lines[
        seq_len(
          body_start - 1
        )
      ]
    } else {
      character()
    },
    
    replacement,
    
    if (body_end < length(html_lines)) {
      html_lines[
        seq.int(
          body_end + 1,
          length(html_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  writeLines(
    updated_html,
    paths$html,
    useBytes = TRUE
  )
  
  invisible(
    paths$html
  )
}

remove_marked_block <- function(
    lines,
    start_pattern,
    end_pattern
) {
  
  repeat {
    
    start <- grep(
      start_pattern,
      lines
    )
    
    if (length(start) == 0) {
      break
    }
    
    start <- start[[1]]
    
    end <- grep(
      end_pattern,
      lines
    )
    
    end <- end[
      end > start
    ]
    
    if (length(end) == 0) {
      break
    }
    
    end <- end[[1]]
    
    lines <- lines[
      -seq.int(
        start,
        end
      )
    ]
  }
  
  lines
}

remove_page_chart_config_blocks <- function(lines) {
  
  chart_types <- c(
    "line",
    "bar",
    "pie",
    "table",
    "map",
    "pyramid",
    "treemap"
  )
  
  for (chart_type in chart_types) {
    
    lines <- remove_marked_block(
      lines,
      paste0(
        "^\\s*//\\s*BuildR ",
        chart_type,
        " chart config start\\s*$"
      ),
      paste0(
        "^\\s*//\\s*BuildR ",
        chart_type,
        " chart config end\\s*$"
      )
    )
  }
  
  lines <- remove_marked_block(
    lines,
    "^\\s*//\\s*BuildR placeholder chart start\\s*$",
    "^\\s*//\\s*BuildR placeholder chart end\\s*$"
  )
  
  lines
}

build_page_treemap_chart_js <- function(
    chart_number,
    matrix,
    settings,
    year_prefix = NULL
) {
  
  data_variable <- paste0(
    matrix,
    "_data"
  )
  
  treemap_data_variable <- paste0(
    "treemap_chart_",
    chart_number,
    "_data"
  )
  
  query_variable <- paste0(
    "treemap_chart_",
    chart_number,
    "_query"
  )
  
  category_source <- if (
    !is.null(settings$category_source) &&
    settings$category_source %in% c(
      "row",
      "pivot"
    )
  ) {
    settings$category_source
  } else {
    "row"
  }
  
  filter_conditions <- character()
  
  #
  # Ordinary filters.
  #
  if (
    !is.null(settings$filters) &&
    length(settings$filters) > 0
  ) {
    
    for (
      column_name in
      names(settings$filters)
    ) {
      
      filter_conditions <- c(
        filter_conditions,
        build_chart_filter_condition(
          column_name = column_name,
          selected_values =
            settings$filters[[
              column_name
            ]],
          year_prefix = year_prefix
        )
      )
    }
  }
  
  #
  # MODE 1:
  # Categories come from a row variable.
  #
  if (identical(
    category_source,
    "row"
  )) {
    
    #
    # Restrict category values if the user selected any.
    #
    if (
      !is.null(settings$category_values) &&
      length(settings$category_values) > 0
    ) {
      
      filter_conditions <- c(
        filter_conditions,
        build_chart_filter_condition(
          column_name =
            settings$categories,
          selected_values =
            settings$category_values,
          year_prefix =
            year_prefix
        )
      )
    }
    
    if (length(filter_conditions) > 0) {
      
      data_block <- c(
        paste0(
          "    const ",
          treemap_data_variable,
          " = ",
          data_variable
        ),
        paste0(
          "        .filter(row => ",
          paste(
            filter_conditions,
            collapse = " &&\n                       "
          ),
          ");"
        )
      )
      
    } else {
      
      data_block <- paste0(
        "    const ",
        treemap_data_variable,
        " = ",
        data_variable,
        ";"
      )
    }
    
    chart_categories <-
      settings$categories
    
    chart_value <-
      settings$value
    
  } else {
    
    #
    # MODE 2:
    # Categories come from the pivoted variable.
    #
    # The filters must identify one source row.
    #
    
    if (
      is.null(settings$pivot_label) ||
      !nzchar(settings$pivot_label)
    ) {
      stop(
        "A pivot-category treemap requires a pivot label."
      )
    }
    
    if (
      is.null(settings$category_values) ||
      length(settings$category_values) == 0
    ) {
      stop(
        "Choose at least one pivot category for the treemap."
      )
    }
    
    #
    # First build the filtered source-row expression.
    #
    source_row_variable <- paste0(
      "treemap_chart_",
      chart_number,
      "_source"
    )
    
    if (length(filter_conditions) > 0) {
      
      source_block <- c(
        paste0(
          "    const ",
          source_row_variable,
          " = ",
          data_variable
        ),
        paste0(
          "        .filter(row => ",
          paste(
            filter_conditions,
            collapse = " &&\n                       "
          ),
          ")[0];"
        ),
        ""
      )
      
    } else {
      
      source_block <- c(
        paste0(
          "    const ",
          source_row_variable,
          " = ",
          data_variable,
          "[0];"
        ),
        ""
      )
    }
    
    #
    # Reshape the selected pivot columns into objects
    # that treemapChart() can consume.
    #
    mapped_rows <- vapply(
      settings$category_values,
      function(category_value) {
        
        paste0(
          "        { ",
          javascript_string(
            settings$pivot_label
          ),
          ": ",
          javascript_string(
            category_value
          ),
          ", value: ",
          source_row_variable,
          "[",
          javascript_string(
            category_value
          ),
          "] }"
        )
      },
      character(1)
    )
    
    data_block <- c(
      source_block,
      paste0(
        "    const ",
        treemap_data_variable,
        " = ["
      ),
      paste0(
        mapped_rows,
        collapse = ",\n"
      ),
      "    ];"
    )
    
    chart_categories <-
      settings$pivot_label
    
    chart_value <-
      "value"
  }
  
  #
  # treemapChart() call.
  #
  chart_call <- c(
    "",
    "    treemapChart({",
    paste0(
      "        data: ",
      treemap_data_variable,
      ","
    ),
    paste0(
      "        categories: ",
      javascript_string(
        chart_categories
      ),
      ","
    ),
    paste0(
      "        value: ",
      javascript_string(
        chart_value
      ),
      ","
    ),
    paste0(
      '        canvas_id: "treemap-canvas-',
      chart_number,
      '",'
    ),
    paste0(
      '        expanded_canvas_id: "treemap-canvas-',
      chart_number,
      '-expanded"'
    ),
    "    });"
  )
  
  #
  # Download query.
  #
  query_entries <- character()
  
  if (
    !is.null(settings$filters) &&
    length(settings$filters) > 0
  ) {
    
    for (
      column_name in
      names(settings$filters)
    ) {
      
      values <- settings$filters[[
        column_name
      ]]
      
      query_value <- if (
        length(values) == 1
      ) {
        
        javascript_query_value(
          values[[1]],
          year_prefix = year_prefix
        )
        
      } else {
        
        paste0(
          "[",
          paste(
            vapply(
              values,
              function(value) {
                javascript_query_value(
                  value,
                  year_prefix = year_prefix
                )
              },
              character(1)
            ),
            collapse = ", "
          ),
          "]"
        )
      }
      
      query_entries <- c(
        query_entries,
        paste0(
          "        ",
          javascript_string(
            column_name
          ),
          ": ",
          query_value
        )
      )
    }
  }
  
  #
  # Row-category mode:
  # selected category values belong to the row variable.
  #
  if (
    identical(
      category_source,
      "row"
    ) &&
    !is.null(settings$category_values) &&
    length(settings$category_values) > 0
  ) {
    
    category_query_value <- if (
      length(settings$category_values) == 1
    ) {
      
      javascript_string(
        settings$category_values[[1]]
      )
      
    } else {
      
      paste0(
        "[",
        paste(
          vapply(
            settings$category_values,
            javascript_string,
            character(1)
          ),
          collapse = ", "
        ),
        "]"
      )
    }
    
    query_entries <- c(
      query_entries,
      paste0(
        "        ",
        javascript_string(
          settings$categories
        ),
        ": ",
        category_query_value
      )
    )
  }
  
  #
  # Pivot-category mode:
  # selected category values belong to the pivot variable.
  #
  if (
    identical(
      category_source,
      "pivot"
    ) &&
    !is.null(settings$pivot_label) &&
    nzchar(settings$pivot_label)
  ) {
    
    pivot_query_value <- if (
      length(settings$category_values) == 1
    ) {
      
      javascript_string(
        settings$category_values[[1]]
      )
      
    } else {
      
      paste0(
        "[",
        paste(
          vapply(
            settings$category_values,
            javascript_string,
            character(1)
          ),
          collapse = ", "
        ),
        "]"
      )
    }
    
    query_entries <- c(
      query_entries,
      paste0(
        "        ",
        javascript_string(
          settings$pivot_label
        ),
        ": ",
        pivot_query_value
      )
    )
  }
  
  #
  # Row-category mode also has a separate selected
  # value column from the pivoted variable.
  #
  if (
    identical(
      category_source,
      "row"
    ) &&
    !is.null(settings$pivot_label) &&
    nzchar(settings$pivot_label) &&
    !is.null(settings$value) &&
    nzchar(settings$value)
  ) {
    
    query_entries <- c(
      query_entries,
      paste0(
        "        ",
        javascript_string(
          settings$pivot_label
        ),
        ": ",
        javascript_string(
          settings$value
        )
      )
    )
  }
  
  query_block <- if (
    length(query_entries) == 0
  ) {
    
    paste0(
      "    const ",
      query_variable,
      " = {};"
    )
    
  } else {
    
    c(
      paste0(
        "    const ",
        query_variable,
        " = {"
      ),
      paste0(
        query_entries,
        collapse = ",\n"
      ),
      "    };"
    )
  }
  
  download_call <- c(
    "",
    "    downloadButton(",
    paste0(
      '        "chart-',
      chart_number,
      '-capture",'
    ),
    paste0(
      "        ",
      javascript_string(
        matrix
      ),
      ","
    ),
    paste0(
      "        dateFormat(",
      matrix,
      "_meta.updated),"
    ),
    paste0(
      "        ",
      query_variable
    ),
    "    );"
  )
  
  c(
    data_block,
    chart_call,
    "",
    query_block,
    download_call
  )
}

update_page_treemap_chart_js <- function(
    project_root,
    page_href,
    chart_number,
    matrix,
    treemap_chart_js,
    use_matrix_years = FALSE
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  js_lines <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  region_start <- grep(
    "^\\s*//\\s*Insert chart content below\\s*$",
    js_lines
  )
  
  region_end <- grep(
    "^\\s*//\\s*End chart content\\s*$",
    js_lines
  )
  
  if (
    length(region_start) != 1 ||
    length(region_end) != 1 ||
    region_end <= region_start
  ) {
    stop(
      "Could not uniquely identify the chart JavaScript region."
    )
  }
  
  chart_marker <- grep(
    paste0(
      "^\\s*//\\s*Content for chart\\s+",
      chart_number,
      "\\s*$"
    ),
    js_lines
  )
  
  chart_marker <- chart_marker[
    chart_marker > region_start &
      chart_marker < region_end
  ]
  
  if (length(chart_marker) != 1) {
    stop(
      paste0(
        "Could not identify the JavaScript section for chart ",
        chart_number,
        "."
      )
    )
  }
  
  needs_update_year_spans <- !page_js_has_update_year_spans_before(
    js_lines = js_lines,
    line_number = chart_marker
  )
  
  year_update_lines <- if (
    isTRUE(needs_update_year_spans)
  ) {
    c(
      paste0(
        "    updateYearSpans(",
        matrix,
        "_data, ",
        matrix,
        "_meta);"
      ),
      ""
    )
  } else {
    character()
  }
  
  matrix_year_lines <- if (
    isTRUE(use_matrix_years) &&
    !isTRUE(needs_update_year_spans)
  ) {
    build_matrix_year_variables_js(
      matrix = matrix
    )
  } else {
    character()
  }
  
  matrix_year_span_lines <- if (
    isTRUE(use_matrix_years) &&
    !isTRUE(needs_update_year_spans)
  ) {
    build_matrix_year_span_js(
      matrix = matrix
    )
  } else {
    character()
  }
  
  all_chart_markers <- grep(
    "^\\s*//\\s*Content for chart\\s+[0-9]+\\s*$",
    js_lines
  )
  
  all_chart_markers <- all_chart_markers[
    all_chart_markers > region_start &
      all_chart_markers < region_end
  ]
  
  later_markers <- all_chart_markers[
    all_chart_markers > chart_marker
  ]
  
  section_end <- if (
    length(later_markers) > 0
  ) {
    min(later_markers) - 1
  } else {
    region_end - 1
  }
  
  section <- js_lines[
    chart_marker:section_end
  ]
  
  #
  # Remove any previous managed chart block,
  # regardless of its chart type.
  #
  section <- remove_page_chart_config_blocks(
    section
  )
  
  while (
    length(section) > 0 &&
    !nzchar(trimws(section[[length(section)]]))
  ) {
    section <- section[-length(section)]
  }
  
  replacement_section <- c(
    section,
    "",
    "    // BuildR treemap chart config start",
    year_update_lines,
    matrix_year_lines,
    matrix_year_span_lines,
    treemap_chart_js,
    "    // BuildR treemap chart config end",
    ""
  )
  
  updated_js <- c(
    if (chart_marker > 1) {
      js_lines[
        seq_len(
          chart_marker - 1
        )
      ]
    } else {
      character()
    },
    
    replacement_section,
    
    if (section_end < length(js_lines)) {
      js_lines[
        seq.int(
          section_end + 1,
          length(js_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  writeLines(
    updated_js,
    paths$js,
    useBytes = TRUE
  )
  
  invisible(
    paths$js
  )
}

build_page_pyramid_chart_js <- function(
    chart_number,
    matrix,
    settings,
    year_prefix = NULL
) {
  
  data_variable <- paste0(
    matrix,
    "_data"
  )
  
  pyramid_data_variable <- paste0(
    "pyramid_chart_",
    chart_number,
    "_data"
  )
  
  query_variable <- paste0(
    "pyramid_chart_",
    chart_number,
    "_query"
  )
  
  filter_conditions <- character()
  
  #
  # Additional filters
  #
  if (
    !is.null(settings$filters) &&
    length(settings$filters) > 0
  ) {
    
    for (
      column_name in
      names(settings$filters)
    ) {
      
      filter_conditions <- c(
        filter_conditions,
        build_chart_filter_condition(
          column_name = column_name,
          selected_values = settings$filters[[column_name]],
          year_prefix = year_prefix
        )
      )
    }
  }
  
  #
  # Optional category restrictions.
  #
  if (
    !is.null(settings$category_values) &&
    length(settings$category_values) > 0
  ) {
    
    filter_conditions <- c(
      filter_conditions,
      build_chart_filter_condition(
        column_name = settings$categories,
        selected_values = settings$category_values,
        year_prefix = year_prefix
      )
    )
  }
  
  #
  # Do NOT filter Year here.
  #
  # pyramidChart() already filters the supplied data
  # using the year argument.
  #
  if (length(filter_conditions) > 0) {
    
    data_block <- c(
      paste0(
        "    const ",
        pyramid_data_variable,
        " = ",
        data_variable
      ),
      paste0(
        "        .filter(row => ",
        paste(
          filter_conditions,
          collapse = " &&\n                       "
        ),
        ");"
      )
    )
    
  } else {
    
    data_block <- paste0(
      "    const ",
      pyramid_data_variable,
      " = ",
      data_variable,
      ";"
    )
  }
  
  #
  # Convert selected year sentinel into JavaScript.
  #
  year_js <- javascript_dynamic_year_value(
    value = settings$year,
    year_prefix = year_prefix
  )
  
  if (is.null(year_js)) {
    year_js <- javascript_query_value(
      settings$year,
      year_prefix = year_prefix
    )
  }
  
  values_js <- paste0(
    "[",
    paste(
      vapply(
        settings$values,
        javascript_string,
        character(1)
      ),
      collapse = ", "
    ),
    "]"
  )
  
  chart_call <- c(
    "",
    "    pyramidChart({",
    paste0(
      "        data: ",
      pyramid_data_variable,
      ","
    ),
    paste0(
      "        meta: ",
      matrix,
      "_meta,"
    ),
    paste0(
      "        categories: ",
      javascript_string(
        settings$categories
      ),
      ","
    ),
    paste0(
      "        values: ",
      values_js,
      ","
    ),
    paste0(
      '        canvas_id: "pyramid-canvas-',
      chart_number,
      '",'
    ),
    paste0(
      '        expanded_canvas_id: "pyramid-canvas-',
      chart_number,
      '-expanded",'
    ),
    paste0(
      "        year: ",
      year_js
    ),
    "    });"
  )
  
  #
  # Download query
  #
  query_entries <- character()
  
  #
  # Year is always included because it is required
  # by pyramidChart().
  #
  query_entries <- c(
    query_entries,
    paste0(
      "        ",
      javascript_string(
        settings$year_column
      ),
      ": ",
      year_js
    )
  )
  
  #
  # Additional filters.
  #
  if (
    !is.null(settings$filters) &&
    length(settings$filters) > 0
  ) {
    
    for (
      column_name in
      names(settings$filters)
    ) {
      
      values <- settings$filters[[
        column_name
      ]]
      
      query_value <- if (
        length(values) == 1
      ) {
        
        javascript_query_value(
          values[[1]],
          year_prefix = year_prefix
        )
        
      } else {
        
        paste0(
          "[",
          paste(
            vapply(
              values,
              function(value) {
                javascript_query_value(
                  value,
                  year_prefix = year_prefix
                )
              },
              character(1)
            ),
            collapse = ", "
          ),
          "]"
        )
      }
      
      query_entries <- c(
        query_entries,
        paste0(
          "        ",
          javascript_string(
            column_name
          ),
          ": ",
          query_value
        )
      )
    }
  }
  
  #
  # Selected category values.
  #
  if (
    !is.null(settings$category_values) &&
    length(settings$category_values) > 0
  ) {
    
    category_query_value <- if (
      length(settings$category_values) == 1
    ) {
      
      javascript_string(
        settings$category_values[[1]]
      )
      
    } else {
      
      paste0(
        "[",
        paste(
          vapply(
            settings$category_values,
            javascript_string,
            character(1)
          ),
          collapse = ", "
        ),
        "]"
      )
    }
    
    query_entries <- c(
      query_entries,
      paste0(
        "        ",
        javascript_string(
          settings$categories
        ),
        ": ",
        category_query_value
      )
    )
  }
  
  #
  # The two value columns represent values of the
  # pivoted metadata variable, e.g. Sex.
  #
  if (
    !is.null(settings$pivot_label) &&
    nzchar(settings$pivot_label)
  ) {
    
    query_entries <- c(
      query_entries,
      paste0(
        "        ",
        javascript_string(
          settings$pivot_label
        ),
        ": ",
        values_js
      )
    )
  }
  
  query_block <- c(
    paste0(
      "    const ",
      query_variable,
      " = {"
    ),
    paste0(
      query_entries,
      collapse = ",\n"
    ),
    "    };"
  )
  
  download_call <- c(
    "",
    "    downloadButton(",
    paste0(
      '        "chart-',
      chart_number,
      '-capture",'
    ),
    paste0(
      "        ",
      javascript_string(matrix),
      ","
    ),
    paste0(
      "        dateFormat(",
      matrix,
      "_meta.updated),"
    ),
    paste0(
      "        ",
      query_variable
    ),
    "    );"
  )
  
  c(
    data_block,
    chart_call,
    "",
    query_block,
    download_call
  )
}

update_page_pyramid_chart_js <- function(
    project_root,
    page_href,
    chart_number,
    matrix,
    pyramid_chart_js,
    use_matrix_years = FALSE
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  js_lines <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  region_start <- grep(
    "^\\s*//\\s*Insert chart content below\\s*$",
    js_lines
  )
  
  region_end <- grep(
    "^\\s*//\\s*End chart content\\s*$",
    js_lines
  )
  
  if (
    length(region_start) != 1 ||
    length(region_end) != 1 ||
    region_end <= region_start
  ) {
    stop(
      "Could not uniquely identify the chart JavaScript region."
    )
  }
  
  chart_marker <- grep(
    paste0(
      "^\\s*//\\s*Content for chart\\s+",
      chart_number,
      "\\s*$"
    ),
    js_lines
  )
  
  chart_marker <- chart_marker[
    chart_marker > region_start &
      chart_marker < region_end
  ]
  
  if (length(chart_marker) != 1) {
    stop(
      paste0(
        "Could not identify the JavaScript section for chart ",
        chart_number,
        "."
      )
    )
  }
  
  needs_update_year_spans <- !page_js_has_update_year_spans_before(
    js_lines = js_lines,
    line_number = chart_marker
  )
  
  year_update_lines <- if (
    isTRUE(needs_update_year_spans)
  ) {
    c(
      paste0(
        "    updateYearSpans(",
        matrix,
        "_data, ",
        matrix,
        "_meta);"
      ),
      ""
    )
  } else {
    character()
  }
  
  matrix_year_lines <- if (
    isTRUE(use_matrix_years) &&
    !isTRUE(needs_update_year_spans)
  ) {
    build_matrix_year_variables_js(
      matrix = matrix
    )
  } else {
    character()
  }
  
  matrix_year_span_lines <- if (
    isTRUE(use_matrix_years) &&
    !isTRUE(needs_update_year_spans)
  ) {
    build_matrix_year_span_js(
      matrix = matrix
    )
  } else {
    character()
  }
  
  all_chart_markers <- grep(
    "^\\s*//\\s*Content for chart\\s+[0-9]+\\s*$",
    js_lines
  )
  
  all_chart_markers <- all_chart_markers[
    all_chart_markers > region_start &
      all_chart_markers < region_end
  ]
  
  later_markers <- all_chart_markers[
    all_chart_markers > chart_marker
  ]
  
  section_end <- if (
    length(later_markers) > 0
  ) {
    min(later_markers) - 1
  } else {
    region_end - 1
  }
  
  section <- js_lines[
    chart_marker:section_end
  ]
  
  #
  # Remove whichever chart configuration was
  # previously stored in this chart section.
  #
  section <- remove_page_chart_config_blocks(
    section
  )
  
  while (
    length(section) > 0 &&
    !nzchar(trimws(section[[length(section)]]))
  ) {
    section <- section[-length(section)]
  }
  
  replacement_section <- c(
    section,
    "",
    "    // BuildR pyramid chart config start",
    year_update_lines,
    matrix_year_lines,
    matrix_year_span_lines,
    pyramid_chart_js,
    "    // BuildR pyramid chart config end",
    ""
  )
  
  updated_js <- c(
    if (chart_marker > 1) {
      js_lines[
        seq_len(
          chart_marker - 1
        )
      ]
    } else {
      character()
    },
    
    replacement_section,
    
    if (section_end < length(js_lines)) {
      js_lines[
        seq.int(
          section_end + 1,
          length(js_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  writeLines(
    updated_js,
    paths$js,
    useBytes = TRUE
  )
  
  invisible(
    paths$js
  )
}

build_page_table_js <- function(
    chart_number,
    matrix,
    settings,
    year_prefix = NULL
) {
  
  data_variable <- paste0(
    matrix,
    "_data"
  )
  
  source_variable <- paste0(
    "table_",
    chart_number,
    "_data_source"
  )
  
  table_variable <- paste0(
    "table_",
    chart_number,
    "_data"
  )
  
  query_variable <- paste0(
    "table_",
    chart_number,
    "_query"
  )
  
  filter_conditions <- character()
  
  #
  # Build row filters.
  #
  if (
    !is.null(settings$filters) &&
    length(settings$filters) > 0
  ) {
    
    for (
      column_name in
      names(settings$filters)
    ) {
      
      filter_conditions <- c(
        filter_conditions,
        build_chart_filter_condition(
          column_name = column_name,
          selected_values = settings$filters[[column_name]],
          year_prefix = year_prefix
        )
      )
    }
  }
  
  #
  # Build filtered source data.
  #
  data_block <- if (
    length(filter_conditions) > 0
  ) {
    
    c(
      paste0(
        "    const ",
        source_variable,
        " = ",
        data_variable
      ),
      paste0(
        "        .filter(row => ",
        paste(
          filter_conditions,
          collapse = " &&\n                       "
        ),
        ");"
      )
    )
    
  } else {
    
    paste0(
      "    const ",
      source_variable,
      " = ",
      data_variable,
      ";"
    )
  }
  
  #
  # Build table_data object.
  #
  table_entries <- lapply(
    settings$columns,
    function(column_settings) {
      
      c(
        paste0(
          "        ",
          javascript_string(
            column_settings$heading
          ),
          ": {"
        ),
        paste0(
          "            values: ",
          source_variable
        ),
        paste0(
          "                .map(col => col[",
          javascript_string(
            column_settings$source
          ),
          "]),"
        ),
        paste0(
          "            format: ",
          javascript_string(
            column_settings$format
          )
        ),
        "        }"
      )
    }
  )
  
  table_object_lines <- character()
  
  for (
    column_number in
    seq_along(table_entries)
  ) {
    
    entry <- table_entries[[
      column_number
    ]]
    
    if (
      column_number <
      length(table_entries)
    ) {
      entry[
        length(entry)
      ] <- paste0(
        entry[
          length(entry)
        ],
        ","
      )
    }
    
    table_object_lines <- c(
      table_object_lines,
      entry,
      if (
        column_number <
        length(table_entries)
      ) {
        ""
      } else {
        character()
      }
    )
  }
  
  table_block <- c(
    "",
    paste0(
      "    const ",
      table_variable,
      " = {"
    ),
    table_object_lines,
    "    };"
  )
  
  #
  # Insert table into standard + expanded targets.
  #
  table_call <- c(
    "",
    "    insertTable(",
    paste0(
      '        "table-',
      chart_number,
      '",'
    ),
    paste0(
      '        "table-',
      chart_number,
      '-expanded",'
    ),
    paste0(
      "        ",
      table_variable
    ),
    "    );"
  )
  
  #
  # Build download query from filters.
  #
  query_entries <- character()
  
  if (
    !is.null(settings$filters) &&
    length(settings$filters) > 0
  ) {
    
    for (
      column_name in
      names(settings$filters)
    ) {
      
      values <- settings$filters[[
        column_name
      ]]
      
      query_value <- if (
        length(values) == 1
      ) {
        
        javascript_query_value(
          values[[1]],
          year_prefix = year_prefix
        )
        
      } else {
        
        paste0(
          "[",
          paste(
            vapply(
              values,
              function(value) {
                javascript_query_value(
                  value,
                  year_prefix = year_prefix
                )
              },
              character(1)
            ),
            collapse = ", "
          ),
          "]"
        )
      }
      
      query_entries <- c(
        query_entries,
        paste0(
          "        ",
          javascript_string(
            column_name
          ),
          ": ",
          query_value
        )
      )
    }
  }
  
  #
  # If displayed columns come from pivoted value columns,
  # add those values to the source query.
  #
  if (
    !is.null(settings$pivot_label) &&
    nzchar(settings$pivot_label)
  ) {
    
    displayed_sources <- unique(
      vapply(
        settings$columns,
        function(column_settings) {
          column_settings$source
        },
        character(1)
      )
    )
    
    #
    # Only sources matching actual pivot columns should
    # be added under pivot_label.
    #
    pivot_sources <- intersect(
      displayed_sources,
      settings$pivot_columns %||%
        character()
    )
    
    if (length(pivot_sources) > 0) {
      
      pivot_query_value <- if (
        length(pivot_sources) == 1
      ) {
        javascript_string(
          pivot_sources[[1]]
        )
      } else {
        paste0(
          "[",
          paste(
            vapply(
              pivot_sources,
              javascript_string,
              character(1)
            ),
            collapse = ", "
          ),
          "]"
        )
      }
      
      query_entries <- c(
        query_entries,
        paste0(
          "        ",
          javascript_string(
            settings$pivot_label
          ),
          ": ",
          pivot_query_value
        )
      )
    }
  }
  
  query_block <- if (
    length(query_entries) == 0
  ) {
    
    paste0(
      "    const ",
      query_variable,
      " = {};"
    )
    
  } else {
    
    c(
      paste0(
        "    const ",
        query_variable,
        " = {"
      ),
      paste0(
        query_entries,
        collapse = ",\n"
      ),
      "    };"
    )
  }
  
  download_call <- c(
    "",
    "    downloadButton(",
    paste0(
      '        "chart-',
      chart_number,
      '-capture",'
    ),
    paste0(
      "        ",
      javascript_string(matrix),
      ","
    ),
    paste0(
      "        dateFormat(",
      matrix,
      "_meta.updated),"
    ),
    paste0(
      "        ",
      query_variable
    ),
    "    );"
  )
  
  c(
    data_block,
    table_block,
    table_call,
    "",
    query_block,
    download_call
  )
}

update_page_table_html <- function(
    project_root,
    page_href,
    chart_number
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  html_lines <- readLines(
    paths$html,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  chart_region <- find_page_chart_cards_region(
    html_lines
  )
  
  chart_start <- chart_region$chart_starts[
    chart_number
  ]
  
  chart_end <- find_closing_div(
    html_lines,
    chart_start
  )
  
  body_starts <- grep(
    'class=["\'][^"\']*\\bcard-body\\b',
    html_lines,
    perl = TRUE
  )
  
  body_starts <- body_starts[
    body_starts > chart_start &
      body_starts < chart_end
  ]
  
  if (length(body_starts) != 1) {
    stop(
      paste0(
        "Could not uniquely identify the card body for chart ",
        chart_number,
        "."
      )
    )
  }
  
  body_start <- body_starts[[1]]
  
  body_end <- find_closing_div(
    html_lines,
    body_start
  )
  
  indent <- sub(
    "^(\\s*).*",
    "\\1",
    html_lines[
      body_start
    ]
  )
  
  opening_tag <- sub(
    "^(\\s*<div\\b[^>]*>).*",
    "\\1",
    html_lines[
      body_start
    ],
    perl = TRUE
  )
  
  replacement <- c(
    opening_tag,
    
    paste0(
      indent,
      "    ",
      '<table id="table-',
      chart_number,
      '" class="table">'
    ),
    
    paste0(
      indent,
      "        <thead></thead>"
    ),
    
    paste0(
      indent,
      "        <tbody></tbody>"
    ),
    
    paste0(
      indent,
      "    </table>"
    ),
    
    paste0(
      indent,
      "</div>"
    )
  )
  
  updated_html <- c(
    if (body_start > 1) {
      html_lines[
        seq_len(
          body_start - 1
        )
      ]
    } else {
      character()
    },
    
    replacement,
    
    if (body_end < length(html_lines)) {
      html_lines[
        seq.int(
          body_end + 1,
          length(html_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  writeLines(
    updated_html,
    paths$html,
    useBytes = TRUE
  )
  
  invisible(
    paths$html
  )
}

update_page_table_js <- function(
    project_root,
    page_href,
    chart_number,
    matrix,
    table_js,
    use_matrix_years = FALSE
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  js_lines <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  region_start <- grep(
    "^\\s*//\\s*Insert chart content below\\s*$",
    js_lines
  )
  
  region_end <- grep(
    "^\\s*//\\s*End chart content\\s*$",
    js_lines
  )
  
  if (
    length(region_start) != 1 ||
    length(region_end) != 1 ||
    region_end <= region_start
  ) {
    stop(
      "Could not uniquely identify the chart JavaScript region."
    )
  }
  
  chart_marker <- grep(
    paste0(
      "^\\s*//\\s*Content for chart\\s+",
      chart_number,
      "\\s*$"
    ),
    js_lines
  )
  
  chart_marker <- chart_marker[
    chart_marker > region_start &
      chart_marker < region_end
  ]
  
  if (length(chart_marker) != 1) {
    stop(
      paste0(
        "Could not identify the JavaScript section for chart ",
        chart_number,
        "."
      )
    )
  }
  
  needs_update_year_spans <- !page_js_has_update_year_spans_before(
    js_lines = js_lines,
    line_number = chart_marker
  )
  
  year_update_lines <- if (
    isTRUE(needs_update_year_spans)
  ) {
    c(
      paste0(
        "    updateYearSpans(",
        matrix,
        "_data, ",
        matrix,
        "_meta);"
      ),
      ""
    )
  } else {
    character()
  }
  
  matrix_year_lines <- if (
    isTRUE(use_matrix_years) &&
    !isTRUE(needs_update_year_spans)
  ) {
    build_matrix_year_variables_js(
      matrix = matrix
    )
  } else {
    character()
  }
  
  matrix_year_span_lines <- if (
    isTRUE(use_matrix_years) &&
    !isTRUE(needs_update_year_spans)
  ) {
    build_matrix_year_span_js(
      matrix = matrix
    )
  } else {
    character()
  }
  
  all_chart_markers <- grep(
    "^\\s*//\\s*Content for chart\\s+[0-9]+\\s*$",
    js_lines
  )
  
  all_chart_markers <- all_chart_markers[
    all_chart_markers > region_start &
      all_chart_markers < region_end
  ]
  
  later_markers <- all_chart_markers[
    all_chart_markers > chart_marker
  ]
  
  section_end <- if (
    length(later_markers) > 0
  ) {
    min(later_markers) - 1
  } else {
    region_end - 1
  }
  
  section <- js_lines[
    chart_marker:section_end
  ]
  
  section <- remove_page_chart_config_blocks(
    section
  )
  
  while (
    length(section) > 0 &&
    !nzchar(trimws(section[[length(section)]]))
  ) {
    section <- section[-length(section)]
  }
  
  replacement_section <- c(
    section,
    "",
    "    // BuildR table chart config start",
    year_update_lines,
    matrix_year_lines,
    matrix_year_span_lines,
    table_js,
    "    // BuildR table chart config end",
    ""
  )
  
  updated_js <- c(
    if (chart_marker > 1) {
      js_lines[
        seq_len(
          chart_marker - 1
        )
      ]
    } else {
      character()
    },
    
    replacement_section,
    
    if (section_end < length(js_lines)) {
      js_lines[
        seq.int(
          section_end + 1,
          length(js_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  writeLines(
    updated_js,
    paths$js,
    useBytes = TRUE
  )
  
  invisible(
    paths$js
  )
}

page_chart_type_choices_for <- function(
    chart_number,
    current_types,
    number_of_charts
) {
  
  choices <- c(
    "Bar chart" = "bar",
    "Table" = "table",
    "Line chart" = "line",
    "Pie chart" = "pie",
    "Map" = "map",
    "Population pyramid" = "pyramid",
    "Treemap" = "treemap"
  )
  
  map_charts <- which(
    current_types == "map"
  )
  
  #
  # Only one map can appear on a page.
  #
  if (
    length(map_charts) > 0 &&
    !chart_number %in% map_charts
  ) {
    choices <- choices[
      choices != "map"
    ]
  }
  
  #
  # Maps use a wider layout and are not supported
  # when three chart cards are present.
  #
  if (
    number_of_charts >= 3 &&
    !identical(
      current_types[[chart_number]],
      "map"
    )
  ) {
    choices <- choices[
      choices != "map"
    ]
  }
  
  choices
}

build_page_map_chart_js <- function(
    chart_number,
    matrix,
    settings,
    year_prefix = NULL
) {
  
  data_variable <- paste0(
    matrix,
    "_data"
  )
  
  map_data_variable <- paste0(
    "map_chart_",
    chart_number,
    "_data"
  )
  
  query_variable <- paste0(
    "map_chart_",
    chart_number,
    "_query"
  )
  
  filter_conditions <- character()
  
  #
  # Additional filters.
  #
  if (
    !is.null(settings$filters) &&
    length(settings$filters) > 0
  ) {
    
    for (
      column_name in
      names(settings$filters)
    ) {
      
      filter_conditions <- c(
        filter_conditions,
        build_chart_filter_condition(
          column_name = column_name,
          selected_values = settings$filters[[column_name]],
          year_prefix = year_prefix
        )
      )
    }
  }
  
  #
  # Exclude the Northern Ireland summary row.
  #
  filter_conditions <- c(
    filter_conditions,
    paste0(
      'row[',
      javascript_string(settings$area),
      '] != "Northern Ireland"'
    )
  )
  
  data_block <- if (
    length(filter_conditions) > 0
  ) {
    
    c(
      paste0(
        "    const ",
        map_data_variable,
        " = ",
        data_variable
      ),
      paste0(
        "        .filter(row => ",
        paste(
          filter_conditions,
          collapse = " &&\n                       "
        ),
        ");"
      )
    )
    
  } else {
    
    paste0(
      "    const ",
      map_data_variable,
      " = ",
      data_variable,
      ";"
    )
  }
  
  #
  # plotMap() call.
  #
  map_call <- c(
    "",
    "    plotMap({",
    paste0(
      '        elementId: "map-container-',
      chart_number,
      '",'
    ),
    paste0(
      '        legendId: "map-legend-',
      chart_number,
      '",'
    ),
    paste0(
      "        data: ",
      map_data_variable,
      ","
    ),
    paste0(
      "        meta: ",
      matrix,
      "_meta,"
    ),
    paste0(
      "        area: ",
      javascript_string(
        settings$area
      ),
      ","
    ),
    paste0(
      "        value: ",
      javascript_string(
        settings$value
      )
    ),
    "    });"
  )
  
  #
  # Download query.
  #
  query_entries <- character()
  
  if (
    !is.null(settings$filters) &&
    length(settings$filters) > 0
  ) {
    
    for (
      column_name in
      names(settings$filters)
    ) {
      
      values <- settings$filters[[
        column_name
      ]]
      
      query_value <- if (
        length(values) == 1
      ) {
        javascript_query_value(
          values[[1]],
          year_prefix = year_prefix
        )
      } else {
        paste0(
          "[",
          paste(
            vapply(
              values,
              function(value) {
                javascript_query_value(
                  value,
                  year_prefix = year_prefix
                )
              },
              character(1)
            ),
            collapse = ", "
          ),
          "]"
        )
      }
      
      query_entries <- c(
        query_entries,
        paste0(
          "        ",
          javascript_string(
            column_name
          ),
          ": ",
          query_value
        )
      )
    }
  }
  
  #
  # Selected value represents one value of the
  # pivoted metadata variable.
  #
  if (
    !is.null(settings$pivot_label) &&
    nzchar(settings$pivot_label)
  ) {
    
    query_entries <- c(
      query_entries,
      paste0(
        "        ",
        javascript_string(
          settings$pivot_label
        ),
        ": ",
        javascript_string(
          settings$value
        )
      )
    )
  }
  
  query_block <- if (
    length(query_entries) == 0
  ) {
    
    paste0(
      "    const ",
      query_variable,
      " = {};"
    )
    
  } else {
    
    c(
      paste0(
        "    const ",
        query_variable,
        " = {"
      ),
      paste0(
        query_entries,
        collapse = ",\n"
      ),
      "    };"
    )
  }
  
  download_call <- c(
    "",
    "    downloadButton(",
    paste0(
      '        "chart-',
      chart_number,
      '-capture",'
    ),
    paste0(
      "        ",
      javascript_string(matrix),
      ","
    ),
    paste0(
      "        dateFormat(",
      matrix,
      "_meta.updated),"
    ),
    paste0(
      "        ",
      query_variable,
      ","
    ),
    '        "map"',
    "    );"
  )
  
  c(
    data_block,
    map_call,
    "",
    query_block,
    download_call
  )
}

update_page_map_html <- function(
    project_root,
    page_href,
    chart_number
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  html_lines <- readLines(
    paths$html,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  chart_region <- find_page_chart_cards_region(
    html_lines
  )
  
  chart_start <- chart_region$chart_starts[
    chart_number
  ]
  
  chart_end <- find_closing_div(
    html_lines,
    chart_start
  )
  
  body_starts <- grep(
    'class=["\'][^"\']*\\bcard-body\\b',
    html_lines,
    perl = TRUE
  )
  
  body_starts <- body_starts[
    body_starts > chart_start &
      body_starts < chart_end
  ]
  
  if (length(body_starts) != 1) {
    stop(
      paste0(
        "Could not uniquely identify the card body for chart ",
        chart_number,
        "."
      )
    )
  }
  
  body_start <- body_starts[[1]]
  
  body_end <- find_closing_div(
    html_lines,
    body_start
  )
  
  indent <- sub(
    "^(\\s*).*",
    "\\1",
    html_lines[
      body_start
    ]
  )
  
  opening_tag <- paste0(
    indent,
    '<div class="card-body">'
  )
  
  replacement <- c(
    opening_tag,
    
    paste0(
      indent,
      "    ",
      '<div id="map-legend-',
      chart_number,
      '" class="mt-3"></div>'
    ),
    
    "",
    
    paste0(
      indent,
      "    ",
      '<small class="text-muted d-block mt-2">'
    ),
    
    paste0(
      indent,
      "        ",
      "Hover over individual areas on the map to see more details."
    ),
    
    paste0(
      indent,
      "    ",
      "</small>"
    ),
    
    "",
    
    paste0(
      indent,
      "    ",
      '<div id="map-container-',
      chart_number,
      '" class="map"></div>'
    ),
    
    paste0(
      indent,
      "</div>"
    )
  )
  
  updated_html <- c(
    if (body_start > 1) {
      html_lines[
        seq_len(
          body_start - 1
        )
      ]
    } else {
      character()
    },
    
    replacement,
    
    if (body_end < length(html_lines)) {
      html_lines[
        seq.int(
          body_end + 1,
          length(html_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  writeLines(
    updated_html,
    paths$html,
    useBytes = TRUE
  )
  
  invisible(
    paths$html
  )
}

update_page_map_chart_js <- function(
    project_root,
    page_href,
    chart_number,
    matrix,
    map_chart_js,
    use_matrix_years = FALSE
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  js_lines <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  region_start <- grep(
    "^\\s*//\\s*Insert chart content below\\s*$",
    js_lines
  )
  
  region_end <- grep(
    "^\\s*//\\s*End chart content\\s*$",
    js_lines
  )
  
  if (
    length(region_start) != 1 ||
    length(region_end) != 1 ||
    region_end <= region_start
  ) {
    stop(
      "Could not uniquely identify the chart JavaScript region."
    )
  }
  
  chart_marker <- grep(
    paste0(
      "^\\s*//\\s*Content for chart\\s+",
      chart_number,
      "\\s*$"
    ),
    js_lines
  )
  
  chart_marker <- chart_marker[
    chart_marker > region_start &
      chart_marker < region_end
  ]
  
  if (length(chart_marker) != 1) {
    stop(
      paste0(
        "Could not identify the JavaScript section for chart ",
        chart_number,
        "."
      )
    )
  }
  
  #
  # If no earlier updateYearSpans() call exists, this
  # chart becomes the owner of the global year values.
  #
  needs_update_year_spans <- !page_js_has_update_year_spans_before(
    js_lines = js_lines,
    line_number = chart_marker
  )
  
  year_update_lines <- if (
    isTRUE(needs_update_year_spans)
  ) {
    c(
      paste0(
        "    updateYearSpans(",
        matrix,
        "_data, ",
        matrix,
        "_meta);"
      ),
      ""
    )
  } else {
    character()
  }
  
  #
  # If another matrix already owns updateYearSpans()
  # and this matrix has a different available year range,
  # create matrix-specific year variables.
  #
  matrix_year_lines <- if (
    isTRUE(use_matrix_years) &&
    !isTRUE(needs_update_year_spans)
  ) {
    build_matrix_year_variables_js(
      matrix = matrix
    )
  } else {
    character()
  }
  
  matrix_year_span_lines <- if (
    isTRUE(use_matrix_years) &&
    !isTRUE(needs_update_year_spans)
  ) {
    build_matrix_year_span_js(
      matrix = matrix
    )
  } else {
    character()
  }
  
  all_chart_markers <- grep(
    "^\\s*//\\s*Content for chart\\s+[0-9]+\\s*$",
    js_lines
  )
  
  all_chart_markers <- all_chart_markers[
    all_chart_markers > region_start &
      all_chart_markers < region_end
  ]
  
  later_markers <- all_chart_markers[
    all_chart_markers > chart_marker
  ]
  
  section_end <- if (
    length(later_markers) > 0
  ) {
    min(later_markers) - 1
  } else {
    region_end - 1
  }
  
  section <- js_lines[
    chart_marker:section_end
  ]
  
  section <- remove_page_chart_config_blocks(
    section
  )
  
  #
  # Remove blank lines left behind by the previous
  # managed chart block so repeated saves do not
  # accumulate whitespace.
  #
  while (
    length(section) > 0 &&
    !nzchar(trimws(section[[length(section)]]))
  ) {
    section <- section[-length(section)]
  }
  
  replacement_section <- c(
    section,
    "",
    "    // BuildR map chart config start",
    year_update_lines,
    matrix_year_lines,
    matrix_year_span_lines,
    map_chart_js,
    "    // BuildR map chart config end",
    ""
  )
  
  updated_js <- c(
    if (chart_marker > 1) {
      js_lines[
        seq_len(
          chart_marker - 1
        )
      ]
    } else {
      character()
    },
    
    replacement_section,
    
    if (section_end < length(js_lines)) {
      js_lines[
        seq.int(
          section_end + 1,
          length(js_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  writeLines(
    updated_js,
    paths$js,
    useBytes = TRUE
  )
  
  invisible(
    paths$js
  )
}

page_js_has_update_year_spans_before <- function(
    js_lines,
    line_number
) {
  
  if (
    length(js_lines) == 0 ||
    line_number <= 1
  ) {
    return(FALSE)
  }
  
  earlier_lines <- js_lines[
    seq_len(line_number - 1)
  ]
  
  any(
    grepl(
      "\\bupdateYearSpans\\s*\\(",
      earlier_lines,
      perl = TRUE
    )
  )
}

remove_matrix_update_year_spans <- function(
    lines,
    matrix
) {
  
  pattern <- paste0(
    "^\\s*updateYearSpans\\s*\\(\\s*",
    matrix,
    "_data\\s*,\\s*",
    matrix,
    "_meta\\s*\\)\\s*;?\\s*$"
  )
  
  lines[
    !grepl(
      pattern,
      lines,
      perl = TRUE
    )
  ]
}

page_dynamic_year_class <- function(
    token,
    year_prefix = NULL
) {
  
  suffix <- switch(
    token,
    "<<latest-year>>" = "latest-year",
    "<<last-year>>" = "last-year",
    "<<first-year>>" = "first-year",
    NULL
  )
  
  if (is.null(suffix)) {
    return(NULL)
  }
  
  if (
    is.null(year_prefix) ||
    !nzchar(year_prefix)
  ) {
    return(suffix)
  }
  
  paste0(
    year_prefix,
    "-",
    suffix
  )
}

build_matrix_year_span_js <- function(
    matrix
) {
  
  c(
    paste0(
      '    document.querySelectorAll(".',
      matrix,
      '-latest-year").forEach(el => {'
    ),
    paste0(
      "        el.innerHTML = ",
      matrix,
      "_latest_year;"
    ),
    "    });",
    "",
    paste0(
      '    document.querySelectorAll(".',
      matrix,
      '-last-year").forEach(el => {'
    ),
    paste0(
      "        el.innerHTML = ",
      matrix,
      "_last_year;"
    ),
    "    });",
    "",
    paste0(
      '    document.querySelectorAll(".',
      matrix,
      '-first-year").forEach(el => {'
    ),
    paste0(
      "        el.innerHTML = ",
      matrix,
      "_first_year;"
    ),
    "    });",
    ""
  )
}

ensure_matrix_year_variables_after_read_data <- function(
    js_lines,
    matrix
) {
  
  read_pattern <- paste0(
    "^\\s*const\\s+\\[\\s*",
    matrix,
    "_data\\s*,\\s*",
    matrix,
    "_meta\\s*\\]\\s*=\\s*await\\s+readData\\(",
    '["\']',
    matrix,
    '["\']',
    "\\);\\s*$"
  )
  
  read_line <- grep(
    read_pattern,
    js_lines,
    perl = TRUE
  )
  
  if (length(read_line) != 1) {
    stop(
      paste0(
        "Could not uniquely identify the readData() call for ",
        matrix,
        "."
      )
    )
  }
  
  #
  # Remove any existing managed year-variable block.
  #
  year_start_pattern <- paste0(
    "^\\s*//\\s*BuildR\\s+",
    matrix,
    "\\s+year variables start\\s*$"
  )
  
  year_end_pattern <- paste0(
    "^\\s*//\\s*BuildR\\s+",
    matrix,
    "\\s+year variables end\\s*$"
  )
  
  year_start <- grep(
    year_start_pattern,
    js_lines,
    perl = TRUE
  )
  
  year_end <- grep(
    year_end_pattern,
    js_lines,
    perl = TRUE
  )
  
  if (
    length(year_start) == 1 &&
    length(year_end) == 1 &&
    year_end > year_start
  ) {
    
    js_lines <- js_lines[
      -seq.int(
        year_start,
        year_end
      )
    ]
  }
  
  #
  # Remove any existing managed year-span block.
  #
  span_start_pattern <- paste0(
    "^\\s*//\\s*BuildR\\s+",
    matrix,
    "\\s+year spans start\\s*$"
  )
  
  span_end_pattern <- paste0(
    "^\\s*//\\s*BuildR\\s+",
    matrix,
    "\\s+year spans end\\s*$"
  )
  
  span_start <- grep(
    span_start_pattern,
    js_lines,
    perl = TRUE
  )
  
  span_end <- grep(
    span_end_pattern,
    js_lines,
    perl = TRUE
  )
  
  if (
    length(span_start) == 1 &&
    length(span_end) == 1 &&
    span_end > span_start
  ) {
    
    js_lines <- js_lines[
      -seq.int(
        span_start,
        span_end
      )
    ]
  }
  
  #
  # Re-find readData() after removals.
  #
  read_line <- grep(
    read_pattern,
    js_lines,
    perl = TRUE
  )
  
  if (length(read_line) != 1) {
    stop(
      paste0(
        "Could not re-identify the readData() call for ",
        matrix,
        "."
      )
    )
  }
  
  year_lines <- build_matrix_year_variables_js(
    matrix = matrix
  )
  
  span_lines <- c(
    paste0(
      "    // BuildR ",
      matrix,
      " year spans start"
    ),
    build_matrix_year_span_js(
      matrix = matrix
    ),
    paste0(
      "    // BuildR ",
      matrix,
      " year spans end"
    ),
    ""
  )
  
  js_lines <- c(
    js_lines[
      seq_len(read_line)
    ],
    "",
    year_lines,
    span_lines,
    if (read_line < length(js_lines)) {
      js_lines[
        seq.int(
          read_line + 1,
          length(js_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  js_lines
}

escape_javascript_template_literal <- function(value) {
  
  value <- as.character(value)[1]
  
  if (
    is.na(value) ||
    !nzchar(value)
  ) {
    return("")
  }
  
  #
  # Protect content inside a JavaScript template literal.
  #
  value <- gsub(
    "\\\\",
    "\\\\\\\\",
    value,
    fixed = TRUE
  )
  
  value <- gsub(
    "`",
    "\\`",
    value,
    fixed = TRUE
  )
  
  value <- gsub(
    "${",
    "\\${",
    value,
    fixed = TRUE
  )
  
  value
}

read_page_info_boxes <- function(
    project_root,
    page_href
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$js)) {
    stop(
      paste0(
        paths$js_filename,
        " was not found."
      )
    )
  }
  
  js_lines <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  start_line <- grep(
    "^\\s*//\\s*BuildR info boxes start\\s*$",
    js_lines
  )
  
  end_line <- grep(
    "^\\s*//\\s*BuildR info boxes end\\s*$",
    js_lines
  )
  
  if (
    length(start_line) != 1 ||
    length(end_line) != 1 ||
    end_line <= start_line
  ) {
    
    return(
      list(
        definitions = "",
        source = "",
        meaning = ""
      )
    )
  }
  
  section <- js_lines[
    seq.int(
      start_line + 1,
      end_line - 1
    )
  ]
  
  extract_box <- function(
    marker
  ) {
    
    marker_line <- grep(
      paste0(
        "^\\s*//\\s*",
        marker,
        "\\s*$"
      ),
      section
    )
    
    if (length(marker_line) != 1) {
      return("")
    }
    
    #
    # Find the first template-literal opening
    # after this marker.
    #
    search_lines <- section[
      seq.int(
        marker_line + 1,
        length(section)
      )
    ]
    
    first_tick <- which(
      grepl(
        "`",
        search_lines,
        fixed = TRUE
      )
    )[1]
    
    if (is.na(first_tick)) {
      return("")
    }
    
    absolute_start <-
      marker_line +
      first_tick
    
    first_line <- section[
      absolute_start
    ]
    
    #
    # Handle an empty or single-line literal.
    #
    tick_positions <- gregexpr(
      "(?<!\\\\)`",
      first_line,
      perl = TRUE
    )[[1]]
    
    if (length(tick_positions) >= 2) {
      
      value <- sub(
        "^[^`]*`",
        "",
        first_line
      )
      
      value <- sub(
        "`\\s*,?\\s*$",
        "",
        value
      )
      
      return(value)
    }
    
    #
    # Otherwise continue until the closing
    # unescaped backtick.
    #
    content <- sub(
      "^[^`]*`",
      "",
      first_line
    )
    
    line_number <- absolute_start + 1
    
    while (
      line_number <= length(section)
    ) {
      
      current_line <- section[
        line_number
      ]
      
      if (
        grepl(
          "(?<!\\\\)`\\s*,?\\s*$",
          current_line,
          perl = TRUE
        )
      ) {
        
        current_line <- sub(
          "`\\s*,?\\s*$",
          "",
          current_line
        )
        
        content <- c(
          content,
          current_line
        )
        
        break
      }
      
      content <- c(
        content,
        current_line
      )
      
      line_number <- line_number + 1
    }
    
    paste(
      content,
      collapse = "\n"
    )
  }
  
  list(
    definitions = extract_box(
      "DEFINITIONS BOX"
    ),
    source = extract_box(
      "SOURCE BOX"
    ),
    meaning = extract_box(
      "DATA MEANING BOX"
    )
  )
}

update_page_info_boxes_js <- function(
    project_root,
    page_href,
    definitions = "",
    source = "",
    meaning = ""
) {
  
  paths <- page_design_paths(
    project_root,
    page_href
  )
  
  if (!file.exists(paths$js)) {
    stop(
      paste0(
        paths$js_filename,
        " was not found."
      )
    )
  }
  
  js_lines <- readLines(
    paths$js,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  start_line <- grep(
    "^\\s*//\\s*BuildR info boxes start\\s*$",
    js_lines
  )
  
  end_line <- grep(
    "^\\s*//\\s*BuildR info boxes end\\s*$",
    js_lines
  )
  
  if (
    length(start_line) != 1 ||
    length(end_line) != 1 ||
    end_line <= start_line
  ) {
    stop(
      "Could not uniquely identify the info-box JavaScript region."
    )
  }
  
  definitions <- escape_javascript_template_literal(
    definitions
  )
  
  source <- escape_javascript_template_literal(
    source
  )
  
  meaning <- escape_javascript_template_literal(
    meaning
  )
  
  replacement <- c(
    "    // BuildR info boxes start",
    "    populateInfoBoxes(",
    "        [",
    '            "Definitions",',
    '            "Source",',
    '            "What does the data mean?"',
    "        ],",
    "        [",
    "            // DEFINITIONS BOX",
    paste0(
      "            `",
      definitions,
      "`,"
    ),
    "",
    "            // SOURCE BOX",
    paste0(
      "            `",
      source,
      "`,"
    ),
    "",
    "            // DATA MEANING BOX",
    paste0(
      "            `",
      meaning,
      "`"
    ),
    "        ]",
    "    );",
    "    // BuildR info boxes end"
  )
  
  updated_js <- c(
    if (start_line > 1) {
      js_lines[
        seq_len(start_line - 1)
      ]
    } else {
      character()
    },
    
    replacement,
    
    if (end_line < length(js_lines)) {
      js_lines[
        seq.int(
          end_line + 1,
          length(js_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  writeLines(
    updated_js,
    paths$js,
    useBytes = TRUE
  )
  
  invisible(
    paths$js
  )
}

extract_page_info_boxes_js <- function(
    js_lines
) {
  
  start_line <- grep(
    "^\\s*//\\s*BuildR info boxes start\\s*$",
    js_lines
  )
  
  end_line <- grep(
    "^\\s*//\\s*BuildR info boxes end\\s*$",
    js_lines
  )
  
  if (
    length(start_line) != 1 ||
    length(end_line) != 1 ||
    end_line <= start_line
  ) {
    return(NULL)
  }
  
  js_lines[
    seq.int(
      start_line,
      end_line
    )
  ]
}

restore_page_info_boxes_js <- function(
    js_lines,
    info_box_lines
) {
  
  if (
    is.null(info_box_lines) ||
    length(info_box_lines) == 0
  ) {
    return(js_lines)
  }
  
  start_line <- grep(
    "^\\s*//\\s*BuildR info boxes start\\s*$",
    js_lines
  )
  
  end_line <- grep(
    "^\\s*//\\s*BuildR info boxes end\\s*$",
    js_lines
  )
  
  #
  # The reset page.js should contain the standard
  # info-box block. Replace that block with the
  # previously saved contents.
  #
  if (
    length(start_line) == 1 &&
    length(end_line) == 1 &&
    end_line > start_line
  ) {
    
    return(
      c(
        if (start_line > 1) {
          js_lines[
            seq_len(start_line - 1)
          ]
        } else {
          character()
        },
        
        info_box_lines,
        
        if (end_line < length(js_lines)) {
          js_lines[
            seq.int(
              end_line + 1,
              length(js_lines)
            )
          ]
        } else {
          character()
        }
      )
    )
  }
  
  #
  # Fallback: if the reset JS somehow does not
  # contain an info-box block, insert the preserved
  # block immediately before the end of the
  # DOMContentLoaded listener.
  #
  closing_line <- grep(
    "^\\s*\\}\\)\\s*;?\\s*$",
    js_lines
  )
  
  if (length(closing_line) == 0) {
    stop(
      "Could not identify the end of the page JavaScript."
    )
  }
  
  closing_line <- tail(
    closing_line,
    1
  )
  
  c(
    if (closing_line > 1) {
      js_lines[
        seq_len(closing_line - 1)
      ]
    } else {
      character()
    },
    "",
    info_box_lines,
    "",
    js_lines[
      seq.int(
        closing_line,
        length(js_lines)
      )
    ]
  )
}