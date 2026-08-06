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
    'class=["\'][^"\']*\\bcurrent-page\\b',
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
    'class=["\'][^"\']*\\bcurrent-page\\b',
    lines,
    perl = TRUE
  )
  
  if (length(start_line) != 1) {
    stop(
      "Could not uniquely identify the .current-page element."
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
    calculation
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
  
  card_number <- as.integer(card_number)
  
  if (
    length(card_number) != 1 ||
    is.na(card_number) ||
    card_number < 1 ||
    card_number > 6
  ) {
    stop("A valid page card number is required.")
  }
  
  if (
    is.null(calculation) ||
    is.null(calculation$matrix) ||
    !nzchar(as.character(calculation$matrix)[1])
  ) {
    stop("No stored calculation was supplied.")
  }
  
  matrix <- as.character(
    calculation$matrix
  )[1]
  
  selected_columns <- as.character(
    calculation$selected_columns
  )
  
  if (length(selected_columns) == 0) {
    stop("No value column was selected.")
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
      "The page-card end marker occurs before its start marker."
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
  
  section_end <- if (length(next_markers) > 0) {
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
  
  # Only treat a declaration as reusable when it occurs
  # earlier inside the page-card content region.
  matrix_load_pattern <- paste0(
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
  
  existing_matrix_loads <- grep(
    matrix_load_pattern,
    js_lines,
    perl = TRUE
  )
  
  matrix_loaded_earlier <- any(
    existing_matrix_loads > start_marker &
      existing_matrix_loads < current_marker
  )
  
  load_lines <- if (matrix_loaded_earlier) {
    
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
          ");"
        )
      },
      
      ""
    )
  }
  
  filter_lines <- character()
  
  if (length(js_filters) > 0) {
    
    filter_conditions <- character()
    
    for (column_name in names(js_filters)) {
      
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
          javascript_string(column_name),
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
          javascript_string(column_name),
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
      collapse = " && "
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
          length(filter_definition$values)
        } else {
          length(filter_definition)
        }
      },
      integer(1)
    ) == 1L
  )
  
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
    
  } else if (length(selected_columns) == 1) {
    
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
    
  } else if (decimal_places > 0) {
    
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
        seq_len(current_marker - 1)
      ]
    } else {
      character()
    },
    
    replacement,
    
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
    js_path,
    useBytes = TRUE
  )
  
  invisible(js_path)
}

clear_page_js_chart_content <- function(
    js_lines,
    js_filename
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
  
  c(
    js_lines[seq_len(chart_start)],
    "",
    "",
    js_lines[chart_end:length(js_lines)]
  )
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
  
  updated_html <- original_html
  
  # Clear the page strapline.
  current_page <- grep(
    'class=["\'][^"\']*\\bcurrent-page\\b',
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
        js_filename = paths$js_filename
      )
      
      updated_js <- updated_js[
        !grepl(
          "^\\s*import\\b.*[\"']\\./charts/",
          updated_js,
          perl = TRUE
        )
      ]
      
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