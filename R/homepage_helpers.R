clear_homepage_files <- function(project_root) {
  
  index_html_path <- file.path(
    project_root,
    "index.html"
  )
  
  index_js_path <- file.path(
    project_root,
    "src",
    "index.js"
  )
  
  if (!file.exists(index_html_path)) {
    stop("index.html was not found.")
  }
  
  if (!file.exists(index_js_path)) {
    stop("src/index.js was not found.")
  }
  
  original_html <- readLines(
    index_html_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  original_js <- readLines(
    index_js_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  updated_html <- original_html
  
  count_open_divs <- function(line) {
    matches <- gregexpr(
      "<div\\b",
      line,
      perl = TRUE
    )[[1]]
    
    if (matches[1] == -1) {
      return(0L)
    }
    
    length(matches)
  }
  
  count_close_divs <- function(line) {
    matches <- gregexpr(
      "</div\\s*>",
      line,
      perl = TRUE
    )[[1]]
    
    if (matches[1] == -1) {
      return(0L)
    }
    
    length(matches)
  }
  
  find_closing_div <- function(lines, start_line) {
    
    depth <- 0L
    
    for (i in seq.int(start_line, length(lines))) {
      
      depth <- depth +
        count_open_divs(lines[i]) -
        count_close_divs(lines[i])
      
      if (depth == 0L) {
        return(i)
      }
    }
    
    stop(
      paste0(
        "Could not find the closing </div> for line ",
        start_line,
        "."
      )
    )
  }
  
  clear_div_contents <- function(lines, start_line) {
    
    end_line <- find_closing_div(
      lines,
      start_line
    )
    
    opening_line <- lines[start_line]
    closing_line <- lines[end_line]
    
    if (start_line == end_line) {
      
      updated_line <- sub(
        "(<div\\b[^>]*>)[\\s\\S]*(</div\\s*>)",
        "\\1\\2",
        opening_line,
        perl = TRUE
      )
      
      lines[start_line] <- updated_line
      
      return(lines)
    }
    
    c(
      if (start_line > 1) {
        lines[seq_len(start_line - 1)]
      } else {
        character()
      },
      opening_line,
      closing_line,
      if (end_line < length(lines)) {
        lines[seq.int(end_line + 1, length(lines))]
      } else {
        character()
      }
    )
  }
  
  # Clear .current-page
  current_page_line <- grep(
    'class=["\'][^"\']*\\bcurrent-page\\b',
    updated_html,
    perl = TRUE
  )
  
  if (length(current_page_line) != 1) {
    stop(
      "Could not uniquely identify the .current-page element."
    )
  }
  
  updated_html <- clear_div_contents(
    updated_html,
    current_page_line
  )
  
  # Find the information card before clearing the other card bodies.
  information_heading_line <- grep(
    "Information about this dashboard",
    updated_html,
    fixed = TRUE
  )
  
  if (length(information_heading_line) != 1) {
    stop(
      paste(
        "Could not uniquely identify the",
        "Information about this dashboard heading."
      )
    )
  }
  
  card_body_lines <- grep(
    'class=["\'][^"\']*\\bcard-body\\b',
    updated_html,
    perl = TRUE
  )
  
  if (length(card_body_lines) == 0) {
    stop("No card-body elements were found.")
  }
  
  information_card_body_line <- tail(
    card_body_lines[
      card_body_lines <= information_heading_line
    ],
    1
  )
  
  if (length(information_card_body_line) != 1) {
    stop(
      "Could not identify the information card body."
    )
  }
  
  # Clear ordinary card bodies from bottom to top so line numbers remain valid.
  ordinary_card_body_lines <- setdiff(
    card_body_lines,
    information_card_body_line
  )
  
  for (line_number in sort(
    ordinary_card_body_lines,
    decreasing = TRUE
  )) {
    updated_html <- clear_div_contents(
      updated_html,
      line_number
    )
  }
  
  # Recalculate line positions after the earlier edits.
  information_heading_line <- grep(
    "Information about this dashboard",
    updated_html,
    fixed = TRUE
  )
  
  card_body_lines <- grep(
    'class=["\'][^"\']*\\bcard-body\\b',
    updated_html,
    perl = TRUE
  )
  
  information_card_body_line <- tail(
    card_body_lines[
      card_body_lines <= information_heading_line
    ],
    1
  )
  
  information_card_end <- find_closing_div(
    updated_html,
    information_card_body_line
  )
  
  heading_end_line <- grep(
    "</h2>",
    updated_html,
    fixed = TRUE
  )
  
  heading_end_line <- heading_end_line[
    heading_end_line >= information_card_body_line &
      heading_end_line <= information_card_end
  ]
  
  if (length(heading_end_line) != 1) {
    stop(
      "Could not identify the end of the information heading."
    )
  }
  
  # Keep the opening card-body line, the h2/SVG block and closing div.
  updated_html <- c(
    if (information_card_body_line > 1) {
      updated_html[
        seq_len(information_card_body_line - 1)
      ]
    } else {
      character()
    },
    
    updated_html[
      information_card_body_line:heading_end_line
    ],
    
    updated_html[information_card_end],
    
    if (information_card_end < length(updated_html)) {
      updated_html[
        seq.int(
          information_card_end + 1,
          length(updated_html)
        )
      ]
    } else {
      character()
    }
  )
  
  # Clear all card footers.
  card_footer_lines <- grep(
    'class=["\'][^"\']*\\bcard-footer\\b',
    updated_html,
    perl = TRUE
  )
  
  if (length(card_footer_lines) == 0) {
    stop("No card-footer elements were found.")
  }
  
  for (line_number in sort(
    card_footer_lines,
    decreasing = TRUE
  )) {
    updated_html <- clear_div_contents(
      updated_html,
      line_number
    )
  }
  
  
  
  tryCatch(
    {
      writeLines(
        updated_html,
        index_html_path,
        useBytes = TRUE
      )
      
      card_count <- length(
        find_homepage_cards_row(updated_html)$card_starts
      )
      
      update_homepage_js_card_sections(
        project_root = project_root,
        card_count = card_count,
        preserve_existing = FALSE
      )
    },
    error = function(error) {
      
      writeLines(
        original_html,
        index_html_path,
        useBytes = TRUE
      )
      
      writeLines(
        original_js,
        index_js_path,
        useBytes = TRUE
      )
      
      stop(
        paste(
          "Homepage files were restored after an error:",
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
  
  invisible(
    list(
      html_path = index_html_path,
      js_path = index_js_path
    )
  )
}

update_homepage_strapline <- function(
    project_root,
    strapline
) {
  index_html_path <- file.path(
    project_root,
    "index.html"
  )
  
  if (!file.exists(index_html_path)) {
    stop("index.html was not found.")
  }
  
  strapline <- trimws(strapline)
  
  if (!nzchar(strapline)) {
    stop("The homepage strapline cannot be empty.")
  }
  
  html_lines <- readLines(
    index_html_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  current_page_line <- grep(
    'class=["\'][^"\']*current-page[^"\']*["\']',
    html_lines,
    perl = TRUE
  )
  
  if (length(current_page_line) != 1) {
    stop(
      "Could not uniquely identify the .current-page element."
    )
  }
  
  start_line <- current_page_line[[1]]
  
  count_open_divs <- function(line) {
    matches <- gregexpr(
      "<div\\b",
      line,
      perl = TRUE
    )[[1]]
    
    if (matches[[1]] == -1) {
      return(0L)
    }
    
    length(matches)
  }
  
  count_close_divs <- function(line) {
    matches <- gregexpr(
      "</div\\s*>",
      line,
      perl = TRUE
    )[[1]]
    
    if (matches[[1]] == -1) {
      return(0L)
    }
    
    length(matches)
  }
  
  find_closing_div <- function(lines, start) {
    depth <- 0L
    
    for (i in seq.int(start, length(lines))) {
      depth <- depth +
        count_open_divs(lines[[i]]) -
        count_close_divs(lines[[i]])
      
      if (depth == 0L) {
        return(i)
      }
    }
    
    stop(
      "Could not find the closing </div> for .current-page."
    )
  }
  
  end_line <- find_closing_div(
    html_lines,
    start_line
  )
  
  escaped_strapline <- as.character(
    htmltools::htmlEscape(
      strapline,
      attribute = FALSE
    )
  )
  
  opening_tag <- sub(
    "^(.*?<div\\b[^>]*>).*",
    "\\1",
    html_lines[[start_line]],
    perl = TRUE
  )
  
  closing_indent <- sub(
    "^(\\s*).*",
    "\\1",
    html_lines[[start_line]]
  )
  
  replacement_line <- paste0(
    opening_tag,
    escaped_strapline,
    "</div>"
  )
  
  updated_lines <- c(
    if (start_line > 1) {
      html_lines[seq_len(start_line - 1)]
    } else {
      character()
    },
    
    replacement_line,
    
    if (end_line < length(html_lines)) {
      html_lines[seq.int(end_line + 1, length(html_lines))]
    } else {
      character()
    }
  )
  
  writeLines(
    updated_lines,
    index_html_path,
    useBytes = TRUE
  )
  
  invisible(index_html_path)
}

count_open_divs <- function(line) {
  
  matches <- gregexpr(
    "<div\\b",
    line,
    perl = TRUE
  )[[1]]
  
  if (matches[1] == -1) {
    return(0L)
  }
  
  length(matches)
}


count_close_divs <- function(line) {
  
  matches <- gregexpr(
    "</div\\s*>",
    line,
    perl = TRUE
  )[[1]]
  
  if (matches[1] == -1) {
    return(0L)
  }
  
  length(matches)
}


find_closing_div <- function(lines, start_line) {
  
  depth <- 0L
  
  for (i in seq.int(start_line, length(lines))) {
    
    depth <- depth +
      count_open_divs(lines[i]) -
      count_close_divs(lines[i])
    
    if (depth == 0L) {
      return(i)
    }
  }
  
  stop(
    paste0(
      "Could not find the closing </div> for line ",
      start_line,
      "."
    )
  )
}


find_homepage_cards_row <- function(html_lines) {
  
  section_candidates <- grep(
    paste0(
      '<div\\b[^>]*class\\s*=\\s*["\']',
      '[^"\']*\\bpy-2\\b',
      '[^"\']*\\btext-center\\b',
      '[^"\']*["\']'
    ),
    html_lines,
    perl = TRUE
  )
  
  if (length(section_candidates) == 0) {
    stop("Could not identify the homepage cards section.")
  }
  
  for (section_start in section_candidates) {
    
    section_end <- find_closing_div(
      html_lines,
      section_start
    )
    
    possible_rows <- grep(
      paste0(
        '<div\\b[^>]*class\\s*=\\s*["\']',
        '[^"\']*\\brow\\b',
        '[^"\']*["\']'
      ),
      html_lines,
      perl = TRUE
    )
    
    possible_rows <- possible_rows[
      possible_rows > section_start &
        possible_rows < section_end
    ]
    
    if (length(possible_rows) == 0) {
      next
    }
    
    for (row_start in possible_rows) {
      
      row_end <- find_closing_div(
        html_lines,
        row_start
      )
      
      card_starts <- grep(
        paste0(
          '<div\\b[^>]*class\\s*=\\s*["\']',
          '[^"\']*\\bcol-6\\b',
          '[^"\']*\\bcol-xl-4\\b',
          '[^"\']*\\bpy-2\\b',
          '[^"\']*["\']'
        ),
        html_lines,
        perl = TRUE
      )
      
      card_starts <- card_starts[
        card_starts > row_start &
          card_starts < row_end
      ]
      
      if (length(card_starts) > 0) {
        return(
          list(
            section_start = section_start,
            section_end = section_end,
            row_start = row_start,
            row_end = row_end,
            card_starts = card_starts
          )
        )
      }
    }
  }
  
  stop("Could not identify the homepage cards row.")
}


count_homepage_cards <- function(project_root) {
  
  index_html_path <- file.path(
    project_root,
    "index.html"
  )
  
  if (!file.exists(index_html_path)) {
    stop("index.html was not found.")
  }
  
  html_lines <- readLines(
    index_html_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  card_section <- find_homepage_cards_row(
    html_lines
  )
  
  length(card_section$card_starts)
}


add_row_centring_class <- function(row_line) {
  
  class_value <- sub(
    paste0(
      '^.*class\\s*=\\s*["\']',
      '([^"\']*)',
      '["\'].*$'
    ),
    "\\1",
    row_line,
    perl = TRUE
  )
  
  classes <- strsplit(
    trimws(class_value),
    "\\s+"
  )[[1]]
  
  if (!"justify-content-center" %in% classes) {
    classes <- c(
      classes,
      "justify-content-center"
    )
  }
  
  replacement_class <- paste(
    classes,
    collapse = " "
  )
  
  sub(
    paste0(
      '(class\\s*=\\s*["\'])',
      '[^"\']*',
      '(["\'])'
    ),
    paste0(
      "\\1",
      replacement_class,
      "\\2"
    ),
    row_line,
    perl = TRUE
  )
}


blank_homepage_card <- function() {
  
  c(
    '                <div class="col-6 col-xl-4 py-2">',
    '                    <div class="home card h-100">',
    '                        <div class="card-body d-flex flex-column justify-content-center text-center"></div>',
    '                        <div class="card-footer blue-bg"></div>',
    '                    </div>',
    '                </div>'
  )
}


update_homepage_card_count <- function(
    project_root,
    card_count
) {
  
  index_html_path <- file.path(
    project_root,
    "index.html"
  )
  
  if (!file.exists(index_html_path)) {
    stop("index.html was not found.")
  }
  
  card_count <- as.integer(card_count)
  
  if (
    length(card_count) != 1 ||
    is.na(card_count) ||
    card_count < 1 ||
    card_count > 9
  ) {
    stop("The number of cards must be between 1 and 9.")
  }
  
  original_html <- readLines(
    index_html_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  card_section <- find_homepage_cards_row(
    original_html
  )
  
  row_start <- card_section$row_start
  row_end <- card_section$row_end
  card_starts <- card_section$card_starts
  
  existing_cards <- lapply(
    card_starts,
    function(card_start) {
      
      card_end <- find_closing_div(
        original_html,
        card_start
      )
      
      original_html[
        card_start:card_end
      ]
    }
  )
  
  cards_to_keep <- min(
    length(existing_cards),
    card_count
  )
  
  updated_cards <- if (cards_to_keep > 0) {
    existing_cards[
      seq_len(cards_to_keep)
    ]
  } else {
    list()
  }
  
  additional_cards <- card_count - cards_to_keep
  
  if (additional_cards > 0) {
    
    updated_cards <- c(
      updated_cards,
      replicate(
        additional_cards,
        blank_homepage_card(),
        simplify = FALSE
      )
    )
  }
  
  row_opening <- add_row_centring_class(
    original_html[row_start]
  )
  
  replacement_row <- c(
    row_opening,
    unlist(
      updated_cards,
      use.names = FALSE
    ),
    original_html[row_end]
  )
  
  updated_html <- c(
    if (row_start > 1) {
      original_html[
        seq_len(row_start - 1)
      ]
    } else {
      character()
    },
    
    replacement_row,
    
    if (row_end < length(original_html)) {
      original_html[
        seq.int(
          row_end + 1,
          length(original_html)
        )
      ]
    } else {
      character()
    }
  )
  
  index_js_path <- file.path(
    project_root,
    "src",
    "index.js"
  )
  
  if (!file.exists(index_js_path)) {
    stop("src/index.js was not found.")
  }
  
  original_js <- readLines(
    index_js_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  tryCatch(
    {
      writeLines(
        updated_html,
        index_html_path,
        useBytes = TRUE
      )
      
      update_homepage_js_card_sections(
        project_root = project_root,
        card_count = card_count,
        preserve_existing = TRUE
      )
    },
    error = function(error) {
      
      writeLines(
        original_html,
        index_html_path,
        useBytes = TRUE
      )
      
      writeLines(
        original_js,
        index_js_path,
        useBytes = TRUE
      )
      
      stop(
        paste(
          "Homepage card changes were restored after an error:",
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
  
  invisible(index_html_path)
}

update_homepage_js_card_sections <- function(
    project_root,
    card_count,
    preserve_existing = TRUE
) {
  
  js_path <- file.path(
    project_root,
    "src",
    "index.js"
  )
  
  if (!file.exists(js_path)) {
    stop("src/index.js was not found.")
  }
  
  card_count <- as.integer(card_count)
  
  if (
    length(card_count) != 1 ||
    is.na(card_count) ||
    card_count < 1 ||
    card_count > 9
  ) {
    stop("card_count must be between 1 and 9.")
  }
  
  lines <- readLines(
    js_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  anchor <- grep(
    "^\\s*//\\s*Insert values into homepage cards below\\s*$",
    lines
  )
  
  if (length(anchor) != 1) {
    stop(
      "Could not find '// Insert values into homepage cards below'."
    )
  }
  
  closing_lines <- grep(
    "^\\s*\\}\\)\\s*;?\\s*$",
    lines
  )
  
  if (length(closing_lines) == 0) {
    stop(
      "Could not identify the end of the event listener."
    )
  }
  
  closing <- tail(
    closing_lines,
    1
  )
  
  if (closing <= anchor) {
    stop(
      "The event listener closes before the card content marker."
    )
  }
  
  sections <- list()
  
  if (isTRUE(preserve_existing)) {
    
    existing_region <- if (closing > anchor + 1) {
      lines[
        seq.int(
          anchor + 1,
          closing - 1
        )
      ]
    } else {
      character()
    }
    
    comment_lines <- grep(
      "^\\s*//\\s*Content for card\\s+[0-9]+\\s*$",
      existing_region
    )
    
    existing_nonblank <- existing_region[
      nzchar(trimws(existing_region))
    ]
    
    if (
      length(comment_lines) == 0 &&
      length(existing_nonblank) > 0
    ) {
      stop(
        paste(
          "Existing homepage JavaScript has not yet been divided",
          "into card sections. Clear the template content first."
        )
      )
    }
    
    if (length(comment_lines) > 0) {
      
      for (i in seq_along(comment_lines)) {
        
        section_start <- comment_lines[i]
        
        section_end <- if (i < length(comment_lines)) {
          comment_lines[i + 1] - 1
        } else {
          length(existing_region)
        }
        
        sections[[i]] <- existing_region[
          section_start:section_end
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
      
      section <- sections[[i]]
      
      # Renumber the comment while preserving all code in the section.
      section[1] <- paste0(
        "    // Content for card ",
        i
      )
      
      # Remove only trailing blank lines so spacing can be rebuilt
      # consistently between card sections.
      while (
        length(section) > 1 &&
        !nzchar(trimws(tail(section, 1)))
      ) {
        section <- head(
          section,
          -1
        )
      }
      
    } else {
      
      section <- paste0(
        "    // Content for card ",
        i
      )
    }
    
    # Add exactly two empty lines after every card section.
    new_region <- c(
      new_region,
      section,
      "",
      ""
    )
  }
  
  updated <- c(
    lines[seq_len(anchor)],
    "",
    new_region,
    lines[closing:length(lines)]
  )
  
  writeLines(
    updated,
    js_path,
    useBytes = TRUE
  )
  
  invisible(js_path)
}

read_homepage_strapline <- function(project_root) {
  
  index_html_path <- file.path(
    project_root,
    "index.html"
  )
  
  if (!file.exists(index_html_path)) {
    return("")
  }
  
  html_lines <- readLines(
    index_html_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  start_line <- grep(
    'class=["\'][^"\']*\\bcurrent-page\\b',
    html_lines,
    perl = TRUE
  )
  
  if (length(start_line) != 1) {
    return("")
  }
  
  end_line <- find_closing_div(
    html_lines,
    start_line
  )
  
  html <- paste(
    html_lines[start_line:end_line],
    collapse = "\n"
  )
  
  html <- sub(
    "^.*?<div\\b[^>]*>",
    "",
    html,
    perl = TRUE
  )
  
  html <- sub(
    "</div>.*$",
    "",
    html,
    perl = TRUE
  )
  
  html <- trimws(html)
  
  if (!nzchar(html)) {
    return("")
  }
  
  xml2::xml_text(
    xml2::read_html(
      paste0("<div>", html, "</div>")
    )
  )
}

update_homepage_card_link <- function(
    project_root,
    card_number,
    page_href = "",
    page_text = ""
) {
  index_html_path <- file.path(
    project_root,
    "index.html"
  )
  
  if (!file.exists(index_html_path)) {
    stop("index.html was not found.")
  }
  
  card_number <- as.integer(card_number)
  
  if (
    length(card_number) != 1 ||
    is.na(card_number) ||
    card_number < 1
  ) {
    stop("A valid card number is required.")
  }
  
  html_lines <- readLines(
    index_html_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  card_section <- find_homepage_cards_row(
    html_lines
  )
  
  if (card_number > length(card_section$card_starts)) {
    stop(
      paste0(
        "Card ",
        card_number,
        " was not found."
      )
    )
  }
  
  card_start <- card_section$card_starts[card_number]
  
  card_end <- find_closing_div(
    html_lines,
    card_start
  )
  
  footer_candidates <- grep(
    'class=["\'][^"\']*\\bcard-footer\\b',
    html_lines,
    perl = TRUE
  )
  
  footer_candidates <- footer_candidates[
    footer_candidates >= card_start &
      footer_candidates <= card_end
  ]
  
  if (length(footer_candidates) != 1) {
    stop(
      paste0(
        "Could not uniquely identify the footer for card ",
        card_number,
        "."
      )
    )
  }
  
  footer_start <- footer_candidates[1]
  
  footer_end <- find_closing_div(
    html_lines,
    footer_start
  )
  
  page_href <- trimws(page_href)
  page_text <- trimws(page_text)
  
  if (nzchar(page_href)) {
    if (!nzchar(page_text)) {
      stop("The selected page has no display title.")
    }
    
    escaped_href <- as.character(
      htmltools::htmlEscape(
        page_href,
        attribute = TRUE
      )
    )
    
    escaped_text <- as.character(
      htmltools::htmlEscape(
        page_text,
        attribute = FALSE
      )
    )
    
    footer_indent <- sub(
      "^(\\s*).*",
      "\\1",
      html_lines[footer_start]
    )
    
    content_line <- paste0(
      footer_indent,
      "    <a href=\"",
      escaped_href,
      "\">",
      escaped_text,
      "</a>"
    )
  } else {
    content_line <- character()
  }
  
  opening_line <- sub(
    "^(.*?<div\\b[^>]*>).*",
    "\\1",
    html_lines[footer_start],
    perl = TRUE
  )
  
  closing_indent <- sub(
    "^(\\s*).*",
    "\\1",
    html_lines[footer_start]
  )
  
  replacement <- c(
    opening_line,
    content_line,
    paste0(
      closing_indent,
      "</div>"
    )
  )
  
  updated_html <- c(
    if (footer_start > 1) {
      html_lines[seq_len(footer_start - 1)]
    } else {
      character()
    },
    
    replacement,
    
    if (footer_end < length(html_lines)) {
      html_lines[
        seq.int(
          footer_end + 1,
          length(html_lines)
        )
      ]
    } else {
      character()
    }
  )
  
  writeLines(
    updated_html,
    index_html_path,
    useBytes = TRUE
  )
  
  invisible(index_html_path)
}

parse_homepage_year_tags <- function(text) {
  
  text <- trimws(
    as.character(text)
  )
  
  placeholders <- c(
    "<<latest-year>>" = "___HOMEPAGE_LATEST_YEAR___",
    "<<last-year>>" = "___HOMEPAGE_LAST_YEAR___",
    "<<first-year>>" = "___HOMEPAGE_FIRST_YEAR___"
  )
  
  for (tag in names(placeholders)) {
    text <- gsub(
      pattern = tag,
      replacement = placeholders[[tag]],
      x = text,
      fixed = TRUE
    )
  }
  
  # Escape all other user-entered HTML.
  text <- as.character(
    htmltools::htmlEscape(
      text,
      attribute = FALSE
    )
  )
  
  replacements <- c(
    "___HOMEPAGE_LATEST_YEAR___" =
      '<span class="latest-year"></span>',
    "___HOMEPAGE_LAST_YEAR___" =
      '<span class="last-year"></span>',
    "___HOMEPAGE_FIRST_YEAR___" =
      '<span class="first-year"></span>'
  )
  
  for (placeholder in names(replacements)) {
    text <- gsub(
      pattern = placeholder,
      replacement = replacements[[placeholder]],
      x = text,
      fixed = TRUE
    )
  }
  
  text
}

update_homepage_card_body <- function(
    project_root,
    card_number,
    top_line = "",
    unit = "",
    bottom_line = ""
) {
  index_html_path <- file.path(
    project_root,
    "index.html"
  )
  
  if (!file.exists(index_html_path)) {
    stop("index.html was not found.")
  }
  
  card_number <- as.integer(card_number)
  
  if (
    length(card_number) != 1 ||
    is.na(card_number) ||
    card_number < 1
  ) {
    stop("A valid card number is required.")
  }
  
  html_lines <- readLines(
    index_html_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  card_section <- find_homepage_cards_row(
    html_lines
  )
  
  if (card_number > length(card_section$card_starts)) {
    stop(
      paste0(
        "Card ",
        card_number,
        " was not found."
      )
    )
  }
  
  card_start <- card_section$card_starts[card_number]
  
  card_end <- find_closing_div(
    html_lines,
    card_start
  )
  
  body_candidates <- grep(
    'class=["\'][^"\']*\\bcard-body\\b',
    html_lines,
    perl = TRUE
  )
  
  body_candidates <- body_candidates[
    body_candidates >= card_start &
      body_candidates <= card_end
  ]
  
  if (length(body_candidates) != 1) {
    stop(
      paste0(
        "Could not uniquely identify the body for card ",
        card_number,
        "."
      )
    )
  }
  
  body_start <- body_candidates[1]
  
  body_end <- find_closing_div(
    html_lines,
    body_start
  )
  
  top_line <- trimws(top_line)
  unit <- trimws(unit)
  bottom_line <- trimws(bottom_line)
  
  parsed_top_line <- parse_homepage_year_tags(
    top_line
  )
  
  escaped_unit <- as.character(
    htmltools::htmlEscape(
      unit,
      attribute = FALSE
    )
  )
  
  parsed_bottom_line <- parse_homepage_year_tags(
    bottom_line
  )
  
  opening_line <- sub(
    "^(.*?<div\\b[^>]*>).*",
    "\\1",
    html_lines[body_start],
    perl = TRUE
  )
  
  body_indent <- sub(
    "^(\\s*).*",
    "\\1",
    html_lines[body_start]
  )
  
  content_indent <- paste0(
    body_indent,
    "    "
  )
  
  value_span <- paste0(
    '<span class="display-4" id="headline-',
    card_number,
    '-value"></span>'
  )
  
  unit_span <- if (nzchar(unit)) {
    paste0(
      '<span class="display-4 unit">',
      escaped_unit,
      "</span>"
    )
  } else {
    ""
  }
  
  body_content <- c(
    paste0(
      content_indent,
      "<p>",
      parsed_top_line
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
      parsed_bottom_line
    ),
    paste0(
      content_indent,
      "</p>"
    )
  )
  
  replacement <- c(
    opening_line,
    body_content,
    paste0(
      body_indent,
      "</div>"
    )
  )
  
  updated_html <- c(
    if (body_start > 1) {
      html_lines[seq_len(body_start - 1)]
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
    index_html_path,
    useBytes = TRUE
  )
  
  invisible(index_html_path)
}

escape_javascript_string <- function(value) {
  
  value <- as.character(value)
  
  value <- gsub(
    "\\\\",
    "\\\\\\\\",
    value
  )
  
  value <- gsub(
    '"',
    '\\\\"',
    value
  )
  
  value <- gsub(
    "\r",
    "\\\\r",
    value
  )
  
  value <- gsub(
    "\n",
    "\\\\n",
    value
  )
  
  value
}


javascript_string <- function(value) {
  
  paste0(
    '"',
    escape_javascript_string(value),
    '"'
  )
}

javascript_filter_value <- function(
    value,
    is_year = FALSE
) {
  value <- as.character(value)[1]
  
  if (isTRUE(is_year)) {
    if (identical(value, "__LATEST_YEAR__")) {
      return("latest_year")
    }
    
    if (identical(value, "__PREVIOUS_YEAR__")) {
      return("last_year")
    }
    
    if (identical(value, "__EARLIEST_YEAR__")) {
      return("first_year")
    }
  }
  
  javascript_string(value)
}

update_homepage_card_value_js <- function(
    project_root,
    card_number,
    calculation
) {
  
  js_path <- file.path(
    project_root,
    "src",
    "index.js"
  )
  
  if (!file.exists(js_path)) {
    stop("src/index.js was not found.")
  }
  
  card_number <- as.integer(card_number)
  
  if (
    length(card_number) != 1 ||
    is.na(card_number) ||
    card_number < 1
  ) {
    stop("A valid card number is required.")
  }
  
  if (
    is.null(calculation) ||
    is.null(calculation$matrix) ||
    !nzchar(calculation$matrix)
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
  
  if (length(current_marker) != 1) {
    stop(
      paste0(
        "Could not identify the JavaScript section for card ",
        card_number,
        "."
      )
    )
  }
  
  all_markers <- grep(
    "^\\s*//\\s*Content for card\\s+[0-9]+\\s*$",
    js_lines,
    perl = TRUE
  )
  
  next_markers <- all_markers[
    all_markers > current_marker
  ]
  
  closing_lines <- grep(
    "^\\s*\\}\\)\\s*;?\\s*$",
    js_lines
  )
  
  if (length(closing_lines) == 0) {
    stop(
      "Could not identify the end of the event listener."
    )
  }
  
  section_end <- if (length(next_markers) > 0) {
    min(next_markers) - 1
  } else {
    tail(closing_lines, 1) - 1
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
    "headline_",
    card_number,
    "_raw"
  )
  
  headline_variable <- paste0(
    "headline_",
    card_number
  )
  
  element_id <- paste0(
    "headline-",
    card_number,
    "-value"
  )
  
  load_lines <- c(
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
  
  filter_lines <- character()
  
  if (length(js_filters) > 0) {
    
    filter_conditions <- character()
    
    for (column_name in names(js_filters)) {
      filter_definition <- js_filters[[
        column_name
      ]]
      
      # Supports both the new structured format and older saved values.
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
        "]);"
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
      "        .reduce((sum, value) => sum + (Number(value) || 0), 0);",
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
      "        .reduce((sum, value) => sum + (Number(value) || 0), 0);",
      ""
    )
  }
  
  if (comma_separator) {
    
    formatting_lines <- c(
      paste0(
        "    const ",
        headline_variable,
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
      headline_variable,
      " = ",
      raw_variable,
      ".toFixed(",
      decimal_places,
      ");"
    )
    
  } else {
    
    formatting_lines <- paste0(
      "    const ",
      headline_variable,
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
      headline_variable,
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

restore_homepage_year_tags <- function(html) {
  
  html <- gsub(
    '<span\\s+class=["\']latest-year["\']\\s*></span>',
    "<<latest-year>>",
    html,
    perl = TRUE
  )
  
  html <- gsub(
    '<span\\s+class=["\']last-year["\']\\s*></span>',
    "<<last-year>>",
    html,
    perl = TRUE
  )
  
  html <- gsub(
    '<span\\s+class=["\']first-year["\']\\s*></span>',
    "<<first-year>>",
    html,
    perl = TRUE
  )
  
  html
}


html_fragment_to_text <- function(html) {
  
  html <- trimws(html)
  
  if (!nzchar(html)) {
    return("")
  }
  
  html <- restore_homepage_year_tags(html)
  
  document <- xml2::read_html(
    paste0(
      "<div>",
      html,
      "</div>"
    )
  )
  
  trimws(
    xml2::xml_text(document)
  )
}


read_homepage_card_values <- function(project_root) {
  
  index_html_path <- file.path(
    project_root,
    "index.html"
  )
  
  if (!file.exists(index_html_path)) {
    stop("index.html was not found.")
  }
  
  display_values <- read_homepage_card_display_values(
    project_root
  )
  
  html_lines <- readLines(
    index_html_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  card_section <- find_homepage_cards_row(
    html_lines
  )
  
  lapply(
    seq_along(card_section$card_starts),
    function(card_number) {
      
      card_start <- card_section$card_starts[
        card_number
      ]
      
      card_end <- find_closing_div(
        html_lines,
        card_start
      )
      
      card_html <- paste(
        html_lines[card_start:card_end],
        collapse = "\n"
      )
      
      document <- xml2::read_html(
        paste0(
          "<html><body>",
          card_html,
          "</body></html>"
        )
      )
      
      card_body <- xml2::xml_find_first(
        document,
        paste0(
          "//div[contains(concat(' ', ",
          "normalize-space(@class), ' '), ",
          "' card-body ')]"
        )
      )
      
      card_footer <- xml2::xml_find_first(
        document,
        paste0(
          "//div[contains(concat(' ', ",
          "normalize-space(@class), ' '), ",
          "' card-footer ')]"
        )
      )
      
      paragraph <- xml2::xml_find_first(
        card_body,
        ".//p"
      )
      
      value_span <- xml2::xml_find_first(
        card_body,
        paste0(
          ".//span[@id='headline-",
          card_number,
          "-value']"
        )
      )
      
      unit_span <- xml2::xml_find_first(
        card_body,
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
        
        value_position <- regexpr(
          value_html,
          paragraph_html,
          fixed = TRUE
        )
        
        if (value_position[1] != -1) {
          
          before_value <- substr(
            paragraph_html,
            1,
            value_position[1] - 1
          )
          
          after_value <- substr(
            paragraph_html,
            value_position[1] +
              attr(
                value_position,
                "match.length"
              ),
            nchar(paragraph_html)
          )
          
          # Remove the opening <p> and the break before the value.
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
          
          # Remove the optional unit, break and closing </p>.
          if (!inherits(unit_span, "xml_missing")) {
            after_value <- sub(
              fixed = TRUE,
              pattern = as.character(unit_span),
              replacement = "",
              x = after_value
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
      
      footer_link <- xml2::xml_find_first(
        card_footer,
        ".//a"
      )
      
      page_href <- if (
        inherits(footer_link, "xml_missing")
      ) {
        ""
      } else {
        xml2::xml_attr(
          footer_link,
          "href"
        )
      }
      
      if (
        is.na(page_href) ||
        is.null(page_href)
      ) {
        page_href <- ""
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
        page_href = page_href
      )
    }
  )
}

read_homepage_card_display_values <- function(project_root) {
  
  js_path <- file.path(
    project_root,
    "src",
    "index.js"
  )
  
  if (!file.exists(js_path)) {
    return(character())
  }
  
  js_lines <- readLines(
    js_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  card_markers <- grep(
    "^\\s*//\\s*Content for card\\s+[0-9]+\\s*$",
    js_lines
  )
  
  if (length(card_markers) == 0) {
    return(character())
  }
  
  closing_lines <- grep(
    "^\\s*\\}\\)\\s*;?\\s*$",
    js_lines
  )
  
  if (length(closing_lines) == 0) {
    return(character())
  }
  
  event_end <- tail(
    closing_lines,
    1
  )
  
  values <- character(
    length(card_markers)
  )
  
  for (i in seq_along(card_markers)) {
    
    section_start <- card_markers[i]
    
    section_end <- if (i < length(card_markers)) {
      card_markers[i + 1] - 1
    } else {
      event_end - 1
    }
    
    section <- js_lines[
      section_start:section_end
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
    } else {
      values[i] <- ""
    }
  }
  
  values
}