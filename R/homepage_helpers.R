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