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
  
  # Clear index.js after the marker.
  marker_text <- "// Insert values into homepage cards below"
  
  marker_line <- grep(
    marker_text,
    original_js,
    fixed = TRUE
  )
  
  if (length(marker_line) != 1) {
    stop(
      paste0(
        "Could not uniquely identify the JavaScript marker: ",
        marker_text
      )
    )
  }
  
  closing_line <- grep(
    "^\\s*\\}\\)\\s*;?\\s*$",
    original_js
  )
  
  if (length(closing_line) == 0) {
    stop(
      paste(
        "Could not identify the closing line of the",
        "DOMContentLoaded event listener."
      )
    )
  }
  
  closing_line <- tail(
    closing_line,
    1
  )
  
  if (closing_line <= marker_line) {
    stop(
      "The JavaScript closing line occurs before the marker."
    )
  }
  
  updated_js <- c(
    original_js[seq_len(marker_line)],
    "",
    original_js[closing_line]
  )
  
  tryCatch(
    {
      writeLines(
        updated_html,
        index_html_path,
        useBytes = TRUE
      )
      
      writeLines(
        updated_js,
        index_js_path,
        useBytes = TRUE
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