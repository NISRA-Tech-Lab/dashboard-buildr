library(shiny)
library(shinyFiles)
library(servr)
library(shinyjs)
library(dplyr)
library(V8)
library(DT)
library(shinythemes)

source("R/file_helpers.R", local = TRUE)
source("R/config_helpers.R", local = TRUE)
source("R/page_helpers.R", local = TRUE)
source("R/matrix_helpers.R", local = TRUE)
source("R/homepage_helpers.R", local = TRUE)

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  useShinyjs(),
  titlePanel("NISRA Dashboard BuildR"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Choose dashboard directory"),
      shinyDirButton(
        id = "folder",
        label = "Browse",
        title = "Choose a folder"
      ),
      hidden(
        div(
          id = "launch-controls",
          h4("Dashboard location"),
          verbatimTextOutput("path"),
          p("Click below to open dashboard in new tab"),
          actionButton(
            "launch-dashboard",
            "Launch dashboard"
          ),
          uiOutput("github_origin")
        )
      ),
      tags$hr(),
      
      uiOutput("loaded_tables_ui")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dashboard settings",
                 h2("Dashboard settings"),
                 DT::dataTableOutput("config_table")),
        tabPanel(
          "Home page design",
          
          h2("Home page design"),
          
          tags$hr(),
          
          h3("Clear content"),
          
          tags$p(
            paste(
              "Clear the example homepage cards, headings, links",
              "and example JavaScript values."
            )
          ),
          
          actionButton(
            inputId = "clear_homepage_content",
            label = "Clear content",
            icon = icon("eraser"),
            class = "btn-danger"
          ),
          
          tags$hr(),
          
          h3("Edit strapline"),
          
          tags$p(
            paste(
              "Enter a short line describing the source of the dashboard data,",
              "for example: This dashboard was built using data from the NISRA Data Portal."
            )
          ),
          
          textInput(
            inputId = "homepage_strapline",
            label = "Homepage strapline",
            value = "",
            width = "100%",
            placeholder = "This dashboard was built using data from..."
          ),
          
          actionButton(
            inputId = "save_homepage_strapline",
            label = "Save strapline",
            icon = icon("save"),
            class = "btn-primary"
          ),
          
          tags$hr(),
          
          h3("Edit cards"),
          
          tags$p(
            paste(
              "Use homepage cards to display one key message for each",
              "dashboard page. You can include between 1 and 9 cards."
            )
          ),
          
          tags$div(
            style = paste(
              "display: flex;",
              "align-items: flex-end;",
              "gap: 10px;",
              "max-width: 360px;"
            ),
            
            tags$div(
              style = "flex: 1;",
              
              numericInput(
                inputId = "homepage_card_count",
                label = "Number of cards",
                value = 6,
                min = 1,
                max = 9,
                step = 1,
                width = "100%"
              )
            ),
            
            actionButton(
              inputId = "save_homepage_card_count",
              label = "Save",
              icon = icon("save"),
              class = "btn-primary"
            )
          )
        ),
        tabPanel("Page design"),
        tabPanel("User notes")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Select a folder to open dashboard ####
  volumes <- c(Home = fs::path_home())
  
  shinyDirChoose(
    input,
    id = "folder",
    roots = volumes,
    session = session
  )
  
  folder <- reactive({
    req(input$folder)
    
    selected_path <- parseDirPath(
      roots = volumes,
      selection = input$folder
    )
    
    req(length(selected_path) > 0)
    
    normalizePath(
      selected_path[[1]],
      winslash = "/",
      mustWork = TRUE
    )
  })
  
  github_origin <- reactive({
    req(folder())
    
    result <- tryCatch(
      system2(
        command = "git",
        args = c(
          "-C",
          shQuote(folder()),
          "remote",
          "get-url",
          "origin"
        ),
        stdout = TRUE,
        stderr = TRUE
      ),
      error = function(error) {
        character()
      }
    )
    
    status <- attr(result, "status")
    
    if (
      length(result) == 0 ||
      (!is.null(status) && status != 0)
    ) {
      return(NULL)
    }
    
    remote_url <- trimws(result[1])
    
    if (!nzchar(remote_url)) {
      return(NULL)
    }
    
    # Convert GitHub SSH remotes:
    # git@github.com:organisation/repository.git
    # into:
    # https://github.com/organisation/repository
    if (grepl("^git@github\\.com:", remote_url)) {
      remote_url <- sub(
        "^git@github\\.com:",
        "https://github.com/",
        remote_url
      )
    }
    
    # Convert ssh://git@github.com/organisation/repository.git
    if (grepl("^ssh://git@github\\.com/", remote_url)) {
      remote_url <- sub(
        "^ssh://git@github\\.com/",
        "https://github.com/",
        remote_url
      )
    }
    
    remote_url <- sub(
      "\\.git$",
      "",
      remote_url
    )
    
    remote_url
  })
  
  output$github_origin <- renderUI({
    origin <- github_origin()
    
    tags$div(
      style = "margin-top: 12px; margin-bottom: 12px;",
      
      tags$strong("GitHub repository"),
      
      tags$br(),
      
      if (is.null(origin)) {
        tags$em(
          "No Git origin was found."
        )
      } else if (grepl("^https?://github\\.com/", origin)) {
        tags$a(
          href = origin,
          target = "_blank",
          rel = "noopener noreferrer",
          sub("^https?://", "", origin)
        )
      } else {
        tags$span(origin)
      }
    )
  })
  
  output$path <- renderText({
    folder()
  })
  
  observeEvent(input$folder, {
    shinyjs::show("launch-controls")
  }, ignoreInit = TRUE)
  
  
  # Open dashboard preview in new tab ####
  observeEvent(input$`launch-dashboard`, {
    req(folder())
    
    port <- 4321
    
    servr::daemon_stop()
    
    servr::httd(
      dir = folder(),
      port = port,
      daemon = TRUE,
      browse = FALSE
    )
    
    browseURL(sprintf("http://127.0.0.1:%d", port))
  })
  
  # Loaded tables ####
  # Loaded data tables ####
  
  loaded_tables_version <- reactiveVal(0)
  
  selected_loaded_table <- reactiveVal(NULL)
  
  
  loaded_table_files <- reactive({
    req(folder())
    
    # Allows this list to be refreshed after data.R runs.
    loaded_tables_version()
    
    data_directory <- file.path(
      folder(),
      "public",
      "data"
    )
    
    if (!dir.exists(data_directory)) {
      return(character())
    }
    
    files <- list.files(
      path = data_directory,
      pattern = "\\.csv$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    sort(files)
  })
  
  
  loaded_table_metadata <- reactive({
    req(folder())
    
    loaded_tables_version()
    
    metadata_path <- file.path(
      folder(),
      "public",
      "data",
      "data.json"
    )
    
    if (!file.exists(metadata_path)) {
      return(list())
    }
    
    # Treat an empty file as "no metadata yet".
    if (file.info(metadata_path)$size == 0) {
      return(list())
    }
    
    metadata_text <- paste(
      readLines(
        metadata_path,
        warn = FALSE,
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )
    
    # Ignore files containing only whitespace.
    if (!nzchar(trimws(metadata_text))) {
      return(list())
    }
    
    tryCatch(
      jsonlite::fromJSON(
        txt = metadata_text,
        simplifyVector = FALSE
      ),
      error = function(error) {
        
        showNotification(
          paste(
            "Could not read public/data/data.json:",
            error$message
          ),
          type = "error",
          duration = NULL
        )
        
        list()
      }
    )
  })
  
  
  output$loaded_tables_ui <- renderUI({
    
    files <- loaded_table_files()
    metadata <- loaded_table_metadata()
    
    tagList(
      h4("Tables loaded"),
      
      if (length(files) == 0) {
        
        tags$em(
          "No tables have been loaded yet."
        )
        
      } else {
        
        tags$div(
          class = "loaded-tables-list",
          
          lapply(files, function(file_path) {
            
            matrix <- tools::file_path_sans_ext(
              basename(file_path)
            )
            
            table_metadata <- metadata[[matrix]]
            
            table_label <- if (
              !is.null(table_metadata) &&
              !is.null(table_metadata$label) &&
              length(table_metadata$label) > 0 &&
              nzchar(as.character(table_metadata$label)[1])
            ) {
              as.character(table_metadata$label)[1]
            } else {
              matrix
            }
            
            display_name <- paste0(
              table_label,
              " (",
              matrix,
              ")"
            )
            
            tags$div(
              style = paste(
                "display: flex;",
                "align-items: center;",
                "justify-content: space-between;",
                "gap: 10px;",
                "margin-bottom: 8px;"
              ),
              
              tags$span(
                display_name,
                title = display_name,
                style = paste(
                  "overflow-wrap: anywhere;",
                  "line-height: 1.25;",
                  "flex: 1;",
                  "min-width: 0;"
                )
              ),
              
              tags$button(
                type = "button",
                class = "btn btn-default btn-sm",
                title = paste0(
                  "View ",
                  matrix
                ),
                style = "flex-shrink: 0;",
                
                onclick = sprintf(
                  paste0(
                    "Shiny.setInputValue(",
                    "'view_loaded_table', ",
                    "{matrix: %s, nonce: Math.random()}, ",
                    "{priority: 'event'}",
                    ");"
                  ),
                  jsonlite::toJSON(
                    matrix,
                    auto_unbox = TRUE
                  )
                ),
                
                icon("eye"),
                " View"
              )
            )
          })
        )
      }
    )
  })
  
  
  observeEvent(input$view_loaded_table, {
    req(input$view_loaded_table$matrix)
    
    matrix <- as.character(
      input$view_loaded_table$matrix
    )
    
    available_matrices <- tools::file_path_sans_ext(
      basename(loaded_table_files())
    )
    
    if (!matrix %in% available_matrices) {
      showNotification(
        "The selected table could not be found.",
        type = "error"
      )
      
      return()
    }
    
    selected_loaded_table(matrix)
    
    metadata <- loaded_table_metadata()
    table_metadata <- metadata[[matrix]]
    
    table_label <- if (
      !is.null(table_metadata) &&
      !is.null(table_metadata$label) &&
      length(table_metadata$label) > 0 &&
      nzchar(as.character(table_metadata$label)[1])
    ) {
      as.character(table_metadata$label)[1]
    } else {
      "Data table"
    }
    
    modal_title <- paste0(
      matrix,
      " - ",
      table_label
    )
    
    showModal(
      modalDialog(
        title = modal_title,
        
        tags$div(
          style = paste(
            "width: 100%;",
            "overflow: hidden;"
          ),
          
          DT::DTOutput("loaded_table_view")
        ),
        
        tags$div(
          style = paste(
            "margin-top: 12px;",
            "font-size: 0.9em;"
          ),
          
          uiOutput("loaded_table_updated")
        ),
        
        footer = modalButton("Close"),
        
        size = "l",
        easyClose = TRUE
      )
    )
  })
  
  
  output$loaded_table_view <- DT::renderDT({
    matrix <- selected_loaded_table()
    
    req(matrix)
    req(folder())
    
    csv_path <- file.path(
      folder(),
      "public",
      "data",
      paste0(matrix, ".csv")
    )
    
    validate(
      need(
        file.exists(csv_path),
        paste0(
          "The CSV file could not be found: ",
          basename(csv_path)
        )
      )
    )
    
    table_data <- read.csv(
      csv_path,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      encoding = "UTF-8"
    )
    
    DT::datatable(
      table_data,
      rownames = FALSE,
      selection = "none",
      filter = "none",
      class = "cell-border compact stripe",
      options = list(
        paging = FALSE,
        searching = FALSE,
        ordering = TRUE,
        info = FALSE,
        scrollX = TRUE,
        scrollY = "60vh",
        scrollCollapse = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  
  output$loaded_table_updated <- renderUI({
    matrix <- selected_loaded_table()
    
    req(matrix)
    
    metadata <- loaded_table_metadata()
    table_metadata <- metadata[[matrix]]
    
    updated_value <- if (
      !is.null(table_metadata) &&
      !is.null(table_metadata$updated) &&
      length(table_metadata$updated) > 0
    ) {
      as.character(table_metadata$updated)[1]
    } else {
      NA_character_
    }
    
    updated_date <- suppressWarnings(
      as.Date(updated_value)
    )
    
    if (is.na(updated_date)) {
      tags$span(
        tags$strong("Updated: "),
        "Date unavailable"
      )
    } else {
      tags$span(
        tags$strong("Updated: "),
        format(
          updated_date,
          "%d/%m/%Y"
        )
      )
    }
  })
  
  # Dashboard settings tab ####
  
  ## Read in config file ####
  config_version <- reactiveVal(0)
  
  config_file <- reactive({
    req(input$folder)
    
    # Re-read the file after it has been updated
    config_version()
    
    config_path <- file.path(
      folder(),
      "src",
      "config",
      "config.js"
    )
    
    validate(
      need(file.exists(config_path), "No config.js file was found.")
    )
    
    config_text <- readLines(config_path, warn = FALSE) |>
      paste(collapse = "\n")
    
    config_text <- sub(
      pattern = "^\\s*export\\s+const\\s+config\\s*=",
      replacement = "var config =",
      x = config_text
    )
    
    ctx <- V8::v8()
    ctx$eval(config_text)
    
    ctx$get("config")
  })
  
  ## Output config information in app
  config_table <- reactive({
    config <- config_file()
    
    format_value <- function(value) {
      
      if (is.null(value)) {
        return("")
      }
      
      if (is.data.frame(value)) {
        rows <- apply(value, 1, function(row) {
          paste(
            paste(names(row), row, sep = ": "),
            collapse = ", "
          )
        })
        
        return(paste(rows, collapse = "<br>"))
      }
      
      if (is.list(value)) {
        entries <- vapply(
          value,
          function(item) {
            
            if (is.null(item)) {
              return("")
            }
            
            if (is.data.frame(item)) {
              rows <- apply(item, 1, function(row) {
                paste(
                  paste(names(row), row, sep = ": "),
                  collapse = ", "
                )
              })
              
              return(paste(rows, collapse = "<br>"))
            }
            
            if (is.list(item)) {
              return(
                paste(
                  paste(
                    names(item),
                    unlist(item, use.names = FALSE),
                    sep = ": "
                  ),
                  collapse = ", "
                )
              )
            }
            
            paste(item, collapse = ", ")
          },
          character(1)
        )
        
        return(paste(entries, collapse = "<br>"))
      }
      
      if (length(value) > 1) {
        return(paste(value, collapse = "<br>"))
      }
      
      as.character(value)
    }
    
    setting_names <- c(
      "Dashboard Title",
      "Pages",
      "portal_url",
      "Department",
      "Data Portal Tables",
      "RateIt link"
    )
    
    content <- vapply(
      names(config),
      function(config_name) {
        
        value <- config[[config_name]]
        
        if (config_name == "navigation") {
          
          default_hrefs <- c(
            "index.html",
            "page.html",
            "user-notes.html"
          )
          
          pages <- value |>
            dplyr::filter(!href %in% default_hrefs)
          
          if (nrow(pages) == 0) {
            return(
              paste0(
                "<em>",
                "No pages yet click Edit to start adding dashboard pages",
                "</em>"
              )
            )
          }
          
          return(
            paste(
              pages$text,
              collapse = "<br>"
            )
          )
        }
        
        if (config_name == "matrix") {
          
          matrices <- value
          
          matrices <- matrices[
            !matrices %in% c(
              "EXAMPLETABLE1",
              "EXAMPLETABLE2"
            )
          ]
          
          if (length(matrices) == 0) {
            return(
              paste0(
                "<em>",
                "No Data Portal tables yet.",
                "</em>"
              )
            )
          }
          
          return(
            paste(
              matrices,
              collapse = "<br>"
            )
          )
        }
        
        format_value(value)
      },
      character(1)
    )
    
    result <- data.frame(
      Setting = setting_names,
      Content = content,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ) |>
      filter(Setting != "portal_url")
    
    result$Edit <- vapply(
      seq_len(nrow(result)),
      function(i) {
        sprintf(
          paste0(
            '<button class="btn btn-primary btn-sm" ',
            'onclick="Shiny.setInputValue(\'edit_setting\', %d, ',
            '{priority: \'event\'})">Edit</button>'
          ),
          i
        )
      },
      character(1)
    )
    
    result
  })
  
  output$config_table <- DT::renderDataTable({
    DT::datatable(
      config_table(),
      escape = FALSE,
      rownames = FALSE,
      selection = "none",
      options = list(
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        info = FALSE
      )
    )
  })
  
  ## Edit the dashboard title ####
  ### Editing panel ####
  observeEvent(input$edit_setting, {
    req(input$edit_setting)
    
    # Only handle the first row for now
    req(input$edit_setting == 1)
    
    config <- config_file()
    
    showModal(
      modalDialog(
        title = "Edit dashboard title",
        
        textInput(
          inputId = "new_dashboard_title",
          label = "Dashboard title",
          value = config$title,
          width = "100%"
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            inputId = "save_dashboard_title",
            label = "Save",
            class = "btn-primary"
          )
        ),
        
        easyClose = FALSE
      )
    )
  })
  
  ### Saving changes to title ####
  observeEvent(input$save_dashboard_title, {
    req(folder())
    req(input$new_dashboard_title)
    
    new_title <- trimws(input$new_dashboard_title)
    
    validate(
      need(nzchar(new_title), "The dashboard title cannot be empty.")
    )
    
    config_path <- file.path(
      folder(),
      "src",
      "config",
      "config.js"
    )
    
    validate(
      need(file.exists(config_path), "No config.js file was found.")
    )
    
    config_lines <- readLines(
      config_path,
      warn = FALSE,
      encoding = "UTF-8"
    )
    
    title_line <- grep(
      '^\\s*["\']title["\']\\s*:',
      config_lines
    )
    
    validate(
      need(
        length(title_line) == 1,
        'Could not uniquely identify the "title" property.'
      )
    )
    
    # Escape characters that have a special meaning in JavaScript strings
    escaped_title <- new_title |>
      gsub("\\\\", "\\\\\\\\", x = _) |>
      gsub('"', '\\\\"', x = _) |>
      gsub("\r", "\\\\r", x = _) |>
      gsub("\n", "\\\\n", x = _)
    
    existing_indent <- sub(
      '^(\\s*).*',
      '\\1',
      config_lines[title_line]
    )
    
    has_comma <- grepl(
      ',\\s*$',
      config_lines[title_line]
    )
    
    config_lines[title_line] <- paste0(
      existing_indent,
      '"title": "',
      escaped_title,
      '"',
      if (has_comma) "," else ""
    )
    
    writeLines(
      config_lines,
      config_path,
      useBytes = TRUE
    )
    
    # Force config_file() and the table to re-read config.js
    config_version(config_version() + 1)
    
    removeModal()
    
    showNotification(
      "Dashboard title updated.",
      type = "message"
    )
  })
  
  ## Edit the RateIt link ####
  observeEvent(input$edit_setting, {
    req(input$edit_setting == 5)
    
    config <- config_file()
    
    showModal(
      modalDialog(
        title = "Update RateIt link",
        
        textInput(
          inputId = "new_rateit_link",
          label = "RateIt link",
          value = config$rateit,
          width = "100%"
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            inputId = "save_rateit_link",
            label = "Save",
            class = "btn-primary"
          )
        ),
        
        easyClose = FALSE
      )
    )
  })
  
  observeEvent(input$save_rateit_link, {
    req(folder())
    
    new_rateit_link <- trimws(input$new_rateit_link)
    
    if (!nzchar(new_rateit_link)) {
      showNotification(
        "The RateIt link cannot be empty.",
        type = "error"
      )
      return()
    }
    
    config_path <- file.path(
      folder(),
      "src",
      "config",
      "config.js"
    )
    
    if (!file.exists(config_path)) {
      showNotification(
        "No config.js file was found.",
        type = "error"
      )
      return()
    }
    
    config_lines <- readLines(
      config_path,
      warn = FALSE,
      encoding = "UTF-8"
    )
    
    rateit_line <- grep(
      '^\\s*["\']rateit["\']\\s*:',
      config_lines
    )
    
    if (length(rateit_line) != 1) {
      showNotification(
        'Could not uniquely identify the "rateit" property.',
        type = "error",
        duration = NULL
      )
      return()
    }
    
    escaped_link <- new_rateit_link |>
      gsub("\\\\", "\\\\\\\\", x = _) |>
      gsub('"', '\\\\"', x = _) |>
      gsub("\r", "\\\\r", x = _) |>
      gsub("\n", "\\\\n", x = _)
    
    existing_indent <- sub(
      '^(\\s*).*',
      '\\1',
      config_lines[rateit_line]
    )
    
    has_comma <- grepl(
      ',\\s*$',
      config_lines[rateit_line]
    )
    
    config_lines[rateit_line] <- paste0(
      existing_indent,
      '"rateit": "',
      escaped_link,
      '"',
      if (has_comma) "," else ""
    )
    
    writeLines(
      config_lines,
      config_path,
      useBytes = TRUE
    )
    
    config_version(
      config_version() + 1
    )
    
    removeModal()
    
    showNotification(
      "RateIt link updated.",
      type = "message"
    )
  })
  
  ### Create, rename, delete and reorder pages ####
  #### Draft object of changes to pages ####
  pages_draft <- reactiveVal(
    data.frame(
      href = character(),
      text = character(),
      original_href = character(),
      stringsAsFactors = FALSE
    )
  )
  
  editing_page_row <- reactiveVal(NULL)
  
  initialise_pages_draft <- function() {
    
    navigation <- config_file()$navigation
    
    default_hrefs <- c(
      "index.html",
      "page.html",
      "user-notes.html"
    )
    
    pages <- navigation[
      !navigation$href %in% default_hrefs,
      c("href", "text"),
      drop = FALSE
    ]
    
    pages$original_href <- pages$href
    
    pages_draft(pages)
  }
  
  #### Display pages edit screen ####
  show_pages_modal <- function() {
    
    showModal(
      modalDialog(
        title = "Edit dashboard pages",
        
        actionButton(
          "add_page",
          label = NULL,
          icon = icon("plus"),
          class = "btn-success",
          title = "Add page"
        ),
        
        tags$div(
          style = "margin-top: 15px;",
          DT::DTOutput("pages_editor_table")
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            "save_pages",
            "Save",
            class = "btn-primary"
          )
        ),
        
        size = "l",
        easyClose = FALSE
      )
    )
  }
  
  observeEvent(input$edit_setting, {
    
    req(input$edit_setting == 2)
    
    initialise_pages_draft()
    show_pages_modal()
  })
  
  output$pages_editor_table <- DT::renderDT({
    
    pages <- pages_draft()
    
    if (nrow(pages) == 0) {
      
      display <- data.frame(
        Page = paste0(
          "<em>",
          "No pages yet. Click + to start adding dashboard pages.",
          "</em>"
        ),
        Actions = "",
        stringsAsFactors = FALSE
      )
      
    } else {
      
      display <- data.frame(
        Page = pages$text,
        Actions = vapply(
          seq_len(nrow(pages)),
          function(i) {
            
            up_disabled <- if (i == 1) {
              "disabled"
            } else {
              ""
            }
            
            down_disabled <- if (i == nrow(pages)) {
              "disabled"
            } else {
              ""
            }
            
            sprintf(
              paste0(
                '<div class="btn-group" role="group">',
                
                '<button class="btn btn-default btn-sm" %s ',
                'title="Move up" ',
                'onclick="Shiny.setInputValue(',
                '\'page_action\', ',
                '{action:\'up\', row:%d, nonce:Math.random()}, ',
                '{priority:\'event\'})">',
                '<i class="fa fa-arrow-up"></i>',
                '</button>',
                
                '<button class="btn btn-default btn-sm" %s ',
                'title="Move down" ',
                'onclick="Shiny.setInputValue(',
                '\'page_action\', ',
                '{action:\'down\', row:%d, nonce:Math.random()}, ',
                '{priority:\'event\'})">',
                '<i class="fa fa-arrow-down"></i>',
                '</button>',
                
                '<button class="btn btn-primary btn-sm" ',
                'title="Rename" ',
                'onclick="Shiny.setInputValue(',
                '\'page_action\', ',
                '{action:\'rename\', row:%d, nonce:Math.random()}, ',
                '{priority:\'event\'})">',
                '<i class="fa fa-pencil"></i>',
                '</button>',
                
                '<button class="btn btn-danger btn-sm" ',
                'title="Delete" ',
                'onclick="Shiny.setInputValue(',
                '\'page_action\', ',
                '{action:\'delete\', row:%d, nonce:Math.random()}, ',
                '{priority:\'event\'})">',
                '<i class="fa fa-trash"></i>',
                '</button>',
                
                '</div>'
              ),
              up_disabled,
              i,
              down_disabled,
              i,
              i,
              i
            )
          },
          character(1)
        ),
        stringsAsFactors = FALSE
      )
    }
    
    DT::datatable(
      display,
      escape = FALSE,
      rownames = FALSE,
      selection = "none",
      options = list(
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        info = FALSE,
        columnDefs = list(
          list(
            targets = 1,
            className = "dt-center",
            width = "180px"
          )
        )
      )
    )
  })
  
  observeEvent(input$add_page, {
    
    showModal(
      modalDialog(
        title = "Add dashboard page",
        
        textInput(
          "new_page_name",
          "Page name",
          value = ""
        ),
        
        tags$small(
          "The file name will be generated automatically."
        ),
        
        footer = tagList(
          actionButton(
            "cancel_add_page",
            "Cancel"
          ),
          actionButton(
            "confirm_add_page",
            "Add",
            class = "btn-primary"
          )
        ),
        
        easyClose = FALSE
      )
    )
  })
  
  observeEvent(input$cancel_add_page, {
    show_pages_modal()
  })
  
  observeEvent(input$confirm_add_page, {
    
    page_name <- trimws(input$new_page_name)
    
    if (!nzchar(page_name)) {
      showNotification(
        "Enter a page name.",
        type = "error"
      )
      return()
    }
    
    new_href <- tryCatch(
      slugify_page(page_name),
      error = function(error) {
        showNotification(
          error$message,
          type = "error"
        )
        NULL
      }
    )
    
    req(new_href)
    
    reserved_hrefs <- c(
      "index.html",
      "page.html",
      "user-notes.html"
    )
    
    if (new_href %in% reserved_hrefs) {
      showNotification(
        "That page name creates a reserved file name.",
        type = "error"
      )
      return()
    }
    
    pages <- pages_draft()
    
    if (new_href %in% pages$href) {
      showNotification(
        "A page with that file name already exists.",
        type = "error"
      )
      return()
    }
    
    pages <- rbind(
      pages,
      data.frame(
        href = new_href,
        text = page_name,
        original_href = NA_character_,
        stringsAsFactors = FALSE
      )
    )
    
    pages_draft(pages)
    show_pages_modal()
  })
  
  observeEvent(input$page_action, {
    
    action <- input$page_action$action
    row <- as.integer(input$page_action$row)
    
    pages <- pages_draft()
    
    req(
      row >= 1,
      row <= nrow(pages)
    )
    
    if (action == "up" && row > 1) {
      
      order <- seq_len(nrow(pages))
      order[c(row - 1, row)] <- order[c(row, row - 1)]
      
      pages_draft(pages[order, , drop = FALSE])
    }
    
    if (action == "down" && row < nrow(pages)) {
      
      order <- seq_len(nrow(pages))
      order[c(row, row + 1)] <- order[c(row + 1, row)]
      
      pages_draft(pages[order, , drop = FALSE])
    }
    
    if (action == "delete") {
      
      pages <- pages[-row, , drop = FALSE]
      rownames(pages) <- NULL
      
      pages_draft(pages)
    }
    
    if (action == "rename") {
      
      editing_page_row(row)
      
      showModal(
        modalDialog(
          title = "Rename dashboard page",
          
          textInput(
            "renamed_page_name",
            "Page name",
            value = pages$text[row]
          ),
          
          footer = tagList(
            actionButton(
              "cancel_rename_page",
              "Cancel"
            ),
            actionButton(
              "confirm_rename_page",
              "Rename",
              class = "btn-primary"
            )
          ),
          
          easyClose = FALSE
        )
      )
    }
  })
  
  observeEvent(input$cancel_rename_page, {
    editing_page_row(NULL)
    show_pages_modal()
  })
  
  observeEvent(input$confirm_rename_page, {
    
    row <- editing_page_row()
    req(row)
    
    page_name <- trimws(input$renamed_page_name)
    
    if (!nzchar(page_name)) {
      showNotification(
        "Enter a page name.",
        type = "error"
      )
      return()
    }
    
    new_href <- tryCatch(
      slugify_page(page_name),
      error = function(error) {
        showNotification(
          error$message,
          type = "error"
        )
        NULL
      }
    )
    
    req(new_href)
    
    reserved_hrefs <- c(
      "index.html",
      "page.html",
      "user-notes.html"
    )
    
    if (new_href %in% reserved_hrefs) {
      showNotification(
        "That page name creates a reserved file name.",
        type = "error"
      )
      return()
    }
    
    pages <- pages_draft()
    
    duplicate <- pages$href == new_href
    duplicate[row] <- FALSE
    
    if (any(duplicate)) {
      showNotification(
        "A page with that file name already exists.",
        type = "error"
      )
      return()
    }
    
    pages$text[row] <- page_name
    pages$href[row] <- new_href
    
    pages_draft(pages)
    editing_page_row(NULL)
    
    show_pages_modal()
  })
  
  observeEvent(input$save_pages, {
    
    project_root <- folder()
    src_directory <- file.path(project_root, "src")
    config_path <- file.path(
      project_root,
      "src",
      "config",
      "config.js"
    )
    
    pages <- pages_draft()
    
    current_navigation <- config_file()$navigation
    
    old_pages <- current_navigation[
      !current_navigation$href %in% c(
        "index.html",
        "page.html",
        "user-notes.html"
      ),
      c("href", "text"),
      drop = FALSE
    ]
    
    tryCatch({
      
      # Check generated destination paths before changing anything.
      for (i in seq_len(nrow(pages))) {
        
        original_href <- pages$original_href[i]
        new_href <- pages$href[i]
        
        is_new <- is.na(original_href)
        is_renamed <- !is_new && original_href != new_href
        
        if (is_new || is_renamed) {
          
          destination_html <- file.path(
            project_root,
            new_href
          )
          
          destination_js <- file.path(
            src_directory,
            page_js_filename(new_href)
          )
          
          if (
            file.exists(destination_html) &&
            !identical(original_href, new_href)
          ) {
            stop(
              "The file already exists: ",
              destination_html
            )
          }
          
          old_js <- if (!is_new) {
            file.path(
              src_directory,
              page_js_filename(original_href)
            )
          } else {
            NA_character_
          }
          
          if (
            file.exists(destination_js) &&
            !identical(destination_js, old_js)
          ) {
            stop(
              "The file already exists: ",
              destination_js
            )
          }
        }
      }
      
      # Delete pages removed from the draft.
      retained_originals <- stats::na.omit(
        pages$original_href
      )
      
      deleted_hrefs <- setdiff(
        old_pages$href,
        retained_originals
      )
      
      for (deleted_href in deleted_hrefs) {
        
        deleted_html <- file.path(
          project_root,
          deleted_href
        )
        
        deleted_js <- file.path(
          src_directory,
          page_js_filename(deleted_href)
        )
        
        if (file.exists(deleted_html)) {
          result <- unlink(deleted_html)
          
          if (result != 0) {
            stop(
              "Could not delete: ",
              deleted_html
            )
          }
        }
        
        if (file.exists(deleted_js)) {
          result <- unlink(deleted_js)
          
          if (result != 0) {
            stop(
              "Could not delete: ",
              deleted_js
            )
          }
        }
      }
      
      # Create new pages and rename existing pages.
      for (i in seq_len(nrow(pages))) {
        
        page_name <- pages$text[i]
        new_href <- pages$href[i]
        original_href <- pages$original_href[i]
        
        new_html_path <- file.path(
          project_root,
          new_href
        )
        
        new_js_path <- file.path(
          src_directory,
          page_js_filename(new_href)
        )
        
        is_new <- is.na(original_href)
        is_renamed <- !is_new && original_href != new_href
        
        if (is_new) {
          
          template_html <- file.path(
            project_root,
            "page.html"
          )
          
          template_js <- file.path(
            src_directory,
            "page.js"
          )
          
          if (!file.exists(template_html)) {
            stop("Template page.html was not found.")
          }
          
          if (!file.exists(template_js)) {
            stop("Template src/page.js was not found.")
          }
          
          copied_html <- file.copy(
            template_html,
            new_html_path,
            overwrite = FALSE
          )
          
          copied_js <- file.copy(
            template_js,
            new_js_path,
            overwrite = FALSE
          )
          
          if (!copied_html || !copied_js) {
            stop(
              "Could not create files for ",
              page_name,
              "."
            )
          }
          
          set_page_html_module(
            html_path = new_html_path,
            js_filename = page_js_filename(new_href)
          )
          
          set_page_js_title(
            js_path = new_js_path,
            page_name = page_name
          )
          
        } else if (is_renamed) {
          
          old_html_path <- file.path(
            project_root,
            original_href
          )
          
          old_js_path <- file.path(
            src_directory,
            page_js_filename(original_href)
          )
          
          if (!file.exists(old_html_path)) {
            stop(
              "The original HTML file was not found: ",
              old_html_path
            )
          }
          
          if (!file.exists(old_js_path)) {
            stop(
              "The original JavaScript file was not found: ",
              old_js_path
            )
          }
          
          if (!file.rename(old_html_path, new_html_path)) {
            stop(
              "Could not rename ",
              original_href,
              "."
            )
          }
          
          if (!file.rename(old_js_path, new_js_path)) {
            
            # Attempt to undo the HTML rename.
            file.rename(new_html_path, old_html_path)
            
            stop(
              "Could not rename ",
              page_js_filename(original_href),
              "."
            )
          }
          
          set_page_html_module(
            html_path = new_html_path,
            js_filename = page_js_filename(new_href)
          )
          
          set_page_js_title(
            js_path = new_js_path,
            page_name = page_name
          )
          
        } else {
          
          # Existing page whose href has not changed.
          # Updating the title is harmless and also handles a name change
          # that produces the same slug.
          set_page_js_title(
            js_path = new_js_path,
            page_name = page_name
          )
        }
      }
      
      # Preserve the existing default entries where possible.
      home <- current_navigation[
        current_navigation$href == "index.html",
        c("href", "text"),
        drop = FALSE
      ]
      
      if (nrow(home) == 0) {
        home <- data.frame(
          href = "index.html",
          text = "Home",
          stringsAsFactors = FALSE
        )
      }
      
      user_notes <- current_navigation[
        current_navigation$href == "user-notes.html",
        c("href", "text"),
        drop = FALSE
      ]
      
      if (nrow(user_notes) == 0) {
        user_notes <- data.frame(
          href = "user-notes.html",
          text = "User Notes",
          stringsAsFactors = FALSE
        )
      }
      
      custom_navigation <- pages[
        ,
        c("href", "text"),
        drop = FALSE
      ]
      
      if (nrow(custom_navigation) == 0) {
        
        placeholder <- current_navigation[
          current_navigation$href == "page.html",
          c("href", "text"),
          drop = FALSE
        ]
        
        if (nrow(placeholder) == 0) {
          placeholder <- data.frame(
            href = "page.html",
            text = "Page",
            stringsAsFactors = FALSE
          )
        }
        
        new_navigation <- rbind(
          home[1, , drop = FALSE],
          placeholder[1, , drop = FALSE],
          user_notes[1, , drop = FALSE]
        )
        
      } else {
        
        # page.html is omitted as soon as custom pages exist.
        new_navigation <- rbind(
          home[1, , drop = FALSE],
          custom_navigation,
          user_notes[1, , drop = FALSE]
        )
      }
      
      config_text <- paste(
        readLines(config_path, warn = FALSE),
        collapse = "\n"
      )
      
      updated_config <- replace_navigation_in_config(
        config_text,
        new_navigation
      )
      
      writeLines(
        updated_config,
        config_path,
        useBytes = TRUE
      )
      
      config_version(
        config_version() + 1
      )
      
      removeModal()
      
      showNotification(
        "Dashboard pages saved.",
        type = "message"
      )
      
    }, error = function(error) {
      
      showNotification(
        paste(
          "Pages could not be saved:",
          error$message
        ),
        type = "error",
        duration = NULL
      )
    })
  })
  
  ### Edit department ####
  #### Read and parse departments.js ####
  departments_file <- reactive({
    req(folder())
    
    departments_path <- file.path(
      folder(),
      "src",
      "config",
      "departments.js"
    )
    
    validate(
      need(
        file.exists(departments_path),
        "No departments.js file was found."
      )
    )
    
    departments_text <- readLines(
      departments_path,
      warn = FALSE,
      encoding = "UTF-8"
    ) |>
      paste(collapse = "\n")
    
    departments_text <- sub(
      pattern = "^\\s*export\\s+const\\s+departments\\s*=",
      replacement = "var departments =",
      x = departments_text
    )
    
    ctx <- V8::v8()
    ctx$eval(departments_text)
    
    ctx$get("departments")
  })
  
  #### Add the deparment edit modal ####
  observeEvent(input$edit_setting, {
    req(input$edit_setting == 3)
    
    config <- config_file()
    departments <- departments_file()
    
    department_keys <- names(departments)
    
    validate(
      need(
        length(department_keys) > 0,
        "No departments were found in departments.js."
      )
    )
    
    current_department <- config$department
    
    if (
      is.null(current_department) ||
      !current_department %in% department_keys
    ) {
      current_department <- "DoF"
    }
    
    department_labels <- vapply(
      department_keys,
      function(key) {
        department_name <- departments[[key]]$name
        
        if (
          is.null(department_name) ||
          !nzchar(department_name)
        ) {
          return(key)
        }
        
        paste0(
          key,
          " — ",
          department_name
        )
      },
      character(1)
    )
    
    department_choices <- stats::setNames(
      department_keys,
      department_labels
    )
    
    showModal(
      modalDialog(
        title = "Edit department",
        
        selectInput(
          inputId = "new_department",
          label = "Department",
          choices = department_choices,
          selected = current_department,
          width = "100%"
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            inputId = "save_department",
            label = "Save",
            class = "btn-primary"
          )
        ),
        
        easyClose = FALSE
      )
    )
  })
  
  #### Save selected department ####
  observeEvent(input$save_department, {
    req(folder())
    req(input$new_department)
    
    new_department <- input$new_department
    departments <- departments_file()
    
    valid_departments <- names(departments)
    
    if (!new_department %in% valid_departments) {
      showNotification(
        "The selected department is not valid.",
        type = "error"
      )
      return()
    }
    
    config_path <- file.path(
      folder(),
      "src",
      "config",
      "config.js"
    )
    
    if (!file.exists(config_path)) {
      showNotification(
        "No config.js file was found.",
        type = "error"
      )
      return()
    }
    
    config_lines <- readLines(
      config_path,
      warn = FALSE,
      encoding = "UTF-8"
    )
    
    department_line <- grep(
      '^\\s*["\']department["\']\\s*:',
      config_lines
    )
    
    if (length(department_line) != 1) {
      showNotification(
        'Could not uniquely identify the "department" property.',
        type = "error",
        duration = NULL
      )
      return()
    }
    
    existing_indent <- sub(
      '^(\\s*).*',
      '\\1',
      config_lines[department_line]
    )
    
    has_comma <- grepl(
      ',\\s*$',
      config_lines[department_line]
    )
    
    config_lines[department_line] <- paste0(
      existing_indent,
      '"department": "',
      new_department,
      '"',
      if (has_comma) "," else ""
    )
    
    writeLines(
      config_lines,
      config_path,
      useBytes = TRUE
    )
    
    config_version(
      config_version() + 1
    )
    
    removeModal()
    
    showNotification(
      paste0(
        "Department updated to ",
        new_department,
        "."
      ),
      type = "message"
    )
  })
  
  ### Display Data Portal Tables modal ####
  
  matrix_draft <- reactiveVal(character())
  
  show_matrix_editor <- function() {
    
    showModal(
      modalDialog(
        title = "Edit Data Portal tables",
        
        tags$p(
          "Data Portal tables are identified by their matrix name. ",
          tags$a(
            href = "https://data.nisra.gov.uk/",
            target = "_blank",
            rel = "noopener noreferrer",
            "Browse the NISRA Data Portal"
          ),
          " and enter matrix codes below."
        ),
        
        actionButton(
          "add_matrix",
          label = NULL,
          icon = icon("plus"),
          class = "btn-success",
          title = "Add Data Portal table"
        ),
        
        tags$div(style = "margin-top: 15px;"),
        
        DT::DTOutput("matrix_editor_table"),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            "save_matrices",
            "Save",
            class = "btn-primary"
          )
        ),
        
        size = "l",
        easyClose = FALSE
      )
    )
  }
  
  
  observeEvent(input$edit_setting, {
    req(input$edit_setting == 4)
    
    config <- config_file()
    
    matrices <- config$matrix
    
    if (is.null(matrices)) {
      matrices <- character()
    }
    
    matrices <- as.character(
      unlist(
        matrices,
        use.names = FALSE
      )
    )
    
    matrices <- trimws(matrices)
    
    matrices <- matrices[
      nzchar(matrices) &
        !matrices %in% c(
          "EXAMPLETABLE1",
          "EXAMPLETABLE2"
        )
    ]
    
    matrix_draft(matrices)
    
    show_matrix_editor()
  })
  
  
  output$matrix_editor_table <- DT::renderDT({
    
    matrices <- matrix_draft()
    
    if (length(matrices) == 0) {
      
      display <- data.frame(
        `Data Portal table` = paste0(
          "<em>",
          "No Data Portal tables yet.",
          "</em>"
        ),
        Actions = "",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
    } else {
      
      display <- data.frame(
        `Data Portal table` = matrices,
        
        Actions = vapply(
          seq_along(matrices),
          function(i) {
            
            sprintf(
              paste0(
                '<button class="btn btn-danger btn-sm" ',
                'title="Delete" ',
                'onclick="Shiny.setInputValue(',
                '\'delete_matrix\', %d, ',
                '{priority:\'event\'})">',
                '<i class="fa fa-trash"></i>',
                '</button>'
              ),
              i
            )
            
          },
          character(1)
        ),
        
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
    
    DT::datatable(
      display,
      escape = FALSE,
      rownames = FALSE,
      selection = "none",
      
      options = list(
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        info = FALSE,
        
        columnDefs = list(
          list(
            targets = 1,
            className = "dt-center",
            width = "80px",
            searchable = FALSE
          )
        )
      )
    )
  })
  
  ### Add data portal table ####
  observeEvent(input$add_matrix, {
    
    showModal(
      
      modalDialog(
        
        title = "Add Data Portal table",
        
        textInput(
          "new_matrix",
          "Matrix name"
        ),
        
        footer = tagList(
          actionButton(
            "cancel_add_matrix",
            "Cancel"
          ),
          actionButton(
            "confirm_add_matrix",
            "Add",
            class = "btn-primary"
          )
        ),
        
        easyClose = FALSE
        
      )
      
    )
    
  })
  
  observeEvent(input$cancel_add_matrix, {
    show_matrix_editor()
  })
  
  observeEvent(input$confirm_add_matrix, {
    
    matrix_name <- toupper(
      trimws(input$new_matrix)
    )
    
    if (!nzchar(matrix_name)) {
      showNotification(
        "Enter a matrix name.",
        type = "error"
      )
      return()
    }
    
    matrices <- matrix_draft()
    
    if (matrix_name %in% toupper(matrices)) {
      showNotification(
        "That matrix already exists.",
        type = "error"
      )
      return()
    }
    
    notification_id <- showNotification(
      paste0(
        "Checking matrix ",
        matrix_name,
        "..."
      ),
      type = "message",
      duration = NULL,
      closeButton = FALSE
    )
    
    verification <- verify_matrix(matrix_name)
    
    removeNotification(notification_id)
    
    if (!verification$valid) {
      showNotification(
        verification$message,
        type = "error",
        duration = NULL
      )
      return()
    }
    
    matrices <- c(
      matrices,
      verification$matrix
    )
    
    matrix_draft(matrices)
    
    show_matrix_editor()
    
    showNotification(
      verification$message,
      type = "message"
    )
  })
  
  observeEvent(input$delete_matrix, {
    
    matrices <- matrix_draft()
    
    matrices <- matrices[-input$delete_matrix]
    
    matrix_draft(matrices)
    
  })
  
  ### Save Matrix ####
  observeEvent(input$save_matrices, {
    
    config_path <- file.path(
      folder(),
      "src",
      "config",
      "config.js"
    )
    
    config_text <- paste(
      readLines(
        config_path,
        warn = FALSE,
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )
    
    updated_config <- replace_matrix_in_config(
      config_text,
      matrix_draft()
    )
    
    writeLines(
      updated_config,
      config_path,
      useBytes = TRUE
    )
    
    data_script <- file.path(
      folder(),
      "src",
      "r",
      "data.R"
    )
    
    if (!file.exists(data_script)) {
      stop("data.R was not found.")
    }
    
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)

    setwd(folder())
    
    source(
      data_script,
      local = new.env(parent = globalenv())
    )
    
    loaded_tables_version(
      loaded_tables_version() + 1
    )
    
    config_version(
      config_version() + 1
    )
    
    removeModal()
    
    showNotification(
      "Data Portal tables saved.",
      type = "message"
    )
    
  })
  
  # Home page design tab ####
  
  ## Clear home page content button ####
  observeEvent(input$clear_homepage_content, {
    
    req(folder())
    
    showModal(
      modalDialog(
        title = "Clear homepage content?",
        
        tags$p(
          paste(
            "This will remove the example content from index.html",
            "and the example value-insertion code from src/index.js."
          )
        ),
        
        tags$p(
          tags$strong(
            "The dashboard layout, information heading and SVG icon will remain."
          )
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          
          actionButton(
            inputId = "confirm_clear_homepage_content",
            label = "Clear content",
            icon = icon("eraser"),
            class = "btn-danger"
          )
        ),
        
        easyClose = FALSE
      )
    )
  })
  
  observeEvent(input$confirm_clear_homepage_content, {
    
    req(folder())
    
    notification_id <- showNotification(
      "Clearing homepage content...",
      type = "message",
      duration = NULL,
      closeButton = FALSE
    )
    
    result <- tryCatch(
      {
        clear_homepage_files(
          project_root = folder()
        )
      },
      error = function(error) {
        removeNotification(notification_id)
        
        showNotification(
          paste(
            "Homepage content could not be cleared:",
            conditionMessage(error)
          ),
          type = "error",
          duration = NULL
        )
        
        NULL
      }
    )
    
    if (is.null(result)) {
      return()
    }
    
    removeNotification(notification_id)
    removeModal()
    
    showNotification(
      "Homepage content cleared.",
      type = "message"
    )
  })
  
  ## Edit strapline ####
  observeEvent(input$save_homepage_strapline, {
    
    req(folder())
    
    strapline <- trimws(
      input$homepage_strapline
    )
    
    if (!nzchar(strapline)) {
      showNotification(
        "Enter a homepage strapline.",
        type = "error"
      )
      
      return()
    }
    
    tryCatch(
      {
        update_homepage_strapline(
          project_root = folder(),
          strapline = strapline
        )
        
        showNotification(
          "Homepage strapline updated.",
          type = "message"
        )
      },
      error = function(error) {
        showNotification(
          paste(
            "Homepage strapline could not be updated:",
            conditionMessage(error)
          ),
          type = "error",
          duration = NULL
        )
      }
    )
  })
  
  ## Edit number of cards on home page ####
  
  observeEvent(folder(), {
    
    existing_count <- tryCatch(
      count_homepage_cards(
        project_root = folder()
      ),
      error = function(error) {
        NULL
      }
    )
    
    if (!is.null(existing_count)) {
      updateNumericInput(
        session = session,
        inputId = "homepage_card_count",
        value = existing_count
      )
    }
    
  }, ignoreInit = TRUE)
  
  ## Save number of homepage cards ####
  
  observeEvent(input$save_homepage_card_count, {
    
    req(folder())
    req(input$homepage_card_count)
    
    requested_count <- as.integer(
      input$homepage_card_count
    )
    
    if (
      is.na(requested_count) ||
      requested_count < 1 ||
      requested_count > 9
    ) {
      showNotification(
        "Choose between 1 and 9 cards.",
        type = "error"
      )
      
      return()
    }
    
    tryCatch(
      {
        update_homepage_card_count(
          project_root = folder(),
          card_count = requested_count
        )
        
        showNotification(
          paste0(
            "Homepage updated to ",
            requested_count,
            if (requested_count == 1) {
              " card."
            } else {
              " cards."
            }
          ),
          type = "message"
        )
      },
      error = function(error) {
        showNotification(
          paste(
            "The homepage cards could not be updated:",
            conditionMessage(error)
          ),
          type = "error",
          duration = NULL
        )
      }
    )
  })
  
  
}

shinyApp(
  ui = ui,
  server = server,
  options = list(launch.browser = TRUE)
)