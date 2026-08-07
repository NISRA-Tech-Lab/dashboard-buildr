library(shiny)
library(shinyFiles)
library(servr)
library(shinyjs)
library(dplyr)
library(V8)
library(DT)
library(shinythemes)

for (file in list.files("R", full.names = TRUE)) {
  source(file, local = TRUE)
}

# ui ####
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
            
            tags$div(
              style = "margin-bottom: 15px;",
              
              actionButton(
                inputId = "save_homepage_card_count",
                label = "Save",
                icon = icon("save"),
                class = "btn-primary"
              )
            )
          ),
          
          tags$div(
            style = "margin-top: 20px;",
            uiOutput("homepage_card_editors")
          ),
          
          tags$hr(),
          
          h3("Dashboard information"),
          
          tags$p(
            paste(
              "Enter supporting information about this dashboard.",
              "You can use HTML for paragraphs, links, lists and other formatting."
            )
          ),
          
          textAreaInput(
            inputId = "homepage_dashboard_information",
            label = "Dashboard information HTML",
            value = "",
            rows = 10,
            width = "100%",
            resize = "vertical",
            placeholder = paste0(
              "<p>This dashboard was built using...</p>\n\n",
              "<p>Further information is available from ",
              "<a href=\"https://example.com\">the data source</a>.</p>"
            )
          ),
          
          actionButton(
            inputId = "save_homepage_dashboard_information",
            label = "Save dashboard information",
            icon = icon("save"),
            class = "btn-primary"
          )
          
        ),
        
        tabPanel(
          "Page design",
          
          h2("Page design"),
          
          selectInput(
            inputId = "page_design_page",
            label = "Page to edit",
            choices = character(),
            width = "100%"
          ),
          
          uiOutput("page_design_interface")
        ),
        
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
    
    tryCatch(
      {
        clear_homepage_files(
          project_root = folder()
        )
        
        updateTextInput(
          session = session,
          inputId = "homepage_strapline",
          value = ""
        )
        
        updateTextAreaInput(
          session = session,
          inputId = "homepage_dashboard_information",
          value = ""
        )
        
        removeModal()
        
        showNotification(
          "Homepage content cleared.",
          type = "message"
        )
      },
      error = function(error) {
        showNotification(
          paste(
            "Homepage content could not be cleared:",
            conditionMessage(error)
          ),
          type = "error",
          duration = NULL
        )
      }
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
  
  observe({
    req(folder())
    
    strapline <- tryCatch(
      read_homepage_strapline(
        project_root = folder()
      ),
      error = function(error) {
        ""
      }
    )
    
    updateTextInput(
      session = session,
      inputId = "homepage_strapline",
      value = strapline
    )
  })
  
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
        
        homepage_card_editor_count(
          requested_count
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
  
  ## Accordions to edit cards
  
  homepage_card_editor_count <- reactiveVal(0)
  
  homepage_card_values <- reactiveVal(
    list()
  )
  
  observe({
    req(folder())
    
    card_values <- tryCatch(
      read_homepage_card_values(
        project_root = folder()
      ),
      error = function(error) {
        showNotification(
          paste(
            "Existing homepage card content could not be read:",
            conditionMessage(error)
          ),
          type = "error",
          duration = NULL
        )
        
        list()
      }
    )
    
    card_count <- length(
      card_values
    )
    
    homepage_card_values(
      card_values
    )
    
    homepage_card_editor_count(
      card_count
    )
    
    if (card_count > 0) {
      updateNumericInput(
        session = session,
        inputId = "homepage_card_count",
        value = card_count
      )
    }
  })
  
  output$homepage_card_editors <- renderUI({
    
    req(folder())
    
    card_count <- homepage_card_editor_count()
    
    if (
      length(card_count) != 1 ||
      is.na(card_count) ||
      card_count < 1
    ) {
      return(
        tags$em(
          "No homepage cards were found."
        )
      )
    }
    
    config <- config_file()
    navigation <- config$navigation
    
    default_hrefs <- c(
      "index.html",
      "page.html",
      "user-notes.html"
    )
    
    custom_pages <- navigation[
      !navigation$href %in% default_hrefs,
      c("href", "text"),
      drop = FALSE
    ]
    
    page_choices <- c(
      "No page link" = "",
      stats::setNames(
        custom_pages$href,
        custom_pages$text
      )
    )
    
    accordion_id <- "homepage-card-accordion"
    
    tags$div(
      id = accordion_id,
      class = "panel-group",
      role = "tablist",
      `aria-multiselectable` = "false",
      
      lapply(
        seq_len(card_count),
        function(card_number) {
          
          card_values <- homepage_card_values()
          
          current_values <- if (
            card_number <= length(card_values)
          ) {
            card_values[[card_number]]
          } else {
            list(
              top_line = "",
              value = "",
              unit = "",
              bottom_line = "",
              page_href = ""
            )
          }
          
          pending_calculation <- card_calculations[[
            as.character(card_number)
          ]]
          
          if (!is.null(pending_calculation)) {
            
            current_values$value <- format_card_calculation_value(
              value = pending_calculation$raw_value,
              decimal_places = pending_calculation$decimal_places,
              comma_separator = pending_calculation$comma_separator
            )
          }
          
          heading_id <- paste0(
            "homepage-card-heading-",
            card_number
          )
          
          collapse_id <- paste0(
            "homepage-card-collapse-",
            card_number
          )
          
          is_open <- card_number == 1
          
          tags$div(
            class = "panel panel-default",
            
            tags$div(
              class = "panel-heading",
              role = "tab",
              id = heading_id,
              
              tags$h4(
                class = "panel-title",
                
                tags$a(
                  class = if (is_open) {
                    ""
                  } else {
                    "collapsed"
                  },
                  role = "button",
                  href = "javascript:void(0);",
                  `data-toggle` = "collapse",
                  `data-parent` = paste0(
                    "#",
                    accordion_id
                  ),
                  `data-target` = paste0(
                    "#",
                    collapse_id
                  ),
                  `aria-expanded` = if (is_open) {
                    "true"
                  } else {
                    "false"
                  },
                  `aria-controls` = collapse_id,
                  
                  tags$span(
                    paste(
                      "Card",
                      card_number
                    )
                  ),
                  
                  tags$span(
                    class = "pull-right",
                    icon("chevron-down")
                  )
                )
              )
            ),
            
            tags$div(
              id = collapse_id,
              class = paste(
                "panel-collapse collapse",
                if (is_open) {
                  "in"
                } else {
                  ""
                }
              ),
              role = "tabpanel",
              `aria-labelledby` = heading_id,
              
              tags$div(
                class = "panel-body",
                
                tags$div(
                  class = "alert alert-info",
                  
                  tags$p(
                    style = "margin-bottom: 8px;",
                    
                    paste(
                      "Use the fields below to construct a short,",
                      "user-friendly sentence that communicates one key message."
                    )
                  ),
                  
                  tags$p(
                    style = "margin-bottom: 0;",
                    
                    "You can insert dynamic years in the Top line or Bottom line using ",
                    
                    tags$code("<<latest-year>>"),
                    ", ",
                    tags$code("<<last-year>>"),
                    " or ",
                    tags$code("<<first-year>>"),
                    "."
                  )
                ),
                
                textInput(
                  inputId = paste0(
                    "card_",
                    card_number,
                    "_top_line"
                  ),
                  label = "Top line",
                  value = current_values$top_line,
                  width = "100%"
                ),
                
                tags$div(
                  style = paste(
                    "display: flex;",
                    "align-items: flex-end;",
                    "gap: 12px;"
                  ),
                  
                  tags$div(
                    style = "flex: 2;",
                    
                    tags$div(
                      class = "form-group",
                      
                      tags$label(
                        `for` = paste0(
                          "card_",
                          card_number,
                          "_value"
                        ),
                        "Value"
                      ),
                      
                      tags$div(
                        class = "input-group",
                        
                        tags$input(
                          id = paste0(
                            "card_",
                            card_number,
                            "_value"
                          ),
                          type = "text",
                          class = "form-control",
                          value = current_values$value,
                          readonly = "readonly",
                          placeholder = "No value calculated"
                        ),
                        
                        tags$span(
                          class = "input-group-btn",
                          
                          actionButton(
                            inputId = paste0(
                              "calculate_card_",
                              card_number
                            ),
                            label = "Calculate",
                            icon = icon("calculator"),
                            class = "btn-default"
                          )
                        )
                      )
                    )
                  ),
                  
                  tags$div(
                    style = "flex: 1;",
                    
                    textInput(
                      inputId = paste0(
                        "card_",
                        card_number,
                        "_unit"
                      ),
                      label = "Unit",
                      value = current_values$unit,
                      width = "100%"
                    )
                  )
                ),
                
                textInput(
                  inputId = paste0(
                    "card_",
                    card_number,
                    "_bottom_line"
                  ),
                  label = "Bottom line",
                  value = current_values$bottom_line,
                  width = "100%"
                ),
                
                selectInput(
                  inputId = paste0(
                    "card_",
                    card_number,
                    "_page"
                  ),
                  label = "Link to page",
                  choices = page_choices,
                  selected = if (
                    current_values$page_href %in%
                    unname(page_choices)
                  ) {
                    current_values$page_href
                  } else {
                    ""
                  },
                  width = "100%"
                ),
                
                actionButton(
                  inputId = paste0(
                    "save_card_",
                    card_number
                  ),
                  label = "Save card changes",
                  icon = icon("save"),
                  class = "btn-primary"
                )
              )
            )
          )
        }
      )
    )
  })
  
  ### Link in footer ####
  
  lapply(
    seq_len(9),
    function(card_number) {
      
      local({
        current_card <- card_number
        
        observeEvent(
          input[[
            paste0(
              "save_card_",
              current_card
            )
          ]],
          {
            req(folder())
            
            if (
              current_card >
              homepage_card_editor_count()
            ) {
              return()
            }
            
            selected_href <- input[[
              paste0(
                "card_",
                current_card,
                "_page"
              )
            ]]
            
            if (is.null(selected_href)) {
              selected_href <- ""
            }
            
            selected_href <- as.character(
              selected_href
            )
            
            top_line <- input[[
              paste0(
                "card_",
                current_card,
                "_top_line"
              )
            ]]
            
            stored_calculation <- card_calculations[[
              as.character(current_card)
            ]]
            
            if (is.null(stored_calculation)) {
              showNotification(
                paste0(
                  "Calculate a value for card ",
                  current_card,
                  " before saving."
                ),
                type = "error"
              )
              
              return()
            }
            
            unit <- input[[
              paste0(
                "card_",
                current_card,
                "_unit"
              )
            ]]
            
            bottom_line <- input[[
              paste0(
                "card_",
                current_card,
                "_bottom_line"
              )
            ]]
            
            config <- config_file()
            navigation <- config$navigation
            
            selected_text <- ""
            
            if (nzchar(selected_href)) {
              matching_page <- navigation[
                navigation$href == selected_href,
                ,
                drop = FALSE
              ]
              
              if (nrow(matching_page) != 1) {
                showNotification(
                  "The selected page could not be identified.",
                  type = "error"
                )
                return()
              }
              
              selected_text <- as.character(
                matching_page$text[1]
              )
            }
            
            tryCatch(
              {
                update_homepage_card_body(
                  project_root = folder(),
                  card_number = current_card,
                  top_line = top_line,
                  unit = unit,
                  bottom_line = bottom_line
                )
                
                update_homepage_card_value_js(
                  project_root = folder(),
                  card_number = current_card,
                  calculation = stored_calculation
                )
                
                update_homepage_card_link(
                  project_root = folder(),
                  card_number = current_card,
                  page_href = selected_href,
                  page_text = selected_text
                )
                
                homepage_card_values(
                  read_homepage_card_values(
                    project_root = folder()
                  )
                )
                
                showNotification(
                  paste0(
                    "Card ",
                    current_card,
                    " updated."
                  ),
                  type = "message"
                )
              },
              error = function(error) {
                showNotification(
                  paste(
                    paste0(
                      "Card ",
                      current_card,
                      " could not be updated:"
                    ),
                    conditionMessage(error)
                  ),
                  type = "error",
                  duration = NULL
                )
              }
            )
          },
          ignoreInit = TRUE
        )
      })
    }
  )
  
  ## Calculate home page values ####
  active_calculation_card <- reactiveVal(NULL)
  
  active_calculation_context <- reactiveVal("homepage")
  
  calculation_data <- reactiveVal(NULL)
  
  card_calculations <- reactiveValues()
  
  load_calculation_matrix <- function(matrix) {
    
    req(folder())
    req(matrix)
    
    result <- tryCatch(
      read_card_calculation_data(
        project_root = folder(),
        matrix = matrix
      ),
      error = function(error) {
        showNotification(
          paste(
            "The table could not be loaded:",
            conditionMessage(error)
          ),
          type = "error",
          duration = NULL
        )
        
        NULL
      }
    )
    
    calculation_data(result)
  }
  
  show_card_calculation_modal <- function(
    card_number,
    context = "homepage"
  ) {
    
    active_calculation_context(context)
    
    files <- loaded_table_files()
    metadata <- loaded_table_metadata()
    
    if (length(files) == 0) {
      showNotification(
        "No Data Portal tables have been loaded.",
        type = "error"
      )
      
      return()
    }
    
    matrices <- tools::file_path_sans_ext(
      basename(files)
    )
    
    matrix_labels <- vapply(
      matrices,
      function(matrix) {
        
        table_metadata <- metadata[[matrix]]
        
        label <- if (
          !is.null(table_metadata) &&
          !is.null(table_metadata$label) &&
          length(table_metadata$label) > 0 &&
          nzchar(as.character(table_metadata$label)[1])
        ) {
          as.character(table_metadata$label)[1]
        } else {
          matrix
        }
        
        paste0(
          label,
          " (",
          matrix,
          ")"
        )
      },
      character(1)
    )
    
    matrix_choices <- stats::setNames(
      matrices,
      matrix_labels
    )
    
    saved_calculation <- if (
      identical(context, "page")
    ) {
      page_card_calculations[[
        as.character(card_number)
      ]]
    } else {
      card_calculations[[
        as.character(card_number)
      ]]
    }
    
    selected_matrix <- if (
      !is.null(saved_calculation) &&
      saved_calculation$matrix %in% matrices
    ) {
      saved_calculation$matrix
    } else {
      matrices[1]
    }
    
    selected_decimals <- if (
      !is.null(saved_calculation)
    ) {
      saved_calculation$decimal_places
    } else {
      0
    }
    
    selected_comma <- if (
      !is.null(saved_calculation)
    ) {
      isTRUE(
        saved_calculation$comma_separator
      )
    } else {
      FALSE
    }
    
    active_calculation_card(
      card_number
    )
    
    showModal(
      modalDialog(
        title = paste(
          "Calculate value for card",
          card_number
        ),
        
        h4("Select data"),
        
        selectInput(
          inputId = "calculation_matrix",
          label = "Data Portal table",
          choices = matrix_choices,
          selected = selected_matrix,
          width = "100%"
        ),
        
        tags$div(
          style = "margin-top: 15px;",
          
          DT::DTOutput(
            "calculation_table_preview"
          )
        ),
        
        tags$hr(),
        
        h4("Filter data"),
        
        uiOutput(
          "calculation_filters_ui"
        ),
        
        tags$div(
          class = "well",
          style = paste(
            "margin-top: 15px;",
            "margin-bottom: 15px;"
          ),
          
          tags$strong(
            "Current value"
          ),
          
          tags$div(
            style = paste(
              "font-size: 2em;",
              "margin-top: 5px;"
            ),
            
            textOutput(
              "calculation_current_value",
              inline = TRUE
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            
            numericInput(
              inputId = "calculation_decimal_places",
              label = "Number of decimal places",
              value = selected_decimals,
              min = 0,
              max = 10,
              step = 1,
              width = "100%"
            )
          ),
          
          column(
            width = 6,
            
            tags$div(
              style = "margin-top: 25px;",
              
              checkboxInput(
                inputId = "calculation_comma_separator",
                label = "Use comma separator",
                value = selected_comma
              )
            )
          )
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          
          actionButton(
            inputId = "finish_card_calculation",
            label = "Done",
            icon = icon("check"),
            class = "btn-primary"
          )
        ),
        
        size = "l",
        easyClose = FALSE
      )
    )
    
    load_calculation_matrix(
      selected_matrix
    )
  }
  
  ## Connect each Calculate button ####
  lapply(
    seq_len(9),
    function(card_number) {
      
      local({
        
        current_card <- card_number
        
        observeEvent(
          input[[
            paste0(
              "calculate_card_",
              current_card
            )
          ]],
          {
            req(folder())
            
            if (
              current_card >
              homepage_card_editor_count()
            ) {
              return()
            }
            
            show_card_calculation_modal(
              current_card
            )
          },
          ignoreInit = TRUE
        )
      })
    }
  )
  
  lapply(
    seq_len(6),
    function(card_number) {
      
      local({
        
        current_card <- card_number
        
        observeEvent(
          input[[
            paste0(
              "calculate_page_card_",
              current_card
            )
          ]],
          {
            req(folder())
            req(selected_page_design())
            
            if (
              current_card >
              page_card_editor_count()
            ) {
              return()
            }
            
            show_card_calculation_modal(
              card_number = current_card,
              context = "page"
            )
          },
          ignoreInit = TRUE
        )
      })
    }
  )
  
  ### Read the selected CSV ####
  observeEvent(
    input$calculation_matrix,
    {
      load_calculation_matrix(
        input$calculation_matrix
      )
    },
    ignoreInit = TRUE
  )
  
  ### Render the dynamic filters ####
  output$calculation_filters_ui <- renderUI({
    
    data_definition <- calculation_data()
    
    req(data_definition)
    
    saved_calculation <- if (
      identical(
        active_calculation_context(),
        "page"
      )
    ) {
      page_card_calculations[[
        as.character(
          active_calculation_card()
        )
      ]]
    } else {
      card_calculations[[
        as.character(
          active_calculation_card()
        )
      ]]
    }
    
    same_saved_matrix <- (
      !is.null(saved_calculation) &&
        identical(
          saved_calculation$matrix,
          data_definition$matrix
        )
    )
    
    row_filter_controls <- lapply(
      data_definition$row_filters,
      function(filter_definition) {
        
        selected_values <- if (
          same_saved_matrix &&
          !is.null(
            saved_calculation$filters[[
              filter_definition$input_id
            ]]
          )
        ) {
          saved_calculation$filters[[
            filter_definition$input_id
          ]]
        } else {
          character()
        }
        
        selectizeInput(
          inputId = filter_definition$input_id,
          label = filter_definition$label,
          choices = filter_definition$choices,
          selected = selected_values,
          multiple = TRUE,
          width = "100%",
          options = list(
            plugins = list(
              "remove_button"
            )
          )
        )
      }
    )
    
    selected_pivot_columns <- if (
      same_saved_matrix &&
      !is.null(
        saved_calculation$selected_columns
      )
    ) {
      intersect(
        saved_calculation$selected_columns,
        data_definition$pivot_columns
      )
    } else {
      character()
    }
    
    tagList(
      row_filter_controls,
      
      selectizeInput(
        inputId = "calculation_pivot_columns",
        label = data_definition$pivot_label,
        choices = data_definition$pivot_columns,
        selected = selected_pivot_columns,
        multiple = TRUE,
        width = "100%",
        options = list(
          plugins = list(
            "remove_button"
          )
        )
      )
    )
  })
  
  ### Gather the current filter selections ####
  calculation_filter_selections <- reactive({
    
    data_definition <- calculation_data()
    
    req(data_definition)
    
    selections <- list()
    
    for (
      filter_definition in
      data_definition$row_filters
    ) {
      
      selections[[
        filter_definition$input_id
      ]] <- input[[
        filter_definition$input_id
      ]]
    }
    
    selections
  })
  
  ### Produce the filtered preview ####
  calculation_filtered_data <- reactive({
    
    data_definition <- calculation_data()
    
    req(data_definition)
    
    filter_card_calculation_data(
      calculation_data = data_definition,
      selections = calculation_filter_selections()
    )
  })
  
  output$calculation_table_preview <- DT::renderDT({
    
    preview_data <- calculation_filtered_data()
    
    DT::datatable(
      preview_data,
      rownames = FALSE,
      selection = "none",
      filter = "none",
      class = "cell-border compact stripe",
      
      options = list(
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        info = FALSE,
        scrollX = TRUE,
        scrollY = "150px",
        scrollCollapse = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  ### Calculate the current value ####
  calculation_current_raw_value <- reactive({
    
    data_definition <- calculation_data()
    filtered_data <- calculation_filtered_data()
    
    req(data_definition)
    
    selected_columns <- input$calculation_pivot_columns
    
    if (is.null(selected_columns)) {
      selected_columns <- character()
    }
    
    calculate_card_value(
      calculation_data = data_definition,
      filtered_data = filtered_data,
      selected_columns = selected_columns
    )
  })
  
  output$calculation_current_value <- renderText({
    
    value <- calculation_current_raw_value()
    
    if (
      length(value) != 1 ||
      is.na(value) ||
      !is.finite(value)
    ) {
      return(
        "No numeric value"
      )
    }
    
    decimal_places <- input$calculation_decimal_places
    
    if (is.null(decimal_places)) {
      decimal_places <- 0
    }
    
    format_card_calculation_value(
      value = value,
      decimal_places = decimal_places,
      comma_separator = isTRUE(
        input$calculation_comma_separator
      )
    )
  })
  
  ### Store the calculation when Done is clicked ####
  observeEvent(
    input$finish_card_calculation,
    {
      card_number <- active_calculation_card()
      data_definition <- calculation_data()
      value <- calculation_current_raw_value()
      
      req(card_number)
      req(data_definition)
      
      if (
        length(value) != 1 ||
        is.na(value) ||
        !is.finite(value)
      ) {
        showNotification(
          paste(
            "The current selections do not produce",
            "a numeric value."
          ),
          type = "error"
        )
        
        return()
      }
      
      decimal_places <- as.integer(
        input$calculation_decimal_places
      )
      
      if (
        is.na(decimal_places) ||
        decimal_places < 0
      ) {
        showNotification(
          "Enter a valid number of decimal places.",
          type = "error"
        )
        
        return()
      }
      
      selected_columns <- input$calculation_pivot_columns
      
      if (
        is.null(selected_columns) ||
        length(selected_columns) == 0
      ) {
        showNotification(
          paste0(
            "Select at least one ",
            data_definition$pivot_label,
            " option."
          ),
          type = "error"
        )
        
        return()
      }
      
      filter_selections <- calculation_filter_selections()
      
      js_filters <- list()
      
      for (
        filter_definition in
        data_definition$row_filters
      ) {
        
        selected_values <- filter_selections[[
          filter_definition$input_id
        ]]
        
        if (
          !is.null(selected_values) &&
          length(selected_values) > 0
        ) {
          js_filters[[
            filter_definition$column
          ]] <- list(
            values = as.character(
              selected_values
            ),
            is_year = isTRUE(
              filter_definition$is_year
            )
          )
        }
      }
      
      stored_calculation <- list(
        matrix = data_definition$matrix,
        filters = filter_selections,
        js_filters = js_filters,
        selected_columns = as.character(
          selected_columns
        ),
        raw_value = value,
        decimal_places = decimal_places,
        comma_separator = isTRUE(
          input$calculation_comma_separator
        )
      )
      
      if (
        identical(
          active_calculation_context(),
          "page"
        )
      ) {
        page_card_calculations[[
          as.character(card_number)
        ]] <- stored_calculation
      } else {
        card_calculations[[
          as.character(card_number)
        ]] <- stored_calculation
      }
      
      display_value <- format_card_calculation_value(
        value = value,
        decimal_places = decimal_places,
        comma_separator = stored_calculation$comma_separator
      )
      
      removeModal()
      
      showNotification(
        paste0(
          "Calculated value stored for card ",
          card_number,
          "."
        ),
        type = "message"
      )
    }
  )
  
  ### Edit information about dashboard ####
  observe({
    
    req(folder())
    
    information_html <- tryCatch(
      read_homepage_information(
        project_root = folder()
      ),
      error = function(error) {
        showNotification(
          paste(
            "Existing dashboard information could not be read:",
            conditionMessage(error)
          ),
          type = "error",
          duration = NULL
        )
        
        ""
      }
    )
    
    updateTextAreaInput(
      session = session,
      inputId = "homepage_dashboard_information",
      value = information_html
    )
  })
  
  observeEvent(
    input$save_homepage_dashboard_information,
    {
      
      req(folder())
      
      information_html <- input$homepage_dashboard_information
      
      if (is.null(information_html)) {
        information_html <- ""
      }
      
      tryCatch(
        {
          update_homepage_information(
            project_root = folder(),
            information_html = information_html
          )
          
          # Reload the saved HTML into the textarea so the UI
          # remains aligned with index.html.
          saved_information <- read_homepage_information(
            project_root = folder()
          )
          
          updateTextAreaInput(
            session = session,
            inputId = "homepage_dashboard_information",
            value = saved_information
          )
          
          showNotification(
            "Dashboard information updated.",
            type = "message"
          )
        },
        error = function(error) {
          showNotification(
            paste(
              "Dashboard information could not be updated:",
              conditionMessage(error)
            ),
            type = "error",
            duration = NULL
          )
        }
      )
    }
  )
  
  ## Page design menu ####
  
  ### Page design server state ####
  page_design_version <- reactiveVal(0)
  
  selected_page_design <- reactive({
    req(input$page_design_page)
    
    input$page_design_page
  })
  
  page_card_editor_count <- reactiveVal(0)
  
  page_chart_editor_count <- reactiveVal(0)
  
  page_chart_values <- reactiveVal(
    list()
  )
  
  page_chart_types <- reactiveVal(
    character()
  )
  
  page_chart_matrices <- reactiveVal(
    character()
  )
  
  page_card_values <- reactiveVal(
    list()
  )
  
  page_card_calculations <- reactiveValues()
  
  active_line_chart <- reactiveVal(NULL)
  
  line_chart_data <- reactiveVal(NULL)
  
  page_line_chart_settings <- reactiveValues()
  
  ### Populate the page dropdown ####
  observe({
    req(folder())
    
    navigation <- config_file()$navigation
    
    default_hrefs <- c(
      "index.html",
      "page.html",
      "user-notes.html"
    )
    
    custom_pages <- navigation[
      !navigation$href %in% default_hrefs,
      c("href", "text"),
      drop = FALSE
    ]
    
    choices <- stats::setNames(
      custom_pages$href,
      custom_pages$text
    )
    
    current_selection <- isolate(
      input$page_design_page
    )
    
    selected <- if (
      !is.null(current_selection) &&
      current_selection %in% custom_pages$href
    ) {
      current_selection
    } else if (nrow(custom_pages) > 0) {
      custom_pages$href[1]
    } else {
      character()
    }
    
    updateSelectInput(
      session = session,
      inputId = "page_design_page",
      choices = choices,
      selected = selected
    )
  })
  
  ### Render the page design interface ####
  output$page_design_interface <- renderUI({
    
    req(folder())
    
    page_href <- input$page_design_page
    
    if (
      is.null(page_href) ||
      !nzchar(page_href)
    ) {
      return(
        tags$em(
          "Create a dashboard page in Dashboard settings before using Page design."
        )
      )
    }
    
    tagList(
      tags$hr(),
      
      h3("Clear content"),
      
      tags$p(
        paste(
          "Clear the example strapline, page cards",
          "and example JavaScript values."
        )
      ),
      
      actionButton(
        inputId = "clear_page_content",
        label = "Clear content",
        icon = icon("eraser"),
        class = "btn-danger"
      ),
      
      tags$hr(),
      
      h3("Edit strapline"),
      
      tags$p(
        "Enter a short line describing the content of this page."
      ),
      
      textInput(
        inputId = "page_design_strapline",
        label = "Page strapline",
        value = "",
        width = "100%"
      ),
      
      actionButton(
        inputId = "save_page_design_strapline",
        label = "Save strapline",
        icon = icon("save"),
        class = "btn-primary"
      ),
      
      tags$hr(),
      
      h3("Edit cards"),
      
      tags$p(
        paste(
          "Use between four and six cards to communicate",
          "the key messages for this page."
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
            inputId = "page_design_card_count",
            label = "Number of cards",
            value = 6,
            min = 4,
            max = 6,
            step = 1,
            width = "100%"
          )
        ),
        
        tags$div(
          style = "margin-bottom: 15px;",
          
          actionButton(
            inputId = "save_page_design_card_count",
            label = "Save",
            icon = icon("save"),
            class = "btn-primary"
          )
        )
      ),
      
      tags$div(
        style = "margin-top: 20px;",
        uiOutput("page_card_editors")
      ),
      
      tags$hr(),
      
      h3("Edit charts"),
      
      tags$p(
        paste(
          "Add up to three chart cards to this page.",
          "Each chart can be configured separately below."
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
            inputId = "page_design_chart_count",
            label = "Number of charts",
            value = page_chart_editor_count(),
            min = 1,
            max = 3,
            step = 1,
            width = "100%"
          )
        ),
        
        tags$div(
          style = "margin-bottom: 15px;",
          
          actionButton(
            inputId = "save_page_design_chart_count",
            label = "Save",
            icon = icon("save"),
            class = "btn-primary"
          )
        )
      ),
      
      tags$div(
        style = "margin-top: 20px;",
        uiOutput("page_chart_editors")
      )
    )
  })
  
  ### Load the selected page ####
  observe({
    req(folder())
    req(selected_page_design())
    
    page_design_version()
    
    values <- tryCatch(
      read_page_card_values(
        project_root = folder(),
        page_href = selected_page_design()
      ),
      error = function(error) {
        
        showNotification(
          paste(
            "Existing page card content could not be read:",
            conditionMessage(error)
          ),
          type = "error",
          duration = NULL
        )
        
        list()
      }
    )
    
    page_card_values(
      values
    )
    
    page_card_editor_count(
      length(values)
    )
    
    strapline <- tryCatch(
      read_page_strapline(
        project_root = folder(),
        page_href = selected_page_design()
      ),
      error = function(error) {
        ""
      }
    )
    
    updateTextInput(
      session = session,
      inputId = "page_design_strapline",
      value = strapline
    )
    
    if (length(values) > 0) {
      updateNumericInput(
        session = session,
        inputId = "page_design_card_count",
        value = length(values)
      )
    }
    
    chart_count <- tryCatch(
      count_page_charts(
        project_root = folder(),
        page_href = selected_page_design()
      ),
      error = function(error) {
        showNotification(
          paste(
            "Existing page charts could not be read:",
            conditionMessage(error)
          ),
          type = "error",
          duration = NULL
        )
        
        0L
      }
    )
    
    page_chart_editor_count(
      chart_count
    )
    
    updateNumericInput(
      session = session,
      inputId = "page_design_chart_count",
      value = chart_count
    )
    
    chart_values <- tryCatch(
      read_page_chart_titles(
        project_root = folder(),
        page_href = selected_page_design()
      ),
      error = function(error) {
        
        showNotification(
          paste(
            "Existing chart titles could not be read:",
            conditionMessage(error)
          ),
          type = "error",
          duration = NULL
        )
        
        list()
      }
    )
    
    page_chart_values(
      chart_values
    )
    
    chart_types <- tryCatch(
      read_page_chart_types(
        project_root = folder(),
        page_href = selected_page_design()
      ),
      error = function(error) {
        showNotification(
          paste(
            "Existing chart types could not be read:",
            conditionMessage(error)
          ),
          type = "error",
          duration = NULL
        )
        
        character()
      }
    )
    
    page_chart_types(
      chart_types
    )
    
    chart_matrices <- tryCatch(
      read_page_chart_matrices(
        project_root = folder(),
        page_href = selected_page_design()
      ),
      error = function(error) {
        
        showNotification(
          paste(
            "Existing chart datasets could not be read:",
            conditionMessage(error)
          ),
          type = "error",
          duration = NULL
        )
        
        character()
      }
    )
    
    page_chart_matrices(
      chart_matrices
    )
  })
  
  ### Clear and strapline observers ####
  observeEvent(input$clear_page_content, {
    
    req(selected_page_design())
    
    showModal(
      modalDialog(
        title = "Clear page content",
        
        tags$p(
          paste(
            "This will clear the page strapline, headline cards,",
            "chart cards and their generated JavaScript."
          )
        ),
        
        tags$p(
          tags$strong(
            "Use Git to restore the previous content if required."
          )
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            inputId = "confirm_clear_page_content",
            label = "Clear content",
            icon = icon("eraser"),
            class = "btn-danger"
          )
        ),
        
        easyClose = FALSE
      )
    )
  })
  
  
  observeEvent(input$confirm_clear_page_content, {
    
    req(folder())
    req(selected_page_design())
    
    tryCatch(
      {
        clear_page_design_files(
          project_root = folder(),
          page_href = selected_page_design()
        )
        
        for (card_number in seq_len(6)) {
          page_card_calculations[[
            as.character(card_number)
          ]] <- NULL
        }
        
        updateTextInput(
          session = session,
          inputId = "page_design_strapline",
          value = ""
        )
        
        page_design_version(
          page_design_version() + 1
        )
        
        removeModal()
        
        showNotification(
          "Page content cleared.",
          type = "message"
        )
      },
      error = function(error) {
        
        showNotification(
          paste(
            "Page content could not be cleared:",
            conditionMessage(error)
          ),
          type = "error",
          duration = NULL
        )
      }
    )
  })
  
  
  observeEvent(input$save_page_design_strapline, {
    
    req(folder())
    req(selected_page_design())
    
    tryCatch(
      {
        update_page_strapline(
          project_root = folder(),
          page_href = selected_page_design(),
          strapline = input$page_design_strapline
        )
        
        showNotification(
          "Page strapline updated.",
          type = "message"
        )
      },
      error = function(error) {
        
        showNotification(
          paste(
            "Page strapline could not be updated:",
            conditionMessage(error)
          ),
          type = "error",
          duration = NULL
        )
      }
    )
  })
  
  ### Save card count ####
  observeEvent(input$save_page_design_card_count, {
    
    req(folder())
    req(selected_page_design())
    
    requested_count <- as.integer(
      input$page_design_card_count
    )
    
    if (
      is.na(requested_count) ||
      requested_count < 4 ||
      requested_count > 6
    ) {
      showNotification(
        "Choose between 4 and 6 cards.",
        type = "error"
      )
      
      return()
    }
    
    tryCatch(
      {
        update_page_card_count(
          project_root = folder(),
          page_href = selected_page_design(),
          card_count = requested_count
        )
        
        page_design_version(
          page_design_version() + 1
        )
        
        showNotification(
          paste0(
            "Page updated to ",
            requested_count,
            " cards."
          ),
          type = "message"
        )
      },
      error = function(error) {
        
        showNotification(
          paste(
            "The page cards could not be updated:",
            conditionMessage(error)
          ),
          type = "error",
          duration = NULL
        )
      }
    )
  })
  
  ### Page card accordions ####
  output$page_card_editors <- renderUI({
    
    req(selected_page_design())
    
    card_count <- page_card_editor_count()
    
    if (card_count < 1) {
      return(
        tags$em(
          "No page cards were found."
        )
      )
    }
    
    values <- page_card_values()
    
    accordion_id <- "page-card-accordion"
    
    tags$div(
      id = accordion_id,
      class = "panel-group",
      role = "tablist",
      
      lapply(
        seq_len(card_count),
        function(card_number) {
          
          current <- if (
            card_number <= length(values)
          ) {
            values[[card_number]]
          } else {
            list(
              top_line = "",
              value = "",
              unit = "",
              bottom_line = "",
              background = "blue"
            )
          }
          
          pending <- page_card_calculations[[
            as.character(card_number)
          ]]
          
          if (!is.null(pending)) {
            current$value <- format_card_calculation_value(
              value = pending$raw_value,
              decimal_places = pending$decimal_places,
              comma_separator = pending$comma_separator
            )
          }
          
          collapse_id <- paste0(
            "page-card-collapse-",
            card_number
          )
          
          is_open <- card_number == 1
          
          tags$div(
            class = "panel panel-default",
            
            tags$div(
              class = "panel-heading",
              
              tags$h4(
                class = "panel-title",
                
                tags$a(
                  class = if (is_open) "" else "collapsed",
                  href = "javascript:void(0);",
                  `data-toggle` = "collapse",
                  `data-parent` = paste0(
                    "#",
                    accordion_id
                  ),
                  `data-target` = paste0(
                    "#",
                    collapse_id
                  ),
                  
                  paste(
                    "Card",
                    card_number
                  ),
                  
                  tags$span(
                    class = "pull-right",
                    icon("chevron-down")
                  )
                )
              )
            ),
            
            tags$div(
              id = collapse_id,
              class = paste(
                "panel-collapse collapse",
                if (is_open) "in" else ""
              ),
              
              tags$div(
                class = "panel-body",
                
                tags$div(
                  class = "alert alert-info",
                  
                  tags$p(
                    paste(
                      "Use these fields to construct a short,",
                      "user-friendly sentence communicating one key message."
                    )
                  ),
                  
                  tags$p(
                    style = "margin-bottom: 0;",
                    
                    "Dynamic year tags: ",
                    tags$code("<<latest-year>>"),
                    ", ",
                    tags$code("<<last-year>>"),
                    " and ",
                    tags$code("<<first-year>>"),
                    "."
                  )
                ),
                
                textInput(
                  inputId = paste0(
                    "page_card_",
                    card_number,
                    "_top_line"
                  ),
                  label = "Top line",
                  value = current$top_line,
                  width = "100%"
                ),
                
                tags$div(
                  style = paste(
                    "display: flex;",
                    "align-items: flex-end;",
                    "gap: 12px;"
                  ),
                  
                  tags$div(
                    style = "flex: 2;",
                    
                    tags$div(
                      class = "form-group",
                      
                      tags$label("Value"),
                      
                      tags$div(
                        class = "input-group",
                        
                        tags$input(
                          id = paste0(
                            "page_card_",
                            card_number,
                            "_value"
                          ),
                          type = "text",
                          class = "form-control",
                          value = current$value,
                          readonly = "readonly",
                          placeholder = "No value calculated"
                        ),
                        
                        tags$span(
                          class = "input-group-btn",
                          
                          actionButton(
                            inputId = paste0(
                              "calculate_page_card_",
                              card_number
                            ),
                            label = "Calculate",
                            icon = icon("calculator"),
                            class = "btn-default"
                          )
                        )
                      )
                    )
                  ),
                  
                  tags$div(
                    style = "flex: 1;",
                    
                    textInput(
                      inputId = paste0(
                        "page_card_",
                        card_number,
                        "_unit"
                      ),
                      label = "Unit",
                      value = current$unit,
                      width = "100%"
                    )
                  )
                ),
                
                textInput(
                  inputId = paste0(
                    "page_card_",
                    card_number,
                    "_bottom_line"
                  ),
                  label = "Bottom line",
                  value = current$bottom_line,
                  width = "100%"
                ),
                
                selectInput(
                  inputId = paste0(
                    "page_card_",
                    card_number,
                    "_background"
                  ),
                  label = "Background",
                  choices = c(
                    "Blue" = "blue",
                    "Navy" = "navy"
                  ),
                  selected = current$background,
                  width = "100%"
                ),
                
                actionButton(
                  inputId = paste0(
                    "save_page_card_",
                    card_number
                  ),
                  label = "Save card changes",
                  icon = icon("save"),
                  class = "btn-primary"
                )
              )
            )
          )
        }
      )
    )
  })
  
  lapply(
    seq_len(6),
    function(card_number) {
      
      local({
        
        current_card <- card_number
        
        observeEvent(
          input[[
            paste0(
              "save_page_card_",
              current_card
            )
          ]],
          {
            req(folder())
            req(selected_page_design())
            
            if (
              current_card >
              page_card_editor_count()
            ) {
              return()
            }
            
            stored_calculation <- page_card_calculations[[
              as.character(current_card)
            ]]
            
            if (is.null(stored_calculation)) {
              showNotification(
                paste0(
                  "Calculate a value for card ",
                  current_card,
                  " before saving."
                ),
                type = "error"
              )
              
              return()
            }
            
            top_line <- input[[
              paste0(
                "page_card_",
                current_card,
                "_top_line"
              )
            ]]
            
            unit <- input[[
              paste0(
                "page_card_",
                current_card,
                "_unit"
              )
            ]]
            
            bottom_line <- input[[
              paste0(
                "page_card_",
                current_card,
                "_bottom_line"
              )
            ]]
            
            background <- input[[
              paste0(
                "page_card_",
                current_card,
                "_background"
              )
            ]]
            
            tryCatch(
              {
                update_page_card_body(
                  project_root = folder(),
                  page_href = selected_page_design(),
                  card_number = current_card,
                  top_line = top_line,
                  unit = unit,
                  bottom_line = bottom_line,
                  background = background
                )
                
                update_page_card_value_js(
                  project_root = folder(),
                  page_href = selected_page_design(),
                  card_number = current_card,
                  calculation = stored_calculation
                )
                
                page_card_values(
                  read_page_card_values(
                    project_root = folder(),
                    page_href = selected_page_design()
                  )
                )
                
                showNotification(
                  paste0(
                    "Page card ",
                    current_card,
                    " updated."
                  ),
                  type = "message"
                )
              },
              error = function(error) {
                showNotification(
                  paste(
                    "Page card could not be updated:",
                    conditionMessage(error)
                  ),
                  type = "error",
                  duration = NULL
                )
              }
            )
          },
          ignoreInit = TRUE
        )
      })
    }
  )
  
  ### Render the chart accordions ####
  output$page_chart_editors <- renderUI({
    
    req(selected_page_design())
    
    chart_count <- page_chart_editor_count()
    
    chart_values <- page_chart_values()
    
    chart_types <- page_chart_types()
    
    chart_matrices <- page_chart_matrices()
    
    files <- loaded_table_files()
    metadata <- loaded_table_metadata()
    
    matrix_codes <- tools::file_path_sans_ext(
      basename(files)
    )
    
    matrix_labels <- vapply(
      matrix_codes,
      function(matrix) {
        
        table_metadata <- metadata[[matrix]]
        
        label <- if (
          !is.null(table_metadata) &&
          !is.null(table_metadata$label) &&
          length(table_metadata$label) > 0 &&
          nzchar(as.character(table_metadata$label)[1])
        ) {
          as.character(table_metadata$label)[1]
        } else {
          matrix
        }
        
        paste0(
          label,
          " (",
          matrix,
          ")"
        )
      },
      character(1)
    )
    
    matrix_choices <- c(
      "Select data" = "",
      stats::setNames(
        matrix_codes,
        matrix_labels
      )
    )
    
    if (
      length(chart_count) != 1 ||
      is.na(chart_count) ||
      chart_count < 1
    ) {
      return(
        tags$em(
          "No charts have been added to this page."
        )
      )
    }
    
    accordion_id <- "page-chart-accordion"
    
    tags$div(
      id = accordion_id,
      class = "panel-group",
      role = "tablist",
      
      lapply(
        seq_len(chart_count),
        function(chart_number) {
          
          current_title <- if (
            chart_number <= length(chart_values)
          ) {
            chart_values[[chart_number]]
          } else {
            ""
          }
          
          if (
            is.null(current_title) ||
            length(current_title) == 0
          ) {
            current_title <- ""
          }
          
          current_title <- as.character(
            current_title
          )[1]
          
          current_type <- if (
            chart_number <= length(chart_types)
          ) {
            chart_types[chart_number]
          } else {
            ""
          }
          
          if (
            is.na(current_type) ||
            is.null(current_type)
          ) {
            current_type <- ""
          }
          
          current_matrix <- if (
            chart_number <= length(chart_matrices)
          ) {
            chart_matrices[chart_number]
          } else {
            ""
          }
          
          if (
            is.na(current_matrix) ||
            is.null(current_matrix)
          ) {
            current_matrix <- ""
          }
          
          collapse_id <- paste0(
            "page-chart-collapse-",
            chart_number
          )
          
          heading_id <- paste0(
            "page-chart-heading-",
            chart_number
          )
          
          is_open <- chart_number == 1
          
          tags$div(
            class = "panel panel-default",
            
            tags$div(
              id = heading_id,
              class = "panel-heading",
              role = "tab",
              
              tags$h4(
                class = "panel-title",
                
                tags$a(
                  class = if (is_open) {
                    ""
                  } else {
                    "collapsed"
                  },
                  
                  href = "javascript:void(0);",
                  
                  `data-toggle` = "collapse",
                  
                  `data-parent` = paste0(
                    "#",
                    accordion_id
                  ),
                  
                  `data-target` = paste0(
                    "#",
                    collapse_id
                  ),
                  
                  `aria-expanded` = if (is_open) {
                    "true"
                  } else {
                    "false"
                  },
                  
                  `aria-controls` = collapse_id,
                  
                  paste(
                    "Chart",
                    chart_number
                  ),
                  
                  tags$span(
                    class = "pull-right",
                    icon("chevron-down")
                  )
                )
              )
            ),
            
            tags$div(
              id = collapse_id,
              
              class = paste(
                "panel-collapse collapse",
                if (is_open) {
                  "in"
                } else {
                  ""
                }
              ),
              
              role = "tabpanel",
              
              `aria-labelledby` = heading_id,
              
              tags$div(
                class = "panel-body",
                
                tags$div(
                  class = "alert alert-info",
                  
                  tags$p(
                    style = "margin-bottom: 0;",
                    
                    paste(
                      "Configure the chart title, type and data.",
                      "The chart preview and detailed data options",
                      "will be added in the next stage."
                    )
                  )
                ),
                
                textInput(
                  inputId = paste0(
                    "page_chart_",
                    chart_number,
                    "_title"
                  ),
                  label = "Chart title",
                  value = current_title,
                  width = "100%",
                  placeholder = "Enter a clear chart title"
                ),
                
                tags$small(
                  class = "help-block",
                  style = "margin-top: -8px;",
                  
                  "Dynamic year tags: ",
                  tags$code("<<latest-year>>"),
                  ", ",
                  tags$code("<<last-year>>"),
                  " and ",
                  tags$code("<<first-year>>"),
                  "."
                ),
                
                selectInput(
                  inputId = paste0(
                    "page_chart_",
                    chart_number,
                    "_type"
                  ),
                  label = "Chart type",
                  choices = c(
                    "Select a chart type" = "",
                    page_chart_type_choices()
                  ),
                  selected = current_type,
                  width = "100%"
                ),
                
                selectInput(
                  inputId = paste0(
                    "page_chart_",
                    chart_number,
                    "_matrix"
                  ),
                  label = "Data Portal table",
                  choices = matrix_choices,
                  selected = current_matrix,
                  width = "100%"
                ),
                
                tags$div(
                  style = "margin-top: 15px;",
                  uiOutput(
                    paste0(
                      "page_chart_",
                      chart_number,
                      "_type_options"
                    )
                  )
                ),
                
                tags$div(
                  class = "well well-sm",
                  
                  tags$strong("Chart preview"),
                  
                  tags$p(
                    style = paste(
                      "margin-top: 8px;",
                      "margin-bottom: 0;"
                    ),
                    
                    "A chart preview will appear here."
                  )
                ),
                
                actionButton(
                  inputId = paste0(
                    "save_page_chart_",
                    chart_number
                  ),
                  label = "Save chart changes",
                  icon = icon("save"),
                  class = "btn-primary"
                )
              )
            )
          )
        }
      )
    )
  })
  
  ### Add chart-title save observers ####
  lapply(
    seq_len(3),
    function(chart_number) {
      
      local({
        
        current_chart <- chart_number
        
        observeEvent(
          input[[
            paste0(
              "save_page_chart_",
              current_chart
            )
          ]],
          {
            req(folder())
            req(selected_page_design())
            
            if (
              current_chart >
              page_chart_editor_count()
            ) {
              return()
            }
            
            chart_title <- input[[
              paste0(
                "page_chart_",
                current_chart,
                "_title"
              )
            ]]
            
            if (is.null(chart_title)) {
              chart_title <- ""
            }
            
            chart_type <- input[[
              paste0(
                "page_chart_",
                current_chart,
                "_type"
              )
            ]]
            
            if (is.null(chart_type)) {
              chart_type <- ""
            }
            
            if (chart_type == "line") {
              line_settings <- page_line_chart_settings[[
                as.character(current_chart)
              ]]
              
              if (is.null(line_settings)) {
                showNotification(
                  paste0(
                    "Configure the lines for chart ",
                    current_chart,
                    " before saving."
                  ),
                  type = "error"
                )
                
                return()
              }
              
              year_mode <- input[[
                paste0(
                  "page_chart_",
                  current_chart,
                  "_year_mode"
                )
              ]]
              
              recent_years <- input[[
                paste0(
                  "page_chart_",
                  current_chart,
                  "_recent_years"
                )
              ]]
              
              selected_years <- select_line_chart_years(
                available_years = line_settings$available_years,
                mode = year_mode,
                recent_years = recent_years
              )
              
              line_chart_js <- build_page_line_chart_js(
                chart_number = current_chart,
                matrix = line_settings$matrix,
                year_column = line_settings$year_column,
                years = selected_years,
                filters = line_settings$filters,
                columns = line_settings$columns
              )
            }
            
            matrix <- input[[
              paste0(
                "page_chart_",
                current_chart,
                "_matrix"
              )
            ]]
            
            if (is.null(matrix)) {
              matrix <- ""
            }
            
            tryCatch(
              {
                update_page_chart_title(
                  project_root = folder(),
                  page_href = selected_page_design(),
                  chart_number = current_chart,
                  chart_title = chart_title
                )
                
                update_page_chart_type(
                  project_root = folder(),
                  page_href = selected_page_design(),
                  chart_number = current_chart,
                  chart_type = chart_type
                )
                
                update_page_chart_matrix(
                  project_root = folder(),
                  page_href = selected_page_design(),
                  chart_number = current_chart,
                  matrix = matrix
                )
                
                refreshed_titles <- read_page_chart_titles(
                  project_root = folder(),
                  page_href = selected_page_design()
                )
                
                page_chart_values(
                  refreshed_titles
                )
                
                page_chart_types(
                  read_page_chart_types(
                    project_root = folder(),
                    page_href = selected_page_design()
                  )
                )
                
                page_chart_matrices(
                  read_page_chart_matrices(
                    project_root = folder(),
                    page_href = selected_page_design()
                  )
                )
                
                showNotification(
                  paste0(
                    "Chart ",
                    current_chart,
                    " title updated."
                  ),
                  type = "message"
                )
              },
              error = function(error) {
                showNotification(
                  paste(
                    "Chart title could not be updated:",
                    conditionMessage(error)
                  ),
                  type = "error",
                  duration = NULL
                )
              }
            )
          },
          ignoreInit = TRUE
        )
      })
    }
  )
  
  ### Connect the configure lines buttons ####
  lapply(
    seq_len(3),
    function(chart_number) {
      local({
        current_chart <- chart_number
        
        observeEvent(
          input[[
            paste0(
              "configure_page_chart_lines_",
              current_chart
            )
          ]],
          {
            req(folder())
            req(selected_page_design())
            
            matrix <- input[[
              paste0(
                "page_chart_",
                current_chart,
                "_matrix"
              )
            ]]
            
            req(matrix)
            req(nzchar(matrix))
            
            chart_type <- input[[
              paste0(
                "page_chart_",
                current_chart,
                "_type"
              )
            ]]
            
            req(identical(chart_type, "line"))
            
            result <- tryCatch(
              find_line_chart_year_definition(
                project_root = folder(),
                matrix = matrix
              ),
              error = function(error) {
                showNotification(
                  paste(
                    "The line chart data could not be loaded:",
                    conditionMessage(error)
                  ),
                  type = "error",
                  duration = NULL
                )
                
                NULL
              }
            )
            
            req(result)
            
            active_line_chart(
              current_chart
            )
            
            line_chart_data(
              result
            )
            
            showModal(
              modalDialog(
                title = paste(
                  "Configure lines for chart",
                  current_chart
                ),
                
                tags$p(
                  paste(
                    "Choose the value columns to display as lines.",
                    "Use the filters to restrict the rows included",
                    "in every selected line."
                  )
                ),
                
                uiOutput(
                  "line_chart_filters_ui"
                ),
                
                footer = tagList(
                  modalButton("Cancel"),
                  
                  actionButton(
                    inputId = "finish_line_chart_configuration",
                    label = "Done",
                    class = "btn-primary"
                  )
                ),
                
                size = "l",
                easyClose = FALSE
              )
            )
          },
          ignoreInit = TRUE
        )
      })
    }
  )
  
  ### Render the line selection modal ####
  output$line_chart_filters_ui <- renderUI({
    chart_data <- line_chart_data()
    
    req(chart_data)
    
    calculation_data <- chart_data$calculation_data
    
    filter_inputs <- lapply(
      calculation_data$row_filters,
      function(filter_definition) {
        
        # Year range is handled separately in the accordion,
        # so do not duplicate the time filter here.
        if (isTRUE(filter_definition$is_year)) {
          return(NULL)
        }
        
        selectizeInput(
          inputId = paste0(
            "line_chart_",
            filter_definition$input_id
          ),
          label = filter_definition$label,
          choices = filter_definition$choices,
          selected = character(),
          multiple = TRUE,
          options = list(
            plugins = list(
              "remove_button"
            ),
            placeholder = "No filter"
          ),
          width = "100%"
        )
      }
    )
    
    tagList(
      filter_inputs,
      
      selectizeInput(
        inputId = "line_chart_columns",
        label = calculation_data$pivot_label,
        choices = calculation_data$pivot_columns,
        selected = character(),
        multiple = TRUE,
        options = list(
          plugins = list(
            "remove_button"
          ),
          placeholder = "Select one or more lines"
        ),
        width = "100%"
      )
    )
  })
  
  #### Store the modal result when Done is clicked ####
  observeEvent(
    input$finish_line_chart_configuration,
    {
      chart_number <- active_line_chart()
      chart_data <- line_chart_data()
      
      req(chart_number)
      req(chart_data)
      
      selected_columns <- input$line_chart_columns
      
      if (
        is.null(selected_columns) ||
        length(selected_columns) == 0
      ) {
        showNotification(
          "Select at least one line.",
          type = "error"
        )
        
        return()
      }
      
      calculation_data <- chart_data$calculation_data
      
      selected_filters <- list()
      
      for (
        filter_definition in
        calculation_data$row_filters
      ) {
        if (isTRUE(filter_definition$is_year)) {
          next
        }
        
        input_id <- paste0(
          "line_chart_",
          filter_definition$input_id
        )
        
        selected_values <- input[[
          input_id
        ]]
        
        if (
          !is.null(selected_values) &&
          length(selected_values) > 0
        ) {
          selected_filters[[
            filter_definition$column
          ]] <- as.character(
            selected_values
          )
        }
      }
      
      year_mode <- input[[
        paste0(
          "page_chart_",
          chart_number,
          "_year_mode"
        )
      ]]
      
      recent_years <- input[[
        paste0(
          "page_chart_",
          chart_number,
          "_recent_years"
        )
      ]]
      
      if (
        is.null(year_mode) ||
        !year_mode %in% c(
          "all",
          "recent"
        )
      ) {
        year_mode <- "all"
      }
      
      if (
        is.null(recent_years) ||
        is.na(recent_years)
      ) {
        recent_years <- 5L
      }
      
      page_line_chart_settings[[
        as.character(chart_number)
      ]] <- list(
        matrix = chart_data$calculation_data$matrix,
        year_column = chart_data$column,
        available_years = chart_data$values,
        year_mode = year_mode,
        recent_years = as.integer(recent_years),
        filters = selected_filters,
        columns = as.character(
          selected_columns
        )
      )
      
      summary_text <- paste(
        selected_columns,
        collapse = ", "
      )
      
      value_input_id <- paste0(
        "page_chart_",
        chart_number,
        "_lines_summary"
      )
      
      shinyjs::runjs(
        sprintf(
          "$('#%s').val(%s);",
          value_input_id,
          jsonlite::toJSON(
            summary_text,
            auto_unbox = TRUE
          )
        )
      )
      
      removeModal()
    }
  )
  
  ### Temporary count button observer ####
  observeEvent(
    input$save_page_design_chart_count,
    {
      
      requested_count <- as.integer(
        input$page_design_chart_count
      )
      
      if (
        length(requested_count) != 1 ||
        is.na(requested_count) ||
        requested_count < 0 ||
        requested_count > 3
      ) {
        showNotification(
          "Choose between 0 and 3 charts.",
          type = "error"
        )
        
        return()
      }
      
      observeEvent(
        input$save_page_design_chart_count,
        {
          
          req(folder())
          req(selected_page_design())
          
          requested_count <- as.integer(
            input$page_design_chart_count
          )
          
          if (
            length(requested_count) != 1 ||
            is.na(requested_count) ||
            requested_count < 0 ||
            requested_count > 3
          ) {
            showNotification(
              "Choose between 0 and 3 charts.",
              type = "error"
            )
            
            return()
          }
          
          tryCatch(
            {
              update_page_chart_count(
                project_root = folder(),
                page_href = selected_page_design(),
                chart_count = requested_count
              )
              
              actual_count <- count_page_charts(
                project_root = folder(),
                page_href = selected_page_design()
              )
              
              page_chart_editor_count(
                actual_count
              )
              
              page_chart_values(
                read_page_chart_titles(
                  project_root = folder(),
                  page_href = selected_page_design()
                )
              )
              
              updateNumericInput(
                session = session,
                inputId = "page_design_chart_count",
                value = actual_count
              )
              
              page_design_version(
                page_design_version() + 1
              )
              
              showNotification(
                if (actual_count == 1) {
                  "Page updated to 1 chart."
                } else {
                  paste0(
                    "Page updated to ",
                    actual_count,
                    " charts."
                  )
                },
                type = "message"
              )
            },
            error = function(error) {
              
              showNotification(
                paste(
                  "The page charts could not be updated:",
                  conditionMessage(error)
                ),
                type = "error",
                duration = NULL
              )
            }
          )
        },
        ignoreInit = TRUE
      )
    }
  )
  
  ## Render UI for line charts ####
  lapply(
    seq_len(3),
    function(chart_number) {
      
      local({
        
        current_chart <- chart_number
        
        output_id <- paste0(
          "page_chart_",
          current_chart,
          "_type_options"
        )
        
        output[[output_id]] <- renderUI({
          
          chart_type <- input[[
            paste0(
              "page_chart_",
              current_chart,
              "_type"
            )
          ]]
          
          matrix <- input[[
            paste0(
              "page_chart_",
              current_chart,
              "_matrix"
            )
          ]]
          
          if (
            is.null(chart_type) ||
            !identical(chart_type, "line") ||
            is.null(matrix) ||
            !nzchar(matrix)
          ) {
            return(NULL)
          }
          
          existing_settings <- page_line_chart_settings[[
            as.character(current_chart)
          ]]
          
          year_mode <- if (
            !is.null(existing_settings) &&
            !is.null(existing_settings$year_mode)
          ) {
            existing_settings$year_mode
          } else {
            "all"
          }
          
          recent_years <- if (
            !is.null(existing_settings) &&
            !is.null(existing_settings$recent_years)
          ) {
            existing_settings$recent_years
          } else {
            5L
          }
          
          lines_summary <- if (
            !is.null(existing_settings) &&
            !is.null(existing_settings$columns) &&
            length(existing_settings$columns) > 0
          ) {
            paste(
              existing_settings$columns,
              collapse = ", "
            )
          } else {
            ""
          }
          
          tagList(
            
            tags$hr(),
            
            h4("Line chart options"),
            
            radioButtons(
              inputId = paste0(
                "page_chart_",
                current_chart,
                "_year_mode"
              ),
              label = "Year range",
              choices = c(
                "All years" = "all",
                "Most recent years" = "recent"
              ),
              selected = year_mode,
              inline = TRUE
            ),
            
            conditionalPanel(
              condition = sprintf(
                "input['page_chart_%d_year_mode'] == 'recent'",
                current_chart
              ),
              
              numericInput(
                inputId = paste0(
                  "page_chart_",
                  current_chart,
                  "_recent_years"
                ),
                label = "Number of most recent years",
                value = recent_years,
                min = 1,
                step = 1,
                width = "220px"
              )
            ),
            
            tags$div(
              class = "form-group",
              
              tags$label("Lines"),
              
              tags$div(
                class = "input-group",
                
                tags$input(
                  id = paste0(
                    "page_chart_",
                    current_chart,
                    "_lines_summary"
                  ),
                  type = "text",
                  class = "form-control",
                  value = lines_summary,
                  readonly = "readonly",
                  placeholder = "No lines configured"
                ),
                
                tags$span(
                  class = "input-group-btn",
                  
                  actionButton(
                    inputId = paste0(
                      "configure_page_chart_lines_",
                      current_chart
                    ),
                    label = "Configure",
                    icon = icon("sliders"),
                    class = "btn-default"
                  )
                )
              )
            )
          )
        })
        
        outputOptions(
          output,
          output_id,
          suspendWhenHidden = FALSE
        )
      })
    }
  )
  
  
}

shinyApp(
  ui = ui,
  server = server,
  options = list(launch.browser = TRUE)
)