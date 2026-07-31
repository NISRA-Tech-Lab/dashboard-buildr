library(shiny)
library(shinyFiles)
library(servr)
library(shinyjs)
library(dplyr)
library(V8)
library(DT)

source("R/file_helpers.R", local = TRUE)
source("R/config_helpers.R", local = TRUE)
source("R/page_helpers.R", local = TRUE)

ui <- fluidPage(
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
        div(id = "launch-controls",
            h4("Dashboard location"),
            verbatimTextOutput("path"),
            p("Click below to open dashboard in new tab"),
            actionButton("launch-dashboard", "Launch dashboard")
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dashboard settings",
                 h2("Dashboard settings"),
                 DT::dataTableOutput("config_table")),
        tabPanel("Home page design")
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
          " and enter Matrix codes below."
        ),
        
        actionButton(
          "add_matrix",
          label = NULL,
          icon = icon("plus"),
          class = "btn-success",
          title = "Add Data Portal table"
        ),
        
        tags$div(style = "margin-top:15px;"),
        
        DT::DTOutput("matrix_editor_table"),
        
        footer = tagList(
          modalButton("Cancel")
        ),
        
        size = "l",
        easyClose = FALSE
      )
    )
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
            width = "80px"
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
          modalButton("Cancel"),
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
  
  observeEvent(input$confirm_add_matrix, {
    
    matrix_name <- trimws(input$new_matrix)
    
    if (!nzchar(matrix_name)) {
      
      showNotification(
        "Enter a matrix name.",
        type = "error"
      )
      
      return()
      
    }
    
    matrices <- matrix_draft()
    
    if (matrix_name %in% matrices) {
      
      showNotification(
        "That matrix already exists.",
        type = "error"
      )
      
      return()
      
    }
    
    matrices <- c(
      matrices,
      matrix_name
    )
    
    matrix_draft(matrices)
    
    removeModal()
    
    ## reopen editor
    
    showModal(
      modalDialog(
        title = "Edit Data Portal tables",
        
        tags$p(
          "Data Portal tables are identified by their matrix name. ",
          tags$a(
            href = "https://data.nisra.gov.uk/",
            target = "_blank",
            rel = "noopener noreferrer",
            "Open the NISRA Data Portal"
          ),
          "."
        ),
        
        actionButton(
          "add_matrix",
          label = NULL,
          icon = icon("plus"),
          class = "btn-success"
        ),
        
        tags$div(style = "margin-top:15px;"),
        
        DTOutput("matrix_editor_table"),
        
        footer = tagList(
          modalButton("Cancel")
        ),
        
        size = "l",
        easyClose = FALSE
      )
    )
    
  })
  
  observeEvent(input$delete_matrix, {
    
    matrices <- matrix_draft()
    
    matrices <- matrices[-input$delete_matrix]
    
    matrix_draft(matrices)
    
  })
  
  
}

shinyApp(
  ui = ui,
  server = server,
  options = list(launch.browser = TRUE)
)