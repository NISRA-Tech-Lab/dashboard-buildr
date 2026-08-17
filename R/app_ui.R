#' Dashboard BuildR user interface
#'
#' @return A Shiny UI definition.
#' @keywords internal
app_ui <- function() {
  
  fluidPage(
    theme = shinytheme("cosmo"),
    useShinyjs(),
    
    tags$head(
      tags$link(
        rel = "icon",
        type = "image/x-icon",
        href = "buildr-assets/favicon.ico"
      )
    ),
    
    titlePanel("NISRA Dashboard BuildR"),
    
    sidebarLayout(
      sidebarPanel(
        
        h3("Getting started"),
        
        HTML("<p>
                You will need a local copy of the NISRA Dashboard Template to use this interface</p>
                <p> See
                <a href='https://github.com/NISRA-Tech-Lab/dashboard-template#5-getting-started' target='_blank'>this link</a>
                for help on getting started. 
             </p>"),
        
        h3("Choose dashboard directory"),
        
        p("Click the button below and navigate to your saved local copy of the Template in order to begin building."),
        
        shinyDirButton(
          id = "folder",
          label = "Browse",
          title = "Choose a folder",
          class = "btn-primary"
        ),
        
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
          
          tabPanel("User notes",
                   p("Enter user notes for this dashboard. You can use HTML for paragraphs, links,
                   lists and other formatting."),
                   p("User notes should include:"),
                   HTML('<ul class="mt-2">
                        <li>When this data was released. Expectations for future updates?</li>
                        <li>Data source(s)</li>
                        <li>Where data can be downloaded</li>
                        <li>Any missing or suppressed data clearly explained</li>
                        <li>Quality assurance information</li>
                        <li>Contact information</li>
                        <li>Link to accessibility statement</li>
                        <li>Link to publication and data pages</li>
                    </ul>'),
                   textAreaInput(
                     "user_notes_html",
                     "User notes HTML",
                     value = "",
                     rows = 20,
                     width = "100%"
                   ),
                   
                   actionButton(
                     "save_user_notes",
                     "Save user notes"
                   )         
          )
        )
      )
    )
  )
  
}