box::use(
  shiny[...],
  bslib[...],
  logger[...],
  dplyr[...],
)

box::use(
  view/incident_response,
  view/card,
  logic/app_data,
  logic/global_functions,
  logic/logging,
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  
  page_fluid(
    div(class = "app-header",
        img(src = 'static/logo.png',
            style = 'width: 40px; margin-right: 10px'),
        span(global_functions$GetSetting('global', 'fire_department_name'))),
    br(),
    h3("Incident Response"),
    theme = bs_theme(version = 5,
                     primary = "#87292b",
                     secondary = '#a05050',
                     success = "#2b8764",
                     info = "#377eb8",
                     warning = "#D76F33",
                     danger = "#640064",
                     light = "#565656",
                     bootswatch = "darkly"),
    layout_columns(
      card(
        card_body(
          incident_response$UI(ns('incident_response'))
        )
      ),
      
      col_widths = c(12, 12)
    ),
    
    card$Output(ns('incident_response')),
    
    br(),
    helpText("v1.0.0-beta © CC BY-NC-SA 2024 Joseph Richey")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Display page spinner while loading
    shinycssloaders::showPageSpinner(
      type = 6,
      color = "#87292b"
    )
    
    session$onFlushed(function() {
      shinycssloaders::hidePageSpinner()
    })
    
    ##### Global Stuff ####
    ns <- session$ns
    
    ##### On app load, initialize all reactive values #####
    rdfs <- reactiveValues(
      firefighter_response = NULL,
      apparatus_response = NULL,
      firefighter_apparatus = NULL,
      incident_unit = NULL,
      response = NULL,
      incident = NULL
    )
    
    global_functions$UpdateReactives(rdfs)
    
    ##### Module Servers #####
    card$Server('incident_response', rdfs)
    incident_response$Server('incident_response', rdfs)
    # incident_response$ModalServer('incident_response')
    
    session$onSessionEnded(function() {
      DBI::dbDisconnect(app_data$CON)
      print('DB Disconnected')
    })
    
    # Keep DB connection live
    observe({
      #FIXME This needs to be set so the shiny session can still time out,
      # but the DB connection is still kept alive
      invalidateLater(180000)  # Ping every 3 minutes
      cat('Pinging database...\n', file = stderr())
      tryCatch(
        dbGetQuery(app_data$CON, "SELECT 1"),
        error = function(e) message("Connection ping failed: ", e$message)
      )
    })
    
    # Catch any unhandled errors
    options(shiny.error = function() { logger::log_error(geterrmessage(), "\n") })
    
    
  })
}
