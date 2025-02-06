box::use(
  shiny[...],
  bslib[...],
  shinyjs[...],
)
#console.log(shinyjs);
box::use(
  view/incident_response,
  logic/app_data,
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  useShinyjs()
  tagList(
    uiOutput(ns('incident_cards'))
  )
  page_fixed(
    title = Sys.getenv('FD'),
    h1(Sys.getenv('FD')),
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
    
    incident_response$Output(ns('incident_response')),
    
    br(),
    helpText("v1.0.0-dev © CC BY-NC-SA 2024 Joseph Richey")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    incident_response$ModalServer('incident_response')
    incident_response$DBWriteServer('incident_response')
    incident_response$CardServer('incident_response')
    incident_response$UpdateIdServer('incident_response')
    
    session$onSessionEnded(function() {
      DBI::dbDisconnect(app_data$CON)
      print('DB Disconnected')
    })
    
    options(shiny.error = function() { cat(geterrmessage(), "\n") })
    
  })
}
