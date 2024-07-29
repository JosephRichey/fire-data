box::use(
  shiny[...],
  bslib[...],
)

box::use(
  view/incident_response,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  page_fixed(
    title = Sys.getenv('FD'),
    h1(Sys.getenv('FD')),
    h3("Incident Response"),
    theme = bs_theme(version = 5,
                     secondary = "#87292b",
                     success = "#87292b",
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
    helpText("v0.3.0 dev"),
    helpText("© CC BY-NC-SA 2024 Joseph Richey")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    incident_response$ModalsServer('incident_response')
    incident_response$DBWriteServer('incident_response')
    incident_response$CardServer('incident_response')
    
    options(shiny.error = function() { cat(geterrmessage(), "\n") })
    
  })
}
