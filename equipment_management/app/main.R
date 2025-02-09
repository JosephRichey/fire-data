box::use(
  shiny[...],
  bslib[...],
  data.table[...],
  dplyr[filter, ...],
  odbc[...],
)

box::use(
  app/view/equipment_management,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fixed(
    title = Sys.getenv('FD'),
    h1(Sys.getenv('FD')),
    h3("Equipment Management"),
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
          min_height = "300px",
          equipment_management$UI(ns('equipment_management'))
        )
      ),
      
      card(
        card_body(
          bslib::card_title("Current Status"),
          equipment_management$Output(ns('equipment_management'))
        ),
        min_height = "1200px"
      ),
      
      col_widths = c(12, 12)
    ),
    
    br(),
    helpText("dev-1.0.0 © CC BY-NC-SA 2024 Joseph Richey")
    
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    equipment_management$Server("equipment_management")
    
  })
}
