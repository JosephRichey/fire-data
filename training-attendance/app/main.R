box::use(
  shiny[...],
  shinyWidgets[...],
  bslib[...],
  data.table[...],
  dplyr[filter, ...],
  odbc[...],
)

box::use(
  app/view/check_in_out,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fixed(
    title = Sys.getenv('FD'),
    h1(Sys.getenv('FD')),
    h3("Training Attendance"),
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
          check_in_out$UI(ns('check_in_out'))
        )
      ),
      
      card(
        card_body(
          #min_height = "600px",
          bslib::card_title("Current Status"),
          check_in_out$Output(ns('check_in_out'))
        ),
        min_height = "1200px"
      ),
      check_in_out$Button(ns('check_in_out')),
      
      col_widths = c(12, 12)
    ),
    
    br(),
    helpText("v0.3.0 © CC BY-NC-SA 2024 Joseph Richey")
    
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    check_in_out$Server("check_in_out")
    
  })
}
