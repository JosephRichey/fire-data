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
  app/view/current_status,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fixed(
    title = "Corinne Fire Department",
    h1("Corinne Fire Department"),
    theme = bs_theme(version = 5,
                     secondary = "#87292b",
                     success = "#87292b",
                     bootswatch = "darkly"),
    layout_columns(
      card(
        card_body(
          check_in_out$UI(ns('check_in_out'))
        )
      ),
      
      card(
        card_body(
          min_height = "800px",
          bslib::card_title("Current Status"),
          current_status$Output(ns('current_status')),
          current_status$UI(ns('current_status'))
        )
      ),
      col_widths = c(12, 12)
    ),
    
    br(),
    helpText("v0.1.1")
    
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    check_in_out$Server("check_in_out")
    current_status$Server("current_status")
    
  })
}
