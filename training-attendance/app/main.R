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
  # app/view/current_status,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fixed(
    title = "Corinne Fire Department",
    h1(paste("Corinne Fire Department")),
    h3("Training Attendance"),
    theme = bs_theme(version = 5,
                     secondary = "#87292b",
                     success = "#87292b",
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
      col_widths = c(12, 12)
    ),
    
    br(),
    helpText("v0.2.2_dev"),
    helpText("© CC BY-NC-SA 2024 Joseph Richey")
    
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    check_in_out$Server("check_in_out")
    
  })
}
