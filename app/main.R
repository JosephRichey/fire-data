box::use(
  shiny[...],
  shinyWidgets[...],
  bslib[...],
  data.table[...],
  dplyr[filter, ...],
  odbc[...],
)

box::use(
  app/logic/functions,
  app/logic/app_data,
  app/modals/modals,
  app/view/check_in_out,
  app/view/current_status,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    title = "Corinne Fire Department",
    h1("Corinne Fire Department"),
    theme = bs_theme(version = 5,
                     success = "#87292b",
                     bootswatch = "darkly"),
    card(
      check_in_out$UI(ns('check_in_out')),
    ),
    
    card(
      bslib::card_title("Current Status"),
      current_status$Output(ns('current_status')),
      current_status$UI(ns('current_status'))
    )
    
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    check_in_out$Server("check_in_out")
    current_status$Server("current_status")
    
    
    
    
    
    
    
  })
}
