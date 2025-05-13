box::use(
  shiny[...],
  bslib[...],
  shinyTime[...],
  lubridate[...],
  data.table[...],
  dplyr[...],
  shinyalert[...],
  DBI[...],
  purrr[...],
  sortable[...],
  htmlwidgets[...],
)

box::use(
  app/logic/app_data,
  app/modal/modal,
)


UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      ns("add_incident"), 
      "Add Incident", 
      class = "btn btn-primary")
  )
}


