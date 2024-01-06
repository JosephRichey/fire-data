box::use(
  shiny[...],
  DT[...],
  DBI[...],
  dplyr[...],
)

box::use(
  ../logic/app_data,
)


UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns('refresh'), 'Refresh')
  )
}

Output <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("current_status"))
  )
}

Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$current_status <- DT::renderDataTable({
        
        app_data$Attendance |> 
          DT::datatable()
      })
      
      
    }
  )
}
