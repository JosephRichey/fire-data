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
      
      #https://stackoverflow.com/questions/61407870/shiny-how-to-refresh-data-loaded-before-server-function
      rv <- reactiveVal(app_data$Attendance)
      
      output$current_status <- DT::renderDataTable({
        # browser()
        DT::datatable(rv(),
                      options = list(scrollX=TRUE, scrollCollapse=TRUE))
      })
      
      observeEvent(input$refresh, {
        # browser()
        print("Refresh")
        updated_attendance <- dbGetQuery(app_data$CON,
                                         "SELECT * FROM cfddb.attendance") |> 
          mutate(check_in = as.POSIXct(check_in),
                 check_out = as.POSIXct(check_out))
          
        rv(updated_attendance)
      })
      
      
    }
  )
}
