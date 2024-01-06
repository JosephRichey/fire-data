box::use(
  shiny[...],
  DT[...],
  DBI[...],
  dplyr[...],
)

box::use(
  ../logic/app_data,
  ../logic/functions,
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
      rv <- reactiveVal(functions$CurrentStatusTable(app_data$Attendance,
                                                     app_data$Roster))
      
      output$current_status <- DT::renderDataTable({
        # browser()
        DT::datatable(rv(),
                      options = list(scrollX=TRUE,
                                     fillContainer = TRUE,
                                     row.names = FALSE))
      })
      
      observeEvent(input$refresh, {
        # browser()
        print("Refresh")
        Updated_Attendance <- dbGetQuery(app_data$CON,
                                         "SELECT * FROM cfddb.attendance") |> 
          mutate(check_in = as.POSIXct(check_in),
                 check_out = as.POSIXct(check_out))
          
        rv(functions$CurrentStatusTable(Updated_Attendance,
                                        app_data$Roster))
      })
      
      
    }
  )
}
