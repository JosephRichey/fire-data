box::use(
  shiny[...],
  dplyr[...],
  shinyWidgets[...],
  DT[...],
)

box::use(
  ../logic/app_data,
)


UI <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(
      ns('type'), 
      'Equipment Type', 
      choices = c(app_data$Equipment_Type$equipment_type |> unique()),
      selected = NULL,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        size = 10
      ),
      multiple = TRUE
    ),
    checkboxInput(
      ns('due-soon-filter'),
      'Due Soon',
      value = FALSE
    ),
    checkboxInput(
      ns('overdue-filter'),
      'Overdue',
      value = FALSE
    ),
    actionButton(
      ns('check_all'),
      'Check All',
      icon = icon('check'),
      class = 'btn-primary'
    ),
    actionButton(
      ns('refresh'), 
      'Refresh', 
      icon = icon('refresh'),
      class = 'btn-light'
    ),
    DT::dataTableOutput(ns('equipment'))
  )
}

Output <- function(id) {
  ns <- NS(id)
  tagList(
    DT::renderDataTable(ns('due_soon')),
    DT::renderDataTable(ns('overdue'))
  )
}

Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      r_Equipment <- reactiveVal(app_data$Equipment)
      
      r_Equipment_Type <- reactiveVal(app_data$Equipment_Type)
      
      r_Equipment_Log <- reactiveVal(app_data$Equipment_Log)
      
      r_Data <- reactive({
        
        # browser()
        data <- r_Equipment() |> 
          left_join(r_Equipment_Type(), by = c('type_id' = 'equipment_type_id')) |>
          left_join(app_data$Firefighter, by = c('firefighter_id' = 'firefighter_id'))
        return(data)
      })
      
      output$equipment <- renderDataTable({
        # browser()
        r_Data() |> 
          select(equipment_name, next_check_date, full_name, equipment_type) |>
          filter(equipment_type %in% input$type) |>
          datatable(
            extensions = 'Buttons'
          )
      })
      
      output$due_soon <- renderDataTable({
        browser()
        r_Data() |> 
          filter(next_check_date < Sys.Date() + 30) |>
          select(equipment_name, next_check_date, full_name, equipment_type) |>
          datatable(
            extensions = 'Buttons'
          )
      })
      
      output$overdue <- renderDataTable({
        r_Data() |> 
          filter(next_check_date < Sys.Date()) |>
          select(equipment_name, next_check_date, full_name, equipment_type) |>
          datatable(
            extensions = 'Buttons'
          )
      })
      
    }
  )
}