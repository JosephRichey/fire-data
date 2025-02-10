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
    selectInput(
      ns('firefighter'), 
      'Firefighter', 
      choices = c("Select Firefighter",
                  app_data$Firefighter$full_name |> unique()),
      selected = "Select Firefighter",
      multiple = FALSE
    ),
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
      ns('due_soon_filter'),
      'Due Soon',
      value = FALSE
    ),
    checkboxInput(
      ns('overdue_filter'),
      'Overdue',
      value = FALSE
    ),
    actionButton(
      ns('check_selected'),
      'Check Selected',
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
      
      Base_Data <- reactive({
        app_data$Base_Data
      })
      
      output$equipment <- renderDataTable({
        # browser()
        A <- Base_Data() |> 
          select(equipment_id, icon, equipment_name, date, 
                 full_name, equipment_type, button, flag_type) |>
          filter(equipment_type %in% input$type)
        
        if(input$due_soon_filter) {
          A <- A |> filter(flag_type == "Check")
        }
        
        if(input$overdue_filter) {
          A <- A |> filter(date < app_data$Current_Local_Date)
        }
          
        
          datatable(
            A,
            extensions = 'Buttons',
            escape = FALSE
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