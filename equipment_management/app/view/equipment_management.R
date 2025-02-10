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
      'Firefighter Performing Check', 
      choices = c("Select Firefighter",
                  app_data$Firefighter$full_name |> unique()),
      selected = "Select Firefighter",
      multiple = FALSE
    ),
    pickerInput(
      ns('type'), 
      'Equipment Type', 
      choices = c(app_data$Equipment_Type$equipment_type |> unique()),
      selected = c(app_data$Equipment_Type$equipment_type |> unique()),
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        size = 10
      ),
      multiple = TRUE
    ),
    checkboxGroupInput(
      ns('due_filter'),
      label = '',
      choices = c('Approaching', 'Due'),
      selected = c('Approaching', 'Due')
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

# Output <- function(id) {
#   ns <- NS(id)
#   tagList(
#     DT::renderDataTable(ns('due_soon')),
#     DT::renderDataTable(ns('overdue'))
#   )
# }

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
          select(equipment_id, icon, equipment_name, next_check_date, 
                 full_name, equipment_type, flag_type, check, snooze) |>
          filter(equipment_type %in% input$type)
        
        print(input$due_filter)
        
        if(is.null(input$due_filter)) {
         print('Do nothing') 
        } else if('Approaching' %in% input$due_filter & 'Due' %in% input$due_filter) {
          A <- A |> 
            filter(flag_type == 'Approaching' | flag_type == 'Due')
        } else if('Approaching' %in% input$due_filter) {
          A <- A |> 
            filter(flag_type == 'Approaching')
        } else if('Due' %in% input$due_filter) {
          A <- A |> 
            filter(flag_type == 'Due')
        }
        
        colnames(A) <- c('ID', 'Icon', 'Equipment Name', 'Next Check Date', 
                         'Assigned To', 'Equipment Type', 'Flag Type', 'Check', 'Snooze')
        
          datatable(
            A,
            extensions = 'Buttons',
            escape = FALSE,
            options = list(
              columnDefs = list(
                list(
                  targets = 1,
                  visible = FALSE
                ),
                list(
                  targets = 7,
                  visible = FALSE
                )
              ),
              order = list(7, 'desc')
            )
          )
      })
      
      # output$due_soon <- renderDataTable({
      #   browser()
      #   r_Data() |> 
      #     filter(next_check_date < Sys.Date() + 30) |>
      #     select(equipment_name, next_check_date, full_name, equipment_type) |>
      #     datatable(
      #       extensions = 'Buttons'
      #     )
      # })
      # 
      # output$overdue <- renderDataTable({
      #   r_Data() |> 
      #     filter(next_check_date < Sys.Date()) |>
      #     select(equipment_name, next_check_date, full_name, equipment_type) |>
      #     datatable(
      #       extensions = 'Buttons'
      #     )
      # })
      
    }
  )
}