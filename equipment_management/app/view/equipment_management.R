box::use(
  shiny[...],
  dplyr[...],
  shinyWidgets[...],
  DT[...],
  shinyalert[...],
)

box::use(
  ../logic/app_data,
  ../logic/functions,
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
    hr(),
    checkboxGroupInput(
      ns('due_filter'),
      label = '',
      choices = c('Approaching', 'Due'),
      selected = c('Approaching', 'Due'),
      inline = TRUE
    ),
    hr(),
    actionButton(
      ns('check_selected'),
      'Check Selected',
      icon = icon('check'),
      class = 'btn-primary'
    ),
    actionButton(
      ns('snooze_selected'),
      'Snooze Selected',
      icon = icon('clock'),
      class = 'btn-secondary'
    ),
    br(),
    br(),
    actionButton(
      ns('refresh'), 
      'Refresh', 
      icon = icon('refresh'),
      class = 'btn-light',
      width = '100%'
    ),
    DT::dataTableOutput(ns('equipment'))
  )
}

Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      Base_Data <- reactiveVal({
        # browser()
        app_data$Base_Data
      })
      
      With_Icons <- reactive({
        # browser()
        Base_Data() |> 
          mutate(check_threshold = functions$GenerateThreshold(next_check_date, check_lead_time, check_lead_time_unit)) |> 
          select(equipment_id, equipment_name, equipment_type,
                 full_name, next_check_date,
                 snooze_expires,  check_threshold) |> 
          mutate(flag_type = case_when(
            snooze_expires <= app_data$Current_Local_Date ~ "Snooze",
            next_check_date <= app_data$Current_Local_Date ~ "Due",
            check_threshold <= app_data$Current_Local_Date ~ "Approaching",
            TRUE ~ 'Normal'),
            icon = case_when(
              flag_type == "Due" ~ bsicons::bs_icon("exclamation-triangle", fill = "red"),  
              flag_type == "Approaching" ~ bsicons::bs_icon("check-circle", fill = "yellow"),
              TRUE ~ bsicons::bs_icon("app", fill = "green")
            ),
            full_name = if_else(is.na(full_name), "", full_name) |> as.character()
          ) |> 
          select(equipment_id, icon, equipment_name, 
                 equipment_type, full_name, next_check_date, flag_type)
      })
      
      Equipment_Table <- reactive({
        # browser()
        A <- With_Icons() |> 
          select(equipment_id, icon, equipment_name, next_check_date, 
                 full_name, equipment_type, flag_type) |>
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
        
        colnames(A) <- c('equipment_id', 'Icon', 'Equipment Name', 'Next Check Date', 
                         'Assigned To', 'Equipment Type', 'Flag Type')
        
        return(A)
      })
      
      
      output$equipment <- renderDataTable({
          datatable(
            Equipment_Table(),
            extensions = 'Buttons',
            options = list(
              columnDefs = list(
                list(
                  targets = c(0,3,5,6),
                  visible = FALSE
                )
              ),
              order = list(3, 'asc')
            ),
            escape = FALSE,
            fillContainer = FALSE,
            rownames = FALSE
          )
      })
      
      observe({
        # browser()
        cat('Checking to see if equipment can be checked', file = stderr())
        
        if(is.null(input$equipment_rows_selected)) {
          shinyalert(
            title = 'Error',
            type = 'error',
            text = 'Please select equipment to perform the check'
          )
          return()
        }
        
        if(input$firefighter == 'Select Firefighter') {
          shinyalert(
            title = 'Error',
            type = 'error',
            text = 'Please select a firefighter to perform the check'
          )
          return()
        }
        
        Ids_to_check <- Equipment_Table()[input$equipment_rows_selected,1]
        
        New <- Base_Data() |>
          mutate(
            next_check_date = if_else(equipment_id %in% Ids_to_check, next_check_date + 365, next_check_date)
          )
        
        Base_Data(New)
        
        for(x in Equipment_Table()[input$equipment_rows_selected,3]) {
          showNotification(
            paste(x, 'Checked'),
            duration = 5,
            type = 'message'
          )
        }
        
      }) |> 
        bindEvent(input$check_selected)
      

      
    }
  )
}