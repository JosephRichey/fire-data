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
    htmlOutput(ns('firefighter_checking')),
    hr(),
    pickerInput(
      ns('type'), 
      'Equipment Type', 
      choices = c(app_data$Equipment_Type$equipment_type |> unique()),
      selected = c(app_data$Equipment_Type$equipment_type |> unique()),
      options = list(
        `actions-box` = TRUE,
        #FIXME set with .csv settings
        # `live-search` = TRUE,
        size = 10
      ),
      multiple = TRUE
    ),
    checkboxGroupInput(
      ns('due_filter'),
      label = '',
      choices = c('Due', 'Approaching', 'Normal', 'Snooze'),
      #FIXME set with .csv settings
      selected = c('Due', 'Approaching'),
      inline = TRUE
    ),
    hr(),
    actionButton(
      ns('check_selected'),
      'Check',
      icon = icon('check'),
      class = 'btn-primary'
    ),
    actionButton(
      ns('snooze_selected'),
      'Snooze',
      icon = icon('clock'),
      class = 'btn-secondary'
    ),
    br(),
    br(),
    DT::dataTableOutput(ns('equipment')),
    br(),
    actionButton(
      ns('switch_firefigher'),
      'Switch Firefighter',
      icon = icon('user'),
      class = 'btn-primary',
      width = '100%'
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
            snooze_expires >= app_data$Current_Local_Date ~ "Snooze",
            next_check_date <= app_data$Current_Local_Date ~ "Due",
            check_threshold <= app_data$Current_Local_Date ~ "Approaching",
            TRUE ~ 'Normal'),
            icon = case_when(
              flag_type == "Due" ~ bsicons::bs_icon("exclamation-triangle", fill = 'red'),  
              flag_type == "Approaching" ~ bsicons::bs_icon("exclamation-triangle", fill = 'yellow'),
              flag_type == "Snooze" ~ bsicons::bs_icon("clock", class = "text-info"),
              flag_type == "Normal" ~ bsicons::bs_icon("check-circle-fill", class = "text-success")
            ),
            full_name = if_else(is.na(full_name), "", full_name) |> as.character()
          ) |> 
          select(equipment_id, icon, equipment_name, 
                 equipment_type, full_name, next_check_date, flag_type)
      })
      
      output$firefighter_checking <- renderUI({
        if(input$firefighter == 'Select Firefighter') {
          return('')
        } else {
          # browser()
          return(
            HTML(paste0(
              "<span style='font-size: 1.2em; font-weight: bold; color: #2b8764;'>", input$firefighter, "</span>",
              " is currently performing checks"
            ))
            
            )
        }
      })
      
      Equipment_Table <- reactive({
        # browser()
        A <- With_Icons() |> 
          select(equipment_id, icon, equipment_name, next_check_date, 
                 full_name, equipment_type, flag_type) |>
          filter(equipment_type %in% input$type) |> 
          filter(flag_type %in% input$due_filter)
        
        print(input$due_filter)
        
        colnames(A) <- c('equipment_id', 'Icon', 'Equipment Name', 'Next Check Date', 
                         'Assigned To', 'Equipment Type', 'Flag Type')
        
        return(A)
      })
      
      ns <- session$ns
      
      observeEvent(input$switch_firefigher, {
          showModal(modalDialog(
            selectInput(
              ns('firefighter'), 
              '', 
              choices = c("Select Firefighter",
                          app_data$Firefighter$full_name |> unique()),
              selected = "Select Firefighter",
              multiple = FALSE
            ),
            title = "Firefighter Performing Check",
            footer = tagList(
              actionButton(ns("ok"), "Ok")
            )
          ))
        }, ignoreNULL = FALSE)
      
      observe({
        if(input$firefighter == 'Select Firefighter') {
          shinyalert(
            title = 'Error',
            type = 'error',
            text = 'Please select a firefighter to perform the check'
          )
          return()
        } else {
          removeModal()
        }
      }) |> 
        bindEvent(input$ok)
      
      observe({
        cat('Refreshing data', file = stderr())
        Base_Data(app_data$Base_Data)
      }) |> 
        bindEvent(input$refresh)
      
      
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
            order = list(3, 'asc'),
            dom = 't',  # Removes search bar, pagination, and 'Show entries'
            paging = FALSE,  # Disables pagination
            searching = FALSE  # Disables search
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
        
        Ids_to_check <- Equipment_Table()[input$equipment_rows_selected,1]
        
        New <- Base_Data() |>
          mutate(
            #FIXME Needs to check and set back number of appropriate number of days
            next_check_date = if_else(equipment_id %in% Ids_to_check, next_check_date + 365, next_check_date)
          )
        
        
        for(x in Equipment_Table()[input$equipment_rows_selected,3]) {
          showNotification(
            paste(x, 'Checked'),
            duration = 5,
            type = 'message'
          )
        }
        
        Base_Data(New)
      }) |> 
        bindEvent(input$check_selected)
      
      observe({
        # browser()
        cat('Checking to see if equipment can be snoozed', file = stderr())
        
        if(is.null(input$equipment_rows_selected)) {
          shinyalert(
            title = 'Error',
            type = 'error',
            text = 'Please select equipment to snooze'
          )
          return()
        }
        
        showModal(modalDialog(
          numericInput(
            ns('snooze_days'),
            'Snooze for (days)',
            value = 30,
            min = 1,
            max = 365
          ),
          title = 'Snooze Equipment',
          footer = tagList(
            actionButton(ns('snooze_ok'), 'Ok')
          )
        ))
      }) |>
        bindEvent(input$snooze_selected)
        
      observe({
        # browser()
        
        removeModal()

        
        Ids_to_check <- Equipment_Table()[input$equipment_rows_selected,1]
        
        New <- Base_Data() |>
          mutate(
            snooze_expires = if_else(equipment_id %in% Ids_to_check, Sys.Date() + input$snooze_days, snooze_expires)
          )
        
        
        for(x in Equipment_Table()[input$equipment_rows_selected,3]) {
          showNotification(
            paste(x, 'Snoozed for', input$snooze_days, 'days'),
            duration = 5,
            type = 'message'
          )
        }
        Base_Data(New)
        
      }) |> 
        bindEvent(input$snooze_ok)
      

      
    }
  )
}