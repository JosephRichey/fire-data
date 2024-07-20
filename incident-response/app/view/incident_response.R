box::use(
  shiny[...],
  bslib[...],
  shinyTime[...],
  lubridate[...],
  data.table[...],
  dplyr[...],
  shinyalert[...],
)

box::use(
  app/logic/app_data,
)

UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("add_incident"), "Add Incident")
  )
}

Output <- function(id) {
  ns <- NS(id)
  tagList(
  
  )
}

Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # Track the number of calls
      ns <- session$ns
      
      call_count <- reactiveValues(count = 0)
      
      # Store call details
      call_details <- reactiveValues()
      
      # First modal of info gathering
      observe({
        
        modal <- modalDialog(
          textInput(ns("incident_id"), "Incident ID:", ""),
          dateInput(ns("dispatch_date"), "Dispatch Date:"),
          timeInput(ns("dispatch_time"), "Dispatch Time:", value = as.ITime(Sys.time() |> with_tz(Sys.getenv('LOCAL_TZ'))-3600), seconds = F),
          dateInput(ns("end_date"), "End Date:"),
          timeInput(ns("end_time"), "End Time:", value = as.ITime(Sys.time() |> with_tz(Sys.getenv('LOCAL_TZ'))), seconds = F),
          textInput(ns('address'), 'Address:', ""),
          # FIXME Need to add area of response
          selectInput(ns("dispatch_reason"), "Dispatch Reason:", app_data$Dispatch_Codes),
          checkboxGroupInput(ns('units'), 'Units', c('EMS', 'Fire', 'Wildland')),
          textInput(ns("call_notes"), "Notes:", ""),
          footer = tagList(
            actionButton(ns('cancel_mod_1'), 'Cancel'),
            actionButton(ns("mod_1_next"), "Next")
          )
        )
        
        # Show modal
        showModal(modal)
      }) |> 
        bindEvent(input$add_incident, ignoreNULL = T)
      
      # Second modal of info gathering
      observe({
        removeModal()
        modal <- modalDialog(
          selectInput(ns('apparatus'), 
                      'Apparatus:', 
                      choices = c(app_data$Apparatus |> pull(apparatus_name)),
                      multiple = TRUE
                      ),
          footer = tagList(
            actionButton(ns('cancel_mod_2'), 'Cancel'),
            actionButton(ns("mod_2_next"), "Next")
          )
        )
        
        showModal(modal)
      }) |> 
        bindEvent(input$mod_1_next, ignoreNULL = T)
      
      # Third modal to assign personal to rigs
      observe({
        removeModal()
        # browser()
        
        apparatus <- input$apparatus
        
        select_inputs <- lapply(apparatus, function(i) {
          selectInput(ns(i), 
                      paste0(i, ':'), 
                      choices = app_data$Firefighter |> pull(firefighter_full_name),
                      multiple = TRUE
          )
        })
        
        modal <- modalDialog(
          tagList(select_inputs),  # Use tagList to include all selectInput elements
          footer = tagList(
            actionButton(ns("cancel_mod_3"), "Cancel"),
            actionButton(ns("mod_3_next"), "Next")
          )
        )
        
        
        showModal(modal)
      }) |> 
        bindEvent(input$mod_2_next, ignoreNULL = T)
      
      # Reactive value to keep track of selected firefighters
      # selected_firefighters <- reactiveValues()
      
      
      # Remove firefighters as they're added to apparatus from the selectInput choices
      # observeEvent(lapply(input$apparatus, function(app) input[[ns(app)]]), {
      #   # Combine selected firefighters from all inputs
      #   all_selected <- unlist(lapply(apparatus, function(app) input[[ns(app)]]))
      #   
      #   # Update selectInput choices to exclude already selected firefighters
      #   for (app in apparatus) {
      #     current_selection <- input[[ns(app)]]
      #     available_choices <- setdiff(app_data$Firefighter$firefighter_full_name, all_selected)
      #     available_choices <- union(current_selection, available_choices) # Keep the current selection available
      #     updateSelectInput(session, ns(app), choices = available_choices, selected = current_selection)
      #   }
      # }, ignoreNULL = FALSE, ignoreInit = TRUE)
      
      ###### Close Modals on cancel, display warning #####
      # FIXME Can't get this to work on all three, so I've got it seperated out for now
      observe({
        removeModal()
        shinyalert(
          title = "Warning",
          text = "Incident not saved",
          type = "warning"
        )
      }) |> 
        bindEvent(input$cancel_mod_1, ignoreNULL = T, ignoreInit = T)
      
      observe({
        removeModal()
        shinyalert(
          title = "Warning",
          text = "Incident not saved",
          type = "warning"
        )
      }) |> 
        bindEvent(input$cancel_mod_2, ignoreNULL = T, ignoreInit = T)
      
      observe({
        removeModal()
        shinyalert(
          title = "Warning",
          text = "Incident not saved",
          type = "warning"
        )
      }) |> 
        bindEvent(input$cancel_mod_3, ignoreNULL = T, ignoreInit = T)
      ####################################################
      
    }
  )
}