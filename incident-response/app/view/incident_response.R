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
    actionButton(ns("add_incident"), "Add Incident"),
    
    #https://stackoverflow.com/questions/40631788/shiny-observe-triggered-by-dynamicaly-generated-inputs
    # Create a variable using js to track the last changed input in the apparatus_div class.
    tags$script(HTML(
      sprintf("$(document).on('change', '.apparatus_div select', function () {
                Shiny.onInputChange('%s', this.id);
              });", ns("lastSelectId"))
    ))
  )
  
}

Output <- function(id) {
  ns <- NS(id)
  tagList(
  
  )
}

ModalsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # First modal of info gathering
      observe({
        
        modal <- modalDialog(
          textInput(ns("incident_id"), "Incident ID:", ""),
          dateInput(ns("dispatch_date"), "Dispatch Date:"),
          timeInput(ns("dispatch_time"), "Dispatch Time:", value = as.ITime(Sys.time() |> with_tz(Sys.getenv('LOCAL_TZ'))-3600), seconds = F),
          dateInput(ns("end_date"), "End Date:"),
          timeInput(ns("end_time"), "End Time:", value = as.ITime(Sys.time() |> with_tz(Sys.getenv('LOCAL_TZ'))), seconds = F),
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
          textInput(ns('address'), 'Address:', ""),
          selectInput(ns('area'), 'Response Area', c('Municipality', 'Primary Area', 'Mutual Aid', 'Outside Aid')),
          selectInput(ns("dispatch_reason"), "Dispatch Reason:", app_data$Dispatch_Codes),
          checkboxGroupInput(ns('units'), 'Units', c('EMS', 'Fire', 'Wildland')),
          textInput(ns("call_notes"), "Notes:", ""),
          footer = tagList(
            actionButton(ns('cancel_mod_2'), 'Cancel'),
            actionButton(ns("mod_2_next"), "Next")
          )
        )
        
        showModal(modal)
      }) |> 
        bindEvent(input$mod_1_next, ignoreNULL = T)
      
      # Third modal of info gathering
      observe({
        removeModal()
        modal <- modalDialog(
          selectInput(ns('apparatus'), 
                      'Apparatus:', 
                      choices = c(app_data$Apparatus |> pull(apparatus_name)),
                      multiple = TRUE
                      ),
          footer = tagList(
            actionButton(ns('cancel_mod_3'), 'Cancel'),
            actionButton(ns("mod_3_next"), "Next")
          )
        )
        
        showModal(modal)
      }) |> 
        bindEvent(input$mod_2_next, ignoreNULL = T)
      
      # Fourth modal to assign personal to rigs
      observe({
        removeModal()
        # browser()
        
        apparatus <- input$apparatus
        
        select_inputs <- lapply(apparatus, function(i) {
          div(class = 'apparatus_div',
              selectInput(ns(i), 
                      paste0(i, ':'), 
                      choices = app_data$Firefighter |> pull(firefighter_full_name),
                      multiple = TRUE
              )
          )
        })
        
        modal <- modalDialog(
          tagList(select_inputs),  # Use tagList to include all selectInput elements
          footer = tagList(
            actionButton(ns("cancel_mod_4"), "Cancel"),
            actionButton(ns("mod_4_submit"), "Submit Incident")
          )
        )
        
        showModal(modal)
      }) |> 
        bindEvent(input$mod_3_next, ignoreNULL = T)
      
      # Observe the last select input to update the selectInput choices
      observe({
        input$lastSelectId
        # FIXME Currently can't get the selectInputs to update for some reason.
        # current_input <- stringr::str_extract(input$lastSelectId, '[^-]*$')
        # all_inputs <- lapply(input$apparatus, function(i) ns(i))
        # 
        # input$battalion_1
        # updateSelectInput(session, "Battalion_1", selected = NULL)
      
        
      })
      
      
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
      
      observe({
        removeModal()
        shinyalert(
          title = "Warning",
          text = "Incident not saved",
          type = "warning"
        )
      }) |> 
        bindEvent(input$cancel_mod_4, ignoreNULL = T, ignoreInit = T)
      ####################################################
      
    }
  )
}


DBWriteServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      observe({
        removeModal()
        
        UTC_dispatch_time_date <- (input$dispatch_time +.01) |> force_tz(Sys.getenv('LOCAL_TZ')) |> with_tz()
        UTC_end_time_date <- (input$end_time +.01) |> force_tz(Sys.getenv('LOCAL_TZ')) |> with_tz()
        
        
        incident_statment <- paste0("INSERT INTO ", 
               Sys.getenv("INCIDENT_TABLE"),
               " VALUES ('",
               input$incident_id, "','",
               UTC_dispatch_time_date, "','",
               UTC_end_time_date, "','",
               input$address, "','",
               input$dispatch_reason, "',",
               if_else("EMS" %in% input$units, 1, 0), ",",
               if_else("Fire" %in% input$units, 1, 0), ",",
               if_else("Wildland" %in% input$units, 1, 0), ",'",
               input$area, "','",
               input$call_notes, "')"
               )
        
        apparatus_firefighter_incident_statement <- paste0("INSERT INTO ", 
                   Sys.getenv("APP_FF_INC_TABLE"), 
                   " (incident_id, apparatus_id, firefighter_id) VALUES "
            )
        
        # Creating mapping vectors to get Ids
        apparatus_mapping <- stats::setNames(app_data$Apparatus$apparatus_id, app_data$Apparatus$apparatus_name)
        firefighter_mapping <- stats::setNames(app_data$Firefighter$firefighter_id, app_data$Firefighter$firefighter_full_name)
        
        for(app in input$apparatus) {
          for(ff in input[[app]]) {
            apparatus_firefighter_incident_statement <- paste0(apparatus_firefighter_incident_statement, "('",
                                                               input$incident_id, "',",
                                                               apparatus_mapping[[app]], ",",
                                                               firefighter_mapping[[ff]], "),"
                                                               )
          }
        }
        
        # Replace the last comma with a semi-colon
        apparatus_firefighter_incident_statement <- sub(",([^,]*)$", ";\\1", apparatus_firefighter_incident_statement)
        
        # Print the statements
        print('Incident statement to be printed.')
        print(incident_statment)
        print('Apparatus Firefighter Incident statement to be printed.')
        print(apparatus_firefighter_incident_statement)
        
        # Execute the statements
        incident_write_result <- DBI::dbExecute(app_data$CON,
                                       incident_statment)
        app_ff_inc_write_result <- DBI::dbExecute(app_data$CON,
                                       apparatus_firefighter_incident_statement)
        
        # Check if the write was successful
        if(incident_write_result == 1 & app_ff_inc_write_result == 1) {
          shinyalert(
            title = "Success",
            text = "Incident saved",
            type = "success"
          )
        } else if(incident_write_result == 1 & app_ff_inc_write_result == 0) {
          shinyalert(
            title = "Error",
            text = "Apparatus Firefighter Incident not saved",
            type = "error"
          )
        } else if(incident_write_result == 0 & app_ff_inc_write_result == 1) {
          shinyalert(
            title = "Error",
            text = "Incident not saved",
            type = "error"
          )
        } else {
          shinyalert(
            title = "Error",
            text = "Incident and Apparatus Firefighter Incident not saved",
            type = "error"
          )
        }
        
        
      }) |> 
        bindEvent(input$mod_4_submit, ignoreNULL = T)
    }
  )
}



