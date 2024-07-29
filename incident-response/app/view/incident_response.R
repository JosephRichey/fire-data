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
      
      # First modal - Incident ID and Dispatch Time/Date
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
      
      # Second modal - Incident Address, units, Response Area
      observe({
        removeModal()
        modal <- modalDialog(
          textInput(ns('address'), 'Address:', ""),
          selectInput(ns('area'), 'Response Area', c('Municipality', 'Primary Area', 'Mutual Aid', 'Outside Aid')),
          selectInput(ns("dispatch_reason"), "Dispatch Reason:", app_data$Dispatch_Codes),
          checkboxGroupInput(ns('units'), 'Units', c('EMS', 'Fire', 'Wildland')),
          footer = tagList(
            actionButton(ns('cancel_mod_2'), 'Cancel'),
            actionButton(ns("mod_2_next"), "Next")
          )
        )
        
        showModal(modal)
      }) |> 
        bindEvent(input$mod_1_next, ignoreNULL = T)
      
      # Third modal - Apparatus and Firefighter selection
      observe({
        removeModal()
        modal <- modalDialog(
          selectInput(ns('apparatus'), 
                      'Apparatus:', 
                      choices = c(app_data$Apparatus |> pull(apparatus_name)),
                      multiple = TRUE
                      ),
          selectInput(ns('firefighter'),
                      'Firefighter',
                      choices = c(app_data$Firefighter |> pull(firefighter_full_name)),
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
      
      # Fourth modal - assign personal to apparatus
      observe({
        removeModal()
        # browser()
        
        firefighter <- input$firefighter
        apparatus <- input$apparatus
        
        select_inputs <- lapply(firefighter, function(i) {
          div(class = 'firefighter_div',
              bslib::layout_columns(
              shiny::strong(i),
              col_widths = c(12)),
              bslib::layout_columns(
              selectInput(paste0(ns(i), '_apparatus'),
                          'Apparatus:',
                          choices = apparatus),
                          width = '200px'
              ),
              col_widths = c(12)
              
              
          )
        })
        
        modal <- modalDialog(
          tagList(select_inputs),
          
          footer = tagList(
            actionButton(ns("cancel_mod_4"), "Cancel"),
            actionButton(ns("mod_4_next"), "Next")
          )
        )
        
        showModal(modal)
      }) |> 
        bindEvent(input$mod_3_next, ignoreNULL = T)
      
      # Fifth modal - Notes and submit
      observe({
        removeModal()
        
        modal <- modalDialog(
          textInput(ns("call_notes"), "Notes:", ""),
          
          footer = tagList(
            actionButton(ns("cancel_mod_5"), "Cancel"),
            actionButton(ns("mod_5_submit"), "Submit Incident")
          )
        )
        
        showModal(modal)
      }) |> 
        bindEvent(input$mod_4_next, ignoreNULL = T)
      
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
      # FIXME Can't get this to work dynamically, so I've got it separated out for now
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
      
      observe({
        removeModal()
        shinyalert(
          title = "Warning",
          text = "Incident not saved",
          type = "warning"
        )
      }) |> 
        bindEvent(input$cancel_mod_5, ignoreNULL = T, ignoreInit = T)
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
        
        ###### Incident Statement Prepration
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
               input$call_notes, "',0)"
               )
        
        ###### Firefighter incident statement preparation ######
        firefighter_incident_statement <- paste0("INSERT INTO ", 
                   Sys.getenv("FF_INC_TABLE"), 
                   " (incident_id, firefighter_id) VALUES "
            )
        
        
        for(ff in input$firefighter) {
          firefighter_incident_statement <- paste0(firefighter_incident_statement, "('",
                                                             input$incident_id, "',",
                                                             app_data$firefighter_mapping[[ff]], "),"
                                                             )
        }
      
        
        # Replace the last comma with a semi-colon
        firefighter_incident_statement <- sub(",([^,]*)$", ";\\1", firefighter_incident_statement)
        
        
        ###### Apparatus incident statement preparation ######
        apparatus_incident_statement <- paste0("INSERT INTO ", 
                                                 Sys.getenv("APP_INC_TABLE"), 
                                                 " (incident_id, apparatus_id) VALUES "
        )
        
        for(app in input$apparatus) {
          apparatus_incident_statement <- paste0(apparatus_incident_statement, "('",
                                                             input$incident_id, "',",
                                                             app_data$apparatus_mapping[[app]], "),"
          )
        }
        
        # Replace the last comma with a semi-colon
        apparatus_incident_statement <- sub(",([^,]*)$", ";\\1", apparatus_incident_statement)
        
        ###### Firefighter Apparatus statement preparation ######
        firefighter_apparatus_statement <- paste0("INSERT INTO ", 
                                                 Sys.getenv("FF_APP_TABLE"), 
                                                 " (incident_id, firefighter_id, apparatus_id) VALUES "
        )
        
        for(ff in input$firefighter) {
          app <- input[[paste0(ff, "_apparatus")]]
          
          
          firefighter_apparatus_statement <- paste0(firefighter_apparatus_statement, "('",
                                                             input$incident_id, "',",
                                                             app_data$firefighter_mapping[[ff]], ",",
                                                             app_data$apparatus_mapping[[app]], "),"
          )
          
        }
        
        # Replace the last comma with a semi-colon
        firefighter_apparatus_statement <- sub(",([^,]*)$", ";\\1", firefighter_apparatus_statement)
        
        #####
        
        # Print the statements
        print('Incident statement to be exectuted.')
        print(incident_statment)
        print('Firefighter Incident statement to be exectuted.')
        print(firefighter_incident_statement)
        print('Apparatus Incident statement to be exectuted.')
        print(apparatus_incident_statement)
        print('Firefighter Apparatus statement to be exectuted.')
        print(firefighter_apparatus_statement)
        
        # Execute the statements
        incident_write_result <- DBI::dbExecute(app_data$CON,
                                       incident_statment)
        ff_inc_write_result <- DBI::dbExecute(app_data$CON,
                                       firefighter_incident_statement)
        app_inc_write_result <- DBI::dbExecute(app_data$CON,
                                       apparatus_incident_statement)
        ff_app_write_result <- DBI::dbExecute(app_data$CON,
                                       firefighter_apparatus_statement)
        
        # Check if the write was successful
        if(all(exists('incident_write_result'), exists('ff_inc_write_result'), exists('app_inc_write_result'), exists('ff_app_write_result'))) {
          shinyalert(
            title = "Success",
            text = "Incident saved",
            type = "success"
          )
        } else {
          shinyalert(
            title = "Error",
            text = glue::glue("Tables not saved properly. Please contact your application administrator."),
            type = "error"
          )
        }
      }) |> 
        bindEvent(input$mod_5_submit, ignoreNULL = T)
    }
  )
}



