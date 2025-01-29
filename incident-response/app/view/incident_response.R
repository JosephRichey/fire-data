box::use(
  shiny[...],
  bslib[...],
  shinyTime[...],
  lubridate[...],
  data.table[...],
  dplyr[...],
  shinyalert[...],
  DBI[...],
  purrr[...],
  sortable[...],
)

box::use(
  app/logic/app_data,
  app/modal/modal,
)

UI <- function(id) {
    
  ns <- NS(id)
  tagList(
    actionButton(ns("add_incident"), "Add Incident", class = "btn btn-primary")
  )
}

Output <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('incident_cards'))
  )
}

ModalServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      ##### Reactives #####
      
      incident_details <- reactiveValues(
        incident_id = NULL,
        dispatch_date = NULL,
        dispatch_time = NULL,
        end_date = NULL,
        end_time = NULL,
        address = NULL,
        area = NULL,
        dispatch_reason = NULL,
        units = NULL,
        canceled = NULL,
        dropped = NULL,
        apparatus = NULL,
        firefighter = NULL,
        call_notes = NULL
      )
      
      ##### Modal Navigation #####
      
      # Show first modal
      observe({
        showModal(modal$key_time(ns, incident_details))
      }) |>
        bindEvent(input$add_incident, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      # Show second modal
      observe({
        removeModal()
        showModal(modal$address_unit(ns, incident_details))
      }) |>
        bindEvent(input$to_address_unit, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      # Show third modal
      observe({
        removeModal()
        showModal(modal$apparatus_ff(ns, incident_details))
      }) |>
        bindEvent(input$to_apparatus_ff, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      # Show fourth modal
      observe({
        removeModal()
        showModal(modalDialog(
          title = "Assign Firefighters to Apparatus",
          uiOutput(ns("dynamic_bucket_list")),
          easyClose = TRUE,
          footer = tagList(
            actionButton(ns("to_apparatus_ff"), "Back", class = "btn btn-secondary"),
            actionButton(ns("cancel_modal"), "Cancel", class = "btn btn-warning"),
            actionButton(ns("to_note"), "Next", class = "btn btn-primary")
          ), size = 'l'
        ))
      }) |>
        bindEvent(input$to_assignment, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      # Show fifth modal
      observe({
        removeModal()
        browser()
        showModal(modal$note(ns, incident_details))
      }) |>
        bindEvent(input$to_note, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      
      # Cancel and reset values
      observeEvent(input$cancel_modal, {
        removeModal()
        shinyalert(
          title = "Warning",
          text = "Incident not saved",
          type = "warning"
        )
        incident_details$incident_id = NULL
        incident_details$dispatch_date = NULL
        incident_details$dispatch_time = NULL
        incident_details$end_date = NULL
        incident_details$end_time = NULL
        incident_details$address = NULL
        incident_details$area = NULL
        incident_details$dispatch_reason = NULL
        incident_details$units = NULL
        incident_details$canceled = NULL
        incident_details$dropped = NULL
        incident_details$apparatus = NULL
        incident_details$firefighter = NULL
        incident_details$call_notes = NULL
      })
      
      # Save and reset values
      observeEvent(input$submit, {
        removeModal()
        shinyalert(
          title = "Success",
          text = "Incident saved",
          type = "success"
        )
        incident_details$incident_id = NULL
        incident_details$dispatch_date = NULL
        incident_details$dispatch_time = NULL
        incident_details$end_date = NULL
        incident_details$end_time = NULL
        incident_details$address = NULL
        incident_details$area = NULL
        incident_details$dispatch_reason = NULL
        incident_details$units = NULL
        incident_details$canceled = NULL
        incident_details$dropped = NULL
        incident_details$apparatus = NULL
        incident_details$firefighter = NULL
        incident_details$call_notes = NULL
      })
      

      
      
      ###### Save Date Automatically When Modified #####
      # List of variables to bind
      fields <- c(
        "incident_id", "dispatch_date", "dispatch_time", "end_date", "end_time", 
        "address", "area", "dispatch_reason", "units", "canceled", 
        "dropped", "apparatus", "firefighter", "call_notes"
      )
      
      # Iterate and bind each field dynamically
      walk(fields, ~ bindEvent(
        observe({
          incident_details[[.x]] <- input[[.x]]
        }),
        input[[.x]]
      ))
      
      ##### Apparatus and Firefighter Assignment #####
      generate_apparatus_firefighter <- reactive({
        req(input$apparatus, input$firefighter)
        
        # browser()
        
        div('rank-list-item', style = "background-color: #996868 !important;")
        
        do.call(
          bucket_list,
          c(
            list(
              header = "Assign Firefighters to Apparatus",
              group_name = "apparatus_list_group",
              orientation = "horizontal",
              add_rank_list(
                text = "Drag from here",
                labels = input$firefighter,
                input_id = "firefighter_list",
                options = sortable_options(
                  group = "apparatus_list_group",
                  class = "sortable-item"
                  )
              )
            ),
            lapply(input$apparatus, function(apparatus_name) {
              add_rank_list(
                text = apparatus_name,
                labels = NULL,
                input_id = paste0("rank_list_", apparatus_name),
                options = sortable_options(
                  group = "apparatus_list_group",
                  class = "sortable-item"
                )
              )
            })
          )
        )
      })
      
      # Render dynamic bucket list inside modal
      output$dynamic_bucket_list <- renderUI({
        generate_apparatus_firefighter()
      })
      
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
        # browser()
        
        # Take only time componenet of the input
        as.POSIXct(input$dispatch_time) |> format('%H:%M:%S')
        
        UTC_dispatch_time_date <-  as.POSIXct((paste(input$dispatch_date, input$dispatch_time |> format('%H:%M:%S')))) |> force_tz(Sys.getenv('LOCAL_TZ')) |> with_tz()
        UTC_end_time_date <- as.POSIXct((paste(input$end_date, input$end_time |> format('%H:%M:%S')))) |> force_tz(Sys.getenv('LOCAL_TZ')) |> with_tz()

        ###### Incident Statement Prepration
        incident_statment_prep <- paste0("INSERT INTO ", SQL(Sys.getenv('INCIDENT_TABLE')), " VALUES ('", input$incident_id, "', '",
                                         UTC_dispatch_time_date, "', '",
                                         UTC_end_time_date, "', '",
                                         input$address, "', '",
                                         input$dispatch_reason, "', ",
                                         if_else("EMS" %in% input$units, 1, 0), ", ",
                                         if_else("Fire" %in% input$units, 1, 0), ", ",
                                         if_else("Wildland" %in% input$units, 1, 0), ", '",
                                         input$area, "', ",
                                         if_else(input$canceled, 1, 0), ", ",
                                         if_else(input$dropped, 1, 0), ", '",
                                         input$call_notes, "', ",
                                         "0);")
        
        incident_statment <- incident_statment_prep
        
        # Interpolate the values into the SQL command safely
        # incident_statment <- sqlInterpolate(
        #   app_data$CON,
        #   incident_statment_prep,
        #   incident_table = SQL(Sys.getenv("INCIDENT_TABLE")),
        #   incident_id = input$incident_id,
        #   dispatch_time = UTC_dispatch_time_date,
        #   end_time = UTC_end_time_date,
        #   address = input$address,
        #   dispatch_reason = input$dispatch_reason,
        #   ems = if_else("EMS" %in% input$units, 1, 0),
        #   fire = if_else("Fire" %in% input$units, 1, 0),
        #   wildland = if_else("Wildland" %in% input$units, 1, 0),
        #   area = input$area,
        #   canceled = if_else(input$canceled, 1, 0),
        #   dropped = if_else(input$dropped, 1, 0),
        #   call_notes = input$call_notes
        # )
        
        ###### Firefighter incident statement preparation ######
        # No interpolation needed here, just a loop to build the statement
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
        # No interpolation needed here, just a loop to build the statement
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
        # No interpolation needed here, just a loop to build the statement
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
            text = "Tables not saved properly. Please contact your application administrator.",
            type = "error"
          )
        }
      }) |> 
        bindEvent(input$mod_5_submit, ignoreNULL = T)
    }
  )
}

CardServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      R_Incident <- reactiveVal(app_data$Incident)
      
      R_Firefighter_Incident <- reactiveVal(app_data$Firefighter_Incident)
      
      ns <- session$ns
      
      updateReactiveValue <- function() {
        R_Incident(DBI::dbGetQuery(app_data$CON, paste0("SELECT * FROM ", Sys.getenv("INCIDENT_TABLE"))))
        R_Firefighter_Incident(DBI::dbGetQuery(app_data$CON, paste0("SELECT * FROM ", Sys.getenv("FF_INC_TABLE"))))
      }
      
      
      ns <- session$ns
      
      output$incident_cards <- renderUI({
        updateReactiveValue()
        
        Incidents <- R_Incident()
        
        Firefighter_Incident <- R_Firefighter_Incident()
        
        Incidents <- Incidents[Incidents$incident_end_time >= Sys.time() - 48*3600, ] |> 
          arrange(desc(incident_end_time))
        
        lapply(seq_len(nrow(Incidents)), function(i) {
          incident <- Incidents[i, ]
          
          firefighters <- Firefighter_Incident[Firefighter_Incident$incident_id == incident$incident_id, ] |> 
            pull(firefighter_id) 
          
          names(app_data$firefighter_mapping)[app_data$firefighter_mapping %in% firefighters]
          
          card(
            card_header(paste(incident$incident_dispatch_time |> with_tz(Sys.getenv('LOCAL_TZ')) |> as.Date(), 
                              incident$incident_dispatch_reason)),
            p(paste(names(app_data$firefighter_mapping)[app_data$firefighter_mapping %in% firefighters], collapse = ", "))
          )
        })
      }) |> 
        bindEvent(input$mod_5_submit, ignoreNULL = F, ignoreInit = F)
      
      
    }
  )
  
}



