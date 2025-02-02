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
  shinyjs[...],
)

box::use(
  app/logic/app_data,
  app/modal/modal,
)

UI <- function(id) {
    
  ns <- NS(id)
  tagList(
    actionButton(ns("add_incident"), "Add Incident", class = "btn btn-primary"),
    actionButton(ns("add_additional_response"), "Add Additional Response", class = "btn btn-secondary"),
    actionButton(ns("edit_incident_id"), "Edit Incident ID", class = "btn btn-light"),
  )
}

Output <- function(id) {
  ns <- NS(id)
  useShinyjs()
  tagList(
    uiOutput(ns('incident_cards'))
  )
  
  
}

ModalServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      ##### Reactives to save until write or reset #####
      
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
      
      # Edit Modal
      observe({
        Incident_Edit <- app_data$Incident() |> 
          filter(incident_id == 'INC006872')
        Ff_App_Edit <- app_data$Firefighter_Apparatus() |> 
          filter(incident_id == 'INC006872') |> 
          left_join(
            app_data$Firefighter(),
            by = c('firefighter_id' = 'firefighter_id')) |>
          left_join(
            app_data$Apparatus(),
            by = c('apparatus_id' = 'apparatus_id')) |> 
          select(full_name, apparatus_name)
        
        
        incident_details$incident_id <- Incident_Edit$incident_id
        incident_details$dispatch_date <- Incident_Edit$dispatch_time |> as.Date()
        incident_details$dispatch_time <- Incident_Edit$dispatch_time
        incident_details$end_date <- Incident_Edit$end_time |> as.Date()
        incident_details$end_time <- Incident_Edit$end_time
        incident_details$address <- Incident_Edit$address
        incident_details$area <- Incident_Edit$area
        incident_details$dispatch_reason <- Incident_Edit$dispatch_reason
        incident_details$units <- c(
          if_else(Incident_Edit$ems == 1, "EMS", NA),
          if_else(Incident_Edit$fire == 1, "Fire", NA),
          if_else(Incident_Edit$wildland == 1, "Wildland", NA)
        )
        incident_details$canceled <- Incident_Edit$canceled
        incident_details$dropped <- Incident_Edit$dropped
        incident_details$apparatus <- Ff_App_Edit$apparatus_name
        incident_details$firefighter <- Ff_App_Edit$full_name
        incident_details$call_notes <- Incident_Edit$notes
        
        showModal(modal$key_time(ns, incident_details))
      }) |>
        bindEvent(input$edit_incident, ignoreNULL = TRUE, ignoreInit = TRUE)

      # Additional Response Modal
      observe({
        showModal(modal$key_time_additional(ns, incident_details))
      }) |>
        bindEvent(input$add_additional_response, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      
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
        showModal(modal$select_ff_aparatus(ns, incident_details))
      }) |>
        bindEvent(input$to_apparatus_ff, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      # Show fourth modal
      
      ##### Assignment Widget #####
      ##### Apparatus and Firefighter Assignment #####
      generate_firefighter_apparatus <- reactive({
        req(input$apparatus, input$firefighter)

        do.call(
          bucket_list,
          c(
            list(
              header = NULL,
              group_name = ns("ff_app_lists"),
              orientation = "horizontal",
              add_rank_list(
                text = "Standby",
                labels = input$firefighter,
                input_id = ns("standby_list"),
                options = sortable_options(
                  group = "bucket_list",
                  class = "sortable-item"
                )
              )
            ),
            lapply(input$apparatus, function(apparatus_name) {
              add_rank_list(
                text = apparatus_name,
                labels = NULL,
                input_id = paste0("apparatus_list_", apparatus_name |> 
                                    stringr::str_to_lower() |> 
                                    stringr::str_replace_all(" ", "_")),
                options = sortable_options(
                  group = "bucket_list",
                  class = "sortable-item"
                )
              )
            })
          )
        )
      })
      
      # TODO Have this built on a cached value.
      # Render dynamic bucket list inside modal
      output$firefighter_apparatus_list <- renderUI({
        generate_firefighter_apparatus()
      })
      
      observe({
        removeModal()
        showModal(modalDialog(
          title = "Assign Firefighters to Apparatus",
          uiOutput(ns("firefighter_apparatus_list")),
          easyClose = TRUE,
          footer = tagList(
            actionButton(ns("to_apparatus_ff"), "Back", class = "btn btn-secondary"),
            actionButton(ns("cancel_modal"), "Cancel", class = "btn btn-warning"),
            actionButton(ns("to_note"), "Next", class = "btn btn-primary")
          ), size = 'l'
        ))
      }) |>
        bindEvent(input$to_assignment, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      ##### End Assignment Widget #####
      
      # Show fifth modal
      observe({
        removeModal()
        showModal(modal$note(ns, incident_details))
      }) |>
        bindEvent(input$to_note, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      
      ##### Cancel and reset values #####
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
      
      observe({
        browser()
        vals <- reactiveValuesToList(incident_details)
        
        print(vals)
      
        list <- input$ff_app_lists
        
        for (i in 1:length(list)) {

          print(
            paste0(
              names(list)[[i]] |> 
                stringr::str_replace("apparatus_list_", "") |> 
                stringr::str_replace("app-incident_response-", ""), 
              ": ", 
              paste(list[[i]], collapse = ", ")
            )
          )
        }
        
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
        
      }) |> 
        bindEvent(input$submit, ignoreNULL = TRUE)
      

      
      
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
        # Take only time component of the input
        as.POSIXct(input$dispatch_time) |> format('%H:%M:%S')
        
        UTC_dispatch_time_date <-  as.POSIXct(
          (
            paste(
              input$dispatch_date,
              input$dispatch_time |> 
                format('%H:%M:%S')
              )
            )
          ) |> 
          force_tz(app_data$TZ) |> with_tz()
        UTC_end_time_date <- as.POSIXct((paste(input$end_date, input$end_time |> format('%H:%M:%S')))) |> force_tz(Sys.getenv('LOCAL_TZ')) |> with_tz()
        
        Current_Incident <- app_data$Incident()
        # browser()
        New_Incident <- data.frame(
          incident_id = input$incident_id,
          dispatch_time = UTC_dispatch_time_date,
          end_time = UTC_end_time_date,
          address = if (is.null(input$address)) "" else input$address,
          dispatch_reason = input$dispatch_reason,
          ems_units = if_else("EMS" %in% input$units, 1, 0),
          fire_units = if_else("Fire" %in% input$units, 1, 0),
          wildland_units = if_else("Wildland" %in% input$units, 1, 0),
          area = input$area,
          canceled = if_else(input$canceled, 1, 0),
          dropped = if_else(input$dropped, 1, 0),
          notes = input$call_notes,
          finalized = 0
        )
        
        app_data$Incident(rbind(Current_Incident, New_Incident)) 
        
        ###### Incident Statement Prepration
        # incident_statment_prep <- paste0("INSERT INTO ", SQL(Sys.getenv('INCIDENT_TABLE')), " VALUES ('", input$incident_id, "', '",
        #                                  UTC_dispatch_time_date, "', '",
        #                                  UTC_end_time_date, "', '",
        #                                  input$address, "', '",
        #                                  input$dispatch_reason, "', ",
        #                                  if_else("EMS" %in% input$units, 1, 0), ", ",
        #                                  if_else("Fire" %in% input$units, 1, 0), ", ",
        #                                  if_else("Wildland" %in% input$units, 1, 0), ", '",
        #                                  input$area, "', ",
        #                                  if_else(input$canceled, 1, 0), ", ",
        #                                  if_else(input$dropped, 1, 0), ", '",
        #                                  input$call_notes, "', ",
        #                                  "0);")
        # 
        # incident_statment <- incident_statment_prep
        
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
        # firefighter_incident_statement <- paste0("INSERT INTO ", 
        #            Sys.getenv("FF_INC_TABLE"), 
        #            " (incident_id, firefighter_id) VALUES "
        #     )
        # 
        # 
        # for(ff in input$firefighter) {
        #   firefighter_incident_statement <- paste0(firefighter_incident_statement, "('",
        #                                                      input$incident_id, "',",
        #                                                      app_data$firefighter_mapping[[ff]], "),"
        #                                                      )
        # }
        # 
        # 
        # # Replace the last comma with a semi-colon
        # firefighter_incident_statement <- sub(",([^,]*)$", ";\\1", firefighter_incident_statement)
        # 
        # 
        # ###### Apparatus incident statement preparation ######
        # # No interpolation needed here, just a loop to build the statement
        # apparatus_incident_statement <- paste0("INSERT INTO ", 
        #                                          Sys.getenv("APP_INC_TABLE"), 
        #                                          " (incident_id, apparatus_id) VALUES "
        # )
        # 
        # for(app in input$apparatus) {
        #   apparatus_incident_statement <- paste0(apparatus_incident_statement, "('",
        #                                                      input$incident_id, "',",
        #                                                      app_data$apparatus_mapping[[app]], "),"
        #   )
        # }
        # 
        # # Replace the last comma with a semi-colon
        # apparatus_incident_statement <- sub(",([^,]*)$", ";\\1", apparatus_incident_statement)
        # 
        # ###### Firefighter Apparatus statement preparation ######
        # # No interpolation needed here, just a loop to build the statement
        # firefighter_apparatus_statement <- paste0("INSERT INTO ", 
        #                                          Sys.getenv("FF_APP_TABLE"), 
        #                                          " (incident_id, firefighter_id, apparatus_id) VALUES "
        # )
        # 
        # for(ff in input$firefighter) {
        #   app <- input[[paste0(ff, "_apparatus")]]
        #   
        #   
        #   firefighter_apparatus_statement <- paste0(firefighter_apparatus_statement, "('",
        #                                                      input$incident_id, "',",
        #                                                      app_data$firefighter_mapping[[ff]], ",",
        #                                                      app_data$apparatus_mapping[[app]], "),"
        #   )
        #   
        # }
        # 
        # # Replace the last comma with a semi-colon
        # firefighter_apparatus_statement <- sub(",([^,]*)$", ";\\1", firefighter_apparatus_statement)
        
        #####
        
        # Print the statements
        # print('Incident statement to be exectuted.')
        # print(incident_statment)
        # print('Firefighter Incident statement to be exectuted.')
        # print(firefighter_incident_statement)
        # print('Apparatus Incident statement to be exectuted.')
        # print(apparatus_incident_statement)
        # print('Firefighter Apparatus statement to be exectuted.')
        # print(firefighter_apparatus_statement)
        
        # Execute the statements
      #   incident_write_result <- DBI::dbExecute(app_data$CON,
      #                                  incident_statment)
      #   ff_inc_write_result <- DBI::dbExecute(app_data$CON,
      #                                  firefighter_incident_statement)
      #   app_inc_write_result <- DBI::dbExecute(app_data$CON,
      #                                  apparatus_incident_statement)
      #   ff_app_write_result <- DBI::dbExecute(app_data$CON,
      #                                  firefighter_apparatus_statement)
      #   
      #   # Check if the write was successful
      #   if(all(exists('incident_write_result'), exists('ff_inc_write_result'), exists('app_inc_write_result'), exists('ff_app_write_result'))) {
      #     shinyalert(
      #       title = "Success",
      #       text = "Incident saved",
      #       type = "success"
      #     )
      #   } else {
      #     shinyalert(
      #       title = "Error",
      #       text = "Tables not saved properly. Please contact your application administrator.",
      #       type = "error"
      #     )
      #   }
      # }) |> 
      #   bindEvent(input$mod_5_submit, ignoreNULL = T)
        
        
        
      }) |> 
          bindEvent(input$submit, ignoreNULL = T)
    }
  )
}

CardServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      
      # updateReactiveValue <- function() {
      #   app_data$Incident(DBI::dbGetQuery(app_data$CON, paste0("SELECT * FROM ", Sys.getenv("INCIDENT_TABLE"))))
      #   app_data$Firefighter_Incident(DBI::dbGetQuery(app_data$CON, paste0("SELECT * FROM ", Sys.getenv("FF_INC_TABLE"))))
      # }
      
      
      ns <- session$ns
      
      output$incident_cards <- renderUI({
        # updateReactiveValue()
        # browser()
        Incidents <- app_data$Incident()
        
        Firefighter_Incident <- app_data$Firefighter_Incident()
        
        Incidents <- Incidents[Incidents$end_time >= Sys.time() - 48*3600, ] |> 
          arrange(desc(end_time))
        
        lapply(seq_len(nrow(Incidents)), function(i) {
          incident <- Incidents[i, ]
          
          firefighters <- Firefighter_Incident[Firefighter_Incident$incident_id == incident$incident_id, ] |> 
            pull(firefighter_id) 
          
          names(app_data$firefighter_mapping)[app_data$firefighter_mapping %in% firefighters]
          
          # browser()
          
          edit_button <- sprintf(
            '<button id="edit_%s" 
      onclick="Shiny.setInputValue(\'edit_row\', \'%s-\' + Math.random(), {priority: \'event\'}); 
               Shiny.setInputValue(\'dummy_event\', Math.random(), {priority: \'event\'})" 
      class="btn btn-primary">Edit</button>',
            incident$incident_id, incident$incident_id
          )
          
          card(
            card_header(HTML(paste(
              incident$incident_id, 
              incident$dispatch_time |> with_tz(Sys.getenv('LOCAL_TZ')) |> as.Date(), 
              incident$dispatch_reason, 
              edit_button  # Insert the button directly into the header
            ))),
            p(paste('test', collapse = ", "))
          )
        })
      }) |> 
        bindEvent(app_data$Incident(), ignoreNULL = F, ignoreInit = F)
      
      
      observeEvent(input$edit_row, {
        showModal(
          modalDialog(
            title = "Edit Incident",
            paste("Editing incident:", input$edit_row)  # Show which ID was clicked
          )
        )
      })
      
      
    }
  )
  
}

UpdateIdServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Show password modal
      observe({
        showModal(modal$password(ns))
      }) |>
        bindEvent(input$edit_incident_id, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      # Show edit incident modal
      observe({
        removeModal()
        if(input$password == app_data$password) {
          showModal(modal$edit_incident_id(ns))
        } else {
          shinyalert(
            title = "Error",
            text = "Incorrect password",
            type = "error"
          )
        }
      }) |> 
        bindEvent(input$to_edit_id, ignoreNULL = TRUE)
      
      # Confirm and save new incident id
      observe({
        removeModal()
        showModal(
          modal$submit_new_id(ns, input$old_incident_id, input$new_incident_id)
        )
      }) |> 
        bindEvent(input$to_edit_confirm, ignoreNULL = TRUE)
      
      # Save new incident id
      observe({
        removeModal()
        
        New <- app_data$Incident() |> 
          mutate(incident_id = if_else(incident_id == input$old_incident_id,
                                       input$new_incident_id,
                                       incident_id)) 
        
        app_data$Incident(New)
        
        shinyalert(
          title = "Success",
          text = "Incident ID saved",
          type = "success"
        )
      }) |> 
        bindEvent(input$submit_new_id, ignoreNULL = TRUE)
      
    }
  )
}



