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
  htmlwidgets[...],
)

box::use(
  app/logic/app_data,
  app/modal/modal,
  app/logic/functions,
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
  tagList(
    uiOutput(ns('incident_cards'))
  )
  
  
}

ModalServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      edit <- reactiveVal(FALSE)
      
      additional <- reactiveVal(FALSE)
      
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
        call_notes = NULL,
        ff_app_assignemnt = NULL
      )
      
      ##### Modal Navigation #####
      
      # Edit Modal
      observe({
        
        edit(TRUE)
        
        Incident_Edit <- app_data$Incident() |> 
          filter(incident_id == input$edit_incident)
        Ff_App_Edit <- app_data$Firefighter_Apparatus() |> 
          filter(incident_id == input$edit_incident) |> 
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
        
        showModal(modal$key_time(ns, incident_details, edit()))
      }) |>
        bindEvent(input$edit_incident, ignoreNULL = TRUE, ignoreInit = TRUE)

      # Additional Response Modal
      observe({
        additional(TRUE)
        showModal(modal$key_time_additional(ns, incident_details))
      }) |>
        bindEvent(input$add_additional_response, ignoreNULL = TRUE, ignoreInit = TRUE)
      

      # Show first modal
      observe({
        showModal(modal$key_time(ns, incident_details, edit()))
      }) |>
        bindEvent(input$add_incident, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      
      # Validate first modal and show second modal
      observe({
        
        # Skip validation if editing
        if(edit()) {
          removeModal()
          showModal(modal$address_unit(ns, incident_details))
          return()
        } else {
          if(!grepl(app_data$incident_pk_regex, input$incident_id)) {
            shinyalert(
              title = "Error",
              text = paste("Please enter an id that matches the following criteria:\n",
                           app_data$incident_pk_message),
              type = "error"
            )
            return()
          }
          
          if(input$incident_id %in% app_data$Incident()$incident_id) {
            shinyalert(
              title = "Error",
              text = "Incident ID already exists",
              type = "error"
            )
            return()
          }
          
          # FIXME Need a way to validate that time is in proper order.
          # This is a little tricky because of crossing dates (probably 
          # need a function so I can do this consistently throughout the app. 
          # I think there are some other places where something similar to this 
          # needs to be done.)
          
          removeModal()
          showModal(modal$address_unit(ns, incident_details))
          
        }
        
        
      }) |>
        bindEvent(input$to_address_unit, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      # Show third modal
      observe({
        removeModal()
        # browser()
        showModal(modal$select_ff_aparatus(ns, incident_details, additional()))
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
            actionButton(ns("to_apparatus_ff"), "Back", class = "btn btn-light"),
            actionButton(ns("cancel_modal"), "Cancel", class = "btn btn-warning"),
            actionButton(ns("to_note"), "Next", class = "btn btn-primary")
          ), size = 'l'
        ))
      }) |>
        bindEvent(input$to_assignment, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      
      ##### End Assignment Widget #####
      
      # Show fifth modal
      observe({
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
        
        functions$resetCachedValues(incident_details, edit, additional)
        
      })
  
      
      ###### Save Date Automatically When Modified #####
      # List of variables to bind
      fields <- c(
        "incident_id", "dispatch_date", "dispatch_time", "end_date", "end_time", 
        "address", "area", "dispatch_reason", "units", "canceled", 
        "dropped", "apparatus", "firefighter", "call_notes", 'incident_id_additional'
      )
      
      # Iterate and bind each field dynamically
      walk(fields, ~ bindEvent(
        observe({
          incident_details[[.x]] <- input[[.x]]
        }),
        input[[.x]]
      ))
      
#     }
#   )
# }
#  # Removed temporarily so edit() could be accessed within this context.
# DBWriteServer <- function(id) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#      
#     ns <- session$ns
      
      ### Everything that happens on sbumit
      
      observe({
        ## Print values to log
        vals <- reactiveValuesToList(incident_details)
        # browser()
        print(vals)
        
        list <- input$ff_app_lists
        
        for (i in 1:length(list)) {
          
          cat(
            paste0(
              names(list)[[i]] |> 
                stringr::str_replace("apparatus_list_", "") |> 
                stringr::str_replace("app-incident_response-", ""), 
              ": ", 
              paste(list[[i]], collapse = "\n")
            ),
            file = stderr()
          )
        }
        
        # Take only time component of the input
        
        # FIXME For now, we're staying in Local TZ since we're not writing to the DB
        # In the future, this needs to handle both. Options are:
        # 1. Write to DB in UTC and convert to local time on read (use update reactive function)
        # 2. Write to DB in UTC and update the reactive values to local time

        
        
        local_dispatch_time <- as.POSIXct(
          paste(
            #FIXME Veryify this actually works- on the second entry,
            # if defaults are changed, then no value is cached.
            # But if defauls don't change, then we should be good to grab
            # the input, right?
            coalesce(incident_details$dispatch_date, input$dispatch_date),
            coalesce(incident_details$dispatch_time, input$dispatch_time) |> format("%H:%M:%S")
          ),
          tz = Sys.getenv('LOCAL_TZ')
        )
        local_end_time <- as.POSIXct(
          paste(
            coalesce(incident_details$end_date, input$end_date),
            coalesce(incident_details$end_time, input$end_time) |> format("%H:%M:%S")
          ),
          tz = Sys.getenv('LOCAL_TZ')
        )
        
        Current_Incident <- app_data$Incident()
        New_Incident <- data.frame(
          id = nrow(Current_Incident) + 1,
          incident_id = coalesce(incident_details$incident_id, incident_details$incident_id_additional, input$incident_id_additional),
          dispatch_time = local_dispatch_time,
          end_time = local_end_time,
          address = if (is.null(incident_details$address)) NA else incident_details$address,
          dispatch_reason = if (is.null(incident_details$dispatch_reason)) NA else incident_details$dispatch_reason,
          # FIXME Current DB structure allows only 3 types of responses. Are we sure there will only ever be three types?
          ems_units = if (is.null(incident_details$units)) NA else if_else("EMS" %in% incident_details$units, 1, 0),
          fire_units = if (is.null(incident_details$units)) NA else if_else("Fire" %in% incident_details$units, 1, 0),
          wildland_units = if (is.null(incident_details$units)) NA else if_else("Wildland" %in% incident_details$units, 1, 0),
          area = if (is.null(incident_details$area)) NA else incident_details$area,
          canceled = if (is.null(incident_details$canceled)) NA else if
              (is.numeric(incident_details$canceled)) incident_details$canceled else
              if_else(incident_details$canceled, 1, 0),
          dropped = if (is.null(incident_details$dropped)) NA else if
            (is.numeric(incident_details$dropped)) incident_details$dropped else
            if_else(incident_details$dropped, 1, 0),
          notes = if (is.null(incident_details$call_notes)) "" else incident_details$call_notes,
          finalized = 0
        )
        
        # If editing, replace record. If not, add new record
        if(edit()) {
          app_data$Incident() |> 
            filter(incident_id != input$edit_incident) |> 
            rbind(New_Incident) |> 
            app_data$Incident()
        } else {
          app_data$Incident(rbind(Current_Incident, New_Incident))
        }
        
        functions$resetCachedValues(incident_details, edit, additional)
        
        cat('finished', file = stderr())
        
        removeModal()
        
        shinyalert(
          title = "Success",
          text = "Incident saved",
          type = "success"
        )
        
        
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
      
      output$incident_cards <- renderUI({
        # updateReactiveValue()
        Incidents <- app_data$Incident()
        
        Firefighter_Incident <- app_data$Firefighter_Incident()
        
        Incidents <- Incidents[Incidents$end_time >= Sys.time() - 96*3600, ] |> 
          arrange(desc(end_time))
        
        lapply(seq_len(nrow(Incidents)), function(i) {
          incident <- Incidents[i, ]
          
          firefighters <- Firefighter_Incident[Firefighter_Incident$incident_id == incident$incident_id, ] |> 
            pull(firefighter_id) 
          
          responded <- names(app_data$firefighter_mapping)[app_data$firefighter_mapping %in% firefighters]
          
          
          
          card(
            card_header(
              HTML(
                paste(
                  incident$incident_id, 
                  format(incident$dispatch_time, "%m-%d-%Y", usetz = F), 
                  if_else(is.na(incident$dispatch_reason), "Additional Response", incident$dispatch_reason), 
                  div(
                    style = "display: inline-block; margin-left: 10px;",
                    tags$button(
                      "Edit",
                      onclick = sprintf("App.edit_incident('%s', '%s')", 
                                        ns(""),
                                        incident$incident_id),
                      class = "btn btn-primary"
                    )
                  ),
                  sep = " | "
                )
              )
            ),
            card_body(
            p(
              paste(
                format(incident$dispatch_time, "%H:%M:%S", usetz = F),
                format(incident$end_time, "%H:%M:%S", usetz = F), 
                sep = " - "
                )
            ),
            p(paste(responded, collapse = ", "))
            )
          )
        })
      }) |> 
        bindEvent(app_data$Incident(), ignoreNULL = F, ignoreInit = F)
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
      #FIXME Use a funciton so same validation happens as when originally adding id
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



