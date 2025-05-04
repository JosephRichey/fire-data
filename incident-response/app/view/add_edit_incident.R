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
  logger[...],
  glue[...],
)

box::use(
  app/logic/app_data,
  app/modal/modal,
  app/logic/local_functions,
  app/logic/global_functions[GetSetting,
                             BuildDateTime,
                             CheckWriteResult],
  app/logic/logging,
)


Server <- function(id, rdfs) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      edit <- reactiveVal(FALSE)
      
      additional <- reactiveVal(FALSE)
      
      ##### Reactives to save until write or reset #####
      
      incident_details <- reactiveValues(
        cad_identifier = NULL,
        incident_start_date = NULL,
        incident_start_time = NULL,
        incident_end_date = NULL,
        incident_end_time = NULL,
        address = NULL,
        area = NULL,
        dispatch_reason = NULL,
        units = NULL,
        canceled = NULL,
        dropped = NULL
      )
      
      response_details <- reactiveValues(
        incident_id = NULL,
        response_start_date = NULL,
        response_start_time = NULL,
        response_end_date = NULL,
        response_end_time = NULL,
        response_notes = NULL,
        apparatus = NULL,
        firefighter = NULL,
        ff_app_assignemnt = NULL
      )
      
      ###### Save Date Automatically When Modified #####
      # List of variables to bind
      incident_fields <- c(
        "cad_identifier", "incident_start_date", "incident_start_time", 
        "incident_end_date", "incident_end_time", 
        "address", "area", "dispatch_reason", "units", "canceled", 
        "dropped")
      
      response_fields <- c(
        "incident_id", "response_start_date", "response_start_time" ,
        "response_end_date", "response_end_time", "notes", 
        "apparatus", "firefighter", "ff_app_assignment"
      )
      
      # Iterate and bind each field dynamically
      walk(incident_fields, ~ bindEvent(
        observe({
          incident_details[[.x]] <- input[[.x]]
        }),
        input[[.x]]
      ))
      
      walk(response_fields, ~ bindEvent(
        observe({
          response_details[[.x]] <- input[[.x]]
        }),
        input[[.x]]
      ))
      
      ##### Modal Navigation #####
      # Show first modal
      observe({
        log_info(
          glue('Showing key time modal in {if_else(edit(), "edit", "add")} mode.'),
          namespace = 'add_edit_incident'
          )
        showModal(modal$key_time(ns, incident_details, edit()))
      }) |>
        bindEvent(input$add_incident, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      
      # Validate first modal and show second modal
      observe({
        # Skip checking for duplicate cad ids if editing
        if(!edit()) {
          # Does the CAD ID already exist?
          if(input$cad_identifier %in% rdfs$incident$cad_identifier) {
            shinyalert(
              title = "Error",
              text = "Incident ID already exists",
              type = "error"
            )
            return()
          }
        } 
        
        # Does the CAD ID match the regex?
        if(!grepl(
          GetSetting('incident',
                     key = 'incident_cad_regex'), 
          input$cad_identifier)
          ) {
          shinyalert(
            title = "Error",
            text = paste("Please enter an id that matches the following criteria:\n",
                         GetSetting('incident',
                                    key = 'cad_failure_message')),
            type = "error"
          )
          return()
        }
        
        # Are times in order?
        start_time <- BuildDateTime(
          time = input$incident_start_time,
          date = input$incident_start_date,
          input = 'local',
          return_type = 'local'
        )
        
        end_time <- BuildDateTime(
          time = input$incident_end_time,
          date = input$incident_end_date,
          input = 'local',
          return_type = 'local'
        )
        
        if(end_time <= start_time) {
          shinyalert(
            title = "Error",
            text = "Dispatch time must be before end time",
            type = "error"
          )
          return()
        }
        
        # All validations passed. Show second modal.
        removeModal()
        showModal(modal$address_unit(ns, incident_details, edit()))
        
      }) |>
        bindEvent(
          input$to_address_unit, 
          ignoreNULL = TRUE, 
          ignoreInit = TRUE
        )
      
      # Show third modal
      observe({
        removeModal()
        showModal(
          modal$select_ff_aparatus(
            ns, 
            response_details, 
            additional())
        )
      }) |>
        bindEvent(
          input$to_apparatus_ff, 
          ignoreNULL = TRUE, 
          ignoreInit = TRUE
        )
      
      # Show fourth modal
      
      ##### Assignment Widget #####
      ##### Apparatus and Firefighter Assignment #####
      generate_firefighter_apparatus <- reactive({
        req(input$apparatus, input$firefighter)
        # browser()
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
        # browser()
        showModal(modal$note(ns, response_details))
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
        
        local_functions$resetCachedValues(incident_details, response_details,
                                          edit, additional)
        
      })
      
      
      
      
      
      ### Everything that happens on sbumit
      
      observe({
        browser()
        
        ## Print values to log
        vals <- reactiveValuesToList(incident_details)
        # browser()
        print(vals)
        
        list <- input$ff_app_lists
        
        if(length(list) != 0) {
          for (i in 1:length(list)) {
            
            cat(
              paste0(
                names(list)[[i]] |> 
                  stringr::str_replace("apparatus_list_", "") |> 
                  stringr::str_replace("app-incident_response-", ""), 
                ": ", 
                paste(list[[i]], collapse = "\n"),
                "\n"
              ),
              file = stderr()
            )
          }
          
        }
        
        start_time <- BuildDateTime(
          time = vals$incident_start_time,
          date = vals$incident_start_date,
          input = 'local',
          return_type = 'UTC'
        ) |> 
          format("%Y-%m-%d %H:%M:%S")
        
        end_time <- BuildDateTime(
          time = vals$incident_end_time,
          date = vals$incident_end_date,
          input = 'local',
          return_type = 'UTC'
        ) |> 
          format("%Y-%m-%d %H:%M:%S")
        
        ##### Write to Incident Table ####
        # if(!edit()) {
        #   
        #   sql_statement <- "INSERT INTO ?incident_table
        #   (cad_identifier, incident_start, incident_end, 
        #   address, dispatch_id, area_id,
        #   canceled, dropped, is_reviewed, 
        #   is_locked, is_deleted, deleted_by) VALUES 
        #   (?cad_identifier, ?incident_start, ?incident_end,
        #   ?address, ?dispatch_id, ?area_id,
        #   ?canceled, ?dropped, 0, 0, NULL, NULL)"
        #   
        #   safe_sql <- sqlInterpolate(
        #     app_data$CON,
        #     sql_statement,
        #     incident_table = SQL('incident'),
        #     cad_identifier = vals$cad_identifier,
        #     incident_start = start_time,
        #     incident_end = end_time,
        #     address = if (is.null(vals$address)) NA else vals$address,
        #     dispatch_id = if (is.null(vals$dispatch_reason)) NA else vals$dispatch_reason,
        #     area_id = if (is.null(vals$area)) NA else vals$area,
        #     canceled = if (is.null(vals$canceled)) NA else if
        #     (is.numeric(vals$canceled)) vals$canceled else
        #       if_else(vals$canceled, 1, 0),
        #     dropped = if (is.null(vals$dropped)) NA else if
        #     (is.numeric(vals$dropped)) vals$dropped else
        #       if_else(vals$dropped, 1, 0)
        #   )
        #   
        # } else {
        # # If editing, replace record. If not, add new record
        #   sql_statement <- "UPDATE ?incident_table
        #   SET cad_identifier = ?cad_identifier,
        #   incident_start = ?incident_start,
        #   incident_end = ?incident_end,
        #   address = ?address,
        #   dispatch_id = ?dispatch_id,
        #   area_id = ?area_id,
        #   canceled = ?canceled,
        #   dropped = ?dropped,
        #   is_reviewed = 0,
        #   is_locked = 0,
        #   is_deleted = NULL,
        #   deleted_by = NULL
        #   WHERE id = ?id"
        #   
        #   safe_sql <- sqlInterpolate(
        #     app_data$CON,
        #     sql_statement,
        #     incident_table = SQL('incident'),
        #     cad_identifier = vals$cad_identifier,
        #     incident_start = start_time,
        #     incident_end = end_time,
        #     address = if (is.null(vals$address)) NA else vals$address,
        #     dispatch_id = if (is.null(vals$dispatch_reason)) NA else vals$dispatch_reason,
        #     area_id = if (is.null(vals$area)) NA else vals$area,
        #     canceled = if (is.null(vals$canceled)) NA else if
        #     (is.numeric(vals$canceled)) vals$canceled else
        #       if_else(vals$canceled, 1, 0),
        #     dropped = if (is.null(vals$dropped)) NA else if
        #     (is.numeric(vals$dropped)) vals$dropped else
        #       if_else(vals$dropped, 1, 0),
        #     id = rdfs$incident |> 
        #       filter(cad_identifier == input$edit_incident) |> 
        #       pull(id)
        #   )
        # }
        # 
        # log_info(glue::glue("Executing SQL command: {safe_sql}"), namespace = "Add Training")
        # 
        # # Execute the safely interpolated SQL command
        # result <- tryCatch(
        #   {
        #     dbExecute(app_data$CON, safe_sql)
        #   },
        #   error = function(e) {
        #     log_error(glue::glue("Database write failed: {e$message}"), namespace = "Add Training")
        #     NA
        #   }
        # )
        # 
        # functions$CheckWriteResult(result,
        #                            successMessage = "Your training has been successfully added. You may now close this window.",
        #                            context = "adding a training",
        #                            expectedMin = 1,
        #                            expectedMax = 1
        # )
        # 
        # ##### Write to Response Table #####
        # sql_statement <- "INSERT INTO ?response_table
        # (incident_id, response_start, response_end,
        # notes, is_deleted, deleted_by) VALUES
        # (?incident_id, ?response_start, ?response_end,
        # ?notes, NULL, NULL)"
        # 
        # 
        # local_functions$resetCachedValues(incident_details, edit, additional)
        # 
        # cat('finished', file = stderr())
        # 
        # removeModal()
        # 
        # shinyalert(
        #   title = "Success",
        #   text = "Incident saved",
        #   type = "success"
        # )
        
        
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
      
      
      # Edit Modal
      observe({
        # browser()

        edit(TRUE)

        Incident_Edit <- rdfs$incident |>
          filter(id == input$edit_incident)
        
        incident_units <- rdfs$incident_unit |>
          filter(incident_id == Incident_Edit$id) |> 
          left_join(app_data$Unit, 
                    by = c("unit_type_id" = "id")) |> 
          pull(unit_type_id)


        incident_details$cad_identifier <- Incident_Edit$cad_identifier
        incident_details$incident_start_date <- Incident_Edit$incident_start |> 
          as.Date(tz = GetSetting('global', key = 'ltz'))
        incident_details$incident_start_time <- Incident_Edit$incident_start
        incident_details$incident_end_date <- Incident_Edit$incident_end  |> 
          as.Date(tz = GetSetting('global', key = 'ltz'))
        incident_details$incident_end_time <- Incident_Edit$incident_end
        incident_details$address <- Incident_Edit$address
        incident_details$area <- Incident_Edit$area |> as.character()
        incident_details$dispatch_reason <- Incident_Edit$dispatch_id |> as.character()
        incident_details$units <- incident_units |> as.character()
        incident_details$canceled <- Incident_Edit$canceled == 1
        incident_details$dropped <- Incident_Edit$dropped == 1
        

        showModal(modal$key_time(ns, incident_details, edit()))
      }) |>
        bindEvent(
          input$edit_incident, 
          ignoreNULL = TRUE, 
          ignoreInit = TRUE
        )
      
      # Add additional response
      observe({
        # browser()
        additional(TRUE)
        response_details$incident_id <- input$add_response
        log_info(glue('Adding additional response to incident {input$add_response}'), 
                 namespace = 'add_edit_incident')
        showModal(modal$key_time_additional(ns, response_details, rdfs))
        
      }) |> 
        bindEvent(input$add_response,
                  ignoreNULL = TRUE, 
                  ignoreInit = TRUE)
      
      # Edit response
      observe({
        browser()
        edit(TRUE)
        
        response <- rdfs$response |>
          filter(id == input$edit_response) 
        
        firefighter_apparatus <- response |> 
          left_join(rdfs$firefighter_response |> 
                      select(-id), 
                    by = c("id" = "response_id")) |>
          left_join(rdfs$firefighter_apparatus |> 
                      select(-id), 
                    by = c("id" = "response_id",
                           "firefighter_id")) |>
          select(firefighter_id, 
                 time_adjustment,
                 apparatus_id)
        
        response_details$apparatus <- firefighter_apparatus$apparatus_id |> 
          unique() |> 
          as.character()
        
        
        
        response_details$incident_id <- input$edit_response
        
        
        
        log_info(glue('Editing response id {input$edit_response}'), 
                 namespace = 'add_edit_incident')
        
        showModal(modal$key_time_additional(ns, response_details, rdfs))
      }) |> 
        bindEvent(input$edit_response)
      

    }
  )
}