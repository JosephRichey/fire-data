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
                             CheckWriteResult,
                             StringToId,
                             IdToString,
                             HippaLog,
                             UpdateReactives],
  app/logic/logging,
  ./modal_handlers[...],
)


Server <- function(id, rdfs) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      edit <- reactiveVal(FALSE)
      
      additional <- reactiveVal(FALSE)
      
      ##########################################################################
      #             Reactives to save until write or reset
      ##########################################################################
      incident_details <- reactiveValues(
        cad_identifier = NULL,
        # This field is used to cache what the cad id
        # is before the user edits it.
        # This allows us to still prevent duplicates.
        edit_cad_identifier = NULL,
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
        response_id = NULL,
        response_start_date = NULL,
        response_start_time = NULL,
        response_end_date = NULL,
        response_end_time = NULL,
        response_notes = NULL,
        apparatus = NULL,
        firefighter = NULL,
        ff_app_assignemnt = NULL
      )
      ##########################################################################
      #              Cache Data Automatically When Modified
      ##########################################################################
      # List of variables to bind
      incident_fields <- c(
        "cad_identifier", "incident_start_date", "incident_start_time", 
        "incident_end_date", "incident_end_time", 
        "address", "area", "dispatch_reason", "units", "canceled", 
        "dropped")
      
      response_fields <- c(
        "incident_id", "response_start_date", "response_start_time" ,
        "response_end_date", "response_end_time", "response_notes", 
        "apparatus", "firefighter", "ff_app_assignment"
      )
      
      # Iterate and bind each field dynamically
      walk(incident_fields, ~ bindEvent(
        observe({
          incident_details[[.x]] <- input[[.x]]
          log_trace(
            glue('Caching {paste(input[[.x]], collapse = ", ")} to {.x}'),
            namespace = 'cache incident details'
          )
        }),
        input[[.x]]
      ))
      
      walk(response_fields, ~ bindEvent(
        observe({
          response_details[[.x]] <- input[[.x]]
          log_trace(
            glue('Caching {paste(input[[.x]], collapse = ", ")} to {.x}'),
            namespace = 'cache response details'
          )
        }),
        input[[.x]]
      ))
      
      # Shiny doesn't recognize setting select inputs null as a change.
      # Build custom observers.
      observe({
        val <- input$firefighter
        # if it’s NULL or length-zero, clear the cache
        response_details$firefighter <-
          if (is.null(val) || length(val) == 0L) character(0) else val
      }) |> bindEvent(input$firefighter)
      
      observe({
        val <- input$apparatus
        # if it’s NULL or length-zero, clear the cache
        response_details$apparatus <-
          if (is.null(val) || length(val) == 0L) character(0) else val
      }) |> 
        bindEvent(input$apparatus)
      
      observeEvent(input$ff_app_lists, {
        # Update cached source of truth
        # Reverse transform names from shiny friendly to user friendly
        clean_names <- names(input$ff_app_lists) |>
          stringr::str_remove("app-incident_response-") |>
          stringr::str_remove("apparatus_list_") |>
          stringr::str_remove("_list") |>
          stringr::str_replace_all("_", " ") |>
          stringr::str_to_title()
        
        response_details$ff_app_lists <- purrr::set_names(input$ff_app_lists, clean_names)
        
        log_trace(
          glue('Cached ff_app_lists: {paste(response_details$ff_app_lists, collapse = ", ")}'),
          namespace = 'observe input$ff_app_lists'
        )
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      ##########################################################################
      #                    Modal Navigation  
      ##########################################################################
      # These handlers deal with all validation and navigation between modals.
      # They are located in app/view/modal_handlers.R
      
      observeAddIncident(input, ns, incident_details, edit)
      observeToAddressUnit(input, ns, rdfs, incident_details, edit)
      observeToApparatusFf(input, ns, response_details, edit, additional)
      observeToAssignment(input, ns, response_details, edit, additional)
      observeToNote(input, ns, response_details, edit, additional)
      
      
      
      
      ##########################################################################
      #                     Assignment Widget
      ##########################################################################
      # This function builds the buckets appropriately
      # using cached inputs and data from the database.
      generate_firefighter_apparatus <- function() {
        req(input$firefighter)
        log_trace("Begin generating firefighter apparatus list widget", 
                  namespace = "generate_firefighter_apparatus")
        
        # Determine current assignments
        assignments <- if (!is.null(isolate(response_details$ff_app_lists))) {
          # If a cached list exists, use it
          log_trace("Using cached bucket list input", 
                    namespace = "generate_firefighter_apparatus")
          isolate(response_details$ff_app_lists)
        } else if (!is.null(isolate(response_details$response_id))) {
          # If a response ID exists, use it to get the current assignments
          log_trace("Using existing response DB data", 
                    namespace = "generate_firefighter_apparatus")
          
          rdfs$firefighter_apparatus |>
            filter(response_id == isolate(response_details$response_id)) |>
            left_join(app_data$Firefighter |> select(id, full_name), 
                      by = c("firefighter_id" = "id")) |>
            left_join(app_data$Apparatus |> select(id, apparatus_name), 
                      by = c("apparatus_id" = "id")) |>
            # Only use firefighters that are in the input list
            filter(full_name %in% input$firefighter) |>
            group_by(apparatus_name) |>
            summarise(assigned = list(full_name), .groups = "drop") |>
            tibble::deframe()
        } else {
          log_trace("No cached list or response ID found", 
                    namespace = "generate_firefighter_apparatus")
          list()  # no assignment yet
        }
        
        # Track current firefighter IDs in use
        assigned_ffs <- unlist(assignments, use.names = FALSE) |> 
          # Only use firefighters that are in the input list
          intersect(input$firefighter)
        
        standby_ffs <- setdiff(input$firefighter, assigned_ffs)
        # re-add any previously-cached standby
        if ("Standby" %in% names(assignments)) {
          standby_ffs <- union(standby_ffs, assignments[["Standby"]])
        }
        # bring back any FFs whose apparatus was removed,
        # but don’t treat “Standby” as an apparatus
        removed <- setdiff(names(assignments), c(input$apparatus, "Standby"))
        if (length(removed)) {
          standby_ffs <- union(standby_ffs,
                               unlist(assignments[removed], use.names = FALSE))
        }
        
        # One last check to make sure we don't have any duplicates and don't have any ffs not in input
        standby_ffs <- unique(standby_ffs)
        standby_ffs <- intersect(standby_ffs, input$firefighter)
        
        # UI construction
        do.call(bucket_list, c(
          list(
            header = NULL,
            group_name = ns("ff_app_lists"),
            orientation = "horizontal",
            add_rank_list(
              text = "Standby",
              labels = standby_ffs,
              input_id = ns("standby_list"),
              options = sortable_options(
                group = "bucket_list",
                class = "sortable-item"
              )
            )
          ),
          lapply(input$apparatus, function(apparatus_name) {
            app_ffs <- assignments[[apparatus_name]] %||% character(0) |> 
              intersect(input$firefighter)
            
            add_rank_list(
              text = apparatus_name,
              labels = app_ffs,
              input_id = paste0("apparatus_list_", 
                                stringr::str_to_lower(apparatus_name) |>
                                  stringr::str_replace_all(" ", "_")),
              options = sortable_options(
                group = "bucket_list",
                class = "sortable-item"
              )
            )
          })
        ))
      }
      
      # Render dynamic bucket list inside modal
      output$firefighter_apparatus_list <- renderUI({
        generate_firefighter_apparatus()
      }) |> 
        bindEvent(input$to_assignment)

      
      ##########################################################################
      #                 Launch Edit Incident
      ##########################################################################
      observe({
        edit(TRUE)
        
        Incident_Edit <- rdfs$incident |>
          filter(id == input$edit_incident)
        
        incident_units <- rdfs$incident_unit |>
          filter(incident_id == Incident_Edit$id) |> 
          left_join(app_data$Unit, 
                    by = c("unit_type_id" = "id")) |> 
          pull(unit_type_id)
        
        
        incident_details$cad_identifier <- Incident_Edit$cad_identifier
        incident_details$edit_cad_identifier <- Incident_Edit$cad_identifier
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
        
        log_trace(
          glue('Editing incident {input$edit_incident}'),
          namespace = 'observe input$edit_incident'
        )
        log_trace(
          glue('incident_details: {paste(incident_details, collapse = ", ")}'),
          namespace = 'observe input$edit_incident'
        )
        
        
        showModal(modal$key_time(ns, incident_details, edit()))
      }) |>
        bindEvent(
          input$edit_incident, 
          ignoreNULL = TRUE, 
          ignoreInit = TRUE
        )
      
      ##########################################################################
      #                            Add additional response
      ##########################################################################
      observe({
        additional(TRUE)
        # Load respons_details vector

        
        response_details$incident_id <- input$add_response
        
        incident <- rdfs$incident |>
          filter(id == input$add_response) 
        
        response_details$response_start_date <- incident$incident_start |> 
          as.Date(tz = GetSetting('global', key = 'ltz'))
        response_details$response_start_time <- incident$incident_start
        response_details$response_end_date <- incident$incident_end |> 
          as.Date(tz = GetSetting('global', key = 'ltz'))
        response_details$response_end_time <- incident$incident_end
        
        
        
        log_trace(glue('Adding additional response to incident {input$add_response}'), 
                 namespace = 'observe input$add_response')
        showModal(modal$key_time_additional(ns, response_details, rdfs))
        
        
      }) |> 
        bindEvent(input$add_response,
                  ignoreNULL = TRUE, 
                  ignoreInit = TRUE)
      
      ##########################################################################
      #                          Edit response
      ##########################################################################
      observe({
        # browser()
        req(input$edit_response)
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
          left_join(app_data$Firefighter |>
                      select(id, full_name), 
                    by = c("firefighter_id" = "id")) |>
          left_join(app_data$Apparatus |>
                      select(id, apparatus_name),
                    by = c("apparatus_id" = "id")) |>
          select(full_name, 
                 time_adjustment,
                 apparatus_name)
        
        response_details$response_id <- response$id
        response_details$apparatus <- firefighter_apparatus$apparatus_name |> 
          unique() |> 
          as.character()
        response_details$firefighter <- firefighter_apparatus$full_name |>
          as.character()
        
        response_details$incident_id <- input$edit_response
        
        response_details$response_start_date <- response$response_start |> 
          as.Date(tz = GetSetting('global', key = 'ltz'))
        response_details$response_start_time <- response$response_start
        response_details$response_end_date <- response$response_end |> 
          as.Date(tz = GetSetting('global', key = 'ltz'))
        response_details$response_end_time <- response$response_end

        response_details$response_notes <- response$notes |> 
          as.character()
        
        log_trace(glue('Editing response id {input$edit_response}'), 
                 namespace = 'observe input$edit_response')
        
        log_trace(
          glue('response_details: {paste(response_details, collapse = ", ")}'),
          namespace = 'observe input$edit_response'
        )
        
        print(input$edit_response)
        print(input$add_additional_response)

        
        showModal(modal$key_time_additional(ns, response_details, rdfs))
      }) |> 
        bindEvent(input$edit_response, input$add_additional_response,
                  ignoreNULL = TRUE, 
                  ignoreInit = TRUE)
      
      ##########################################################################
      #                  Cancel and reset values
      ##########################################################################
      observeEvent(input$cancel_modal, {
        removeModal()
        
        log_trace(
          glue('Resetting values'),
          namespace = 'observe input$cancel_modal'
        )
        local_functions$resetCachedValues(incident_details, response_details,
                                          edit, additional)
        

        
        
        # browser()
        updateSelectInput(session, "edit_response", selected = NULL)
        
      })
      
      
      
      
      ##########################################################################
      #             Everything that happens on submit
      ##########################################################################
      observe({
        
        shinycssloaders::showPageSpinner(
          type = 6,
          color = "#87292b"
        )

        ## Pull Values into local variables
        inc_vals <- reactiveValuesToList(incident_details)
        resp_vals <- reactiveValuesToList(response_details)
        assignments <- response_details$ff_app_lists
        
        inc_start_time <- BuildDateTime(
          time = inc_vals$incident_start_time,
          date = inc_vals$incident_start_date,
          input = 'local',
          return_type = 'UTC'
        ) |> 
          format("%Y-%m-%d %H:%M:%S")
        
        inc_end_time <- BuildDateTime(
          time = inc_vals$incident_end_time,
          date = inc_vals$incident_end_date,
          input = 'local',
          return_type = 'UTC'
        ) |> 
          format("%Y-%m-%d %H:%M:%S")
        
        resp_start_time <- BuildDateTime(
          time = resp_vals$response_start_time,
          date = resp_vals$response_start_date,
          input = 'local',
          return_type = 'UTC'
        ) |> 
          format("%Y-%m-%d %H:%M:%S")
        
        resp_end_time <- BuildDateTime(
          time = resp_vals$response_end_time,
          date = resp_vals$response_end_date,
          input = 'local',
          return_type = 'UTC'
        ) |> 
          format("%Y-%m-%d %H:%M:%S")
        
        write_results <- c()
        
        ##### Write to Incident Table ####
        # There are four possible scenarios for writing to the database
        #1 - Adding a new incident (which also adds a response)
        #2 - Editing an existing incident
        #3 - Adding a new response to an existing incident
        #4 - Editing an existing response
        
        ### Tables that need to be modified
        # incidient (inc)
        # response (resp)
        # incident_unit (inc)
        # firefighter_response (resp)
        # apparatus_response (resp)
        # firefighter_apparatus (resp)
        
        
        
        
        
        #1 - Adding a new incident
        if(!edit() & !additional()) {

          log_trace(
            glue('Preparing statement for adding new incident'),
            namespace = 'observe input$submit'
          )

          incident_sql_statement <- "INSERT INTO ?incident_table
          (cad_identifier, incident_start, incident_end,
          address, dispatch_id, area_id,
          canceled, dropped, is_reviewed,
          is_locked, is_deleted, deleted_by) VALUES
          (?cad_identifier, ?incident_start, ?incident_end,
          ?address, ?dispatch_id, ?area_id,
          ?canceled, ?dropped, 0, 0, NULL, NULL)"

          incident_safe_sql <- sqlInterpolate(
            app_data$CON,
            incident_sql_statement,
            incident_table = SQL('incident'),
            cad_identifier = inc_vals$cad_identifier,
            incident_start = inc_start_time,
            incident_end = inc_end_time,
            address = if (is.null(inc_vals$address)) NA else inc_vals$address,
            dispatch_id = inc_vals$dispatch_reason,
            area_id = inc_vals$area,
            canceled = if (inc_vals$canceled) 1 else 0,
            dropped = if (inc_vals$dropped) 1 else 0
          )
          
          # Execute and write to HIPPA log
          result <- dbExecute(app_data$CON, incident_safe_sql)
          
          log_trace("Identifying id for incident we just added")
          inc_vals$incident_id <- dbGetQuery(app_data$CON, "SELECT LAST_INSERT_ID()") |>
            pull(`LAST_INSERT_ID()`) |> as.numeric()
          
          HippaLog(
            incident_safe_sql |> as.character(),
            session
          )
          
          # Append result to write_resuls
          write_results <- c(write_results,
                             CheckWriteResult(
                              result,
                              context = 'adding an incident',
                              showMessage = FALSE
                            )
          )
          
          
          
          log_trace(
            glue::glue("Preparing statement for incident_unit"),
            namespace = "Submit"
          )
          
          
          #### Write to incident_unit ####
          for(i in inc_vals$units) {
            result <- dbExecute(
              app_data$CON,
              glue("INSERT INTO incident_unit
                    (incident_id, unit_type_id) VALUES
                    ({inc_vals$incident_id}, {i})")
            )
            
            write_results <- c(
              write_results,
              CheckWriteResult(
                result,
                context = 'adding an incident unit',
                showMessage = FALSE
              )
            )
          }
          
        }
        
        # 2 - Editing an existing incident
        if(edit() & is.null(resp_vals$incident_id)) {

          log_trace(
            glue('Preparing statement for editing incident'),
            namespace = 'observe input$submit'
          )
          
          # If editing an incident, update the incident table
          incident_sql_statement <- "UPDATE ?incident_table
          SET cad_identifier = ?cad_identifier,
          incident_start = ?incident_start,
          incident_end = ?incident_end,
          address = ?address,
          dispatch_id = ?dispatch_id,
          area_id = ?area_id,
          canceled = ?canceled,
          dropped = ?dropped,
          is_reviewed = 0,
          is_locked = 0,
          is_deleted = NULL,
          deleted_by = NULL
          WHERE id = ?id"
          
          incident_safe_sql <- sqlInterpolate(
            app_data$CON,
            incident_sql_statement,
            incident_table = SQL('incident'),
            cad_identifier = inc_vals$cad_identifier,
            incident_start = inc_start_time,
            incident_end = inc_end_time,
            address = if (is.null(inc_vals$address)) NA else inc_vals$address,
            dispatch_id = inc_vals$dispatch_reason,
            area_id = inc_vals$area,
            canceled = if (inc_vals$canceled) 1 else 0,
            dropped = if (inc_vals$dropped) 1 else 0,
            id = input$edit_incident
          )
          
          # Execute and write to HIPPA log
          result <- dbExecute(app_data$CON, incident_safe_sql)
          HippaLog(
            incident_safe_sql |> as.character(),
            session
          )
          
          # Append result to write_resuls
          write_results <- c(write_results,
                             CheckWriteResult(
                               result,
                               context = 'editing an incident',
                               showMessage = FALSE
                             )
          )
          
          
          log_trace(
            glue::glue("Preparing statement for incident_unit"),
            namespace = "Submit"
          )
          
          # Write to incident_unit
          # Delete all incident units for this incident
          dbExecute(
            app_data$CON,
            glue("DELETE FROM incident_unit WHERE incident_id = {input$edit_incident}")
          )
          
          # Add new incident units
          for(i in inc_vals$units) {
            result <- dbExecute(
              app_data$CON,
              glue("INSERT INTO incident_unit
                    (incident_id, unit_type_id) VALUES
                    ({input$edit_incident}, {i})")
            )
            
            write_results <- c(
              write_results,
              CheckWriteResult(
                result,
                context = 'editing an incident unit',
                showMessage = FALSE
              )
            )
          }
          
          
        }
        
        # 3 - Adding a new response (to an existing incident or a new incident)
        if(
          (!edit() & !additional() & length(resp_vals$firefighter) > 0) | # For new incidents, where there is a response (there are firefighters assigned)
          (!edit() & additional()) # For adding additional incidents
        ) {
          

          ##### Write to Response Table #####
          response_sql_statement <- "INSERT INTO ?response_table
          (incident_id, response_start, response_end,
          notes, is_deleted, deleted_by) VALUES
          (?incident_id, ?response_start, ?response_end,
          ?notes, NULL, NULL)"

          response_safe_sql <- sqlInterpolate(
            app_data$CON,
            response_sql_statement,
            response_table = SQL('response'),
            
            incident_id = coalesce(inc_vals$incident_id, input$add_response),
            # Since this handles new incidents and 
            # additional responses, coalesce the two times
            response_start = if(is_empty(resp_start_time)) inc_start_time else resp_start_time,
            response_end = if(is_empty(resp_end_time)) inc_end_time else resp_end_time,
            notes = if (is.null(resp_vals$response_notes)) NA else resp_vals$response_notes
          )
          
          
          # Execute the safely interpolated SQL command
          result <- tryCatch(
            {
              dbExecute(app_data$CON, response_safe_sql)
            },
            error = function(e) {
              log_error(glue::glue("Database write failed: {e$message}"), namespace = "Add Response")
              NA
            }
          )
          
          # Grab the response id
          resp_vals$response_id <- dbGetQuery(app_data$CON, "SELECT LAST_INSERT_ID()") |>
            pull(`LAST_INSERT_ID()`) |> as.numeric()
          
          HippaLog(
            response_safe_sql |> as.character(),
            session
          )
          
          # Append result to write_resuls
          write_results <- c(write_results,
                             CheckWriteResult(
                               result,
                               context = 'adding a response',
                               showMessage = FALSE
                             )
          )
          
          ###### Firefighter incident statement preparation ######
          # No interpolation needed here, just a loop to build the statement
          firefighter_response_statement <- paste0("INSERT INTO firefighter_response
                                                   (response_id, firefighter_id, time_adjustment) VALUES "
              )
          
          if(!is.null(resp_vals$firefighter)) {
            time_adjustments <- stats::setNames(
              lapply(resp_vals$firefighter, function(ff) {
                # sanitize exactly the same way as before
                sanitized <- ff  |> 
                  tolower() |> 
                  stringr::str_replace_all(" ", "_")
                
                # grab the numeric input value
                input[[ paste0("time_adj_", sanitized) ]]
              }),
              resp_vals$firefighter
            )
            
            # loop and append each tuple
            for (ff in names(time_adjustments)) {
              ff_id   <- StringToId(app_data$Firefighter, full_name, ff)
              adj_val <- time_adjustments[[ff]]
              
              firefighter_response_statement <- paste0(
                firefighter_response_statement,
                "('", resp_vals$response_id, "',",
                ff_id, ",",
                adj_val, "),"
              )
            }
            
            
            # Replace the last comma with a semi-colon
            firefighter_response_statement <- sub(",([^,]*)$", ";\\1", firefighter_response_statement)
            
            
            
          }
          
          if(!is.null(resp_vals$apparatus)) {

            ###### Apparatus incident statement preparation ######
            # No interpolation needed here, just a loop to build the statement
            apparatus_response_statement <- "INSERT INTO apparatus_response (response_id, apparatus_id) VALUES "
  
            for(app in resp_vals$apparatus) {
              apparatus_response_statement <- paste0(
                apparatus_response_statement, "('",
                resp_vals$incident_id, "',",
                StringToId(
                  app_data$Apparatus,
                  apparatus_name,
                  app
                  )
                , "),"
              )
            }
  
            # Replace the last comma with a semi-colon
            apparatus_response_statement <- sub(",([^,]*)$", ";\\1", apparatus_response_statement)
  
            ###### Firefighter Apparatus statement preparation ######
            # No interpolation needed here, just a loop to build the statement
            firefighter_apparatus_statement <- "INSERT INTO firefighter_apparatus (response_id, firefighter_id, apparatus_id) VALUES "
  
            for(app in resp_vals$apparatus) {
              ffs <- assignments[[app]]
              app_id <- StringToId(app_data$Apparatus, apparatus_name, app)
  
              # Loop through each firefighter in the apparatus
              for(ff in ffs) {
                # Get the firefighter id
                ff_id <- StringToId(app_data$Firefighter, full_name, ff)
                
                # Append the statement
                firefighter_apparatus_statement <- paste0(firefighter_apparatus_statement, "('",
                                                                   resp_vals$response_id, "',",
                                                                   ff_id, ",",
                                                                   app_id, "),"
                )
              }
  
            }
  
            # Replace the last comma with a semi-colon
            firefighter_apparatus_statement <- sub(",([^,]*)$", ";\\1", firefighter_apparatus_statement)
            
            # Execute the statements
            result <- dbExecute(app_data$CON, firefighter_response_statement)
            HippaLog(
              firefighter_response_statement |> as.character(),
              session
            )
            write_results <- c(write_results,
                               CheckWriteResult(
                                 result,
                                 context = 'adding a firefighter response',
                                 showMessage = FALSE
                               )
            )
          }
          
          #####
          
          
          

        } 
        
        # 4 - Editing an existing response
        if (edit() & !is.null(resp_vals$incident_id)) {

          log_trace(
            glue('Preparing statement for editing response'),
            namespace = 'observe input$submit'
          )
          
          # If editing a response, update the response table
          response_sql_statement <- "UPDATE ?response_table
          SET response_start = ?response_start,
          response_end = ?response_end,
          notes = ?notes,
          is_deleted = NULL,
          deleted_by = NULL
          WHERE id = ?id"

          response_safe_sql <- sqlInterpolate(
            app_data$CON,
            response_sql_statement,
            response_table = SQL('response'),
            response_start = resp_start_time,
            response_end = resp_end_time,
            notes = if (is.null(resp_vals$response_notes)) NA else 'resp_vals$response_notes',
            id = input$edit_response
          )
          

          
          # Execute and write to HIPPA log
          result <- dbExecute(app_data$CON, response_safe_sql)
          HippaLog(
            response_safe_sql |> as.character(),
            session
          )
          
          # Append result to write_resuls
          write_results <- c(write_results,
                             CheckWriteResult(
                               result,
                               context = 'editing a response',
                               showMessage = FALSE
                             )
          )
          
          # Delete old firefighter response, apparatus response, and firefighter apparatus
          dbExecute(
            app_data$CON,
            glue("DELETE FROM firefighter_response WHERE response_id = {input$edit_response}")
          )
          
          dbExecute(
            app_data$CON,
            glue("DELETE FROM apparatus_response WHERE response_id = {input$edit_response}")
          )
          
          dbExecute(
            app_data$CON,
            glue("DELETE FROM firefighter_apparatus WHERE response_id = {input$edit_response}")
          )
          
          if(!is.null(resp_vals$firefighter)) {
            # Add new firefighter response, apparatus response, and firefighter apparatus
            # Write to firefighter_response
            # No interpolation needed here, just a loop to build the statement
            firefighter_response_statement <- paste0("INSERT INTO firefighter_response
                                                     (response_id, firefighter_id, time_adjustment) VALUES "
            )
            
            time_adjustments <- stats::setNames(
              lapply(resp_vals$firefighter, function(ff) {
                # sanitize exactly the same way as before
                sanitized <- ff  |> 
                  tolower() |> 
                  stringr::str_replace_all(" ", "_")
                
                # grab the numeric input value
                input[[ paste0("time_adj_", sanitized) ]]
              }),
              resp_vals$firefighter
            )
            
            # loop and append each tuple
            for (ff in names(time_adjustments)) {
              ff_id   <- StringToId(app_data$Firefighter, full_name, ff)
              adj_val <- time_adjustments[[ff]]
              
              firefighter_response_statement <- paste0(
                firefighter_response_statement,
                "('", input$edit_response, "',",
                ff_id, ",",
                adj_val, "),"
              )
            }
            
            # Replace the last comma with a semi-colon
            firefighter_response_statement <- sub(",([^,]*)$", ";\\1", firefighter_response_statement)
          
          }
          
          if(!is.null(resp_vals$apparatus)) {
          
            # Write to firefighter_apparatus
            # No interpolation needed here, just a loop to build the statement
            firefighter_apparatus_statement <- paste0("INSERT INTO firefighter_apparatus
                                                     (response_id, firefighter_id, apparatus_id) VALUES "
            )
            
            for(app in resp_vals$apparatus) {
              ffs <- assignments[[app]]
              app_id <- StringToId(app_data$Apparatus, apparatus_name, app)
              
              # Loop through each firefighter in the apparatus
              for(ff in ffs) {
                # Get the firefighter id
                ff_id <- StringToId(app_data$Firefighter, full_name, ff)
                
                # Append the statement
                firefighter_apparatus_statement <- paste0(firefighter_apparatus_statement, "('",
                                                                   input$edit_response, "',",
                                                                   ff_id, ",",
                                                                   app_id, "),"
                )
              }
              
            }
            
            # Replace the last comma with a semi-colon
            firefighter_apparatus_statement <- sub(",([^,]*)$", ";\\1", firefighter_apparatus_statement)
            
            # Write to apparatus_response
            # No interpolation needed here, just a loop to build the statement
            apparatus_response_statement <- paste0("INSERT INTO apparatus_response
                                                     (response_id, apparatus_id) VALUES "
            )
            
            for(app in resp_vals$apparatus) {
              apparatus_response_statement <- paste0(apparatus_response_statement, "('",
                                                                   input$edit_response, "',",
                                                                   StringToId(
                                                                     app_data$Apparatus,
                                                                     apparatus_name,
                                                                     app
                                                                   ), "),"
              )
            }
            
            # Replace the last comma with a semi-colon
            apparatus_response_statement <- sub(",([^,]*)$", ";\\1", apparatus_response_statement)
            
            # Execute the statements
            result <- dbExecute(app_data$CON, firefighter_response_statement)
            HippaLog(
              firefighter_response_statement |> as.character(),
              session
            )
            write_results <- c(write_results,
                               CheckWriteResult(
                                 result,
                                 expectedMax = length(resp_vals$firefighter),
                                 context = 'editing a firefighter response',
                                 showMessage = FALSE
                               )
            )
            
            result <- dbExecute(app_data$CON, firefighter_apparatus_statement)
            HippaLog(
              firefighter_apparatus_statement |> as.character(),
              session
            )
            write_results <- c(write_results,
                               CheckWriteResult(
                                 result,
                                 expectedMax = length(resp_vals$firefighter),
                                 context = 'editing a firefighter apparatus',
                                 showMessage = FALSE
                               )
            )
            
            result <- dbExecute(app_data$CON, apparatus_response_statement)
            HippaLog(
              apparatus_response_statement |> as.character(),
              session
            )
            
            write_results <- c(write_results,
                               CheckWriteResult(
                                 result,
                                 expectedMax = length(resp_vals$firefighter),
                                 context = 'editing an apparatus response',
                                 showMessage = FALSE
                               )
            )
  
            }
        }
        
        local_functions$resetCachedValues(incident_details, response_details, edit, additional)
        
        UpdateReactives(rdfs)

        removeModal()
        
        shinycssloaders::hidePageSpinner()
        
        if(length(write_results) > 0) {
          # Check if any of the writes failed
          if(any(write_results == FALSE)) {
            shinyalert(
              title = "Error",
              text = "There was an error saving the incident",
              type = "error"
            )
            return()
          } else {
            shinyalert(
              title = "Success",
              text = "Incident saved",
              type = "success"
            )
          }
        } else if (length(write_results) == 0) {
          # If no writes were attempted, show a warning
          shinyalert(
            title = "Warning",
            text = "No changes were made to the incident",
            type = "warning"
          )
          return()
        }
        
        
        
      }) |> 
        bindEvent(input$submit, ignoreNULL = T)
      

    }
  )
}