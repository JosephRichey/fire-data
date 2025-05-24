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

log_trace("Loading incident_response.R", namespace = "incident_response")

box::use(
  app/logic/app_data,
  app/modal/modal,
  app/logic/local_functions[...],
  app/logic/global_functions[GetSetting,
                             BuildDateTime,
                             CheckWriteResult,
                             StringToId,
                             IdToString,
                             HipaaLog,
                             UpdateReactives],
  app/logic/logging,
  ./modal_handlers[...],
  ./cache_values[...],
  ./edit[...],
  ./write[...],
)

UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      ns("add_incident"), 
      "Add Incident", 
      class = "btn btn-primary")
  )
}


Server <- function(id, rdfs) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      ##########################################################################
      #             Reactives to save until write or reset
      ##########################################################################
      
      edit <- reactiveVal(FALSE)
      
      additional <- reactiveVal(FALSE)
      
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
      # These handlers cache the values of the inputs. See app/view/cache_values.R
      CacheToList(input, incident_details, response_details)
      ObserveAssignmentList(input, response_details)
      
      observe({
        # browser()
        
        # Check if the adjustment section was enabled
        # If it isn't, set all inputs to 0.
        if (!isTruthy(input$time_adjust_needed)) {
          # browser()
            lapply(response_details$firefighter, function(ff) {
              id <- paste0("time_adj_", tolower(ff) |> stringr::str_replace_all(" ", "_"))
              updateNumericInput(session, id, value = 0)
            })
          return()
        }
        
        # Iterate over each firefighter
        adjustment_list <- stats::setNames(
          lapply(response_details$firefighter, function(ff) {
            id <- paste0("time_adj_", tolower(ff) |> stringr::str_replace_all(" ", "_"))
            input[[id]]
          }),
          response_details$firefighter
        )
        
        response_details$time_adjustments <- adjustment_list
        
        log_trace(
          glue::glue("Cached time adjustments: {toString(adjustment_list)}"),
          namespace = "cache response details"
        )
      }) |> 
        bindEvent(input$time_adjust_needed, 
                  input$submit, 
                  input$to_assignment, 
                  input$to_apparatus_ff, 
                  ignoreInit = TRUE)
      
      
      ##########################################################################
      #                    Modal Navigation  
      ##########################################################################
      # These handlers deal with all validation and navigation between modals.
      # They are located in app/view/modal_handlers.R
      
      ObserveAddIncident(input, ns, incident_details, edit)
      ObserveToAddressUnit(input, ns, rdfs, incident_details, edit)
      ObserveToApparatusFf(input, ns, response_details, edit, additional)
      ObserveToAssignment(input, ns, response_details, edit, additional, rdfs)
      ObserveToNote(input, ns, response_details, edit, additional, rdfs)
      ObserveToKeyTimeAdditional(input, ns, response_details, edit, additional, rdfs)
      ObserveCancelModel(input, incident_details, response_details, edit, additional, session)
      
      ##########################################################################
      #                     Assignment Widget
      ##########################################################################
      # Render dynamic bucket list inside modal
      output$firefighter_apparatus_list <- renderUI({
        generate_firefighter_apparatus(input, ns, response_details, rdfs)
      }) |> 
        bindEvent(input$to_assignment)

      
      ##########################################################################
      #                Observers Watching for Edits or Additional Response
      ##########################################################################
      ObserveEditIncident(input, ns, incident_details, rdfs, edit)
      ObserveEditResponse(input, ns, response_details, rdfs, edit)
      ObserveAddResponse(input, ns, response_details, rdfs, additional)
      
      ##########################################################################
      #             Submit
      ##########################################################################
      observe({
        # Load values to pass to write functions
        

        removeModal()
        session$sendCustomMessage(type = "jsCode", list(code = "window.enableScroll();"))
        
        shinycssloaders::showPageSpinner(
          type = 6,
          color = "#87292b"
        )

        ## Pull Values into local variables
        # Coaleace the reactives with inputs
        # If an input doesn't change between multiple entries, the reactive won't 
        # catch it.
        inc_keys <- c(
          "cad_identifier", "incident_start_date", "incident_start_time",
          "incident_end_date", "incident_end_time", "address",
          "dispatch_reason", "area", "canceled", "dropped", "units"
        )
        resp_keys <- c(
          "incident_id", "response_id",
          "response_start_date", "response_start_time", "response_end_date",
          "response_end_time", "response_notes", "firefighter", "apparatus", "ff_app_lists"
        )
        
        # browser()
        inc_vals <- CoalesceReactiveWithInput(reactiveValuesToList(incident_details), input, inc_keys)
        resp_vals <- CoalesceReactiveWithInput(reactiveValuesToList(response_details), input, resp_keys)
  
          #FIXME Make consistent logic here and abstract into function. Be really clear on data flow.
        assignments <- resp_vals$ff_app_lists
        
        clean_names <- names(assignments) |>
          stringr::str_remove("app-incident_response-") |>
          stringr::str_remove("apparatus_list_") |>
          stringr::str_remove("_list") |>
          stringr::str_replace_all("_", " ") |>
          stringr::str_to_title()
        
        if(!is.null(assignments)) {
          assignments <- purrr::set_names(assignments, clean_names)
        }
        
        
        ##### Build Times #####
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
        
        #####
        
        write_results <- c()
        
        ##### Write to Incident Table ####
        # There are four possible scenarios for writing to the database
        #1 - Adding a new incident (which also adds a response)
        #2 - Editing an existing incident
        #3 - Adding a new response to an existing incident
        #4 - Editing an existing response
        
        
        #1 - Adding a new incident
        if(!edit() & !additional()) {
          return <- WriteIncident(
            session = session,
            write_results = write_results,
            inc_vals = inc_vals,
            inc_start_time = inc_start_time,
            inc_end_time = inc_end_time
          )
            
          write_results <- c(return[[1]])
          inc_vals$incident_id <- return[[2]]
        }
      
        # 2 - Editing an existing incident
        if(edit() & is.null(resp_vals$incident_id)) {
          write_results <- EditIncident(
            session = session,
            write_results = write_results,
            inc_vals = inc_vals,
            inc_start_time = inc_start_time,
            inc_end_time = inc_end_time,
            input = input
          )
          
        }
        
        # 3 - Adding a new response (to an existing incident or a new incident)
        if(
          (!edit() & !additional() & length(resp_vals$firefighter) > 0) | # For new incidents, where there is a response (there are firefighters assigned)
          (!edit() & additional()) # For adding additional incidents
        ) {

          write_results <- AddResponse(
            session = session,
            resp_start_time = if(is_empty(resp_start_time)) inc_start_time else resp_start_time,
            resp_end_time = if(is_empty(resp_end_time)) inc_end_time else resp_end_time,
            write_results = write_results,
            inc_vals = inc_vals,
            resp_vals = resp_vals,
            input,
            assignments = assignments
          )

        } 
        
        # 4 - Editing an existing response
        if (edit() & !is.null(resp_vals$incident_id)) {
          # browser()
          write_results <- EditResponse(
            session = session,
            resp_start_time = resp_start_time,
            resp_end_time = resp_end_time,
            write_results = write_results,
            resp_vals = resp_vals,
            input,
            assignments = assignments
          )
        }

        resetCachedValues(incident_details, response_details, edit, additional, session)

        UpdateReactives(rdfs)
        
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

        session$sendCustomMessage(type = "jsCode", list(code = "window.enableScroll();"))
        
      }) |> 
        bindEvent(input$submit, ignoreNULL = T)
      

    }
  )
}

log_trace("Loading incident_response.R complete", namespace = "incident_response")