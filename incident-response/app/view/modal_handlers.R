box::use(
  shiny[...],
  shinyalert[shinyalert],
  glue[glue],
  logger[...],
  dplyr[...],
  ../logic/global_functions[...],
  ../modal/modal
)

log_trace("Loading modal_handlers.R", namespace = "modal_handlers")

#' @export
ObserveAddIncident <- function(input, ns, incident_details, edit) {
  observe({
    log_trace(
          glue('Showing incident key time modal in {dplyr::if_else(edit(), "edit", "add")} mode.'),
          namespace = 'observe input$add_incident'
          )
    showModal(modal$key_time(ns, incident_details, edit))
  }) |> 
    bindEvent(
      input$add_incident, 
      ignoreInit = TRUE, 
      ignoreNULL = TRUE
      )
}

#' @export
ObserveToAddressUnit <- function(input, ns, rdfs, incident_details, edit) {
  log_trace(
    "Validating inputs for key time modal.",
    namespace = 'observe input$to_address_unit'
  )
  
  observe({
    # Check for duplicate IDs
    log_trace(
      glue('Checking for duplicate cad id {input$cad_identifier}'),
      namespace = 'observe input$to_address_unit'
    )
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
    } else {
      log_trace(
        glue("Editing incident {input$edit_incident}", 
             namespace = 'observe input$to_address_unit')
      )
      # Special check when editing an incident
      already_used_cad_ids <- rdfs$incident |>
        filter(cad_identifier != incident_details$edit_cad_identifier) |> 
        pull(cad_identifier)
      
      if(input$cad_identifier %in% already_used_cad_ids) {
        shinyalert(
          title = "Error",
          text = "Incident ID already exists",
          type = "error"
        )
        return()
      }
    }
    
    log_success(
      "No duplicate cad ids exist.",
      namespace = 'observe input$to_address_unit'
    )
    
    
    # Does the CAD ID match the regex?
    log_trace(
      glue('Checking cad id {input$cad_identifier} against regex'),
      namespace = 'observe input$to_address_unit'
    )
    log_trace(
      glue('Regex: {GetSetting("incident", key = "incident_cad_regex")}'),
      namespace = 'observe input$to_address_unit'
    )
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
    
    log_success(
      "CAD ID matches regex.",
      namespace = 'observe input$to_address_unit'
    )
    
    log_trace(
      glue('Checking if dispatch time {input$incident_start_time} is before end time {input$incident_end_time}'),
      namespace = 'observe input$to_address_unit'
    )
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
    log_trace(
      glue('Start time: {start_time}'),
      namespace = 'observe input$to_address_unit'
    )
    log_trace(
      glue('End time: {end_time}'),
      namespace = 'observe input$to_address_unit'
    )
    
    if(end_time <= start_time) {
      shinyalert(
        title = "Error",
        text = "Dispatch time must be before end time",
        type = "error"
      )
      return()
    }
    
    log_success(
      "Dispatch time is before end time.",
      namespace = 'observe input$to_address_unit'
    )
    
    log_info(
      "All validations passed.",
      namespace = 'observe input$to_address_unit'
    )
    
    # All validations passed. Show second modal.
    
    log_trace(
      glue('Showing address unit modal in {dplyr::if_else(edit(), "edit", "add")} mode'),
      namespace = 'observe input$to_address_unit'
    )
    showModal(modal$address_unit(ns, incident_details, edit()))
  }) |> 
    bindEvent(
      input$to_address_unit, 
      ignoreInit = TRUE, 
      ignoreNULL = TRUE
    )
}


#' @export
ObserveToApparatusFf <- function(input, ns, response_details, edit, additional) {
  observe({
    # Validate inputs ONLY for add and edit additional response
    # Use edit and additional. Flow path for edit incident never makes it here.
    if(edit() | 
       additional()) {
      log_trace(
        glue('Checking if dispatch time {input$incident_start_time} is before end time {input$incident_end_time}'),
        namespace = 'observe input$to_address_unit'
      )
      # Are times in order?
      start_time <- BuildDateTime(
        time = input$response_start_time,
        date = input$response_start_date,
        input = 'local',
        return_type = 'local'
      )
      
      end_time <- BuildDateTime(
        time = input$response_end_time,
        date = input$response_end_date,
        input = 'local',
        return_type = 'local'
      )
      log_trace(
        glue('Start time: {start_time}'),
        namespace = 'observe input$to_address_unit'
      )
      log_trace(
        glue('End time: {end_time}'),
        namespace = 'observe input$to_address_unit'
      )
      
      if(end_time <= start_time) {
        shinyalert(
          title = "Error",
          text = "Dispatch time must be before end time",
          type = "error"
        )
        return()
      }
      
      log_success(
        "Dispatch time is before end time.",
        namespace = 'observe input$to_address_unit'
      )
    }
    
    
    log_trace(
      glue('Showing firefighter apparatus modal in {if_else(edit(), "edit", "add")} {if_else(additional(), "response", "incident")} mode'),
      namespace = 'observe input$to_apparatus_ff'
    )
    
    showModal(
      modal$select_ff_aparatus(
        ns,
        response_details,
        additional(),
        edit())
    ) 
  }) |>
    bindEvent(
      input$to_apparatus_ff,
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
}

#' @export
ObserveToAssignment <- function(input, ns, response_details, edit, additional, rdfs) {
  observe({
    
    if(length(input$firefighter) == 0 & 
       length(input$apparatus) != 0) {
      shinyalert(
        title = "Error",
        text = "If you've select an apparatus, you must also select a firefighter",
        type = "error"
      )
      return()
    }

    # If the user has not selected any firefighters or apparatus, skip to notes modal
    if(length(input$apparatus) == 0 & 
       length(input$firefighter) == 0) {
      response_details$apparatus <- NULL
      response_details$firefighter <- NULL
      log_trace(
        'Skipping to notes modal. No firefighters or apparatus selected',
        namespace = 'observe input$to_assignment'
      )
      showModal(modal$note(ns, response_details, length = length(input$firefighter) + 
                             length(input$apparatus), rdfs))
      return()
    }
    
    # If the appaaratus assignemnt model is toggled off, skip to notes modal
    if(!GetSetting(domain = 'incident',
                   key = 'track_apparatus_assignments')) {
      log_trace(
        'Skipping to notes modal. Apparatus assignment tracking is disabled',
        namespace = 'observe input$to_assignment'
      )
      showModal(modal$note(ns, response_details, length = length(input$firefighter) + 
                             length(input$apparatus), rdfs))
      return()
    }
    
    
    log_trace(
      glue('Showing Assign Firefighters to Apparatus modal in {if_else(edit(), "edit", "add")} {if_else(additional(), "response", "incident")} mode'),
      namespace = 'observe input$to_assignment'
    )
    
    showModal(modalDialog(
      title = "Assign Firefighters to Apparatus",
      uiOutput(ns("firefighter_apparatus_list")),
      footer = tagList(
        actionButton(ns("to_apparatus_ff"), "Back", class = "btn btn-light"),
        actionButton(ns("cancel_modal"), "Cancel", class = "btn btn-warning"),
        actionButton(ns("to_note"), "Next", class = "btn btn-primary")
      ), size = 'l'
    ))
    
  }) |>
    bindEvent(input$to_assignment, ignoreNULL = TRUE, ignoreInit = TRUE)
}

#' @export
ObserveToNote <- function(input, ns, response_details, edit, additional, rdfs) {

  observe({
    # Check if stanby is allowed and conditions met
    if(!GetSetting(domain = 'incident', 
                   key = 'standby_allowed')) {
      log_trace(
        "standby is not an allowed status. Checking if there are any firefighters in standby.",
        namespace = 'observe input$to_note'
      )

      if(length(response_details$ff_app_lists$Standby) > 0 & # If there are firefighters in standby
         length(response_details$apparatus) != 0) { # And there are other possible options...
        # Issue a warning.
        log_warn(
          "Standby is not an allowed status. Showing warning modal.",
          namespace = 'observe input$to_note'
        )
        shinyalert(
          title = "Warning",
          text = "Standby is not an allowed status. Please assign all firefighters to an apparatus.",
          type = "warning"
        )
        return()
      }
    }
    
    log_trace(
      glue('Showing note modal in {if_else(edit(), "edit", "add")} {if_else(additional(), "response", "incident")} mode'),
      namespace = 'observe input$to_note'
    )
    showModal(modal$note(ns, response_details, length = length(input$firefighter) + 
                           length(input$apparatus), rdfs))
    
  }) |>
    bindEvent(
      input$to_note,
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
}

#' @export
# Because we have inputs that are created and managed by JS,
# we have to use (or it's easier to use) inputs on the R side
# to control the backtracking feature.
ObserveToKeyTimeAdditional <- function(input, ns, response_details, edit, additional, rdfs) {
  observe({
    log_trace(
      glue('Showing key time additional modal in {if_else(edit(), "edit", "add")} {if_else(additional(), "response", "incident")} mode'),
      namespace = 'observe input$to_key_time_additional'
    )
    showModal(modal$key_time_additional(ns, response_details, rdfs))
  }) |>
    bindEvent(
      input$to_key_time_additional,
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
}

log_trace("Loading modal_handlers.R complete", namespace = "modal_handlers")
