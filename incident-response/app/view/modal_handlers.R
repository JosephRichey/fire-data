box::use(
  shiny[...],
  shinyalert[shinyalert],
  glue[glue],
  logger[...],
  dplyr[...],
  ../logic/global_functions[...],
  ../modal/modal
)

#' @export
observeAddIncident <- function(input, ns, incident_details, edit) {
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
observeToAddressUnit <- function(input, ns, rdfs, incident_details, edit) {
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
    removeModal()
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
observeToApparatusFf <- function(input, ns, response_details, edit, additional) {
  observe({

    log_trace(
      glue('Showing firefighter apparatus modal in {if_else(edit(), "edit", "add")} {if_else(additional(), "response", "incident")} mode'),
      namespace = 'observe input$to_apparatus_ff'
    )
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
}

#' @export
observeToAssignment <- function(input, ns, response_details, edit, additional) {
  observe({

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
                             length(input$apparatus)))
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
                             length(input$apparatus)))
      return()
    }
    
    removeModal()
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
observeToNote <- function(input, ns, response_details, edit, additional) {
  observe({
    # Check if stanby is allowed and conditions met
    if(!GetSetting(domain = 'incident', 
                   key = 'standby_allowed')) {
      log_warning(
        "standby is not an allowed status. Checking if there are any firefighters in standby.",
        namespace = 'observe input$to_note'
      )
      
      if(length(response_details$ff_app_lists$Standby) > 0) {
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
                           length(input$apparatus)))
    
  }) |>
    bindEvent(
      input$to_note,
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
}
