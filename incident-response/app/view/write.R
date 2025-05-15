box::use(
  shiny[...],
  dplyr[...],
  logger[...],
  tibble[...],
  sortable[...],
  glue[...],
  DBI[...],
)

log_trace("Loading write.R", namespace = "write")

box::use(
  ../logic/app_data,
  ../logic/global_functions[HipaaLog,
                     CheckWriteResult,
                     StringToId],
)


##### Write Helper Functions ####
hWriteIncidentUnit <- function(inc_vals, write_results, incident_id) {

  if(is.null(inc_vals$units)) {
    log_trace("No units to add to incident_unit",
              namespace = "WriteIncident")
  } else {
    for(i in inc_vals$units) {

      result <- hSafedbExecute(
        glue("INSERT INTO incident_unit
                      (incident_id, unit_type_id) VALUES
                      ({incident_id}, {i})"),
        namespace = "hWriteIncidentUnit"
      )
      
      log_info(glue("INSERT INTO incident_unit
                      (incident_id, unit_type_id) VALUES
                      ({incident_id}, {i})"), 'SQL Statement')
      
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
  
  return(write_results)
}

hSafedbExecute <- function(sql_statement, namespace) {
  # Execute the safely interpolated SQL command
  result <- tryCatch(
    {
      dbExecute(app_data$CON, sql_statement)
    },
    error = function(e) {
      log_error(glue::glue("Database write failed: {e$message}"), namespace = "hSafedbExecute")
      NA
    }
  )
  
  return(result)
}

hWriteFirefighterResponse <- function(resp_vals, input, remove_old, write_results, session) {
  firefighter_response_statement <- paste0("INSERT INTO firefighter_response
                                                   (response_id, firefighter_id, time_adjustment) VALUES "
  )
  
  # browser()
  
  if(remove_old) {
    dbExecute(
      app_data$CON,
      glue("DELETE FROM firefighter_response WHERE response_id = {input$edit_response}")
    )
    
    HipaaLog(
      glue("DELETE FROM firefighter_response WHERE response_id = {input$edit_response}"),
      session
    )
  }
  
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
    
    # Execute the statements
    log_info(
      firefighter_response_statement |> as.character(),
      'SQL Statement'
    )
    result <- hSafedbExecute(firefighter_response_statement, 'Write Firefighter Response')
    HipaaLog(
      firefighter_response_statement |> as.character(),
      session
    )
    write_results <- c(write_results,
                       CheckWriteResult(
                         result,
                         expectedMin = 1,
                         expectedMax = length(resp_vals$firefighter),
                         context = 'adding a firefighter response',
                         showMessage = FALSE
                       )
    )
    
  }
  
  
  return(write_results)
  
}

hWriteApparatusResponse <- function(resp_vals, input, remove_old, write_results, session) {
  apparatus_response_statement <- paste0("INSERT INTO apparatus_response
                                                   (response_id, apparatus_id) VALUES "
  )

  if(remove_old) {
    dbExecute(
      app_data$CON,
      glue("DELETE FROM apparatus_response WHERE response_id = {input$edit_response}")
    )
    
    HipaaLog(
      glue("DELETE FROM apparatus_response WHERE response_id = {input$edit_response}"),
      session
    )
  }

  # loop and append each tuple
  if(!is.null(resp_vals$apparatus)) {
    for (app in resp_vals$apparatus) {
      app_id <- StringToId(app_data$Apparatus, apparatus_name, app)
      
      apparatus_response_statement <- paste0(
        apparatus_response_statement,
        "('", resp_vals$response_id, "',",
        app_id, "),"
      )
    }
    
    # Replace the last comma with a semi-colon
    apparatus_response_statement <- sub(",([^,]*)$", ";\\1", apparatus_response_statement)
    
    # Execute the statements
    log_info(
      apparatus_response_statement |> as.character(),
      'SQL Statement'
    )
    result <- hSafedbExecute(apparatus_response_statement, 'Write Apparatus Response')
    HipaaLog(
      apparatus_response_statement |> as.character(),
      session
    )
    write_results <- c(write_results,
                       CheckWriteResult(
                         result,
                         expectedMin = 1,
                         expectedMax = length(resp_vals$apparatus),
                         context = 'adding an apparatus response',
                         showMessage = FALSE
                       )
    )
  }
  
  return(write_results)
}


hWriteFirefighterApparatus <- function(resp_vals, input, remove_old, write_results, session, assignments) {
  firefighter_apparatus_statement <- paste0("INSERT INTO firefighter_apparatus
                                                   (response_id, firefighter_id, apparatus_id) VALUES "
  )

  if(remove_old) {
    dbExecute(
      app_data$CON,
      glue("DELETE FROM firefighter_apparatus WHERE response_id = {input$edit_response}")
    )
    
    HipaaLog(
      glue("DELETE FROM firefighter_apparatus WHERE response_id = {input$edit_response}"),
      session
    )
  }
  
  if(!is.null(resp_vals$firefighter)) {
    # loop and append each tuple
    for (app in c('Standby', resp_vals$apparatus)) {
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
    log_info(
      firefighter_apparatus_statement |> as.character(),
      'SQL Statement'
    )
    result <- hSafedbExecute(firefighter_apparatus_statement, 'Write Firefighter Apparatus')
    HipaaLog(
      firefighter_apparatus_statement |> as.character(),
      session
    )
    write_results <- c(write_results,
                       CheckWriteResult(
                         result,
                         expectedMin = 1,
                         expectedMax = length(resp_vals$firefighter),
                         context = 'adding a firefighter apparatus',
                         showMessage = FALSE
                       )
    )
  }
  
  return(write_results)
}


##### Write Functions #####
#' @export
WriteIncident <- function(session, write_results, inc_vals, inc_start_time, inc_end_time) {
  log_trace(
    glue('Preparing statement for adding new incident'),
    namespace = 'WriteIncident'
  )
  
  incident_sql_statement <- "INSERT INTO ?incident_table
          (cad_identifier, incident_start, incident_end,
          address, dispatch_id, area_id,
          canceled, dropped, is_reviewed,
          is_locked, is_deleted, deleted_by) VALUES
          (?cad_identifier, ?incident_start, ?incident_end,
          ?address, ?dispatch_id, ?area_id,
          ?canceled, ?dropped, 0, 0, NULL, NULL)"
  # browser()
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
  
  # Execute and write to Hipaa log
  log_info(incident_safe_sql |> as.character(), 'SQL Statement')
  result <- hSafedbExecute(incident_safe_sql, "WriteIncident")
  
  log_trace("Identifying id for incident we just added",
            namespace = "WriteIncident")

  incident_id <- dbGetQuery(app_data$CON, "SELECT LAST_INSERT_ID()") |>
    pull(`LAST_INSERT_ID()`) |> as.numeric()
  
  HipaaLog(
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
    namespace = "WriteIncident"
  )
  
  #### Write to incident_unit ####
  write_results <- hWriteIncidentUnit(inc_vals, write_results, incident_id)
  
  return(list(write_results, incident_id))
}


#' @export
EditIncident <- function(session, write_results, inc_vals, inc_start_time, inc_end_time, input) {

  log_trace(
    glue('Preparing statement for editing incident'),
    namespace = 'EditIncident'
  )
  # browser()
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
  
  # Execute and write to Hipaa log
  log_info(incident_safe_sql |> as.character(), 'SQL Statement')
  result <- hSafedbExecute(incident_safe_sql, "EditIncident")
  HipaaLog(
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
    namespace = "EditIncident"
  )
  
  # Write to incident_unit
  # Delete all incident units for this incident
  dbExecute(
    app_data$CON,
    glue("DELETE FROM incident_unit WHERE incident_id = {input$edit_incident}")
  )
  
  # Add new incident units
  write_results <- hWriteIncidentUnit(inc_vals, write_results, input$edit_incident)
  
  return(write_results)
  
}

#' @export
AddResponse <- function(session, 
                        resp_start_time, resp_end_time,
                        write_results, inc_vals, resp_vals, input, assignments) {

  log_trace(
    glue('Preparing statement for adding new response'),
    namespace = 'AddResponse'
  )

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
    response_start = resp_start_time,
    response_end = resp_end_time,
    notes = if (is.null(resp_vals$response_notes)) NA else resp_vals$response_notes
  )
  
  
  # Execute the safely interpolated SQL command
  log_info(response_safe_sql |> as.character(), 'SQL Statement')
  result <- hSafedbExecute(response_safe_sql, "AddResponse")
  
  # Grab the response id
  resp_vals$response_id <- dbGetQuery(app_data$CON, "SELECT LAST_INSERT_ID()") |>
    pull(`LAST_INSERT_ID()`) |> as.numeric()
  
  HipaaLog(
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
  
  ###### Firefighter response  ######
  write_results <- hWriteFirefighterResponse(
    resp_vals,
    input,
    remove_old = FALSE,
    write_results,
    session
  )
  

  ###### Apparatus response ######
  write_results <- hWriteApparatusResponse(
    resp_vals,
    input,
    remove_old = FALSE,
    write_results,
    session
  )
    
  ###### Firefighter Apparatus ######
  write_results <- hWriteFirefighterApparatus(
    resp_vals,
    input,
    remove_old = FALSE,
    write_results,
    session, 
    assignments
  )

  
  return(write_results)
}

#' @export
EditResponse <- function(session, 
                         resp_start_time, resp_end_time,
                         write_results, resp_vals, input, assignments) {
  log_trace(
    glue('Preparing statement for editing response'),
    namespace = 'EditResponse'
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
    notes = if (is.null(resp_vals$response_notes)) NA else resp_vals$response_notes,
    id = input$edit_response
  )
  
  # Execute and write to Hipaa log
  log_info(response_safe_sql |> as.character(), 'SQL Statement')
  result <- hSafedbExecute(response_safe_sql, "EditResponse")
  HipaaLog(
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
  
  # Update firefighter response
  write_results <- hWriteFirefighterResponse(
    resp_vals,
    input,
    remove_old = TRUE,
    write_results,
    session
  )

  # Update apparatus response
  write_results <- hWriteApparatusResponse(
    resp_vals,
    input,
    remove_old = TRUE,
    write_results,
    session
  )
  
  # Update firefighter apparatus
  write_results <- hWriteFirefighterApparatus(
    resp_vals,
    input,
    remove_old = TRUE,
    write_results,
    session, 
    assignments
  )
  
  return(write_results)
}

log_trace("Loaded write.R", namespace = "write")