box::use(
  shiny[...],
  dplyr[...],
  logger[...],
  glue[...],
)

log_trace("Loading edit.R", namespace = "edit")

box::use(
  app/logic/app_data,
  app/modal/modal,
  app/logic/global_functions[GetSetting],
)

#' @export
ObserveEditIncident <- function(input, ns, incident_details, rdfs, edit) {
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
}

#' @export
ObserveEditResponse <- function(input, ns, response_details, rdfs, edit) {
  observe({
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
    
    response_details$incident_id <- rdfs$response |> 
      filter(id == input$edit_response) |> 
      pull(incident_id)
    
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
    bindEvent(input$edit_response,
              ignoreNULL = TRUE, 
              ignoreInit = TRUE)
}

#' @export
ObserveAddResponse <- function(input, ns, response_details, rdfs, additional) {
  observe({
    additional(TRUE)
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
}

log_trace("Loading edit.R complete", namespace = "edit")
