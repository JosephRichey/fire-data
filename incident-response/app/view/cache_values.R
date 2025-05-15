box::use(
  shiny[...],
  purrr[...],
  logger[...],
  glue[...],
  dplyr[...],
  stringr[...],
)

log_trace("Loading cache_values.R", namespace = "cache_values")

box::use(
  app/logic/app_data,
  app/modal/modal,
  app/logic/local_functions,
  app/logic/global_functions[GetSetting,
                             BuildDateTime,
                             CheckWriteResult,
                             StringToId,
                             IdToString,
                             UpdateReactives],
  app/logic/logging,
  ./modal_handlers[...],
)

CacheToList <- function(input, incident_details, response_details) {

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
    input[[.x]],
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  ))
  
  walk(response_fields, ~ bindEvent(
    observe({
      response_details[[.x]] <- input[[.x]]
      log_trace(
        glue('Caching {paste(input[[.x]], collapse = ", ")} to {.x}'),
        namespace = 'cache response details'
      )
    }),
    input[[.x]],
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  ))

}

ObserveAssignmentList <- function(input, response_details) {
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
  
}

ObserveCancelModel <- function(input, incident_details, response_details, edit, additional, session) {
  observeEvent(input$cancel_modal, {
    removeModal()
    
    log_trace(
      glue('Resetting values'),
      namespace = 'observe input$cancel_modal'
    )
    local_functions$resetCachedValues(incident_details, response_details,
                                      edit, additional)
    session$sendCustomMessage(type = "jsCode", list(code = "window.enableScroll();"))
    
    
  })
}

log_trace("Loading cache_values.R complete", namespace = "cache_values")