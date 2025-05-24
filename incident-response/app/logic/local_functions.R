box::use(
  shiny[...],
  dplyr[...],
  logger[...],
  tibble[...],
  sortable[...],
)

log_trace("Loading local_functions.R", namespace = "local_functions")

box::use(
  ./app_data,
  ../modal/modal,
)

#' @export
resetCachedValues <- function(incident_list, response_list,
                              edit_state, additional, session, input) {

  for (i in names(incident_list)) { 
    log_trace(paste('resetting', i, "\n"), namespace = "resetCachedValues")
    incident_list[[i]] <- NULL 
  }
  
  for (i in names(response_list)) { 
    log_trace(paste('resetting', i, "\n"), namespace = "resetCachedValues")
    response_list[[i]] <- NULL 
  }
  
  edit_state(FALSE)
  additional(FALSE)
  
  session$sendCustomMessage(type = "jsCode", list(code = "window.enableScroll();"))
  
  
  log_info("Reset cached values", 
           namespace = "resetCachedValues")
}

#' @export
generate_firefighter_apparatus <- function(input, ns, response_details,
                                           rdfs) {
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

#' @export
CoalesceReactiveWithInput <- function(reactiveList, input, keys) {
  result <- list()
  for (key in keys) {
    result[[key]] <- if (!is.null(reactiveList[[key]])) {
      reactiveList[[key]]
    } else {
      input[[key]]
    }
  }
  result
}

log_trace("Loading local_functions.R complete", namespace = "local_functions")

