box::use(
  shiny[...],
  dplyr[...],
  logger[...],
)

#' @export
resetCachedValues <- function(incident_list, response_list,
                              edit_state, additional) {
  for (i in names(incident_list)) { 
    log_info(paste('resetting', i, "\n"), namespace = "resetCachedValues")
    incident_list[[i]] <- NULL 
  }
  
  for (i in names(response_list)) { 
    log_info(paste('resetting', i, "\n"), namespace = "resetCachedValues")
    response_list[[i]] <- NULL 
  }
  
  edit_state(FALSE)
  additional(FALSE)
}
