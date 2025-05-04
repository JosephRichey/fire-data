box::use(
  shiny[...],
  dplyr[...],
)

#' @export
resetCachedValues <- function(incident_list, response_list,
                              edit_state, additional) {
  for (i in names(incident_list)) { 
    cat(paste('resetting', i, "\n"), file = stderr())
    incident_list[[i]] <- NULL 
  }
  
  for (i in names(response_list)) { 
    cat(paste('resetting', i, "\n"), file = stderr())
    response_list[[i]] <- NULL 
  }
  
  edit_state(FALSE)
  additional(FALSE)
}
