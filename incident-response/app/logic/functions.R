box::use(
  shiny[...],
  dplyr[...],
)

#' @export
resetCachedValues <- function(reactive_list, edit_state, additional) {
  for (i in names(reactive_list)) { 
    cat(paste('resetting', i, "\n"), file = stderr())
    reactive_list[[i]] <- NULL 
  }
  
  edit_state(FALSE)
  additional(FALSE)
}
