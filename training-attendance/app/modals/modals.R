box::use(
  shiny[...],
)

#' @export
noTrainingModal <- function() {
  modalDialog(
    title = "No Training Found",
    "There is no training scheduled currently. If you believe there should be,
    please reach out to the officer in charge of todays training."
  )
}

#' @export
errorModal <- function(message) {
  modalDialog(
    title = "Application Error",
    "There has been an error. Please contact the application administrator. Please include a screenshot or the error message below.",
    message
  )
}

#' @export
warningModal <- function(message) {
  modalDialog(
    title = "Warning",
    message
  )
}

#' @export
cancelModal <- function() {
  modalDialog(
    title = "Action canceled.",
    "No action is taken. Please close this message to return to the app.",
    easyClose = TRUE
  )
}
