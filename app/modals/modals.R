box::use(
  shiny[...],
)

#' @export
noTrainingModal <- function() {
  modalDialog(
    title = "No Training Found",
    "There is no training scheduled currently. If you believe there should be,
    please reach out to the officer in charge of todays training or to Joseph Richey."
  )
}

#' @export
errorModal <- function(message) {
  modalDialog(
    title = "Application Error",
    "There has been an error. Please call or text Joseph Richey at 801-644-6893. Please include a screenshot or the error message below.",
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