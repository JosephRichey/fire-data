box::use(
  shiny[...],
  shinyTime[...],
)

#' @export
errorModal <- function(message) {
  modalDialog(
    title = "Application Error",
    "There has been an error. ",
    message
  )
}

#' @export
warningModal <- function(message) {
  modalDialog(
    title = "Warning",
    message,
    easyClose = TRUE
  )
}

#' @export
successModal <- function(message) {
  modalDialog(
            title = "Success!",
            message,
            easyClose = TRUE
            )
}

#' @export
trainingModal <- function(ns,
                          edit = FALSE,
                          training_date,
                          start_time,
                          end_time,
                          type_choices,
                          type,
                          topic,
                          training_description,
                          training_trainers,
                          trainer) {

  # browser()
  modalDialog(
    title = if(edit) "Edit Training" else "Add Training",
    dateInput(ns('training_date'), 'Training Date', value = training_date),
    # FIXME Default training time set by settings
    timeInput(ns('start_time'), "Start Time", value = start_time, minute.steps = 5),
    timeInput(ns('end_time'), "End Time", value = end_time, minute.steps = 5),
    # FIXME Default training types and topics set by settings
    selectInput(ns('training_type'), 'Training Type', choices = type_choices, selected = type),
    selectInput(ns('topic'), 'Training Topic', choices = c(), selected = topic),
    textAreaInput(ns('training_description'), 'Training Description', value = training_description),
    selectInput(ns('trainer'), 'Training Led By', choices = training_trainers, selected = trainer),
    footer = tagList(
      modalButton("Cancel"),
      if(edit) {
        actionButton(ns("submit_edit_training"), "Submit Edit")
      } else {
        actionButton(ns("submit_add_training"), "Add Training")
      }
    ),
    easyClose = TRUE
  )
}
