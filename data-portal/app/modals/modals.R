box::use(
  shiny[...],
  shinyTime[...],
  bslib[...],
  shinyalert[...],
)

box::use(
  ../logic/app_data,
)

#FIXME Going to replace error and warning modals with shinyalerts.
#' @export
errorModal <- function(message) {
  modalDialog(
    title = tagList(
      bsicons::bs_icon("exclamation-triangle", fill = "red"),
      " Application Error"
    ),
    "There has been an error. ",
    message,
    footer = tagList(
      modalButton("Close")
    )
  )
}

#' @export
WarningAlert <- function(message) {
  shinyalert(
    title = "Warning",
    type = "warning",
    text = message,
    closeOnClickOutside = TRUE,
    confirmButtonText = "OK"
  )
}

#' @export
ErrorAlert <- function(message) {
  shinyalert(
    title = "Error",
    type = "error",
    text = message,
    closeOnClickOutside = TRUE,
    confirmButtonText = "OK"
  )
}

#' @export
SuccessAlert <- function(message) {
  shinyalert(
    title = "Success!",
    type = "success",
    text = message,
    closeOnClickOutside = TRUE,
    confirmButtonText = "OK"
  )
}



#' @export
warningModal <- function(message) {
  modalDialog(
    title = "Warning",
    message,
    easyClose = TRUE,
    footer = tagList(
      modalButton("Close")
    )
  )
}

#' @export
successModal <- function(message) {
  modalDialog(
            title = "Success!",
            message,
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close")
            )
            )
}

#' @export
trainingModal <- function(ns,
                          edit = FALSE,
                          training_date,
                          start_time,
                          end_time,
                          category_choices,
                          category,
                          topic,
                          training_description,
                          trainers,
                          trainer) {

  modalDialog(
    title = if(edit) "Edit Training" else "Add Training",
    dateInput(ns('training_date'), 'Training Date', value = training_date),
    timeInput(ns('start_time'), "Start Time", value = start_time, minute.steps = 5),
    timeInput(ns('end_time'), "End Time", value = end_time, minute.steps = 5),
    numericInput(ns('credit_hours'), 'Training Hours', value = 0, min = 0, step = 0.25),
    selectInput(ns('training_category'), 'Training Category', choices = category_choices, selected = category),
    selectInput(ns('training_topic'), 'Training Topic', choices = c(), selected = topic),
    textAreaInput(ns('training_description'), 'Training Description', value = training_description),
    selectInput(ns('trainer'), 'Training Led By', choices = trainers, selected = trainer),
    footer = tagList(
      modalButton("Cancel"),
      if(edit) {
        actionButton(
          ns("submit_edit_training"),
          "Submit Edit",
          class = "btn-primary"
          )
      } else {
        actionButton(
          ns("submit_add_training"),
          "Add Training",
          class = "btn-primary")
      }
    ),
    easyClose = TRUE
  )
}

#' @export
attendanceModal <- function(ns,
                            firefighter_choices,
                            check_in,
                            check_out) {

  modalDialog(
    title = "Add Attendance",
    selectInput(ns('add_attendance_firefighters'),
                'Select Attendee',
                choices = firefighter_choices,
                multiple = TRUE),
    timeInput(ns('add_attendance_check_in'),
              "Check In Time",
              value = check_in,
              minute.steps = 1),
    timeInput(ns('add_attendance_check_out'),
              "Check Out Time",
              value = check_out,
              minute.steps = 1),
    radioButtons(ns('add_attendance_attendance_credit'),
                 'Attendance Status',
                 choices = c('Credit', 'No Credit'),
                 selected = 'Credit'),



    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("submit_add_attendance"), "Submit Attendance",
                   class = "btn-primary")
    ),
    easyClose = TRUE
  )
}

#' @export
deleteAttendanceModal <- function(ns,
                                  firefighter_choices
                                  ) {
  modalDialog(
    title = "Delete Attendance",
    selectInput(ns('delete_attendance_firefighters'),
                'Select Attendee',
                choices = firefighter_choices,
                multiple = TRUE),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("submit_delete_attendance"), "Delete Attendance",
                   class = "btn-warning")
    ),
    easyClose = TRUE
  )
}

#' @export
excuseAttendanceModal <- function(ns,
                                  firefighter_choices
) {
  modalDialog(
    title = "Excuse Attendance",
    selectInput(ns('excuse_attendance_firefighters'),
                'Select Attendee',
                choices = firefighter_choices,
                multiple = TRUE),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("submit_excuse_attendance"), "Excuse Attendance",
                   class = "btn-primary")
    ),
    easyClose = TRUE
  )
}

#' @export
addFirefighterModal <- function(ns) {
  modalDialog(
    size = 'l',
    title = "Add Firefighter",
    helpText('Enter the first and last name of the firefighter you would like to add. This is how it will appear in all reports and apps.'),
    layout_column_wrap(
      width = 1/2,
      textInput(
        ns('add_full_name'),
        'Full Name',
        placeholder = 'John Smith'
      ),
      dateInput(
        ns('add_start_date'),
        'Start Date',
        value = app_data$Local_Date
      )
    ),
    hr(),
    layout_column_wrap(
      width = 1/2,
      selectInput(
        ns('add_reports_to'),
        'Reports To',
        #FIXME
        choices = app_data$Firefighter |> dplyr::filter(active_status == 1) |> dplyr::pull(full_name),
      ),
      selectInput(
        ns('add_company'),
        'Company',
        choices = app_data$Company$company_name,
        selected = app_data$Company$company_name[1]
      ),
      selectInput(
        ns('add_role'),
        'Role',
        #FIXME Set this with .csv settings
        choices = c('Chief', 'Captain', 'Lieutenant', 'Firefighter', 'Probationary')
      )
    ),
    layout_column_wrap(
      width = 1/2,
      checkboxInput(
        ns('add_trainer'),
        'Trainer',
        value = FALSE
      ),
      checkboxInput(
        ns('add_officer'),
        'Officer',
        value = FALSE
      )
    ),
    hr(),
    layout_column_wrap(
      width = 1/2,
      textInput(
        ns('add_street_1'),
        'Street Address 1',
        placeholder = '123 Main St.'
      ),
      textInput(
        ns('add_street_2'),
        'Street Address 2'
      ),
      textInput(
        ns('add_city'),
        'City',
        placeholder = 'Anytown'
      ),
      selectInput(
        ns('add_state'),
        'State',
        choices = datasets::state.abb,
        selected = 'UT' #FIXME CSV Settings
      ),
      textInput(
        ns('add_zip'),
        'Zip Code',
        placeholder = '12345'
      ),
      textInput(
        ns('add_phone'),
        'Phone Number',
        placeholder = '123-456-7890'
      )
    ),
    textInput(
      ns('add_email'),
      'Email Address',
      width = '100%',
      placeholder = 'john.smith@gmail.com'
    ),

    footer = tagList(
      modalButton('Cancel'),
      actionButton(ns("action_add_firefigher"), "Submit",
                   class = 'btn-primary')
    ),
    easyClose = TRUE
  )
}
