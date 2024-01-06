box::use(
  shiny[...],
  dplyr[filter, ...],
)

box::use(
  ../logic/app_data,
  ../logic/functions,
  ../modals/modals,
)

#' @export
UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns('name'), "Name", app_data$Roster$firefighter_full_name, selected = NULL),
    actionButton(ns('check_in_out'), "Check In/Check Out")
  )
}

#' @export
Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      rv <- reactiveValues()
      
      observeEvent(input$check_in_out, {
        # browser()
        rv$All <- app_data$Attendance
        
        # Catch all time errors for checking in and display modals.
        # If no errors found, returns true and check in process begins.
        if(functions$VerifyTrainingTime(sysTime = Sys.time()))
        {
          
          # Get ff id and traing id.
          
          rv$target_ff_id <- app_data$Roster |>
            filter(firefighter_full_name == input$name) |>
            dplyr::select(firefighter_id) |>
            unlist() |>
            unname()
          
          rv$target_ff_full_name <- app_data$Roster[app_data$Roster$firefighter_id == rv$target_ff_id,]$firefighter_full_name
          
          rv$target_training_id <- app_data$Training |>
            filter(training_date == Sys.Date()) |>
            dplyr::select(training_id) |>
            unlist() |>
            unname()
          
          
          Firefighter_Attendance <- app_data$Attendance |> 
            dplyr::filter(firefighter_id == rv$target_ff_id) |> 
            dplyr::filter(training_id == rv$target_training_id)
          
          rv$attendance_id <- Firefighter_Attendance$attendance_id
          
          # https://stackoverflow.com/questions/48127459/using-modal-window-in-shiny-module
          ns <- session$ns
          
          if(nrow(Firefighter_Attendance) == 0) {
            showModal(
              modalDialog(
                title = "Confirm Check In",
                paste0(rv$target_ff_full_name, " is not checked in. Would you like to confirm check in?"),
                footer = tagList(
                  actionButton(ns("confirm_check_in"), "Confirm Check In"),
                  actionButton(ns("cancel_check_in"), "Cancel")
                )
              )
            )
          } else if (nrow(Firefighter_Attendance == 1) & !is.na(Firefighter_Attendance$check_out)) {
            showModal(
              modals$warningModal(
                paste0(rv$target_ff_full_name, " is already checked out. No changes will be made.")
              )
            )
          } else if (nrow(Firefighter_Attendance == 1) & is.na(Firefighter_Attendance$check_out)) {
            # browser()
            showModal(
              modalDialog(
                title = "Confirm Check Out",
                paste0(rv$target_ff_full_name, " is checked in. Would you like to confirm check out?"),
                footer = tagList(
                  actionButton(ns("confirm_check_out"), "Confirm Check Out"),
                  actionButton(ns("cancel_check_out"), "Cancel")
                )
              )
            )
          } else {
            showModal(modals$errorModal("While attempting to check in/out, no conditions met. No action taken."))
          }
        }
      })
      
      observeEvent(input$cancel_check_in, {
        removeModal()
        showModal(
          modalDialog(
            title = "Action canceled.",
            "No action is taken. Please close this message to return to the app.",
            easyClose = TRUE
          )
        )
      })
      
      observeEvent(input$cancel_check_out, {
        removeModal()
        showModal(
          modalDialog(
            title = "Action canceled.",
            "No action is taken. Please close this message to return to the app.",
            easyClose = TRUE
          )
        )
      })
      
      observeEvent(input$confirm_check_in, {
        # browser()
        removeModal()
        
        Temp <- data.frame(attendance_id = nrow(app_data$Attendance) + 1,
                           firefighter_id = rv$target_ff_id,
                           training_id = rv$target_training_id,
                           check_in = as.POSIXct(Sys.time()))
        
        Write_Df <- dplyr::bind_rows(app_data$Attendance, Temp)
        rv$All <- Write_Df
        
        DBI::dbWriteTable(conn = app_data$CON,
                          name = "attendance",
                          value = Write_Df,
                          row.names = FALSE,
                          overwrite = TRUE)
        
        showModal(
          modalDialog(
            title = "Success",
            paste0(rv$target_ff_full_name, " has successfully checked in. You may now close this window."),
            easyClose = TRUE
          )
        )
      })
      
      observeEvent(input$confirm_check_out, {
        removeModal()
        # browser()
        Write_Df <- app_data$Attendance |> 
          dplyr::mutate(check_out = if_else(attendance_id == rv$attendance_id, as.POSIXct(Sys.time()), check_out))
        rv$All <- Write_Df
        
        DBI::dbWriteTable(app_data$CON,
                          "attendance",
                          value = Write_Df,
                          row.names = FALSE,
                          overwrite = TRUE)
        
        showModal(
          modalDialog(
            title = "Success",
            paste0(rv$target_ff_full_name, " has successfully checked out. You may now close this window."),
            easyClose = TRUE
          )
        )
      })
      
    }
  )
}
