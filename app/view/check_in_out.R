box::use(
  shiny[...],
  dplyr[filter, ...],
  DBI[...],
  lubridate[...],
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
    selectInput(ns('name'), "Name", app_data$Firefighter$firefighter_full_name, selected = NULL),
    actionButton(ns('check_in_out'), "Check In/Check Out")
  )
}

#' @export
Output <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("current_status")),
    actionButton(ns('refresh'), 'Refresh')
  )
}

#' @export
Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      rvs <- reactiveValues()
      
      atten <- reactiveVal(app_data$Attendance)
      
      observeEvent(input$check_in_out, {
        # browser()
        
        # Catch all time errors for checking in and display modals.
        # If no errors found, returns true and check in process begins.
        # Take current time and convert to UTC.
        if(functions$VerifyTrainingTime(sysTime = Sys.time()))
        {
          
          # First thing, update attendance so latest data is being worked with.
          functions$UpdateAttendance(atten)
          
          # Get ff id and training id. Store in rvs to access in other parts of server.
          
          rvs$target_ff_id <- app_data$Firefighter |>
            dplyr::filter(firefighter_full_name == input$name) |>
            pull(firefighter_id)
          
          rvs$target_ff_full_name <- input$name
          
          rvs$target_training_id <- app_data$Training |>
            dplyr::filter(as.Date(Sys.time()) == as.Date(training_start_time)) |>
            pull(training_id)
          
          # Filter to target firefighter's current status.
          Firefighter_Attendance <- atten() |> 
            dplyr::filter(firefighter_id == rvs$target_ff_id) |> 
            dplyr::filter(training_id == rvs$target_training_id)
          
          rvs$attendance_id <- Firefighter_Attendance$attendance_id
          
          # https://stackoverflow.com/questions/48127459/using-modal-window-in-shiny-module
          ns <- session$ns
          
          # Check series of conditions and call appropriate modals.
          # TODO Rework logic to support mutliple training sessions in a day.
          
          # No check ins today.
          if(nrow(Firefighter_Attendance) == 0) {
            showModal(
              modalDialog(
                title = "Confirm Check In",
                paste0(rvs$target_ff_full_name, " is not checked in. Would you like to confirm check in?"),
                footer = tagList(
                  actionButton(ns("confirm_check_in"), "Confirm Check In"),
                  actionButton(ns("cancel_check_in"), "Cancel")
                )
              )
            )
            
            # They've checked in and out already.
          } else if (nrow(Firefighter_Attendance == 1) & !is.na(Firefighter_Attendance$check_out)) {
            showModal(
              modals$warningModal(
                paste0(rvs$target_ff_full_name, " is already checked out from today's training. No changes will be made.")
              )
            )
            # They've checked in but haven't checked out.
          } else if (nrow(Firefighter_Attendance == 1) & is.na(Firefighter_Attendance$check_out)) {
            # browser()
            showModal(
              modalDialog(
                title = "Confirm Check Out",
                paste0(rvs$target_ff_full_name, " is checked in. Would you like to confirm check out?"),
                footer = tagList(
                  actionButton(ns("confirm_check_out"), "Confirm Check Out"),
                  actionButton(ns("cancel_check_out"), "Cancel")
                )
              )
            )
          } else {
            showModal(modals$errorModal("While attempting to check in/out, no conditions met. No action taken. Please contact Joseph Richey."))
          }
        }
      })
      
      observeEvent(input$cancel_check_in, {
        removeModal()
        showModal(
          modals$cancelModal()
        )
      })
      
      observeEvent(input$cancel_check_out, {
        removeModal()
        showModal(
          modals$cancelModal()
        )
      })
      
      observeEvent(input$confirm_check_in, {
        # browser()
        removeModal()
        sql_command <- paste0(
          "INSERT INTO ", Sys.getenv("ATTENDANCE_TABLE"), "(firefighter_id, training_id, check_in, check_out) VALUES (",
           rvs$target_ff_id, ", ",
           rvs$target_training_id, ", '",
           as.POSIXct(Sys.time()), "', ",
           "NULL)")
        
        write_result <- DBI::dbExecute(app_data$CON,
                       sql_command)
        if(write_result == 1) {
          showModal(
            modalDialog(
              title = "Success",
              paste0(rvs$target_ff_full_name, " has successfully checked in. You may now close this window."),
              easyClose = TRUE
            )
          )
        } else {
          showModal(
            modals$errorModal(paste("Check in write failed with write result equal to", write_result))
          )
        }
        
        # Update attendance so latest data is displayed in current status table
        functions$UpdateAttendance(atten)
      })
      
      observeEvent(input$confirm_check_out, {
        removeModal()
        
        sql_command <- paste0(
          "UPDATE ", Sys.getenv("ATTENDANCE_TABLE"), " SET check_out = ",
          "'", as.POSIXct(Sys.time()), "'",
          "WHERE attendance_id = ", rvs$attendance_id)
        
        write_result <- DBI::dbExecute(app_data$CON,
                                       sql_command)
        
        if(write_result == 1) {
          showModal(
            modalDialog(
              title = "Success",
              paste0(rvs$target_ff_full_name, " has successfully checked out. You may now close this window."),
              easyClose = TRUE
            )
          )
        } else {
          showModal(
            modals$errorModal(paste("Check out write failed with write result equal to", write_result))
          )
        }
        
        # Update attendance so latest data is displayed in current status table
        functions$UpdateAttendance(atten)
        
      })
      
      
      output$current_status <- DT::renderDataTable({
        DT::datatable(functions$CurrentStatusTable(atten(),
                                                   app_data$Firefighter),
                      options = list(scrollX=TRUE,
                                     scrollY='800px',
                                     fillContainer = TRUE,
                                     row.names = FALSE))
      })
      
      observeEvent(input$refresh, {
        # browser()
        print("Refresh")
        functions$UpdateAttendance(atten)
      })
      
    }
  )
}
