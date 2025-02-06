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
    selectInput(ns('name'), "Name", app_data$Firefighter$full_name, selected = NULL),
    actionButton(ns('check_in_out'), "Check In/Check Out", class = 'btn-primary'),
    actionButton(ns('refresh_trainings'), "Refresh Trainings", class = 'btn-light')
  )
}

#' @export
Output <- function(id) {
  ns <- NS(id)
  
  tagList(
    DT::dataTableOutput(ns("current_status")),
    actionButton(ns('refresh_attendance'), 'Refresh Attendance Table', class = 'btn-light')
  )
}

#' @export
Button <- function(id) {
  ns <- NS(id)
  actionButton(ns('all_checkout'), 'Check Everyone Out', class = 'btn-secondary')
}

#' @export
Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # https://stackoverflow.com/questions/48127459/using-modal-window-in-shiny-module
      ns <- session$ns
      
      rvs <- reactiveValues()
      
      atten <- reactiveVal(app_data$Attendance)
      
      r_training <- reactiveVal(app_data$Training)
      
      observeEvent(input$check_in_out, {
        
        # Use variable to help with debugging.
        sysTime <- Sys.time()
        # sysTime <- as.POSIXct("2024-07-06 02:00:00")
        
        # Check if there is a valid training currently.
        verification <- functions$VerifyTrainingTime(sysTime, r_training())
        
        
        # browser()
        # Handle any errors and display appropriate modal.
        if(typeof(verification) == 'list') {
          if(verification[[2]] == 'warning') {
            showModal(
              modals$warningModal(verification[[1]])
            )
            return()
          } else {
            showModal(
              modals$errorModal(verification[[1]])
            )
            return()
          }
        }
        
        # If no errors, continue with check in/out process.
        if(verification)
        {
          
          # First thing, update attendance so latest data is being worked with.
          functions$UpdateAttendance(atten)
          
          # Get ff id and training id. Store in rvs to access in other parts of server.
          rvs$target_ff_id <- app_data$Firefighter |>
            dplyr::filter(full_name == input$name) |>
            pull(firefighter_id)
          
          rvs$target_ff_full_name <- input$name
          
          #FIXME This shoulnd't be duplicated- use a function.
          rvs$target_training_id <- app_data$Training |>
            dplyr::filter(
              sysTime + 300 > start_time & # Same logic as VerifyTrainingTime
                sysTime - (60 * 60) < end_time # Same logic as VerifyTrainingTime
            ) |>
            pull(training_id)
          
          # Filter to target firefighter's current status.
          Firefighter_Attendance <- atten() |> 
            dplyr::filter(firefighter_id == rvs$target_ff_id) |> 
            dplyr::filter(training_id == rvs$target_training_id)
          
          rvs$attendance_id <- Firefighter_Attendance$attendance_id
          
          
          
          # Check series of conditions and call appropriate modals.
          # browser()
          # No check ins for current training.
          if(nrow(Firefighter_Attendance) == 0) {
            showModal(
              modalDialog(
                title = "Confirm Check In",
                paste0(rvs$target_ff_full_name, " is not checked in. Would you like to confirm check in?"),
                footer = tagList(
                  actionButton(ns("confirm_check_in"), "Confirm Check In", class = "btn-success"),
                  actionButton(ns("cancel"), "Cancel", class = 'btn-light')
                ),
                easyClose = TRUE
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
                  actionButton(ns("confirm_check_out"), "Confirm Check Out", class = "btn-secondary"),
                  actionButton(ns("cancel"), "Cancel", class = 'btn-light')
                ),
                easyClose = TRUE
              )
            )
          } else {
            showModal(modals$errorModal("While attempting to check in/out, no conditions met. 
                                        No action taken. Please contact application administrator."))
          }
        }
      })
      
      observeEvent(input$cancel, {
        removeModal()
        showModal(
          modals$cancelModal()
        )
      })
      
      observeEvent(input$confirm_check_in, {
        browser()
        removeModal()
        
        # No sqlInterpolate needed. All inputs are preselected.
        sql_command <- paste0(
          "INSERT INTO attendance (firefighter_id, training_id, check_in, check_out) VALUES (",
           rvs$target_ff_id, ", ",
           rvs$target_training_id, ", '",
           as.POSIXct(Sys.time()) |> format(tz = "UTC", usetz = FALSE), "', ",
           "NULL)")
        
        write_result <- DBI::dbExecute(app_data$CON,
                       sql_command)
        if(write_result == 1) {
          showModal(
            modalDialog(
              title = "Success",
              paste0(rvs$target_ff_full_name, " has successfully checked in. You may now close this window."),
              easyClose = TRUE,
              class = 'btn-light'
            )
          )
        } else {
          showModal(
            modals$errorModal(paste("Check in write failed with write result equal to", write_result, 
                                    ". Please contact application administrator."))
          )
        }
        
        # Update attendance so latest data is displayed in current status table
        functions$UpdateAttendance(atten)
      })
      
      observeEvent(input$confirm_check_out, {
        removeModal()
        
        # No sqlInterpolate needed. All inputs are preselected.
        sql_command <- paste0(
          "UPDATE attendance SET check_out = ",
          "'", as.POSIXct(Sys.time()) |> format(tz = "UTC", usetz = FALSE), "'",
          "WHERE attendance_id = ", rvs$attendance_id)
        
        write_result <- DBI::dbExecute(app_data$CON,
                                       sql_command)
        
        if(write_result == 1) {
          showModal(
            modalDialog(
              title = "Success",
              paste0(rvs$target_ff_full_name, " has successfully checked out. You may now close this window."),
              easyClose = TRUE,
              class = 'btn-light'
            )
          )
        } else {
          showModal(
            modals$errorModal(paste("Check out write failed with write result equal to", write_result, 
                                    ". Please contact application administrator."))
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
      
      observeEvent(input$all_checkout, {
        showModal(
          modalDialog(
            title = "Confirm Check Out",
            "Please enter an admin password",
            passwordInput(ns("admin_password"), ""),
            footer = tagList(
              actionButton(ns("confirm_all_checkout"), "Confirm Check Out", class = "btn-secondary"),
              actionButton(ns("cancel"), "Cancel", class = 'btn-light')
            ),
            easyClose = TRUE
          )
        )
        
      })
      
      
      observeEvent(input$confirm_all_checkout, {
        removeModal()
        
        # Check if admin password is correct
        if(input$admin_password != '123') {
          showModal(
            modals$warningModal("Admin password incorrect. Please try again.")
          )
          return()
        }
        
        
        # Execute sql statement that checks out everyone currently checked in
        # No sqlInterpolate needed.
        sql_command <- paste0(
          "UPDATE attendance SET check_out = ",
          "'", as.POSIXct(Sys.time()) |> format(tz = "UTC", usetz = FALSE), "'",
          " WHERE check_out IS NULL")
        
        DBI::dbExecute(app_data$CON, sql_command)
        
        functions$UpdateAttendance(atten)
        
        showNotification("All firefighters checked out.", duration = 5)
        
        
      })
      
      
      observeEvent(input$refresh_attendance, {
        # browser()
        print("Refreshed attendance data.")
        functions$UpdateAttendance(atten)
        showNotification("Current status refreshed.", duration = 5)
      })
      
      observeEvent(input$refresh_trainings, {
        # browser()
        print("Refreshed training data.")
        functions$UpdateTrainings(r_training)
        showNotification("Trainings refreshed.", duration = 5)
      })
      
      session$onSessionEnded(function() {
        DBI::dbDisconnect(app_data$CON)
        print('DB disconnected')
      })
      
    }
  )
}
