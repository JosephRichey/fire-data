box::use(
  shiny[...],
  bslib[...],
  shinyWidgets[...],
  DT[...],
  dplyr[filter, ...],
  lubridate[...],
  shinyTime[...],
  stats[setNames],
  DBI[...],
  tibble[...],
  hms[...],
  bsicons[...],
)

box::use(
  ../logic/app_data,
  ../logic/functions,
  ../modals/modals,
)

training_trainers <- app_data$Firefighter |>
  filter(firefighter_trainer == TRUE) |>
  select(firefighter_full_name, firefighter_id) %>%
  # https://ivelasq.rbind.io/blog/understanding-the-r-pipe/
  # See "Getting to the solution" sect
  (\(.) setNames(.$firefighter_id, .$firefighter_full_name))




TrainingUI <- function(id) {
  ns <- NS(id)

  tagList(
    actionButton(ns('add_training'), "Add Training"),
    actionButton(ns('modify_training'), "Modify Training"),
    actionButton(ns("delete_training"), "Delete Training"),
    accordion(
      open = FALSE,
      accordion_panel(
        title = "Filter Trainings",
        open = FALSE,
        icon = bsicons::bs_icon("funnel-fill"),
        dateRangeInput(ns('training_filter_range'),
                       "Show trainings between:",
                       start = as.Date(app_data$Local_Date - dyears(1) + ddays(1)),
                       end = app_data$Local_Date
                       ),
        pickerInput(ns('filter_training_type'),
                    'Training Type',
                    choices = unique(app_data$Training$training_type),
                    selected = unique(app_data$Training$training_type),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
        ),

        pickerInput(ns('filter_training_officer'),
                    'Training Officer',
                    choices = training_trainers,
                    selected = training_trainers,
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
        )
      )
    )
  )
}

TrainingOutput <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      fill = FALSE,
      card_body(
        fillable = FALSE,
        DTOutput(ns('view_trainings'))
      )
    )
  )

}

AttendanceUI <- function(id) {
  ns <- NS(id)

  tagList(
    actionButton(ns('add_attendance'), "Add Attendance"),
    actionButton(ns('modify_attendance'), "Modify Attendance"),
    actionButton(ns("delete_attendance"), "Delete Attendance"),
    accordion(
      open = FALSE,
      accordion_panel(
        title = "Filter Attendance",
        open = FALSE,
        icon = bsicons::bs_icon("funnel-fill"),
        dateRangeInput(ns('attendance_filter_range'),
                       "Show trainings between:",
                       start = as.Date(app_data$Local_Date - dyears(1) + ddays(1)),
                       end = app_data$Local_Date
        ),
        pickerInput(ns('filter_attendance_type'),
                    'Training Type',
                    choices = unique(app_data$Training$training_type),
                    selected = unique(app_data$Training$training_type),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
        ),

        pickerInput(ns('filter_attendance_officer'),
                    'Training Officer',
                    choices = training_trainers,
                    selected = training_trainers,
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
        )
      )
    )
  )
}

AttendanceOutput <- function(id) {
  ns <- NS(id)
  tagList(

  )
}




Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      r_Training <- reactiveVal(app_data$Training)

      ns <- session$ns

      updateReactiveValue <- function() {
        r_Training(DBI::dbGetQuery(app_data$CON, paste0("SELECT * FROM training")) |>
             mutate(
               start_time = as.POSIXct(training_start_time, tz = 'UTC') |> with_tz(tzone = app_data$LOCAL_TZ),
               end_time = as.POSIXct(training_end_time, tz = 'UTC') |> with_tz(tzone = app_data$LOCAL_TZ)
             )
        )
      }

      # Display current trainings
      output$view_trainings <- renderDT({
        # browser()
        Table_Data <- r_Training() |>
          mutate(
            date = start_time |> functions$FormatLocalDate(),
            date_to_filter = start_time |> functions$FormatLocalDate(asPosix = TRUE),
            start_time = start_time |> functions$FormatLocalTime(),
            end_time = end_time |> functions$FormatLocalTime()
            ) |>
          filter(
            date_to_filter >= input$training_filter_range[1] &
              date_to_filter <= input$training_filter_range[2] &
              training_type %in% input$filter_training_type &
              is.na(training_delete) &
              trainer %in% input$filter_training_officer
            ) |>
          select(training_id, training_type, topic, training_description,
                 trainer, date, start_time, end_time) |>
          # Do fancy magic to replace the training officer key with the name.
          mutate(trainer = names(training_trainers)[match(trainer, training_trainers)])

        Table_Data <- functions$FixColNames(Table_Data)
        colnames(Table_Data) <- gsub("Training ", "", colnames(Table_Data))
        rownames(Table_Data) <- Table_Data$Id
        Table_Data$Id <- NULL

        sort_col <- which(names(Table_Data) == "Date")

        DT::datatable(
          Table_Data,
          selection = 'single',
          rownames = FALSE,
          height = "100%",
          options = list(
            lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')),
            pageLength = 10,
            columnDefs = list(
              list(className = 'dt-center', targets = "_all")
            ),
            order = list(list(sort_col - 1, 'desc')),
            scrollX = TRUE
          )
        )


      })

      # Update the add training topic based on the training type.
      observe({
        # browser()
        x <- input$add_training_type

        if(is.null(x)) {
          # Do nothing
        }

        # Can also set the label and select items
        else if(x == "EMS") {
          updateSelectInput(session,
                            "add_training_topic",
                            choices = c(
                              "Airway/Respiraroty/Ventilation",
                              "Cardiovascular",
                              "Trauma",
                              "Medical",
                              "Operations",
                              "Other"
                            ))
        }

        else if(x == "Fire") {
          updateSelectInput(session,
                            "add_training_topic",
                            choices = c(
                              "Fire"
                            ))
        } else if (x == "Wildland") {
          updateSelectInput(session,
                            "add_training_topic",
                            choices = c( "Wildland")
          )
        } else if (x == "Other") {
          updateSelectInput(session,
                            "add_training_topic",
                            choices = c("General")
          )
        }
      })

      # Update the modify training topic based on the type input
      observe({
        # browser()
        x <- input$modify_training_type

        if(is.null(x)) {
          # Do nothing
        }

        # Can also set the label and select items
        else if(x == "EMS") {
          updateSelectInput(session,
                            "modify_training_topic",
                            choices = c(
                              "Airway/Respiratory/Ventilation",
                              "Cardiovascular",
                              "Trauma",
                              "Medical",
                              "Operations",
                              "General",
                              "Other"
                            )
          )
        }

        else if(x == "Fire") {
          updateSelectInput(session,
                            "modify_training_topic",
                            choices = c(
                              "Fire"
                            )
          )
        } else if (x == "Wildland") {
            updateSelectInput(session,
                              "modify_training_topic",
                              choices = c("Wildland")
            )
        } else if (x == "Other") {
          updateSelectInput(session,
                            "modify_training_topic",
                            choices = c("General")
          )
        }
      })

      # Enter information to create the training.
      observeEvent(input$add_training, {
        showModal(modalDialog(
          title = "Add Training",
          dateInput(ns('add_training_date'), 'Training Date', value = app_data$Local_Date),
          timeInput(ns('add_start_time'), "Start Time", value = "18:00:00", minute.steps = 5),
          timeInput(ns('add_end_time'), "End Time", value = "20:00:00", minute.steps = 5),
          selectInput(ns('add_training_type'), 'Training Type', choices = c("EMS", "Fire", "Wildland", "Other"), selected = "EMS"),
          selectInput(ns('add_training_topic'), 'Training Topic', choices = c()), # Update with above observe statement
          textAreaInput(ns('add_description'), 'Training Description'),
          selectInput(ns('add_training_trainer'), 'Training Led By', choices = training_trainers),
          footer = tagList(
            actionButton(ns("action_add_training"), "Add Training")
          ),
          easyClose = TRUE
        ))

      })

      # # Create the training
      observeEvent(input$action_add_training, {
        # browser()
        removeModal()

        local_start_time <- as.POSIXct(paste0(input$add_training_date, " ", data.table::as.ITime(input$add_start_time))) |> force_tz(Sys.getenv('LOCAL_TZ')) + .01 # add hundredth of a second to prevent dumping hms, db stores in character
        local_end_time <- as.POSIXct(paste0(input$add_training_date, " ", data.table::as.ITime(input$add_end_time))) |> force_tz(Sys.getenv('LOCAL_TZ')) + .01 # add hundredth of a second to prevent dumping hms, db stores in character

        if(!functions$VerifyNoOverlap(input$add_start_time, input$add_end_time)) {
          showModal(
            modals$warningModal("Training times overlap with existing training. Please select a different time.")
          )
          return()
        }

        # Use sqlInterpolate
        sql_command <- "INSERT INTO ?training_table (training_type, training_topic, training_description, training_start_time, training_end_time, training_trainer, training_delete) VALUES (?training_type, ?training_topic, ?training_description, ?training_start_time, ?training_end_time, ?training_trainer, NULL);"

        safe_sql <- sqlInterpolate(
          app_data$CON,
          sql_command,
          training_table = SQL(Sys.getenv("TRAINING_TABLE")),
          training_type = input$add_training_type,
          training_topic = input$add_training_topic,
          training_description = input$add_description,
          training_start_time = local_start_time |> with_tz(),
          training_end_time = local_end_time |> with_tz(),
          training_trainer = input$add_training_trainer
        )

        # Execute the safely interpolated SQL command
        write_result <- dbExecute(app_data$CON, safe_sql)


        if(write_result == 1) {
          showModal(
            modalDialog(
              title = "Success",
              "Your training has been successfully added. You may now close this window.",
              easyClose = TRUE
            )
          )
        } else {
          showModal(
            modals$errorModal(paste("Training add failed with write result equal to", write_result))
          )
        }

        # browser()
        updateReactiveValue()


      })


      # Modify the training
      observeEvent(input$modify_training, {
        # browser()

        #FIXME - Currently, the modify training button is not working.
        # The cell_click is not identifying the correct row.
        #####################################################
        cell_click <- input$view_trainings_cell_clicked
        # Make sure a row was clicked
        if (length(cell_click) != 0) {

          showModal(modals$warningModal("This functionality currently isn't working."))
          # showModal(modalDialog(
          #   title = "Modify Training",
          #   strong("Please double check the training type and topic before submitting."),
          #   dateInput(ns('modify_training_date'), 'Training Date', value = rv()[cell_click$row,]$training_date),
          #   timeInput(ns('modify_start_time'), "Start Time", value = rv()[cell_click$row,]$training_start_time, minute.steps = 5),
          #   timeInput(ns('modify_end_time'), "End Time", value = rv()[cell_click$row,]$training_end_time, minute.steps = 5),
          #   selectInput(ns('modify_training_type'), 'Training Type', choices = c("EMS", "Fire", "Wildland"), selected = rv()[cell_click$row,]$training_type),
          #   selectInput(ns('modify_training_topic'), 'Training Topic', choices = c(), selected = rv()[cell_click$row,]$training_topic),
          #   textAreaInput(ns('modify_description'), 'Training Description', value = rv()[cell_click$row,]$training_description),
          #   selectInput(ns('modify_training_trainer'), 'Training Led By', choices = training_trainers, selected = rv()[cell_click$row,]$training_officer),
          #   footer = tagList(
          #     actionButton(ns("action_modify_training"), "Modify Training")
          #   ),
          #   easyClose = TRUE
          # ))
        } else {
          showModal(
            modals$warningModal("Please select a training to modify.")
          )
        }
        ############################################################
      })


      # observeEvent(input$action_modify_training, {
      #   # browser()
      #   removeModal()
      #
      #   cell_click <- input$view_trainings_cell_clicked
      #   modify_training_id <- row.names(rv()[cell_click$row,])
      #
      #
      #   sql_command <- paste0(
      #     "UPDATE ", Sys.getenv("TRAINING_TABLE")," SET training_type = '", input$modify_training_type,
      #     "', training_topic = '", input$modify_training_topic,
      #     "', training_description = '", input$modify_description,
      #     "', training_date = '", input$modify_training_date,
      #     "', training_start_time = '", input$modify_start_time |> strftime(format = "%T"),
      #     "', training_end_time = '", input$modify_end_time |> strftime(format = "%T"),
      #     "', training_trainer = '", input$modify_training_trainer,
      #     "' WHERE training_id = ", modify_training_id, ";"
      #   )
      #
      #   write_result <- DBI::dbExecute(app_data$CON, sql_command)
      #
      #   if(write_result == 1) {
      #     showModal(
      #       modalDialog(
      #         title = "Success",
      #         "Your training has been successfully modified. You may now close this window.",
      #         easyClose = TRUE
      #       )
      #     )
      #   } else {
      #     showModal(
      #       modals$errorModal(paste("Training modify failed with write result equal to", write_result))
      #     )
      #   }
      #
      #   updateReactiveValue()
      #
      # })

      observeEvent(input$delete_training, {
        # browser()

        #FIXME - Currently, the delete training button is not working.
        # The cell_click is not identifying the correct row.
        ##################
        cell_click <- input$view_trainings_cell_clicked

        if (length(cell_click) != 0) {
          showModal(modals$warningModal("This functionality currently isn't working."))
          # showModal(modalDialog(
          #   title = "Confirm Deletion",
          #   "Are you sure you want to delete this training? If you're sure, please type \"Delete\"",
          #   textInput(ns("confirm_deletion"), ""),
          #   footer = tagList(
          #     actionButton(ns("action_delete_training"), "Delete Training")
          #   ),
          #   easyClose = TRUE
          # ))
        } else {
          showModal(
            modals$warningModal("Please select a training to delete.")
          )
        }
        ##################
      })

      # observeEvent(input$action_delete_training, {
      #   # browser()
      #   removeModal()
      #
      #   if(input$confirm_deletion == "Delete") {
      #
      #     cell_click <- input$view_trainings_cell_clicked
      #     modify_training_id <- row.names(rv()[cell_click$row,])
      #
      #     sql_command <- paste0(
      #       "UPDATE ", Sys.getenv("TRAINING_TABLE")," SET training_delete = NOW() WHERE training_id = ", modify_training_id, ";"
      #     )
      #
      #     write_result <- DBI::dbExecute(app_data$CON, sql_command)
      #
      #     if(write_result == 1) {
      #       showModal(
      #         modalDialog(
      #           title = "Success!",
      #           "Your training has been successfully deleted. You may now close this window.",
      #           easyClose = TRUE
      #         )
      #       )
      #     } else {
      #       showModal(
      #         modals$errorModal(paste("Training delete failed with write result equal to", write_result))
      #       )
      #     }
      #
      #
      #   } else {
      #     showModal(modalDialog("Records will not be deleted.", title = "Delete Failed"))
      #   }
      #
      #
      #
      #   updateReactiveValue()
      #
      #
      # })

    }
  )
}
