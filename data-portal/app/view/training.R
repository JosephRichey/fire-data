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
  shinyalert[...],
)

box::use(
  ../logic/app_data,
  ../logic/functions,
  ../modals/modals,
)

training_trainers <- app_data$Firefighter |>
  filter(trainer == TRUE) |>
  select(full_name, firefighter_id) %>%
  # https://ivelasq.rbind.io/blog/understanding-the-r-pipe/
  # See "Getting to the solution" sect
  (\(.) setNames(.$firefighter_id, .$full_name))




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
        #FIXME Set by settings
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

      rv <- reactiveValues()

      ns <- session$ns

      updateReactiveValue <- function() {
        r_Training(DBI::dbGetQuery(app_data$CON, "SELECT * FROM training") |>
             mutate(
               start_time = functions$ConvertLocalPosix(start_time),
               end_time = functions$ConvertLocalPosix(end_time)
             )
        )
      }

      displayedTrainings <- reactive({
        Table_Data <- r_Training() |>
          mutate(
            date = start_time |> functions$FormatLocalDate(asPosix = TRUE),
            start_time = start_time |> functions$FormatLocalTime(),
            end_time = end_time |> functions$FormatLocalTime()
          ) |>
          filter(
            date >= input$training_filter_range[1] &
              date <= input$training_filter_range[2] &
              training_type %in% input$filter_training_type &
              is.na(training_delete) &
              trainer %in% input$filter_training_officer
          ) |>
          select(training_id, training_type, topic, training_description,
                 trainer, date, start_time, end_time) |>
          mutate(trainer = names(training_trainers)[match(trainer, training_trainers)])

        Table_Data <- functions$FixColNames(Table_Data)
        colnames(Table_Data) <- gsub("Training ", "", colnames(Table_Data))
        rownames(Table_Data) <- Table_Data$Id
        Table_Data$Id <- NULL
        return(Table_Data)
      })

      # Display current training data
      output$view_trainings <- renderDT({
        # browser()

        Table_Data <- displayedTrainings()

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
        ) |>
          formatDate(columns = c("Date"), method = "toLocaleDateString")


      })

      # Update the add training topic based on the training type.
      observe({
        #FIXME For some reason, this isn't alwasy updating, especially when doing repeated updates.
        # It probably has to do with the fact that there's not change in the input, so the observe isn't triggering.
        req(input$training_type)
        x <- input$training_type

        # FIXME This will be set by settings table
        if(x == "EMS") {
          updateSelectInput(session,
                            "topic",
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
                            "topic",
                            choices = c(
                              "Fire"
                            ))
        } else if (x == "Wildland") {
          updateSelectInput(session,
                            "topic",
                            choices = c( "Wildland")
          )
        } else if (x == "Other") {
          updateSelectInput(session,
                            "topic",
                            choices = c("General")
          )
        }
      })



      # Enter information to create the training.
      observeEvent(input$add_training, {
        # browser()

        showModal(
          modals$trainingModal(ns,
                               edit = FALSE,
                               training_date = app_data$Local_Date,
                               start_time = "18:00:00",
                               end_time = "20:00:00",
                               type_choices = c("EMS", "Fire", "Wildland", "Other"), #FIXME
                               type = NULL,
                               topic = NULL,
                               training_description = NULL,
                               training_trainers = training_trainers,
                               trainer = NULL
                               )
        )

      })

      # # Create the training
      observeEvent(input$submit_add_training, {
        # browser()
        removeModal()

        utc_start_time <- functions$BuiltDateTime(input$start_time, input$training_date, 'local')
        utc_end_time <- functions$BuiltDateTime(input$end_time, input$training_date, 'local')

        # FIXME Disabling this until final build
        # if(!functions$VerifyNoOverlap(input$add_start_time, input$add_end_time)) {
        #   showModal(
        #     modals$warningModal("Training times overlap with existing training. Please select a different time.")
        #   )
        #   return()
        # }

        # Use sqlInterpolate
        sql_command <- "INSERT INTO ?training_table
        (training_type, topic, training_description,
        start_time, end_time,
        trainer, training_delete) VALUES
        (?training_type, ?topic, ?training_description, ?start_time, ?end_time, ?trainer, NULL);"

        safe_sql <- sqlInterpolate(
          app_data$CON,
          sql_command,
          training_table = SQL('training'),
          training_type = input$training_type,
          topic = input$topic,
          training_description = input$training_description,
          start_time = utc_start_time |> format("%Y-%m-%d %H:%M:%S", tz = 'UTC', usetz = FALSE),
          end_time = utc_end_time |> format("%Y-%m-%d %H:%M:%S", tz = 'UTC', usetz = FALSE),
          trainer = input$trainer
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

        updateReactiveValue()


      })


      # Modify the training
      observe({
        # browser()
        cat("Modify Training", file = stderr())
        #####################################################
        cell_click <- input$view_trainings_rows_selected
        # Make sure a row was clicked
        if (length(cell_click) != 0) {

          Modify_ID <- row.names(displayedTrainings()[cell_click,])
          To_Modify <- r_Training() |>
            filter(training_id == Modify_ID) |>
            mutate(
              date = start_time |> functions$FormatLocalDate(asPosix = TRUE),
              start_time = start_time |> functions$FormatLocalTime(),
              end_time = end_time |> functions$FormatLocalTime()
            )

          showModal(
            modals$trainingModal(ns,
                                 edit = TRUE,
                                 training_date = To_Modify$date,
                                 start_time = paste0(To_Modify$start_time, ":00"), #FIXME Workaround to get input to render correctly
                                 end_time = paste0(To_Modify$end_time, ":00"),
                                 type_choices = c("EMS", "Fire", "Wildland", "Other"), #FIXME
                                 type = To_Modify$training_type,
                                 topic = To_Modify$topic,
                                 training_description = To_Modify$training_description,
                                 training_trainers = training_trainers,
                                 trainer = To_Modify$trainer
            )
          )

        } else {
          shinyalert(
            title = "Warning",
            type = "warning",
            text = "Please select a training to modify."
            )
        }
        ############################################################
      }) |>
        bindEvent(input$modify_training)


      observeEvent(input$submit_edit_training, {
        # browser()
        removeModal()

        Modify_ID <- row.names(displayedTrainings()[input$view_trainings_rows_selected,])

        # FIXME Disabling this until final build
        # if(!functions$VerifyNoOverlap(input$add_start_time, input$add_end_time)) {
        #   showModal(
        #     modals$warningModal("Training times overlap with existing training. Please select a different time.")
        #   )
        #   return()
        # }

        utc_start_time <- functions$BuiltDateTime(input$start_time, input$training_date, 'local')
        utc_end_time <- functions$BuiltDateTime(input$end_time, input$training_date, 'local')

        # Use sqlInterpolate
        sql_command <- "UPDATE ?training_table
        SET training_type = ?training_type,
        topic = ?topic,
        training_description = ?training_description,
        start_time = ?start_time,
        end_time = ?end_time,
        trainer = ?trainer
        WHERE training_id = ?training_id;"

        safe_sql <- sqlInterpolate(
          app_data$CON,
          sql_command,
          training_table = SQL('training'),
          training_type = input$training_type,
          topic = input$topic,
          training_description = input$training_description,
          start_time = utc_start_time |> format("%Y-%m-%d %H:%M:%S", tz = 'UTC', usetz = FALSE),
          end_time = utc_end_time |> format("%Y-%m-%d %H:%M:%S", tz = 'UTC', usetz = FALSE),
          trainer = input$trainer,
          training_id = Modify_ID
        )

        write_result <- DBI::dbExecute(app_data$CON, safe_sql)

        if(write_result == 1) {
          showModal(
            modalDialog(
              title = "Success",
              "Your training has been successfully modified. You may now close this window.",
              easyClose = TRUE
            )
          )
        } else {
          showModal(
            modals$errorModal(paste("Training modify failed with write result equal to", write_result))
          )
        }

        updateReactiveValue()

      })

      observeEvent(input$delete_training, {
        # browser()
        cat("Delete Training", file = stderr())
        #####################################################
        cell_click <- input$view_trainings_rows_selected
        # Make sure a row was clicked
        if (length(cell_click) != 0) {

          Delete_ID <- row.names(displayedTrainings()[cell_click,])

          showModal(
            modalDialog(
              title = "Warning",
              "Are you sure you want to delete this training?",
              footer = tagList(
                modalButton("Cancel"),
                actionButton(ns("confirm_deletion"), "Delete", class = "btn-danger")
              )
            )
          )

        } else {
          shinyalert(
            title = "Warning",
            type = "warning",
            text = "Please select a training to delete."
          )
        }
        ##################
      })

      observe({
        removeModal()

        Delete_ID <- row.names(displayedTrainings()[input$view_trainings_rows_selected,])

        sql_command <- paste0(
          "UPDATE training SET training_delete = NOW() WHERE training_id = ", Delete_ID, ";"
        )

        #FIXME This should be in a try catch
        write_result <- DBI::dbExecute(app_data$CON, sql_command)

        if(write_result == 1) {
          showModal(
            modalDialog(
              title = "Success!",
              "Your training has been successfully deleted. You may now close this window.",
              easyClose = TRUE
            )
          )
        } else {
          showModal(
            modals$errorModal(paste("Training delete failed with write result equal to", write_result))
          )
        }


        updateReactiveValue()


      }) |>
        bindEvent(input$confirm_deletion)

    }
  )
}
