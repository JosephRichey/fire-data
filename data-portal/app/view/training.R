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
  rhandsontable[...],
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
    actionButton(
      ns('add_training'),
      "Add Training",
      class = 'btn-primary'),
    actionButton(
      ns('modify_training'),
      "Modify Training",
      class = 'btn-secondary'),
    actionButton(
      ns("delete_training"),
      "Delete Training",
      class = 'btn-warning'
      ),
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
    actionButton(
      ns('select_training'),
      "Select Training",
      class = "btn-primary"),
    hr(),
    actionButton(
      ns('add_attendance'),
      "Add Attendance",
      class = "btn-secondary",
      disabled = TRUE),
    actionButton(
      ns("delete_attendance"),
      "Delete Attendance",
      class = 'btn-warning',
      disabled = TRUE)
  )
}



AttendanceOutput <- function(id) {
  ns <- NS(id)
  tagList(
      card(
        card_header(textOutput(ns("which_attendance"))),
        card_body(
          rHandsontableOutput(ns('View_Attendance'),
                              width = "100%", height = "100%"),
          htmlOutput(ns("save_warning"))
        ),
        actionButton(ns("submit_attendance_update"),
                     "Submit Edits",
                     class = "btn-primary",
                     disabled = TRUE)
      )

  )
}

AttendanceUITest <- function(id) {
  ns <- NS(id)

  tagList(
    actionButton(
      ns('select_training'),
      "Select Training",
      class = "btn-primary"),
    hr(),
    actionButton(
      ns('add_attendance'),
      "Add Attendance",
      disabled = TRUE),
    actionButton(
      ns("delete_attendance"),
      "Delete Attendance",
      disabled = TRUE),
    actionButton(
      ns("excuse_attendee"),
      "Excuse Attendee",
      disabled = TRUE,
      class = "btn-warning")
  )
}

AttendanceOutputTest <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(textOutput(ns("which_attendance"))),
      card_body(
        rHandsontableOutput(ns('View_Attendance_Test'),
                            width = "auto", height = "100%"),
        textOutput(ns('excused_firefighters')),
        htmlOutput(ns("save_warning"))
      ),
      actionButton(ns("submit_attendance_update"),
                   "Submit Edits",
                   class = "btn-primary",
                   disabled = TRUE)
    )

  )
}



Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      ##### GLOBAL #####

      r_Training <- reactiveVal(app_data$Training)

      ns <- session$ns

      updateReactiveTraining <- function() {
        r_Training(DBI::dbGetQuery(app_data$CON, "SELECT * FROM training") |>
             mutate(
               start_time = functions$ConvertLocalPosix(start_time),
               end_time = functions$ConvertLocalPosix(end_time)
             )
        )
      }

      updateReactiveAttendance <- function() {
        r_Attendance(DBI::dbGetQuery(app_data$CON, "SELECT * FROM attendance") |>
                       mutate(check_in = functions$ConvertLocalPosix(check_in),
                              check_out = functions$ConvertLocalPosix(check_out))
        )
      }

      ##### Trainings #####
      # This is everything that's visible in the table after filters are applied. Use it to generate View_Trainings.
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

        # FIXME Abstract
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

        updateReactiveTraining()


      })


      # Modify the training
      observe({
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

        updateReactiveTraining()

      })

      observeEvent(input$delete_training, {
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
                actionButton(ns("confirm_deletion"), "Delete", class = "btn-warning")
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


        updateReactiveTraining()


      }) |>
        bindEvent(input$confirm_deletion)

      ##### Attendance #####

      # This holds the data that will be displayed in the editable attendance table.
      r_Editable_Attendance <- reactive({
        req(r_Target_Training())

        Table_Data <- r_Attendance() |>
          filter(training_id == r_Target_Training()[1, "training_id"]) |>
          #FIXME This happens often enough should probably be a function
          # mutate(firefighter_id = names(app_data$Firefighter$full_name)[match(firefighter_id, app_data$Firefighter$firefighter_id)])
          left_join(app_data$Firefighter, by = c("firefighter_id" = "firefighter_id")) |>
          select(attendance_id, full_name, check_in, check_out, credit, excused) |>
          mutate(
            check_in = check_in |> functions$FormatLocalTime(),
            check_out = check_out |> functions$FormatLocalTime(),
            credit = ifelse(credit == 1, TRUE, FALSE),
            excused = ifelse(excused == 1, TRUE, FALSE)
          )

        #FIXME Same with some of these things.
        Table_Data <- functions$FixColNames(Table_Data)
        colnames(Table_Data) <- gsub("Attendance ", "", colnames(Table_Data))
        rownames(Table_Data) <- Table_Data$Id
        Table_Data$Id <- NULL
        return(Table_Data)
      })

      # Root dataframe. When updated, flows through rest of app.
      r_Attendance <- reactiveVal(app_data$Attendance)

      # This holds the data about which training we're editing attendance for.
      r_Target_Training <- reactiveVal(NULL)

      # All trainings that can have their attendance modified
      r_Eligible_Trainings <- reactive({
        Table_Data <- r_Training() |>
          mutate(
            date = start_time |> functions$FormatLocalDate(asPosix = TRUE)
          ) |>
          filter(
            date >= Sys.Date() - 30 & #FIXME Set with settings
              is.na(training_delete)
          ) |>
          select(training_id, training_type, training_description,
                 trainer, date) |>
          mutate(trainer = names(training_trainers)[match(trainer, training_trainers)])

        Table_Data <- functions$FixColNames(Table_Data)
        colnames(Table_Data) <- gsub("Training ", "", colnames(Table_Data))
        rownames(Table_Data) <- Table_Data$Id
        Table_Data$Id <- NULL
        return(Table_Data)
      })



      output$Select_Training_Table <- DT::renderDataTable({
        Table_Data <- r_Eligible_Trainings()

        sort_col <- which(names(Table_Data) == "Date")

        DT::datatable(
          Table_Data,
          selection = 'single',
          rownames = FALSE,
          height = "100%",
          options = list(
            dom = 't',
            paging = FALSE,
            searching = FALSE,
            columnDefs = list(
              list(className = 'dt-center', targets = "_all")
            ),
            order = list(list(sort_col - 1, 'desc')),
            scrollX = TRUE
          )
        ) |>
          formatDate(columns = c("Date"), method = "toLocaleDateString")
      })

      observe({
        cat("Select Training", file = stderr())
        #####################################################
        showModal(
          modalDialog(
            title = "Select Training",
            DTOutput(ns('Select_Training_Table')),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("load_training_attendance"), "Select", class = "btn-primary")
            )
          )
        )

      }) |>
        bindEvent(input$select_training)

      observe({
        removeModal()

        r_Target_Training(
          r_Eligible_Trainings()[input$Select_Training_Table_rows_selected,] |>
            rownames_to_column("training_id")
        )

        updateActionButton(session, "add_attendance", disabled = FALSE)
        updateActionButton(session, "modify_attendance", disabled = FALSE)
        updateActionButton(session, "delete_attendance", disabled = FALSE)
        updateActionButton(session, "excuse_attendee", disabled = FALSE)
        updateActionButton(session, "submit_attendance_update", disabled = FALSE)
      }) |>
          bindEvent(input$load_training_attendance)

      output$View_Attendance <- renderRHandsontable({

        Table_Data <- r_Editable_Attendance()

        if(is.null(Table_Data)) {
          return(NULL)
        }

        rhandsontable(Table_Data,
                      rowHeaders = FALSE,
                      contextMenu = FALSE) |>
          hot_col(col = "Full Name", readOnly = TRUE)


      })

      observe({
        cat("Delete Attendee", file = stderr())
        #####################################################
        firefighter_choices <- r_Editable_Attendance()$'Full Name'

        showModal(
          modals$deleteAttendanceModal(ns,
                             firefighter_choices = firefighter_choices)
        )

      }) |>
        bindEvent(input$delete_attendance)


      observe({

        if(length(input$delete_attendance_firefighters) == 0) {
          shinyalert(
            title = "Warning",
            type = "warning",
            text = "Please select firefighter(s) to delete attendance for."
          )
          return()
        }

        removeModal()

        write_result <- c()

        for(i in input$delete_attendance_firefighters) {

          firefighter_id <- app_data$Firefighter$firefighter_id[app_data$Firefighter$full_name == i]

          sql_statement <- paste0("DELETE FROM attendance WHERE training_id = ",
                                  r_Target_Training()[1, "training_id"], " AND firefighter_id = ", firefighter_id, ";")
          write_result <- c(write_result, dbExecute(app_data$CON, sql_statement))

        }

        #FIXME Bypassing test for now. Will need a more robust check.
        if(TRUE) {
          showModal(
            modalDialog(
              title = "Success",
              "Your attendance has been successfully deleted. You may now close this window.",
              easyClose = TRUE
            )
          )
        } else {
          showModal(
            modals$errorModal(paste("Attendance delete failed with write result equal to", write_result))
          )
        }

        updateReactiveAttendance()

      }) |>
        bindEvent(input$submit_delete_attendance)

      ##### TESTING #####
      output$View_Attendance_Test <- renderRHandsontable({

        Table_Data <- r_Editable_Attendance() |>
          select(-Excused)

        if(is.null(Table_Data)) {
          return(NULL)
        }

        rhandsontable(Table_Data,
                      rowHeaders = FALSE,
                      contextMenu = FALSE) |>
          hot_col(col = "Full Name", readOnly = TRUE)


      })

      observe({
        cat("Excuse Attendee", file = stderr())
        #####################################################
        firefighter_choices <- app_data$Firefighter |>
          filter(active_status == 1) |>
          pull(full_name)

        showModal(
          modals$excuseAttendanceModal(ns,
                                       firefighter_choices = firefighter_choices)
        )

      }) |>
        bindEvent(input$excuse_attendee)

      observe({


        if(length(input$excuse_attendance_firefighters) == 0) {
          shinyalert(
            title = "Warning",
            type = "warning",
            text = "Please select firefighter(s) to excuse attendance for."
          )
          return()
        }

        removeModal()

        write_result <- c()

        for(i in input$excuse_attendance_firefighters) {

          firefighter_id <- app_data$Firefighter$firefighter_id[app_data$Firefighter$full_name == i]

          sql_statement <- paste0("UPDATE attendance SET excused = 1 WHERE training_id = ",
                                  r_Target_Training()[1, "training_id"], " AND firefighter_id = ", firefighter_id, ";")
          write_result <- c(write_result, dbExecute(app_data$CON, sql_statement))

        }

        #FIXME Bypassing test for now. Will need a more robust check.
        if(TRUE) {
          showModal(
            modalDialog(
              title = "Success",
              "Your attendance has been successfully excused. You may now close this window.",
              easyClose = TRUE
            )
          )
        } else {
          showModal(
            modals$errorModal(paste("Attendance excuse failed with write result equal to", write_result))
          )
        }

        updateReactiveAttendance()

      }) |>
        bindEvent(input$submit_excuse_attendance)

      output$excused_firefighters <- renderText({
        req(r_Target_Training())

        excused <- r_Attendance() |>
          filter(training_id == r_Target_Training()[1, "training_id"]) |>
          filter(excused == 1) |>
          left_join(app_data$Firefighter, by = c("firefighter_id" = "firefighter_id")) |>
          pull(full_name)

        if(length(excused) == 0) {
          return("No firefighters have been excused.")
        } else {
          return(paste("The following firefighters have been excused:", paste(excused, collapse = ", ")))
        }
      })


      ##### TESTING #####

      observe({
        cat("Add Attendance", file = stderr())
        #####################################################
        # FIXME Probably need to abstract quite a bit of this. It'll be used a lot of places.
        firefighter_choices <- setdiff(
          app_data$Firefighter |>
                 filter(active_status == 1) |>
                 pull(full_name),
          r_Editable_Attendance()$'Full Name'
        )

        showModal(
          modals$attendanceModal(ns,
                                 firefighter_choices = firefighter_choices,
                                 check_in = paste0(r_Training() |>
                                   filter(training_id == r_Target_Training()[1, "training_id"]) |>
                                   pull(start_time) |>
                                   functions$FormatLocalTime(), ":00"),
                                 check_out = paste0(r_Training() |>
                                   filter(training_id == r_Target_Training()[1, "training_id"]) |>
                                   pull(end_time) |>
                                   functions$FormatLocalTime(), ":00")
          )
        )

      }) |>
        bindEvent(input$add_attendance)

      observeEvent(input$submit_add_attendance, {


        if(length(input$add_attendance_firefighters) == 0) {
          shinyalert(
            title = "Warning",
            type = "warning",
            text = "Please select a firefighter to add attendance for."
          )
          return()
        }


        removeModal()

        write_result <- c()

        for(i in input$add_attendance_firefighters) {

          firefighter_id <- app_data$Firefighter$firefighter_id[app_data$Firefighter$full_name == i]

          sql_statement <- paste0("INSERT INTO attendance (training_id, firefighter_id, check_in, check_out, credit, excused) VALUES (",
                                  r_Target_Training()[1, "training_id"], ", ",
                                  firefighter_id, ", '",
                                  input$add_attendance_check_in |>
                                    functions$BuiltDateTime(r_Target_Training()[1,'Date'], 'local') |>
                                    functions$FormatUtcDateTime(), "', '",
                                  input$add_attendance_check_out |>
                                    functions$BuiltDateTime(r_Target_Training()[1,'Date'], 'local') |>
                                    functions$FormatUtcDateTime(), "', ",
                                  if_else(input$add_attendance_attendance_credit == 'Credit', "1", "0"), ", ",
                                  "0);")
          write_result <- c(write_result, dbExecute(app_data$CON, sql_statement))


        }


        #FIXME Bypassing test for now. Will need a more robust check.
        if(TRUE) {
          showModal(
            modalDialog(
              title = "Success",
              "Your attendance has been successfully added. You may now close this window.",
              easyClose = TRUE
            )
          )
        } else {
          showModal(
            modals$errorModal(paste("Attendance add failed with write result equal to", write_result))
          )
        }

        updateReactiveAttendance()

      })



      observe({
        cat("Editing Attendance", file = stderr())
        new_data <- input$View_Attendance |>
          hot_to_r() |>
          rownames_to_column("Id")

        # FIXME Removing this- can be empty with excused attendance
        # if (any(is.na(new_data) | new_data == "")) {
        #   shinyalert(
        #     title = "Warning",
        #     type = "warning",
        #     text = "Please fill in all fields."
        #   )
        #   return()
        # }

        if(any(!grepl('^\\d{2}:\\d{2}$',new_data$'Check In')) |
           any(!grepl('^\\d{2}:\\d{2}$',new_data$'Check Out'))) {

          shinyalert(
            title = "Warning",
            type = "warning",
            text = "Please enter times in the format HH:MM."
          )
          return()
        }

        if(any(new_data$'Check In' > new_data$'Check Out')) {
          #FIXME This won't allow overnight trainings (i.e., clockin in at 22:00 and out at 06:00)
          shinyalert(
            title = "Warning",
            type = "warning",
            text = "Check out time must be after check in time."
          )
          return()
        }

        write_results <- c()

        for(i in 1:nrow(new_data)) {
          sql_statement <- paste0("UPDATE attendance SET check_in = ?check_in, check_out = ?check_out, credit = ", ifelse(new_data[i, "Credit"], 1, 0), ", excused = ", ifelse(new_data[i, "Excused"], 1, 0), " WHERE attendance_id = ", new_data[i, "Id"], ";")

          safe_sql <- sqlInterpolate(
            app_data$CON,
            sql_statement,
            check_in = new_data[i, "Check In"] |>
              functions$BuiltDateTime(r_Target_Training()[1,'Date'], 'local') |>
              functions$FormatUtcDateTime(),
            check_out = new_data[i, "Check Out"] |>
              functions$BuiltDateTime(r_Target_Training()[1,'Date'], 'local') |>
              functions$FormatUtcDateTime()
          )

          #FIXME Needs to be in a try catch
          write_results <- c(write_results, dbExecute(app_data$CON, safe_sql))

        }

        updateReactiveAttendance()

        #FIXME Bypassing test for now. Will need a more robust check.
        if(TRUE) {
          showModal(
            modalDialog(
              title = "Success",
              "Your attendance has been successfully updated. You may now close this window.",
              easyClose = TRUE,
              footer = tagList(
                modalButton("Close")
              )
            )
          )
        } else {
          showModal(
            modals$errorModal(paste("Attendance update failed with write results equal to", write_results))
          )
        }

      }) |>
        bindEvent(input$submit_attendance_update)

      ##### Rendered messages displayed to user #####
      output$which_attendance <- renderText({
        if(is.null(r_Target_Training())) {
          return("No training selected.")
        } else {
          target_training <- r_Training() |>
            filter(training_id == r_Target_Training()[1, "training_id"])

          return(
            paste(
              "Editing attendance for",
              target_training$training_type,
              "training on",
              target_training$start_time |>
                functions$FormatLocalDate(asPosix = TRUE),
              "running from",
              target_training$start_time |>
                functions$FormatLocalTime(),
              "to",
              target_training$end_time |>
                functions$FormatLocalTime()
            )
          )
        }
      })

      output$save_warning <- renderText({
        req(input$View_Attendance)

        Table_Data <- r_Editable_Attendance()

        changes <- all.equal(hot_to_r(input$View_Attendance), Table_Data, check.attributes = FALSE)
        if(!isTRUE(changes)) {
            return(
              HTML(paste0(
                "<span style='font-size: 1.2em; font-weight: bold; color: #9933CC;'>You have unsaved changes</span>"
              ))

            )
        } else {
          return(NULL)
        }

      })



    }
  )
}
