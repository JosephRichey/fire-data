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
  ../../logic/app_data,
  ../../logic/functions,
  ../../modals/modals,
)

#' @export
UI <- function(id) {
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

#' @export
Output <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(textOutput(ns("which_attendance"))),
      card_body(
        rHandsontableOutput(ns('View_Attendance'),
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


#' @export
Server <- function(id, rdfs) {
  moduleServer(
    id,
    function(input, output, session) {

      ##### GLOBAL #####
      ns <- session$ns

      ##### Attendance #####
      # All trainings that can have their attendance modified
      r_Eligible_Trainings <- reactive({
        Table_Data <- rdfs$training |>
          mutate(
            date = start_time |> functions$ConvertToLocalPosix(
              input = 'datetime',
              output = 'date'
            )
          ) |>
          filter(
            date >= Sys.Date() - functions$GetSetting('training', key = 'training_edit_restriction') &
              is.na(is_deleted)
          ) |>
          left_join(app_data$Training_Classifcaion, by = c("classification_id" = "id")) |>
          left_join(rdfs$firefighter, by = c("trainer" = "id")) |>
          select(id, training_category, training_description,
                 full_name, date) |>
          rename(trainer = full_name)

        Table_Data <- functions$FixColNames(Table_Data, prefix = "Training ")
        rownames(Table_Data) <- Table_Data$Id
        Table_Data$Id <- NULL
        return(Table_Data)
      })

      # This holds the data that will be displayed in the editable attendance table.
      r_Editable_Attendance <- reactive({
        browser()

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

        Table_Data <- functions$FixColNames(Table_Data, prefix = "Attendance ")
        rownames(Table_Data) <- Table_Data$Id
        Table_Data$Id <- NULL
        return(Table_Data)
      })

      # This holds the data about which training we're editing attendance for.
      r_Target_Training <- reactiveVal(NULL)


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

      # output$View_Attendance <- renderRHandsontable({
      #
      #   Table_Data <- r_Editable_Attendance()
      #
      #   if(is.null(Table_Data)) {
      #     return(NULL)
      #   }
      #
      #   rhandsontable(Table_Data,
      #                 rowHeaders = FALSE,
      #                 contextMenu = FALSE) |>
      #     hot_col(col = "Full Name", readOnly = TRUE)
      #
      #
      # })

      # observe({
      #   cat("Delete Attendee", file = stderr())
      #   #####################################################
      #   firefighter_choices <- r_Editable_Attendance()$'Full Name'
      #
      #   showModal(
      #     modals$deleteAttendanceModal(ns,
      #                                  firefighter_choices = firefighter_choices)
      #   )
      #
      # }) |>
      #   bindEvent(input$delete_attendance)
      #
      #
      # observe({
      #
      #   if(length(input$delete_attendance_firefighters) == 0) {
      #     shinyalert(
      #       title = "Warning",
      #       type = "warning",
      #       text = "Please select firefighter(s) to delete attendance for."
      #     )
      #     return()
      #   }
      #
      #   removeModal()
      #
      #   write_result <- c()
      #
      #   for(i in input$delete_attendance_firefighters) {
      #
      #     firefighter_id <- app_data$Firefighter$firefighter_id[app_data$Firefighter$full_name == i]
      #
      #     sql_statement <- paste0("DELETE FROM attendance WHERE training_id = ",
      #                             r_Target_Training()[1, "training_id"], " AND firefighter_id = ", firefighter_id, ";")
      #     write_result <- c(write_result, dbExecute(app_data$CON, sql_statement))
      #
      #   }
      #
      #   #FIXME Bypassing test for now. Will need a more robust check.
      #   if(TRUE) {
      #     showModal(
      #       modalDialog(
      #         title = "Success",
      #         "Your attendance has been successfully deleted. You may now close this window.",
      #         easyClose = TRUE
      #       )
      #     )
      #   } else {
      #     showModal(
      #       modals$errorModal(paste("Attendance delete failed with write result equal to", write_result))
      #     )
      #   }
      #
      #   updateReactiveAttendance()
      #
      # }) |>
      #   bindEvent(input$submit_delete_attendance)
      #
      # ##### TESTING #####
      output$View_Attendance <- renderRHandsontable({
        browser()

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


      # ##### TESTING #####
      #
      # observe({
      #   cat("Add Attendance", file = stderr())
      #   #####################################################
      #   # FIXME Probably need to abstract quite a bit of this. It'll be used a lot of places.
      #   firefighter_choices <- setdiff(
      #     app_data$Firefighter |>
      #       filter(active_status == 1) |>
      #       pull(full_name),
      #     r_Editable_Attendance()$'Full Name'
      #   )
      #
      #   showModal(
      #     modals$attendanceModal(ns,
      #                            firefighter_choices = firefighter_choices,
      #                            check_in = paste0(r_Training() |>
      #                                                filter(training_id == r_Target_Training()[1, "training_id"]) |>
      #                                                pull(start_time) |>
      #                                                functions$FormatLocalTime(), ":00"),
      #                            check_out = paste0(r_Training() |>
      #                                                 filter(training_id == r_Target_Training()[1, "training_id"]) |>
      #                                                 pull(end_time) |>
      #                                                 functions$FormatLocalTime(), ":00")
      #     )
      #   )
      #
      # }) |>
      #   bindEvent(input$add_attendance)
      #
      # observeEvent(input$submit_add_attendance, {
      #
      #
      #   if(length(input$add_attendance_firefighters) == 0) {
      #     shinyalert(
      #       title = "Warning",
      #       type = "warning",
      #       text = "Please select a firefighter to add attendance for."
      #     )
      #     return()
      #   }
      #
      #
      #   removeModal()
      #
      #   write_result <- c()
      #
      #   for(i in input$add_attendance_firefighters) {
      #
      #     firefighter_id <- app_data$Firefighter$firefighter_id[app_data$Firefighter$full_name == i]
      #
      #     sql_statement <- paste0("INSERT INTO attendance (training_id, firefighter_id, check_in, check_out, credit, excused) VALUES (",
      #                             r_Target_Training()[1, "training_id"], ", ",
      #                             firefighter_id, ", '",
      #                             input$add_attendance_check_in |>
      #                               functions$BuiltDateTime(r_Target_Training()[1,'Date'], 'local') |>
      #                               functions$FormatUtcDateTime(), "', '",
      #                             input$add_attendance_check_out |>
      #                               functions$BuiltDateTime(r_Target_Training()[1,'Date'], 'local') |>
      #                               functions$FormatUtcDateTime(), "', ",
      #                             if_else(input$add_attendance_attendance_credit == 'Credit', "1", "0"), ", ",
      #                             "0);")
      #     write_result <- c(write_result, dbExecute(app_data$CON, sql_statement))
      #
      #
      #   }
      #
      #
      #   #FIXME Bypassing test for now. Will need a more robust check.
      #   if(TRUE) {
      #     showModal(
      #       modalDialog(
      #         title = "Success",
      #         "Your attendance has been successfully added. You may now close this window.",
      #         easyClose = TRUE
      #       )
      #     )
      #   } else {
      #     showModal(
      #       modals$errorModal(paste("Attendance add failed with write result equal to", write_result))
      #     )
      #   }
      #
      #   updateReactiveAttendance()
      #
      # })
      #
      #
      #
      # observe({
      #   cat("Editing Attendance", file = stderr())
      #   new_data <- input$View_Attendance |>
      #     hot_to_r() |>
      #     rownames_to_column("Id")
      #
      #   # FIXME Removing this- can be empty with excused attendance
      #   # if (any(is.na(new_data) | new_data == "")) {
      #   #   shinyalert(
      #   #     title = "Warning",
      #   #     type = "warning",
      #   #     text = "Please fill in all fields."
      #   #   )
      #   #   return()
      #   # }
      #
      #   if(any(!grepl('^\\d{2}:\\d{2}$',new_data$'Check In')) |
      #      any(!grepl('^\\d{2}:\\d{2}$',new_data$'Check Out'))) {
      #
      #     shinyalert(
      #       title = "Warning",
      #       type = "warning",
      #       text = "Please enter times in the format HH:MM."
      #     )
      #     return()
      #   }
      #
      #   if(any(new_data$'Check In' > new_data$'Check Out')) {
      #     #FIXME This won't allow overnight trainings (i.e., clockin in at 22:00 and out at 06:00)
      #     shinyalert(
      #       title = "Warning",
      #       type = "warning",
      #       text = "Check out time must be after check in time."
      #     )
      #     return()
      #   }
      #
      #   write_results <- c()
      #
      #   for(i in 1:nrow(new_data)) {
      #     sql_statement <- paste0("UPDATE attendance SET check_in = ?check_in, check_out = ?check_out, credit = ", ifelse(new_data[i, "Credit"], 1, 0), ", excused = ", ifelse(new_data[i, "Excused"], 1, 0), " WHERE attendance_id = ", new_data[i, "Id"], ";")
      #
      #     safe_sql <- sqlInterpolate(
      #       app_data$CON,
      #       sql_statement,
      #       check_in = new_data[i, "Check In"] |>
      #         functions$BuiltDateTime(r_Target_Training()[1,'Date'], 'local') |>
      #         functions$FormatUtcDateTime(),
      #       check_out = new_data[i, "Check Out"] |>
      #         functions$BuiltDateTime(r_Target_Training()[1,'Date'], 'local') |>
      #         functions$FormatUtcDateTime()
      #     )
      #
      #     #FIXME Needs to be in a try catch
      #     write_results <- c(write_results, dbExecute(app_data$CON, safe_sql))
      #
      #   }
      #
      #   updateReactiveAttendance()
      #
      #   #FIXME Bypassing test for now. Will need a more robust check.
      #   if(TRUE) {
      #     showModal(
      #       modalDialog(
      #         title = "Success",
      #         "Your attendance has been successfully updated. You may now close this window.",
      #         easyClose = TRUE,
      #         footer = tagList(
      #           modalButton("Close")
      #         )
      #       )
      #     )
      #   } else {
      #     showModal(
      #       modals$errorModal(paste("Attendance update failed with write results equal to", write_results))
      #     )
      #   }
      #
      # }) |>
      #   bindEvent(input$submit_attendance_update)
      #
      # ##### Rendered messages displayed to user #####
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
      #
      # output$save_warning <- renderText({
      #   req(input$View_Attendance)
      #
      #   Table_Data <- r_Editable_Attendance()
      #
      #   changes <- all.equal(hot_to_r(input$View_Attendance), Table_Data, check.attributes = FALSE)
      #   if(!isTRUE(changes)) {
      #     return(
      #       HTML(paste0(
      #         "<span style='font-size: 1.2em; font-weight: bold; color: #9933CC;'>You have unsaved changes</span>"
      #       ))
      #
      #     )
      #   } else {
      #     return(NULL)
      #   }
      #
      # })
    }
)}
