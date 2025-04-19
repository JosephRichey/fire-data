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
  magrittr[...],
  logger[...],
)

box::use(
  ../../logic/app_data,
  ../../logic/functions,
  ../../logic/app_data,
  ../../modals/modals,
  ../../logic/logging,
)

#' @export
UI <- function(id) {
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
                       start = functions$GetSetting('training', key = 'start_date_filter'),
                       end = functions$GetSetting('training', key = 'end_date_filter')
        ),
        pickerInput(ns('filter_training_category'),
                    'Training Category',
                    choices = app_data$training_category_filter,
                    selected = app_data$training_category_filter,
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
        ),
        uiOutput(ns('training_officer_filter'))
      )
    )
  )
}

#' @export
Output <- function(id) {
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

#' @export
Server <- function(id, rdfs) {
  moduleServer(
    id,
    function(input, output, session) {

      ##### UI Dynamic Rendering #####
      output$training_officer_filter <- renderUI({
        log_info("Rendering training_officer_filter", namespace = "Training")
        pickerInput(
          session$ns('filter_training_officer'),
          'Training Officer',
          choices = functions$BuildNamedVector(
            df = rdfs$firefighter,
            name = full_name,
            value = id,
            # Trainer filter is all those who can lead trainings or have in the past.
            filterExpr = id %in% rdfs$training$trainer |
              id %in% rdfs$firefighter$trainer),
          selected = functions$BuildNamedVector(
            df = rdfs$firefighter,
            name = full_name,
            value = id,
            filterExpr = id %in% rdfs$training$trainer |
              id %in% rdfs$firefighter$trainer),
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        )
      })

      ##### GLOBAL #####
      ns <- session$ns

      ##### Trainings #####
      # This is everything that's visible in the table after filters are applied.
      # Use it to generate View_Trainings and grab inputs from view_trainings.
      displayedTrainings <- reactive({
        log_info("Rendering dispayedTrainings reactive.", name = "Training")
        Table_Data <- rdfs$training |>
          mutate(
            date = functions$ConvertToLocalPosix(
              dt = start_time,
              input = 'datetime',
              output = 'date'),
            start_time = functions$FormatLocal(
              dt = start_time,
              input = 'datetime',
              output = 'time',
              seconds = FALSE),
            end_time = functions$FormatLocal(
              dt = end_time,
              input = 'datetime',
              output = 'time',
              seconds = FALSE)
          ) |>
          # Bring in firefighter and training classification data
          left_join(app_data$Training_Classifcaion |>
                      select(-is_active),
                    by = c("classification_id" = 'id')) |>
          left_join(rdfs$firefighter |>
                      select(full_name, id),
                    by = c("trainer" = 'id')) |>
          filter(
            date >= input$training_filter_range[1] &
              date <= input$training_filter_range[2] &
              training_category %in% input$filter_training_category &
              is.na(is_deleted) &
              # If input$filter_training_officer is not initialized (accordion
              # hasn't been expanded), then use all training officers.
              #FIXME This is not filtering correctly.
              trainer %in% (input$filter_training_officer %||% rdfs$firefighter$id)
          ) |>
          select(id, training_category, training_topic, training_description,
                 full_name, date, start_time, end_time) |>
          rename(trainer = full_name)

        # Clean up names, remove ID column, and set row names to PK.
        Table_Data <- functions$FixColNames(Table_Data)
        colnames(Table_Data) <- gsub("Training ", "", colnames(Table_Data))
        rownames(Table_Data) <- Table_Data$Id
        Table_Data$Id <- NULL

        log_info(glue::glue("Rendering dispayedTrainings reactive complete with {nrow(Table_Data)} rows."),
                 namespace = "Training")
        return(Table_Data)
      })

      # Display current training data
      output$view_trainings <- renderDT({
        log_info("Rendering view_trainings table", namespace = "Training")
        Table_Data <- displayedTrainings()

        sort_col <- which(names(Table_Data) == functions$GetSetting('training', key = 'sort_col'))

        DT::datatable(
          Table_Data,
          selection = 'single',
          rownames = FALSE,
          height = "100%",
          options = list(
            lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')),
            pageLength = functions$GetSetting('training', key = 'default_length'),
            columnDefs = list(
              list(className = 'dt-center', targets = "_all")
            ),
            order = list(list(sort_col - 1, functions$GetSetting('training', key = 'sort_order'))),
            scrollX = TRUE
          )
        ) |>
          formatDate(columns = c("Date"), method = "toLocaleDateString")
      })

      ##### ADD TRAINING #####

      # Enter information to create the training.
      observeEvent(input$add_training, {
        log_info("Adding Training", namespace = "Add Training")
        showModal(
          modals$trainingModal(ns,
                               edit = FALSE,
                               training_date = app_data$local_date,
                               start_time = functions$GetSetting('training', key = 'default_start_time'),
                               end_time = functions$GetSetting('training', key = 'default_end_time'),
                               type_choices = app_data$training_category_active,
                               type = NULL,
                               topic = NULL,
                               training_description = NULL,
                               trainers = functions$BuildNamedVector(
                                 df = rdfs$firefighter,
                                 name = full_name,
                                 value = id,
                                 filterExpr = trainer == TRUE & is_active == TRUE),
                               trainer = NULL
          )
        )
      })

      # Update the add training topic based on the training type.
      observe({
        req(input$training_category)
        x <- input$training_category

        log_info("Updating training topic based on training type", namespace = "Add Training")
        log_info(paste0("Training Category: ", input$training_category), namespace = "Add Training")

        topics <- app_data$Training_Classifcaion |>
          filter(training_category == x) |>
          select(training_topic) |>
          pull() |>
          unique() |>
          sort()

        if(length(topics) == 0) {
          log_info(glue::glue("No topics found for training category {input$training_catgory}"),
                              namespace = "Add Training")

          log_info("Setting training topic to NULL", namespace = "Add Training")
          updateSelectInput(session, "training_topic", choices = c('No Topics Found'))

          log_info("Disabling training topic input", namespace = "Add Training")
          session$sendCustomMessage("disableInput", ns('training_topic'))

        } else {
          log_info(glue::glue("Topics found for training category {input$training_category}"),
                   namespace = "Add Training")

          log_info("Setting training topics", namespace = "Add Training")
          topics |>
            (\(x) updateSelectInput(session, "training_topic", choices = x))()

          log_info("Enabling training topic input", namespace = "Add Training")
          session$sendCustomMessage("enableInput", ns('training_topic'))
        }

      }) |>
        bindEvent(input$training_category, input$add_training, ignoreInit = TRUE)


      observe({
        log_info("Updating credit hours", namespace = "Add Training")

        updateNumericInput(session,
                           "credit_hours",
                           value = difftime(
                             input$end_time,
                             input$start_time,
                             units = "hours") |>
                             as.numeric()
                           )
      }) |>
        bindEvent(input$start_time, input$end_time, ignoreInit = TRUE)


      # # Create the training
      observeEvent(input$submit_add_training, {
        removeModal()
        log_info("Submitting add training", namespace = "Add Training")
        # browser()

        local_start_time <- functions$BuildDateTime(input$start_time, input$training_date, 'local', 'local')
        local_end_time <- functions$BuildDateTime(input$end_time, input$training_date, 'local', 'local')

        #This function will return a character with details if there is an overlap.
        overlap_status <- functions$CheckTrainingsOverlap(
          start_time = local_start_time,
          end_time = local_end_time,
          df = rdfs$training)

        if(is.character(overlap_status)) {
          log_warn(glue::glue("Training times overlap with existing training. {input$start_time} - {input$end_time}"),
                   namespace = "Add Training")
          log_warn(glue::glue("{overlap_status}"), namespace = "Add Training")
          showModal(
            modals$warningModal(glue::glue("{overlap_status}. Please select a different time.
                                           Trainings cannot overlap including early check in and late check out periods."))
          )
          return()
        }



        # Use sqlInterpolate
        # sql_command <- "INSERT INTO ?training_table
        # (classification_id, training_description,
        # start_time, end_time,
        # credit_hours, trainer, is_deleted) VALUES
        # (?classification_id, ?training_description,
        # ?start_time, ?end_time, ?credit_hours, ?trainer, NULL);"
        #
        # safe_sql <- sqlInterpolate(
        #   app_data$CON,
        #   sql_command,
        #   training_table = SQL('training'),
        #   training_type = input$training_type,
        #   topic = input$topic,
        #   training_description = input$training_description,
        #   start_time = utc_start_time |> format("%Y-%m-%d %H:%M:%S", tz = 'UTC', usetz = FALSE),
        #   end_time = utc_end_time |> format("%Y-%m-%d %H:%M:%S", tz = 'UTC', usetz = FALSE),
        #   trainer = input$trainer
        # )
        #
        # # Execute the safely interpolated SQL command
        # write_result <- dbExecute(app_data$CON, safe_sql)
        #
        # # FIXME Abstract
        # if(write_result == 1) {
        #   showModal(
        #     modalDialog(
        #       title = "Success",
        #       "Your training has been successfully added. You may now close this window.",
        #       easyClose = TRUE
        #     )
        #   )
        # } else {
        #   showModal(
        #     modals$errorModal(paste("Training add failed with write result equal to", write_result))
        #   )
        # }
        #
        # updateReactiveTraining()


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
                                 training_trainers = trainers,
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




    }
  )
}
