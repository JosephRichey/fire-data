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

      ##### GLOBAL #####
      ns <- session$ns

      ##### UI DYNAMIC RENDERING #####
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

      ##### TRAINING TABLE #####
      # This is everything that's visible in the table after filters are applied.
      # Use it to generate View_Trainings and grab inputs from view_trainings.
      displayedTrainings <- reactive({
        log_info("Rendering dispayedTrainings reactive.", namespace = "Training")
        Table_Data <- rdfs$training |>
          mutate(
            date = functions$ConvertToLocalPosix(
              dt = start_time,
              input = 'datetime',
              output = 'date'),
            start_time = functions$FormatDateTime(
              dt = start_time,
              input = 'datetime',
              output = 'time',
              target_tz = 'local',
              seconds = FALSE),
            end_time = functions$FormatDateTime(
              dt = end_time,
              input = 'datetime',
              output = 'time',
              target_tz = 'local',
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

        # Grab custom columns order from settings
        col_order <- functions$GetSetting("training", key = "display_cols_order") |>
          strsplit(",\\s*") |>
          unlist()


        Table_Data <- displayedTrainings()|>
          select(all_of(col_order))

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
                               category_choices = app_data$training_category_active,
                               category = NULL,
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

      # Create the training
      observeEvent(input$submit_add_training, {
        removeModal()
        log_info("Submitting add training", namespace = "Add Training")

        local_start_time <- functions$BuildDateTime(input$start_time, input$training_date, 'local', 'local')
        local_end_time <- functions$BuildDateTime(input$end_time, input$training_date, 'local', 'local')

        classification_id <- functions$GetTrainingClassificationId(
          df = app_data$Training_Classifcaion,
          category = input$training_category,
          topic = input$training_topic
        )

        log_info(glue::glue("Classification ID is {classification_id}"), namespace = "Add Training")
        log_info(glue::glue("Start time is {local_start_time}"), namespace = "Add Training")
        log_info(glue::glue("End time is {local_end_time}"), namespace = "Add Training")

        #This function will return a character with details if there is an overlap.
        overlap_status <- functions$CheckTrainingsOverlap(
          startTime = local_start_time,
          endTime = local_end_time,
          df = rdfs$training)

        if(is.character(overlap_status)) {
          log_warn(glue::glue("Training times overlap with existing training. User input:
                              {input$training_date} {input$start_time |> as_hms()} - {input$end_time |> as_hms()}"),
                   namespace = "Add Training")
          log_warn(glue::glue("{overlap_status}"), namespace = "Add Training")

          modals$WarningAlert(paste(overlap_status,"\n Your training will not be added."))

          return()
        }

        log_info('No overlap detected', namespace = "Add Training")

        # Use sqlInterpolate
        sql_command <- "INSERT INTO ?training_table
        (classification_id, training_description,
        start_time, end_time,
        credit_hours, trainer, is_deleted) VALUES
        (?classification_id, ?training_description,
        ?start_time, ?end_time, ?credit_hours, ?trainer, NULL);"

        safe_sql <- sqlInterpolate(
          app_data$CON,
          sql_command,
          training_table = SQL('training'),
          classification_id = classification_id,
          training_description = input$training_description,
          start_time = local_start_time |> functions$FormatDateTime(input = 'datetime', output = 'datetime', target_tz = 'UTC'),
          end_time = local_end_time |> functions$FormatDateTime(input = 'datetime', output = 'datetime', target_tz = 'UTC'),
          credit_hours = input$credit_hours,
          trainer = input$trainer
        )

        log_info(glue::glue("Executing SQL command: {safe_sql}"), namespace = "Add Training")

        # Execute the safely interpolated SQL command
        result <- tryCatch(
          {
            dbExecute(app_data$CON, safe_sql)
          },
          error = function(e) {
            log_error(glue::glue("Database write failed: {e$message}"), namespace = "Add Training")
            NA
          }
        )

        functions$CheckWriteResult(result,
                                   successMessage = "Your training has been successfully added. You may now close this window.",
                                   context = "adding a training",
                                   expectedMin = 1,
                                   expectedMax = 1
                                   )

        functions$UpdateReactives(rdfs, 'training')

      })

      ##### MODIFY TRAINING #####
      # Modify the training
      observe({
        cell_click <- input$view_trainings_rows_selected
        # Make sure a row was clicked
        if (length(cell_click) != 0) {

          Modify_ID <- row.names(displayedTrainings()[cell_click,])

          log_info(glue::glue("Cell click is {cell_click}"), namespace = "Modify Training")
          log_info(glue::glue("Modifying training with ID {Modify_ID}"), namespace = "Modify Training")

          To_Modify <- rdfs$training |>
            filter(id == Modify_ID) |>
            mutate(
              date = functions$ConvertToLocalPosix(
                dt = start_time,
                input = 'datetime',
                output = 'date'),
              start_time = functions$FormatDateTime(
                dt = start_time,
                input = 'datetime',
                output = 'time',
                target_tz = 'local',
                seconds = TRUE),
              end_time = functions$FormatDateTime(
                dt = end_time,
                input = 'datetime',
                output = 'time',
                target_tz = 'local',
                seconds = TRUE)
            ) |>
            left_join(app_data$Training_Classifcaion |>
                      select(-is_active),
                    by = c("classification_id" = 'id'))

          log_info(glue::glue("Launching modal for modify training"), namespace = "Modify Training")

          showModal(
            modals$trainingModal(ns,
                                 edit = TRUE,
                                 training_date = To_Modify$date,
                                 start_time = To_Modify$start_time,
                                 end_time = To_Modify$end_time,
                                 category_choices = app_data$training_category_active,
                                 category = To_Modify$training_category,
                                 topic = To_Modify$training_topic,
                                 training_description = To_Modify$training_description,
                                 trainers = functions$BuildNamedVector(
                                   df = rdfs$firefighter,
                                   name = full_name,
                                   value = id,
                                   filterExpr = trainer == TRUE & is_active == TRUE),
                                 trainer = To_Modify$trainer
            )
          )

        } else {
          modals$WarningAlert("Please select a training to modify.")
        }
      }) |>
        bindEvent(input$modify_training)


      observeEvent(input$submit_edit_training, {
        # FUTURE This could be refactored so there's not so much duplication
        # between add and edit
        removeModal()

        log_info("Submitting training modifications", namespace = "Modify Training")

        Modify_ID <- row.names(displayedTrainings()[input$view_trainings_rows_selected,])

        local_start_time <- functions$BuildDateTime(input$start_time, input$training_date, 'local', 'local')
        local_end_time <- functions$BuildDateTime(input$end_time, input$training_date, 'local', 'local')

        classification_id <- functions$GetTrainingClassificationId(
          df = app_data$Training_Classifcaion,
          category = input$training_category,
          topic = input$training_topic
        )

        log_info(glue::glue("Modified classification ID is {classification_id}"), namespace = "Modify Training")
        log_info(glue::glue("Modified start time is {local_start_time}"), namespace = "Modify Training")
        log_info(glue::glue("Modified end time is {local_end_time}"), namespace = "Modify Training")

        #This function will return a character with details if there is an overlap.
        overlap_status <- functions$CheckTrainingsOverlap(
          startTime = local_start_time,
          endTime = local_end_time,
          df = rdfs$training,
          editTrainingId = Modify_ID)

        if(is.character(overlap_status)) {
          log_warn(glue::glue("Training times overlap with existing training. User input:
                              {input$training_date} {input$start_time |> as_hms()} - {input$end_time |> as_hms()}"),
                   namespace = "Modify Training")
          log_warn(glue::glue("{overlap_status}"), namespace = "Modify Training")

          modals$WarningAlert(paste(overlap_status,"\n Your training will not be modified."))

          return()
        }

        log_info('No overlap detected', namespace = "Modify Training")

        # Use sqlInterpolate
        sql_command <- "UPDATE ?training_table
        SET classification_id = ?classification_id,
        training_description = ?training_description,
        start_time = ?start_time,
        end_time = ?end_time,
        credit_hours = ?credit_hours,
        trainer = ?trainer
        WHERE id = ?training_id;"

        safe_sql <- sqlInterpolate(
          app_data$CON,
          sql_command,
          training_table = SQL('training'),
          classification_id = classification_id,
          training_description = input$training_description,
          start_time = local_start_time |> functions$FormatDateTime(input = 'datetime', output = 'datetime', target_tz = 'UTC'),
          end_time = local_end_time |> functions$FormatDateTime(input = 'datetime', output = 'datetime', target_tz = 'UTC'),
          credit_hours = input$credit_hours,
          trainer = input$trainer,
          training_id = Modify_ID
        )

        log_info(glue::glue("Executing SQL command: {safe_sql}"), namespace = "Modify Training")

        # Execute the safely interpolated SQL command
        result <- tryCatch(
          {
            dbExecute(app_data$CON, safe_sql)
          },
          error = function(e) {
            log_error(glue::glue("Database write failed: {e$message}"), namespace = "Modify Training")
            NA
          }
        )

        functions$CheckWriteResult(result,
                                   successMessage = "Your training has been successfully modified. You may now close this window.",
                                   context = "modifiying a training",
                                   expectedMin = 1,
                                   expectedMax = 1
        )

        functions$UpdateReactives(rdfs, 'training')

      })

      ##### DELETE TRAINING #####
      observeEvent(input$delete_training, {

        cell_click <- input$view_trainings_rows_selected
        # Make sure a row was clicked
        if (length(cell_click) != 0) {

          log_info("Requesting to Delete Training", namespace = "Delete Training")
          delete_id <- row.names(displayedTrainings()[cell_click,])

          log_info(glue::glue("Cell click is {cell_click}"), namespace = "Delete Training")
          log_info(glue::glue("Confirming deletion request with ID {delete_id}"), namespace = "Delete Training")

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
          modals$WarningAlert("Please select a training to delete.")
        }
      })

      observe({
        removeModal()

        log_info("Confirmed. Deleting training", namespace = "Delete Training")

        delete_id <- row.names(displayedTrainings()[input$view_trainings_rows_selected,])

        sql_command <- paste0(
          "UPDATE training SET is_deleted = NOW() WHERE id = ", delete_id, ";"
        )

        # Execute the SQL command
        result <- tryCatch(
          {
            dbExecute(app_data$CON, sql_command)
          },
          error = function(e) {
            log_error(glue::glue("Database write failed: {e$message}"), namespace = "Delete Training")
            NA
          }
        )

        functions$CheckWriteResult(result,
                                   successMessage = "Your training has been successfully deleted. You may now close this window.",
                                   context = "deleting a training",
                                   expectedMin = 1,
                                   expectedMax = 1
        )

        functions$UpdateReactives(rdfs, 'training')


      }) |>
        bindEvent(input$confirm_deletion)

      ##### Observers #####

      # Update the add training topic based on the training category.
      observe({
        req(input$training_category)
        x <- input$training_category

        log_info("Updating training topic based on training category", namespace = "Training Observer")
        log_info(paste0("Training Category: ", input$training_category), namespace = "Training Observer")

        topics <- app_data$Training_Classifcaion |>
          filter(training_category == x) |>
          select(training_topic) |>
          pull() |>
          unique() |>
          sort()

        if(length(topics) == 0) {
          log_info(glue::glue("No topics found for training category {input$training_catgory}"),
                   namespace = "Training Observer")

          log_info("Setting training topic to NULL", namespace = "Training Observer")
          updateSelectInput(session, "training_topic", choices = c('No Topics Found'))

          log_info("Disabling training topic input", namespace = "Training Observer")
          session$sendCustomMessage("disableInput", ns('training_topic'))

        } else {
          log_info(glue::glue("Topics found for training category {input$training_category}"),
                   namespace = "Training Observer")

          log_info("Setting training topics", namespace = "Training Observer")
          topics |>
            (\(x) updateSelectInput(session, "training_topic", choices = x))()

          log_info("Enabling training topic input", namespace = "Training Observer")
          session$sendCustomMessage("enableInput", ns('training_topic'))
        }

      }) |>
        bindEvent(input$training_category, input$add_training, input$modify_training, ignoreInit = FALSE)


      observe({
        log_info("Updating credit hours", namespace = "Training Observer")

        req(input$start_time, input$end_time)

        updateNumericInput(session,
                           "credit_hours",
                           value = difftime(
                             input$end_time,
                             input$start_time,
                             units = "hours") |>
                             as.numeric()
        )
      }) |>
        bindEvent(input$start_time, input$end_time, input$add_training, input$modify_training, ignoreInit = TRUE)


    }
  )
}
