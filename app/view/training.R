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
)

box::use(
  ../logic/app_data,
  ../logic/functions,
)

training_officers <- app_data$Roster |>
  filter(firefighter_officer == TRUE) |>
  select(firefighter_full_name, firefighter_id) %>%
  # https://ivelasq.rbind.io/blog/understanding-the-r-pipe/
  # See "Getting to the solution" sect
  (\(.) setNames(.$firefighter_id, .$firefighter_full_name))




UI <- function(id) {
  ns <- NS(id)

  tagList(
    actionButton(ns('add_training'), "Add Training"),
    # actionButton(ns('modify_training'), "Modify Training"),
    # actionButton(ns("delete_training"), "Delete Training"),
    accordion(
      open = FALSE,
      accordion_panel(
        title = "Filter Trainings",
        open = FALSE,
        card(
          helpText("Filter by training date"),
          dateRangeInput(ns('training_filter_range'),
                         "Show trainings between:",
                         start = paste0(year(Sys.Date()), "-01-01"),
                         end = paste0(year(Sys.Date()), "-12-31")
                         ),
          helpText("Filter by training type"),
          pickerInput(ns('filter_training_type'),
                      'Training Type',
                      choices = unique(app_data$Training$training_type),
                      selected = unique(app_data$Training$training_type),
                      options = list(`actions-box` = TRUE),
                      multiple = TRUE
          ),
          helpText("Filter by training ffficer"),

          pickerInput(ns('filter_training_officer'),
                      'Training Officer',
                      choices = training_officers,
                      selected = training_officers,
                      options = list(`actions-box` = TRUE),
                      multiple = TRUE
          )
        )
      )
    )
  )
}

Output <- function(id) {
  ns <- NS(id)
  tagList(
    card(DTOutput(ns('view_trainings')))
  )
}


Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      rv <- reactiveVal(app_data$Training)

      ns <- session$ns

      # Display current trainings
      output$view_trainings <- renderDT({
        # browser()
        Table_Data <- rv() |>
          filter(training_date > input$training_filter_range[1] &
                   training_date < input$training_filter_range[2] &
                   training_type %in% input$filter_training_type &
                   is.na(training_delete) &
                   training_officer %in% input$filter_training_officer
                   ) |>
          select(-training_delete) |>
          # Do fancy magic to replace the training officer key with the name.
          mutate(training_officer = names(training_officers)[match(training_officer, training_officers)])

        Table_Data <- functions$FixColNames(Table_Data)

        DT::datatable(
          Table_Data,
          selection = 'single',
          rownames = FALSE,
          options = list(
            lengthMenu = list(c(25, 50, -1), c('25', '50', 'All')),
            pageLength = 25,  # Set the default page length
            columnDefs = list(
              list(className = 'dt-center', targets = "_all"),
              list(visible = FALSE, targets = "Training Id")
            ),
            order = list(list(4, 'desc'))
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
                              "Operations"
                            ))
        }

        else if(x == "Fire") {
          updateSelectInput(session,
                            "add_training_topic",
                            choices = c(
                              "Fire 1",
                              "Fire 2",
                              "Hazmat",
                              "Ops"
                            ))
        } else if (x == "Wildland") {
          updateSelectInput(session,
                            "add_training_topic",
                            choices = c( "Wildland")
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
                              "Airway/Respiraroty/Ventilation",
                              "Cardiovascular",
                              "Trauma",
                              "Medical",
                              "Operations"
                            ),
                            selected = MyReactives$trainings[input$view_trainings_cell_clicked$row,]$topic
          )
        }

        else if(x == "Fire") {
          updateSelectInput(session,
                            "modify_training_topic",
                            choices = c(
                              "Fire 1",
                              "Fire 2",
                              "Hazmat",
                              "Ops"
                            ),
                            selected = MyReactives$trainings[input$view_trainings_cell_clicked$row,]$topic
          )
        } else if (x == "Wildland") {
            updateSelectInput(session,
                              "add_training_topic",
                              choices = c("Wildland")
            )
        }
      })

      # Enter information to create the training.
      observeEvent(input$add_training, {
        showModal(modalDialog(
          title = "Add Training",
          dateInput(ns('add_training_date'), 'Training Date', value = Sys.Date()),
          timeInput(ns('add_start_time'), "Start Time", value = "18:00:00", minute.steps = 5),
          timeInput(ns('add_end_time'), "End Time", value = "20:00:00", minute.steps = 5),
          selectInput(ns('add_training_type'), 'Training Type', choices = c("EMS", "Fire", "Wildland"), selected = "EMS"),
          selectInput(ns('add_training_topic'), 'Training Topic', choices = c()), # Update with above observe statement
          textAreaInput(ns('add_description'), 'Training Description'),
          selectInput(ns('add_training_officer'), 'Training Officer', choices = training_officers),
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

        sql_command <- paste0(
          "INSERT INTO cfddb.training (training_type, training_topic, training_description, training_date, training_start_time, training_end_time, training_officer, training_delete) VALUES ('",
          input$add_training_type, "', '", input$add_training_topic, "', '", input$add_description, "', '", input$add_training_date, "', '", input$add_start_time |> strftime(format = "%T"), "', '", input$add_end_time |> strftime(format = "%T"), "', '", input$add_training_officer, "', NULL);"
        )

        write_result <- DBI::dbExecute(app_data$CON, sql_command)

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

        rv(DBI::dbGetQuery(app_data$CON, "SELECT * FROM cfddb.training
                                      WHERE training_delete IS NULL;"))


      })
      #
      # # Modify the training
      # observeEvent(input$modify_training, {
      #   # browser()
      #   cell_click <- input$view_trainings_cell_clicked
      #   # Make sure a row was clicked
      #   if (length(cell_click) != 0) {
      #     showModal(modalDialog(
      #       selectInput('modify_training_type', 'Training Type', choices = c("EMS", "Fire", "Wildland"), selected = MyReactives$trainings[cell_click$row,]$training_type),
      #       selectInput('modify_training_topic', 'Training Topic', choices = c()),
      #       numericInput('modify_training_length', "Training Length (Hours)", value = MyReactives$trainings[cell_click$row,]$training_length),
      #       textAreaInput('modify_description', 'Training Description', value = MyReactives$trainings[cell_click$row,]$description),
      #       dateInput('modify_training_date', 'Training Date', value = MyReactives$trainings[cell_click$row,]$date),
      #       title = "Modify Training",
      #       footer = tagList(
      #         actionButton("action_modify_training", "Modify Training")
      #       ),
      #       easyClose = TRUE
      #     ))
      #   }
      # })
      #
      #
      # observeEvent(input$action_modify_training, {
      #   # browser()
      #   removeModal()
      #
      #   Trainings <- MyReactives$trainings
      #   showModal(modalDialog("Please wait...", title = "Processing Changes"))
      #
      #   cell_click <- input$view_trainings_cell_clicked
      #   MyReactives$trainings <- rows_update(x = Trainings,
      #                                        y = data.frame(training_id = MyReactives$trainings[cell_click$row,]$training_id,
      #                                                       training_type = input$modify_training_type,
      #                                                       topic = input$modify_training_topic,
      #                                                       training_length = input$modify_training_length,
      #                                                       description = input$modify_description,
      #                                                       date = input$modify_training_date,
      #                                                       delete = FALSE
      #                                        ),
      #                                        by = 'training_id'
      #   )
      #
      #   board %>% pin_write(MyReactives$trainings, "trainings", "rds")
      #   removeModal()
      #   showModal(modalDialog("Your training has been successfully modified.",
      #                         title = "Success!",
      #                         easyClose = TRUE))
      # })
      #
      # observeEvent(input$delete_training, {
      #   # browser()
      #   cell_click <- input$view_trainings_cell_clicked
      #
      #   if (length(cell_click) != 0) {
      #     showModal(modalDialog(
      #       title = "Confirm Deletion",
      #       "Are you sure you want to delete this training? If you're sure, please type \"Delete\"",
      #       textInput("confirm_deletion", ""),
      #       footer = tagList(
      #         actionButton("action_delete_training", "Delete Training")
      #       ),
      #       easyClose = TRUE
      #     ))
      #   }
      # })
      #
      # observeEvent(input$action_delete_training, {
      #   # browser()
      #   removeModal()
      #
      #   if(input$confirm_deletion == "Delete") {
      #     showModal(modalDialog("Please wait...", title = "Processing Changes"))
      #     cell_click <- input$view_trainings_cell_clicked
      #
      #     MyReactives$trainings <- rows_update(x = MyReactives$trainings,
      #                                          y = data.frame(training_id = MyReactives$trainings[cell_click$row,]$training_id,
      #                                                         training_type = MyReactives$trainings[cell_click$row,]$training_type,
      #                                                         topic = MyReactives$trainings[cell_click$row,]$topic,
      #                                                         training_length = MyReactives$trainings[cell_click$row,]$training_length,
      #                                                         description = MyReactives$trainings[cell_click$row,]$description,
      #                                                         date = as.Date(MyReactives$trainings[cell_click$row,]$date),
      #                                                         delete = TRUE
      #                                          ),
      #                                          by = 'training_id'
      #     )
      #
      #     board %>% pin_write(MyReactives$trainings, "trainings", "rds")
      #     removeModal()
      #     showModal(modalDialog("Your training has been successfully deleted.",
      #                           title = "Success!",
      #                           easyClose = TRUE))
      #
      #   } else {
      #     showModal(modalDialog("Records will not be deleted.", title = "Delete Failed"))
      #   }
      #   Trainings <- MyReactives$trainings
      #
      #
      #
      #
      # })

    }
  )
}
