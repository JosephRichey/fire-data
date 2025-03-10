box::use(
  shiny[...],
  dplyr[...],
  shinyWidgets[...],
  DT[...],
  shinyalert[...],
  logger[...],
  bslib[...],
  glue[...],
)

box::use(
  ../logic/app_data,
  ../logic/functions,
)


Checks_UI <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(
      ns('type'),
      'Equipment Type',
      choices = c(app_data$Equipment_Type$equipment_type |> unique()),
      selected = c(app_data$Equipment_Type$equipment_type |> unique()),
      options = list(
        `actions-box` = TRUE,
        #FIXME set with .csv settings
        # `live-search` = TRUE,
        size = 10
      ),
      multiple = TRUE
    ),
    checkboxGroupInput(
      ns('due_filter'),
      label = '',
      choices = c('Due', 'Approaching', 'Normal', 'Snooze'),
      #FIXME set with .csv settings
      selected = c('Due', 'Approaching'),
      inline = TRUE
    ),
    hr(),
    br(),
    actionButton(
      ns('check_all'),
      'Check All',
      icon = icon('check'),
      class = 'btn-warning',
      width = '100%'
    ),

    actionButton(
      ns('refresh'),
      'Refresh',
      icon = icon('refresh'),
      class = 'btn-light',
      width = '100%'
    ),
  )
}

Checks_Output <- function(id) {
  ns <- NS(id)
  tagList(
    card(
    fill = FALSE,
    card_body(
        fillable = FALSE,
          DT::dataTableOutput(ns('equipment'))
      )
    )
  )
}

Expiration_UI <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(
      ns('expire_type'),
      'Equipment Type',
      choices = c(app_data$Equipment_Type$equipment_type |> unique()),
      selected = c(app_data$Equipment_Type$equipment_type |> unique()),
      options = list(
        `actions-box` = TRUE,
        #FIXME set with .csv settings
        # `live-search` = TRUE,
        size = 10
      ),
      multiple = TRUE
    ),
    checkboxGroupInput(
      ns('expire_due_filter'),
      label = '',
      choices = c('Due', 'Approaching', 'Normal'),
      #FIXME set with .csv settings
      selected = c('Due', 'Approaching'),
      inline = TRUE
    ),
    hr(),
    br(),
    actionButton(
      ns('renew'),
      'Renew',
      icon = icon('check'),
      class = 'btn-secondary',
      width = '100%'
    ),

    actionButton(
      ns('expire_refresh'),
      'Refresh',
      icon = icon('rotate-right'),
      class = 'btn-light',
      width = '100%'
    ),
  )
}

Expiration_Output <- function(id) {
  ns <- NS(id)
  tagList(
    card(
    fill = FALSE,
    card_body(
        fillable = FALSE,
          DT::dataTableOutput(ns('expiration'))
      )
    )
  )
}

Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      Base_Equipment_Data <- reactiveVal({
        # browser()
        app_data$Base_Equipment_Data
      })

      With_Icons <- reactive({
        # browser()
        Base_Equipment_Data() |>
          mutate(check_threshold = functions$GenerateThreshold(next_check_date, check_lead_time, check_lead_time_unit)) |>
          select(equipment_id, equipment_name, equipment_type,
                 full_name, next_check_date,
                 snooze_expires,  check_threshold) |>
          mutate(flag_type = case_when(
            snooze_expires >= app_data$Current_Local_Date ~ "Snooze",
            next_check_date <= app_data$Current_Local_Date ~ "Due",
            check_threshold <= app_data$Current_Local_Date ~ "Approaching",
            TRUE ~ 'Normal'),
            icon = case_when(
              flag_type == "Due" ~ bsicons::bs_icon("exclamation-triangle", fill = 'red'),
              flag_type == "Approaching" ~ bsicons::bs_icon("exclamation-triangle", fill = 'yellow'),
              flag_type == "Snooze" ~ bsicons::bs_icon("clock", class = "text-info"),
              flag_type == "Normal" ~ bsicons::bs_icon("check-circle-fill", fill = 'green')
            ),
            full_name = if_else(is.na(full_name), "", full_name) |> as.character()
          ) |>
          select(equipment_id, icon, equipment_name,
                 equipment_type, full_name, next_check_date, snooze_expires, flag_type)
      })


      Equipment_Table <- reactive({
        # browser()
        A <- With_Icons() |>
          select(equipment_id, icon, equipment_name, next_check_date,
                 snooze_expires, full_name, equipment_type, flag_type) |>
          filter(equipment_type %in% input$type) |>
          filter(flag_type %in% input$due_filter)

        print(input$due_filter)

        colnames(A) <- c('equipment_id', 'Icon', 'Equipment Name', 'Next Check Date',
                         'Snooze Expires', 'Assigned To', 'Equipment Type', 'Flag Type')

        return(A)
      })


      observe({
        log_info('Refreshing equipment data')
        Base_Equipment_Data(app_data$Base_Equipment_Data)
      }) |>
        bindEvent(input$refresh)


      output$equipment <- renderDataTable({
        datatable(
          Equipment_Table(),
          extensions = 'Buttons',
          options = list(
            columnDefs = list(
              list(
                targets = c(0,6,7),
                visible = FALSE
              )
            ),
            order = list(3, 'asc'),
            dom = 't',  # Removes search bar, pagination, and 'Show entries'
            paging = FALSE,  # Disables pagination
            searching = FALSE  # Disables search
          ),
          escape = FALSE,
          fillContainer = FALSE,
          rownames = FALSE
        )

    })

      observe({
        showModal(
          modalDialog(
            title = 'Confirm Check All',
            'Are you sure you want to check all equipment currently displayed in the table?',
            footer = tagList(
              modalButton('Cancel'),
              actionButton(ns('confirm_check_all'), 'Confirm',
                           class = "btn-warning")
            )
          )
        )
      }) |>
        bindEvent(input$check_all)

      observe({
        removeModal()
        log_trace('Checking all equipment')

        Ids_to_check <- Equipment_Table()[,1]

        # browser()
        New <- Base_Equipment_Data() |>
          mutate(
            #FIXME Needs to check and set back number of appropriate number of days
            #FIXME Also need to investigate interaction of snoozed and checked (i.e., checking resets snooze?)
            next_check_date = if_else(equipment_id %in% Ids_to_check, next_check_date + 365, next_check_date)
          )


        for(x in Equipment_Table()[,3]) {
          # browser()
          showNotification(
            paste(x, 'Checked'),
            duration = 5,
            type = 'message'
          )
        }

        Base_Equipment_Data(New)

      }) |>
        bindEvent(input$confirm_check_all)


      ##### Expiration Tracking #####
      Base_Equipment_Data <- reactiveVal({
        # browser()
        app_data$Base_Equipment_Data
      })

      Expiration_With_Icons <- reactive({
        # browser()
        Base_Equipment_Data() |>
          mutate(warning_threshold = functions$GenerateThreshold(expiration_date, expire_lead_time, expire_lead_time_unit)) |>
          select(equipment_id, equipment_name, equipment_type,
                 full_name, expiration_date,
                 snooze_expires,  warning_threshold) |>
          mutate(flag_type = case_when(
            expiration_date <= app_data$Current_Local_Date ~ "Due",
            warning_threshold <= app_data$Current_Local_Date ~ "Approaching",
            TRUE ~ 'Normal'),
            icon = case_when(
              flag_type == "Due" ~ bsicons::bs_icon("exclamation-triangle", fill = 'red'),
              flag_type == "Approaching" ~ bsicons::bs_icon("exclamation-triangle", fill = 'yellow'),
              flag_type == "Normal" ~ bsicons::bs_icon("check-circle-fill", fill = 'green')
            ),
            full_name = if_else(is.na(full_name), "", full_name) |> as.character()
          ) |>
          select(equipment_id, icon, equipment_name,
                 equipment_type, full_name, expiration_date, snooze_expires, flag_type)
      })


      Expiration_Table <- reactive({
        # browser()
        A <- Expiration_With_Icons() |>
          select(equipment_id, icon, equipment_name, expiration_date,
                 full_name, equipment_type, flag_type) |>
          filter(equipment_type %in% input$expire_type) |>
          filter(flag_type %in% input$expire_due_filter)


        colnames(A) <- c('equipment_id', 'Icon', 'Equipment Name', 'Expiration Date',
                         'Assigned To', 'Equipment Type', 'Flag Type')

        return(A)
      })


      observe({
        log_info('Refreshing expiration data')
        Base_Equipment_Data(app_data$Base_Equipment_Data)
      }) |>
        bindEvent(input$expire_refresh)


      output$expiration <- renderDataTable({
        datatable(
          Expiration_Table(),
          extensions = 'Buttons',
          options = list(
            columnDefs = list(
              list(
                targets = c(0,5,6),
                visible = FALSE
              )
            ),
            order = list(3, 'asc'),
            dom = 't',  # Removes search bar, pagination, and 'Show entries'
            paging = FALSE,  # Disables pagination
            searching = FALSE  # Disables search
          ),
          escape = FALSE,
          fillContainer = FALSE,
          rownames = FALSE
        )

      })

      observe({

        log_trace('Checking to see if equipment expiration can be updated')

        if(is.null(input$expiration_rows_selected)) {
          shinyalert(
            title = 'Error',
            type = 'error',
            text = 'Please select equipment to modify expiration date.'
          )
          return()
        }

        # browser()

        # Take the top expiration date suggestion when multiple rows are selected.
        renew_ids <- Expiration_Table()[input$expiration_rows_selected,1]
        renew_types <- app_data$Equipment |>
          filter(equipment_id %in% renew_ids) |>
          select(equipment_type_id) |>
          pull()

        dates <- app_data$Equipment_Type |>
          filter(equipment_type_id %in% renew_types) |>
          arrange(desc(expire_time)) |>
          slice_head(n = 1)

        recommended_expiration <- functions$GenerateThreshold(app_data$Current_Local_Date, dates$expire_time, dates$expire_time_unit, TRUE) |>
          as.Date()


        showModal(
          modalDialog(
            title = 'Select New Expiration Date',

            'Please select the new expiration date for the selected equipment.',
            dateInput(
              ns('new_expiration_date'),
              'New Expiration Date',
              min = app_data$Current_Local_Date,
              value = recommended_expiration
              ),

            footer = tagList(
              modalButton('Cancel'),
              actionButton(ns('confirm_expiration_update'), 'Confirm',
                           class = "btn-primary")
            )
          )
        )
      }) |>
        bindEvent(input$renew)

      observe({
        removeModal()
        log_trace('Update expiration date')

        renew_ids <- Expiration_Table()[input$expiration_rows_selected,1]

        # browser()
        New <- Base_Equipment_Data() |>
          mutate(
            #FIXME Needs to check and set back number of appropriate number of days
            #FIXME Also need to investigate interaction of snoozed and checked (i.e., checking resets snooze?)
            expiration_date = if_else(equipment_id %in% renew_ids, input$new_expiration_date, expiration_date)
          )


        for(x in Expiration_Table()[input$expiration_rows_selected,3]) {
          # browser()
          showNotification(
            paste(x, 'expiration updated to', input$new_expiration_date, 'successfully.'),
            duration = 5,
            type = 'message'
          )
        }

        Base_Equipment_Data(New)

      }) |>
        bindEvent(input$confirm_expiration_update)


  }
  )
}

Manage_Equipment_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      ns('add_equipment'),
      'Add New Equipment',
      icon = icon('plus'),
      class = 'btn-primary',
      width = '100%'
    ),
    actionButton(
      ns('edit_equipment'),
      'Edit Equipment',
      icon = icon('pencil'),
      class = 'btn-secondary',
      width = '100%'
    ),
    actionButton(
      ns('delete_equipment'),
      'Delete Equipment',
      icon = icon('trash'),
      class = 'btn-warning',
      width = '100%'
    ),
    pickerInput(
      ns('manage_equipment_type'),
      'Equipment Type',
      choices = c(app_data$Equipment_Type$equipment_type |> unique()),
      selected = c(app_data$Equipment_Type$equipment_type |> unique()),
      options = list(
        `actions-box` = TRUE,
        #FIXME set with .csv settings
        # `live-search` = TRUE,
        size = 10
      ),
      multiple = TRUE
    )

  )
}

Manage_Equipment_Output <- function(id) {
  ns <- NS(id)
  tagList(
    strong(helpText("A(n) [equipment type] needs to be checked every [number] [time interval]. [Equipment piece] is an [equipment type].")),
    h5(strong(helpText("Example: An apparatus needs to be checked every 3 months. Engine 82 is an apparatus."))),
    card(
      title = 'Equipment Pieces',
    fill = FALSE,
    card_body(
        fillable = FALSE,
        DT::dataTableOutput(ns('equipment_pieces')),
      )
    )
  )
}

Manage_Equipment_Type_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      ns('add_type'),
      'Add New Equipment Type',
      icon = icon('plus'),
      class = 'btn-primary',
      width = '100%'
    ),
    actionButton(
      ns('edit_type'),
      'Edit Equipment Type',
      icon = icon('pencil'),
      class = 'btn-secondary',
      width = '100%'
    ),
    actionButton(
      ns('delete_equipment_type'),
      'Delete Equipment Type',
      icon = icon('trash'),
      class = 'btn-warning',
      width = '100%'
    ),

  )
}

Manage_Equipment_Type_Output <- function(id) {
  ns <- NS(id)
  tagList(
    strong(helpText("A(n) [equipment type] needs to be checked every [number] [time interval]. [Equipment piece] is an [equipment type].")),
    h5(strong(helpText("Example: An apparatus needs to be checked every 3 months. Engine 82 is an apparatus."))),
    card(
      title = 'Equipment Types',
      fill = FALSE,
      card_body(
        fillable = FALSE,
        DT::dataTableOutput(ns('equipment_types'))
      )
    )
  )
}

Manage_Equipment_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      Base_Equipment_Data <- reactiveVal({
        # browser()
        app_data$Base_Equipment_Data
      })

      Visible_Equipment_Table <- reactive({
        # browser()
        df <- Base_Equipment_Data() |>
          filter(is.na(expire_equipment)) |>
          filter(equipment_type %in% input$manage_equipment_type) |>
          select(equipment_id, equipment_name, equipment_type,
                 full_name, expiration_date, next_check_date, snooze_expires)

        colnames(df) <- c('equipment_id', 'Equipment Name', 'Equipment Type',
                          'Assigned To', 'Expiration Date', 'Next Check Date', 'Snooze Expires')

        return(df)
      })

      output$equipment_pieces <- DT::renderDataTable({
        datatable(
          Visible_Equipment_Table(),
          escape = FALSE,
          rownames = FALSE,
          extensions = 'Buttons',
          options = list(
            columnDefs = list(
              list(
                targets = c(0),
                visible = FALSE
              )
            ),
            order = list(1, 'asc'),
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
          )

        )
      })

      observe({
        # browser()
        log_info('Adding new equipment')
        showModal(
          modalDialog(
            title = 'Add New Equipment',
            textInput(
              ns('new_equipment_name'),
              'Equipment Name',
              placeholder = 'Enter equipment name'
            ),
            selectInput(
              ns('new_equipment_type'),
              'Equipment Type',
              #FIXME Not reactive
              choices = c(r_Equipment_Type() |>
                            filter(is.na(equipment_type_expire)) |>
                            pull(equipment_type))
            ),
            selectInput(
              ns('new_assigned_to'),
              'Assigned To',
              choices = c('None', app_data$Firefighter |>
                            filter(active_status == 1) |>
                            pull(full_name))
            ),
            dateInput(
              ns('new_expiration_date'),
              'Expiration Date',
              min = app_data$Current_Local_Date,
              value = app_data$Current_Local_Date
            ),
            footer = tagList(
              modalButton('Cancel'),
              actionButton(ns('confirm_add_equipment'), 'Confirm',
                           class = "btn-primary")
            )
          )
        )

      }) |>
        bindEvent(input$add_equipment)

      # Set Expiration date to auto rec.
      observe({
        # browser()
        tmp <- r_Equipment_Type() |>
          filter(equipment_type == input$new_equipment_type) |>
          select(expire_time, expire_time_unit)

        expire_date <- functions$GenerateThreshold(app_data$Current_Local_Date,
                                                   tmp$expire_time,
                                                   tmp$expire_time_unit,
                                                   TRUE)


        updateDateInput(session,
                        'new_expiration_date',
                        value = expire_date)
      }) |>
        bindEvent(input$new_equipment_type)

      observe({
        # browser()
        removeModal()
        log_trace('Adding new equipment')

        New <- Base_Equipment_Data() |>
          add_row(
            equipment_id = max(Base_Equipment_Data()$equipment_id) + 1,
            equipment_name = input$new_equipment_name,
            #FIXME Use function to get equipment type
            equipment_type_id = r_Equipment_Type()[r_Equipment_Type()[,2] == input$new_equipment_type,][1] |> pull(),
            firefighter_id = if(input$new_assigned_to == 'None') NA else app_data$Firefighter[app_data$Firefighter[,2] == input$new_assigned_to,][1] |> pull(),
            next_check_date = app_data$Current_Local_Date + 30,
            expiration_date = if(length(input$new_expiration_date) == 0) as.Date(NA) else input$new_expiration_date,
            snooze_expires = as.Date(NA),
            expire_equipment = NA_character_,
            equipment_type = input$new_equipment_type,
            #FIXME Use function to get assigned to
            full_name = if(input$new_assigned_to == 'None') NA_character_ else input$new_assigned_to,
            #FIXME have this match whatever the preset is
          )

        # FIXME Add check via logs to see if the new equipment is being added correctly

        Base_Equipment_Data(New)

        showNotification(
          paste(input$new_equipment_name, 'added successfully.'),
          duration = 5,
          type = 'message'
        )

      }) |>
        bindEvent(input$confirm_add_equipment)

      observe({

        log_info('Editing equipment')

        if(is.null(input$equipment_pieces_rows_selected)) {
          shinyalert(
            title = 'Error',
            type = 'error',
            text = 'Please select equipment to edit.'
          )
          return()
        }

        if(length(input$equipment_pieces_rows_selected) > 1) {
          shinyalert(
            title = 'Error',
            type = 'error',
            text = 'Please select only one piece of equipment to edit.'
          )
          return()
        }

        # browser()

        edit_ids <- Visible_Equipment_Table()[input$equipment_pieces_rows_selected,1]

        edit_data <- Base_Equipment_Data() |>
          filter(equipment_id %in% edit_ids)

        showModal(
          modalDialog(
            title = 'Edit Equipment',
            textInput(
              ns('edit_equipment_name'),
              'Equipment Name',
              value = edit_data$equipment_name
            ),
            selectInput(
              ns('edit_equipment_type'),
              'Equipment Type',
              choices = c(r_Equipment_Type() |>
                            filter(is.na(equipment_type_expire)) |>
                            pull(equipment_type),
              selected = edit_data$equipment_type)
            ),
            selectInput(
              ns('edit_assigned_to'),
              'Assigned To',
              choices = c('None', app_data$Firefighter |>
                            filter(active_status == 1) |>
                            pull(full_name),
              selected = edit_data$full_name)
            ),
            dateInput(
              ns('edit_expiration_date'),
              'Expiration Date',
              min = app_data$Current_Local_Date,
              value = edit_data$expiration_date
            ),
            footer = tagList(
              modalButton('Cancel'),
              actionButton(ns('confirm_edit_equipment'), 'Confirm',
                           class = "btn-primary")
            )
          )
        )

      }) |>
        bindEvent(input$edit_equipment)

      observe({
        # browser()
        removeModal()
        log_trace('Editing equipment')

        edit_ids <- Visible_Equipment_Table()[input$equipment_pieces_rows_selected,1]

        New <- Base_Equipment_Data() |>
          filter(!equipment_id %in% edit_ids) |>
          add_row(
            equipment_id = edit_ids,
            equipment_name = input$edit_equipment_name,
            #FIXME Use function to get equipment type
            equipment_type_id = r_Equipment_Type()[r_Equipment_Type()[,2] == input$edit_equipment_type,][1] |> pull(),
            firefighter_id = if(input$edit_assigned_to == 'None') NA else app_data$Firefighter[app_data$Firefighter[,2] == input$edit_assigned_to,][1] |> pull(),
            next_check_date = app_data$Current_Local_Date + 30,
            expiration_date = if(length(input$edit_expiration_date) == 0) NA else input$edit_expiration_date,
            snooze_expires = NA,
            expire_equipment = NA,
            equipment_type = input$edit_equipment_type,
            #FIXME Use function to get assigned to
            full_name = if(input$edit_assigned_to == 'None') NA else input$edit_assigned_to,
            #FIXME have this match whatever the preset is
          )

        Base_Equipment_Data(New)

        showNotification(
          paste(input$edit_equipment_name, 'edited successfully.'),
          duration = 5,
          type = 'message'
        )

      }) |>
        bindEvent(input$confirm_edit_equipment)

      observe({

        log_info('Deleting equipment')

        if(is.null(input$equipment_pieces_rows_selected)) {
          shinyalert(
            title = 'Error',
            type = 'error',
            text = 'Please select equipment to delete.'
          )
          return()
        }

        showModal(
          modalDialog(
            title = 'Confirm Delete',
            'Are you sure you want to delete the selected equipment?',
            br(),
            Visible_Equipment_Table()[input$equipment_pieces_rows_selected,2] |> paste(collapse = ', '),
            footer = tagList(
              modalButton('Cancel'),
              actionButton(ns('confirm_delete_equipment'), 'Confirm',
                           class = "btn-warning")
            )
          )
        )

      }) |>
        bindEvent(input$delete_equipment)

      observe({

        # removeModal()

        # browser()

        delete_ids <- Visible_Equipment_Table()[input$equipment_pieces_rows_selected,1]

        New <- Base_Equipment_Data() |>
          filter(!equipment_id %in% delete_ids)


        for(x in Visible_Equipment_Table()[input$equipment_pieces_rows_selected,2]) {
          showNotification(
            paste(x, 'deleted successfully.'),
            duration = 5,
            type = 'message'
          )
        }

        Base_Equipment_Data(New)
      }) |>
        bindEvent(input$confirm_delete_equipment)

      r_Equipment_Type <- reactiveVal({
        # browser()
        app_data$Equipment_Type
      })


      Visible_Equipment_Types <- reactive({
        # browser()
        df <- r_Equipment_Type() |>
          filter(is.na(equipment_type_expire)) |>
          mutate(
            check = glue("Check every {check_time} {check_time_unit}(s), with {check_lead_time} {check_lead_time_unit}(s) warning."),
            expire = glue("Expire every {expire_time} {expire_time_unit}(s), with {expire_lead_time} {expire_lead_time_unit}(s) warning.")
          ) |>
          select(equipment_type_id, equipment_type, check, expire)

        colnames(df) <- c('equipment_type_id','Equipment Type', 'Checks', 'Expiration')

        return(df)
      })

      output$equipment_types <- DT::renderDataTable({
        datatable(
          Visible_Equipment_Types(),
          escape = FALSE,
          rownames = FALSE,
          extensions = 'Buttons',
          options = list(
            columnDefs = list(
              list(
                targets = c(0),
                visible = FALSE
              )
            ),
            order = list(1, 'asc'),
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
          )
        )
      })

      observe({
        # browser()
        showModal(
          modalDialog(
            title = 'Add New Equipment Type',
            textInput(
              ns('add_new_equipment_type'),
              'Equipment Type',
              width = '100%',
              placeholder = 'Enter equipment type'
            ),
            layout_column_wrap(
              width = 1/2,
              numericInput(
                ns('add_new_check_time'),
                'Check Every',
                value = 1,
                min = 1
              ),
              div(
                style = "margin-top: 8px;",
                selectInput(
                  ns('add_new_check_time_unit'),
                  ' ',
                  choices = c('Day', 'Month', 'Year'),
                  selected = 'Month'
                )
              ),
              numericInput(
                ns('add_new_check_lead_time'),
                'with a warning of',
                value = 1,
                min = 1
              ),
              div(
                style = "margin-top: 8px;",
                selectInput(
                  ns('add_new_check_lead_time_unit'),
                  ' ',
                  choices = c('Day', 'Month', 'Year'),
                  selected = 'Day'
                )
              )
            ),
            hr(),
            layout_column_wrap(
              width = 1/2,
              numericInput(
                ns('add_new_expire_time'),
                'Expire Every',
                value = 1,
                min = 1
              ),
              div(
                style = "margin-top: 8px;",
                selectInput(
                  ns('add_new_expire_time_unit'),
                  ' ',
                  choices = c('Day', 'Month', 'Year'),
                  selected = 'Year'
                )
              ),
              numericInput(
                ns('add_new_expire_lead_time'),
                'with a warning of',
                value = 1,
                min = 1
              ),
              div(
                style = "margin-top: 8px;",
                selectInput(
                  ns('add_new_expire_lead_time_unit'),
                  ' ',
                  choices = c('Day', 'Month', 'Year'),
                  selected = 'Month'
                )
              )
            ),
            footer = tagList(
              modalButton('Cancel'),
              actionButton(ns('confirm_add_type'), 'Confirm',
                            class = 'btn-primary')
            )
          )
        )

      }) |>
        bindEvent(input$add_type)

      observe({
        # browser()
        removeModal()
        log_trace('Adding new equipment type')

        New <- app_data$Equipment_Type |>
          add_row(
            equipment_type_id = max(app_data$Equipment_Type$equipment_type_id) + 1,
            equipment_type = input$add_new_equipment_type,
            check_time = input$add_new_check_time,
            check_time_unit = input$add_new_check_time_unit |> tolower(),
            check_lead_time = input$add_new_check_lead_time,
            check_lead_time_unit = input$add_new_check_lead_time_unit |> tolower(),
            expire_time = input$add_new_expire_time,
            expire_time_unit = input$add_new_expire_time_unit |> tolower(),
            expire_lead_time = input$add_new_expire_lead_time,
            expire_lead_time_unit = input$add_new_expire_lead_time_unit |> tolower()
          )

        r_Equipment_Type(New)

        showNotification(
          paste(input$add_new_equipment_type, 'added successfully.'),
          duration = 5,
          type = 'message'
        )

      }) |>
        bindEvent(input$confirm_add_type)


    }
  )
}
