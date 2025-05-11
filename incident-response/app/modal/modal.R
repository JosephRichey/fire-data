box::use(
  shiny[...],
  lubridate[...],
  dplyr[...],
  shinyTime[timeInput],
  bslib[...],
)

box::use(
  ../logic/app_data,
  ../logic/global_functions[GetSetting,
                            BuildNamedVector],
)

key_time <- function(ns, incident_details, edit) {
  
  # In cases when a call ends between 00:00 and 00:59, 
  # the time is set to 23:59 the previous day.
  # Computer the start date as 60 minutes prior to the current time.
  incident_start_date_with_rollback <- Sys.time() |>
    with_tz(GetSetting('global', key = 'ltz')) |>
    floor_date("minute") - 3600
  
  incident_start_date_with_rollback <- as.Date(incident_start_date_with_rollback,
                                               tz = GetSetting('global', key = 'ltz')
                                               )
  
  modalDialog(
    textInput(
      inputId = ns("cad_identifier"),
      label = "Incident ID:",
      value = coalesce(incident_details$cad_identifier, "")
    ),
    dateInput(
      inputId = ns("incident_start_date"),
      label = "Dispatch Date:",
      max = app_data$Current_Local_Date |> as.Date(),
      value = coalesce(incident_details$incident_start_date, 
                       incident_start_date_with_rollback)
    ),
    timeInput(
      inputId = ns("incident_start_time"),
      label = "Dispatch Time:",
      value = coalesce(
        incident_details$incident_start_time,
        Sys.time() |>
          with_tz(Sys.getenv("LOCAL_TZ")) |>
          floor_date("minute") - 3600
      ),
      seconds = GetSetting("incident",
                           group = "incident_response",
                           key = "input_seconds")
    ),
    dateInput(
      inputId = ns("incident_end_date"),
      label = "End Date:",
      max = app_data$Current_Local_Date |> as.Date(),
      value = coalesce(incident_details$incident_end_date, app_data$Current_Local_Date)
    ),
    timeInput(
      inputId = ns("incident_end_time"),
      label = "End Time:",
      value = coalesce(
        incident_details$incident_end_time,
        Sys.time() |>
          with_tz(Sys.getenv("LOCAL_TZ")) |>
          floor_date("minute")
      ),
      seconds = GetSetting("incident",
                           group = "incident_response",
                           key = "input_seconds")
    ),
    footer = tagList(
      modalButton(
        'Cancel'
      ),
      actionButton(
        inputId = ns("to_address_unit"),
        label = "Next",
        class = "btn btn-primary"
      )
    )
  )
}

address_unit <- function(ns, incident_details, edit) {
  # browser()
  modalDialog(
    if (GetSetting("incident",
                   group = "incident_response",
                   key = "address")) {
      textInput(
        inputId = ns("address"),
        label = "Address",
        value = coalesce(incident_details$address, "")
      )
    },
    selectInput(
      inputId = ns("area"),
      label = "Response Area",
      choices = BuildNamedVector(
        app_data$Area,
        name = area,
        value = id,
        filter = is_active == 1
      ),
      selected = coalesce(incident_details$area, "")
    ),
    selectInput(
      inputId = ns("dispatch_reason"),
      label = "Dispatch Reason",
      choices = BuildNamedVector(
        app_data$Dispatch_Code,
        name = dispatch_code,
        value = id,
        filter = is_active == 1
      ),
      selected = coalesce(incident_details$dispatch_reason, "")
    ),
    checkboxGroupInput(
      inputId = ns("units"),
      label = "Units",
      choices = BuildNamedVector(
        app_data$Unit,
        name = unit_type,
        value = id,
        filter = is_active == 1
      ),
      selected = coalesce(incident_details$units, "")
    ),
    if (GetSetting("incident",
                   group = "incident_response",
                   key = "canceled")) {
      tagList(
        hr(style = "solid #000000;"),
        checkboxInput(
          inputId = ns("canceled"),
          label = "Canceled before arrival",
          value = coalesce(incident_details$canceled, FALSE)
        )
      )
    },
    if (GetSetting("incident",
                   group = "incident_response",
                   key = "dropped")) {
      tagList(
        hr(style = "solid #000000;"),
        checkboxInput(
          inputId = ns("dropped"),
          label = "Dropped call",
          value = coalesce(incident_details$dropped, FALSE)
        )
      )
    },
    footer = tagList(
      actionButton(ns("add_incident"), "Back", class = "btn btn-light"),
      actionButton(ns("cancel_modal"), "Cancel", class = "btn btn-warning"),
      if(edit) {
        actionButton(ns('submit'), 'Submit Edits', class = "btn btn-success")
      } else {
        actionButton(ns("to_apparatus_ff"), "Next", class = "btn btn-primary")
      }
    )
  )
}
 

select_ff_aparatus <- function(ns, response_details, additional) {
  # browser()
  modalDialog(
    selectInput(
      inputId = ns("apparatus"),
      label = "Apparatus:",
      choices = app_data$Apparatus |> 
        filter(is_active == 1) |> 
        pull(apparatus_name),
      selected = coalesce(response_details$apparatus, ""),
      multiple = TRUE
    ),
    selectInput(
      inputId = ns("firefighter"),
      label = "Firefighter",
      choices = app_data$Firefighter |> 
        filter(is_active == 1) |> 
        pull(full_name),
      selected = coalesce(response_details$firefighter, ""),
      multiple = TRUE
    ),
    footer = tagList(
      if(additional) {
        actionButton(ns("add_additional_response"), "Back", class = "btn btn-light")
      } else {
        actionButton(ns("to_address_unit"), "Back", class = "btn btn-light")
      },
      actionButton(ns("cancel_modal"), "Cancel", class = "btn btn-warning"),
      actionButton(ns("to_assignment"), "Next", class = "btn btn-primary")
    )
  )
}

key_time_additional <- function(ns, response_details, rdfs) {
  # browser()
  
  # In cases when a call ends between 00:00 and 00:59, 
  # the time is set to 23:59 the previous day.
  # Computer the start date as 60 minutes prior to the current time.
  response_start_date_with_rollback <- Sys.time() |>
    with_tz(GetSetting('global', key = 'ltz')) |>
    floor_date("minute") - 3600
  
  response_start_date_with_rollback <- as.Date(response_start_date_with_rollback,
                                               tz = GetSetting('global', key = 'ltz')
  )
  
  modalDialog(
    dateInput(
      inputId = ns("response_start_date"),
      label = "Start Date:",
      max = app_data$Current_Local_Date,
      value = coalesce(response_details$response_start_date, 
                       response_start_date_with_rollback)
    ),
    timeInput(
      inputId = ns("response_start_time"),
      label = "Start Time:",
      value = coalesce(
        response_details$response_start_time,
        Sys.time() |>
          with_tz(Sys.getenv("LOCAL_TZ")) |>
          floor_date("minute") - 3600
      ),
      seconds = GetSetting("incident",
                           group = "incident_response",
                           key = "input_seconds")
    ),
    dateInput(
      inputId = ns("response_end_date"),
      label = "End Date:",
      max = app_data$Current_Local_Date,
      value = coalesce(response_details$response_end_date, 
                       app_data$Current_Local_Date)
    ),
    timeInput(
      inputId = ns("response_end_time"),
      label = "End Time:",
      value = coalesce(
        response_details$response_end_time,
        Sys.time() |>
          with_tz(Sys.getenv("LOCAL_TZ")) |>
          floor_date("minute")
      ),
      seconds = GetSetting("incident",
                           group = "incident_response",
                           key = "input_seconds")
    ),
    footer = tagList(
      modalButton('Cancel'),
      actionButton(
        inputId = ns("to_apparatus_ff"),
        label = "Next",
        class = "btn btn-primary"
      )
    )
  )
}


note <- function(ns, response_details, length) {

  modalDialog(
    textAreaInput(
      inputId = ns("response_notes"),
      label = "Notes:",
      value = coalesce(response_details$response_notes, "")
    ),
    # Checkbox to trigger time adjustments
    checkboxInput(
      inputId = ns("time_adjust_needed"),
      label = "Do time adjustments need to be made?",
      value = FALSE
    ),
    # Conditional numeric inputs for each firefighter
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("time_adjust_needed")),
      # Use do.call to properly pass each column to fluidRow
      do.call(
        fluidRow,
        lapply(response_details$firefighter, function(ff) {
          # sanitize ID: lowercase, replace spaces with underscores
          id <- paste0(
            "time_adj_",
            ff |> tolower() |> stringr::str_replace_all(" ", "_")
          )
          column(
            width = 6,
            numericInput(
              inputId = ns(id),
              label = ff,
              value = 0,
              max = GetSetting(
                "incident",
                group = "incident_response",
                key = "time_adjust_max"
              ),
              min = GetSetting(
                "incident",
                group = "incident_response",
                key = "time_adjust_min"
              )
            )
          )
        })
      )
    ),
    
    footer = tagList(
      # Back button logic
      if (length != 0 && GetSetting(
        "incident",
        group = "incident_response",
        key = "firefighter_apparatus_assignment"
      )) {
        actionButton(ns("to_assignment"), "Back", class = "btn btn-light")
      } else {
        actionButton(ns("to_apparatus_ff"), "Back", class = "btn btn-light")
      },
      actionButton(ns("cancel_modal"), "Cancel", class = "btn btn-warning"),
      actionButton(ns("submit"), "Submit", class = "btn btn-success")
    )
  )
}


