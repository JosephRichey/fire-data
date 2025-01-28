box::use(
  shiny[...],
  lubridate[...],
  dplyr[...],
  shinyTime[timeInput],
)

box::use(
  ../logic/app_data,
)

key_time <- function(ns, incident_details) {
    modalDialog(
      textInput(
        inputId = ns("incident_id"), 
                  label = "Incident ID:", 
                  value = coalesce(incident_details$incident_id, "")
                ),
      # TODO Check that this dispatch date works with the tz conversion
      dateInput(ns("dispatch_date"), 
                  label = "Dispatch Date:",
                  value = coalesce(incident_details$date, Sys.Date())
                ),
      timeInput(ns("dispatch_time"), 
                  label = "Dispatch Time:", 
                  value = coalesce(
                    incident_details$time,
                    Sys.time() |>
                      with_tz(Sys.getenv('LOCAL_TZ')) |>
                      floor_date("minute") - 3600
                    ),
                    seconds = app_data$dispatch_time_seconds
                  ),
      dateInput(
        inputId = ns("end_date"), 
        label = "End Date:",
        value = coalesce(incident_details$end_date, Sys.Date())
        ),
      timeInput(inputId = ns("end_time"), 
                label = "End Time:", 
                value = coalesce(
                  incident_details$end_time,
                  Sys.time() |>
                  with_tz(Sys.getenv('LOCAL_TZ')) |>
                  floor_date("minute")),
                seconds = app_data$end_time_seconds),
      footer = tagList(
        actionButton(
          inputId = ns('cancel_modal'), 
          label = 'Cancel', 
          class = 'btn btn-warning'
          ),
        actionButton(
          inputId = ns("to_address_unit"), 
          label = "Next", 
          class = 'btn btn-primary')
      )
  )
}

address_unit <- function(ns, incident_details) {
  modalDialog(
    if(app_data$address) {
      textInput(
        inputId = ns('address'), 
        label = 'Address', 
        value = coalesce(incident_details$address, "")
      )
    },
    selectInput(
      inputId = ns('area'), 
      label = 'Response Area', 
      choices = app_data$response_area,
      selected = coalesce(incident_details$area, "")
    ),
    selectInput(
      inputId = ns("dispatch_reason"), 
      label = "Dispatch Reason", 
      choices = app_data$Dispatch_Codes,
      selected = coalesce(incident_details$dispatch_reason, "")
    ),
    checkboxGroupInput(
      inputId = ns('units'), 
      label = 'Units', 
      choices = app_data$response_units,
      selected = coalesce(incident_details$units, "")
    ),
    if(app_data$canceled) {
      checkboxInput(
        inputId = ns('canceled'), 
        label = 'Canceled before arrival',
        value = coalesce(incident_details$canceled, FALSE)
      )
    },
    if(app_data$dropped) {
      checkboxInput(
        inputId = ns('dropped'), 
        label = 'Dropped call',
        value = coalesce(incident_details$dropped, FALSE)
      )
    },
    footer = tagList(
      actionButton(ns('add_incident'), 'Back', class = 'btn btn-secondary'),
      actionButton(ns('cancel_modal'), 'Cancel', class = 'btn btn-warning'),
      actionButton(ns("to_apparatus_ff"), "Next", class = 'btn btn-primary')
    )
  )
}



apparatus_ff <- function(ns, incident_details) {
  modalDialog(
  # selectInput(ns('apparatus'), 
  #             'Apparatus:', 
  #             choices = c(app_data$Apparatus |> pull(apparatus_name)),
  #             multiple = TRUE
  # ),
  # selectInput(ns('firefighter'),
  #             'Firefighter',
  #             choices = c(app_data$Firefighter |> pull(firefighter_full_name)),
  #             multiple = TRUE
  # ),
  # footer = tagList(
  #   actionButton(ns('cancel_mod_3'), 'Cancel'),
  #   actionButton(ns("mod_3_next"), "Next")
  # )
  
    "I'm fine",
    footer = tagList(
      actionButton(ns('to_address_unit'), 'Back', class = 'btn btn-secondary'),
      actionButton(ns('cancel_modal'), 'Cancel', class = 'btn btn-warning'),
      actionButton(ns("to_assignment"), "Next", class = 'btn btn-primary')
    )
  )
}

  
assignment <- function(ns, incident_details) {
  modalDialog(
  # tagList(select_inputs),
  # 
  # footer = tagList(
  #   actionButton(ns("cancel_mod_4"), "Cancel"),
  #   actionButton(ns("mod_4_next"), "Next")
  # )
    "Thanks for asking",
    footer = tagList(
      actionButton(ns('to_apparatus_ff'), 'Back', class = 'btn btn-secondary'),
      actionButton(ns('cancel_modal'), 'Cancel', class = 'btn btn-warning'),
      actionButton(ns("to_note"), "Next", class = 'btn btn-primary')
    )
  )
}
  
  
note <- function(ns, incident_details) {
  modalDialog(
  # textInput(ns("call_notes"), "Notes:", ""),
  # 
  # footer = tagList(
  #   actionButton(ns("cancel_mod_5"), "Cancel"),
  #   actionButton(ns("mod_5_submit"), "Submit Incident")
  # )
    "Goodbye",
    footer = tagList(
      actionButton(ns('to_assignment'), 'Back', class = 'btn btn-secondary'),
      actionButton(ns('cancel_modal'), 'Cancel', class = 'btn btn-warning'),
      actionButton(ns("submit"), "Submit", class = 'btn btn-success')
    )
  )
}