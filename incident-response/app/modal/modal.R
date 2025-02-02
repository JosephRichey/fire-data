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
    dateInput(
      inputId = ns("dispatch_date"),
      label = "Dispatch Date:",
      value = coalesce(incident_details$dispatch_date, app_data$Current_Local_Date)
    ),
    timeInput(
      inputId = ns("dispatch_time"),
      label = "Dispatch Time:",
      value = coalesce(
        incident_details$dispatch_time,
        Sys.time() |>
          with_tz(Sys.getenv("LOCAL_TZ")) |>
          floor_date("minute") - 3600
      ),
      seconds = app_data$dispatch_time_seconds
    ),
    dateInput(
      inputId = ns("end_date"),
      label = "End Date:",
      value = coalesce(incident_details$end_date, app_data$Current_Local_Date)
    ),
    timeInput(
      inputId = ns("end_time"),
      label = "End Time:",
      value = coalesce(
        incident_details$end_time,
        Sys.time() |>
          with_tz(Sys.getenv("LOCAL_TZ")) |>
          floor_date("minute")
      ),
      seconds = app_data$end_time_seconds
    ),
    footer = tagList(
      actionButton(
        inputId = ns("cancel_modal"),
        label = "Cancel",
        class = "btn btn-warning"
      ),
      actionButton(
        inputId = ns("to_address_unit"),
        label = "Next",
        class = "btn btn-primary"
      )
    )
  )
}
 
# FIXME Have to prevent going back after forward and ending up in a modal user shouldn't be in
# FIXME Need to cache and reset values

key_time_additional <- function(ns, incident_details) {
  modalDialog(
    selectInput(
      inputId = ns("incident_id_additional"),
      label = "Incident ID:",
      choices = app_data$Incident() |> 
        arrange(desc(dispatch_time)) |>
        filter(dispatch_time > Sys.time() - days(4)) |>
        pull(incident_id),
      selected = coalesce(incident_details$incident_id, "")
    ),
    dateInput(
      inputId = ns("dispatch_date"),
      label = "Dispatch Date:",
      value = coalesce(incident_details$dispatch_date, app_data$Current_Local_Date)
    ),
    timeInput(
      inputId = ns("dispatch_time"),
      label = "Dispatch Time:",
      value = coalesce(
        incident_details$dispatch_time,
        Sys.time() |>
          with_tz(Sys.getenv("LOCAL_TZ")) |>
          floor_date("minute") - 3600
      ),
      seconds = app_data$dispatch_time_seconds
    ),
    dateInput(
      inputId = ns("end_date"),
      label = "End Date:",
      value = coalesce(incident_details$end_date, app_data$Current_Local_Date)
    ),
    timeInput(
      inputId = ns("end_time"),
      label = "End Time:",
      value = coalesce(
        incident_details$end_time,
        Sys.time() |>
          with_tz(Sys.getenv("LOCAL_TZ")) |>
          floor_date("minute")
      ),
      seconds = app_data$end_time_seconds
    ),
    footer = tagList(
      actionButton(
        inputId = ns("cancel_modal"),
        label = "Cancel",
        class = "btn btn-warning"
      ),
      actionButton(
        inputId = ns("to_apparatus_ff"),
        label = "Next",
        class = "btn btn-primary"
      )
    )
  )
}

address_unit <- function(ns, incident_details) {
  modalDialog(
    if (app_data$address) {
      textInput(
        inputId = ns("address"),
        label = "Address",
        value = coalesce(incident_details$address, "")
      )
    },
    selectInput(
      inputId = ns("area"),
      label = "Response Area",
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
      inputId = ns("units"),
      label = "Units",
      choices = app_data$response_units,
      selected = coalesce(incident_details$units, "")
    ),
    if (app_data$canceled) {
      checkboxInput(
        inputId = ns("canceled"),
        label = "Canceled before arrival",
        value = coalesce(incident_details$canceled, FALSE)
      )
    },
    if (app_data$dropped) {
      checkboxInput(
        inputId = ns("dropped"),
        label = "Dropped call",
        value = coalesce(incident_details$dropped, FALSE)
      )
    },
    footer = tagList(
      actionButton(ns("add_incident"), "Back", class = "btn btn-light"),
      actionButton(ns("cancel_modal"), "Cancel", class = "btn btn-warning"),
      actionButton(ns("to_apparatus_ff"), "Next", class = "btn btn-primary")
    )
  )
}

select_ff_aparatus <- function(ns, incident_details) {
  modalDialog(
    selectInput(
      inputId = ns("apparatus"),
      label = "Apparatus:",
      choices = app_data$Apparatus() |> pull(apparatus_name),
      selected = coalesce(incident_details$apparatus, ""),
      multiple = TRUE
    ),
    selectInput(
      inputId = ns("firefighter"),
      label = "Firefighter",
      choices = app_data$Firefighter() |> pull(full_name),
      selected = coalesce(incident_details$firefighter, ""),
      multiple = TRUE
    ),
    footer = tagList(
      actionButton(ns("to_address_unit"), "Back", class = "btn btn-light"),
      actionButton(ns("cancel_modal"), "Cancel", class = "btn btn-warning"),
      actionButton(ns("to_assignment"), "Next", class = "btn btn-primary")
    )
  )
}


assignment <- function(ns, incident_details) {
  modalDialog(
    "Still building...",
    footer = tagList(
      actionButton(ns("to_apparatus_ff"), "Back", class = "btn btn-light"),
      actionButton(ns("cancel_modal"), "Cancel", class = "btn btn-warning"),
      actionButton(ns("to_note"), "Next", class = "btn btn-primary")
    )
  )
}


note <- function(ns, incident_details) {
  modalDialog(
    textAreaInput(
      inputId = ns("call_notes"),
      label = "Notes:",
      value = coalesce(incident_details$call_notes, "")
    ),
    footer = tagList(
      actionButton(ns("to_assignment"), "Back", class = "btn btn-light"),
      actionButton(ns("cancel_modal"), "Cancel", class = "btn btn-warning"),
      actionButton(ns("submit"), "Submit", class = "btn btn-success")
    )
  )
}


password <- function(ns) {
  modalDialog(
    "Please enter an admin password to edit the incident id.",
    passwordInput(
      inputId = ns("password"),
      label = "",
      placeholder = "Password"
    ),
    footer = tagList(
      actionButton(ns("cancel_modal"), "Cancel", class = "btn btn-warning"),
      actionButton(ns("to_edit_id"), "Next", class = "btn btn-primary")
    )
  )
}

# TODO - have this set to have the 14 days be editable
edit_incident_id <- function(ns) {
  modalDialog(
    selectInput(
      inputId = ns("old_incident_id"),
      label = "Incident ID to edit:",
      choices = app_data$Incident() |> 
        arrange(desc(dispatch_time)) |>
        filter(dispatch_time > Sys.time() - days(14)) |>
        pull(incident_id)
    ),
    textInput(
      inputId = ns("new_incident_id"),
      label = "New Incident ID:"
    ),
    footer = tagList(
      actionButton(ns("cancel_modal"), "Cancel", class = "btn btn-warning"),
      actionButton(ns("to_edit_confirm"), "Next", class = "btn btn-primary")
    )
  )
}

submit_new_id <- function(ns, old, new) {
  modalDialog(
    "Are you sure you want to submit this incident?",
    br(),
    br(),
    strong(paste("Incident ID", old, 
                "will be updated to", new)),
    
    footer = tagList(
      actionButton(ns("cancel_modal"), "Cancel", class = "btn btn-warning"),
      actionButton(ns("submit_new_id"), "Submit", class = "btn btn-success")
    )
  )
}
