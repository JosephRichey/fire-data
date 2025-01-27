box::use(
  shiny[...],
  lubridate[...],
  dplyr[...],
)

key_time <- function(ns) {
    modalDialog(
    # textInput(ns("incident_id"), "Incident ID:", ""),
    # dateInput(ns("dispatch_date"), "Dispatch Date:"),
    # timeInput(ns("dispatch_time"), "Dispatch Time:", value = as.ITime(Sys.time() |> with_tz(Sys.getenv('LOCAL_TZ'))-3600), seconds = F),
    # dateInput(ns("end_date"), "End Date:"),
    # timeInput(ns("end_time"), "End Time:", value = as.ITime(Sys.time() |> with_tz(Sys.getenv('LOCAL_TZ'))), seconds = F),
    'Hello World',
    footer = tagList(
      actionButton(ns('cancel_modal'), 'Cancel', class = 'btn btn-warning'),
      actionButton(ns("to_address_unit"), "Next", class = 'btn btn-primary')
    )
  )
}

address_unit <- function(ns) {
  modalDialog(
    # textInput(ns('address'), 'Address:', ""),
    # selectInput(ns('area'), 'Response Area', c('Municipality', 'Primary Area', 'Mutual Aid', 'Outside Aid')),
    # selectInput(ns("dispatch_reason"), "Dispatch Reason:", app_data$Dispatch_Codes),
    # checkboxGroupInput(ns('units'), 'Units', c('EMS', 'Fire', 'Wildland')),
    # checkboxInput(ns('canceled'), 'Canceled before arrival'),
    # checkboxInput(ns('dropped'), 'Dropped call'),
    "How are you?",
    footer = tagList(
      actionButton(ns('add_incident'), 'Back', class = 'btn btn-secondary'),
      actionButton(ns('cancel_modal'), 'Cancel', class = 'btn btn-warning'),
      actionButton(ns("to_apparatus_ff"), "Next", class = 'btn btn-primary')
    )
  )
}


apparatus_ff <- function(ns) {
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

  
assignment <- function(ns) {
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
  
  
note <- function(ns) {
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