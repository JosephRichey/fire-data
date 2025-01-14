#https://chatgpt.com/share/67867dd6-7cd0-800b-9a98-7d314538ace6

library(shiny)
library(shinyjs)
library(dplyr)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Incident Reporting"),
  actionButton("create_incident", "Create Incident"),
  tags$head(tags$style(".modal-dialog { max-width: 500px; }"))
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values to store incident details
  incident_details <- reactiveValues(
    date = NULL,
    type = NULL
  )
  
  # Observe "Create Incident" button
  observeEvent(input$create_incident, {
    showModal(modalDateInput())
  })
  
  # Modal for Date Input
  modalDateInput <- function() {
    modalDialog(
      title = "Enter Incident Date",
      dateInput("incident_date", "Incident Date:", value = coalesce(incident_details$date, Sys.Date())),
      footer = tagList(
        actionButton("cancel_modal", "Cancel", class = "btn btn-default"),
        actionButton("next_to_type", "Next")
      )
    )
  }
  
  # Modal for Type Input
  modalTypeInput <- function() {
    modalDialog(
      title = "Select Incident Type",
      radioButtons("incident_type", "Incident Type:",
                   choices = c("Fire" = "fire", "EMS" = "ems"),
                   selected = coalesce(incident_details$type, 'fire')),
      footer = tagList(
        actionButton("cancel_modal", "Cancel", class = "btn btn-default"),
        actionButton("back_to_date", "Back"),
        actionButton("next_to_summary", "Next")
      )
    )
  }
  
  # Modal for Summary and Submission
  modalSummary <- function() {
    modalDialog(
      title = "Confirm Incident Details",
      p(paste("Date:", incident_details$date)),
      p(paste("Type:", incident_details$type)),
      footer = tagList(
        actionButton("cancel_modal", "Cancel", class = "btn btn-default"),
        actionButton("back_to_type", "Back"),
        actionButton("submit_incident", "Submit")
      )
    )
  }
  
  # Save Date Automatically When Modified
  bindEvent(
    observe({
      incident_details$date <- input$incident_date
    }),
    input$incident_date
  )
  
  # Save Type Automatically When Modified
  bindEvent(
    observe({
      incident_details$type <- input$incident_type
    }),
    input$incident_type
  )
  
  # Reset Reactive Values on Cancel
  observeEvent(input$cancel_modal, {
    incident_details$date <- NULL
    incident_details$type <- NULL
    removeModal()
  })
  
  # Navigation Logic
  observeEvent(input$next_to_type, {
    removeModal()
    showModal(modalTypeInput())
  })
  
  observeEvent(input$back_to_date, {
    removeModal()
    showModal(modalDateInput())
  })
  
  observeEvent(input$next_to_summary, {
    removeModal()
    showModal(modalSummary())
  })
  
  observeEvent(input$back_to_type, {
    removeModal()
    showModal(modalTypeInput())
  })
  
  observeEvent(input$submit_incident, {
    removeModal()
    showNotification(paste("Incident saved:",
                           "Date:", incident_details$date,
                           "Type:", incident_details$type),
                     type = "message")
    # Reset after submission (optional)
    incident_details$date <- NULL
    incident_details$type <- NULL
  })
}

# Run App
shinyApp(ui, server)
