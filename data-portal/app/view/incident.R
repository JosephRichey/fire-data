box::use(
  dplyr[...],
  lubridate[...],
  reactable[...],
  shiny[...],
  bslib[...],
  shinyWidgets[...],
)

box::use(
  ../logic/functions,
  ../logic/app_data,
)

UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("delete"),
                 "Delete Incident",
                 icon = icon("trash"),
                 class = "btn-danger"),
    accordion(
      accordion_panel(
        title = "Filter Incidents",
        open = FALSE,
        icon = bsicons::bs_icon("funnel-fill"),
        dateRangeInput(ns('incident_filter_range'),
                       "Show incidents between:",
                       start = as.Date(app_data$Local_Date - ddays(30)), #FIXME Set by settings
                       end = app_data$Local_Date
        ),
        pickerInput(ns('incident_filter_reason'),
                    'Dispatch Code',
                    choices = c('Chest Pain', 'Choking'),#app_data$Dispatch_Codes, #FIXME Have in doc the risks of removing from this list.
                    selected = c('Chest Pain', 'Choking'),#app_data$Dispatch_Codes,
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
        ),

        pickerInput(ns('incident_filter_response_type'),
                    'Response Type',
                    choices = c('EMS', 'Fire', 'Wildland', 'None'),
                    selected = c('EMS', 'Fire', 'Wildland', 'None'),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
        )
      )
    )
  )
}

Output <- function(id) {
  ns <- NS(id)
  tagList(
    reactableOutput(ns("incident_table"))
  )
}

Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      r_Incident_Data <- reactiveVal({
        cat('Initiating Incident Data', file = stderr())
        app_data$Incident
        })

      r_Displayed_Incidents <- reactive({
        cat('Filtering Incidents', file = stderr())

        # browser()

        #FIXME This is a garbace way to do this, but it's 1 AM and I'm tired.
        df <- r_Incident_Data() |>
          mutate(units = paste(
            if_else(ems_units == 1, 'EMS ',''),
            if_else(fire_units == 1, 'Fire ',''),
            if_else(wildland_units == 1, 'Wildland ',''),
            if_else(ems_units == 0 & fire_units == 0 & wildland_units == 0, 'None', '')
            )) |>
          filter(dispatch_time >= input$incident_filter_range[1],
                 dispatch_time <= input$incident_filter_range[2],
                 dispatch_reason %in% input$incident_filter_reason,
                 grepl(paste(input$incident_filter_response_type, collapse = '|'), units)
        ) |>
          select(-units)


        cat("Filtered Rows:", nrow(df), "\n", file = stderr())

        return(df)
      })


      observe({
        showModal(
          modalDialog(
            title = "Delete Incident",
            'Select which incident you would like to delete.',
            selectInput(
              ns('delete_incident_id'),
              'Incident',
              choices = r_Displayed_Incidents() |>
                filter(dispatch_time >= Sys.Date() - ddays(30)) |>
                pull(incident_id),
              selected = NULL,
              multiple = FALSE
            ),
            easyClose = TRUE,
            footer = tagList(
              modalButton('Cancel'),
              actionButton(ns('delete_incident_button'),
                           'Delete',
                           icon = icon('trash'),
                           class = 'btn-danger'),
            )
          )
        )

      }) |>
        bindEvent(input$delete)

      observeEvent(input$delete_incident_button, {
        cat('Deleting Incident', file = stderr())
        #FIXME Delete Incident

        browser()

        r_Incident_Data(
          r_Incident_Data() |>
            filter(incident_id != input$delete_incident_id))

        removeModal()

        showModal(
          modalDialog(
            title = "Incident Deleted",
            'The incident has been deleted.',
            easyClose = TRUE,
            footer = tagList(
              modalButton('Close'),
            )
          )
        )
      })




      output$incident_table <- renderReactable({
        # browser()

        print('Rendering table')
        reactable(r_Displayed_Incidents(),
                  theme = reactableTheme(
                    color = 'white',
                    backgroundColor = '#333',
                    stripedColor = '#555',
                    highlightColor = '#2b8764',
                    pageButtonHoverStyle = list(
                      color = 'white',
                      backgroundColor = '#a05050'
                    ),
                    pageButtonActiveStyle = list(
                      color = 'white',
                      backgroundColor = '#87292b'
                    ),
                  ))
        # reactable(
        #   r_Displayed_Incidents(),
        #   columns = list(
        #     dispatch_time = colDef(
        #       name = "Dispatch Time",
        #       format = "DateTime"
        #     ),
        #     dispatch_code = colDef(
        #       name = "Dispatch Code"
        #     ),
        #     response_type = colDef(
        #       name = "Response Type"
        #     ),
        #     address = colDef(
        #       name = "Address"
        #     ),
        #     city = colDef(
        #       name = "City"
        #     ),
        #     state = colDef(
        #       name = "State"
        #     ),
        #     zip = colDef(
        #       name = "Zip"
        #     ),
        #     county = colDef(
        #       name = "County"
        #     ),
        #     latitude = colDef(
        #       name = "Latitude"
        #     ),
        #     longitude = colDef(
        #       name = "Longitude"
        #     )
        #   )
        # )
      })
    }
  )
}
