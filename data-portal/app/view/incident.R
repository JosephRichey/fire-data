box::use(
  dplyr[...],
  lubridate[...],
  reactable[...],
  shiny[...],
  bslib[...],
  shinyWidgets[...],
  reactable.extras[...],
)

box::use(
  ../logic/functions,
  ../logic/app_data,
)

# This theme follows the other DT themes.This is the global setting.
options(reactable.theme = reactableTheme(
  color = 'white',
  backgroundColor = '#333',
  borderColor = 'black',
  borderWidth = '1px',
  stripedColor = '#555',
  highlightColor = '#81D7B6',
  pageButtonHoverStyle = list(
    color = 'white',
    backgroundColor = '#a05050'
  ),
  pageButtonActiveStyle = list(
    color = 'white',
    backgroundColor = '#87292b'
  ),
))


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
        # FIXME Don't know if this filter is worth it.... Also having trouble with wrapping the inputs
        # pickerInput(ns('incident_filter_reason'),
        #             'Dispatch Code',
        #             choices = app_data$Dispatch_Codes, #FIXME Have in doc the risks of removing from this list.
        #             selected = app_data$Dispatch_Codes,
        #             width = 'fit',
        #             options = list(`actions-box` = TRUE),
        #             multiple = TRUE,
        #             choicesOpt = list(
        #               content = stringr::str_wrap(app_data$Dispatch_Codes, 30) |>
        #                 stringr::str_replace_all('\\n', '<br>')
        #             )
        # ),

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
    HTML("<span>To edit incidents, please navigate to the <a href='https://fire-data.shinyapps.io/incident-response/' target='_blank'>Incident Response App</a>. Unfinalized incidents are editable.</span>"),
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
                 # dispatch_reason %in% input$incident_filter_reason,
                 grepl(paste(input$incident_filter_response_type, collapse = '|'), units)
        ) |>
          select(-units) |>
          mutate(across(c(ems_units, fire_units, wildland_units, canceled, dropped),
                        ~ as.character(if_else(.x == 1, bsicons::bs_icon('check', class = 'text-success fs-3'), bsicons::bs_icon('x', class = 'text-primary fs-3'))))) |>
          mutate(
            info = '') |>
          relocate(info, .before = finalized)



        cat("Filtered Rows:", nrow(df), "\n", file = stderr())

        return(df)
      })






      output$incident_table <- renderReactable({
        # browser()

        print('Rendering table')
        reactable(r_Displayed_Incidents(),
                  striped = TRUE,
                  highlight = TRUE,
                  defaultSorted = "dispatch_time",
                  defaultSortOrder = "desc",
                  defaultColDef = colDef(
                    header = function(value) gsub("_", " ", value, fixed = TRUE) |> stringr::str_to_title(),
                    align = "center"
                  ),
                  columns = list(
                    dispatch_time = colDef(
                      name = "Dispatch Time",
                      format = colFormat(datetime = T, hour12 = F)
                    ),
                    end_time = colDef(
                      name = "End Time",
                      format = colFormat(datetime = T, hour12 = F)
                    ),

                    finalized = colDef(
                      name = "Finalized",
                      html = TRUE,
                      cell = function(value, index) {
                        icon_svg <- as.character(bsicons::bs_icon('check-square-fill'))  # Convert to string

                        htmltools::HTML(sprintf(
                          "<span onclick=\"App.finalize_incident('%s', '%s')\" class='fs3' style='cursor: pointer; font-size: 1.5em;%s'>%s</span>",
                          ns(""),
                          r_Displayed_Incidents()$incident_id[index] |> as.character(),
                          if_else(value == 1, "color: #2b8764;", "color: #87292b;"),
                          icon_svg  # Insert raw SVG
                        ))
                      }
                    ),

                    info = colDef(
                      cell = function(value, index) {
                        htmltools::HTML(
                          sprintf(
                            "<span onclick=\"App.show_details('%s', '%s')\" class='fs3' style='cursor: pointer; font-size: 1.5em; color:#377eb4;'>&#x1F6C8; </span>",
                            ns(""),
                            r_Displayed_Incidents()$incident_id[index] |> as.character()
                          )
                        )

                      },
                      html = TRUE  # Important for rendering HTML inside reactable
                    ),


                    id = colDef(
                      show = FALSE
                    ),
                    #FIXME This table is too wide. Will have to figure out how to summarise better.
                    ems_units = colDef(
                      show = FALSE
                    ),
                    fire_units = colDef(
                      show = FALSE
                    ),
                    wildland_units = colDef(
                      show = FALSE
                    ),
                    canceled = colDef(
                      show = FALSE
                    ),
                    dropped = colDef(
                      show = FALSE
                    ),

                    notes = colDef(
                      show = FALSE
                    ),

                    area = colDef(
                      show = FALSE
                    ),

                    address = colDef(
                      show = FALSE
                    )

                  ),
                  details = function(index) {
                    target_id <- r_Displayed_Incidents()$incident_id[index]
                    ff_data <- app_data$Firefighter_Incident |>
                      filter(incident_id == target_id) |>
                      left_join(app_data$Firefighter, by = 'firefighter_id') |>
                      left_join(app_data$Firefighter_Apparatus, by = c('incident_id', 'firefighter_id')) |>
                      left_join(app_data$Apparatus, by = 'apparatus_id') |>
                      select(full_name, apparatus_name, time_adjustment)

                    htmltools::div(style = "padding: 1rem",
                                   reactable(ff_data,
                                             striped = TRUE,
                                             highlight = TRUE,
                                             defaultSorted = 'apparatus_name',
                                             defaultColDef = colDef(
                                               header = function(value) gsub("_", " ", value, fixed = TRUE) |> stringr::str_to_title(),
                                               align = "center"
                                             ),
                                             theme = reactableTheme(
                                               backgroundColor = '#999',
                                               stripedColor = '#bbb',
                                               color = 'black'
                                             ),
                                             columns = list(
                                               full_name = colDef(
                                                 name = "Firefighter",
                                                 footer = function(values) sprintf("Total: %s", length(values))
                                               )

                                             ),
                                             outlined = TRUE
                                             )
                                   )

                    }
        )
      })

        observe({
          # Toggle the state of the finalized column
          #FIXME Write change to DB
          r_Incident_Data() |>
            mutate(finalized = if_else(incident_id == input$finalize_incident, !finalized, finalized)) |>
            r_Incident_Data()

        }) |>
          bindEvent(input$finalize_incident)

        observe({
          # Show a modal with all info from call
          #FIXME Write change to DB
          # browser()
          details <- r_Incident_Data() |>
            filter(incident_id == input$show_details)

          showModal(
            modalDialog(
              title = "Incident Details",
                HTML(
                  paste(
                    '<strong>', details$area, " | ", details$address, '</strong>', '<br>',
                    if_else(details$ems_units == 1, 'EMS: <span class = checkmark>\U2713</span>', 'EMS: <span class = xmark>X</span>'), '<br>',
                    if_else(details$fire_units == 1, 'Fire: <span class = checkmark>\U2713</span>', 'Fire: <span class = xmark>X</span>'), '<br>',
                    if_else(details$wildland_units == 1, 'Wildland: <span class = checkmark>\U2713</span>', 'Wildland: <span class = xmark>X</span>'),
                    hr(),
                    if_else(details$canceled == 1, 'Canceled: <span class = checkmark>\U2713</span>', 'Canceled: <span class = xmark>X</span>'), '<br>',
                    if_else(details$dropped == 1, 'Dropped: <span class = checkmark>\U2713</span>', 'Dropped: <span class = xmark>X</span>'), '<br>',
                    hr(),
                    if_else(is.na(details$notes), "", details$notes)
                    )
                  ),



              easyClose = TRUE,
              footer = tagList(
                modalButton('Close'),
              )
            )
          )

        }) |>
          bindEvent(input$show_details)

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

        # browser()

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

    }
  )
}
