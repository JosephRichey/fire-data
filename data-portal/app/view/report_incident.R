box::use(
  shiny[...],
  bslib[...],
  shinyWidgets[...],
  DT[...],
  dplyr[filter, ...],
  DBI[...],
  lubridate[...],
  bsicons[...],
  plotly[...],
  ggplot2[...],
)


box::use(
  ../logic/app_data,
  ../logic/functions,
)

#' @export
UI <- function(id) {
  ns <- NS(id)

  tagList(
    dateRangeInput(ns('incident_filter_range'),
                   "Show incidents between:",
                   start = with_tz(Sys.time(), tzone = Sys.getenv('LOCAL_TZ')) - months(1),
                   end = with_tz(Sys.time(), tzone = Sys.getenv('LOCAL_TZ'))),
    downloadButton(ns("download"), "Download",
                   class = 'btn-secondary'),
    hr(),
    actionButton(
      ns('choose_custom'),
      'Generate Custom Report',
      class = 'btn-primary',
      icon = icon('wand-magic-sparkles')
    )
  )
}

#' @export
Output <- function(id) {
  ns <- NS(id)

  tagList(
      layout_columns(
        value_box("Number of Incidents",
                  textOutput(ns("number")),
                  showcase = bs_icon('hash',
                                     color = 'white')),
        value_box(
          "EMS Incidents",
          textOutput(ns("ems_number")),
          theme = "info"
          ),
        value_box(
          "Fire Incidents",
          textOutput(ns("fire_number")),
          theme = 'primary'
          ),
        value_box(
          "Wildland Incidents",
          textOutput(ns("wildland_number")),
          theme = 'success'
          ),
        value_box("Time Spent on Incidents (Incident/People)",
                  textOutput(ns("hours")),
                  showcase = bs_icon('clock-fill',
                                     color = 'white')),
        value_box(
          "EMS Hours",
          textOutput(ns("ems_hours")),
          theme = 'info'
          ),
        value_box(
          "Fire Hours",
          textOutput(ns("fire_hours")),
          theme = 'primary'
          ),
        value_box(
          "Wildland Hours",
          textOutput(ns("wildland_hours")),
          theme = 'success'
          ),

        col_widths = c(3,3,3,3),
        row_widths = c(1,1)
      ),
      card(
        plotlyOutput(ns("incidents"))
      )

  )

}

#' @export
Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns


      observe({
        showModal(
          modalDialog(
            title = "Custom Report",
            "Choose a custom report to generate",
            selectInput(
              ns("custom_report"),
              "Report Type",
              c("Individual Response to Incididnets",
                "Additional Response Incidents",
                "Multi Type Incidents",
                "Cancellation Summary")
            ),
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close"),
              actionButton(ns('submit'), 'Submit', class = 'btn-primary')
            )
          )
        )
      }) |>
        bindEvent(input$choose_custom)

      observeEvent(input$submit, {
        removeModal()
        shinyalert::shinyalert(
          title = "Report Generated",
          text = "At this point, a report would be generated based on the custom report selected.",
          type = "success"
        )
      })

      R_Incidents <- reactive({

        #FIXME Haven't validated any of the training reports
        df <- app_data$Incident |>
          mutate(incident_date = with_tz(dispatch_time, tzone = Sys.getenv('LOCAL_TZ'))|> as.Date())  |>
          # Note, all three dates are in the local time zone
          filter(incident_date >= input$incident_filter_range[1] & incident_date <= input$incident_filter_range[2]) |>
          mutate(incident_length = difftime(end_time, dispatch_time, units = 'hours') |> as.numeric()) |>
          left_join(app_data$Firefighter_Incident |> select(firefighter_id, incident_id),
                    by = c("incident_id")) |>
          select(incident_id, incident_length, incident_date, ems_units, fire_units, wildland_units)


        return(df)

      })

      output$number <- renderText({
        R_Incidents() |>
          select(incident_id) |>
          distinct() |>
          nrow()
      })


      output$hours <- renderText({
        # browser()

        inc <- R_Incidents() |>
          distinct() |>
          select(incident_length) |>
          sum() |>
          round()

        ind <- R_Incidents() |>
          select(incident_length) |>
          sum() |>
          round()

          paste(inc, "/", ind)
      })

      output$ems_hours <- renderText({
        inc <- R_Incidents() |>
          filter(ems_units == 1) |>
          distinct() |>
          select(incident_length) |>
          sum() |>
          round()

        ind <- R_Incidents() |>
          filter(ems_units == 1) |>
          select(incident_length) |>
          sum() |>
          round()

        paste(inc, "/", ind)
      })

      output$ems_number <- renderText({
        R_Incidents() |>
          filter(ems_units == 1) |>
          select(incident_id) |>
          distinct() |>
          nrow()
      })

      output$fire_hours <- renderText({
        inc <- R_Incidents() |>
          filter(fire_units == 1) |>
          distinct() |>
          select(incident_length) |>
          sum() |>
          round()

        ind <- R_Incidents() |>
          filter(fire_units == 1) |>
          select(incident_length) |>
          sum() |>
          round()

        paste(inc, "/", ind)

      })

      output$fire_number <- renderText({
        R_Incidents() |>
          filter(fire_units == 1) |>
          select(incident_id) |>
          distinct() |>
          nrow()
      })

      output$wildland_hours <- renderText({
        inc <- R_Incidents() |>
          filter(wildland_units == 1) |>
          distinct() |>
          select(incident_length) |>
          sum() |>
          round()

        ind <- R_Incidents() |>
          filter(wildland_units == 1) |>
          select(incident_length) |>
          sum() |>
          round()

        paste(inc, "/", ind)

      })

      output$wildland_number <- renderText({
        R_Incidents() |>
          filter(wildland_units == 1) |>
          select(incident_id) |>
          distinct() |>
          nrow()
      })

      output$incidents <- renderPlotly({
        # browser()
        plot_data <- R_Incidents() |>
          select(-incident_length) |>
          unique() |>
          mutate(
            mixed = if_else((ems_units + fire_units + wildland_units) > 1, 1, 0),
            neither = if_else((ems_units + fire_units + wildland_units) == 0, 1, 0)
          ) |>
          group_by(incident_date) |>
          summarize(
            ems = sum(ems_units),
            fire = sum(fire_units),
            wildland = sum(wildland_units),
            mixed = sum(mixed),
            neither = sum(neither)
          ) |>
          mutate(total = ems + fire + wildland + mixed + neither)  # Total column for text annotations

        plot_ly(plot_data, x = ~incident_date) |>
          add_bars(y = ~ems, name = "EMS", marker = list(color = "#3498DB")) |>
          add_bars(y = ~fire, name = "Fire", marker = list(color = "#E74C3C")) |>
          add_bars(y = ~wildland, name = "Wildland", marker = list(color = "#00BC8C")) |>
          add_bars(y = ~mixed, name = "Mixed", marker = list(color = "#F39C12")) |>
          add_bars(y = ~neither, name = "Neither", marker = list(color = "#BDC3C7")) |>
          add_text(
            y = ~total + 0.25,
            text = ~total,
            textposition = "outside",
            showlegend = FALSE
          ) |>
          layout(
            title = "Incidents Over Time",
            xaxis = list(
              title = "Date",
              titlefont = list(color = '#FFFFFF'),
              tickfont = list(color = '#FFFFFF'),
              gridcolor = '#2d2d2d'
            ),
            yaxis = list(
              title = "Incidents",
              titlefont = list(color = '#FFFFFF'),
              tickfont = list(color = '#FFFFFF'),
              gridcolor = '#2d2d2d',
              zeroline = FALSE,
              dtick = 1
            ),
            plot_bgcolor = '#222222',
            paper_bgcolor = '#222222',
            font = list(color = '#FFFFFF'),
            barmode = 'stack',
            showlegend = TRUE
          )



      })


    }
  )
}

