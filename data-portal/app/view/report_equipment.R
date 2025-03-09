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
  tibble[...],
  stats[...],
)


box::use(
  ../logic/app_data,
  ../logic/functions,
)

#' @export
UI <- function(id) {
  ns <- NS(id)

  tagList(
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
      value_box(
        'Overdue Checks',
        "5",
        theme = value_box_theme(
          bg = '#F4C542',
          fg = 'white'
        ),
        icon = icon('exclamation-triangle', lib = 'font-awesome')
      ),
      value_box(
        'Expired Equipment',
        # textOutput(ns('expired_equipment')),
        "1",
        theme = 'danger',
        icon = icon('exclamation-triangle', lib = 'font-awesome')
      ),

      col_widths = c(5, -2, 5)
    ),

    plotlyOutput(ns('equipment_plot'))

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
              c("Report A",
                "Report B",
                "Report C")
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

      # Create a fake placeholder plot. Build a dataframe shows percentage compliant over time.
      # It should randomly go up and down, starting at 90%. It should show a year of data.
      # Compliance can not go over 100%.
      output$equipment_plot <- renderPlotly({
        data <- tibble(
          date = seq.Date(from = as.Date('2024-01-01'), to = as.Date('2024-12-31'), by = 'day'),
          compliance = 90 + cumsum(rnorm(length(date), mean = 0, sd = 1))
        ) %>%
          mutate(compliance = pmin(compliance, 100))

        plot_ly(data, x = ~date, y = ~compliance) %>%
          add_lines() %>%
          layout(
            title = "Equipment Compliance Over Time",
            xaxis = list(title = "Date"),
            yaxis = list(title = "Compliance (%)")
          )

      })




    }
  )
}

