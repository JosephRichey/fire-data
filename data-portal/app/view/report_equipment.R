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
          fg = 'black'
        ),
        showcase = bsicons::bs_icon('exclamation')
      ),
      value_box(
        'Expired Equipment',
        # textOutput(ns('expired_equipment')),
        "0",
        theme = 'warning',
        showcase = bsicons::bs_icon('exclamation-triangle')
      ),
      value_box(
        'Failed Checks',
        "1",
        theme = 'primary',
        showcase = bsicons::bs_icon('exclamation-octagon')

      ),

      col_widths = c(4, 4, 4)
    ),

    layout_columns(
      plotlyOutput(ns('equipment_plot')),
      card(
        card_header('Equipment Messages/Failures'),
        DT::DTOutput(ns('equipment_messages'))
      )

    )


  )

}

#' @export
Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$equipment_messages <- DT::renderDT(
        data.frame(
          Message = c('We need more SCBA wipes',
                      "Saw failure: Roof saw on 82 won't start"),
          Date = c('2025-03-01', Sys.Date() |> as.character()),
          Action = c(
            sprintf('<button id="resolve_%d" class="btn btn-success">Resolve</button>', 1),
            sprintf('<button id="resolve_%d" class="btn btn-success">Resolve</button>', 2)
          )
        ) |>
          DT::datatable(
            escape = FALSE,
            filter = 'none',
            rownames = FALSE,
            selection = 'none',
            options = list(dom = 't')
          )

      )


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
          add_lines(line = list(color = 'white')) %>%
          layout(
            title = "Equipment Compliance Over Time",
            xaxis = list(
              title = "Date",
              titlefont = list(color = '#FFFFFF'),
              tickfont = list(color = '#FFFFFF'),
              gridcolor = '#2d2d2d'
            ),
            yaxis = list(title = "Compliance (%)",
                         titlefont = list(color = '#FFFFFF'),
                         tickfont = list(color = '#FFFFFF'),
                         gridcolor = '#2d2d2d',
                         zeroline = FALSE
            ),
            plot_bgcolor = '#222222',
            paper_bgcolor = '#222222',
            font = list(color = '#FFFFFF'),
            showlegend = FALSE
          )

      })




    }
  )
}

