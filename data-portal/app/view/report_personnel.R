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

      #FIXME This is bleh. Need to get ideas for what reporting would be useful.
      !!!purrr::map(app_data$Certification_Type$certification_name, function(cert) {
        value_box(
          paste0(cert, " Count"),
          textOutput(ns(paste0(cert |> tolower() |> stringr::str_replace_all(" ", "_"), "_count"))),
          theme = "light"
        )
      }),

      col_widths = rep(3, length(app_data$Certification_Type$certification_name))
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

      Certifications <- reactive({
        app_data$Certification_Type |>
          left_join(app_data$Certification, by = c("certification_type_id" = "type_id")) |>
          select(certification_name) |>
          count(certification_name)

      })

      observe({
        req(Certifications())

        for(cert in app_data$Certification_Type$certification_name) {
          local({
            cert_name <- cert |> tolower() |> stringr::str_replace_all(" ", "_")
            captured_cert <- cert  # Captures `cert` inside `local()` for each iteration

            output[[paste0(cert_name, "_count")]] <- renderText({
              Certifications() |>
                filter(certification_name == captured_cert) |>  # Use captured_cert here
                pull(n) |>
                unname() |>
                as.character()
            })
          })
        }
      })




    }
  )
}

