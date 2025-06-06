box::use(
  shiny[...],
  bslib[...],
  shinyWidgets[...],
  DT[...],
  stats[rnorm],
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
    ),
    selectInput(
      ns('firefighter'),
      'Select Firefighter',
      choices = app_data$Firefighter$full_name
    )
  )
}

#' @export
Output <- function(id) {
  ns <- NS(id)

  tagList(

    card(
      card_header("Department Certification Counts"),
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
    ),


    card(
      card_header(textOutput(ns("firefighter_name"))),

      layout_columns(
        uiOutput(ns("firefighter_details")),
        value_box("90 Day Training Hours",
                  textOutput(ns("firefighter_training_hours")),
                  theme = "info"),
        value_box("90 Day Incident Response",
                  textOutput(ns("firefighter_incident_count")),
                  theme = "info"),
        value_box("90 Day Training Percentage",
                  textOutput(ns("firefighter_training_percent")),
                  theme = "info"),
        value_box("90 Day Incident Response Percentage",
                  textOutput(ns("firefighter_incident_percent")),
                  theme = "info"),
        col_widths = c(4, 4, 4, -4, 4, 4)
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

      output$firefighter_name <- renderText({
        input$firefighter
      })

      output$firefighter_details <- renderUI({

        # browser()

        details <- paste0("Details for <b>", input$firefighter, "</b>:<br>")

        certifications <- app_data$Certification |>
          filter(
            firefighter_id == app_data$Firefighter$firefighter_id[
              app_data$Firefighter$full_name == input$firefighter
            ]
          ) |>
          left_join(app_data$Certification_Type, by = c("type_id" = "certification_type_id")) |>
          pull(certification_name) |>
          paste(collapse = "<br>")


        HTML(
          paste0(
            "<span>",
            details,
            '<b><u>Certifications:</u></b><br>',
            certifications,
            "</span>"
            )
          )


      })

      output$firefighter_training_hours <- renderText({
        # Generate random placeholder number
        req(input$firefighter)
        rnorm(1, 38, 10) |> round(0) |> as.character()
      })

      output$firefighter_incident_count <- renderText({
        req(input$firefighter)
        rnorm(1, 15, 5) |> round(0) |> as.character()
      })

      output$firefighter_training_percent <- renderText({
        req(input$firefighter)
        rnorm(1, 75, 10) |> round(0) |> as.character() |> paste0("%")
      })

      output$firefighter_incident_percent <- renderText({
        req(input$firefighter)
        rnorm(1, 85, 10) |> round(0) |> as.character() |> paste0("%")
      })




    }
  )
}

