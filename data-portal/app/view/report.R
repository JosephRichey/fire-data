box::use(
  gt[...],
  shiny[...],
  bslib[...],
  shinyWidgets[...],
  DT[...],
  dplyr[filter, ...],
  DBI[...],
  tibble[...],
  lubridate[...],
  hms[...],
  bsicons[...],
  plotly[...],
  ggplot2[...],
)


box::use(
  ../logic/app_data,
  ../logic/openai,
  ../logic/functions,
  ../modals/modals,
)

#' @export
Training_UI <- function(id, ag_level) {
  ns <- NS(id)

  if(ag_level == "Individual") {
    tagList(
      selectInput(ns("summary_firefighter"), "Firefighter", app_data$Firefighter$full_name),
      dateRangeInput(ns('training_filter_range'),
                     "Show trainings between:",
                     start = with_tz(Sys.time(), tzone = Sys.getenv('LOCAL_TZ')) - years(1),
                     end = with_tz(Sys.time(), tzone = Sys.getenv('LOCAL_TZ'))),
      actionButton(
        ns('set_last_quarter'),
        'Last Quarter'),
      actionButton(
        ns('set_rolling_year'),
        'Rolling Year'),
      downloadButton(ns("download"), "Download Firefighter Training Data",
                     class = 'btn-secondary'),
      hr(),
      actionButton(
        ns('choose_custom'),
        'Generate Custom Report',
        class = 'btn-primary',
        icon = icon('wand-magic-sparkles')
      )
    )
  } else {
    tagList(
      dateRangeInput(ns('training_filter_range'),
                     "Show trainings between:",
                     start = with_tz(Sys.time(), tzone = Sys.getenv('LOCAL_TZ')) - years(1),
                     end = with_tz(Sys.time(), tzone = Sys.getenv('LOCAL_TZ'))),
      hr(),
      actionButton(
        ns('choose_custom'),
        'Generate Custom Report',
        class = 'btn-primary',
        icon = icon('wand-magic-sparkles')
      )
    )
  }
}

#' @export
Training_Output <- function(id, ag_level) {
  ns <- NS(id)

  tagList(
    layout_columns(
      value_box("Total Training Hours",
                textOutput(ns("total_hours")),
                showcase = bs_icon('clock-fill',
                                   color = 'white')),
      layout_columns(
        value_box("EMS Hours",
                  textOutput(ns("ems_hours")),
                  showcase = bs_icon('activity',
                                     class = 'text-info')),
        value_box("Fire Hours",
                  textOutput(ns("fire_hours")),
                  showcase = bs_icon('fire',
                                     class = 'text-primary')),
        value_box("Wildland Hours",
                  textOutput(ns("wildland_hours")),
                  showcase = bs_icon('tree-fill',
                                     class = 'text-success')),
        value_box("Other Hours",
                  textOutput(ns("other_hours")),
                  showcase = bs_icon('question-circle-fill',
                                     class = 'text-light')),

        col_widths = c(6,6),
        row_widths = c(1,1)
      ),

      col_widths = c(4, 8),
      row_widths = c(1,1)
    ),
    card(
      plotlyOutput(ns("ff_hours_plot"))
    )

  )

}

#' @export
Training_Server <- function(id, ag_level) {
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
              c("Individual Attendance Percentage",
                "Department Attendance Percentage",
                "Actual Time vs. Scheduled Time")
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

      R_Training_Attendance <- reactive({

        # browser()

        #FIXME Haven't validated any of the training reports
        A <- app_data$Attendance |>
          left_join(app_data$Firefighter |> select(firefighter_id, full_name),
                    by = c("firefighter_id")) |>
          left_join(app_data$Training |> select(training_id, training_type, topic, training_description, start_time, end_time),
                    by = c("training_id" = "training_id")) |>
          select(-c(attendance_id, firefighter_id, training_id)) |>
          mutate(training_date = with_tz(start_time, tzone = Sys.getenv('LOCAL_TZ'))|> as.Date())  |>
          # Note, all three dates are in the local time zone
          filter(training_date >= input$training_filter_range[1] & training_date <= input$training_filter_range[2]) |>
          mutate(training_length = difftime(end_time, start_time, units = 'hours') |> as.numeric())

        # Filter to indiviual if that's the level of aggregation
        if(ag_level == "Individual") {
          A <- A |>
            filter(full_name == input$summary_firefighter)
        } else {
          A
        }

        return(A)

      })


      output$total_hours <- renderText({
        # browser()

        R_Training_Attendance() |>
          filter(credit == 1) |>
          select(training_length) |>
          sum()
      })

      output$ems_hours <- renderText({
        R_Training_Attendance() |>
          filter(training_type == "EMS" & credit == 1) |>
          select(training_length) |>
          sum()
      })

      output$fire_hours <- renderText({
        R_Training_Attendance() |>
          filter(training_type == "Fire" & credit == 1) |>
          select(training_length) |>
          sum()
      })

      output$wildland_hours <- renderText({
        R_Training_Attendance() |>
          filter(training_type == "Wildland" & credit == 1) |>
          select(training_length) |>
          sum()
      })

      output$other_hours <- renderText({
        R_Training_Attendance() |>
          filter(training_type == "Other" & credit == 1) |>
          select(training_length) |>
          sum()
      })

      output$ff_hours_plot <- renderPlotly({
        # browser()
        # Assuming your dataframe has a "date" column
        Training_Data <- R_Training_Attendance() %>%
          mutate(Month = format(as.Date(training_date), "%Y-%m")) |>
          filter(credit == 1)

        # Generate a complete set of months
        all_months <- expand.grid(training_type = unique(Training_Data$training_type),
                                  Month = unique(Training_Data$Month),
                                  stringsAsFactors = FALSE)

        # Merge with the training data to fill in missing months with zeros
        plot_data <- merge(all_months, Training_Data, by = c("training_type", "Month"), all.x = TRUE) %>%
          mutate(training_length = ifelse(is.na(training_length), 0, training_length)) |>
          group_by(training_type, Month) %>%
          summarise(Total_Length = sum(training_length))

        # Create the plot with specified colors, legend, and hover text
        plot <- plot_ly(plot_data,
                        x = ~Month,
                        y = ~Total_Length,
                        color = ~training_type,
                        type = 'scatter',
                        mode = 'lines',
                        colors = c("EMS" = "#3498DB", "Fire" = "#E74C3C", "Wildland" = "#00BC8C", "Other" = '#ADB5BD'),
                        text = ~paste("Total Hours: ", Total_Length, " hours")) %>%
          layout(title = "Training Summary",
                 xaxis = list(title = "Month",
                              titlefont = list(color = '#FFFFFF'),
                              tickfont = list(color = '#FFFFFF'),
                              gridcolor = '#2d2d2d'),
                 yaxis = list(title = "Training Length (hours)",
                              titlefont = list(color = '#FFFFFF'),
                              tickfont = list(color = '#FFFFFF'),
                              gridcolor = '#2d2d2d',
                              zeroline = FALSE),
                 plot_bgcolor = '#222222',
                 paper_bgcolor = '#222222',
                 font = list(color = '#FFFFFF'),
                 showlegend = TRUE)

        plot
      })

      # Data Download
      R_Data_Download <- reactive({
        print('Generating Data Download')
        R_Training_Attendance() |>
          select(full_name, training_type, topic,
                 check_in, check_out, auto_checkout,
                 training_length, training_description)
      })

      # Download Handler
      output$download <- downloadHandler(


        filename = function() {
          name <- paste0(input$summary_firefighter,
                         "-training-data_", Sys.Date(), ".csv")
          name <- gsub(" ", "-", name)
          return(name)
        },
        content = function(file) {
          utils::write.csv(R_Data_Download(), file)
        }
      )
    }
  )
}

#' @export
AI_UI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      min_height = '75%',
      title = 'Answer',
      card_body(
        shinycssloaders::withSpinner(gt_output(ns('table'))),
        textOutput(ns('answer'))
      )
    ),
    card(
      title = 'Question',
      card_body(
        textInput(ns('question'),
                  'Question',
                  width = '100%',
                  placeholder = 'Ask a question'),
        actionButton(
          ns('submit'),
          'Submit',
          class = 'btn-primary'
                     )
      )
    ),
    accordion(
      open = FALSE,
      accordion_panel(
        title = 'SQL',
        textOutput(ns('sql'))
      )
    )
  )
}


#' @export
AI_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      ##### AI Reports ####
      openai_response <- reactiveVal()

      sql <- reactiveVal()

      observeEvent(input$submit, {
        # browser()
        updateTextInput(session, 'question', value = '')

        openai_response(openai$chat$chat(
          input$question
        ))
      })

      output$table <- render_gt({
        req(input$submit)
        # browser()

        theme <- bs_get_variables(session$getCurrentTheme(),
                                         c("bg", "fg", "primary", "secondary"))



        sql(stringr::str_extract(openai_response(), "SELECT[\\s\\S]*?;"))

        #FIXME Using this to capture some of the queries and response. DOn't include in prod (probably)
        query <- DBI::sqlInterpolate(
          app_data$CON,
          'INSERT INTO ai_response_log (prompt, response) VALUES (?query, ?response)',
          query = input$question,
          response = openai_response()
        )

        DBI::dbExecute(app_data$CON, query)

        if(is.na(sql())) {
          return(gt(data.frame()))
        }

        # Check if the SQL contains any unwanted keywords
        if (grepl("\\b(DELETE|DROP|UPDATE|INSERT|ALTER|TRUNCATE)\\b", sql(), ignore.case = TRUE)) {
          logger::log_error("Unsafe SQL detected! The AI is taking over!")
          stop()
        } else {

          table <- DBI::dbGetQuery(app_data$CON, sql()) |>
            functions$FixColNames()

          gt(table) |>
            tab_options(
              table.background.color = theme['bg'],
              table.font.color = theme['fg'],
              heading.background.color = theme['primary'],
              column_labels.background.color = theme['secondary'],
              column_labels.font.weight = "bold",
              table.border.top.color = theme['primary'],
              table.border.bottom.color = theme['primary']
            )


        }


      }) |>
        bindEvent(input$submit)

      output$sql <- renderText({
        req(input$submit)
        sql()
      })

      output$answer <- renderText({

        # browser()
        req(input$submit)

        stringr::str_replace(openai_response(), "```[\\s\\S.]*```","") |> stringr::str_trim()
      })

    }
  )
}


