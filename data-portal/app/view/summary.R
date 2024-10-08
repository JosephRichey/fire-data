box::use(
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
  ../logic/functions,
  ../modals/modals,
)

#' @export
UI <- function(id, ag_level) {
  ns <- NS(id)

  if(ag_level == "Individual") {
    tagList(
      selectInput(ns("summary_firefighter"), "Firefighter", app_data$Firefighter$firefighter_full_name),
      dateRangeInput(ns('training_filter_range'),
                     "Show trainings between:",
                     start = with_tz(Sys.time(), tzone = Sys.getenv('LOCAL_TZ')) - years(1),
                     end = with_tz(Sys.time(), tzone = Sys.getenv('LOCAL_TZ'))),
      downloadButton(ns("download"), "Download Firefighter Training Data")
    )
  } else {
    tagList(
      dateRangeInput(ns('training_filter_range'),
                     "Show trainings between:",
                     start = with_tz(Sys.time(), tzone = Sys.getenv('LOCAL_TZ')) - years(1),
                     end = with_tz(Sys.time(), tzone = Sys.getenv('LOCAL_TZ')))
    )
  }
}

#' @export
Output <- function(id, ag_level) {
  ns <- NS(id)

  tagList(
    layout_columns(
      value_box("Total Training Hours",
                textOutput(ns("total_hours")),
                showcase = bs_icon('clock-fill',
                                   class = 'text-success')),
      layout_columns(
        value_box("EMS Hours",
                  textOutput(ns("ems_hours")),
                  showcase = bs_icon('activity',
                                     class = 'text-info')),
        value_box("Fire Hours",
                  textOutput(ns("fire_hours")),
                  showcase = bs_icon('fire',
                                     class = 'text-danger')),
        value_box("Wildland Hours",
                  textOutput(ns("wildland_hours")),
                  showcase = bs_icon('tree-fill',
                                     color = '#00BC8C')),
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
Server <- function(id, ag_level) {
  moduleServer(
    id,
    function(input, output, session) {

      R_Data <- reactive({

        A <- app_data$Attendance |>
          left_join(app_data$Firefighter |> select(firefighter_id, firefighter_full_name),
                    by = c("firefighter_id" = "firefighter_id")) |>
          left_join(app_data$Training |> select(training_id, training_type, training_topic, training_description, training_start_time, training_end_time),
                    by = c("training_id" = "training_id")) |>
          select(-c(attendance_id, firefighter_id, training_id)) |>
          mutate(training_date = with_tz(training_start_time, tzone = Sys.getenv('LOCAL_TZ'))|> as.Date())  |>
          # Note, all three dates are in the local time zone
          filter(training_date >= input$training_filter_range[1] & training_date <= input$training_filter_range[2]) |>
          mutate(training_length = difftime(training_end_time, training_start_time, units = 'hours') |> as.numeric())

        # Filter to indiviual if that's the level of aggregation
        if(ag_level == "Individual") {
          A <- A |>
            filter(firefighter_full_name == input$summary_firefighter)
        } else {
          A
        }

        return(A)

      })


      output$total_hours <- renderText({
        # browser()

        R_Data() |>
          filter(credit == 1) |>
          select(training_length) |>
          sum()
      })

      output$ems_hours <- renderText({
        R_Data() |>
          filter(training_type == "EMS" & credit == 1) |>
          select(training_length) |>
          sum()
      })

      output$fire_hours <- renderText({
        R_Data() |>
          filter(training_type == "Fire" & credit == 1) |>
          select(training_length) |>
          sum()
      })

      output$wildland_hours <- renderText({
        R_Data() |>
          filter(training_type == "Wildland" & credit == 1) |>
          select(training_length) |>
          sum()
      })

      output$other_hours <- renderText({
        R_Data() |>
          filter(training_type == "Other" & credit == 1) |>
          select(training_length) |>
          sum()
      })

      output$ff_hours_plot <- renderPlotly({
        # browser()
        # Assuming your dataframe has a "date" column
        Training_Data <- R_Data() %>%
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
        R_Data() |>
          select(firefighter_full_name, training_type, training_topic,
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



