# An app that allows officers to add, manage, and record training
# and roster information. This also includes an analysis pane.


# 56.75 Hours

box::use(
  shiny[...],
  bslib[...],
  DT[...],
)

box::use(
  view/training,
  view/roster,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    title = "Corinne Fire Department",
    theme = bs_theme(version = 5,
                     success = "#87292b",
                     bootswatch = "darkly"),
    nav_panel(title = "Training",
              layout_sidebar(
                sidebar = sidebar(
                  width = 400,
                  open = "desktop",
                  training$UI(ns('training'))
                ),
                training$Output(ns('training'))
              )
    ),

    nav_panel(title = "Manage Roster",
              layout_sidebar(
                sidebar = sidebar(
                  width = 400,
                  open = "desktop",
                  roster$UI(ns('roster')),
                ),

                roster$Output(ns('roster'))

              )

    ),
  #   nav_panel(title = "Training Summary",
  #             navset_pill(
  #               nav_panel(title = "Individual",
  #                         layout_sidebar(
  #                           sidebar = sidebar(
  #                             title = "Set Filters",
  #                             selectInput("summary_firefighter", "Firefighter", Roster$full_name),
  #                             dateRangeInput('ind_training_filter_range',
  #                                            "Show trainings between:",
  #                                            start = as.Date(paste0(year(Sys.Date()), "-01-01")),
  #                                            end = as.Date(paste0(year(Sys.Date()), "-12-31"))),
  #                             downloadButton("download_ind", "Download Firefighter Training Summary")
  #                           ),
  #                           layout_columns(
  #                             value_box("EMS Hours", textOutput("ff_ems_hours")),
  #                             value_box("Fire Hours", textOutput("ff_fire_hours")),
  #                             value_box("Wildland Hours", textOutput("ff_wildland_hours")),
  #                           ),
  #                           card(
  #                             plotlyOutput("ff_hours_plot")
  #                           )
  #                         )
  #               ),
  #
  #               nav_panel(title = "Department",
  #                         layout_sidebar(
  #                           sidebar = sidebar(
  #                             title = "Set Filters",
  #                             dateRangeInput('dep_training_filter_range',
  #                                            "Show trainings between:",
  #                                            start = as.Date(paste0(year(Sys.Date()), "-01-01")),
  #                                            end = as.Date(paste0(year(Sys.Date()), "-12-31"))),
  #                             downloadButton("download_dep", "Download Department Training Summary")
  #                           ),
  #                           value_box("Total Training Hours", textOutput("dep_total_hours")),
  #                           layout_columns(
  #                             value_box("EMS Hours", textOutput("dep_ems_hours")),
  #                             value_box("Fire Hours", textOutput("dep_fire_hours")),
  #                             value_box("Wildland Hours", textOutput("dep_wildland_hours")),
  #                           ),
  #                           card(
  #                             plotlyOutput("dep_hours_plot")
  #                           )
  #                         )
  #               )
  #             )
  #   ),
    nav_spacer(),
    nav_menu(
      title = "Settings",
      align = "right",
      nav_item(actionButton(ns("sign_out"), "Lock"), align = "center"),
      nav_item(helpText("v0.1.1"), align = "center")
    )

  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ##### Global Stuff #####
    # Use reactiveValues to maintain a local copy of the roster that is available at all times.
    # Update the local copy whenever the stored copy is updated.
    # MyReactives <- reactiveValues()
    # MyReactives$roster <- board %>% pin_read("roster") %>%
    #   dplyr::mutate(start_date = as.Date(start_date))
    # MyReactives$Training <- board %>% pin_read("trainings") %>%
    #   dplyr::mutate(date = as.Date(date))

    ns <- session$ns
    # Sign in/out capabilities
    observeEvent(input$sign_out, {
      showModal(modalDialog(
        textInput(ns("username"), "Username"),
        passwordInput(ns("password"), "Password"),
        title = "Unlock",
        footer = tagList(
          actionButton(ns("sign_in"), "Unlock")
        )
      ))
    }, ignoreNULL = FALSE)

    # Check password and username
    observeEvent(input$sign_in, {
      # browser()
      if(input$username == "CFD" && input$password == "1975") {
        removeModal()
      } else {
        stopApp()
      }
    })

    training$Server('training')

    roster$Server('roster')


    # ###### Manage Roster ######


    # ##### Ind Training Summary ####
    # R_Training_Data <- reactive({
    #   Filtered_Trainings <- MyReactives$trainings |>
    #     filter(delete == FALSE &
    #              date > input$ind_training_filter_range[1] &
    #              date < input$ind_training_filter_range[2])
    #
    #   Filtered_Roster <- MyReactives$roster |>
    #     filter(active_status == TRUE) |>
    #     mutate(full_name = paste(first_name, last_name))
    #
    #   Attendance |>
    #     left_join(Filtered_Roster) |>
    #     left_join(Filtered_Trainings) |>
    #     filter(!is.na(delete) & !is.na(active_status))
    #
    # })
    #
    # output$ff_ems_hours <- renderText({
    #   Data <- R_Training_Data() |>
    #     filter(full_name == input$summary_firefighter) |>
    #     filter(training_type == "EMS")
    #
    #   paste(sum(Data$training_length))
    # })
    #
    # output$ff_fire_hours <- renderText({
    #   Data <- R_Training_Data() |>
    #     filter(full_name == input$summary_firefighter) |>
    #     filter(training_type == "Fire")
    #
    #   paste(sum(Data$training_length))
    # })
    #
    # output$ff_wildland_hours <- renderText({
    #   Data <- R_Training_Data() |>
    #     filter(full_name == input$summary_firefighter) |>
    #     filter(training_type == "Wildland")
    #
    #   paste(sum(Data$training_length))
    # })
    #
    # output$ff_hours_plot <- renderPlotly({
    #
    #   # Assuming your dataframe has a "date" column
    #   R_Training_Data <- R_Training_Data() %>%
    #     mutate(Month = format(as.Date(date), "%Y-%m")) |>
    #     filter(full_name == input$summary_firefighter)
    #
    #   # Generate a complete set of months
    #   all_months <- expand.grid(training_type = unique(R_Training_Data$training_type),
    #                             Month = unique(R_Training_Data$Month),
    #                             stringsAsFactors = FALSE)
    #
    #   # Merge with the training data to fill in missing months with zeros
    #   plot_data <- merge(all_months, R_Training_Data, by = c("training_type", "Month"), all.x = TRUE) %>%
    #     mutate(training_length = ifelse(is.na(training_length), 0, training_length)) |>
    #     group_by(training_type, Month) %>%
    #     summarise(Total_Length = sum(training_length))
    #
    #   # Create the plot with specified colors, legend, and hover text
    #   plot <- plot_ly(plot_data, x = ~Month, y = ~Total_Length, color = ~training_type,
    #                   type = 'scatter', mode = 'lines', colors = c("blue", "red", "green"),
    #                   text = ~paste("Total Hours: ", Total_Length, " hours")) %>%
    #     layout(title = "Training Summary",
    #            xaxis = list(title = "Month"),
    #            yaxis = list(title = "Training Length (hours)", zeroline = FALSE),
    #            showlegend = TRUE)
    #
    #   plot
    # })
    #
    # # Individual Data Download
    # R_Ind_Data_Download <- reactive({
    #   R_Training_Data() |>
    #     filter(full_name == input$summary_firefighter) |>
    #     select(full_name, training_type, topic, training_length, description, date)
    # })
    #
    # # Download Handler
    # output$download_ind <- downloadHandler(
    #   filename = function() {
    #     paste0(input$summary_firefighter, "-training-data-", Sys.Date(), ".csv")
    #   },
    #   content = function(file) {
    #     write.csv(R_Ind_Data_Download(), file)
    #   }
    # )
    #
    # ##### Dep Training Summary ####
    # R_Dep_Training_Data <- reactive({
    #   Filtered_Trainings <- MyReactives$trainings |>
    #     filter(delete == FALSE &
    #              date > input$dep_training_filter_range[1] &
    #              date < input$dep_training_filter_range[2])
    #
    #   Filtered_Roster <- MyReactives$roster |>
    #     filter(active_status == TRUE) |>
    #     mutate(full_name = paste(first_name, last_name))
    #
    #   Attendance |>
    #     left_join(Filtered_Roster) |>
    #     left_join(Filtered_Trainings) |>
    #     filter(!is.na(delete) & !is.na(active_status))
    #
    # })
    #
    #
    # output$dep_ems_hours <- renderText({
    #   Data <- R_Dep_Training_Data() |>
    #     filter(training_type == "EMS")
    #
    #   paste(sum(Data$training_length))
    # })
    #
    # output$dep_fire_hours <- renderText({
    #   Data <- R_Dep_Training_Data() |>
    #     filter(training_type == "Fire")
    #
    #   paste(sum(Data$training_length))
    # })
    #
    # output$dep_wildland_hours <- renderText({
    #   Data <- R_Dep_Training_Data() |>
    #     filter(training_type == "Wildland")
    #
    #   paste(sum(Data$training_length))
    # })
    #
    # output$dep_total_hours <- renderText({
    #   Data <- R_Dep_Training_Data()
    #
    #   paste(sum(Data$training_length))
    # })
    #
    # output$dep_hours_plot <- renderPlotly({
    #
    #   # Assuming your dataframe has a "date" column
    #   R_Training_Data <- R_Dep_Training_Data() %>%
    #     mutate(Month = format(as.Date(date), "%Y-%m"))
    #
    #   # Generate a complete set of months
    #   all_months <- expand.grid(training_type = unique(R_Training_Data$training_type),
    #                             Month = unique(R_Training_Data$Month),
    #                             stringsAsFactors = FALSE)
    #
    #   # Merge with the training data to fill in missing months with zeros
    #   plot_data <- merge(all_months, R_Training_Data, by = c("training_type", "Month"), all.x = TRUE) %>%
    #     mutate(training_length = ifelse(is.na(training_length), 0, training_length)) |>
    #     group_by(training_type, Month) %>%
    #     summarise(Total_Length = sum(training_length))
    #
    #   # Create the plot with specified colors, legend, and hover text
    #   plot <- plot_ly(plot_data, x = ~Month, y = ~Total_Length, color = ~training_type,
    #                   type = 'scatter', mode = 'lines', colors = c("blue", "red", "green"),
    #                   text = ~paste("Total Hours: ", Total_Length, " hours")) %>%
    #     layout(title = "Training Summary",
    #            xaxis = list(title = "Month"),
    #            yaxis = list(title = "Training Length (hours)", zeroline = FALSE),
    #            showlegend = TRUE)
    #
    #   plot
    # })
    #
    # # Individual Data Download
    # R_Dep_Data_Download <- reactive({
    #   R_Dep_Training_Data() |>
    #     select(full_name, training_type, topic, training_length, description, date)
    # })
    #
    # # Download Handler
    # output$download_dep <- downloadHandler(
    #   filename = function() {
    #     paste0("cfd-training-data-", Sys.Date(), ".csv")
    #   },
    #   content = function(file) {
    #     write.csv(R_Dep_Data_Download(), file)
    #   }
    # )

  })
}
