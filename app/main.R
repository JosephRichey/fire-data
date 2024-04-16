# An app that allows officers to add, manage, and record training
# and roster information. This also includes an analysis pane.

box::use(
  shiny[...],
  bslib[...],
  DT[...],
  # thematic[...],
)

box::use(
  view/training,
  view/roster,
  view/summary,
  logic/app_data,
)

# thematic_shiny()

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    title = paste0("Corinne Fire Department",Sys.getenv("TESTING"), " - Data Portal"),
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

    nav_panel(title = "Attendance Management",
                navset_pill(
                  nav_panel(title = "Training",
                    layout_sidebar(
                      sidebar = sidebar(
                        width = 400,
                        open = 'desktop',
                        actionButton('add_missing_attendance',
                                     'Add Missing Attendance'),
                        actionButton('delete_attendance',
                                     'Delete Attendance'),
                        actionButton('submit_changes',
                                     'Submit Changes')
                      ),
                      card(
                        height = 600,
                        card_body(
                        app_data$Attendance |>
                          dplyr::left_join(app_data$Firefighter) |>
                          dplyr::left_join(app_data$Training) |>
                          dplyr::select(training_type, training_topic, training_date,
                                        firefighter_full_name,
                                        check_in, check_out) |>
                          DT::datatable(editable = TRUE)
                        )
                      )

                    ),
                  ),
                  nav_panel(title = "Calls"

                  )
                ),
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

    nav_panel(title = "Training Summary",
              navset_pill(
                # bg = "#87292b",
                nav_panel(title = "Individual",
                          layout_sidebar(
                            sidebar = sidebar(
                              title = "Set Filters",
                              summary$UI(ns('ind_summary'), "Individual")
                              ),
                            summary$Output(ns('ind_summary'), "Individual")
                            )
                          ),

                nav_panel(title = "Department",
                          layout_sidebar(
                            sidebar = sidebar(
                              title = "Set Filters",
                              summary$UI(ns('dep_summary'), "Department")
                              ),
                            summary$Output(ns('dep_summary'), "Department")
                            )
                          )
                )


    ),
    nav_spacer(),
    nav_menu(
      title = "Settings",
      align = "right",
      # nav_item(actionButton(ns("sign_out"), "Lock"), align = "center"),
      nav_item(helpText("v0.2.0-beta"), align = "center")
    ),
    helpText("© CC BY-NC-SA 2024 Joseph Richey")

  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ##### Global Stuff #####
    ns <- session$ns
    # Sign in/out capabilities
    ## NOT MEANT TO BE SECURE
    # observeEvent(input$sign_out, {
    #   showModal(modalDialog(
    #     textInput(ns("username"), "Username"),
    #     passwordInput(ns("password"), "Password"),
    #     title = "Unlock",
    #     footer = tagList(
    #       actionButton(ns("sign_in"), "Unlock")
    #     )
    #   ))
    # }, ignoreNULL = FALSE)

    # Check password and username
    # observeEvent(input$sign_in, {
    #   # browser()
    #   if(input$username == "CFD" && input$password == "1975") {
    #     removeModal()
    #   } else {
    #     stopApp()
    #   }
    # })

    training$Server('training')

    roster$Server('roster')

    summary$Server('ind_summary', 'Individual')

    summary$Server('dep_summary', 'Department')

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
