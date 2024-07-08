# An app that allows officers to add, manage, and record training
# and roster information. This also includes an analysis pane.

box::use(
  shiny[...],
  bslib[...],
  DT[...],
  DBI[dbDisconnect],
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
    title = paste0("Corinne Fire Department", " - Data Portal"),
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

    # nav_panel(title = "Attendance Management",
    #             navset_pill(
    #               nav_panel(title = "Training",
    #                 layout_sidebar(
    #                   sidebar = sidebar(
    #                     width = 400,
    #                     open = 'desktop',
    #                     actionButton('add_missing_attendance',
    #                                  'Add Missing Attendance'),
    #                     actionButton('delete_attendance',
    #                                  'Delete Attendance'),
    #                     actionButton('submit_changes',
    #                                  'Submit Changes')
    #                   ),
    #                   card(
    #                     height = 600,
    #                     card_body(
    #                     app_data$Attendance |>
    #                       dplyr::left_join(app_data$Firefighter) |>
    #                       dplyr::left_join(app_data$Training) |>
    #                       dplyr::select(training_type, training_topic,
    #                                     firefighter_full_name,
    #                                     check_in, check_out) |>
    #                       DT::datatable(editable = TRUE)
    #                     )
    #                   )
    #
    #                 ),
    #               ),
    #               nav_panel(title = "Calls"
    #
    #               )
    #             ),
    #           ),

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

    # nav_panel(title = "Training Summary",
    #           navset_pill(
    #             # bg = "#87292b",
    #             nav_panel(title = "Individual",
    #                       layout_sidebar(
    #                         sidebar = sidebar(
    #                           title = "Set Filters",
    #                           summary$UI(ns('ind_summary'), "Individual")
    #                           ),
    #                         summary$Output(ns('ind_summary'), "Individual")
    #                         )
    #                       ),
    #
    #             nav_panel(title = "Department",
    #                       layout_sidebar(
    #                         sidebar = sidebar(
    #                           title = "Set Filters",
    #                           summary$UI(ns('dep_summary'), "Department")
    #                           ),
    #                         summary$Output(ns('dep_summary'), "Department")
    #                         )
    #                       )
    #             )
    #
    #
    # ),
    nav_spacer(),
    nav_menu(
      title = "Settings",
      align = "right"#,
      # nav_item(actionButton(ns("sign_out"), "Lock"), align = "center"),
      # nav_item(helpText("v0.2.0-beta"), align = "center")
    ),
    helpText("v0.2.2 © CC BY-NC-SA 2024 Joseph Richey")

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

    # Disconnect from the database on app close
    session$onSessionEnded(function() {
      DBI::dbDisconnect(app_data$CON)
      print('Disconnected from database.')
    })

  })
}
