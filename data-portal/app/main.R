box::use(
  shiny[...],
  bslib[...],
  DT[...],
  DBI[dbDisconnect, dbGetQuery],
  shiny.router[...],
  fontawesome[...],
  bsicons[...],
)

box::use(
  view/training,
  view/personnel,
  view/summary,
  view/incident,
  view/equipment_management,
  logic/app_data,
  logic/logging,
  view/report,
  view/report_incident,
  view/report_personnel,
  view/report_equipment,
  view/messaging,
)

menu <- tags$ul(
  class = "nav-icons",
  tags$li(a(href = route_link("/"), fa("book-open", title = "Training"), class = 'active')),
  tags$li(a(href = route_link("incident"), fa("calendar-check", title = "Incident"))),
  tags$li(a(href = route_link("equipment"), fa("fire-extinguisher", title = "Equipment Management"))),
  tags$li(a(href = route_link("reports"), fa("chart-pie", title = "reports"))),
  tags$li(a(href = route_link("personnel"), fa("user", title = "Personnel"))),
  tags$li(a(href = route_link("messaging"), fa("envelope", title = "Messaging"))),
  tags$li(a(href = route_link("settings"), fa("cogs", title = "Settings")))
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    title = paste0(Sys.getenv('FD'), " - Data Portal"),

    # Theme
    theme = bs_theme(version = 5,
                     primary = "#87292b",
                     secondary = '#a05050',
                     success = "#2b8764",
                     info = "#377eb8",
                     warning = "#D76F33",
                     danger = "#892A6B",#"#9933CC",
                     light = "#565656",
                     "accordion-button-active-bg" = "#87292b",
                     "accordion-button-active-color" = "white",
                     "nav-underline-link-active-color" = "#2b8764",
                     "nav-link-color" = "white",
                     "nav-link-hover-color" = "#a05050",
                     "btn-hover-border-shade-amount" = '100%',
                     bootswatch = "darkly"),

    # Sidebar with icons
    div(class = "nav-icons", menu),

    # Header
    div(class = "app-header",
        img(src = 'static/logo.png',
            style = 'width: 40px; margin-right: 10px'),
        span(Sys.getenv('FD'), " - Data Portal")),

    # Main content
    div(class = "main-content",
        router_ui(

          route("/",
                navset_pill(
                  nav_panel(
                    title = "Trainings",
                    layout_sidebar(
                      sidebar = sidebar(
                        open = "desktop",
                        training$TrainingUI(ns('training_page'))
                      ),
                      training$TrainingOutput(ns('training_page'))
                    )
                  ),

                  nav_panel(
                    title = "Attendance",
                    layout_sidebar(
                      sidebar = sidebar(
                        open = "desktop",
                        training$AttendanceUI(ns('training_page'))
                      ),
                      training$AttendanceOutput(ns('training_page'))
                    )
                  )

                  # nav_panel(
                  #   title = "Attendance 2",
                  #   layout_sidebar(
                  #     sidebar = sidebar(
                  #       open = "desktop",
                  #       training$AttendanceUITest(ns('training_page'))
                  #     ),
                  #     training$AttendanceOutputTest(ns('training_page'))
                  #   )
                  # )
                )
          ),
          route("incident",
                layout_sidebar(
                  sidebar = sidebar(
                    open = "desktop",
                    incident$UI(ns('incident'))
                  ),
                    incident$Output(ns('incident'))
                )
                ),
          route("equipment",
                navset_pill(
                  nav_panel(
                    title = "Equipment Checks",
                    layout_sidebar(
                      sidebar = sidebar(

                        open = "desktop",
                        equipment_management$Checks_UI(ns('equipment')),
                      ),
                      equipment_management$Checks_Output(ns('equipment'))
                    )
                  ),

                  nav_panel(
                    title = "Equipment Expiration",
                    layout_sidebar(
                      sidebar = sidebar(

                        open = "desktop",
                        equipment_management$Expiration_UI(ns('equipment')),
                      ),
                      equipment_management$Expiration_Output(ns('equipment'))
                    )
                  ),

                  nav_panel(
                    title = "Manage Equipment",
                    layout_sidebar(
                      sidebar = sidebar(

                        open = "desktop",
                        equipment_management$Manage_Equipment_UI(ns('equipment')),
                      ),

                      equipment_management$Manage_Equipment_Output(ns('equipment'))
                    )
                  ),
                  nav_panel(
                    title = "Manage Equipment Type",
                    layout_sidebar(
                      sidebar = sidebar(

                        open = "desktop",
                        equipment_management$Manage_Equipment_Type_UI(ns('equipment')),
                      ),

                      equipment_management$Manage_Equipment_Type_Output(ns('equipment'))
                    )
                  ),
                )
                ),
          route("reports",
            navset_pill(
              nav_panel(
                title = "Trainings",
                navset_card_underline(
                  nav_panel(
                    title = "Individual",
                    layout_sidebar(
                      sidebar = sidebar(
                        title = "Set Filters",
                        report$Training_UI(ns('ind_training'), "Individual")
                      ),
                      report$Training_Output(ns('ind_training'), "Individual")
                    )
                  ),
                  nav_panel(
                    title = "Department",
                    layout_sidebar(
                      sidebar = sidebar(
                        title = "Set Filters",
                        report$Training_UI(ns('dep_training'), "Department")
                      ),
                      report$Training_Output(ns('dep_training'), "Department")
                    )
                  )
                )
               ),

              nav_panel(title = "Incidents",
                        layout_sidebar(
                          sidebar = sidebar(
                            title = "Set Filters",
                            report_incident$UI(ns('incident'))
                            ),
                            report_incident$Output(ns('incident'))
                        )
              ),

              nav_panel(title = "Equipment",
                        layout_sidebar(
                          sidebar = sidebar(
                            title = "Set Filters",
                            report_equipment$UI(ns('equipment'))

                          ),
                          report_equipment$Output(ns('equipment'))
                        )
              ),

              nav_panel(title = "Personnel",
                        layout_sidebar(
                          sidebar = sidebar(
                            report_personnel$UI(ns('personnel'))
                          ),
                          report_personnel$Output(ns('personnel'))
                        )
              ),

              nav_panel(title = "AI Reports",
                        report$AI_UI(ns('report'))
              )
            )
          ),
          route("personnel",
                navset_pill(
                  nav_panel(
                    title = "Roster",
                    layout_sidebar(
                      sidebar = sidebar(

                        open = "desktop",
                        personnel$Roster_UI(ns('personnel')),
                      ),

                      personnel$Roster_Output(ns('personnel'))

                    )
                  ),

                  nav_panel(
                    title = 'Certifications',
                    layout_sidebar(
                      sidebar = sidebar(
                        open = "desktop",
                        personnel$Certification_UI(ns('personnel')),
                      ),
                      personnel$Certification_Output(ns('personnel'))
                    )
                  ),

                  nav_panel(
                    title = 'Org Chart',

                    personnel$Org_Chart_Output(ns('personnel'))
                  )

                )


          ),
          route("messaging",
                layout_sidebar(
                  sidebar = sidebar(
                    open = "desktop",
                    messaging$UI(ns('messaging'))
                  ),
                  card(
                    messaging$Output(ns('messaging'))
                  )
                )
                ),
          route("settings",
                tagList(
                  actionButton("input1", "Change Password",
                               class = 'btn-primary',
                               style = 'margin-bottom: 10px'
                               ),
                  actionButton("input2", "Change Main Settings",
                               class = 'btn-primary',
                               style = 'margin-bottom: 10px'
                               )
                )
              )
          ),

    nav_spacer(),
    helpText("dev-1.0.0 © CC BY-NC-SA 2024 Joseph Richey")
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    shinycssloaders::showPageSpinner(
      type = 6,
      color = "#87292b"
    )

    ##### Global Stuff #####
    ns <- session$ns

    training$Server('training_page')

    personnel$Server('personnel')

    incident$Server('incident')

    equipment_management$Server('equipment')

    equipment_management$Manage_Equipment_Server('equipment')

    report$AI_Server('report')

    report$Training_Server('ind_training', 'Individual')

    report$Training_Server('dep_training', 'Department')

    report_incident$Server('incident')

    report_personnel$Server('personnel')

    report_equipment$Server('equipment')

    messaging$Server('messaging')

    router_server()

    # Keep DB connection live
    observe({
      invalidateLater(180000)  # Ping every 3 minutes
      cat('Pinging database...\n', file = stderr())
      tryCatch(
        dbGetQuery(app_data$CON, "SELECT 1"),
        error = function(e) message("Connection ping failed: ", e$message)
      )
    })


    # Disconnect from the database on app close
    session$onSessionEnded(function() {
      DBI::dbDisconnect(app_data$CON)
      print('Disconnected from database.')
    })

    session$onFlushed(function() {
      shinycssloaders::hidePageSpinner()
    })

  })
}
