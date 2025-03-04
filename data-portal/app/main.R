box::use(
  shiny[...],
  bslib[...],
  DT[...],
  DBI[dbDisconnect],
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
  page_fillable(
    title = paste0(Sys.getenv('FD'), " - Data Portal"),

    # Theme
    theme = bs_theme(version = 5,
                     primary = "#87292b",
                     secondary = '#a05050',
                     success = "#2b8764",
                     info = "#377eb8",
                     warning = "#D76F33",
                     danger = "#9933CC",
                     light = "#565656",
                     "accordion-button-active-bg" = "#87292b",
                     "accordion-button-active-color" = "white",
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
                layout_sidebar(
                  sidebar = sidebar(
                    title = "Set Filters",
                    summary$UI(ns('ind_summary'), "Individual")
                    ),
                  summary$Output(ns('ind_summary'), "Individual")
                  )
                ),

              nav_panel(title = "Incidents",
                        layout_sidebar(
                          sidebar = sidebar(
                            title = "Set Filters",

                            ),
                            value_box("Total EMS Incident Hours",
                                      textOutput("1000"),
                                      showcase = bs_icon('clock-fill',
                                                         class = 'text-success')),
                            value_box("Total Fire Incident Hours",
                                      textOutput("1001"),
                                      showcase = bs_icon('clock-fill',
                                                         class = 'text-danger')),
                        )
              ),

              nav_panel(title = "Equipment",
                        layout_sidebar(
                          sidebar = sidebar(
                            title = "Set Filters"

                          )
                        )
              ),

              nav_panel(title = "Personnel",
                        layout_sidebar(
                          sidebar = sidebar(
                            title = "Set Filters"

                          )
                        )
              ),

              nav_panel(title = "AI Reports",
                        layout_sidebar(
                          sidebar = sidebar(
                            title = "Set Filters"

                          )
                        )
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
                    title = "Certifications",
                    layout_sidebar(
                      sidebar = sidebar(

                        open = "desktop",
                        actionButton("input6", "Add Cert"),
                        actionButton("input1", "Add Cert Type"),
                        hr(),
                        actionButton("input3", "Edit Cert"),
                        actionButton("input2", "Edit Cert Type"),
                        actionButton("input4", "Renew Cert"),
                        hr(),
                        actionButton("input5", "Delete Cert"),
                        actionButton("input7", "Delete Cert Type"),
                      ),
                      datatable(
                        data.frame(
                          ID = 1:10,
                          Name = letters[1:10],
                          Type = rep(c("A", "B"), 5)
                        )

                      ),
                      datatable(
                        data.frame(
                          ID = 1:10,
                          Name = letters[1:10],
                          Type = rep(c("A", "B"), 5)
                        )

                      )
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
                    actionButton("input1", "Copy Email Addresses"),
                    actionButton("input2", "Copy Phone Numbers"),
                  ),
                    textInput("input1", "Recipient", value = ""),
                    textInput("input2", "Subject", value = ""),
                    textAreaInput("input3", "Message", value = ""),
                    actionButton("input4", "Send Message")
                )
                ),
          route("settings",
                tagList(
                  actionButton("input1", "Change Password"),
                  actionButton("input2", "Change Main Settings")
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

    ##### Global Stuff #####
    ns <- session$ns

    training$Server('training_page')

    personnel$Server('personnel')

    incident$Server('incident')

    equipment_management$Server('equipment')

    equipment_management$Manage_Equipment_Server('equipment')

    # summary$Server('ind_summary', 'Individual')

    # summary$Server('dep_summary', 'Department')

    router_server()

    output$org_chart <- renderPlot({
      # Plot using ggraph
      # browser()

      return(ggraph(graph, layout = "tree") +
        geom_edge_link() +  # No arrows specified
        geom_node_point(ggplot2::aes(color = team), size = 6) +
        geom_node_text(ggplot2::aes(label = paste(name, "\n", role), size = 5)) +
        theme_minimal() +
        theme(
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          legend.position = "bottom"
        ) +
        labs(color = "Team"))
    })

    # Disconnect from the database on app close
    session$onSessionEnded(function() {
      DBI::dbDisconnect(app_data$CON)
      print('Disconnected from database.')
    })

  })
}
