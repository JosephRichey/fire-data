# An app that allows officers to add, manage, and record training
# and roster information. This also includes an analysis pane.

box::use(
  shiny[...],
  bslib[...],
  DT[...],
  DBI[dbDisconnect],
  shiny.router[...],
  fontawesome[...],
)

box::use(
  view/training,
  view/roster,
  view/summary,
  logic/app_data,
)

menu <- tags$ul(
  class = "nav-icons",
  tags$li(a(href = route_link("/"), fa("book-open", title = "Training"), class = 'active')),
  tags$li(a(href = route_link("incident"), fa("calendar-check", title = "Incident"))),
  tags$li(a(href = route_link("equipment"), fa("fire-extinguisher", title = "Equipment Management"))),
  tags$li(a(href = route_link("reports"), fa("chart-pie", title = "reports"))),
  tags$li(a(href = route_link("roster"), fa("user", title = "Roster"))),
  tags$li(a(href = route_link("messaging"), fa("envelope", title = "Messaging"))),
  tags$li(a(href = route_link("settings"), fa("cogs", title = "Settings")))
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    title = paste0(Sys.getenv('FD'), " - Data Portal"),
    theme = bs_theme(version = 5,
                     primary = "#87292b",
                     secondary = '#a05050',
                     success = "#2b8764",
                     info = "#377eb8",
                     warning = "#D76F33",
                     danger = "#640064",
                     light = "#565656",
                     bootswatch = "darkly"),
    # Sidebar with icons
    div(class = "nav-icons", menu),
    div(class = "app-header",
        img(src = 'static/logo.png',
            style = 'width: 60px; margin-right: 10px'),
        span(Sys.getenv('FD'), " - Data Portal")),
    div(class = "main-content",
        router_ui(
          route("/",
                layout_sidebar(
                  sidebar = sidebar(
                    width = 400,
                    open = "desktop",
                    training$UI(ns('training'))
                  ),
                  training$Output(ns('training'))
                )
          ),
          # route("training",
          #       layout_sidebar(
          #                     sidebar = sidebar(
          #                       width = 400,
          #                       open = "desktop",
          #                       training$UI(ns('training'))
          #                     ),
          #                     training$Output(ns('training'))
          #                   )
          # ),
          route("incident",
                page_navbar(
                  title = "Incident",
                  sidebar = sidebar(
                    numericInput("input2", "Input 2", value = 0)
                  )
                )),
          route("equipment",
                page_navbar(
                  title = "Equipment Management",
                  sidebar = sidebar(
                    textInput("input3", "Input 3", value = "")
                  )
                )),
          route("reports",
            navset_pill(
              nav_panel(
                title = "Individual",
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
                            summary$UI(
                              ns('dep_summary'),
                              "Department")
                            ),
                          summary$Output(
                            ns('dep_summary'),
                            "Department")
                          )
                        )
              )
            ),
          route("roster",
                layout_sidebar(
                              sidebar = sidebar(
                                width = 400,
                                open = "desktop",
                                roster$UI(ns('roster')),
                              ),

                              roster$Output(ns('roster'))

                            )
          ),
          route("messaging",
                page_navbar(
                  title = "Messaging",
                  sidebar = sidebar(
                    dateInput("input6", "Input 6", value = Sys.Date())
                  )
                )),
          route("settings",
                page_navbar(
                  title = "Settings",
                  sidebar = sidebar(
                    dateInput("input7", "Input 7", value = Sys.Date())
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

    ##### Global Stuff #####
    ns <- session$ns

    training$Server('training')

    roster$Server('roster')

    summary$Server('ind_summary', 'Individual')

    summary$Server('dep_summary', 'Department')

    router_server()

    # Disconnect from the database on app close
    session$onSessionEnded(function() {
      DBI::dbDisconnect(app_data$CON)
      print('Disconnected from database.')
    })

  })
}
