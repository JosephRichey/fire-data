# An app that allows officers to add, manage, and record training
# and roster information. This also includes an analysis pane.

box::use(
  shiny[...],
  bslib[...],
  DT[...],
  DBI[dbDisconnect],
)

box::use(
  view/training,
  view/roster,
  view/summary,
  logic/app_data,
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    title = paste0(Sys.getenv('FD'), " - Data Portal"),
    theme = bs_theme(version = 5,
                     success = "#87292b",
                     primary = '#ae3537',
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

    nav_panel(title = "Training Summary",
              navset_pill(
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
    helpText("v0.3.0 © CC BY-NC-SA 2024 Joseph Richey")

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

    # Disconnect from the database on app close
    session$onSessionEnded(function() {
      DBI::dbDisconnect(app_data$CON)
      print('Disconnected from database.')
    })

  })
}
