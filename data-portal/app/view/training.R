box::use(
  bslib[...],
  shiny[...],
  dplyr[...],
)


box::use(
  training_sub_modules/training,
  training_sub_modules/attendance,
)


#' @export
UI <- function(id) {
  ns <- NS(id)
  tagList(
    navset_pill(
      nav_panel(
        title = "Trainings",
        layout_sidebar(
          sidebar = sidebar(
            open = "desktop",
            training$UI(ns('training_tab'))
          ),
          training$Output(ns('training_tab'))
        )
      ),

      nav_panel(
        title = "Attendance",
        layout_sidebar(
          sidebar = sidebar(
            open = "desktop",
            attendance$UI(ns('attendance_tab'))
          ),
          attendance$Output(ns('attendance_tab'))
        )
      )
    )
    )
}


#' @export
Server <- function(id, rdfs) {
  moduleServer(
    id,
    function(input, output, session) {

      training$Server('training_tab', rdfs)
      attendance$Server('attendance_tab', rdfs)


      return(NULL)
    }
  )
}

