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
      )

      # nav_panel(
      #   title = "Attendance",
      #   layout_sidebar(
      #     sidebar = sidebar(
      #       open = "desktop",
      #       attendance$AttendanceUI(ns('attendance_tab'))
      #     ),
      #     attendance$AttendanceOutput(ns('attendance_tab'))
      #   )
      # )

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
    )
}


#' @export
Server <- function(id, rdfs) {
  moduleServer(
    id,
    function(input, output, session) {

      training$Server('training_tab', rdfs)
      # attendance$Server('attendance_tab', rdfs)


      return(NULL)
    }
  )
}

