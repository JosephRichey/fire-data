box::use(
  bslib[...],
  shiny[...],
  dplyr[...],
)


box::use(
  training_sub_modules/training,
  training_sub_modules/attendance,
)

# training_trainers <- app_data$Firefighter |>
#   filter(trainer == TRUE) |>
#   select(full_name, firefighter_id) |>
#   # https://ivelasq.rbind.io/blog/understanding-the-r-pipe/
#   # See "Getting to the solution" sect
#   (\(.) setNames(.$firefighter_id, .$full_name))


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
            training$TrainingUI(ns('training_tab'))
          ),
          training$TrainingOutput(ns('training_tab'))
        )
      ),

      nav_panel(
        title = "Attendance",
        layout_sidebar(
          sidebar = sidebar(
            open = "desktop",
            attendance$AttendanceUI(ns('attendance_tab'))
          ),
          attendance$AttendanceOutput(ns('attendance_tab'))
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

