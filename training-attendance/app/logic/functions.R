box::use(
  dplyr[filter, ...],
  shiny[...],
  DBI[...],
  lubridate[with_tz],
)

box::use(
  ./app_data,
  ../modals/modals,
)

#' @export
VerifyTrainingTime <- function(sysTime) {
  # browser()

  # Check if there is a training happening currently.
  Current_Training <- app_data$Training |>
    dplyr::filter(
      sysTime + 300 > training_start_time & # Allow clock in 5 minutes early
        sysTime - (60 * 60) < training_end_time # Allow clock out 60 minutes late
    )

  Next_Training <- app_data$Training |>
    dplyr::filter(
      training_start_time > sysTime
    ) |>
    dplyr::slice(1)

  # If there is no training scheduled currently, return text and indicate warning modal.
  if (nrow(Current_Training) == 0) {
    return(
        list(paste0(
          "There is no training scheduled currently. If you believe there should be,
          please reach out to the application administrator. ",
          if_else(nrow(Next_Training) == 0,
            "There are no future trainings scheduled.",
            paste0(
              "The next training is scheduled for ",
              with_tz(Next_Training$training_start_time, tzone = Sys.getenv("LOCAL_TZ")),
              ". You can check in five minutes before the scheduled start time."
            )
          ),
          " If you forgot to check out, you will be automatically checked out at the
           scheduled end time of the training."
        ),
        'warning')
    )
    
  } else if (nrow(Current_Training) != 1) {
    return(
      list("Error: More than one training scheduled currently", 'error')
    )
  } else {
    return(TRUE)
  }
}

#' @export
FixColNames <- function(Data) {
  colnames(Data) <- gsub("_", " ", colnames(Data))
  colnames(Data) <- stringr::str_to_title(colnames(Data))

  return(Data)
}

#' @export
CurrentStatusTable <- function(Attendance, Roster) {
  # browser()
  Attendance |>
    left_join(Roster) |>
    dplyr::filter(is.na(check_out)) |>
    dplyr::filter(as.Date(check_in) == as.Date(lubridate::with_tz(Sys.time()))) |>
    transmute(
      firefighter_full_name = firefighter_full_name,
      check_in = format(with_tz(check_in, tzone = Sys.getenv("LOCAL_TZ")), "%H:%M"),
      check_out = format(with_tz(check_out, tzone = Sys.getenv("LOCAL_TZ")), "%H:%M")
    ) |>
    FixColNames()
}

#' @export
UpdateAttendance <- function(rv) {
  Updated_Attendance <- DBI::dbGetQuery(
    app_data$CON,
    paste0("SELECT * FROM ", Sys.getenv("ATTENDANCE_TABLE"))
  ) |>
    mutate(
      check_in = as.POSIXct(check_in),
      check_out = as.POSIXct(check_out)
    )

  rv(Updated_Attendance)
}
