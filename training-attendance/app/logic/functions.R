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
VerifyTrainingTime <- function(sysTime, r_training) {
  # browser()

  # Check if there is a training happening currently.
  Current_Training <- r_training |>
    dplyr::filter(
      #FIXME Customize how early they can clock in and how late they can clock out.
      sysTime + 300 > start_time & # Allow clock in 5 minutes early
        sysTime - (60 * 60) < end_time # Allow clock out 60 minutes late
    )

  Next_Training <- r_training |>
    dplyr::filter(
      start_time > sysTime
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
              with_tz(Next_Training$start_time, tzone = Sys.getenv("LOCAL_TZ")),
              
            #FIXME This message needs to show the correct time they can check in.
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
  # FIXME This still assumes one training per day. 
  # Eventually, I want to make it so that it can handle multiple trainings per day.
  
  # browser()
  Attendance |>
    left_join(Roster) |>
    dplyr::filter(as.Date(check_in) == as.Date(Sys.Date())) |>
    transmute(
      full_name = full_name,
      check_in = format(check_in, "%Y-%m-%d %H:%M:%S", tz = Sys.getenv('LOCAL_TZ'), usetz = FALSE),
      check_out = format(check_out, "%Y-%m-%d %H:%M:%S", tz = Sys.getenv('LOCAL_TZ'), usetz = FALSE)
    ) |>
    FixColNames()
}

#' @export
UpdateAttendance <- function(rv) {
  Updated_Attendance <- DBI::dbGetQuery(app_data$CON,
                                        paste0("SELECT * FROM attendance")) |> 
    mutate(
      check_in = as.POSIXct(check_in, tz = 'UTC') |> 
        with_tz(Sys.getenv('TZ')),
      check_out = as.POSIXct(check_out, tz = 'UTC') |> 
        with_tz(Sys.getenv('TZ'))
    )

  rv(Updated_Attendance)
}

#' @export
UpdateTrainings <- function(rv) {
  Updated_Trainings <- DBI::dbGetQuery(app_data$CON,"SELECT * FROM training 
                       WHERE training_delete IS NULL") |> 
    mutate(
      start_time = as.POSIXct(start_time, tz = 'UTC') |> 
        with_tz(Sys.getenv('TZ')),
      end_time = as.POSIXct(end_time, tz = 'UTC') |> 
        with_tz(Sys.getenv('TZ'))
    )

  rv(Updated_Trainings)
}
