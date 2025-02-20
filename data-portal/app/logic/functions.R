box::use(
  stringr[...],
  lubridate[...],
  hms[...],
  dplyr[...],
)

box::use(
  app/logic/app_data,
)

#' @export
FixColNames <- function(Data) {
  colnames(Data) <- gsub("_", " ", colnames(Data))
  colnames(Data) <- stringr::str_to_title(colnames(Data))

  return(Data)
}

#' @export
ParseUserInput <- function(string) {
  string <- stringr::str_to_title(string)
  string <- trimws(string)

  return(string)
}

#' @export
as.MT.Date <- function(date_time) {
  as.Date(date_time, tz = Sys.getenv("TZ"))
}


#' @export
#' Not tested extensively at the limits. This function is only meant to prevent people from entering duplicates.
#  Theoretically, you could create a training that crashes the entire app since you can log in early and log out late.
VerifyNoOverlap <- function(start_time, end_time) {

  UTC_start_time <- (start_time |> force_tz(Sys.getenv('LOCAL_TZ')) + .01) |> with_tz('UTC')
  UTC_end_time <- (end_time |> force_tz(Sys.getenv('LOCAL_TZ')) + .01) |> with_tz('UTC')

  Overlap <- app_data$Training |>
    dplyr::filter(
      # starts during an existing training
      (UTC_start_time >= training_start_time & UTC_start_time <= training_end_time) |
      # ends during an existing training
      (UTC_end_time >= training_start_time & UTC_end_time <= training_end_time) |
      # starts before and ends after an existing training
      (UTC_start_time <= training_start_time & UTC_end_time >= training_end_time)
    )

  if (nrow(Overlap) > 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }

}


#' @export
FormatLocalDate <- function(dt, asPosix = FALSE) {

  if (asPosix) {
    dt |>
      as.Date(tz = Sys.getenv('LOCAL_TZ'))
  } else {
    dt |>
      format('%m-%d-%Y',
             tz = Sys.getenv('LOCAL_TZ'),
             usetz = FALSE)
  }
}

#' @export
FormatLocalTime <- function(dt, seconds = FALSE) {
  if (seconds) {
    dt |>
      format('%H:%M:%S',
             tz = Sys.getenv('LOCAL_TZ'),
             usetz = FALSE)
  } else {
    dt |>
      format('%H:%M',
             tz = Sys.getenv('LOCAL_TZ'),
             usetz = FALSE)
  }
}

#' @export
FormatLocalDateTime <- function(dt) {
  dt |>
    format('%m-%d-%Y %H:%M:%S',
           tz = Sys.getenv('LOCAL_TZ'),
           usetz = FALSE)
}

