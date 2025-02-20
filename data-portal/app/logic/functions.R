box::use(
  stringr[...],
  lubridate[...],
  hms[...],
  dplyr[...],
  data.table[...],
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

  # FIXME This needs to be passed as an argument
  # Overlap <- app_data$Training |>
  #   dplyr::filter(
  #     # starts during an existing training
  #     (UTC_start_time >= training_start_time & UTC_start_time <= training_end_time) |
  #     # ends during an existing training
  #     (UTC_end_time >= training_start_time & UTC_end_time <= training_end_time) |
  #     # starts before and ends after an existing training
  #     (UTC_start_time <= training_start_time & UTC_end_time >= training_end_time)
  #   )

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

#' @export
ConvertLocalPosix <- function(dt) {
  as.POSIXct(dt, tz = 'UTC') |> with_tz(tzone = Sys.getenv('LOCAL_TZ'))

}

#' @export
FormatUtcDateTime <- function(dt) {
  dt |>
    format(tz = 'UTC',
           usetz = FALSE)
}

#' @export
BuiltDateTime <- function(time, date, input_type, return_type = 'UTC') {
  if (input_type == 'local') {
    dt <- as.POSIXct(
      paste(
        date,
        as.ITime(time)
      ),
      tz = Sys.getenv('LOCAL_TZ')
    )
  } else {
    dt <- as.POSIXct(
      paste(
        date,
        as.ITime(time)
      ),
      tz = 'UTC'
    )
  }


  if (return_type == 'local') {
    return(dt |> with_tz(tzone = Sys.getenv('LOCAL_TZ')))
  } else {
    return(dt|> with_tz(tzone = 'UTC'))
  }
}


# local_time <- as.POSIXlt("2025-02-20 9:18:52")
# ust_time <- as.POSIXlt("2025-02-20 16:18:52", tz = 'UTC')
# date <- as.Date("2025-02-20")
#
# BuiltDateTime(local_time, date, 'local', 'local')
# BuiltDateTime(local_time, date, 'local', 'UTC')
# BuiltDateTime(ust_time, date, 'UST', 'local')
# BuiltDateTime(ust_time, date, 'UST', 'UST')
