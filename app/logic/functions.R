box::use(
  dplyr[filter, ...],
  shiny[...],
)

box::use(
  ./app_data,
  ../modals/modals,
)

#' @export
VerifyTrainingTime <- function(sysTime) {
  # browser()
  # FIXME For testing purposes only. Remove before production.
  # sysTime <- as.POSIXct("2024-01-03 20:00:13 MST")
  
  today_training <- app_data$Training |> 
    dplyr::filter(as.Date(training_start_time, tz = Sys.getenv("TZ")) == as.Date(sysTime, tz = Sys.getenv("TZ")))
  
  if(nrow(today_training) == 0) {
    
    showModal(modals$noTrainingModal())
    return(FALSE)
    
  } else if (nrow(today_training) != 1) {
    
    showModal(modals$errorModal("Error: More than one training scheduled for a day."))
    return(FALSE)
    
  }
  
  if(format(sysTime, format = "%H:%M:%S") < "17:55:00" |
     format(sysTime, format = "%H:%M:%S") > "20:05:00") {
    
    showModal(modals$warningModal("You are outside of applicable check in time. Please wait until 17:55 to check in.
                        If you forgot to check out, you will be automatically checked out at 20:05."))
    return(FALSE)
    
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
    dplyr::filter(as.Date(check_in, tz = Sys.getenv("TZ")) == as.Date(lubridate::with_tz(Sys.time(), Sys.getenv("TZ")), tz = Sys.getenv("TZ"))) |>
    transmute(firefighter_full_name = firefighter_full_name, 
              check_in = format(check_in, "%H:%M:%S"), 
              check_out = format(check_out, "%H:%M:%S")) |> 
    FixColNames()
}