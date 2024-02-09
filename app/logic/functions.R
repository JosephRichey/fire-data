box::use(
  dplyr[filter, ...],
  shiny[...],
  DBI[...],
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
  
  # Extract information from training to validate with.
  today_training <- app_data$Training |> 
    dplyr::filter(training_date == as.Date(sysTime, tz = Sys.getenv("TZ")))
  
  today_training_start_time <- today_training |> 
    pull(training_start_time)
    
  today_training_end_time <- today_training |> 
    pull(training_end_time)
  
  if(nrow(today_training) == 0) {
    
    showModal(modals$noTrainingModal())
    return(FALSE)
    
  } else if (nrow(today_training) != 1) {
    
    showModal(modals$errorModal("Error: More than one training scheduled for a day."))
    return(FALSE)
    
  }
  
  if(format(sysTime + 300, format = "%H:%M:%S") < today_training_start_time |
     format(sysTime - 300, format = "%H:%M:%S") > today_training_end_time) {
    
    showModal(modals$warningModal(
      paste0(
        "You are outside of applicable check in time. You can check in five minutes before the scheduled start time of ", 
        today_training_start_time,
        ". If you forgot to check out, you will be automatically checked out at ",  
        today_training_end_time,
        ".")))
      
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

#' @export
UpdateAttendance <- function(rv) {
  Updated_Attendance <- DBI::dbGetQuery(app_data$CON,
                                   paste0("SELECT * FROM cfddb.attendance", Sys.getenv("TESTING"))) |> 
    mutate(check_in = as.POSIXct(check_in),
           check_out = as.POSIXct(check_out))
  
  rv(Updated_Attendance)
}


