box::use(
  dplyr[...],
  shiny[...],
)

box::use(
  ../data/app_data,
  ../modals/modals,
)

#' @export
VerifyTrainingTime <- function(sysTime) {
  # browser()
  sysTime <- as.POSIXct("2024-01-03 20:00:13 MST")
  
  today_training <- app_data$Training |> 
    dplyr::filter(training_date == as.Date(sysTime))
  
  if(nrow(today_training) == 0) {
    showModal(modals$noTrainingModal())
    print("no training modal")
    return(FALSE)
  } else if (nrow(today_training) != 1) {
    print("Error: More than one training scheduled for a day.")
    showModal(modals$errorModal("Error: More than one training scheduled for a day."))
    return(FALSE)
  }
  
  if(format(sysTime, format = "%H:%M:%S") < "17:55:00" |
     format(sysTime, format = "%H:%M:%S") > "20:05:00") {
    showModal(modals$warningModal("You are outside of applicable check in time. Please wait until 17:55 to check in.
                        If you forgot to check out, you have been automatically checked out at 20:05."))
    print("Outside time modal.")
    return(FALSE)
  } else {
    print("Valid check in.")
    return(TRUE)
  }
  
  
}

#' @export
FirefighterStatus <- function() {
  
}

#' @export
FixColNames <- function(Data) {
  colnames(Data) <- gsub("_", " ", colnames(Data))
  colnames(Data) <- stringr::str_to_title(colnames(Data))
  
  return(Data)
}