box::use(
  dplyr[filter, ...],
  odbc[...],
)

cat("Loading app_data.R\n", file = stderr())

attempt_connection <- function(max_attempts = 3, wait_secs = 3) {
  for (i in seq_len(max_attempts)) {
    try({
      cat(sprintf("Attempt %d to connect...\n", i), file = stderr())
      con <- dbConnect(RMySQL::MySQL(),
                       dbname = "cfddb",
                       host = Sys.getenv("DB_HOST"),
                       port = 3306,
                       user = "admin",
                       password = Sys.getenv("DB_PASSWORD"))
      cat("Connected successfully\n", file = stderr())
      return(con)
    }, silent = TRUE)
    Sys.sleep(wait_secs)
  }
  stop("Failed to connect after retries.")
}

#' @export
CON <- attempt_connection()

#' @export
Training <- dbGetQuery(CON,
                       paste0("SELECT * FROM ", Sys.getenv("TRAINING_TABLE"),
                       " WHERE training_delete IS NULL")) |> 
  mutate(training_start_time = as.POSIXct(training_start_time, format = "%Y-%m-%d %H:%M:%OS"),
         training_end_time = as.POSIXct(training_end_time, format = "%Y-%m-%d %H:%M:%OS"))

cat("Training data loaded\n", file = stderr())

#' @export
Firefighter <- dbGetQuery(CON,
                     paste0("SELECT * FROM ", Sys.getenv("FIREFIGHTER_TABLE"),
                     " WHERE firefighter_deactive_date IS NULL"))

cat("Firefighter data loaded\n", file = stderr())

#' @export
Attendance <- dbGetQuery(CON,
                         paste0("SELECT * FROM ", Sys.getenv("ATTENDANCE_TABLE"))) |> 
  mutate(check_in = as.POSIXct(check_in),
         check_out = as.POSIXct(check_out))

cat("Attendance data loaded\n", file = stderr())
