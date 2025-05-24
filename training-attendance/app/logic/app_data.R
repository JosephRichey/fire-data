box::use(
  dplyr[filter, ...],
  odbc[...],
  lubridate[with_tz],
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
CON <- dbConnect(RMySQL::MySQL(),
                 dbname = "test",
                 host = Sys.getenv("DB_HOST"),
                 port = 3306,
                 user = "admin",
                 password = Sys.getenv("DB_PASSWORD"))

#' @export
Training <- dbGetQuery(CON,"SELECT * FROM training 
                       WHERE training_delete IS NULL") |> 
  mutate(
    start_time = as.POSIXct(start_time, tz = 'UTC') |> 
      with_tz(Sys.getenv('TZ')),
    end_time = as.POSIXct(end_time, tz = 'UTC') |> 
      with_tz(Sys.getenv('TZ'))
    )

cat("Training data loaded\n", file = stderr())

#' @export
Firefighter <- dbGetQuery(CON, "SELECT * FROM firefighter 
                          WHERE active_status = 1")

cat("Firefighter data loaded\n", file = stderr())

#' @export
Attendance <- dbGetQuery(CON,
                         paste0("SELECT * FROM attendance")) |> 
  mutate(
    check_in = as.POSIXct(check_in, tz = 'UTC') |> 
      with_tz(Sys.getenv('TZ')),
    check_out = as.POSIXct(check_out, tz = 'UTC') |> 
      with_tz(Sys.getenv('TZ'))
    )

cat("Attendance data loaded\n", file = stderr())
