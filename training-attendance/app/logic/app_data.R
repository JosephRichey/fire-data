box::use(
  dplyr[filter, ...],
  odbc[...],
)

#' @export
CON <- dbConnect(RMySQL::MySQL(),
                dbname = "cfddb",
                host = Sys.getenv("CFDDB_HOST"),
                port = 3306,
                user = "admin",
                password = Sys.getenv("CFDDB_PASSWORD"))

#' @export
Training <- dbGetQuery(CON,
                       paste0("SELECT * FROM ", Sys.getenv("TRAINING_TABLE"),
                       " WHERE training_delete IS NULL")) |> 
  mutate(training_start_time = as.POSIXct(training_start_time, format = "%Y-%m-%d %H:%M:%OS"),
         training_end_time = as.POSIXct(training_end_time, format = "%Y-%m-%d %H:%M:%OS"))

#' @export
Firefighter <- dbGetQuery(CON,
                     paste0("SELECT * FROM ", Sys.getenv("FIREFIGHTER_TABLE"),
                     " WHERE firefighter_deactive_date IS NULL"))

#' @export
Attendance <- dbGetQuery(CON,
                         paste0("SELECT * FROM ", Sys.getenv("ATTENDANCE_TABLE"))) |> 
  mutate(check_in = as.POSIXct(check_in),
         check_out = as.POSIXct(check_out))


