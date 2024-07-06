box::use(
  DBI[...],
  tibble[...],
  dplyr[...],
  lubridate[...],
  hms[as_hms],
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
                       paste0("SELECT * FROM ", Sys.getenv("TRAINING_TABLE"))) |>
  mutate(training_date = as.Date(with_tz(training_start_time, Sys.getenv('LOCAL_TZ')),tz = Sys.getenv('LOCAL_TZ')),
         training_start_time = with_tz(as.POSIXct(training_start_time), tz = Sys.getenv('LOCAL_TZ')) |> as_hms(),
         training_end_time = with_tz(as.POSIXct(training_end_time), tz = Sys.getenv('LOCAL_TZ'))|> as_hms())

#' @export
Firefighter <- dbGetQuery(CON,
                     paste0("SELECT * FROM ", Sys.getenv("FIREFIGHTER_TABLE"))) |>
  mutate(firefighter_start_date = as.Date(firefighter_start_date),
         firefighter_deactive_date = as.Date(firefighter_deactive_date))

#' @export
Attendance <- dbGetQuery(CON,
                         paste0("SELECT * FROM ", Sys.getenv("ATTENDANCE_TABLE"))) |>
  mutate(check_in = as.POSIXct(check_in),
         check_out = as.POSIXct(check_out))

#' @export
Local_Date <- as.Date(Sys.time() |> with_tz(tzone = Sys.getenv('LOCAL_TZ')), tz = Sys.getenv('LOCAL_TZ'))
