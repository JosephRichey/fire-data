box::use(
  DBI[...],
  tibble[...],
  dplyr[...],
  lubridate[...],
  hms[as_hms],
)

#' @export
CON <- dbConnect(RMySQL::MySQL(),
                dbname = "test",
                host = Sys.getenv("DB_HOST"),
                port = 3306,
                user = "admin",
                password = Sys.getenv("DB_PASSWORD"))

#' @export
LOCAL_TZ <- Sys.getenv('LOCAL_TZ')

#' @export
Training <- dbGetQuery(CON,
                       "SELECT * FROM training") |>
  mutate(
    start_time = as.POSIXct(start_time, tz = 'UTC') |> with_tz(tzone = LOCAL_TZ),
    end_time = as.POSIXct(end_time, tz = 'UTC') |> with_tz(tzone = LOCAL_TZ)
  )

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
