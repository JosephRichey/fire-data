box::use(
  DBI[...],
  tibble[...],
  dplyr[...],
  lubridate[...],
  hms[as_hms],
)

box::use(
  ./functions
)

#' @export
CON <- dbConnect(RMySQL::MySQL(),
                dbname = "test",
                host = Sys.getenv("DB_HOST"),
                port = 3306,
                user = "admin",
                password = Sys.getenv("DB_PASSWORD"))

#' @export
Training <- dbGetQuery(CON,
                       "SELECT * FROM training") |>
  mutate(
    start_time = functions$ConvertLocalPosix(start_time),
    end_time = functions$ConvertLocalPosix(end_time)
  )

#' @export
Firefighter <- dbGetQuery(CON, "SELECT * FROM firefighter") |>
  mutate(start_date = functions$FormatLocalDate(start_date, TRUE))

#' @export
Attendance <- dbGetQuery(CON,
                         paste0("SELECT * FROM ", Sys.getenv("ATTENDANCE_TABLE"))) |>
  mutate(check_in = as.POSIXct(check_in),
         check_out = as.POSIXct(check_out))

#' @export
Local_Date <- as.Date(Sys.time() |> with_tz(tzone = Sys.getenv('LOCAL_TZ')), tz = Sys.getenv('LOCAL_TZ'))
