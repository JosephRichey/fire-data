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
                         paste0("SELECT * FROM attendance")) |>
  mutate(check_in = functions$ConvertLocalPosix(check_in),
         check_out = functions$ConvertLocalPosix(check_out))

#' @export
Incident <- dbGetQuery(CON, "SELECT * FROM incident") |>
  mutate(dispatch_time = functions$ConvertLocalPosix(dispatch_time),
         end_time = functions$ConvertLocalPosix(end_time))

#' @export
Incident_Xref <- dbGetQuery(CON, "SELECT * FROM incident_xref")

#' @export
Firefighter_Incident <- dbGetQuery(CON, "SELECT * FROM firefighter_incident")

#' @export
Firefighter_Apparatus <- dbGetQuery(CON, "SELECT * FROM firefighter_apparatus")

#' @export
Local_Date <- as.Date(Sys.time() |> with_tz(tzone = Sys.getenv('LOCAL_TZ')), tz = Sys.getenv('LOCAL_TZ'))



Setting <- dbGetQuery(CON, "SELECT * FROM setting")

Dispatch_Setting <- Setting |>
  filter(major_setting_key == "dispatch_code")

#' @export
Dispatch_Codes <- lapply(
  split(Dispatch_Setting$setting_value, Dispatch_Setting$minor_setting_key),
  function(values) {
    as.list(values)
  }
)


