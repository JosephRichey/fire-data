box::use(
  dplyr[filter, ...],
  odbc[...],
  shiny[...],
  lubridate[with_tz],
)

#' @export
TZ <- Sys.getenv("LOCAL_TZ")

#' @export
CON <- dbConnect(RMySQL::MySQL(),
                 dbname = "test",
                 host = Sys.getenv("DB_HOST"),
                 port = 3306,
                 user = "admin",
                 password = Sys.getenv("DB_PASSWORD"))

#' @export
Firefighter <- reactiveVal(dbGetQuery(CON,"SELECT * FROM firefighter"))

#' @export
Apparatus <- reactiveVal(dbGetQuery(CON, "SELECT * FROM apparatus"))

#' @export
Firefighter_Apparatus <- reactiveVal(dbGetQuery(CON, "SELECT * FROM firefighter_apparatus"))

#' @export
Incident <- reactiveVal(dbGetQuery(CON, "SELECT * FROM incident") |> 
  mutate(dispatch_time = as.POSIXct(dispatch_time, tz = 'UTC') |> with_tz(TZ),
         end_time = as.POSIXct(end_time, tz = "UTC") |> with_tz(TZ))
  )

#' @export
Firefighter_Incident <- reactiveVal(dbGetQuery(CON, "SELECT * FROM firefighter_incident"))


##### Extract Setting Information #####

Setting <- dbGetQuery(CON, "SELECT * FROM setting")

#' @export
dispatch_time_seconds <- if_else(Setting |> 
  filter(major_setting_key == "dispatch_seconds" &
         minor_setting_key == "end_time") |>
  select(setting_value) == 'TRUE', TRUE, FALSE)

#' @export
end_time_seconds <- if_else(Setting |> 
  filter(major_setting_key == "dispatch_seconds" &
          minor_setting_key == "dispatch_time") |>
  select(setting_value) == 'TRUE', TRUE, FALSE)

#' @export
response_area <- Setting |> 
  filter(major_setting_key == "response_area") |> 
  select(setting_value) |> 
  pull()

#' @export
response_units <- Setting |> 
  filter(major_setting_key == "response_units") |> 
  select(setting_value) |> 
  pull()

#' @export
canceled <- if_else(Setting |> 
  filter(major_setting_key == "canceled") |>
  select(setting_value) == 'TRUE', TRUE, FALSE)

#' @export
dropped <- if_else(Setting |> 
  filter(major_setting_key == "dropped") |>
  select(setting_value) == 'TRUE', TRUE, FALSE)

#' @export
address <- if_else(Setting |> 
  filter(major_setting_key == "address") |>
  select(setting_value) == 'TRUE', TRUE, FALSE)

#' @export
password <- Setting |> 
  filter(major_setting_key == "password") |> 
  select(setting_value) |> 
  pull()

Dispatch_Setting <- Setting |> 
  filter(major_setting_key == "dispatch_code")
#' @export
Dispatch_Codes <- lapply(
  split(Dispatch_Setting$setting_value, Dispatch_Setting$minor_setting_key),
  function(values) {
    as.list(values)
  }
)

#' @export
# FIXME This won't work if a call is recorded at 00:30.
Current_Local_Date <- Sys.time() |> with_tz(Sys.getenv('LOCAL_TZ')) |> as.Date(tz = Sys.getenv('LOCAL_TZ'))
  


# Creating mapping vectors to get Ids
Static_Apparatus <- dbGetQuery(CON, "SELECT * FROM apparatus")
Static_Firefighter <- dbGetQuery(CON, "SELECT * FROM firefighter
                                        WHERE active_status = 1")

#' @export
apparatus_mapping <- stats::setNames(Static_Apparatus$apparatus_id, Static_Apparatus$apparatus_name)

#' @export
firefighter_mapping <- stats::setNames(Static_Firefighter$firefighter_id, Static_Firefighter$firefighter_full_name)
