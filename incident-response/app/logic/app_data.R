box::use(
  dplyr[filter, ...],
  odbc[...],
  shiny[...],
)

#' @export
CON <- dbConnect(RMySQL::MySQL(),
                 dbname = "test",
                 host = Sys.getenv("DB_HOST"),
                 port = 3306,
                 user = "admin",
                 password = Sys.getenv("DB_PASSWORD"))

#' @export
Firefighter <- dbGetQuery(CON,"SELECT * FROM firefighter")

#' @export
Apparatus <- dbGetQuery(CON, "SELECT * FROM apparatus")

#' @export
Incident <- dbGetQuery(CON, "SELECT * FROM incident")

#' @export
Firefighter_Incident <- dbGetQuery(CON, "SELECT * FROM firefighter_incident")


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



Dispatch_Setting <- Setting |> 
  filter(major_setting_key == "dispatch_code")
#' @export
Dispatch_Codes <- lapply(
  split(Dispatch_Setting$setting_value, Dispatch_Setting$minor_setting_key),
  function(values) {
    as.list(values)
  }
)
  


# Creating mapping vectors to get Ids
#' @export
apparatus_mapping <- stats::setNames(Apparatus$apparatus_id, Apparatus$apparatus_name)

#' @export
firefighter_mapping <- stats::setNames(Firefighter$firefighter_id, Firefighter$firefighter_full_name)