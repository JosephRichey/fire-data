box::use(
  DBI[...],
  tibble[...],
  dplyr[...],
  lubridate[...],
  hms[as_hms],
  logger[...]
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
Firefighter_Contact <- dbGetQuery(CON, "SELECT * FROM firefighter_contact")

#' @export
Chain_Of_Command <- dbGetQuery(CON, "SELECT * FROM chain_of_command")

#' @export
Company <- dbGetQuery(CON, "SELECT * FROM company")

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
Firefighter_Incident <- dbGetQuery(CON, "SELECT * FROM firefighter_incident")

#' @export
Firefighter_Apparatus <- dbGetQuery(CON, "SELECT * FROM firefighter_apparatus")

#' @export
Apparatus <- dbGetQuery(CON, "SELECT * FROM apparatus")


#' @export
Local_Date <- as.Date(Sys.time() |> with_tz(tzone = Sys.getenv('LOCAL_TZ')), tz = Sys.getenv('LOCAL_TZ'))

#' @export
Equipment <- dbGetQuery(CON, "SELECT * FROM equipment") |>
  mutate(
    next_check_date = as.Date(next_check_date),
    expiration_date = as.Date(expiration_date),
    snooze_expires = as.Date(snooze_expires)
  )

#' @export
Equipment_Type <- dbGetQuery(CON, "SELECT * FROM equipment_type")

#' @export
Equipment_Log <- dbGetQuery(CON, "SELECT * FROM equipment_log")

#' @export
Base_Equipment_Data <- Equipment |>
  left_join(Equipment_Type, by = "equipment_type_id") |>
  left_join(Firefighter |> select(firefighter_id, full_name), by = "firefighter_id")


#' @export
Certification_Type <- dbGetQuery(CON, "SELECT * FROM certification_type")

#' @export
Certification <- dbGetQuery(CON, "SELECT * FROM certification") |>
  mutate(
    expiration_date = as.Date(expiration_date)
  )

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

#' @export
Current_Local_Date <- Sys.time() |>
  with_tz(Sys.getenv('LOCAL_TZ')) |>
  as.Date(tz = Sys.getenv('LOCAL_TZ'))

log_info("App Data loaded.")
