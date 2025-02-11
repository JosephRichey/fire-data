box::use(
  dplyr[filter, ...],
  odbc[...],
  shiny[...],
  lubridate[...],
  tidyr[pivot_longer],
)

box::use(
  ./functions
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
Firefighter <- dbGetQuery(CON, "SELECT firefighter_id, full_name 
                          FROM firefighter")


#' @export
Current_Local_Date <- Sys.time() |> 
  with_tz(Sys.getenv('LOCAL_TZ')) |> 
  as.Date(tz = Sys.getenv('LOCAL_TZ'))

#' @export
Base_Data <- Equipment |> 
  left_join(Equipment_Type, by = "equipment_type_id") |> 
  left_join(Firefighter, by = "firefighter_id")
  





#' @export

