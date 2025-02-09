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
Equipment <- dbGetQuery(CON, "SELECT * FROM equipment")

#' @export
Equipment_Type <- dbGetQuery(CON, "SELECT * FROM equipment_type")

#' @export
Equipment_Log <- dbGetQuery(CON, "SELECT * FROM equipment_log")

#' @export
Firefighter <- dbGetQuery(CON, "SELECT * FROM firefighter")


#' @export
# FIXME This won't work if a call is recorded at 00:30.
Current_Local_Date <- Sys.time() |> with_tz(Sys.getenv('LOCAL_TZ')) |> as.Date(tz = Sys.getenv('LOCAL_TZ'))

