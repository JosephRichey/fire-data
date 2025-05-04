box::use(
  dplyr[filter, ...],
  odbc[...],
  shiny[...],
  lubridate[with_tz],
)

box::use(
  ./logging,
)

#' @export
TZ <- Sys.getenv("LOCAL_TZ")

#' @export
CON <- dbConnect(RMySQL::MySQL(),
                 dbname = "crabapple",
                 host = Sys.getenv("DB_HOST"),
                 port = 3306,
                 user = "admin",
                 password = Sys.getenv("DB_PASSWORD"))

#' @export
Firefighter <- dbGetQuery(CON,"SELECT * FROM firefighter")

#' @export
Apparatus <- dbGetQuery(CON, "SELECT * FROM apparatus")

#' @export
Unit <- dbGetQuery(CON, "SELECT * FROM unit")

#' @export
Area <- dbGetQuery(CON, "SELECT * FROM area")

#' @export
Setting <- dbGetQuery(CON, "SELECT * FROM setting")

#' @export
Dispatch_Code <- dbGetQuery(CON, "SELECT * FROM dispatch_code")


#' @export
# FIXME This won't work if a call is recorded at 00:30.
Current_Local_Date <- Sys.time() |> 
  with_tz(Sys.getenv('LOCAL_TZ')) |> 
  as.Date(tz = Sys.getenv('LOCAL_TZ'))
  
