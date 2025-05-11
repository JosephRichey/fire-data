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


cat("Loading app_data.R\n", file = stderr())
cat("Attempting conncet to database\n", file = stderr())
cat("DB_HOST: ", Sys.getenv("DB_HOST"), "\n", file = stderr())



#' @export
CON <- dbConnect(RMariaDB::MariaDB(),
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
Current_Local_Date <- Sys.time() |> 
  with_tz(Sys.getenv('LOCAL_TZ')) |> 
  as.Date(tz = Sys.getenv('LOCAL_TZ'))
  
