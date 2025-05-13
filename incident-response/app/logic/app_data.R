box::use(
  dplyr[filter, ...],
  odbc[...],
  shiny[...],
  logger[...],
  lubridate[with_tz],
)

box::use(
  ./logging,
)

#' @export
TZ <- Sys.getenv("LOCAL_TZ")


cat("Loading app_data.R\n", file = stderr())
cat("Attempting to connect to database\n", file = stderr())


#' @export
CON <- tryCatch({
  dbConnect(RMariaDB::MariaDB(),
            dbname = "crabapple",
            host = Sys.getenv("DB_HOST"),
            port = 3306,
            user = "admin",
            password = Sys.getenv("DB_PASSWORD"))
}, error = function(e) {
  log_error(glue::glue("Database connection failed: {e$message}"),
            namespace = "global")
  NULL  # fallback to NULL if connection fails
})

if(is.null(CON)) {
  log_error("Database connection is NULL. Exiting.")
  stop("Database connection failed. Exiting.")
} else {
  log_info("Database connection established successfully.", namespace = "global")
}

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
  
