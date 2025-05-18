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

log_trace("Loading app_data.R", namespace = "app_data")


log_trace("Attempting to connect to database", namespace = "app_data")
#' @export
CON <- tryCatch({
  dbConnect(RMariaDB::MariaDB(),
            dbname = "corinne",
            host = Sys.getenv("DB_HOST"),
            port = 3306,
            user = "admin",
            password = Sys.getenv("DB_PASSWORD"))
}, error = function(e) {
  log_error(glue::glue("Database connection failed: {e$message}"),
            namespace = "app_data")
  NULL  # fallback to NULL if connection fails
})

if(is.null(CON)) {
  log_error("Database connection is NULL. Exiting.",
            namespace = "app_data")
  stop("Database connection failed. Exiting.")
} else {
  log_success("Database connection established successfully.", namespace = "app_data")
}

log_trace("Loading static tables", namespace = "app_data")

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

tz <- dbGetQuery(
  CON, 
  "SELECT * FROM setting
  WHERE domain = 'global' AND 
    setting_key = 'ltz'") |>
    pull(setting_value)

#' @export
Current_Local_Date <- Sys.time() |> 
    with_tz(tz) |> 
    as.Date(tz = tz)

  
log_trace("Loading app_data.R complete", namespace = "app_data")
