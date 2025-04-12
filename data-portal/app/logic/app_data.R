box::use(
  DBI[...],
  tibble[...],
  dplyr[...],
  lubridate[...],
  hms[as_hms],
  logger[...]
)

log_info("Running app_data.R", namespace = "app_data.R")

#' @export
CON <- dbConnect(RMySQL::MySQL(),
                dbname = "crabapple",
                host = Sys.getenv("DB_HOST"),
                port = 3306,
                user = "admin",
                password = Sys.getenv("DB_PASSWORD"))

log_info("Connected to database", namespace = "app_data.R")

#' @export
Setting <- dbGetQuery(CON, "SELECT * FROM setting")

log_info("app_data.R loaded", namespace = "app_data.R")


