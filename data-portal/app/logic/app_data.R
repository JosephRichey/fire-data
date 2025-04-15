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

init_ltz <- Setting |>
  filter(setting_key == "ltz") |>
  pull(setting_value)

log_info(glue::glue("Extracted local timezone: {init_ltz}"), namespace = "app_data.R")

#' @export
local_date <- Sys.time() |>
  with_tz(tz = init_ltz) |>
  as_date(tz = init_ltz)

log_info(glue::glue("Extracted local date: {local_date}"), namespace = "app_data.R")


##### Training #####
Training_Classifcaion <- dbGetQuery(CON, "SELECT * FROM training_classification")


#' @export
Training_Category_Filter <- Training_Classifcaion |>
  filter(!is.na(training_category)) |>
  mutate(training_category = if_else(
    is_active == 1,
    training_category,
    paste("*", training_category)
    )
  ) |>
  pull(training_category) |>
  unique()

#' @export
Training_Category_Active <- Training_Classifcaion |>
  filter(is_active == 1) |>
  pull(training_category) |>
  unique()



log_info("app_data.R loaded", namespace = "app_data.R")


