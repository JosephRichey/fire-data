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

Setting <- dbGetQuery(functions$CON, "SELECT * FROM setting")

##### Extract and set values to be passed to the app based on settings

# Training Page

## Training Filter Start Date
start <- Setting |>
  filter(major_setting_key == "training_page") |>
  select(minor_setting_key) |>
  pull() |>
  as.character()

period <- case_when(
  substr(start, 1, 1) == "M" ~ "month",
  substr(start, 1, 1) == "W" ~ "week",
  substr(start, 1, 1) == "D" ~ "day",
  substr(start, 1, 1) == "Y" ~ "year"
)

add <- substr(start, 2, 2) == "+"

number <- as.numeric(substr(start, 3, 3))

#' @export
Training_Filter_Start_Date <- Sys.time() |>
  with_tz(Sys.getenv('LOCAL_TZ')) |>
  as.Date(tz = Sys.getenv('LOCAL_TZ')) - 365
