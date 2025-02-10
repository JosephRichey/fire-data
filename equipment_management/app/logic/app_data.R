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
  left_join(Firefighter, by = "firefighter_id") |> 
  mutate(check_threshold = functions$GenerateThreshold(next_check_date, check_lead_time, check_lead_time_unit),
         expire_threshold = functions$GenerateThreshold(expiration_date, expire_lead_time, expire_lead_time_unit)) |> 
  select(equipment_id, equipment_name, equipment_type,
         full_name, next_check_date, expiration_date,
         snooze_expires,  check_threshold, expire_threshold) |> 
  pivot_longer(cols = c(check_threshold, expire_threshold),
               names_to = "date_type", values_to = "date") |>
  mutate(flag_type = case_when(
    date_type == "check_threshold" & date <= Current_Local_Date ~ "Check",
    date_type == "expire_threshold" & snooze_expires >= Current_Local_Date ~ "Snooze",
    date_type == "expire_threshold" & date <= Current_Local_Date ~ "Expire",
    TRUE ~ 'Normal'),
    icon = case_when(
      flag_type == "Check" ~ "✔️",  
      flag_type == "Expire" ~ "☠️",
      TRUE ~ ""
    ),
    if_else(is.na(full_name), "", full_name) |> as.character(),
    button = case_when(
      flag_type == "Check" ~ sprintf('<button id="check" 
                        onclick = "shinyjs.check(%s)" 
                        class="btn btn-primary" 
                        data-toggle="modal" 
                        data-target="#editModal">Check</button>',
            equipment_id),
      flag_type == "Expire" ~ sprintf('<button id="snooze" 
                        onclick = "shinyjs.snooze(%s)" 
                        class="btn btn-primary" 
                        data-toggle="modal" 
                        data-target="#editModal">Snooze</button>',
            equipment_id),
      TRUE ~ ""
    )
    ) |> 
  select(equipment_id, icon, equipment_name, 
         equipment_type, full_name, date, flag_type, button)
  





#' @export

