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
  mutate(check_threshold = functions$GenerateThreshold(next_check_date, check_lead_time, check_lead_time_unit)) |> 
  select(equipment_id, equipment_name, equipment_type,
         full_name, next_check_date,
         snooze_expires,  check_threshold) |> 
  mutate(flag_type = case_when(
    snooze_expires <= Current_Local_Date ~ "Snooze",
    next_check_date <= Current_Local_Date ~ "Due",
    check_threshold <= Current_Local_Date ~ "Approaching",
    TRUE ~ 'Normal'),
    icon = case_when(
      flag_type == "Due" ~ bsicons::bs_icon("exclamation-triangle", fill = "red"),  
      flag_type == "Approaching" ~ bsicons::bs_icon("check-circle", fill = "yellow"),
      TRUE ~ bsicons::bs_icon("app", fill = "green")
    ),
    full_name = if_else(is.na(full_name), "", full_name) |> as.character(),
    check = sprintf('<button id="check" 
                        onclick = "shinyjs.check(%s)" 
                        class="btn btn-primary" 
                        data-toggle="modal" 
                        data-target="#editModal">Check</button>',
            equipment_id),
    snooze = sprintf('<button id="snooze" 
                        onclick = "shinyjs.snooze(%s)" 
                        class="btn btn-primary" 
                        data-toggle="modal" 
                        data-target="#editModal">Snooze</button>',
            equipment_id)
    ) |> 
  select(equipment_id, icon, equipment_name, 
         equipment_type, full_name, next_check_date, flag_type, check, snooze)
  





#' @export

