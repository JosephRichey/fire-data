box::use(
  dplyr[filter, ...],
  odbc[...],
  lubridate[with_tz],
)

#' @export
CON <- dbConnect(RMySQL::MySQL(),
                 dbname = "test",
                 host = Sys.getenv("DB_HOST"),
                 port = 3306,
                 user = "admin",
                 password = Sys.getenv("DB_PASSWORD"))

#' @export
Training <- dbGetQuery(CON,"SELECT * FROM training 
                       WHERE training_delete IS NULL") |> 
  mutate(
    start_time = as.POSIXct(start_time, tz = 'UTC') |> 
      with_tz(Sys.getenv('TZ')),
    end_time = as.POSIXct(end_time, tz = 'UTC') |> 
      with_tz(Sys.getenv('TZ'))
    )

#' @export
Firefighter <- dbGetQuery(CON, "SELECT * FROM firefighter 
                          WHERE active_status = 1")

#' @export
Attendance <- dbGetQuery(CON,
                         paste0("SELECT * FROM attendance")) |> 
  mutate(
    check_in = as.POSIXct(check_in, tz = 'UTC') |> 
      with_tz(Sys.getenv('TZ')),
    check_out = as.POSIXct(check_out, tz = 'UTC') |> 
      with_tz(Sys.getenv('TZ'))
    )


