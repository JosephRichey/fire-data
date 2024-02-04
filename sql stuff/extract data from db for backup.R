box::use(
  dplyr[filter, ...],
  odbc[...],
)

#' @export
CON <- dbConnect(RMySQL::MySQL(),
                 dbname = "cfddb",
                 host = Sys.getenv("CFDDB_HOST"),
                 port = 3306,
                 user = "admin",
                 password = Sys.getenv("CFDDB_PASSWORD"))

#' @export
Training <- dbGetQuery(CON,
                       "SELECT * 
                       FROM cfddb.training")

#' @export
Roster <- dbGetQuery(CON,
                     "SELECT * FROM cfddb.firefighter")

#' @export
Attendance <- dbGetQuery(CON,
                         "SELECT * FROM cfddb.attendance")

Roster_Sql_Export <- Roster |> 
  mutate(statement = paste0("(",
                            firefighter_id, ",",
                            "\"", firefighter_first_name, "\",",
                            "\"", firefighter_last_name, "\",",
                            "\"", firefighter_full_name, "\",",
                            "\"", firefighter_start_date, "\",",
                            "\"", firefighter_officer, "\",",
                            "\"", firefighter_deactive_date, "\"",
                            if_else(firefighter_id == max(Roster$firefighter_id), ");", "),")))


Training_Sql_Export <- Training |> 
  mutate(statement = paste0("(",
                            training_id, ",",
                            "\"", training_type, "\",",
                            "\"", training_topic, "\",",
                            "\"", training_description, "\",",
                            "\"", training_date, "\",",
                            "\"", training_start_time, "\",",
                            "\"", training_end_time, "\",",
                            "\"", training_officer, "\"",
                            "\"", training_delete, "\"",
                            if_else(training_id == max(Training$training_id), ");", "),")))


Attendance_Sql_Export <- Attendance |>
  mutate(statement = paste0("(",
                            attendance_id, ",",
                            "\"", firefighter_id, "\",",
                            "\"", training_id, "\",",
                            "\"", as.POSIXct(check_in), "\",",
                            "\"", as.POSIXct(check_out), "\"",
                            if_else(attendance_id == max(Attendance$attendance_id), ");", "),")))

writeLines(c("INSERT INTO cfddb.firefighter VALUES", Roster_Sql_Export$statement), "sql stuff/Roster_Sql_Export.sql")
writeLines(c("INSERT INTO cfddb.firefighter VALUES", Training_Sql_Export$statement), "sql stuff/Training_Sql_Export.sql")
writeLines(c("INSERT INTO cfddb.firefighter VALUES", Attendance_Sql_Export$statement), "sql stuff/Attendance_Sql_Export.sql")
