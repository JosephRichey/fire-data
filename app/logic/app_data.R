box::use(
  dplyr[filter, ...],
  odbc[...],
)

#' @export
CON = dbConnect(RMySQL::MySQL(),
                dbname = "cfddb",
                host = Sys.getenv("CFDDB_HOST"),
                port = 3306,
                user = "admin",
                password = Sys.getenv("CFDDB_PASSWORD"))

#' @export
Training <- dbGetQuery(CON,
                       "SELECT * 
                       FROM cfddb.training
                       WHERE training_delete IS NULL")

#' @export
Roster <- dbGetQuery(CON,
                     "SELECT * FROM cfddb.firefighter")

#' @export
Attendance <- dbGetQuery(CON,
                         "SELECT * FROM cfddb.attendance") |> 
  mutate(check_in = as.POSIXct(check_in),
         check_out = as.POSIXct(check_out))


