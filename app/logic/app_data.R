box::use(
  DBI[...],
  tibble[...],
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
                       paste0("SELECT * FROM cfddb.training", Sys.getenv("TESTING"),
                              " WHERE training_delete IS NULL"))

#' @export
Roster <- dbGetQuery(CON,
                     paste0("SELECT * FROM cfddb.firefighter", Sys.getenv("TESTING"),
                            " WHERE firefighter_deactive_date IS NULL"))

#' @export
Attendance <- dbGetQuery(CON,
                         paste0("SELECT * FROM cfddb.attendance", Sys.getenv("TESTING")))
