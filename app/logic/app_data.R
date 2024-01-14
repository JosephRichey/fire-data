box::use(
  DBI[...],
  tibble[...],
)

CON <- dbConnect(RMySQL::MySQL(),
                dbname = "cfddb",
                host = Sys.getenv("CFDDB_HOST"),
                port = 3306,
                user = "admin",
                password = Sys.getenv("CFDDB_PASSWORD"))

Training <- dbGetQuery(CON,
                       "SELECT * FROM cfddb.training
                       WHERE training_delete IS NULL") |>
  remove_rownames() |>
  column_to_rownames("training_id")

Roster <- dbGetQuery(CON,
                     "SELECT * FROM cfddb.firefighter")

Attendance <- dbGetQuery(CON,
                         "SELECT * FROM cfddb.attendance")


dbListTables(CON)
