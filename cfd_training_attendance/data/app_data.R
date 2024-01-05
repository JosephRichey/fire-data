box::use(
  dplyr[...],
  odbc[...],
)

CON = dbConnect(RMySQL::MySQL(),
                dbname = "cfddb",
                host = Sys.getenv("CFDDB_HOST"),
                port = 3306,
                user = "admin",
                password = Sys.getenv("CFDDB_PASSWORD"))

Training <- dbGetQuery(CON,
                       "SELECT * 
                       FROM cfddb.training
                       WHERE training_delete IS NULL")

Roster <- dbGetQuery(CON,
                     "SELECT * FROM cfddb.firefighter")

Attendance <- dbGetQuery(CON,
                         "SELECT * FROM cfddb.attendance") |> 
  mutate(check_in = as.POSIXct(check_in),
         check_out = as.POSIXct(check_out))
