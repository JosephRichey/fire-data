box::use(
  DBI[...],
)

CON <- dbConnect(RMySQL::MySQL(),
                dbname = "cfddb",
                host = Sys.getenv("CFDDB_HOST"),
                port = 3306,
                user = "admin",
                password = Sys.getenv("CFDDB_PASSWORD"))

Training <- dbGetQuery(CON,
                       "SELECT * FROM cfddb.training")

Roster <- dbGetQuery(CON,
                     "SELECT * FROM cfddb.firefighter")

Attendance <- dbGetQuery(CON,
                         "SELECT * FROM cfddb.attendance")


dbListTables(CON)
