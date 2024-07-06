box::use(
  shiny[testServer],
  testthat[...],
  DBI[dbConnect, dbGetQuery],
)
box::use(
  app/logic/functions[...],
)

test_that("Can connect to test database", {
  CON <- dbConnect(RMySQL::MySQL(),
                   dbname = "cfddb",
                   host = Sys.getenv("CFDDB_HOST"),
                   port = 3306,
                   user = "admin",
                   password = Sys.getenv("CFDDB_PASSWORD"))
  result <- dbGetQuery(CON, paste0("SELECT * FROM ", Sys.getenv("TRAINING_TABLE"),
                         " WHERE training_delete IS NULL LIMIT 1"))
  testthat::expect_is(result, "data.frame")
  
})
