box::use(
  shiny[testServer],
  testthat[...],
  DBI[dbConnect, dbGetQuery],
)
box::use(
  ../../app/logic/functions[FixColNames],
)

# TODO Do shinytest2 tests
