box::use(
  testthat[...],
  DBI[dbConnect, dbGetQuery],
)
box::use(
  ../../app/logic/functions[FixColNames]
)

# TODO Write tests for generation of insert statements