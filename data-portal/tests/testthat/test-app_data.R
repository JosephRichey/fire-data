box::use(
  testthat[...],
  DBI[...],
)

box::use(
  app/logic/app_data,
)


test_that("Can connect to test database", {
  Tables <- dbGetQuery(app_data$CON, "SHOW TABLES")

  #FIXME Eventually, once structure is solid, update this test
  # to have actual table names
  testthat::expect_gt(nrow(Tables), 2)
})
