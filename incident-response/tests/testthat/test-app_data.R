box::use(
  shiny[testServer],
  testthat[...],
)
box::use(
  app/main,
  app/logic/app_data,
)

# Test that database connection is established
test_that("Database connection is established", {
  expect_true(!is.null(app_data$CON))

})
