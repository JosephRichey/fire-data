box::use(
  shiny[testServer],
  testthat[...],
)
box::use(
  app/main[...],
)

test_that("empty test", {
  expect_true(TRUE)
})
