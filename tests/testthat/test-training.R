box::use(
  testthat[...],
)

box::use(
  app/view/training[...],
)

test_that("Pull eligible trainers", {
  expect_vector(training_trainers)
})
