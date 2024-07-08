box::use(
  testthat[...],
)

box::use(
  app/logic/functions,
)

test_that("VerifyNoOverlap function works", {
  # In the test data, there is a training every day of the year from 18:00 to 20:00
  # Not tested extensively at the limits. This function is only meant to prevent people from entering duplicates.
  # Theoretically, you could create a training that crashes the entire app since you can log in early and log out late.

  # Test function returns false when new training starts during an existing training
  testthat::expect_false(functions$VerifyNoOverlap('2024-07-07 19:59:00' |> as.POSIXct(),
                                                   '2024-07-07 21:00:00' |> as.POSIXct()),
                         label = "function returns false when new training starts at the end of another training")

  # Test function returns false when new training ends during an existing training
  testthat::expect_false(functions$VerifyNoOverlap('2024-07-07 17:00:00' |> as.POSIXct(),
                                                   '2024-07-07 18:01:00' |> as.POSIXct()),
                         label = "function returns false when new training ends at the start of another training")

  # Test function returns false when new training is identical to another training
  testthat::expect_false(functions$VerifyNoOverlap('2024-07-07 18:00:00' |> as.POSIXct(),
                                                   '2024-07-07 20:00:00' |> as.POSIXct()),
                         label = "function returns false when new training is identical to another training")

  # Test function returns false when new training encompasses another training
  testthat::expect_false(functions$VerifyNoOverlap('2024-07-07 17:00:00' |> as.POSIXct(),
                                                   '2024-07-07 21:00:00' |> as.POSIXct()),
                         label = "function returns true when new training encompases another training")

  # Test function returns true when new training is outside of another training
  testthat::expect_true(functions$VerifyNoOverlap('2024-07-07 16:00:00' |> as.POSIXct(),
                                                  '2024-07-07 17:59:00' |> as.POSIXct()),
                        label = "function returns true when new training is outside of another training")

  # Test function returns true when new training is outside of another training
  testthat::expect_true(functions$VerifyNoOverlap('2024-07-07 20:01:00' |> as.POSIXct(),
                                                  '2024-07-07 21:00:00' |> as.POSIXct()),
                        label = "function returns true when new training is outside of another training")

})
