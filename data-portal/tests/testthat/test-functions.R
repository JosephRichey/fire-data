box::use(
  testthat[...],
  DBI[dbGetQuery],
  shiny[reactiveConsole, reactiveValues],
)

box::use(
  app/logic/functions,
)

test_that("Can connect to test database", {
  Tables <- dbGetQuery(functions$CON, "SHOW TABLES")

  #FIXME Eventually, once structure is solid, update this test
  # to have actual table names
  testthat::expect_gt(nrow(Tables), 2)
})

test_that("QueryDatabase function works", {
  # Test function returns a data frame
  testthat::expect_s3_class(functions$QueryDatabase('firefighter'), "data.frame")

  # Test that function handles errors gracefully
  testthat::expect_no_error(functions$QueryDatabase('non_existent_table'))

  # Test function returns a data frame with the correct number of columns
  testthat::expect_equal(ncol(functions$QueryDatabase('firefighter')), 8,
                         label = "function returns a data frame with the correct number of columns")

  # Test function returns a data frame with the correct column names
  testthat::expect_equal(colnames(functions$QueryDatabase('firefighter')),
                         c("id", "full_name", "start_date", "trainer", "officer", "active_status", "company_id", "firefighter_role"),
                         label = "function returns a data frame with the correct column names")
})

test_that("UpdateReactives function works", {
  reactiveConsole(TRUE)
  rdfs <- reactiveValues(
    training = NULL,
    firefighter = NULL,
    attendance = NULL
  )
  functions$UpdateReactives(rdfs, c("training", "firefighter"))

  # Test reactive returns a data frame
  testthat::expect_s3_class(rdfs$training, "data.frame")

  # Test functions fails when the number of reactives and database tables do not match
  testthat::expect_error(functions$UpdateReactives(c("training", "firefighter"), c(r_Training)),
                         label = "function fails when the number of reactives and database tables do not match")

  # Test function exits quietly when no database tables to update
  testthat::expect_no_error(functions$UpdateReactives(c(), c()))

  # Test works when a single database table is updated
  testthat::expect_no_error(functions$UpdateReactives(rdfs, c("training")))

  reactiveConsole(FALSE)


})

test_that("GetSetting works", {
  # Test that invalid setting returns na
  testthat::expect_equal(functions$GetSetting('non_existent_setting'), NA,
                         label = "function returns NA for non-existent setting")

  # Test that pulling dispatch codes returns a list
  testthat::expect_vector(functions$GetSetting('incident', group = 'dispatch_code'))

  # Test that pulling password only returns a single value
  testthat::expect_length(functions$GetSetting('password'), 1)

})


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
