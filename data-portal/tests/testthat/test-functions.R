box::use(
  testthat[...],
  DBI[dbGetQuery],
  shiny[reactiveConsole, reactiveValues],
  lubridate[...],
)

box::use(
  app/logic/functions,
)

# Settings to be used throughout testing
local_tz <- functions$GetSetting("global", "ltz")


test_that("GetSetting works", {
  # Test that invalid setting returns na
  testthat::expect_equal(functions$GetSetting('non_existent_setting'), NA,
                         label = "function returns NA for non-existent setting")

  # Test that pulling dispatch codes returns a list
  testthat::expect_vector(functions$GetSetting('incident', group = 'dispatch_code'))

  # Test that pulling password only returns a single value
  testthat::expect_length(functions$GetSetting('password'), 1)

})

test_that("FormatLocal handles valid date -> date", {
  expect_equal(
    functions$FormatLocal(as.Date("2025-04-10"), input = "date", output = "date"),
    format(as.Date("2025-04-10"),
           functions$GetSetting("global", "date_format"),
           tz = local_tz)
  )
})

test_that("FormatLocal handles datetime -> datetime", {
  dt <- as.POSIXct("2025-04-10 14:45:30", tz = "UTC")
  expected <- format(dt,
                     functions$GetSetting("global", "date_time_format"),
                     tz = local_tz)
  expect_equal(
    functions$FormatLocal(dt, input = "datetime", output = "datetime"),
    expected
  )
})

test_that("FormatLocal handles datetime -> date", {
  dt <- as.POSIXct("2025-04-10 14:45:30", tz = "UTC")
  expected <- format(dt, functions$GetSetting("global", "date_format"), tz = local_tz)
  expect_equal(
    functions$FormatLocal(dt, input = "datetime", output = "date"),
    expected
  )
})

test_that("FormatLocal handles datetime -> time (no seconds)", {
  dt <- as.POSIXct("2025-04-10 14:45:30", tz = "UTC")
  expected <- format(dt, "%H:%M", tz = local_tz)
  expect_equal(
    functions$FormatLocal(dt, input = "datetime", output = "time", seconds = FALSE),
    expected
  )
})

test_that("FormatLocal handles datetime -> time (with seconds)", {
  dt <- as.POSIXct("2025-04-10 14:45:30", tz = "UTC")
  expected <- format(dt, "%H:%M:%S", tz = local_tz)
  expect_equal(
    functions$FormatLocal(dt, input = "datetime", output = "time", seconds = TRUE),
    expected
  )
})

test_that("FormatLocal errors on invalid input/output combos", {
  expect_error(
    functions$FormatLocal(Sys.Date(), input = "date", output = "time"),
    "Invalid conversion"
  )
  expect_error(
    functions$FormatLocal(Sys.Date(), input = "date", output = "datetime"),
    "Invalid conversion"
  )
})

test_that("ConvertToLocalPosix converts datetime string from UTC to local time", {
  input <- "2025-04-10 14:30:00"

  expected <- format(
    lubridate::with_tz(as.POSIXct(input, tz = "UTC"), tzone = local_tz),
    functions$GetSetting("global", "date_time_format"),
    tz = local_tz,
    usetz = FALSE
  )

  result <- functions$ConvertToLocalPosix(input, input = "datetime", output = "datetime")
  expect_equal(result, expected)
})

test_that("ConvertToLocalPosix converts UTC datetime to local date string", {
  input <- "2025-04-10 01:45:00"

  expected <- format(
    lubridate::with_tz(as.POSIXct(input, tz = "UTC"), tzone = local_tz),
    functions$GetSetting("global", "date_format"),
    tz = local_tz,
    usetz = FALSE
  )

  result <- functions$ConvertToLocalPosix(input, input = "datetime", output = "date")
  expect_equal(result, expected)
})

test_that("ConvertToLocalPosix returns a date as-is when input and output are both 'date'", {
  input <- "2025-04-10"
  expected <- as.Date(input)

  result <- functions$ConvertToLocalPosix(input, input = "date", output = "date")
  expect_equal(result, expected)
})

test_that("ConvertToLocalPosix errors on invalid input/output combinations", {
  expect_error(
    functions$ConvertToLocalPosix("2025-04-10", input = "date", output = "datetime"),
    "Invalid conversion"
  )
})

test_that("ConvertToLocalPosix works with POSIXct input", {
  input <- as.POSIXct("2025-04-10 05:00:00", tz = "UTC")


  expected <- format(
    lubridate::with_tz(input, tzone = local_tz),
    functions$GetSetting("global", "date_time_format"),
    tz = local_tz,
    usetz = FALSE
  )

  result <- functions$ConvertToLocalPosix(input, input = "datetime", output = "datetime")
  expect_equal(result, expected)
})

test_that("BuildDateTime handles local -> UTC conversion correctly", {

  date <- "2025-04-10"
  time <- "14:30"

  result <- functions$BuildDateTime(time, date, input = "local", return_type = "UTC")

  expected <- with_tz(
    as.POSIXct(paste(date, time), tz = local_tz),
    tzone = "UTC"
  )

  expect_s3_class(result, "POSIXct")
  expect_equal(result, expected)

  time <- "14:30:23"

  result <- functions$BuildDateTime(time, date, input = "local", return_type = "UTC")

  expected <- with_tz(
    as.POSIXct(paste(date, time), tz = local_tz),
    tzone = "UTC"
  )

  expect_s3_class(result, "POSIXct")
  expect_equal(result, expected)
})

test_that("BuildDateTime returns UTC POSIXct when both input and output are UTC", {
  date <- "2025-04-10"
  time <- "14:30"

  result <- functions$BuildDateTime(time, date, input = "not_local", return_type = "UTC")

  expect_s3_class(result, "POSIXct")
  expect_equal(attr(result, "tzone"), "UTC")
})

test_that("BuildDateTime handles UTC -> local conversion correctly", {

  date <- "2025-04-10"
  time <- "14:30:34"

  result <- functions$BuildDateTime(time, date, input = "not_local", return_type = "local")

  expected <- with_tz(
    as.POSIXct(paste(date, time), tz = "UTC"),
    tzone = local_tz
  )

  expect_s3_class(result, "POSIXct")
  expect_equal(result, expected)
})

test_that("BuildDateTime returns local POSIXct when both input and output are local", {

  date <- "2025-04-10"
  time <- "14:30"

  result <- functions$BuildDateTime(time, date, input = "local", return_type = "local")

  expect_s3_class(result, "POSIXct")
  expect_equal(attr(result, "tzone"), local_tz)
})

test_that("BuildDateTime throws error on invalid input values", {
  expect_error(
    functions$BuildDateTime("14:30", "2025-04-10", input = "banana", return_type = "UTC")
  )

  expect_error(
    functions$BuildDateTime("14:30", "2025-04-10", input = "local", return_type = "banana")
  )
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
                         c("id", "full_name", "start_date", "trainer", "officer", "is_active", "company_id", "firefighter_role"),
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

test_that("ParseRelativeDate handles relative days (D+N / D-N)", {
  expect_equal(
    functions$ParseRelativeDate("D+3", type = "start", refDate = as.Date("2025-04-10")),
    as.Date("2025-04-13")
  )

  expect_equal(
    functions$ParseRelativeDate("D-2", refDate = as.Date("2025-04-10")),
    as.Date("2025-04-08")
  )
})

test_that("ParseRelativeDate handles relative months", {
  expect_equal(
    functions$ParseRelativeDate("M+1", refDate = as.Date("2025-04-10")),
    as.Date("2025-05-10")
  )

  expect_equal(
    functions$ParseRelativeDate("M-1", refDate = as.Date("2025-04-10")),
    as.Date("2025-03-10")
  )
})

test_that("ParseRelativeDate handles relative years", {
  expect_equal(
    functions$ParseRelativeDate("Y+1", refDate = as.Date("2025-04-10")),
    as.Date("2026-04-10")
  )

  expect_equal(
    functions$ParseRelativeDate("Y-2", refDate = as.Date("2025-04-10")),
    as.Date("2023-04-10")
  )
})

test_that("ParseRelativeDate handles current month snap (CM)", {
  expect_equal(
    functions$ParseRelativeDate("CM", type = "start", refDate = as.Date("2025-04-15")),
    as.Date("2025-04-01")
  )

  expect_equal(
    functions$ParseRelativeDate("CM", type = "end", refDate = as.Date("2025-04-15")),
    as.Date("2025-04-30")
  )
})

test_that("ParseRelativeDate handles current quarter snap (CQ)", {
  expect_equal(
    functions$ParseRelativeDate("CQ", type = "start", refDate = as.Date("2025-04-15")),
    as.Date("2025-04-01")
  )

  expect_equal(
    functions$ParseRelativeDate("CQ", type = "end", refDate = as.Date("2025-04-15")),
    as.Date("2025-06-30")
  )
})

test_that("ParseRelativeDate handles current year snap (CY)", {
  expect_equal(
    functions$ParseRelativeDate("CY", type = "start", refDate = as.Date("2025-04-15")),
    as.Date("2025-01-01")
  )

  expect_equal(
    functions$ParseRelativeDate("CY", type = "end", refDate = as.Date("2025-04-15")),
    as.Date("2025-12-31")
  )
})

test_that("ParseRelativeDate handles offset snap strings (CQ-1, CY+2, etc.)", {
  expect_equal(
    functions$ParseRelativeDate("CQ-1", type = "start", refDate = as.Date("2025-04-15")),
    as.Date("2025-01-01")
  )

  expect_equal(
    functions$ParseRelativeDate("CY+2", type = "end", refDate = as.Date("2025-04-15")),
    as.Date("2027-12-31")
  )
})

test_that("ParseRelativeDate handles 'today'", {
  expect_equal(
    functions$ParseRelativeDate("today", refDate = as.Date("2025-04-15")),
    as.Date("2025-04-15")
  )
})

test_that("ParseRelativeDate errors on bad input", {
  expect_error(
    functions$ParseRelativeDate("banana", refDate = as.Date("2025-04-10")),
    "Invalid relative date format"
  )
})

df <- data.frame(
  id = 1:10,
  name = c("Alice", "Bob", "Charlie", "David", "Eve",
           "Frank", "Grace", "Heidi", "Ivan", "Judy"),
  age = c(25, 30, 35, 40, 28,
          32, 29, 31, 27, 26),
  valid = c(TRUE, FALSE, TRUE, TRUE, FALSE,
            TRUE, FALSE, TRUE, FALSE, TRUE)
)

test_that("CreateNamedVector returns named vector", {
  name <- "name"
  age <- "age"
  id <- 'id'

  # Test function returns a named vector
  result <- functions$CreateNamedVector(df, name, id, NULL)
  test <- stats::setNames(df[['id']], df[['name']])
  testthat::expect_equal(result, test)

  # Test function returns a named vector with correct names
  testthat::expect_equal(names(result), df[[name]])

  # Test function returns a named vector with correct values
  testthat::expect_equal(result |> unname(), df[[id]])

  # Test function returns a named vector with correct length
  testthat::expect_equal(length(result), nrow(df))

  # Test function returns a named vector with correct values when valid is FALSE
  result_invalid <- functions$CreateNamedVector(df, name = name, value = id, valid == FALSE)
  testthat::expect_equal(names(result_invalid), df[[name]][df$valid == FALSE])

})

test_that("IdToString works", {
  expect_equal(functions$IdToString(df, column = name, id = 3),
                'Charlie')
  expect_equal(functions$IdToString(df, column = name, id = 5),
               'Eve')
  expect_equal(functions$IdToString(df, column = age, id = 5),
               28)
})

test_that("StringToId works", {
  expect_equal(functions$StringToId(df, column = name, value = 'Bob'),
               2)
  expect_equal(functions$StringToId(df, column = name, value = 'Eve'),
               5)
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
