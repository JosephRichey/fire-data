box::use(
  shiny[testServer],
  testthat[...],
  DBI[dbConnect, dbGetQuery],
)

box::use(
  app/logic/functions[...],
  app/logic/app_data[...],
)

test_that("Testing VerifyTrainingTime function", {
  
  ## Check during Daylight Savings Time ##
  
  # Test 4 minutes before start
  expect_true(VerifyTrainingTime(as.POSIXct("2024-03-10 00:56:00")), label = "4 minutes before start DST")
  # Test 6 minutes before start
  six_minutes_early <- VerifyTrainingTime(as.POSIXct("2024-03-10 00:54:00"))
  expect_true("warning" %in% six_minutes_early, label = "6 minutes before start DST")
  # Test on start
  expect_true(VerifyTrainingTime(as.POSIXct("2024-03-10 01:00:00")), label = "On start DST")
  # Test 10 minutes after start
  expect_true(VerifyTrainingTime(as.POSIXct("2024-03-10 01:10:00")), label = "10 minutes after start DST")
  
  # Test 5 minutes before end
  expect_true(VerifyTrainingTime(as.POSIXct("2024-03-10 02:55:00")), label = "5 minutes before end DST")
  # Test 5 minutes after end
  expect_true(VerifyTrainingTime(as.POSIXct("2024-03-10 03:05:00")), label = "5 minutes after end DST")
  # Test on end
  expect_true(VerifyTrainingTime(as.POSIXct("2024-03-10 03:00:00")), label = "On end DST")
  # Test 61 minutes after end
  sixty_one_minutes_after <- VerifyTrainingTime(as.POSIXct("2024-03-10 04:01:00"))
  expect_true("warning" %in% sixty_one_minutes_after, label = "61 minutes after end DST")
  
  ## Check during Standard Time ##
  
  # Test 4 minutes before start
  expect_true(VerifyTrainingTime(as.POSIXct("2024-03-10 23:56:00")), label = "4 minutes before start ST")
  # Test 6 minutes before start
  six_minutes_early <- VerifyTrainingTime(as.POSIXct("2024-03-10 23:54:00"))
  expect_true("warning" %in% six_minutes_early, label = "6 minutes before start ST")
  # Test on start
  expect_true(VerifyTrainingTime(as.POSIXct("2024-03-11 00:00:00")), label = "On start ST")
  # Test 10 minutes after start
  expect_true(VerifyTrainingTime(as.POSIXct("2024-03-11 00:10:00")), label = "10 minutes after start ST")
  
  # Test 5 minutes before end
  expect_true(VerifyTrainingTime(as.POSIXct("2024-03-11 01:55:00")), label = "5 minutes before end ST")
  # Test 5 minutes after end
  expect_true(VerifyTrainingTime(as.POSIXct("2024-03-11 02:05:00")), label = "5 minutes after end ST")
  # Test on end
  expect_true(VerifyTrainingTime(as.POSIXct("2024-03-11 02:00:00")), label = "On end ST")
  # Test 61 minutes after end
  sixty_one_minutes_after <- VerifyTrainingTime(as.POSIXct("2024-03-11 03:01:00"))
  expect_true("warning" %in% sixty_one_minutes_after, label = "61 minutes after end ST")
  
  ## Check multiple trainings
  duplicate_training <- VerifyTrainingTime(as.POSIXct("2024-01-02 01:00:00"))
  expect_true("error" %in% duplicate_training, label = "Duplicate training on 2024-01-02")
})
