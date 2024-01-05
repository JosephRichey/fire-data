# Load application support files into testing environment
shinytest2::load_app_env()

box::use(
  ../../cfd_training_attendance/functions/functions,
)

# test_that("Test verify training time returns errors when appropriate.", {
#   functions$VerifyTrainingTime(as.POSIXct("2024-01-03 20:44:13 MST"))
# })
