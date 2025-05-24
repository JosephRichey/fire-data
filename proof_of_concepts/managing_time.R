library(DBI)
library(RMySQL)
library(lubridate)
library(hms)
library(dplyr)

CON <- dbConnect(RMySQL::MySQL(),
                 dbname = "test",
                 host = Sys.getenv("DB_HOST"),
                 port = 3306,
                 user = "admin",
                 password = Sys.getenv("DB_PASSWORD"))


# Example Utility Functions
to_utc_string <- function(dt) format(dt, tz = "UTC", usetz = FALSE)
to_local_posix <- function(dt, local_tz) as.POSIXct(dt, tz = "UTC") |> with_tz(local_tz)
to_local_string <- function(dt, local_tz) format(dt, "%Y-%m-%d %H:%M:%S", tz = local_tz, usetz = FALSE)


x1 <- as.POSIXct('2025-01-23 00:00:00')

exampleWorkflow <- function(dt) {

  ## General Guidelines
  # 1. Datetime is always in UTC in the database in a character string
    #  format(dt, tz = "UTC", usetz = FALSE)
  # 2. Anytime data is in memory in R, convert to local time zone in a POSIXct object
    # as.POSIXct(dt, tz = "UTC") |> with_tz("America/Denver")
  # 3. When displaying value, convert to local time zone in a character string
    # format(dt, "%Y-%m-%d %H:%M:%S", tz = "America/Denver", usetz = FALSE)


  cat("Input date-time:", dt, "\n")
  cat("Input str() of date-time:")
  str(dt)
  cat("\n\n\n")

  # Take a user input. Comes in as a datetime with default tz (UTC). Force it to local.
  dt <- force_tz(dt, tzone = "America/Denver")

  cat("Local date-time:", dt, "\n")
  cat("Local str() of date-time:")
  str(dt)
  cat("\n\n\n")

  # Convert to UTC and write to database
  dt_utc <- format(dt, tz = "UTC", usetz = FALSE)

  cat("UTC date-time:", dt_utc, "\n")
  cat("UTC str() of date-time:")
  str(dt_utc)
  cat("\n\n\n")

  dbExecute(CON, paste0("Insert into test (test_1) values ('", dt_utc, "')"))

  # Read from database and convert back to local time zone
  dt_import <- dbGetQuery(CON, "SELECT * from test")$test_1[1]
  dbExecute(CON, "DELETE FROM test")

  cat("dt_import from database:", dt_import, "\n")
  cat("str() of dt_import from database:")
  str(dt_import)
  cat("\n\n\n")

  dt_local_ct <- as.POSIXct(dt_import, tz = "UTC") |> with_tz("America/Denver")

  cat("Read UTC date-time from database:", dt_utc, "\n")
  cat("Read UTC str() of date-time from database:")
  str(dt_local_ct)
  cat("\n\n\n")

  dt_local_output <- format(dt_local_ct, "%Y-%m-%d %H:%M:%S", tz = "America/Denver", usetz = FALSE)

  cat("dt_local at end:", dt_local, "\n")
  cat("str() of dt_local at end:")
  str(dt_local_output)
  cat("\n\n\n")

}

exampleWorkflow(x1)



