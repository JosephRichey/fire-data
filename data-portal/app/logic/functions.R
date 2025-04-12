box::use(
  stringr[...],
  lubridate[...],
  hms[...],
  dplyr[...],
  data.table[...],
  DBI[...],
  logger[...],
  glue[glue],
)

box::use(
  ./logging,
  ./app_data,
)

#' @export
GetSetting <- function(domain, key = NULL, group = NULL) {
  Filtered <- app_data$Setting |>
    filter(domain == !!domain)

  if (!is.null(group)) {
    Filtered <- Filtered |> filter(setting_group == !!group)
  }

  if (!is.null(key)) {
    Filtered <- Filtered |> filter(setting_key == !!key)
  }

  # If no matching key, return nothing and show a warning
  if (nrow(Filtered) == 0) {
    log_warn(glue("No setting found for domain '{domain}' and key '{key}'"),
             namespace = "GetSetting")
    return(NA)
  }

  result <- Filtered$setting_value
  type <- Filtered$value_type[1] # Assuming same type for all

  coerce <- switch(
    type,
    "string" = as.character,
    "numeric" = as.numeric,
    "boolean" = function(x) tolower(x) %in% c("true", "1", "t", "yes"),
    "date" = function(x) as.Date(x),
    as.character # fallback
  )

  result <- coerce(result)
  return(
    if (length(result) == 1) result[[1]] else result
  )
}

#' @export
FormatLocal <- function(dt, input = c("datetime", "date"),
                        output = c("datetime", "date", "time"),
                        seconds = FALSE) {

  input <- match.arg(input)
  output <- match.arg(output)

  # Restrict incompatible input-output combinations
  invalid_combo <- (input == "date"   && output == "time")     ||
    (input == "time"   && output == "datetime") ||
    (input == "date"   && output == "datetime")

  if (invalid_combo) {
    stop(glue::glue("Invalid conversion: cannot format input type '{input}' as output type '{output}'"))
  }

  tz <- GetSetting('global', key = 'ltz')

  # dt should already by POSIXct or date, but just to be safe
  # Normalize input to POSIXct based on type
  dt <- switch(
    input,
    "date" = as.Date(dt),
    "datetime" = as.POSIXct(dt, tz = tz)
  )

  # Format as string based on desired output
  fmt <- switch(
    output,
    "date" = GetSetting('global', key = 'date_format'),
    "time" = if (seconds) "%H:%M:%S" else "%H:%M",
    "datetime" = GetSetting('global', key = 'date_time_format')
  )

  format(dt, fmt, tz = tz, usetz = FALSE)
}


#' @export
ConvertToLocalPosix <- function(dt,
                              input = c("datetime", "date"),
                              output = c("datetime", "date")) {
  input <- match.arg(input)
  output <- match.arg(output)

  # Restrict incompatible input-output combinations
  invalid_combo <- (input == "date"   && output == "datetime")

  if (invalid_combo) {
    stop(glue::glue("Invalid conversion: cannot format input type '{input}' as output type '{output}'"))
  }

  tz_local <- GetSetting("global", "ltz")

  # Dates can only be converted to dates, so return as is
  if (input == "date" && output == "date") {
    return(as.Date(dt))
  }

  # Normalize input into UTC
  dt_utc <- as.POSIXct(dt, tz = "UTC")

  # Convert to local timezone
  dt_local <- lubridate::with_tz(dt_utc, tzone = tz_local)

  # Handle output formats
  fmt <- switch(
    output,
    "datetime" = GetSetting("global", "date_time_format"),
    "date" = GetSetting("global", "date_format")
  )

  # Always return as string
  format(dt_local, fmt, tz = tz_local, usetz = FALSE)
}

#' @export
BuildDateTime <- function(time,
                          date,
                          input = c("local", "not_local"),
                          return_type = c('UTC', 'local')) {
  input <- match.arg(input)
  return_type <- match.arg(return_type)

  tz_local <- GetSetting("global", key = "ltz")

  # Combine date and time (time is expected to be a string like "14:30")
  dt <- as.POSIXct(
    paste(date, time),
    tz = if (input == 'local') tz_local else 'UTC'
  )

  # Return converted to the desired time zone
  if (return_type == 'local') {
    return(with_tz(dt, tzone = tz_local))
  } else {
    return(with_tz(dt, tzone = 'UTC'))
  }
}



#' @export
QueryDatabase <- function(table_name) {
  query <- paste0("SELECT * FROM ", table_name)
  Data <- tryCatch(
    {
      dbGetQuery(app_data$CON, query)
    },
    error = function(e) {
      log_error("Database query failed: {e$message}", namespace = "QueryDatabase")
      NULL
    },
    warning = function(w) {
      log_warn("Database query warning: {w$message}", namespace = "QueryDatabase")
    }
  )

  return(Data)
}

#' @export
UpdateReactives <- function(
    rdfs,
    dbTableName = c('training', 'firefighter', 'attendance') #FIXME Have all tables listed
    ) {

    log_info("Updating multiple database tables.",
             namespace = "UpdateReactives")

    for (name in dbTableName) {
      df <- QueryDatabase(name)

      df <- switch(
        name,

        # Training table - convert start_time and end_time to local time
        "training" = df |>
          mutate(start_time = ,
                 end_time = as.POSIXct(end_time, tz = Sys.getenv("LTZ"))),

        "attendance" = df |>
          mutate(timestamp = as.POSIXct(timestamp, tz = Sys.getenv("LTZ"))),
      )


      rdfs[[name]] <- df
    }

    log_success(glue("All database tables updated successfully. Tables: {paste(dbTableName, collapse = ', ')}"),
             namespace = "UpdateReactives")
}





#' @export
FixColNames <- function(Data) {
  colnames(Data) <- gsub("_", " ", colnames(Data))
  colnames(Data) <- stringr::str_to_title(colnames(Data))

  return(Data)
}

#' @export
ParseUserInput <- function(string) {
  string <- stringr::str_to_title(string)
  string <- trimws(string)

  return(string)
}

#' @export
as.MT.Date <- function(date_time) {
  as.Date(date_time, tz = Sys.getenv("TZ"))
}


#' @export
#' Not tested extensively at the limits. This function is only meant to prevent people from entering duplicates.
#  Theoretically, you could create a training that crashes the entire app since you can log in early and log out late.
VerifyNoOverlap <- function(start_time, end_time) {

  UTC_start_time <- (start_time |> force_tz(GetSetting('global', key = 'ltz')) + .01) |> with_tz('UTC')
  UTC_end_time <- (end_time |> force_tz(GetSetting('global', key = 'ltz')) + .01) |> with_tz('UTC')

  # FIXME This needs to be passed as an argument
  # Overlap <- app_data$Training |>
  #   dplyr::filter(
  #     # starts during an existing training
  #     (UTC_start_time >= training_start_time & UTC_start_time <= training_end_time) |
  #     # ends during an existing training
  #     (UTC_end_time >= training_start_time & UTC_end_time <= training_end_time) |
  #     # starts before and ends after an existing training
  #     (UTC_start_time <= training_start_time & UTC_end_time >= training_end_time)
  #   )

  Overlap <- data.frame(var = 1)


  if (nrow(Overlap) > 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }

}



# local_time <- as.POSIXlt("2025-02-20 9:18:52")
# ust_time <- as.POSIXlt("2025-02-20 16:18:52", tz = 'UTC')
# date <- as.Date("2025-02-20")
#
# BuiltDateTime(local_time, date, 'local', 'local')
# BuiltDateTime(local_time, date, 'local', 'UTC')
# BuiltDateTime(ust_time, date, 'UST', 'local')
# BuiltDateTime(ust_time, date, 'UST', 'UST')


#' @export
GenerateThreshold <- function(date, leadTime, leadTimeUnit, expireCalc = FALSE) {

  if(expireCalc) {
    case_when(
      leadTimeUnit == "day" ~ date + days(leadTime),
      leadTimeUnit == 'month' ~ date %m+% months(leadTime),
      leadTimeUnit == 'year' ~ date %m+% years(leadTime)
    )
  } else {
    case_when(
      leadTimeUnit == "day" ~ date - days(leadTime),
      leadTimeUnit == 'month' ~ date %m-% months(leadTime),
      leadTimeUnit == 'year' ~ date %m-% years(leadTime)
    )
  }
}

