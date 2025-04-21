box::use(
  stringr[...],
  lubridate[...],
  hms[...],
  dplyr[...],
  data.table[...],
  DBI[...],
  logger[...],
  glue[glue],
  rlang[...],
  shinyalert[...],
)

box::use(
  ./logging,
  ./app_data,
)



#' @export
ParseRelativeDate <- function(relativeString, type = c("start", "end"), refDate = app_data$local_date) {
  type <- match.arg(type)
  today <- refDate

  # Handle 'today' explicitly
  if (tolower(relativeString) == "today") {
    return(refDate)
  }

  # Match snap patterns: CM, CQ, CY, optionally with +N/-N
  snap_match <- stringr::str_match(relativeString, "^(C[MQY])([+-]?)(\\d*)$")
  # snap_match has 4 columns: full match, snap unit, offset direction, amount

  # First, check there is a match for the snap pattern
  if (!is.na(snap_match[1, 1])) {
    snap_unit <- snap_match[1, 2]  # CM, CQ, CY
    sign <- snap_match[1, 3]
    amount <- snap_match[1, 4]

    # If amount is empty, default to 0
    offset <- if (amount == "") 0 else as.integer(amount)
    # and invert if sign is '-'
    if (sign == "-") offset <- -offset

    base_date <- switch(
      snap_unit,
      "CM" = refDate %m+% months(offset),
      "CQ" = refDate %m+% months(3 * offset),
      "CY" = refDate %m+% years(offset),
      stop("Unknown snap code")
    )

    return(switch(
      snap_unit,
      "CM" = if (type == "start") floor_date(base_date, "month") else ceiling_date(base_date, "month") - days(1),
      "CQ" = if (type == "start") floor_date(base_date, "quarter") else ceiling_date(base_date, "quarter") - days(1),
      "CY" = if (type == "start") floor_date(base_date, "year") else ceiling_date(base_date, "year") - days(1)
    ))
  }

  # Offset-only logic: M-1, D+3, etc.
  match <- stringr::str_match(relativeString, "^([MWDY])([+-])(\\d+)$")

  if (any(is.na(match))) {
    stop(glue::glue("Invalid relative date format: '{relativeString}'"))
  }

  unit <- match[, 2]
  sign <- match[, 3]
  n <- as.integer(match[, 4])

  offset <- if (sign == "-") -n else n

  result <- switch(
    unit,
    "M" = refDate %m+% months(offset),
    "W" = refDate + weeks(offset),
    "D" = refDate + days(offset),
    "Y" = refDate %m+% years(offset),
    stop("Unknown unit")
  )

  return(result)
}


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
    log_error(glue("No setting found for domain '{domain}' and key '{key}'"),
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
    "relative_date" = function(x) ParseRelativeDate(x),
    as.character # fallback
  )

  result <- coerce(result)
  return(
    if (length(result) == 1) result[[1]] else result
  )
}

#' @export
#' dt - datetime object in local timezone
#' input - type of input
#' output - type of output
#' target_tz - local or UTC
#' seconds - whether to include seconds in the output
#' returns - formatted string in local timezone or UTC, depedning on input
FormatDateTime <- function(dt, input = c("datetime", "date"),
                        output = c("datetime", "date", "time"),
                        target_tz = c("local", "UTC"),
                        seconds = FALSE) {

  input <- match.arg(input)
  output <- match.arg(output)
  target_tz <- match.arg(target_tz)

  # Restrict incompatible input-output combinations
  invalid_combo <- (input == "date"   && output == "time")     ||
    (input == "time"   && output == "datetime") ||
    (input == "date"   && output == "datetime")

  if (invalid_combo) {
    stop(glue::glue("Invalid conversion: cannot format input type '{input}' as output type '{output}'"))
  }

  tz <- switch(
    target_tz,
    "local" = GetSetting('global', key = 'ltz'),
    "UTC" = "UTC"
  )

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

  # UTC outputs go to the database and must be in this format
  if(output == 'datetime' & target_tz == 'UTC') {
    fmt <- "%Y-%m-%d %H:%M:%S"
  }

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

  if(input == 'datetime' && output == 'date') {
    return(as.Date(dt_local, tz = tz_local))
  }

  # Default is to return as datetime.
  return(dt_local)
}

#' @export
BuildDateTime <- function(time,
                          date,
                          input = c("local", "UTC"),
                          return_type = c('UTC', 'local')) {
  input <- match.arg(input)
  return_type <- match.arg(return_type)

  tz_local <- GetSetting("global", key = "ltz")

  # If input is posix, strip out time. shinyinputs automatically add a date.
  if(is.list(time)) {
    time <- time |>
      hms::as_hms() |>
      as.character()
  }

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
          mutate(start_time = ConvertToLocalPosix(start_time,
                                                  input = 'datetime',
                                                  output = 'datetime'),
                 end_time = ConvertToLocalPosix(end_time,
                                                  input = 'datetime',
                                                  output = 'datetime')),

        "attendance" = df |>
          mutate(check_in = ConvertToLocalPosix(check_in,
                                                  input = 'datetime',
                                                  output = 'datetime'),
                 check_out = ConvertToLocalPosix(check_out,
                                                input = 'datetime',
                                                output = 'datetime')),
        "firefighter" = df |>
          mutate(start_date = ConvertToLocalPosix(start_date,
                                                  input = 'date',
                                                  output = 'date'))
      )


      rdfs[[name]] <- df
    }

    log_success(glue("All database tables updated successfully. Tables: {paste(dbTableName, collapse = ', ')}"),
             namespace = "UpdateReactives")
}

#' @export
BuildNamedVector <- function(df, name, value, filterExpr = NULL) {
  # Convert arguments to quosures
  name <- enquo(name)
  value <- enquo(value)
  filter_expr <- enquo(filterExpr)

  # Only filter if a filter expression was actually passed
  if (quo_is_null(filter_expr)) {
    v <- df
  } else {
    v <- df |> filter(!!filter_expr)
  }

  # Grab the appropriate two columns
  v <- v |> select(name = !!name, value = !!value)

  # return the named vector
  return(stats::setNames(v$value, v$name))
}

#' @export
IdToString <- function(df, column, id) {
  column <- enquo(column)
  id <- enquo(id)

  v <- df |>
    filter(id == !!id) |>
    select(!!column) |>
    pull()

  return(v)
}


#' @export
StringToId <- function(df, column, value) {
  column <- enquo(column)

  v <- df |>
    filter(!!column == value) |>
    select(id) |>
    pull()

  return(v)
}

#' @export
GetTrainingClassificationId <- function(df, category, topic = NULL) {

  # To get the updateSelectStatemnet to work, there has to be topic given.
  # If no topic exists, it is set to No Topics Found
  # Swithc this to null so we can do a proper search.

  if(topic == "No Topics Found") {
    topic <- NULL
  }

  v <- df |>
    filter(
      training_category == category,
      if (is.null(topic)) is.na(training_topic) else training_topic == topic
    ) |>
    select(id) |>
    pull()

  if (length(v) != 1) {
    log_error(glue::glue("Expected exactly one match for category = '{category}', topic = '{topic}', but found {length(v)}."),
             namespace = "GetTrainingClassificationId")
    stop()
  }

  return(v)
}


#' @export
FixColNames <- function(data, prefix = NULL) {
  colnames(data) <- gsub("_", " ", colnames(data))
  colnames(data) <- stringr::str_to_title(colnames(data))

  if(!is.null(prefix)) {
    colnames(data) <- gsub(prefix, "", colnames(data))
  }

  return(data)
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
#' This will return a string of trainings that overlap. This also considers
#' the early check in and late check out times.
CheckTrainingsOverlap <- function(startTime, endTime, df, editTrainingId = NULL) {

  input_interval <- lubridate::interval(
    startTime - minutes(GetSetting('training', key = 'early_check_in')),
    endTime + minutes(GetSetting('training', key = 'late_check_out'))
  )

  Overlap <- df |>
    filter(is.na(is_deleted)) |>
    filter(
      lubridate::int_overlaps(
        lubridate::interval(
          start_time - minutes(GetSetting('training', key = 'early_check_in')),
          end_time + minutes(GetSetting('training', key = 'late_check_out'))
        ),
        input_interval
      )
    )

  # If editTrainingId is provided, exclude it from the overlap check
  # When editing an exisitng training, we don't want to include it in the overlap check
  if(!is.null(editTrainingId)) {
    Overlap <- Overlap |>
      filter(id != editTrainingId)
  }

  if (nrow(Overlap) > 0) {
    return(
      glue::glue(
        "Training overlaps with existing training(s) on {paste(Overlap$start_time, collapse = ', ')}"
      )
      )
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


#' Check the result of a database write and show success/error modals
#'
#' @param result Number returned by dbExecute
#' @param successMessage Text for success modal
#' @param failureContext Optional text for failure detail (e.g., "inserting training")
#' @param expectedMin Number of rows expected (defaults to 1)
#' @param expectedMax Number of rows expected (defaults to 1)
#'
#' @export
CheckWriteResult <- function(result,
                             successMessage = "Write successful.",
                             context = NULL,
                             expectedMin = 1,
                             expectedMax = 1) {
  if (!is.numeric(result) || is.na(result) || result < expectedMin ||
      result > expectedMax) {
    log_error(glue::glue(
      "Database write failed {context}. ",
      "Result: {result}"
    ), namespace = "CheckWriteResult")

    shinyalert(
      title = "Error",
      text = glue::glue("Database write failed {context}. ",
                        "Result: {result}
                        Please contact your application administrator."),
      type = "error",
      closeOnClickOutside = TRUE
    )
  } else {
    shinyalert(
      title = "Success",
      text = successMessage,
      type = "success",
      closeOnClickOutside = TRUE
    )

    log_success(glue::glue(
      "Database write successful {context}. ",
      "Result: {result}"
    ), namespace = "CheckWriteResult")
  }
}


